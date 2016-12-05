from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import copy
import termcolor
import pyrsistent

import server

class attrdict(dict):
  def __init__(self, *args, **kwargs):
    dict.__init__(self, *args, **kwargs)
    self.__dict__ = self


class LazyValue(object):
  def __init__(self, node, *args, **kwargs):
    self.node = node
    self.args = args
    self.kwargs = kwargs

  def deref(self):
    args = [arg.deref() for arg in self.args]
    kwargs = {k: v for (k,v) in self.kwargs}
    return self.node.get(*args, **kwargs)


class node:
  def __init__(meta,
               datasource=False,
               datasink=False,
               numinputs=None,
               fields=[]):
    assert isinstance(fields, list)
    meta.fields = fields
    meta.numinputs = numinputs
    meta.datasource = datasource
    meta.datasink = datasink

  def __call__(meta, func):
    class ANode(Node):
      def __init__(self, *args):
        assert len(args) == len(meta.fields)
        self.args = args

      def is_datasource(self):
        return meta.datasource

      def is_datasink(self):
        return meta.datasink

      def name(self):
        return "%s-%x" % (func.__name__, id(self))

      def get(self, *inputs):
        # TODO: get numinputs from looking at the args of f
        assert meta.numinputs == None or meta.numinputs == len(inputs)
        d = attrdict()
        for i, f in enumerate(meta.fields):
          d[f] = self.args[i]
        if len(d) > 0:

          return func(d, *inputs)
        else:
          return func(*inputs)

    return ANode

class Node:
  def is_datasource(self):
    return False

  def is_datasink(self):
    return False

  def __str__(self):
    return self.name()


def tojson(l):
  def default(val):
    return None
  return json.dumps(l, default=default)

def immut(v):
  # To avoid errors, all data is immutable. For convenience, some
  # functions may return mutable values, so we need to convert them
  if v == None:
    return None
  return pyrsistent.freeze(v)

class Dark(server.Server):

  def __init__(self):
    super().__init__()
    self.url_map = Map()
    self.nodes = {}
    self.edges = {}
    self.reverse_edges = {}
    #self.add_standard_routes()
    self.add_admin_routes()

  def add_admin_routes(self):
    def showgraph(request):
      return self.render_template('graph.html',
                                  nodes=tojson(list(self.nodes.keys())),
                                  edges=tojson(self.edges),
                                  reverse_edges=self.reverse_edges)

    self.url_map.add(Rule('/admin/graph', endpoint=showgraph))

  def add_standard_routes(self):
    # TODO: move to a component
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def _add(self, node):
    self.nodes[node.name()] = node

  def has(self, node):
    return node.name() in self.nodes

  def add_edge(self, n1, n2):
    self._add(n1)
    self._add(n2)

    if n1.name() not in self.edges:
      self.edges[n1.name()] = []
    self.edges[n1.name()].append(n2.name())

    if n2.name() not in self.reverse_edges:
      self.reverse_edges[n2.name()] = []
    self.reverse_edges[n2.name()].append(n1.name())

    return (n1, n2)

  def print_graph(self):
    print(self.nodes)
    print(self.edges)
    print(self.reverse_edges)

  def get_children(self, node):
    if node.is_datasink(): return []

    children = self.edges[node.name()] or []
    return [self.nodes[c] for c in children]

  def get_parents(self, node):
    if node.is_datasource(): return []

    parents = self.reverse_edges.get(node.name(), [])
    return [self.nodes[p] for p in parents]


  def input_value(self, node, inputs, ind):

    # Inputs are a discrete event, whereas outputs are continuous. This
    # changes how we think about the flow of data.

    # We can have lazy values that "block" until a value is put in them.
    # Then they all fire.

    # But here's what we actually did: push the data down from the root.
    # If we come to a fork, chase up the fork to get a value.

    # TODO: The bug here is that it goes down both paths. It caches to
    # stop the input being processed twice, but we still process the
    # output twice. It's pushed down both paths until it hits the DB,
    # and then it enters it twice. Problem: its a shit algorithm.

    # Better algorithm: when we create the graph we create lazy values
    # representing their inputs. Then when we get an input we fire the
    # end state. I imagine this is how spreadsheets work.

    thisval = LazyVal(node, *inputs)
    self.tracker[node] = val
    if node.is_datasource(): return

    children = self.get_children(node)
    for c in children:
      if c.is_datasink():
        inputs = [val]
      else:
        inputs = [thisval if p == node else self.output_value(p)
                  for p in self.get_parents(c)]

      self.input_value(c, inputs)


  # Like _output_value, but with logging and assertions and caching
  def output_value(self, node):
    if node in self.tracker:
      return self.tracker[node]

    result = self._run_output(node, ind)
    assert result != None

    self.tracker[node] = result
    return result

  def _output_value(self, node):
    args = [self.output_value(p)
            for p in self.get_parents(node)]
    return LazyVal(node, *args)

  def build_outval(self, node):



  def add_output(self, node, verb, url):
    self._add(node)
    self.output_vals[node] = self.build_outval(node)

    def h(request):
      val = self.outvals[node].deref()
      print(val)
      return Response(val, mimetype='text/html')

    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))

  def add_input(self, node, verb, url, redirect_url):
    self._add(node)
    self.invals[node] += self.build_inval(node)
    def h(request):
      val = request.values.to_dict()
      for leaf in self.invals[node]:
        leaf.deref(node, val)
      return redirect(redirect_url)
    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))
    return node
