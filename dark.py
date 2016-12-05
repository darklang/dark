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

class node:
  def __init__(meta, datasource=False, numinputs=None, fields=[]):
    if isinstance(fields, str): fields = [fields]
    meta.fields = fields
    meta.numinputs = numinputs
    meta.datasource = datasource

  def __call__(meta, func):
    class ANode(Node):
      def __init__(self, *args):
        assert len(args) == len(meta.fields)
        self.args = args

      def is_datasource(self):
        return meta.datasource

      def name(self):
        return "%s-%x" % (func.__name__, id(self))

      def get(self, *inputs):
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

  def name(self):
    raise Exception("Must have a name")

  def __str__(self):
    return self.name()

  def push(self, *inputs):
    return self.get(*inputs)


def pr(ind, str):
  print("%s %s%s" % (ind, ind*". ", termcolor.colored(str, 'red')))

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
    if node.is_datasource(): return []

    children = self.edges[node.name()] or []
    return [self.nodes[c] for c in children]

  def get_parents(self, node):
    if node.is_datasource(): return []

    parents = self.reverse_edges.get(node.name(), [])
    return [self.nodes[p] for p in parents]


  def run_input(self, node, inputs, ind):
    # Inputs are a discrete event, whereas outputs are continuous. This
    # changes how we think about the flow of data.

    # We can have lazy values that "block" until a value is put in them.
    # Then they all fire.

    # But here's what we actually did: push the data down from the root.
    # If we come to a fork, chase up the fork to get a value.

    pr(ind, "%s %s" % (node, inputs))
    computed_value = immut(node.push(*inputs))
    self.tracker[node] = computed_value

    children = self.get_children(node)
    for c in children:
      inputs = []
      parents = self.get_parents(c)
      for p in parents:
        if p == node:
          inputs.append(computed_value)
        else:
          pr(ind, "parent: %s" % (p))
          val = immut(self.run_output(p, ind+1))
          pr(ind, "return from parent node %s: %s" % (p, val))
          inputs.append(val)

      self.run_input(c, inputs, ind+1)


  # Like _run_output, but with logging and assertions and caching
  def run_output(self, node, ind):
    if node in self.tracker:
      pr(ind, "found existing val for %s: %s" % (node,
                                                 self.tracker[node]))
      return self.tracker[node]

    pr(ind, "run_output: %s" % (node))
    result = self._run_output(node, ind)
    assert result != None
    pr(ind, "%s returns %s" % (node, str(result)))

    self.tracker[node] = result
    return result

  def _run_output(self, node, ind):
    args = [immut(self.run_output(p, ind+1))
            for p in self.get_parents(node)]
    return immut(node.get(*args))

  def add_output(self, node, verb, url):
    self._add(node)
    def h(request):
      self.tracker = {}
      val = pyrsistent.thaw(self.run_output(node, 0))
      print(val)
      return Response(val, mimetype='text/html')

    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))

  def add_input(self, node, verb, url, redirect_url):
    self._add(node)
    def h(request):
      self.tracker = {}
      self.run_input(node, [immut(request.values.to_dict())], 0)
      return redirect(redirect_url)
    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))
    return node
