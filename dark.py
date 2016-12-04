from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import copy
import termcolor

import server

class attrdict(dict):
  def __init__(self, *args, **kwargs):
    dict.__init__(self, *args, **kwargs)
    self.__dict__ = self

class createnode:
  def __init__(meta, datasource=False, numinputs=None, fields=[], is_schema=False):
    if isinstance(fields, str): fields = [fields]
    meta.fields = fields
    meta.numinputs = numinputs
    meta.datasource = datasource
    meta.is_schema = is_schema

  def __call__(meta, func):
    class ANode(Node):
      def __init__(self, *args):
        assert len(args) == len(meta.fields)
        self.args = args

      def is_datasource(self):
        return meta.datasource

      def name(self):
        return "%s-%x" % (func.__name__, id(self))

      def _get(self, *inputs):
        assert numinputs == len(inputs)
        d = attrdict()
        for i, f in enumerate(fields):
          d[f] = self.args[i]
        if len(d) > 0:

          return func(d, *inputs)
        else:
          return func(*inputs)

      def __getattr__(self, name):
        if ((meta.is_schema and name == "get_schema")
            or (not meta.is_schema and name == "get_data")):
          return self._get
        return None

    return ANode

class schema(createnode):
  def __init__(self, **kwargs):
    kwargs["is_schema"] = True
    createnode.__init__(self, **kwargs)

class data(createnode):
  def __init__(self, **kwargs):
    kwargs["is_schema"] = False
    createnode.__init__(self, **kwargs)


class Node:
  def is_datasource(self):
    return False

  def name(self):
    raise Exception("Must have a name")

  def __str__(self):
    return self.name()

  def push_data(self, *inputs):
    return self.get_data(*inputs)


def pr(ind, str):
  print("%s %s%s" % (ind, ind*". ", termcolor.colored(str, 'red')))

def tojson(l):
  def default(val):
    return None
  return json.dumps(l, default=default)

def immut(v):
  "To avoid errors, all data is immutable. For convenience, some functions may return mutable values, so we need to convert them"
  print(v)
  raise Exception("TODO: immutable")

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

    "Inputs are a discrete event, whereas outputs are continuous. This"
    "changes how we think about the flow of data"

    "Push the data down from the root. If we come to a fork, chase up the fork to get a value."

    pr(ind, "%s %s" % (node, str(inputs)))
    new_input = immut(node.push_data(*inputs))
    self.tracker[node] = new_input

    children = self.get_children(node)
    for c in children:
      inputs = [new_input]
      parents = self.get_parents(c)
      for p in parents:
        if p != node:
          pr(ind, "parent: %s" % (p))
          (data, schema) = immut(self.run_output(p, True, False, ind+1))
          assert(schema == None)
          pr(ind, "return from parent node %s: %s" % (p, data))
          inputs.append(data)

      self.run_input(c, inputs, ind+1)


  def run_output(self, node, get_data, get_schema, ind):
    if node in self.tracker:
      pr(ind, "found existing val for %s: %s" % (node, self.tracker[node]))
      return (self.tracker[node], None)

    pr(ind, "run_output: %s" % (node))

    parents = self.get_parents(node)

    datas = []
    schemas = []
    get_data_orig, get_schema_orig = get_data, get_schema
    get_data = get_data and hasattr(node, "get_data")
    get_schema = get_schema and hasattr(node, "get_schema")

    # error checking

    if get_data_orig and not get_data:
      pr(ind, "No longer getting data for %s" % (node))

    if get_schema_orig and not get_schema:
      pr(ind, "No longer getting schema for %s" % (node))

    if not get_data and not get_schema:
      raise Exception("Stopped getting both at %s" % node)

    for p in parents:
      data, schema = immut(self.run_output(p,
                                           get_data,
                                           get_schema,
                                           ind+1))
      pr(ind, "parent %s has output %s" % (p, (data, schema)))
      datas.append(data)
      schemas.append(schema)

    data_out = None
    if get_data:
      pr(ind, "current %s(%s)" % (node, datas))
      data_out = immut(node.get_data(*datas))
      pr(ind, "has output %s" % (data_out))

    schema_out = None
    if get_schema:
      schema_out = immut(node.get_schema(*schemas))

    res = (data_out, schema_out)
    if res == (None, None):
      raise Exception("Both values can't be none for %s" % str(node))

    pr(ind, "%s returns %s" % (node, str(res)))

    self.tracker[node] = data_out
    return res

  def add_output(self, node, verb, url):
    self._add(node)
    def h(request):
      self.tracker = {}
      (val1, val2) = immut(self.run_output(node, True, True, 0))
      return Response(val1 or val2, mimetype='text/html')

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
