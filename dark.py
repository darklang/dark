from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule

import server
import copy

class Node:
  def is_datasource(self):
    return False

  def __str__(self):
    return self.name

  def push_data(self, *inputs):
    "Unless specified, this is just pulling data from the other direction"
    return self.get_data(*inputs)



class Dark(server.Server):

  def __init__(self):
    super().__init__()
    self.url_map = Map()
    self.nodes = {}
    self.edges = {}
    self.reverse_edges = {}
    #self.add_standard_routes()

  def add_standard_routes(self):
    # TODO: move to a component
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def generate_name(self):
    import random
    hash = random.getrandbits(32)
    return ("%08x" % hash)

  def add(self, node):
    if not hasattr(node, "name"):
      name = type(node).__name__ + "-" + self.generate_name()
      node.name = name
    self.nodes[node.name] = node
    return node

  def edge_from(self, n1, n2):
    self.add(n1)
    self.add(n2)
    if n1.name not in self.edges:
      self.edges[n1.name] = []
    self.edges[n1.name].append(n2.name)

    if n2.name not in self.reverse_edges:
      self.reverse_edges[n2.name] = []
    self.reverse_edges[n2.name].append(n1.name)

  def print_graph(self):
    print(self.nodes)
    print(self.edges)
    print(self.reverse_edges)

  def get_children(self, node):
    if node.is_datasource(): return []

    children = self.edges[node.name] or []
    return [self.nodes[c] for c in children]

  def get_parents(self, node):
    if node.is_datasource(): return []

    parents = self.reverse_edges[node.name] or []
    return [self.nodes[p] for p in parents]


  def run_input(self, node, inputs):
    "Inputs are a discrete event, whereas outputs are continuous. This"
    "changes how we think about the flow of data"

    "Push the data down from the root. If we come to a fork, chase up the fork to get a value."

    print("run_input: " + str(node))
    new_input = node.push_data(*inputs)
    children = self.get_children(node)

    for c in children:
      inputs = [new_input]
      parents = self.get_parents(c)
      for p in parents:
        print("node is %s" % node)
        print("inputs are " + str(inputs))
        if p != node:
          (data, schema) = self.run_output(p, True, False)
          inputs.append(data)

      print("running on %s with %s" % (c, inputs))
      self.run_input(c, inputs)


  def run_output(self, node, get_data, get_schema):
    # TODO: inputs pull schema the opposite way that data flows

    print("run_output: %s" % node.name)

    parents = self.get_parents(node)

    datas = []
    schemas = []
    get_data_orig, get_schema_orig = get_data, get_schema
    get_data = get_data and hasattr(node, "get_data")
    get_schema = get_schema and hasattr(node, "get_schema")

    # error checking

    if get_data_orig and not get_data:
      print("No longer getting data for %s", node)

    if get_schema_orig and not get_schema:
      print("No longer getting schema for %s", node)

    if not get_data and not get_schema:
      raise Exception("Stopped getting both at %s" % node)

    for p in parents:
      (data, schema) = self.run_output(p, get_data, get_schema)
      datas.append(data)
      schemas.append(schema)

    data_out = None
    if get_data:
      data_out = node.get_data(*datas)

    schema_out = None
    if get_schema:
      schema_out = node.get_schema(*schemas)

    res = (data_out, schema_out)
    if res == (None, None):
      raise Exception("Both values can't be none for %s" % str(node))

    print("%s returns %s" % (node.name, str(res)))

    return copy.copy(res)


  def add_output(self, node, verb, url):
    self.add(node)
    def h(request):
      (val1, val2) = self.run_output(node, True, True)
      return Response(val1 or val2, mimetype='text/html')

    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))

  def add_input(self, node, verb, url):
    self.add(node)
    def h(request):
      return self.run_input(node, [request.values.to_dict()])
    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))
