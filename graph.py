import inspect
import random

import pyrsistent as pyr


class Node:
  def __init__(self, fnname):
    self.fnname = fnname
    self._id = random.randint(0, 2**32)
    self.x = -1
    self.y = -1

  def _getfn(self):
    import fns
    return getattr(fns, self.fnname)

  def is_datasource(self):
    return getattr(self._getfn(), "datasource", False)

  def is_datasink(self):
    return getattr(self._getfn(), "datasink", False)

  def get_parameters(self):
    func = self._getfn()
    (params, _, _, _) = inspect.getargspec(func)
    return params

  def exe(self, **args):
    func = self._getfn()
    params = self.get_parameters()

    assert len(params) == len(args)
    for p in params:
      assert p in args

    # TODO: get named arguments here
    return func(**args)

  def to_frontend(self):
    return { "name": self.fnname,
             "parameters": self.get_parameters(),
             "id": self.id(),
             "x": self.x,
             "y": self.y}

  def id(self):
    return "%s-%04X" % (self.fnname, self._id % 2**16)



class Graph():

  def __init__(self):
    super().__init__()
    self.nodes = {}
    self.edges = {}
    self.datastores = {}
    self.reverse_edges = {}

  def _add(self, node):
    self.nodes[node.id()] = node

  def add_datastore(self, ds):
    self.datastores[ds.name()] = ds

  def has(self, node):
    return node.id() in self.nodes

  def add_edge(self, n1, n2, n2param):
    self._add(n1)
    self._add(n2)

    if n1.name() not in self.edges:
      self.edges[n1.id()] = []
    self.edges[n1.id()].append((n2.id(), n2param))

    if n2.name() not in self.reverse_edges:
      self.reverse_edges[n2.id()] = []
    self.reverse_edges[n2.id()].append(n1.id())

    return (n1, n2)

  def print_graph(self):
    print(self.nodes)
    print(self.edges)
    print(self.reverse_edges)

  def get_children(self, node):
    if node.is_datasink(): return []

    children = self.edges[node.id()] or []
    return {param: self.nodes[c] for (c, param) in children}

  def get_parents(self, node):
    if node.is_datasource(): return []

    parents = self.reverse_edges.get(node.id(), [])
    return [self.nodes[p] for p in parents]


  def execute(self, node, only=None, eager={}):
    # print("executing node: %s" % (node))
    if node in eager:
      result = eager[node]
    else:
      args = {param: self.execute(p, eager=eager)
              for (p, param) in self.get_parents(node).items()
              if only in [None, p]}
      result = node.exe(**args)

    return pyr.freeze(result)

  def find_sink_edges(self, node):
    results = set()
    for c in self.get_children(node):
      if c.is_datasink():
        results |= {(node, c)}
      else:
        results |= self.find_sink_edges(c)
    return results

  def run_input(self, node, val):
    for (parent, sink) in self.find_sink_edges(node):
      # print("run_input on sink,parent: %s, %s" %(sink, parent))
      self.execute(sink, only=parent, eager={node: val})

  def run_output(self, node):
    # print("run_output on node: %s" % (node))
    return self.execute(node)

  def to_frontend(self, cursor):
    nodes = [n.to_frontend() for n in self.nodes.values()]
    dses = [ds.to_frontend() for ds in self.datastores.values()]
    allnodes = {n["id"]: n for n in (nodes + dses)}
    if cursor == None:
      cursor = ""
    else:
      cursor = cursor.id()
    from util import (tojson)
    return tojson({"nodes": allnodes, "cursor": cursor})
