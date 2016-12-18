import inspect
import random
import json

import pyrsistent as pyr

class Value:
  def __init__(self, valuestr):
    self.name = valuestr
    self.value = exec(valuestr)
    self._id = random.randint(0, 2**32)

  def is_datasource(self):
    return True

  def is_datasink(self):
    return False

  def exe(self):
    return self.value

  def to_frontend(self):
    return { "name": self.name,
             "parameters": [],
             "id": self.id(),
             "x": self.x,
             "y": self.y}

  def id(self):
    return "VALUE-%04X" % (self._id % 2**16)

class Node:
  def __init__(self, fnname):
    self.fnname = fnname
    self._id = random.randint(0, 2**32)
    self.x = -1
    self.y = -1
    assert self._getfn()

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
    self.reverse_edges = {}
    self.datastores = {}

  def _add(self, node):
    self.nodes[node.id()] = node
    if node.id() not in self.edges:
      self.edges[node.id()] = []
    if node.id() not in self.reverse_edges:
      self.reverse_edges[node.id()] = []

  def add_datastore(self, ds):
    self.datastores[ds.id()] = ds
    self.nodes[ds.id()] = ds

  def has(self, node):
    return node.id() in self.nodes

  def delete_node(self, node):
    self.clear_edges(node)
    del self.nodes[node.id()]


  def add_edge(self, n1, n2, n2param):
    self._add(n1)
    self._add(n2)

    self.edges[n1.id()].append((n2.id(), n2param))
    self.reverse_edges[n2.id()].append(n1.id())

    return (n1, n2)

  def clear_edges(self, node):
    """As we develop, sometimes graphs get weird. So we actually check the whole
    graph to fix it up, not just doing what we expect to find."""
    E = self.edges
    R = self.reverse_edges
    id = node.id()
    for s, ts in E.items():
      E[s] = [(t,p) for (t, p) in ts if t != id]
    for t, ss in R.items():
      R[t] = [s for s in ss if s != id]
    if id in E:
      del E[id]
    if id in R:
      del R[id]

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

  def to_frontend_edges(self):
    result = []
    for s in self.edges.keys():
      for (t,p) in self.edges[s]:
        result.append({"source": s, "target": t, "paramname": p})
    return result

  def run_output(self, node):
    # print("run_output on node: %s" % (node))
    return self.execute(node)

  def to_frontend(self, cursor):
    nodes = {n.id(): n.to_frontend() for n in self.nodes.values()}
    edges = self.to_frontend_edges()

    result = {"nodes": nodes, "edges": edges}
    if cursor :
      result["cursor"] = cursor.id()
    return json.dumps(result, sort_keys=True, indent=2)
