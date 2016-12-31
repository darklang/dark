import inspect
import random
import json
import os
import pickle

import pyrsistent as pyr

import fns

def get_all_graphnames():
  return [f[:-5] for f in os.listdir("appdata") if f.endswith(".dark")]

def filename_for(name):
  return "appdata/" + name + ".dark"

def load(name):
  filename = filename_for(name)
  with open(filename, 'rb') as file:
    graph = pickle.load(file)
  graph.migrate(name)
  return graph

def save(G):
  filename = filename_for(G.name)

  # dont use pickle.dump - it tends to corrupt
  data = pickle.dumps(G)

  with open(filename, 'wb') as file:
    file.write(data)

  # sanity check
  load(G.name)




class Value:
  def __init__(self, valuestr):
    self.name = valuestr
    self.value = eval(valuestr)
    self._id = random.randint(0, 2**32)

  def is_datasource(self):
    return True

  def is_datasink(self):
    return False

  def is_page(self):
    return False

  def exe(self):
    return self.value

  def to_frontend(self):
    return { "name": self.name,
             "parameters": [],
             "id": self.id(),
             "type": "value",
             "x": self.x,
             "y": self.y}

  def id(self):
    return "VALUE-%04X (%s)" % ((self._id % 2**16), self.name)

class Node:
  def __init__(self, fnname):
    self.fnname = fnname
    self._id = random.randint(0, 2**32)
    self.x = -1
    self.y = -1
    assert self._getfn()

  def __str__(self):
    return self.id()

  def is_page(self):
    return "page" in self.fnname

  def _getfn(self):
    import fns
    fn = getattr(fns, self.fnname, None)
    if not fn and "page" in self.fnname:
      fn = fns.page
    if fn == None:
      def fn(args):
        return {}
    return fn

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

    # TODO: figure out page
    # assert len(params) == len(args)
    # for p in params:
      # assert p in args

    # TODO: get named arguments here
    return func(**args)

  def to_frontend(self):
    return { "name": self.fnname,
             "parameters": self.get_parameters(),
             "id": self.id(),
             "type": "page" if "page" in self.fnname else "function",
             "x": self.x,
             "y": self.y}

  def id(self):
    return "%s-%04X" % (self.fnname, self._id % 2**16)



class Graph:

  def __init__(self, name):
    self.name = name
    self.nodes = {}
    self.edges = {}

  def __getattr__(self, name):
    # pickling is weird with getattr
    if name.startswith('__') and name.endswith('__'):
      return super(Graph, self).__getattr__(key)

    if name == "reverse_edges":
      result = {}
      for k,_ in self.nodes.items():
        result[k] = []
      for s,ts in self.edges.items():
        for (t,_) in ts:
          result[t] += [s]
      return result

    if name == "pages":
      return {k: v for k,v in self.nodes.items() if v.is_page()}

    if name == "datastores":
      return {k: v for k,v in self.nodes.items() if v.is_datastore()}

    return None

  def _add(self, node):
    self.nodes[node.id()] = node
    if node.id() not in self.edges:
      self.edges[node.id()] = []

  def add_datastore(self, ds):
    self.nodes[ds.id()] = ds

  def has(self, node):
    return node.id() in self.nodes

  def delete_node(self, node):
    self.clear_edges(node)
    del self.nodes[node.id()]
    if node.id() in self.nodes:
      del self.nodes[node.id()]

  def add_edge(self, n1, n2, n2param):
    self._add(n1)
    self._add(n2)

    self.edges[n1.id()].append((n2.id(), n2param))

    return (n1, n2)

  def clear_edges(self, node):
    """As we develop, sometimes graphs get weird. So we actually check the whole
    graph to fix it up, not just doing what we expect to find."""
    E = self.edges
    id = node.id()
    for s, ts in E.items():
      E[s] = [(t,p) for (t, p) in ts if t != id]
    if id in E:
      del E[id]

  def get_children(self, node):
    children = self.edges[node.id()] or []
    return {param: self.nodes[c] for (c, param) in children}

  def get_parents(self, node):
    parents = self.reverse_edges.get(node.id(), [])
    result = []
    for p in parents:
      t = self.nodes[p]
      paramname = self.get_target_param_name(node, t)
      result += [(paramname, t)]
    return {paramname: t for (paramname, t) in result}

  def get_named_parents(self, node, paramname):
    ps = self.get_parents(node)
    result = []
    for pparam, p in ps.items():
      if pparam == paramname:
        result += [p]
    return result

  def get_target_param_name(self, target, src):
    for (c, p) in self.edges[src.id()]:
      if c == target.id():
        return p
    assert(False and "Shouldnt happen")

  def execute(self, node, only=None, eager={}):
    # print("executing node: %s, with only=%s and eager=%s" % (node, only, eager))
    if node in eager:
      result = eager[node]
    else:
      args = {paramname: self.execute(p, eager=eager)
              for paramname, p in self.get_parents(node).items()
              if only in [None, p]}
      result = node.exe(**args)

    return pyr.freeze(result)

  def find_sink_edges(self, node):
    # print("finding sink edges: %s" % (node))
    results = set()
    for _, c in self.get_children(node).items():
      if c.is_datasink():
        results |= {(node, c)}
      else:
        results |= self.find_sink_edges(c)
    return results

  def run_input(self, node, val):
    # print("running input: %s (%s)" % (node, val))
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

  def to_debug(self, cursor):
    result = []
    edges = self.to_frontend_edges()
    for e in edges:
      out = "%s --(%s)--> %s)" % (e["source"], e["paramname"], e["target"])
      result.append(out)
    for n in self.nodes.values():
      if n.id() in self.edges and \
         len(self.edges[n.id()]) == 0 and \
         len(self.reverse_edges[n.id()]) == 0:
        result.append("Solo node: " + n.id())

    return "\n".join(sorted(result))

  def migrate(self, name):
    # no name and no version
    if not getattr(self, "version", None):
      self.name = name
      self.version = 1

    print("Migrating %s from version %s" % (name, self.version))

    # pages are double listed
    if self.version == 1:
      names = [n for n in self.pages.keys()]
      for name in names:
        if not name in self.nodes:
          del self.pages[name]
      self.version = 2

    # let's avoid all denormalization for now
    if self.version == 2:
      del self.reverse_edges
      del self.pages
      del self.datastores
      self.version = 3

    print("Migration: %s is now version %s" % (name, self.version))
