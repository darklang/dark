import json
import os
import pickle

import pyrsistent as pyr

from node import Node
import datastore

from typing import Any, Callable, cast, Tuple, List, Dict, Optional, Set

def debug(str : str) -> None:
  print(str.replace("\n", "\n  "))

def get_all_graphnames() -> List[str]:
  return [f[:-5] for f in os.listdir("appdata") if f.endswith(".dark")]

def filename_for(name : str) -> str:
  return "appdata/" + name + ".dark"

def load(name : str) -> 'Graph':
  filename = filename_for(name)
  with open(filename, 'rb') as file:
    graph = pickle.load(file)
  graph.migrate(name)
  return graph

def save(G : 'Graph') -> None:
  filename = filename_for(G.name)

  # dont use pickle.dump - it tends to corrupt
  data = pickle.dumps(G)

  with open(filename, 'wb') as file:
    file.write(data)

  # sanity check
  load(G.name)


class Graph:
  def __init__(self, name : str) -> None:
    self.name = name
    self.nodes = {} # type: Dict[str, Node]
    # first Tuple arg is n2.id, 2nd one is param
    self.edges = {} # type: Dict[str, List[Tuple[str, str]]]

  def __getattr__(self, name : str) -> Any:
    # pickling is weird with getattr
    if name.startswith('__') and name.endswith('__'):
      return super().__getattr__(name) # type: ignore

    if name == "reverse_edges":
      result = {} # type: Dict[str, List[str]]
      for k in self.nodes.keys():
        result[k] = []
      for s,ts in self.edges.items():
        for (t,_) in ts:
          result[t].append(s)
      return result

    if name == "pages":
      return {k: v for k,v in self.nodes.items() if v.is_page()}

    if name == "datastores":
      return {k: v for k,v in self.nodes.items() if isinstance(v, datastore.Datastore)}

    return None

  def _add(self, node : Node) -> None:
    self.nodes[node.id()] = node
    if node.id() not in self.edges:
      self.edges[node.id()] = []

  def add_datastore(self, ds : datastore.Datastore) -> None:
    self.nodes[ds.id()] = ds

  def has(self, node : Node) -> bool:
    return node.id() in self.nodes

  def delete_node(self, node : Node) -> None:
    self.clear_edges(node)
    del self.nodes[node.id()]
    if node.id() in self.nodes:
      del self.nodes[node.id()]

  def add_edge(self, n1 : Node, n2 : Node, n2param : str) -> None:
    self.edges[n1.id()].append((n2.id(), n2param))

  def clear_edges(self, node : Node) -> None:
    """As we develop, sometimes graphs get weird. So we actually check the whole
    graph to fix it up, not just doing what we expect to find."""
    E = self.edges
    id = node.id()
    for s, ts in E.items():
      E[s] = [(t,p) for (t, p) in ts if t != id]
    if id in E:
      del E[id]

  def get_children(self, node : Node) -> Dict[str, Node]:
    children = self.edges[node.id()] or []
    return {param: self.nodes[c] for (c, param) in children}

  def get_parents(self, node : Node) -> Dict[str, Node]:
    parents = self.reverse_edges.get(node.id(), [])
    result = [] # type: List[Tuple[str, Node]]
    for p in parents:
      t = self.nodes[p]
      paramname = self.get_target_param_name(node, t)
      result += [(paramname, t)]
    return {paramname: t for (paramname, t) in result}

  def get_named_parents(self, node : Node, paramname : str) -> List[Node]:
    return [v for k,v in self.get_parents(node).items()
            if paramname == k]

  def get_target_param_name(self, target : Node, src : Node) -> str:
    for (c, p) in self.edges[src.id()]:
      if c == target.id():
        return p
    assert(False and "Shouldnt happen")

  def execute(self, node : Node, only : Node = None, eager : Dict[Node, Any] = {}) -> Any:
    # debug("executing node: %s, with only=%s and eager=%s" % (node, only, eager))
    if node in eager:
      result = eager[node]
    else:
      args = {}
      for paramname, p in self.get_parents(node).items():
        # make sure we don't traverse beyond datasinks (see find_sink_edges)
        if only in [None, p]:
          # make sure we don't traverse beyond datasources
          new_only = p if p.is_datasource() else None

          args[paramname] = self.execute(p, eager=eager, only=new_only)

      result = node.exe(**args)

    return pyr.freeze(result)

  def find_sink_edges(self, node : Node) -> Set[Tuple[Node, Node]]:
    # debug("finding sink edges: %s" % (node))
    results = set() # type: Set[Tuple[Node, Node]]
    for _, c in self.get_children(node).items():
      if c.is_datasink():
        results |= {(node, c)}
      else:
        results |= self.find_sink_edges(c)
    return results

  def run_input(self, node : Node, val : Any) -> None:
    # debug("running input: %s (%s)" % (node, val))
    for (parent, sink) in self.find_sink_edges(node):
      # debug("run_input on sink,parent: %s, %s" %(sink, parent))
      self.execute(sink, only=parent, eager={node: val})

  def run_output(self, node : Node) -> Any:
    # print("run_output on node: %s" % (node))
    return self.execute(node)

  def to_frontend_edges(self) -> List[Dict[str, str]]:
    result = []
    for s in self.edges.keys():
      for (t,p) in self.edges[s]:
        result.append({"source": s, "target": t, "paramname": p})
    return result

  def to_frontend(self, cursor : Optional[Node]) -> str:
    nodes = {n.id(): n.to_frontend() for n in self.nodes.values()}
    edges = self.to_frontend_edges()

    result = {"nodes": nodes, "edges": edges}
    if cursor :
      result["cursor"] = cursor.id()
    return json.dumps(result, sort_keys=True, indent=2)

  def to_debug(self, cursor : Optional[Node]) -> str:
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

  def migrate(self, name : str) -> None:
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

    # we made a Node into a FnNode
    if self.version == 3:
      ns = [n for n in self.nodes.values()]
      for n in ns:
        if hasattr(n, 'fnname'):
          new = FnNode(n.fnname) # type: ignore
          new.x = n.x
          new.y = n.y
          new._id = n._id # type: ignore
          self.nodes[new.id()] = new
      self.version = 4

    print("Migration: %s is now version %s" % (name, self.version))
