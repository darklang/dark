import random
import json
import os
import pickle
import enum

import pyrsistent as pyr

from dark.node import Node, FnNode, Value, ID
from . import datastore
from . import fields
from .util import req

from typing import Any, Callable, cast, Tuple, List, Dict, Optional, Set, NamedTuple

def debug(str:str) -> None:
  print(str.replace("\n", "\n  "))

def get_all_graphnames() -> List[str]:
  return [f[:-5] for f in os.listdir("appdata") if f.endswith(".dark")]

def filename_for(name:str) -> str:
  return "appdata/" + name + ".dark"

def load(name:str) -> 'Graph':
  filename = filename_for(name)
  if os.path.isfile(filename):
    with open(filename, 'rb') as file:
      graph = pickle.load(file)
    graph.migrate(name)
    return graph
  else:
    return Graph(name)

def save(G:'Graph') -> None:
  filename = filename_for(G.name)

  # dont use pickle.dump - it tends to corrupt
  data = pickle.dumps(G)

  with open(filename, 'wb') as file:
    file.write(data)

  # sanity check
  load(G.name)

class Op(NamedTuple):
  op : str
  args : Tuple[Any, ...]

class Graph:
  def __init__(self, name:str) -> None:
    self.name = name
    self.version = 5
    self.ops : List[Op] = []
    self.nodes : Dict[ID,Node] = {}
    # first Tuple arg is n2.id, 2nd one is param
    self.edges : Dict[ID, List[Tuple[ID,str]]] = {}


  # Pickling
  def __getstate__(self) -> Dict[str,Any]:
    return {"name": self.name,
            "version": self.version,
            "ops": self.ops}

  # Unpickling
  def __setstate__(self, state: Dict[str,Any]) -> None:
    self.name = state["name"]
    self.version = state["version"]
    self.ops = state["ops"]
    self.nodes = {}
    self.edges = {}
    for op in self.ops:
      self.apply_op(op)

  def __getattr__(self, name:str) -> Any:
    # pickling is weird with getattr
    if name.startswith('__') and name.endswith('__'):
      return super().__getattr__(name) # type: ignore

    # dynamically build useful constructs
    if name == "reverse_edges":
      result : Dict[str, List[str]] = {}
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


    # generalize the operations
    if name in ["add_fn", "add_datastore", "add_value",
                "add_datastore_field", "update_node_position",
                "delete_node", "add_edge",
                "delete_edge", "clear_edges"]:
      def fn(*args : Any) -> Optional[ID]:
        print("args are" + str(args))
        id = None
        print("name is: " + name)
        if name in ["add_fn", "add_datastore", "add_value"]:
          id = random.randint(0, 2**32)
          args += (id,)
        op = Op(name, args)
        return self.add_op(op)
      return fn

    return None

  def _add(self, node:Node) -> None:
    self.nodes[node.id()] = node
    if node.id() not in self.edges:
      self.edges[node.id()] = []

  def has(self, node:Node) -> bool:
    return node.id() in self.nodes


  #############
  # operations
  #############
  def apply_op(self, op:Op) -> Optional[ID]:
    fn = getattr(self, "_" + op.op)
    return fn(*op.args)

  def add_op(self, op:Op) -> Optional[ID]:
    self.ops.append(op)
    return self.apply_op(op)

  def _add_fn(self, name:str, x:int, y:int, id:int) -> ID:
    fn = FnNode(name, x, y, id)
    self._add(fn)
    return fn.id()

  def _add_value(self, valuestr: str, x:int, y:int, id:int) -> ID:
    v = Value(valuestr, x, y, id)
    self._add(v)
    return v.id()

  def _add_datastore(self, name:ID, x:int, y:int) -> ID:
    ds = datastore.Datastore(name, x, y)
    self._add(ds)
    return ds.id()

  def _add_datastore_field(self, id:ID, fieldname:str, typename:str, is_list:bool) -> None:
    ds = self.datastores[id]
    fieldFn = getattr(fields, typename, None)
    if fieldFn:
      ds.add_field(fieldFn(fieldname, is_list=is_list))
    else:
      ds.add_field(fields.Foreign(fieldname, typename, is_list=is_list))

  def _update_node_position(self, id:ID, x:int, y:int) -> None:
    n = self.nodes[id]
    n.x, n.y = x, y

  def _delete_node(self, id:ID) -> None:
    node = self.nodes[id]
    self.clear_edges(id)
    del self.nodes[node.id()]

  def _add_edge(self, src_id:ID, target_id:ID, param:str) -> None:
    src = self.nodes[src_id]
    target = self.nodes[target_id]
    self.edges[src.id()].append((target.id(), param))

  def _delete_edge(self, src_id:ID, target_id:ID, param:str) -> None:
    E = self.edges
    ts = E[src_id]
    E[src_id] = [(t,p) for (t, p) in ts
                 if t != target_id and p != param]


  def _clear_edges(self, id:ID) -> None:
    """As we develop, sometimes graphs get weird. So we actually check the whole
    graph to fix it up, not just doing what we expect to find."""
    E = self.edges
    for s, ts in E.items():
      for t,p in ts:
        if t == id:
          self._delete_edge(s,id,p)

    for (t,p) in E[id]:
      self._delete_edge(id, t, p)

  #############
  # execution
  #############
  def get_children(self, node:Node) -> Dict[str, Node]:
    children = self.edges[node.id()] or []
    return {param: self.nodes[c] for (c, param) in children}

  def get_parents(self, node:Node) -> Dict[str, Node]:
    parents = self.reverse_edges.get(node.id(), [])
    result : List[Tuple[str, Node]] = []
    for p in parents:
      t = self.nodes[p]
      paramname = self.get_target_param_name(node, t)
      result += [(paramname, t)]
    return {paramname: t for (paramname, t) in result}

  def get_named_parents(self, node:Node, paramname:str) -> List[Node]:
    return [v for k,v in self.get_parents(node).items()
            if paramname == k]

  def get_target_param_name(self, target:Node, src:Node) -> str:
    for (c, p) in self.edges[src.id()]:
      if c == target.id():
        return p
    assert(False and "Shouldnt happen")

  def execute(self, node:Node, only:Node = None, eager:Dict[Node, Any] = {}) -> Any:
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

  def find_sink_edges(self, node:Node) -> Set[Tuple[Node, Node]]:
    # debug("finding sink edges: %s" % (node))
    results : Set[Tuple[Node, Node]] = set()
    for _, c in self.get_children(node).items():
      if c.is_datasink():
        results |= {(node, c)}
      else:
        results |= self.find_sink_edges(c)
    return results

  def run_input(self, node:Node, val:Any) -> None:
    # debug("running input: %s (%s)" % (node, val))
    for (parent, sink) in self.find_sink_edges(node):
      # debug("run_input on sink,parent: %s, %s" %(sink, parent))
      self.execute(sink, only=parent, eager={node: val})

  def run_output(self, node:Node) -> Any:
    # print("run_output on node: %s" % (node))
    return self.execute(node)


  #############
  # output and debugging
  #############
  def to_frontend_edges(self) -> List[Dict[str, str]]:
    result = []
    for s in self.edges.keys():
      for (t,p) in self.edges[s]:
        result.append({"source": s, "target": t, "paramname": p})
    return result

  def to_frontend(self, cursor_id:Optional[ID]) -> str:
    nodes = {n.id(): n.to_frontend() for n in self.nodes.values()}
    edges = self.to_frontend_edges()

    result = {"nodes": nodes, "edges": edges}
    if cursor_id:
      result["cursor"] = cursor_id
    return json.dumps(result, sort_keys=True, indent=2)

  def to_debug(self, cursor:Optional[ID]) -> str:
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

  #############
  # Migration
  #############
  def migrate(self, name:str) -> None:
    # 1. no name and no version
    # 2. pages are double listed
    # 3. remove some fields to avoid denormalization
    # 4. changed a Node into a FnNode
    # 5. create ops array
    # TODO: move existing structure into ops
    pass
