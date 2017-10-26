class Graph:

  def __getattr__(self, name:str) -> Any:
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


  def _add_datastore_field(self, id:ID, fieldname:str, typename:str, is_list:bool) -> None:
    ds = self.datastores[id]
    fieldFn = getattr(fields, typename, None)
    if fieldFn:
      ds.add_field(fieldFn(fieldname, is_list=is_list))
    else:
      ds.add_field(fields.Foreign(fieldname, typename, is_list=is_list))


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


