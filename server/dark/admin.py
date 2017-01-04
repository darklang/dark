from . import graph

def migrate_all_graphs() -> None:
  # If you change module names, you'll need this
  # import sys
  # from . import node, datastore, fields
  # sys.modules['graph'] = graph
  # sys.modules['node'] = node
  # sys.modules['datastore'] = datastore
  # sys.modules['fields'] = fields
  for name in graph.get_all_graphnames():
    G = graph.load(name)
    print("Graph %s: v%s" % (G.name, G.version))
    graph.save(G)
