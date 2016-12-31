from werkzeug.wrappers import Response, Request
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import pickle
import os.path
import traceback
import re
import sys

from typing import Optional

import fields
import graph
from node import Node
import server
import datastore


class Dark(server.Server):
  def __init__(self) -> None:
    super().__init__()
    self.init_url_map()

  def handler(self, G : graph.Graph, request : Request) -> Optional[Node]:
    str_req = request.data().decode('utf-8')
    params = json.loads(str_req)
    print("Requesting: " + str(params))

    command = params["command"]
    args = params["args"]

    cursor = None # type: Optional[Node]

    if command == "add_datastore":
      name = args["name"]
      ds = datastore.Datastore(name)
      ds.x = args["x"]
      ds.y = args["y"]
      G.add_datastore(ds)
      cursor = ds

    elif command == "add_datastore_field":
      ds = G.datastores[args["id"]]
      fieldname = args["name"]
      typename = args["type"]
      is_list = False
      if typename[0] == "[" and typename[-1] == "]":
        is_list = True
        typename = typename[1:-1]
      fieldFn = getattr(fields, typename, None)
      if fieldFn:
        ds.add_field(fieldFn(fieldname, is_list=is_list))
      else:
        ds.add_field(fields.Foreign(fieldname, typename, is_list=is_list))

    elif command == "add_function_call":
      nodename = args["name"]
      node = node.FnNode(nodename)
      node.x = args["x"]
      node.y = args["y"]
      G._add(node)
      cursor = node

    elif command == "add_value":
      valuestr = args["value"]
      node = node.Value(valuestr)
      node.x = args["x"]
      node.y = args["y"]
      G._add(node)
      cursor = node

    elif command == "update_node_position":
      node = G.nodes[args["id"]]
      node.x = args["x"]
      node.y = args["y"]

    elif command == "add_edge":
      src = G.nodes[args["src"]]
      target = G.nodes[args["target"]]
      paramname = args["param"]
      G.add_edge(src, target, paramname)

    elif command == "delete_node":
      node = G.nodes[args["id"]]
      G.delete_node(node)

    elif command == "clear_edges":
      node = G.nodes[args["id"]]
      G.clear_edges(node)

    elif command == "remove_last_field":
      node = G.nodes[args["id"]]
      if node.__class__.__name__ == "Datastore":
        node.remove_last_field()
      else:
        node.remove_last_edge()

    elif command == "load_initial_graph":
      pass

    else:
      raise Exception("Invalid command: " + str(request.data))

    return cursor


  def init_url_map(self) -> None:
    self.url_map = Map()
    self.add_ui_route()
    self.add_standard_routes()
    self.add_admin_route()
    self.add_app_route()

  def add_admin_route(self) -> None:
    def endpoint(request : Request) -> Response:
      try:
        name = self.subdomain(request)

        G = graph.load(name)
        cursor = self.handler(G, request)
        graph.save(G)

        print("Responding: \n" + G.to_debug(cursor))

        return Response(response=G.to_frontend(cursor))

      except BaseException as e:
        traceback.print_exc()
        eClass = e.__class__.__name__
        stack = traceback.extract_tb(e.__traceback__)
        frame = stack[-1]
        eFile = frame.filename # type: ignore
        eFile = os.path.basename(eFile)
        eLine = frame.lineno # type: ignore
        eFunc = frame.name # type: ignore
        eMsg = str(e)
        msg = "%s:%s:%s() %s: %s" % (eFile, eLine, eFunc, eClass, eMsg)

        return Response(status=500, response=msg)

    self.url_map.add(Rule('/admin/api/rpc', endpoint=endpoint))

  def add_ui_route(self) -> None:
    def fn(request : Request) -> Response:
      return self.render_template('ui.html')
    self.url_map.add(Rule('/admin/ui', endpoint=fn))

  def add_standard_routes(self) -> None:
    # TODO: move to a component
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def add_app_route(self) -> None:
    def dispatcher(request, path=""):
      name = self.subdomain(request)
      path = request.path
      values = request.values.to_dict()
      G = graph.load(name)

      for p in G.pages.values():
        urls = G.get_named_parents(p, "url")
        urls = [u.exe() for u in urls]
        for url in urls:
          if re.compile("^%s$" % url).match(path):
            if request.method == "GET":
              val = G.run_output(p)
              return Response(val, mimetype='text/html')
            if request.method == "POST":
              G.run_input(p, request.values.to_dict())
              return redirect("/")

      return Response(status=404)

    self.url_map.add(Rule('/<path:path>', endpoint=dispatcher))
    # not matched above for some reason
    self.url_map.add(Rule('/', endpoint=dispatcher))

  def subdomain(self, request : Request) -> str:
    return request.host().split('.')[0]

  @staticmethod
  def migrate_all_graphs() -> None:
    for name in graph.get_all_graphnames():
      G = graph.load(name)
      print("Graph %s: v%s" % (G.name, G.version))
      graph.save(G)


if __name__ == "__main__":
  if len(sys.argv) > 1 and sys.argv[1] == "--migrate":
    Dark.migrate_all_graphs()
    sys.exit(0)
  else:
    d = Dark()
    d.serve()
