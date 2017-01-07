from werkzeug.wrappers import Response, Request
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import pickle
import os.path
import traceback
import re
import sys

from typing import Optional, Any

from . import fields
from . import graph
from .node import Node
from . import appserver
from . import datastore
from .util import pluck

class Server(appserver.AppServer):
  def __init__(self) -> None:
    super().__init__()
    self.init_url_map()

  def handler(self, G : graph.Graph, request : Request) -> Optional[Node]:
    str_req = request.data.decode('utf-8')
    params = json.loads(str_req)
    print("Requesting: " + str(params))

    command = params["command"]
    args = params["args"]

    cursor : Optional[Node] = None

    if command == "add_datastore":
      name, x, y = pluck(args, "name", "x", "y")
      cursor = G.add_datastore(name, x, y)

    elif command == "add_datastore_field":
      id_, fieldname, typename = pluck(args, "id", "name", "type")
      is_list = False
      if typename[0] == "[" and typename[-1] == "]":
        is_list = True
        typename = typename[1:-1]
      G.add_datastore_field(id_, fieldname, typename, is_list)

    elif command == "add_function_call":
      name, x, y = pluck(args, "name", "x", "y")
      cursor = G.add_fnnode(name, x, y)

    elif command == "add_value":
      valuestr, x, y = pluck(args, "value", "x", "y")
      cursor = G.add_value(valuestr, x, y)

    elif command == "update_node_position":
      id_, x, y = pluck(args, "id", "x", "y")
      G.update_node_position(id_, x, y)

    elif command == "add_edge":
      src, target, param = pluck(args, "src", "target", "param")
      G.add_edge(src, target, param)

    elif command == "delete_node":
      G.delete_node(args["id"])

    elif command == "clear_edges":
      G.clear_edges(args["id"])

    elif command == "remove_last_field":
      raise Exception("TODO")
      node = G.nodes[args["id"]]
      # if node.__class__.__name__ == "Datastore":
        # node.remove_last_field()
      # else:
        # node.remove_last_edge()

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
    def favico(*v:Any) -> Response: return Response()
    def sitemap(*v:Any) -> Response: return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def add_app_route(self) -> None:
    def dispatcher(request : Request, path : str ="") -> Response:
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
    return request.host.split('.')[0]
