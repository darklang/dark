from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import pickle
import os.path
import traceback
import sys

import fields
import graph
import server
import datastore

class Dark(server.Server):
  def __init__(self):
    super().__init__()

    self.graph = graph.Graph()
    if os.path.isfile("dark.graph"):
      self.graph = pickle.load(open( "dark.graph", "rb"))

    self.init_url_map()

  def handler(self, request):
    str_response = request.data.decode('utf-8')
    params = json.loads(str_response)
    print("Requesting: " + str(params))

    command = params["command"]
    args = params["args"]

    G = self.graph
    cursor = None

    if command == "add_datastore":
      name = args["name"]
      node = datastore.Datastore(name)
      node.x = args["x"]
      node.y = args["y"]
      G.add_datastore(node)
      cursor = node

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
      node = graph.Node(nodename)
      node.x = args["x"]
      node.y = args["y"]
      G._add(node)
      cursor = node

    elif command == "add_value":
      valuestr = args["value"]
      node = graph.Value(valuestr)
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

    self.init_url_map()
    return cursor


  def add_admin_route(self):
    def endpoint(request):
      try:
        cursor = self.handler(request)
        response = self.graph.to_frontend(cursor)
        print("Responding: " + str(response))

        # Roundtrip so we find bugs early
        pickle.dump(self.graph, open( "dark.graph", "wb" ))
        self.graph = pickle.load(open( "dark.graph", "rb"))

        return Response(response=response)

      except BaseException as e:
        traceback.print_exc()
        eClass = e.__class__.__name__
        stack = traceback.extract_tb(e.__traceback__)
        frame = stack[-1]
        eFile = frame.filename
        eFile = os.path.basename(eFile)
        eLine = frame.lineno
        eFunc = frame.name
        eMsg = str(e)
        msg = "%s:%s:%s() %s: %s" % (eFile, eLine, eFunc, eClass, eMsg)

        return Response(status=500, response=msg)

    self.url_map.add(Rule('/admin/api/rpc', endpoint=endpoint))

  def add_ui_route(self):
    def fn(request):
      return self.render_template('ui.html')
    self.url_map.add(Rule('/admin/ui', endpoint=fn))

  def add_standard_routes(self):
    # TODO: move to a component
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def init_url_map(self):
    map = Map()
    self.url_map = map
    self.add_ui_route()
    self.add_standard_routes()
    self.add_admin_route()
    self.add_app_routes()

  def add_app_routes(self):
    for p in self.graph.pages.values():
      print("adding page")
      urls = self.graph.get_named_parents(p, "url")
      urls = [u.exe() for u in urls]
      outputs = self.graph.get_named_parents(p, "outputs")
      inputs = self.graph.get_named_parents(p, "inputs")
      print("url, input, output:")
      print(urls)
      print(inputs)
      print(outputs)
      for url in urls:
        if len(outputs) > 0:
          self.set_output(p, url)
        if len(inputs) > 0:
          self.set_input(p, url)

  def set_output(self, node, url):
    print("Setting output: %s, %s" % (node, url))
    def h(request):
      val = self.graph.run_output(node)
      return Response(val, mimetype='text/html')
    self.url_map.add(Rule(url, endpoint=h, methods=["GET"]))

  def set_input(self, node, url):
    print("Setting input: %s, %s" % (node, url))
    def h(request):
      self.graph.run_input(node, request.values.to_dict())
      return redirect("/")
    self.url_map.add(Rule(url, endpoint=h, methods=["POST"]))
