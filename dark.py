from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import pickle
import os.path
import traceback

import fields
import graph
import server
import datastore

class Dark(server.Server):
  def __init__(self):
    super().__init__()
    self.url_map = Map()

    #self.add_standard_routes()
    self.graph = graph.Graph()
    if os.path.isfile("dark.graph"):
      self.graph = pickle.load(open( "dark.graph", "rb"))

    self.add_ui_route()
    self.add_api_route()

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
      fieldFn = getattr(fields, typename)
      ds.add_field(fieldFn(fieldname))

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
      nodeid = args["id"]
      node = G.get_node(nodeid)
      node.x = args["x"]
      node.y = args["y"]

    elif command == "add_edge":
      src = G.get_node(args["src"])
      target = G.get_node(args["target"])
      paramname = args["param"]
      G.add_edge(src, target, paramname)

    elif command == "delete_node":
      node = G.get_node(args["id"])
      G.delete_node(node)

    elif command == "clear_edges":
      node = G.get_node(args["id"])
      G.clear_edges(node)

    elif command == "remove_last_field":
      node = G.get_node(args["id"])
      if node.__class__.__name__ == "Datastore":
        node.remove_last_field()
      else:
        node.remove_last_edge()

    elif command == "load_initial_graph":
      pass

    else:
      raise Exception("Invalid command: " + str(request.data))

    return cursor


  def add_api_route(self):
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

  def add_output(self, node, verb, url):
    self.graph._add(node)
    assert node.is_datasink()

    def h(request):
      val = self.graph.run_output(node)
      return Response(val, mimetype='text/html')

    self.url_map.add(Rule(url, endpoint=h, methods=[verb]))

  def add_input(self, node, verb, url, redirect_url):
    self.graph._add(node)
    assert node.is_datasource()

    def h(request):
      self.graph.run_input(node, request.values.to_dict())
      return redirect(redirect_url)

    self.url_map.add(Rule(url, endpoint=h, methods=[verb]))
    return node
