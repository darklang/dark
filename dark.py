from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import pickle
import os.path

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

    self.add_graph_routes()
    self.add_api_routes()

  def add_api_routes(self):
    def fn(request):
      str_response = request.data.decode('utf-8')
      params = json.loads(str_response)
      print("Requesting: " + str(params))

      cursorname = params["cursor"]
      command = params["command"]
      args = params["args"]

      if command == "add_datastore":
        name = args["name"]
        cursor = datastore.Datastore(name)
        cursor.x = args["x"]
        cursor.y = args["y"]
        self.graph.add_datastore(cursor)

      elif command == "add_datastore_field":
        ds = self.graph.datastores[cursorname]
        fieldname = args["name"]
        typename = args["type"]
        fieldFn = getattr(fields, typename)
        ds.add_field(fieldFn(fieldname))
        cursor = ds

      elif command == "add_function_call":
        nodename = args["name"]
        cursor = graph.Node(nodename)
        cursor.x = args["x"]
        cursor.y = args["y"]
        self.graph._add(cursor)

      elif command == "load_initial_graph":
        cursor = None

      else:
        raise Exception("Invalid command: " + str(request.data))


      response = self.graph.to_frontend(cursor)
      print("Responding: " + str(response))
      pickle.dump(self.graph, open( "dark.graph", "wb" ))
      return Response(response=response)


    self.url_map.add(Rule('/admin/api/rpc', endpoint=fn))

  def add_graph_routes(self):
    def fn(request):
      nodes = [n.cytonode() for n in self.graph.nodes.values()]
      dses = [ds.cytonode() for ds in self.graph.datastores.values()]
      return self.render_template('graph.html',
                                  nodes=tojson(nodes + dses),
                                  edges=tojson(self.graph.edges),
                                  reverse_edges=self.graph.reverse_edges)
    self.url_map.add(Rule('/admin/graph', endpoint=fn))

    def fn(request):
      return self.render_template('graphelm.html')
    self.url_map.add(Rule('/admin/graph2', endpoint=fn))

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
