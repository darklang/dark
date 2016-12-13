from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import graph
import server
import datastore

from util import (tojson)

class Dark(graph.Graph, server.Server):
  def __init__(self):
    super().__init__()
    self.url_map = Map()

    #self.add_standard_routes()
    self.add_graph_routes()
    self.add_api_routes()

  def nodes_as_json(self):
    nodes = []
    return tojson(nodes)

  def graph_json(self, cursor):
    nodes = [n.to_frontend() for n in self.nodes.values()]
    dses = [ds.to_frontend() for ds in self.datastores.values()]
    return tojson({"nodes": nodes + dses, "cursor": cursor.id()})

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
        self.add_datastore(cursor)

      elif command == "add_datastore_field":
        ds = self.datastores[cursorname]
        fieldname = args["name"]
        typename = args["type"]
        import fields
        fieldFn = getattr(fields, typename)
        ds.add_field(fieldFn(fieldname))
        cursor = ds

      else:
        raise Exception("Invalid command: " + str(request.data))


      response = self.graph_json(cursor)
      print("Responding: " + str(response))
      return Response(response=response)


    self.url_map.add(Rule('/admin/api/rpc', endpoint=fn))

  def add_graph_routes(self):
    def fn(request):
      nodes = [n.cytonode() for n in self.nodes.values()]
      dses = [ds.cytonode() for ds in self.datastores.values()]
      return self.render_template('graph.html',
                                  nodes=tojson(nodes + dses),
                                  edges=tojson(self.edges),
                                  reverse_edges=self.reverse_edges)
    self.url_map.add(Rule('/admin/graph', endpoint=fn))

    def fn(request):
      nodes = [n.cytonode() for n in self.nodes.values()]
      dses = [ds.cytonode() for ds in self.datastores.values()]
      return self.render_template('graphelm.html',
                                  nodes=tojson(nodes + dses),
                                  edges=tojson(self.edges),
                                  reverse_edges=self.reverse_edges)
    self.url_map.add(Rule('/admin/graph2', endpoint=fn))

  def add_standard_routes(self):
    # TODO: move to a component
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def add_output(self, node, verb, url):
    self._add(node)
    assert node.is_datasink()

    def h(request):
      val = self.run_output(node)
      return Response(val, mimetype='text/html')

    self.url_map.add(Rule(url, endpoint=h, methods=[verb]))

  def add_input(self, node, verb, url, redirect_url):
    self._add(node)
    assert node.is_datasource()

    def h(request):
      self.run_input(node, request.values.to_dict())
      return redirect(redirect_url)

    self.url_map.add(Rule(url, endpoint=h, methods=[verb]))
    return node
