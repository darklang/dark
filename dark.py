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

  def graph_json(self):
    nodes = [n.to_frontend() for n in self.nodes.values()]
    dses = [ds.to_frontend() for ds in self.datastores]
    return tojson({"nodes": nodes + dses})

  def add_api_routes(self):
    def fn(request):
      str_response = request.data.decode('utf-8')
      params = json.loads(str_response)
      ds = datastore.Datastore(params["name"])
      self.add_datastore(ds)
      return Response(response=self.graph_json())
    self.url_map.add(Rule('/admin/api/add_datastore', endpoint=fn))

  def add_graph_routes(self):
    def fn(request):
      nodes = [n.cytonode() for n in self.nodes.values()]
      dses = [ds.cytonode() for ds in self.datastores]
      return self.render_template('graph.html',
                                  nodes=tojson(nodes + dses),
                                  edges=tojson(self.edges),
                                  reverse_edges=self.reverse_edges)
    self.url_map.add(Rule('/admin/graph', endpoint=fn))

    def fn(request):
      nodes = [n.cytonode() for n in self.nodes.values()]
      dses = [ds.cytonode() for ds in self.datastores]
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
