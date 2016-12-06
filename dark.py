from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.utils import redirect

import json
import copy
import inspect
import random

import termcolor
import pyrsistent as pyr
from util import *

import server

class node:
  def __init__(meta,
               datasource=False,
               datasink=False,
               numinputs=None,
               fields=[]):
    assert isinstance(fields, list)
    meta.fields = fields
    meta.numinputs = numinputs
    meta.datasource = datasource
    meta.datasink = datasink

  def __call__(meta, func):
    class Node:
      def __init__(self, *args):
        assert len(args) == len(meta.fields)
        self.args = args
        self.id = random.randint(0, 2**32)

      def is_datasource(self): return meta.datasource
      def is_datasink(self): return meta.datasink

      def __str__(self):
        return self.name()

      def __repr__(self):
        return "<%s>" % self.name()

      def cytonode(self):
        result = {"id": self.name()}
        if "ds" in meta.fields:
          for i, f in enumerate(meta.fields):
            if f == "ds":
              result["parent"] = self.args[i].name()
        return result


      def name(self):
        return "%s-%04X" % (func.__name__, self.id % 2**16)

      def exe(self, *inputs):
        # error checking
        (args, varargs, _, _) = inspect.getargspec(func)
        numinputs = len(args)
        if len(meta.fields): numinputs -= 1
        if varargs == None:
          assert numinputs == len(inputs)

        d = attrdict()
        for i, f in enumerate(meta.fields):
          d[f] = self.args[i]
        if len(d) > 0:

          return func(d, *inputs)
        else:
          return func(*inputs)

    return Node


class Dark(server.Server):

  def __init__(self):
    super().__init__()
    self.url_map = Map()
    self.nodes = {}
    self.edges = {}
    self.datastores = []
    self.reverse_edges = {}

    #self.add_standard_routes()
    self.add_admin_routes()

  def add_admin_routes(self):
    def showgraph(request):
      nodes = [n.cytonode() for n in self.nodes.values()]
      dses = [ds.cytonode() for ds in self.datastores]
      return self.render_template('graph.html',
                                  nodes=tojson(nodes + dses),
                                  edges=tojson(self.edges),
                                  reverse_edges=self.reverse_edges)

    self.url_map.add(Rule('/admin/graph', endpoint=showgraph))

  def add_standard_routes(self):
    # TODO: move to a component
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def _add(self, node):
    self.nodes[node.name()] = node

  def add_datastore(self, ds):
    self.datastores.append(ds)

  def has(self, node):
    return node.name() in self.nodes

  def add_edge(self, n1, n2):
    self._add(n1)
    self._add(n2)

    if n1.name() not in self.edges:
      self.edges[n1.name()] = []
    self.edges[n1.name()].append(n2.name())

    if n2.name() not in self.reverse_edges:
      self.reverse_edges[n2.name()] = []
    self.reverse_edges[n2.name()].append(n1.name())

    return (n1, n2)

  def print_graph(self):
    print(self.nodes)
    print(self.edges)
    print(self.reverse_edges)

  def get_children(self, node):
    if node.is_datasink(): return []

    children = self.edges[node.name()] or []
    return [self.nodes[c] for c in children]

  def get_parents(self, node):
    if node.is_datasource(): return []

    parents = self.reverse_edges.get(node.name(), [])
    return [self.nodes[p] for p in parents]


  def execute(self, node, only=None, eager={}):
    # print("executing node: %s" % (node))
    if node in eager:
      result = eager[node]
    else:
      args = [self.execute(p, eager=eager)
              for p in self.get_parents(node)
              if only in [None, p]]
      result = node.exe(*args)

    return pyr.freeze(result)

  def find_sink_edges(self, node):
    results = set()
    for c in self.get_children(node):
      if c.is_datasink():
        results |= {(node, c)}
      else:
        results |= self.find_sink_edges(c)
    return results

  def run_input(self, node, val):
    for (parent, sink) in self.find_sink_edges(node):
      # print("run_input on sink,parent: %s, %s" %(sink, parent))
      self.execute(sink, only=parent, eager={node: val})

  def run_output(self, node):
    # print("run_output on node: %s" % (node))
    return self.execute(node)

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
