from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
import sqlite3

import server

class DB(object):
  def __init__(self, name):
    name = name.lower()
    self.name = name
    self.conn = sqlite3.connect(":memory:", check_same_thread=False)
    self.create_table(name)

  def exe(self, sql, *values):
    print(sql)
    try:
      return self.conn.execute(sql)
    except Error as e:
      print("error: " + e)

  def create_table(self, name):
    self.exe("create table " + name + "(id)")

  def update(self, value, key):
    raise "TODO"
    self.exe()

  def fetch(self, num):
    return self.exe("select * from " + self.name + " limit " + str(num)).fetchall()

  def fetch_by_key(self, key, keyname):
    return self.exe("select * from " + self.name + " where " + keyname + "=" + str(key) + " limit 1").fetchone()


class Datastore(object):

  def __init__(self, name):
    self.db = DB(name) # TODO single DB for multiple DSs
    self.name = name
    self.fields = {}
    self.derived = {}

  def add_field(self, f, derived=None):
    if derived:
      self.derived[f.name] = derived

    self.fields[f.name] = f

  def client_fields(self):
    return [ f for f in self.fields.values() if not f.client_facing() ]

  def validate_key(self, key_name, value):
    self.fields[key_name].validate(value)

  def validate(self, value):
    for k, v in value.items():
      self.fields[k].validate(v)
    if len(value.items()) != len(self.fields):
      raise "either missing field declaration or missing value"


  def insert(self, value):
    self.validate(value)
    self.db.insert(value)

  def replace(key, value):
    self.validate_key(key)
    self.validate(value)
    self.db.update(key, value, self.key)

  def fetch(self, num=10):
    return self.db.fetch(num)

  def fetch_one(self, key, key_name):
    self.db.fetch_by_key(key_name, key)


class Dark(server.Server):

  def __init__(self):
    super().__init__()
    self.url_map = Map()
    self.nodes = {}
    self.edges = {}
    self.reverse_edges = {}
    #self.add_standard_routes()

  def add_standard_routes(self):
    # TODO: move to a component
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def generate_name(self):
    import random
    hash = random.getrandbits(128)
    return ("%032x" % hash)

  def add(self, node):
    if not hasattr(node, "name"):
      name = self.generate_name()
      node.name = name
    self.nodes[node.name] = node
    return node

  def edge_from(self, n1, n2):
    self.add(n1)
    self.add(n2)
    if n1.name not in self.edges:
      self.edges[n1.name] = []
    self.edges[n1.name].append(n2.name)

    if n2.name not in self.reverse_edges:
      self.reverse_edges[n2.name] = []
    self.reverse_edges[n2.name].append(n1.name)

  def get_parents(self, node):
    parents = self.reverse_edges[node.name] or []
    return [self.nodes[p] for p in parents]


  def execute(self, node):
    print("calling node: %s (%s)" % (node.name, type(node)))
    parents = self.get_parents(node)
    args = []
    for p in parents:
      if hasattr(p, "exe"):
        arg = self.execute(p)
      else:
        arg = p
      args.append(arg)
    res = node.exe(args)
    print("%s (%s) returns %s" % (node.name, type(node), str(res)))
    return clone(res)


  def add_output(self, node, verb, url):
    self.add(node)
    def h(request):
      return self.execute(node)
    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))

  def add_input(self, node, verb, url):
    self.add(node)
    def h(request):
      return self.execute(node)
    self.url_map.add(Rule(url,
                          endpoint=h,
                          methods=[verb]))
