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

  def add_field(self, f):
    self.fields[f.name] = f

  def set_derived(self, ffrom, to):
    pass

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
    self.datastores = {}
    self.add_standard_routes()

  def add_standard_routes(self):
    def favico(*v): return Response()
    def sitemap(*v): return Response()
    self.url_map.add(Rule('/favicon.ico', endpoint=favico))
    self.url_map.add(Rule('/sitemap.xml', endpoint=sitemap))

  def add_datastore(self, ds):
    self.datastores[ds.name] = ds
    return ds

  def add(self, node):
    for r in node.rules:
      self.url_map.add(r)
    node.server = self
    return node
