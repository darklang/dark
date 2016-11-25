from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
import sqlite3

import server

class DB(object):
  def __init__(self, name):
    self.conn = sqlite3.connect(':memory:', check_same_thread=False)
    self.name = name
    self.create_table(name)

  def exe(self, sql, *values):
    print(sql)
    self.conn.cursor().execute(sql, values)

  def create_table(self, name):
    self.exe("create table " + name + "(id)")

  def update(self, value, key):
    self.exe()

  def fetch(self, num):
    self.exe("select * from " + self.name + " limit " + str(num))

  def fetch_by_key(self, key, keyname):
    pass


class Datastore(object):

  def __init__(self, name):
    self.db = DB(name) # TODO single DB for multiple DSs
    self.name = name
    self.fields = {}
    self.primary = None

  def add_field(self, name, tyype, primary=False):
    self.fields[name] = tyype
    if primary:
      self.primary = name


  def validate_key(self, key):
    self.fields[self.primary].validate(key)

  def validate(self, value):
    for k, v in value.items():
      self.fields[k].validate(v)
    if len(value.items()) != len(self.fields):
      raise "this noise"


  def insert(self, value):
    self.validate(value)
    self.db.insert(value)

  def replace(key, value):
    self.validate_key(key)
    self.validate(value)
    self.db.update(key, value, self.key)

  def fetch(self, num=10):
    self.db.fetch(num)

  def fetch_one(self, key):
    self.validate_key(key)
    self.db.fetch_by_key(self.primary, key)[0]


class Dark(server.Server):

  def __init__(self):
    super(server.Server, self).__init__()
    self.url_map = Map()
    self.datastores = {}

  def add_datastore(self, name):
    ds = Datastore(name)
    self.datastores[name] = ds
    return ds

  def add_create(self, ds, route):

    def view(request, **values):
      # TODO: something with ds
      return self.render_template('create.html')

    def action(request, **values):
      new_value = request.values
      ds.validate(new_value)
      datastore.add(new_value)
      return Response()

    view_rule = Rule(route, methods=["GET"], endpoint=view)
    action_rule = Rule(route, methods=["POST"], endpoint=action)
    self.url_map.add(view_rule)
    self.url_map.add(action_rule)


  def add_list(self, ds, route):

    def view(request, **values):
      values = ds.fetch(10)
      return self.render_template('list.html')

    rule = Rule(route, methods=["GET"], endpoint=view)
    self.url_map.add(rule)



  def add_edit(self, ds, route):

    def view(request, **values):
      url = request.values["url"]
      value = request.values.by("url", url)
      return self.render_template('edit.html', value=value)

    def action(request, **values):
      url = values.url
      new_value = request.values
      ds.validate(new_value)
      datastore.replace(url, new_value)
      return Response()

    view_rule = Rule(route, methods=["GET"], endpoint=view)
    action_rule = Rule(route, methods=["PUT"], endpoint=action)
    self.url_map.add(view_rule)
    self.url_map.add(action_rule)


  def add_read(self, ds, route):
    def view(request, url):
      value = ds.find(url)
      return self.render_template('read.html', value=value)

    rule = Rule(route, methods=["GET"], endpoint=view)
    self.url_map.add(rule)
