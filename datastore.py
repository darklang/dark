import sqlite3

import dark
import fields

class DB(object):
  def __init__(self, tablename):
    tablename = tablename.lower()
    self.tablename = tablename
    self.conn = sqlite3.connect(":memory:", check_same_thread=False)
    self.create_table()

  def exe(self, sql, *values):
    print(sql)
    try:
      return self.conn.execute(sql)
    except BaseException as e:
      print("error: " + str(e))

  def create_table(self):
    self.exe("create table " + self.tablename + "(id INTEGER PRIMARY KEY AUTOINCREMENT) ")

  def add_column(self, field):
    self.exe("ALTER TABLE %s ADD %s" % (self.tablename, field))

  def insert(self, value):
    cols = value.keys()
    vals = [str(v) for v in value.values()]

    self.exe("insert into %s values (NULL, \"%s\")" % (self.tablename, "\",\"".join(vals)))


  def update(self, value, key):
    raise "TODO"
    self.exe()

  def fetch(self, num):
    return self.exe("select * from " + self.tablename + " limit " + str(num)).fetchall()

  def fetch_by_key(self, key, keyname):
    return self.exe("select * from " + self.tablename + " where " + keyname + "=" + str(key) + " limit 1").fetchone()


class Datastore():
  def __init__(self, tablename):
    self.db = DB(tablename) # TODO single DB connection for multiple DSs
    self.tablename = tablename
    self.fields = []
    self.fields_by_name = {}

  def name(self):
    return "DS-" + self.tablename

  def add_field(self, f):
    self.fields_by_name[f.name] = f
    self.fields.append(f)
    self.db.add_column(f.name)

  def validate_key(self, key_name, value):
    self.fields_by_name[key_name].validate(value)

  def validate(self, value):
    for k, v in value.items():
      self.fields_by_name[k].validate(v)
    if len(value.items()) != len(self.fields):
      raise "either missing field declaration or missing value"

  def get(self):
    return self

  def push(self, value):
    self.insert(value)

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
    return self.db.fetch_by_key(key_name, key)

# TODO: make the graph display as if it's a node, by adding an Entry node and then schema, etc, nodes
@dark.node(datasource=True, fields=["ds"])
def fetch(m):
  return m.ds.fetch()

@dark.node(datasource=True, fields=["ds"])
def schema(m):
  return m.ds.fields

@dark.node(datasink=True, fields=["ds"])
def insert(m, value):
  return m.ds.insert(value)
