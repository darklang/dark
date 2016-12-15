import sqlite3

import graph
import fields

class DB(object):
  def __init__(self, tablename):
    tablename = tablename.lower()
    self.tablename = tablename
    self.conn = sqlite3.connect(":memory:", check_same_thread=False)
    self.create_table()
    self.fields = []

  def exe(self, sql, *values):
    print(sql)
    return self.conn.execute(sql)

  def create_table(self):
    self.exe("create table %s (id INTEGER PRIMARY KEY AUTOINCREMENT) " %
             (self.tablename))

  def add_column(self, field):
    self.exe("ALTER TABLE %s ADD %s" % (self.tablename, field))
    self.fields.append(field)

  def insert(self, value):
    cols = self.fields
    vals = [str(value[f]) for f in self.fields if f in value]

    self.exe("insert into %s values (NULL, \"%s\")" % (
      self.tablename,
      "\",\"".join(vals)))


  def update(self, value, key):
    raise "TODO"
    self.exe()

  def fetch(self, num):
    return self.exe("select %s from %s limit %d" % (
      ",".join(self.fields),
      self.tablename,
      num)).fetchall()

  def fetch_by_key(self, key, keyname):
    return self.exe("select * from " + self.tablename + " where " + keyname + "=" + str(key) + " limit 1").fetchone()


class Datastore():
  def __init__(self, tablename):
    self.db = DB(tablename) # TODO single DB connection for multiple DSs
    self.tablename = tablename
    self.fields = []
    self.fields_by_name = {}
    self.x = -1
    self.y = -1

  # Pickling
  def __getstate__(self):
    return (self.tablename, self.fields, self.fields_by_name, self.x, self.y)

  # Unpickling
  def __setstate__(self, state):
    self.tablename, self.fields, self.fields_by_name, self.x, self.y = state
    self.db = DB(self.tablename)
    for f in self.fields:
      self.db.add_column(f.name)


  def name(self):
    return "DS-" + self.tablename

  def add_field(self, f):
    self.fields_by_name[f.name] = f
    self.fields.append(f)
    self.db.add_column(f.name)

  def to_frontend(self):
    return { "name": self.tablename,
             "id": self.name(),
             "fields": { f.name: f.__class__.__name__ for f in self.fields},
             "is_datastore": True,
             "x": self.x,
             "y": self.y
    }

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

