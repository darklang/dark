import sqlite3
import dark

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


class Datastore(dark.Node):

  def __init__(self, name):
    self.db = DB(name) # TODO single DB for multiple DSs
    self.name = name
    self.fields = {}
    self.derived = {}


  def is_datasource(self):
    return True

  def add_field(self, f, derived=None):
    if derived:
      self.derived[f.name] = derived

    self.fields[f.name] = f

  def validate_key(self, key_name, value):
    self.fields[key_name].validate(value)

  def validate(self, value):
    for k, v in value.items():
      self.fields[k].validate(v)
    if len(value.items()) != len(self.fields):
      raise "either missing field declaration or missing value"

  def get_data(self, *args):
    assert(len(args) == 0)
    return self.fetch(10000)

  def get_schema(self, *args):
    return {f.name: f for f in self.fields.values()}

  def push_data(self, *args):
    assert(len(args) == 1)
    self.insert(args[0])

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
