import sqlite3

from typing import Any, List, Tuple, Dict

from . import fields
from .fields import Field
from . import node
from .node import ID, Val

class DB:
  def __init__(self, tablename:str) -> None:
    tablename = tablename.lower()
    self.tablename = tablename
    self.conn = sqlite3.connect(":memory:", check_same_thread=False)
    self.create_table()
    self.fields : List[str] = []

  def exe(self, sql:str, *values:Val) -> Val:
    print(sql)
    return self.conn.execute(sql)

  def create_table(self) -> Any:
    self.exe("create table %s (id INTEGER PRIMARY KEY AUTOINCREMENT) " %
             (self.tablename))

  def add_column(self, field:str) -> None:
    self.exe("ALTER TABLE %s ADD %s" % (self.tablename, field))
    self.fields.append(field)

  def remove_column(self, field:str) -> None:
    #self.exe("ALTER TABLE %s DROP %s" % (self.tablename, field)) - sqlite doesn't support dropping
    self.fields = [f for f in self.fields if field != f]

  def insert(self, value:Any) -> Any:
    cols = self.fields
    vals = [str(value[f]) for f in self.fields if f in value]

    self.exe("insert into %s values (NULL, \"%s\")" % (
      self.tablename,
      "\",\"".join(vals)))


  def update(self, value:Any, key:str) -> None:
    raise Exception("TODO")
    self.exe()

  def fetch(self, num:int) -> List[Val]:
    if len(self.fields) == 0:
      return []
    data = self.exe("select %s from %s limit %d" % (
      ",".join(self.fields),
      self.tablename,
      num)).fetchall()
    return data

  def fetch_by_key(self, key:Any, keyname:str) -> Any:
    return self.exe("select * from " + self.tablename + " where " + keyname + "=" + str(key) + " limit 1").fetchone()


class Datastore(node.Node):
  def __init__(self, tablename:str, x:int, y:int) -> None:
    self.db = DB(tablename) # TODO single DB connection for multiple DSs
    self.x = x
    self.y = y
    self.tablename = tablename
    self.fields : List[Field] = []
    self.fields_by_name : Dict[str, Field] = {}

  def is_datasource(self) -> bool: return True
  def is_datasink(self) -> bool: return True

  # Pickling
  def __getstate__(self) -> Tuple[str,List[Field], Dict[str,Field],int,int]:
    return (self.tablename, self.fields, self.fields_by_name, self.x, self.y)

  # Unpickling
  def __setstate__(self, state: Tuple[str,List[Field], Dict[str,Field],int,int]) -> None:
    self.tablename, self.fields, self.fields_by_name, self.x, self.y = state
    self.db = DB(self.tablename)
    for f in self.fields:
      self.db.add_column(f.name)

  def name(self) -> str:
    return "DS-" + self.tablename

  def id(self) -> ID:
    return ID(self.name())

  def add_field(self, f:Field) -> None:
    self.fields_by_name[f.name] = f
    self.fields.append(f)
    self.db.add_column(f.name)

  def remove_last_field(self) -> None:
    r = self.fields[-1]
    self.fields = self.fields[:-1]
    self.db.remove_column(r.name)

  def to_frontend(self) -> Any:
    return { "name": self.tablename,
             "id": self.id(),
             "fields": [ (f.name, f.to_frontend()) for f in self.fields],
             "parameters": ["ds"],
             "type": "datastore",
             "x": self.x,
             "y": self.y
    }

  def validate_key(self, key_name:str, value:Any) -> None:
    self.fields_by_name[key_name].validate(value)

  def validate(self, value:Any) -> None:
    for k, v in value.items():
      self.fields_by_name[k].validate(v)
    if len(value.items()) != len(self.fields):
      raise Exception("either missing field declaration or missing value")

  def exe(self, **args:Val) -> 'Datastore':
    assert len(args) == 0
    return self

  def push(self, value:Any) -> None:
    self.insert(value)

  def insert(self, value:Any) -> None:
    self.validate(value)
    self.db.insert(value)

  def replace(self, key:str, value:Any) -> None:
    self.validate_key(key, value)
    self.validate(value)
    self.db.update(key, value)

  def fetch(self, num:int = 10) -> List[Any]:
    vals = self.db.fetch(num)
    print(vals)
    # this is wrong. Should be a list of objects
    raise Exception("wtf am i doint here")
    # return {k: v for (k,v) in zip(self.fields, self.db.fetch(num))}

  def fetch_one(self, key:str, key_name:str ) -> Any:
    return self.db.fetch_by_key(key_name, key)

