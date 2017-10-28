import sqlite3

class DB:
  def __init__(self, tablename:str) -> None:
    tablename = tablename.lower()
    self.tablename = tablename
    self.create_table()
    self.fields : List[str] = []

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

  def add_field(self, f:Field) -> None:
    self.fields_by_name[f.name] = f
    self.fields.append(f)
    self.db.add_column(f.name)

  def remove_last_field(self) -> None:
    r = self.fields[-1]
    self.fields = self.fields[:-1]
    self.db.remove_column(r.name)

  def validate_key(self, key_name:str, value:Any) -> None:
    self.fields_by_name[key_name].validate(value)

  def validate(self, value:Any) -> None:
    for k, v in value.items():
      self.fields_by_name[k].validate(v)
    if len(value.items()) != len(self.fields):
      raise Exception("either missing field declaration or missing value")


