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



