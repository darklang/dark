import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

from . import fields
from .datastore import Datastore
from typing import List, Any, Dict

PObj = Dict[str,Any]

# from .typechecker import tList

# These are the built-in types. Stdlib should be using these.
class DVal:
  def __init__(self, tipe : str) -> None:
    self.tipe = tipe

class DObj(DVal):
  def __init__(self, tipe : str, val: PObj) -> None:
    super().__init__(tipe)
    self.val = val

class DList(DVal):
  def __init__(self, val: List[Any]) -> None:
    super().__init__("List")
    self.val = val

class DStr(DVal):
  def __init__(self, val: str = "") -> None:
    super().__init__("String")
    self.val = val

class DInt(DVal):
  def __init__(self, val: int) -> None:
    super().__init__("Integer")
    self.val = val

class DSchema(DVal):
  def __init__(self, val: PObj) -> None:
    super().__init__("Schema")
    self.val = val

class DUrl(DVal):
  def __init__(self, val : str) -> None:
    super().__init__("Url")
    self.val = val

class DTitle(DVal):
  def __init__(self, val : str) -> None:
    super().__init__("Title")
    self.val = val

class DDate(DVal):
  def __init__(self, val : float) -> None:
    super().__init__("Date")
    self.val = val

class DMarkdown(DVal):
  def __init__(self, val : str) -> None:
    super().__init__("Markdown")
    self.val = val



#############################################################################
### BELOW HERE, ALL FUNCTIONS ARE WRITTEN IN "Dark".
#############################################################################

# TODO: is this supposed to be the input value of the page, or the render value...
def Str_append(s1 : str, s2 : str) -> str:
  return s1 + s2

def Url_to_slug(str : str) -> DUrl:
  return DUrl(slugify(str))

def Date_now() -> DDate:
  return DDate(datetime.datetime.now().timestamp())
Date_now.datasource = True # type: ignore

def Obj_dissoc(obj : PObj, keys: List[str]) -> PObj:
  return {k: v for k,v in obj if k not in keys}

def Obj_merge(vals : List[PObj]) -> PObj:
  result = {}
  for v in vals:
    result.update(v)
  return result

def Obj_get(obj : PObj, name : str) -> Any:
  return obj[name]

def Obj_select(obj : PObj, names : List[str]) -> PObj:
  return {name: obj[name] for name in names}

def Obj_wrap(key : str, val : str) -> PObj:
  return { key: val }

def Page_page(url : DUrl, outputs : List[DVal] = []) -> DVal:
  outs = [dval2page_elem(o, url) for o in outputs]
  return DStr("".join(outs))
Page_page.datasink = True # type: ignore
Page_page.datasource = True # type: ignore

def dval2page_elem(val : DVal, url : DUrl) -> str:
  raise Exception("TODO")

def tag2html(val : DVal) -> DStr:
  pass

def Page_to_form(schema : DObj, target : DStr) -> DStr:
  output = DStr()
  # for tag in schema.raw.val:
  #   output = Str_append(output, tag2html(tag))
  return DStr("<form action='URL_VAR' method='POST'>"
              + "<fieldset>"
              + output.val
              + "<br><input type='submit' name='submit' value='Submit' />"
              + "</fieldset>"
              + "</form>")


def Page_to_table(schema : DSchema, data : PObj) -> str:
  head = ""
  body = ""
  for k,v in schema.val:
    head += "<th>%s</th>" % k

  for row in data:
    body += "<tr>"
    for cell in row:
      body += "<td>%s</td>" % cell
    body += "</tr>"

  return ("<table>"
          + "<thead>" + head + "</thead>"
          + "<tbody>" + body + "</tbody>")


def DB_fetch(ds : "Datastore") -> List[DObj]:
  return [DObj(ds.tablename, v) for v in ds.fetch()]

def DB_schema(ds : "Datastore") -> DSchema:
  return DSchema({f.name: f.__class__.__name__ for f in ds.fields})

def DB_insert(ds : "Datastore", val : Any) -> Any:
  value = value.discard("submit")
  print("inserting: %s" % value)
  def inserter(ds : "Datastore") -> Any:
    ds.insert(value)
  return inserter


# def fetch_by(data, key, val): pass
# def count(data): pass
# def take(data, count): pass
# def take_end(data, count): pass
# def concat(data): pass
# def inverse(fieldname, data): pass
# def each(data, fn): pass
