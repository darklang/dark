import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

from typing import Any, List, Callable, Dict, Union

from . import fields
from .datastore import Datastore

from .typechecker import TypeConstraint

datasinks = [] # type: List[Callable[...,Any]]
datasources = [] # type: List[Callable[...,Any]]
params = {} # type: Dict[str, Dict[str, TypeConstraint]]

Obj = Dict[str, Any]


def page(url, inputs=[], outputs=[]):
  inputs = [i.replace("URL_VAR", url) for i in inputs]
  return str(outputs) + str(inputs)

# params["page"]["url"] = TypeConstraint()
# params["page"]["inputs"] = TypeConstraint()
# params["page"]["outputs"] = TypeConstraint()
# params["page"]["return"] = TypeConstraint()
datasinks.append(page)
datasources.append(page)

# TODO: no python objects allowed
def form(schema : List[fields.Field]) -> str:
  output = ""
  for tag in schema:
    output += tag.as_tag().to_html()
  return ("<form action='URL_VAR' method='POST'>"
          + "<fieldset>"
          + output
          + "<br><input type='submit' name='submit' value='Submit' />"
          + "</fieldset>"
          + "</form>")

def except_fields(exclude : List[str], data : List[Obj]) -> List[Any]:
  return [f for f in data if f.name not in exclude]

def date_now() -> datetime.datetime: return datetime.datetime.now()
datasources.append(date_now)

def merge(vals : List[Obj]) -> Obj: return pyr.m().update(vals)
def get_field(obj : Obj, name : str) -> Any: return obj[name]
def select_fields(obj : Obj, names : List[str]) -> Obj:
  return {name: obj[name] for name in names}

def wrap(key : str, val : Any) -> Obj: return { key: val }
def to_slug(str) -> str: return slugify(str)

def to_table(schema : List[Any], data : Obj) -> str:
  head = ""
  body = ""
  for field in schema:
    head += "<th>%s</th>" % field.name

  for row in data:
    body += "<tr>"
    for cell in row:
      body += "<td>%s</td>" % cell
    body += "</tr>"

  return ("<table>"
          + "<thead>" + head + "</thead>"
          + "<tbody>" + body + "</tbody>")


def fetch(ds : Datastore) -> List[Obj]: return ds.fetch()
# TODO: fields are python objects, which makes no sense. Maybe ADTs would be
# better, in which case I need to figure out case statements and destructuring.
def schema(ds : Datastore) -> Any: return ds.fields

def insert(ds : Datastore, value : Obj) -> Any:
  value = value.discard("submit") # type: ignore
  print("inserting: %s" % value)
  def inserter(ds) -> Any:
    ds.insert(value)
  return inserter


def fetch_by(data, key, val) -> Any: pass
def count(data) -> Any: pass
def take(data, count) -> Any: pass
def take_end(data, count) -> Any: pass
def concat(data) -> Any: pass
def inverse(fieldname, data) -> Any: pass
def each(data, fn) -> Any: pass
