import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

from typing import Any, List, Callable, Dict

import fields
from datastore import Datastore

DarkObj = Dict[str,Any]

datasinks = [] # type: List[Callable[...,Any]]
datasources = [] # type: List[Callable[...,Any]]


def page(url : str, inputs : List[str]=[], outputs : List[str]=[]) -> str:
  inputs = [i.replace("URL_VAR", url) for i in inputs]
  return str(outputs) + str(inputs)
datasinks.append(page)
datasources.append(page)

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

def except_fields(exclude : List[str], data : List[Any]) -> List[Any]:
  return [f for f in data if f.name not in exclude]

def date_now() -> datetime.datetime: return datetime.datetime.now()
datasources.append(date_now)

def merge(vals : List[DarkObj]) -> DarkObj: return pyr.m().update(vals)
def get_field(obj : DarkObj, name : str) -> Any: return obj[name]
def select_fields(obj : DarkObj, names : List[str]) -> DarkObj:
  return {name: obj[name] for name in names}

def wrap(key : str, val : Any) -> DarkObj: return { key: val }
def to_slug(str) -> str: return slugify(str)

def to_table(schema : List[Any], data : DarkObj) -> str:
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


def fetch(ds : Datastore) -> List[DarkObj]: return ds.fetch()
# TODO: fields are python objects, which makes no sense. Maybe ADTs would be
# better, in which case I need to figure out case statements and destructuring.
def schema(ds : Datastore) -> Any: return ds.fields

def insert(ds : Datastore, value : DarkObj) -> Any:
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
