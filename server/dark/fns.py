import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

from . import fields
from .datastore import Datastore

from .typechecker import TypeConstraint

datasinks = [] # type: List[Callable[...,Any]]
datasources = [] # type: List[Callable[...,Any]]
params = {} # type: Dict[str, Dict[str, TypeConstraint]]

def page(url, inputs=[], outputs=[]):
  inputs = [i.replace("URL_VAR", url) for i in inputs]
  return str(outputs) + str(inputs)
page.datasink = True
page.datasource = True

# params["page"]["url"] = TypeConstraint()
# params["page"]["inputs"] = TypeConstraint()
# params["page"]["outputs"] = TypeConstraint()
# params["page"]["return"] = TypeConstraint()

# TODO: no python objects allowed
def form(schema) -> str:
  output = ""
  for tag in schema:
    output += tag.as_tag().to_html()
  return ("<form action='URL_VAR' method='POST'>"
          + "<fieldset>"
          + output
          + "<br><input type='submit' name='submit' value='Submit' />"
          + "</fieldset>"
          + "</form>")

def except_fields(exclude, data):
  return [f for f in data if f.name not in exclude]

def date_now(): return datetime.datetime.now()
date_now.datasource = True

def merge(vals): return pyr.m().update(vals)
def get_field(obj, name): return obj[name]
def select_fields(obj, names):
  return {name: obj[name] for name in names}

def wrap(key, val): return { key: val }
def to_slug(str): return slugify(str)

def to_table(schema, data):
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


def fetch(ds): return ds.fetch()
# TODO: fields are python objects, which makes no sense. Maybe ADTs would be
# better, in which case I need to figure out case statements and destructuring.
def schema(ds): return ds.fields

def insert(ds, value):
  value = value.discard("submit")
  print("inserting: %s" % value)
  def inserter(ds) -> Any:
    ds.insert(value)
  return inserter


def fetch_by(data, key, val): pass
def count(data): pass
def take(data, count): pass
def take_end(data, count): pass
def concat(data): pass
def inverse(fieldname, data): pass
def each(data, fn): pass
