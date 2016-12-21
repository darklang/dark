import datetime
import copy
from slugify import slugify
import pyrsistent as pyr


def page(url, inputs=[], outputs=[]):
  inputs = inputs.replace("URL_VAR", url)
  return str(outputs) + inputs
page.bothdata=True

def form(schema):
  output = ""
  for tag in schema:
    output += tag.as_tag().to_html()
  return ("<form action='URL_VAR' method='POST'>"
          + "<fieldset>"
          + output
          + "<br><input type='submit' name='submit' value='Submit' />"
          + "</fieldset>"
          + "</form>")

def urldata(key, page): pass
def display(input): pass

def endpoint(input): return
endpoint.datasource = True

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
def schema(ds): return ds.fields

def insert(ds, value):
  value = value.discard("submit")
  print("inserting: %s" % value)
  return ds.insert(value)
insert.datasink = True


def fetch_by(data, key, val): pass
def count(data): pass
def take(data, count): pass
def take_end(data, count): pass
def concat(data): pass
def inverse(fieldname, data): pass
def each(data, fn): pass
