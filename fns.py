import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

def form_for(action, schema):
  output = ""
  for tag in schema:
    output += tag.as_tag().to_html()
  return ("<form action='%s' method='POST'>" % m.action
          + "<fieldset>"
          + output
          + "<br><input type='submit' name='submit' value='Submit' />"
          + "</fieldset>"
          + "</form>")


def page(components):
  return ""
page.datasource=True
page.datasink=True

def form(schema): pass
def urldata(key, page): pass
def display(input): pass

def endpoint(input): return input.discard("submit")
endpoint.datasource = True

def except_fields(exclude, fields):
  return [f for f in fields if f.name not in exclude]

def date_now(): return datetime.datetime.now()
date_now.datasource = True

def merge(vals): return pyr.m().update(vals)
def get_field(name, obj): return obj[name]
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
fetch.datasource=True

def schema(ds): return ds.fields
schema.datasource=True

def insert(ds, value): return ds.insert(value)
insert.datasink = True


def fetch_by(data, key, val): pass
def count(data): pass
def take(data, count): pass
def takeEnd(data, count): pass
def concat(data): pass
def inverse(fieldname, data): pass
def each(data, fn): pass
