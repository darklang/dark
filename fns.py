import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

from graph import node

@node(fields=["action"])
def form_for(m, schema):
  output = ""
  for tag in schema:
    output += tag.as_tag().to_html()
  return ("<form action='%s' method='POST'>" % m.action
          + "<fieldset>"
          + output
          + "<br><input type='submit' name='submit' value='Submit' />"
          + "</fieldset>"
          + "</form>")

@node(datasink=True)
def to_page(input):
  # TODO: this feels wrong. The schema is the markup? hmmm
  # What if we need to combine schema and data to generate the page?
  # This should be a page object with a form object, and then it can get
  # auto-converted to html, or a page in an ios app.
  return "<html><head></head><body>" + input + "</body></html>"

@node(datasource=True)
def endpoint(input):
  return input.discard("submit")

@node(fields=["exclude"])
def except_fields(m, fields):
  return [f for f in fields if f.name not in m.exclude]

@node(datasource=True)
def date_now(): return datetime.datetime.now()

@node()
def merge(*vals):
  return pyr.m().update(*vals)

@node(fields=["fieldname"])
def get_field(m, obj): return obj[m.fieldname]

@node(fields=["key"])
def to_key_val_val(m, val):
  return { m.key: val }

@node()
def to_slug(str):
  return slugify(str)

@node()
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


