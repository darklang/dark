import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

from dark import schema, data

@schema(fields="action")
def form_for(m, schema):
  output = ""
  for tag in schema.values():
    output += tag.as_tag().to_html()
  return ("<form action='%s' method='POST'>" % m.action
          + "<fieldset>"
          + output
          + "<br><input type='submit' name='submit' value='Submit' />"
          + "</fieldset>"
          + "</form>")

@schema(numinputs=1)
def to_page(input):
  # TODO: this feels wrong. The schema is the markup? hmmm
  # What if we need to combine schema and data to generate the page?
  # This should be a page object with a form object, and then it can get
  # auto-converted to html, or a page in an ios app.
  return "<html><head></head><body>" + input + "</body></html>"

@data(numinputs=1)
def endpoint(input):
  return input.discard("submit")

@schema(fields="fields", numinputs=1)
def except_fields(d, input):
  for f in d.fields:
    input = input.discard(f)
  return input

@data(datasource=True, numinputs=0)
def date_now(): return datetime.datetime.now().time()

@data()
def merge(*inputs):
  return pyr.m().update(*inputs)

@data(fields="fieldname")
def get_field(m, input): return input[m.fieldname]

@data(fields="key")
def to_key_val_val(m, input):
  return { m.key: input }

@data()
def to_slug(input):
  return slugify(input)
