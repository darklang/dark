import datetime
import copy
from slugify import slugify
import pyrsistent as pyr

from dark import node

@node(fields="action")
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

@node(numinputs=1)
def to_page(input):
  # TODO: this feels wrong. The schema is the markup? hmmm
  # What if we need to combine schema and data to generate the page?
  # This should be a page object with a form object, and then it can get
  # auto-converted to html, or a page in an ios app.
  return "<html><head></head><body>" + input + "</body></html>"

@node(numinputs=1)
def endpoint(input):
  return input.discard("submit")

@node(fields="fields", numinputs=1)
def except_fields(m, input):
  for f in m.fields:
    input = input.discard(f)
  return input

@node(datasource=True, numinputs=0)
def date_now(): return datetime.datetime.now().time()

@node()
def merge(*inputs):
  return pyr.m().update(*inputs)

@node(fields="fieldname")
def get_field(m, input): return input[m.fieldname]

@node(fields="key")
def to_key_val_val(m, input):
  return { m.key: input }

@node()
def to_slug(input):
  return slugify(input)

@node()
def to_table(inputs):
  print(inputs)
  raise
