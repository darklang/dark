import os

import dark
import fields
import fns
import datastore

blog = dark.Dark()

entry = datastore.Datastore("Entry")
entry.add_field(fields.Title("title"))
entry.add_field(fields.Url("url"))
entry.add_field(fields.Date("publication_date"))
entry.add_field(fields.Markdown("content", placeholder="Type in your dreams"))

def E(*args):
  return blog.add_edge(*args)

# new page
_, sc = E(entry, datastore.schema())
_, ef = E(sc, fns.except_fields(["url", "publication_date"]))
_, form = E(ef, fns.form_for('/new'))
_, page = E(form, fns.to_page())
blog.add_output(page, "GET", "/new")

# new form upload
endpoint = blog.add_input(fns.endpoint(), "POST", "/new", '/')
datenow, kvv = E(fns.date_now(), fns.to_key_val_val("publication_date"))
_, url = E(endpoint, fns.get_field("title"))
_, to_slug = E(url, fns.to_slug())
_, rewrap = E(to_slug, fns.to_key_val_val("url"))
_, merge = E(endpoint, fns.merge())
E(kvv, merge)
E(rewrap, merge)
E(merge, entry)


# list all blogs
_, fetch = E(entry, datastore.fetch())
_, table = E(fetch, fns.to_table())
_, page = E(table, fns.to_page())
blog.add_output(page, "GET", "/")


blog.serve()
