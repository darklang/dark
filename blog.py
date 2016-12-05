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
sc, ef = E(datastore.schema(entry), fns.except_fields(["url", "publication_date"]))
_, form = E(ef, fns.form_for('/new'))
_, newpage = E(form, fns.to_page())
blog.add_output(newpage, "GET", "/new")

# new form upload
endpoint = blog.add_input(fns.endpoint(), "POST", "/new", '/')
datenow, kvv = E(fns.date_now(), fns.to_key_val_val("publication_date"))
_, url = E(endpoint, fns.get_field("title"))
_, to_slug = E(url, fns.to_slug())
_, rewrap = E(to_slug, fns.to_key_val_val("url"))
_, merge = E(endpoint, fns.merge())
E(kvv, merge)
E(rewrap, merge)
E(merge, datastore.insert(entry))


# list all blogs
_, table = E(sc, fns.to_table())
fetch, _= E(datastore.fetch(entry), table)
_, listpage = E(table, fns.to_page())
blog.add_output(listpage, "GET", "/")


print("homepage: %s" % blog.run_output(listpage))
print("new page: %s" % blog.run_output(newpage))
blog.run_input(endpoint, {"content": "asdasdasd", "title": "title"})
print("homepage: %s" % blog.run_output(listpage))

blog.serve()
