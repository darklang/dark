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

_, ef = blog.add_edge(entry, fns.except_fields(["url", "publication_date"]))
_, form = blog.add_edge(ef, fns.form_for('/new'))
_, page = blog.add_edge(form, fns.to_page())
blog.add_output(page, "GET", "/new")
endpoint = blog.add_input(fns.endpoint(), "POST", "/new", '/')
datenow, kvv = blog.add_edge(fns.date_now(),
                                fns.to_key_val_val("publication_date"))
_, url = blog.add_edge(endpoint, fns.get_field("title"))
_, to_slug = blog.add_edge(url, fns.to_slug())
_, rewrap = blog.add_edge(to_slug, fns.to_key_val_val("url"))
_, merge = blog.add_edge(endpoint, fns.merge())
blog.add_edge(kvv, merge)
blog.add_edge(rewrap, merge)
blog.add_edge(merge, entry)


blog.serve()
