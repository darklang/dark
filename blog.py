import os

import dark
import fields
import pages
import data
import datastore

blog = dark.Dark()


entry = datastore.Datastore("Entry")
entry.add_field(fields.Title("title"))
entry.add_field(fields.Url("url"))
entry.add_field(fields.Date("publication_date"))
entry.add_field(fields.Markdown("content", placeholder="Type in your dreams"))


_, ef = blog.add_edge(entry, data.except_fields("url", "publication_date"))
form = blog.add_edge(ef, pages.form_for('/new'))
page = blog.add_edge(form, pages.to_page())
blog.add_output(page, "GET", "/new")
endpoint = blog.add_input(pages.endpoint(), "POST", "/new", '/')
(datenow, kvv) = blog.add_edge(data.date_now(),
                                data.to_key_val_val("publication_date"))
url = blog.add_edge(endpoint, data.get_field("title"))
to_slug = blog.add_edge(url, data.to_slug())
rewrap = blog.add_edge(to_slug, data.to_key_val_val("url"))
merge = blog.add_edge(endpoint, data.merge())
blog.add_edge(kvv, merge)
blog.add_edge(rewrap, merge)
blog.add_edge(merge, entry)


blog.serve()
