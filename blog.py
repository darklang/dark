import os

import werkzeug.serving

import dark
import fields
import pages
import data
import datastore

blog = dark.Dark()
entry = datastore.Datastore("Entry")
ds = blog.add(entry)

entry.add_field(fields.Title("title"))
entry.add_field(fields.Url("url"))
entry.add_field(fields.Date("publication_date"))
entry.add_field(fields.Markdown("contents",
                             placeholder="Type in your dreams"))

ef = data.except_fields("url", "publication_date")
blog.edge_from(ds, ef)

form = pages.form_for('/new')
blog.edge_from(ef, form)

page = pages.to_page()
blog.add_output(form, "GET", "/new")

endpoint = pages.endpoint()
blog.add_input(endpoint, "POST", "/new", '/')

datenow = blog.add(data.date_now())
kvv = data.to_key_val_val("publication_date")
blog.edge_from(datenow, kvv)

url = blog.add(data.get_field("title"))
to_slug = blog.add(data.to_slug())
rewrap = blog.add(data.to_key_val_val("url"))
blog.edge_from(endpoint, url)
blog.edge_from(url, to_slug)
blog.edge_from(to_slug, rewrap)

merge = data.merge()
blog.edge_from(endpoint, merge)
blog.edge_from(kvv, merge)
blog.edge_from(rewrap, merge)

blog.edge_from(merge, ds)



# c = blog.add(pages.Create(ds, "/new"))
# e = blog.add(ds, pages.Edit, '/<url>/edit')
# l = blog.add(ds, pages.Listm '/')
# r = blog.add(ds, pages.Read '/<url>')

if __name__ == '__main__':
  werkzeug.serving.run_simple('127.0.0.1',
                              3000,
                              blog,
                              use_debugger=True,
                              use_reloader=True)
