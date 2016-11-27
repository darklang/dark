import os

import werkzeug.serving

import dark
import fields
import pages

blog = dark.Dark()
ds = blog.add_datastore(dark.Datastore("Entry"))

ds.add_field(fields.Title("title"))
ds.add_field(fields.Url("url", derived="title"))
ds.add_field(fields.Date("publication_date",
                         default=fields.Date.creation,
                         automatic=True))
ds.add_field(fields.Markdown("contents",
                             placeholder="Type in your dreams"))

c = blog.add(pages.Create(ds, "/new"))
e = blog.add(pages.Edit(ds, '/<url>/edit'))
l = blog.add(pages.List(ds, '/'))
r = blog.add(pages.Read(ds, '/<url>'))

if __name__ == '__main__':
  werkzeug.serving.run_simple('127.0.0.1',
                              3000,
                              blog,
                              use_debugger=True,
                              use_reloader=True)
