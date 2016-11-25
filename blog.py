import werkzeug.serving

import dark
import fields

blog = dark.Dark()
ds = blog.add_datastore("Entry")
ds.add_field("contents", fields.Markdown)
ds.add_field("date", fields.Date)
ds.add_field("title", fields.Title, True)
blog.add_create(ds, '/new')
blog.add_edit(ds, '/<url>/edit')
blog.add_list(ds, '/')
blog.add_read(ds, '/<url>')

if __name__ == '__main__':
  werkzeug.serving.run_simple('127.0.0.1', 3000, blog, use_debugger=True, use_reloader=True)
