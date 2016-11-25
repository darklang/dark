import os
import urllib.parse

from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule
from werkzeug.exceptions import HTTPException, NotFound
from werkzeug.wsgi import SharedDataMiddleware
from werkzeug.utils import redirect
from jinja2 import Environment, FileSystemLoader

class Builtin(object):

  def __init__(self):
    self.wsgi_app = SharedDataMiddleware(self.wsgi_app, {
      '/static':  os.path.join(os.path.dirname(__file__), 'static')
    })

    template_path = os.path.join(os.path.dirname(__file__), 'templates')
    self.jinja_env = Environment(loader=FileSystemLoader(template_path),
                                 autoescape=True)


  def render_template(self, template_name, **context):
    t = self.jinja_env.get_template(template_name)
    return Response(t.render(context), mimetype='text/html')

  def dispatch_request(self, request):
    adapter = self.url_map.bind_to_environ(request.environ)
    try:
        endpoint, values = adapter.match()
        return endpoint(request, **values)
        #return getattr(self, 'on_' + endpoint)(request, **values)
    except HTTPException as e:
      return e

  def wsgi_app(self, environ, start_response):
    request = Request(environ)
    response = self.dispatch_request(request)
    return response(environ, start_response)

  def __call__(self, environ, start_response):
    return self.wsgi_app(environ, start_response)



class Datastore(object):
  def __init__(self, name):
    self.name = name
    self.fields = {}

  def add_field(self, name, tyype):
    self.fields[name] = tyype
    return Field(self, name)



class Dark(Builtin):

  def __init__(self):
    super(Builtin, self).__init__()
    self.datastores = {}

  def add_datastore(self, name):
    ds = Datastore(name)
    self.datastores[name] = ds
    return ds

  def add_create(self, ds, route):

    def view(request, **values):
      # TODO: something with ds
      return self.render_template('create.html')

    def action(request, **values):
      new_value = request.values
      ds.validate(new_value)
      datastore.add(new_value)
      return Response()

    view_rule = Rule(route, methods=["GET"], endpoint=view)
    action_rule = Rule(route, methods=["POST"], endpoint=action)
    self.url_map.add(rule)
    self.url_map.add(rule)


  def add_list(self, ds, route):

    def view(request, **values):
      values = ds.entries[0:10]
      return self.render_template('list.html')

    rule = Rule(route, methods=["GET"], endpoint=view)
    self.url_map.add(rule)



  def add_edit(self, ds, route):

    def view(request, **values):
      url = request.values["url"]
      value = request.values.by("url", url)
      return self.render_template('edit.html', value=value)

    def action(request, **values):
      url = values.url
      new_value = request.values
      ds.validate(new_value)
      datastore.replace(url)
      return Response()

    view_rule = Rule(route, methods=["GET"], endpoint=view)
    action_rule = Rule(route, methods=["PUT"], endpoint=action)
    self.url_map.add(view_rule)
    self.url_map.add(action_rule)


  def add_read(self, ds, route):
    def view(request, url):
      value = ds.entries.by("url", url)
      return self.render_template('read.html', value=value)

    rule = Rule(route, methods=["GET"], endpoint=view)
    self.url_map.add(rule)



blog = Dark()
ds = blog.add_datastore("Entry")
ds.add_field("contents", types.Markdown)
ds.add_field("date", types.Date)
ds.add_field("title", types.Title)
ds.set_addressable("title")
blog.add_create(ds, '/new')
blog.add_edit(ds, '<url>/edit')
blog.add_list(ds, '/')
blog.add_read(ds, '/<url>')

if __name__ == '__main__':
  werkzeug.serving.run_simple('127.0.0.1', 3000, blog, use_debugger=True, use_reloader=True)
