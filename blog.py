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
        return getattr(self, 'on_' + endpoint)(request, **values)
    except HTTPException as e:
      return e

  def wsgi_app(self, environ, start_response):
    request = Request(environ)
    response = self.dispatch_request(request)
    return response(environ, start_response)

  def __call__(self, environ, start_response):
    return self.wsgi_app(environ, start_response)

class Shortly(Builtin):
  def __init__(self, config):
    self.url_map = Map([
      Rule('/', endpoint='homepage', methods=["GET"]),
      Rule('/', endpoint='new_entry', methods=["POST"]),
      Rule('/<short_id>/edit', endpoint='view_edit_entry', methods=["GET"]),
      Rule('/<short_id>', endpoint='update_entry', methods=["PUT"]),
      Rule('/<short_id>', endpoint='view_entry', methods=["GET"])
    ])

  def on_homepage(self, request):
    values = datastore.entries[0:10]
    self.render(homepage, values)

  def on_new_entry(self, request):
    new_value = request.values
    self.validate(new_value)
    datastore.entries.add(new_value)
    return Response()

  def on_view_entry(self, request, short_id):
    value = datastore.entries.by_short_id(short_id)
    self.render(view_page, value)

  def on_update_entry(self, request, short_id):
    value = request.values
    datastore.entries.by_short_id(short_id).replace_with(value)
    return Response()

  def on_view_edit_entry(self, request, short_id):
    value = request.values.by_short_id(short_id)
    return self.render_template('edit_page.html', value)
#    return self.render_template('new_url.html', error=error, url=url)

class Model(object):
  def __init__(self, contents, date, title):
    self.contents = contents
    self.date = date
    self.title = title
    self.short_url = re.replace(title, r"[-_!@#$%^&*()]+", "-")



if __name__ == '__main__':
  from werkzeug.serving import run_simple
  app = Shortly()
  run_simple('127.0.0.1', 3000, app, use_debugger=True, use_reloader=True)
