import os
import urllib.parse

from werkzeug.wrappers import Request, Response
from werkzeug.exceptions import HTTPException, NotFound
from werkzeug.wsgi import SharedDataMiddleware
from werkzeug.utils import redirect
import werkzeug.serving
from jinja2 import Environment, FileSystemLoader

class Server(object):

  def __init__(self):
    template_path = os.path.join(os.path.dirname(__file__), 'templates')
    self.jinja_env = Environment(loader=FileSystemLoader(template_path),
                                 autoescape=True)
    self.wsgi_app = SharedDataMiddleware(self.wsgi_app, {
      '/static': os.path.join(os.path.dirname(__file__), 'static')
    })

  def serve(self):
    werkzeug.serving.run_simple('127.0.0.1',
                                3000,
                                self,
                                use_debugger=True,
                                use_reloader=True)


  def render_template(self, template_name, **context):
    t = self.jinja_env.get_template(template_name)
    return Response(t.render(context), mimetype='text/html')

  def dispatch_request(self, request):
    adapter = self.url_map.bind_to_environ(request.environ)
    try:
      endpoint, values = adapter.match()
      return endpoint(request, **values)
    except HTTPException as e:
      return e

  def wsgi_app(self, environ, start_response):
    request = Request(environ)
    response = self.dispatch_request(request)
    return response(environ, start_response)

  def __call__(self, environ, start_response):
    return self.wsgi_app(environ, start_response)
