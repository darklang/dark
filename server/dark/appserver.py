import os
import urllib.parse

from werkzeug.routing import Map
from werkzeug.wrappers import Request, Response
from werkzeug.exceptions import HTTPException
from werkzeug.wsgi import SharedDataMiddleware
import werkzeug.serving
from jinja2 import Environment, FileSystemLoader

from typing import Optional, Union, Any

class AppServer(object):

  def __init__(self) -> None:
    self.url_map = Map() # unused, here for Mypy

    self.users = {"dark": "2DqMHguUfsAGCPerWgyHRxPi"}

    template_path = os.path.join(os.path.dirname(__file__), '..', '..', 'templates')
    self.jinja_env = Environment(loader=FileSystemLoader(template_path),
                                 autoescape=True)
    self.wsgi_app = SharedDataMiddleware(self.app, {
      '/static': os.path.join(os.path.dirname(__file__), '..', '..', 'static')
    })

  def serve(self) -> None:
    werkzeug.serving.run_simple('127.0.0.1',
                                int(os.getenv("PORT", "3000")),
                                self,
                                use_debugger=False,
                                use_reloader=True)


  def render_template(self, template_name:str, **context:str) -> Response:
    t = self.jinja_env.get_template(template_name)
    return Response(t.render(context), mimetype='text/html')

  def dispatch_request(self, request : Request) -> Union[Response, HTTPException]:
    adapter = self.url_map.bind_to_environ(request.environ)
    try:
      endpoint, values = adapter.match()
      return endpoint(request, **values)
    except HTTPException as e:
      return e

  def app(self, environ:Any, start_response:Any) -> Any:
    request = Request(environ)
    auth = request.authorization
    if not auth or not self.check_auth(auth.username, auth.password):
      response = self.auth_required(request)
    else:
      response = self.dispatch_request(request)
    return response(environ, start_response)

  def check_auth(self, username, password):
    return username in self.users and self.users[username] == password

  def auth_required(self, request):
    return Response('Could not verify your access level for that URL.\n'
                    'You have to login with proper credentials', 401,
                    {'WWW-Authenticate': 'Basic realm="Dark"'})

  def __call__(self, environ:Any, start_response:Any) -> Any:
    return self.wsgi_app(environ, start_response)
