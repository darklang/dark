from werkzeug.wrappers import Request, Response
from werkzeug.routing import Map, Rule

import server

class Datastore(object):
  def __init__(self, name):
    self.name = name
    self.fields = {}
    self.addressable = None

  def add_field(self, name, tyype):
    self.fields[name] = tyype

  def set_addressable(self, name):
    self.addressable = name


class Dark(server.Server):

  def __init__(self):
    super(server.Server, self).__init__()
    self.url_map = Map()
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
    self.url_map.add(view_rule)
    self.url_map.add(action_rule)


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
