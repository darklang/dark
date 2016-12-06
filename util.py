import json


class attrdict(dict):
  def __init__(self, *args, **kwargs):
    dict.__init__(self, *args, **kwargs)
    self.__dict__ = self


def tojson(l):
  def default(val):
    return None
  return json.dumps(l, default=default)
