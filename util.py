import json


class attrdict(dict):
  def __init__(self, *args, **kwargs):
    dict.__init__(self, *args, **kwargs)
    self.__dict__ = self


def tojson(l):
  def default(val):
    if getattr(val, "__str__"):
      return str(val)

    raise TypeError()
  return json.dumps(l, default=default)
