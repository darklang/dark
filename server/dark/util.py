import json

from typing import Dict, List, Iterator, TypeVar

X = TypeVar('X')
Y = TypeVar('Y')

def pluck(dict : Dict[X,Y], *args : X) -> Iterator[Y]:
  return (dict[arg] for arg in args)

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
