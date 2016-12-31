import random
import inspect

from typing import Any, Dict, Callable, List

import fns

class Node:
  def __init__(self) -> None:
    # satisfy the type checker
    self.x = -1
    self.y = -1

  def is_datasource(self) -> bool:
    return True

  def is_datasink(self) -> bool:
    return False

  def is_page(self) -> bool:
    return False

  def id(self) -> str:
    raise Exception("Base Node exists")

  def exe(self, **args) -> Any:
    raise Exception("this is the base class")

  def to_frontend(self) -> Any:
    raise Exception("this is the base class")

class Value(Node):
  def __init__(self, valuestr : str) -> None:
    self.name = valuestr
    self.value = eval(valuestr)
    self._id = random.randint(0, 2**32)

  def exe(self, **args) -> Any:
    assert len(args) == 0
    return self.value

  def to_frontend(self) -> Dict[str, Any]:
    return { "name": self.name,
             "parameters": [],
             "id": self.id(),
             "type": "value",
             "x": self.x,
             "y": self.y}

  def id(self) -> str:
    return "VALUE-%04X (%s)" % ((self._id % 2**16), self.name)

class FnNode(Node):
  def __init__(self, fnname : str) -> None:
    self.fnname = fnname
    self._id = random.randint(0, 2**32)
    assert self._getfn()

  def __str__(self) -> str:
    return self.id()

  def is_page(self) -> bool:
    return "page" in self.fnname

  def _getfn(self) -> Callable[..., Any]:
    import fns
    fn = getattr(fns, self.fnname, None)
    if not fn and "page" in self.fnname:
      fn = fns.page
    if fn == None:
      def fn(args):
        return {}
    return fn

  def is_datasource(self) -> bool:
    return self._getfn() in fns.datasources

  def is_datasink(self) -> bool:
    return self._getfn() in fns.datasinks

  def get_parameters(self) -> List[str]:
    func = self._getfn()
    (params, _1, _2, _3) = inspect.getargspec(func)
    # getargspec can return lists of lists (when there are tuples in the function
    # definition. We don't use that, soignore.)
    return params

  def exe(self, **args) -> Any:
    func = self._getfn()
    params = self.get_parameters()

    # TODO: figure out page
    # assert len(params) == len(args)
    # for p in params:
      # assert p in args

    # TODO: get named arguments here
    return func(**args)

  def to_frontend(self) -> Dict[str, Any]:
    return { "name": self.fnname,
             "parameters": self.get_parameters(),
             "id": self.id(),
             "type": "page" if "page" in self.fnname else "function",
             "x": self.x,
             "y": self.y}

  def id(self) -> str:
    return "%s-%04X" % (self.fnname, self._id % 2**16)
