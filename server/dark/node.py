import random
import inspect

from typing import Any, Dict, Callable, List, NewType

# TODO:
# - replace simple classes with typing.NamedTuple
# - replace comment types with real types
# - replace string types with named vars
# - add ID type
# we now have format strings

ID = NewType("ID", str)
Val = Any

class Node:
  x:int
  y:int
  _id:int

  def is_datasource(self) -> bool:
    return True

  def is_datasink(self) -> bool:
    return False

  def is_page(self) -> bool:
    return False

  def id(self) -> ID:
    raise Exception("Base Node exists")

  def exe(self, **args:Val) -> Val:
    raise Exception("this is the base class")

  def to_frontend(self) -> Any:
    raise Exception("this is the base class")

class Value(Node):
  def __init__(self, valuestr:str, x:int, y:int, id:int) -> None:
    self.name = valuestr
    self.value = eval(valuestr)
    self.x = x
    self.y = y
    self._id = id

  def exe(self, **args:Val) -> Val:
    assert len(args) == 0
    return self.value

  def to_frontend(self) -> Dict[str, Any]:
    return { "name": self.name,
             "parameters": [],
             "id": self.id(),
             "type": "value",
             "x": self.x,
             "y": self.y}

  def id(self) -> ID:
    return ID("VALUE-%04X (%s)" % ((self._id % 2**16), self.name))

class FnNode(Node):
  def __init__(self, fnname:str, x:int, y:int, id:int) -> None:
    self.fnname = fnname
    self.x = x
    self.y = y
    self._id = id
    if not self._getfn():
      raise Exception("Error: no function named " + self.fnname)

  def __str__(self) -> str:
    return self.id()

  def is_page(self) -> bool:
    return "page" in self.fnname

  def _getfn(self) -> Callable[..., Any]:
    from . import fns
    fn = getattr(fns, self.fnname, None)
    if not fn and "Page_page" in self.fnname:
      fn = fns.Page_page
    return fn

  def is_datasource(self) -> bool:
    return getattr(self._getfn(), "datasource", False)

  def is_datasink(self) -> bool:
    return getattr(self._getfn(), "datasink", False)

  def get_parameters(self) -> List[str]:
    func = self._getfn()
    argspec = inspect.getfullargspec(func)
    return argspec.args

  def exe(self, **args:Val) -> Val:
    func = self._getfn()
    params = self.get_parameters()

    # TODO: pages can have less (probably do with default params)
    if not self.is_page():
      assert len(params) == len(args)
      for p in params:
        assert p in args

    # TODO: get named arguments here
    return func(**args)

  def to_frontend(self) -> Dict[str, Any]:
    return { "name": self.fnname,
             "parameters": self.get_parameters(),
             "id": self.id(),
             "type": "page" if "page" in self.fnname else "function",
             "x": self.x,
             "y": self.y}

  def id(self) -> ID:
    return ID("%s-%04X" % (self.fnname, self._id % 2**16))
