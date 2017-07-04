from typing import List, Any, Dict

PObj = Dict[str,Any]

# These are the built-in types. Stdlib should be using these.
class DVal:
  def __init__(self, tipe : str) -> None:
    self.tipe = tipe

class DObj(DVal):
  def __init__(self, tipe : str, val: PObj) -> None:
    super().__init__(tipe)
    self.val = val

class DList(DVal):
  def __init__(self, val: List[Any]) -> None:
    super().__init__("List")
    self.val = val

class DStr(DVal):
  def __init__(self, val: str = "") -> None:
    super().__init__("String")
    self.val = val

class DInt(DVal):
  def __init__(self, val: int) -> None:
    super().__init__("Integer")
    self.val = val

class DSchema(DVal):
  def __init__(self, val: PObj) -> None:
    super().__init__("Schema")
    self.val = val

class DUrl(DVal):
  def __init__(self, val : str) -> None:
    super().__init__("Url")
    self.val = val

class DTitle(DVal):
  def __init__(self, val : str) -> None:
    super().__init__("Title")
    self.val = val

class DDate(DVal):
  def __init__(self, val : float) -> None:
    super().__init__("Date")
    self.val = val

class DMarkdown(DVal):
  def __init__(self, val : str) -> None:
    super().__init__("Markdown")
    self.val = val


def py2dval(val):
  if isinstance(val, int):
    return DInt(val)
  if isinstance(val, str):
    return DStr(val)
  raise Exception("Unknown type: " + str(val.__class__) + " in " + str(val))

def prettify_type(t):
  return t.__name__

class DTypeError(Exception):
  def __init__(self, t1, t2):

    # Call the base class constructor with the parameters it needs
    super(DTypeError, self).__init__("Type check exception")
    self.t1 = t1
    self.t2 = t2
    self.p1 = prettify_type(t1)
    self.p2 = prettify_type(t2)

def check(a : type, b : type):
  if not _check(a, b) and not _check(b, a):
    raise DTypeError(a, b)

def _check(a : type, b : type) -> bool:
  ts = (a, b)

  if a == b:
    return True
  if ts == (DStr, DUrl):
    return True

  return False
