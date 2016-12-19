import datetime

class Tag:
  def __init__(self, name, contents, tag, **attributes):
    self.name = name
    self.tag = tag
    self.contents = contents
    self.attributes = attributes

  def to_html(self):
    attrs = ""
    for a in self.attributes:
      attrs += " %s=\"%s\"" % (a, self.attributes[a])
    return "<br>%s: <br><%s %s name='%s'>%s</%s>" % (self.name,
                                                     self.tag,
                                                     attrs,
                                                     self.name,
                                                     self.contents,
                                                     self.tag)

class Field:
  def __init__(self, name, is_list=False):
    self.name = name
    self.is_list = is_list

  def validate(self, value):
    return True

  def __str__(self):
    if self.is_list:
      return "[ " + self.name + "]"
    return self.name

  def to_frontend(self):
    # Legacy graphs
    if getattr(self, "is_list", None) == None:
      self.is_list = False

    cls = self.__class__.__name__
    if self.__class__ == Foreign:
      cls = self.typename
    if self.is_list:
      return "[ " + cls + " ]"
    return cls

class Markdown(Field):
  def as_tag(self, value=""):
    return Tag(self.name, value, "textarea")

class Title(Field):
  def as_tag(self, value=""):
    return Tag(self.name, value, "input", type="text")

class Date(Field): pass
class Url(Field): pass
class Name(Field): pass
class Email(Field): pass
class Text(Field): pass
class Image(Field): pass
class Geo(Field): pass
class Password(Field): pass
class Foreign(Field):
  def __init__(self, name, typename, is_list=False):
    # TODO: constructor
    self.name = name
    self.typename = typename
    self.is_list = is_list
