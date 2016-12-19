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
  def __init__(self, name, **props):
    self.name = name
    self.props = props

  def validate(self, value):
    return True

  def __str__(self):
    return self.name

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
