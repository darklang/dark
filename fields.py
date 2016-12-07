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

  def __str__(self):
    return self.name

class Markdown(Field):
  def validate(self, value):
    return True

  def as_tag(self, value=""):
    return Tag(self.name, value, "textarea")

class Date(Field):
  def validate(self, value):
    return True

class Title(Field):
  def validate(self, value):
    return True

  def as_tag(self, value=""):
    return Tag(self.name, value, "input", type="text")

class Url(Field):
  def validate(self, value):
    return True
