import datetime


class Field:

  def __init__(self, name, **props):
    self.name = name
    self.props = props

  def client_facing(self):
    return "derived" not in self.props and "automatic" not in self.props


class Markdown(Field):

  def validate(self, value):
    return True

  def defaults(self):
    return {
      type: None,
      tag: "textarea",
      placeholder: "Enter your " + self.name
    }


class Date(Field):
  def validate(self, value):
    return True

  def creation():
    return datetime.datetime.now().time()



class Title(Field):
  def validate(self, value):
    return True

  def defaults(self):
    return {
      tag: "input",
      type: "text",
      placeholder: "Enter your " + self.name
    }

class Url(Field):
  pass
