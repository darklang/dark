import datetime


class Field:
  def __init__(self, name, **props):
    self.name = name
    self.props = props

class Markdown(Field):
  def validate(self, value):
    return True

class Date(Field):
  def validate(self, value):
    return True

  def creation():
    return datetime.datetime.now().time()

class Title(Field):
  def validate(self, value):
    return True

class Url(Field):
  def validate(self, value):
    return True

def title_to_slug(title):
  from slugify import slugify
  return slugify(title)

derivations = {}
derivations["Title"] = {}
derivations["Title"]["Url"] = title_to_slug
