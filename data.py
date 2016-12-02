import datetime
import copy

import dark

class except_fields(dark.Node):
  def __init__(self, *fields):
    self.fields = fields

  def get_schema(self, input):
    output = copy.deepcopy(input)
    for f in self.fields:
      if f in output:
        del output[f]
    return output


# datasource
class date_now:
  def is_datasource(self):
    return True

  def get_data(self, *inputs):
    assert(len(inputs) == 0)
    return datetime.datetime.now().time()

class merge(dark.Node):
  def get_data(self, *inputs):
    print(inputs)
    x = {}
    for dict_arg in inputs:
      x.update(dict_arg)
    return x

class get_field(dark.Node):
  def __init__(self, fieldname):
    self.fieldname = fieldname

  def get_data(self, input):
    return input[self.fieldname]

class to_key_val_val(dark.Node):
  def __init__(self, key):
    self.key = key

  def get_data(self, *inputs):
    return {self.key: inputs[0]}

class to_slug(dark.Node):
  def get_data(self, input):
    from slugify import slugify
    return slugify(input)
