import datetime

import dark

class except_fields(dark.Node):
  def __init__(self, *fields):
    self.fields = fields

  def get_schema(self, input):
    output = input
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
  def get_data(self, *input):
    print(input)
    raise
