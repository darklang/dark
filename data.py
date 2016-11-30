# data manipulation fn
class except_fields:
  def __init__(self, *fields):
    self.fields = fields

  def exe(self, inputs):
    output = inputs[0]
    for f in self.fields:
      if f in output:
        del output[f]
    return input


# datasource
class date_now:
  def exe(self, input):
    raise

class merge:
  def exe(self, input):
    raise
