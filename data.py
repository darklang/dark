# data manipulation fn
class except_fields:
  def __init__(self, *fields):
    self.fields = fields

  def exe(self, input):
    for f in self.fields:
      if f in input:
        del input[f]
    return input


# datasource
class date_now:
  def exe(self, input):
    raise

class merge:
  def exe(self, input):
    raise
