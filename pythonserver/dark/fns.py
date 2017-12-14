def Page_to_form(schema : DObj, target : DStr) -> DStr:
  output = DStr()
  # for tag in schema.raw.val:
  #   output = Str_append(output, tag2html(tag))
  return DStr("<form action='URL_VAR' method='POST'>"
              + "<fieldset>"
              + output.val
              + "<br><input type='submit' name='submit' value='Submit' />"
              + "</fieldset>"
              + "</form>")


def Page_to_table(schema : DSchema, data : PObj) -> str:
  head = ""
  body = ""
  for k,v in schema.val:
    head += "<th>%s</th>" % k

  for row in data:
    body += "<tr>"
    for cell in row:
      body += "<td>%s</td>" % cell
    body += "</tr>"

  return ("<table>"
          + "<thead>" + head + "</thead>"
          + "<tbody>" + body + "</tbody>")

