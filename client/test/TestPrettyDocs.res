open Tester
open PrettyDocs

let run = () => {
  describe("Regex", () => {
    test("exactly", () => expect(Regex.exactly(~re="ok", "ok")) |> toEqual(true))
    test("captures has no matches", () =>
      expect(Regex.captures(~re=Regex.regex(tagEx), "Hello")) |> toEqual(list{})
    )
    test("captures has matches", () =>
      expect(Regex.captures(~re=Regex.regex(tagEx), "<type Option>")) |> toEqual(list{
        "<type Option>",
        "",
        "type",
        "Option",
        "",
      })
    )
    test("captures code block", () =>
      expect(Regex.captures(~re=Regex.regex(codeEx), "for example: {{let a = 1}}")) |> toEqual(list{
        "for example: {{let a = 1}}",
        "for example: ",
        "let a = 1",
        "",
      })
    )
    ()
  })
  describe("PrettyDocs", () => {
    test("convert_ catches invalid tags", () =>
      expect(convert_("<div contenteditable>")) |> toEqual(
        ParseFail(list{("<div contenteditable>", "'div' is not a valid tag type")}),
      )
    )
    test("convert_ catches nested tags", () =>
      expect(convert_("<type <var bad bunny>>")) |> toEqual(
        ParseFail(list{("<type <var bad bunny>>", "contains nested tags")}),
      )
    )
    test("convert_ catches nested code blocks", () =>
      expect(convert_("{{Just {{ok}} }}")) |> toEqual(
        ParseFail(list{("{{Just {{ok}} }}", "contains nested code blocks")}),
      )
    )
    test("converts tagged string", () =>
      expect(convert("takes in <type Option>")) |> toEqual(list{
        txt("takes in "),
        tag("type", list{txt("Option")}),
      })
    )
    test("converts normal string", () => expect(convert("Bye")) |> toEqual(list{txt("Bye")}))
    test("converts code blocks", () =>
      expect(convert("{{Ok <var value>}}")) |> toEqual(list{
        tag("code", list{txt("Ok "), tag("var", list{txt("value")})}),
      })
    )
    test("converts link tag", () =>
      expect(convert("Into the [dark](http://www.darklang.com)")) |> toEqual(list{
        txt("Into the "),
        link("dark", "http://www.darklang.com"),
      })
    )
    test("converts string with multiple tags, a link, and a code block", () =>
      expect(
        convert(
          "Returns an <type Result>.\n It will got to [error rail](https://docs.darklang.com/tutorials/handle-error-errorrail), if it is {{Error <var message>}}",
        ),
      ) |> toEqual(list{
        txt("Returns an "),
        tag("type", list{txt("Result")}),
        txt(".\n It will got to "),
        link("error rail", "https://docs.darklang.com/tutorials/handle-error-errorrail"),
        txt(", if it is "),
        tag("code", list{txt("Error "), tag("var", list{txt("message")})}),
      })
    )
    ()
  })
  ()
}
