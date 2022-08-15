open Tester
open! Tc

open Runtime
module RT = RuntimeTypes

let run = () => {
  describe("pathFromInputVars", () => {
    let noRequest = Belt.Map.String.empty
    let noURL = Belt.Map.String.fromArray([("request", RT.Dval.obj(list{}))])
    let generate = url =>
      Belt.Map.String.fromArray([("request", RT.Dval.obj(list{("url", RT.Dval.DStr(url))}))])

    test("returns None if no request object", () =>
      expect(pathFromInputVars(noRequest)) |> toEqual(None)
    )
    test("returns None if no url object", () => expect(pathFromInputVars(noURL)) |> toEqual(None))
    test("returns None if no url is parseable - numbers", () =>
      expect(pathFromInputVars(generate("123456"))) |> toEqual(None)
    )
    test("returns None if no url is parseable - no slashes", () =>
      expect(pathFromInputVars(generate("localhost"))) |> toEqual(None)
    )
    test("returns None if no url is parseable - no scheme", () =>
      expect(pathFromInputVars(generate("//foobar.builwithdark.com"))) |> toEqual(None)
    )
    test("returns path with no query string", () =>
      expect(pathFromInputVars(generate("https://foobar.builwithdark.com/hello"))) |> toEqual(
        Some("/hello"),
      )
    )
    test("returns path with query string", () =>
      expect(
        pathFromInputVars(generate("https://foobar.builwithdark.com/hello?foo=bar&baz=quux")),
      ) |> toEqual(Some("/hello?foo=bar&baz=quux"))
    )
  })
  ()
}
