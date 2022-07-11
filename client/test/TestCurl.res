open Prelude
open Tester
open CurlCommand
module B = BlankOr

let defaultTLID = TLID.fromString("7")

let http = (~path: string, ~meth="GET", ()): handler => {
  ast: FluidAST.ofExpr(EBlank(gid())),
  hTLID: defaultTLID,
  pos: {x: 0, y: 0},
  spec: {space: B.newF("HTTP"), name: B.newF(path), modifier: B.newF(meth)},
}

// Sets the model with the appropriate toplevels
let makeModel = (~handlers=list{}, ~traces=Map.String.empty, ~cursorState, ()): model => {
  let default = Defaults.defaultModel
  {
    ...default,
    handlers: Handlers.fromList(handlers),
    canvasName: "test-curl",
    cursorState: cursorState,
    traces: traces,
  }
}

let run = () => {
  describe("strAsBodyCurl", () => {
    test("returns jsonfied curl flag", () =>
      expect(strAsBodyCurl(DStr("{\"a\":1,\"b\":false}"))) |> toEqual(
        Some("-d '{\"a\":1,\"b\":false}'"),
      )
    )
    test("returns None if input dval is not DObj", () =>
      expect(strAsBodyCurl(DNull)) |> toEqual(None)
    )
  })
  describe("objAsHeaderCurl", () => {
    test("returns header curl flag string", () => {
      let obj = Dval.obj(list{
        ("Content-Type", DStr("application/json")),
        ("Authorization", DStr("Bearer abc123")),
      })

      expect(objAsHeaderCurl(obj)) |> toEqual(
        Some("-H 'Authorization:Bearer abc123' -H 'Content-Type:application/json'"),
      )
    })
    test("returns None if input dval is not DObj", () =>
      expect(objAsHeaderCurl(DNull)) |> toEqual(None)
    )
  })
  describe("curlFromSpec", () => {
    let m = makeModel(
      ~handlers=list{http(~path="/test", ())},
      ~cursorState=Selecting(defaultTLID, None),
      (),
    )

    test("returns command for /test GET", () =>
      expect(curlFromSpec(m, defaultTLID)) |> toEqual(
        Some("curl http://test-curl.builtwithdark.com/test"),
      )
    )
    test("returns command in https if env=prod", () => {
      let m1 = {...m, environment: "production"}
      expect(curlFromSpec(m1, defaultTLID)) |> toEqual(
        Some("curl https://test-curl.builtwithdark.com/test"),
      )
    })
    test("returns None if tlid not found", () =>
      expect(curlFromSpec(m, TLID.fromString("1"))) |> toEqual(None)
    )
    test("returns None for non-HTTP handlers", () => {
      let cronTLID = TLID.fromString("2")
      let cron = {
        ast: FluidAST.ofExpr(EBlank(gid())),
        hTLID: cronTLID,
        pos: {x: 0, y: 0},
        spec: {
          space: B.newF("CRON"),
          name: B.newF("cleanKitchen"),
          modifier: B.newF("Fortnightly"),
        },
      }

      let m1 = {...m, handlers: Handlers.fromList(list{cron})}
      expect(curlFromSpec(m1, cronTLID)) |> toEqual(None)
    })
  })
  describe("curlFromCurrentTrace", () => {
    let traces = input =>
      Map.String.empty |> Map.add(
        ~key="7",
        ~value=list{
          (
            "123",
            Ok({
              input: input,
              timestamp: "2019-09-17T12:00:30Z",
              functionResults: list{},
            }),
          ),
        },
      )

    test("returns command for /test GET with headers", () => {
      let headers = Dval.obj(list{
        ("Content-Type", DStr("application/json")),
        ("Authorization", DStr("Bearer abc123")),
      })

      let input =
        Belt.Map.String.empty->Belt.Map.String.set(
          "request",
          Dval.obj(list{
            ("body", DNull),
            ("headers", headers),
            ("url", DStr("http://test-curl.builtwithdark.com/test")),
          }),
        )

      let m = makeModel(
        ~handlers=list{http(~path="/test", ())},
        ~traces=traces(input),
        ~cursorState=Selecting(defaultTLID, None),
        (),
      )

      expect(curlFromCurrentTrace(m, defaultTLID)) |> toEqual(
        Some(
          "curl -H 'Authorization:Bearer abc123' -H 'Content-Type:application/json' -X GET 'http://test-curl.builtwithdark.com/test'",
        ),
      )
    })
    test("returns command for /test POST with body", () => {
      let input = Belt.Map.String.set(
        Belt.Map.String.empty,
        "request",
        Dval.obj(list{
          ("fullBody", DStr("{\"a\":1,\"b\":false}")),
          ("headers", DNull),
          ("url", DStr("http://test-curl.builtwithdark.com/test")),
        }),
      )

      let m = makeModel(
        ~handlers=list{http(~path="/test", ~meth="POST", ())},
        ~traces=traces(input),
        ~cursorState=Selecting(defaultTLID, None),
        (),
      )

      expect(curlFromCurrentTrace(m, defaultTLID)) |> toEqual(
        Some("curl -d '{\"a\":1,\"b\":false}' -X POST 'http://test-curl.builtwithdark.com/test'"),
      )
    })
  })
  ()
}
