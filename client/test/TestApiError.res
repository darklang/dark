open Prelude
open Tester

module Http = Tea.Http
module StringMap = Caml.Map.Make(Caml.String)

let addOpsError: APIError.t = {
  context: "AddOps",
  originalError: Http.BadStatus({
    url: "url",
    headers: StringMap.empty,
    body: StringResponse("body response"),
    status: {code: 502, message: "Error msg"},
  }),
  requestParams: None,
  reload: false,
  importance: IgnorableError,
}

let other502Error: APIError.t = {
  context: "Error context",
  originalError: Http.BadStatus({
    url: "url",
    headers: StringMap.empty,
    body: StringResponse("body response"),
    status: {code: 502, message: "Error msg"},
  }),
  requestParams: None,
  reload: false,
  importance: IgnorableError,
}

let networkError: APIError.t = {
  context: "Network error context",
  originalError: Http.NetworkError,
  requestParams: None,
  reload: false,
  importance: IgnorableError,
}

let run = () => {
  describe("msg", () => {
    test("502 AddOps", () =>
      expect(APIError.msg(addOpsError)) |> toEqual(
        "We're sorry, but we were unable to save your most recent edit. Please refresh and try again.",
      )
    )
    test("502 other than AddOps", () =>
      expect(APIError.msg(other502Error)) |> toEqual(
        "Bad status: Error msg - body response (Error context)",
      )
    )
    test("NetworkError msg", () =>
      expect(APIError.msg(networkError)) |> toEqual(
        "Network error - is the server running? (Network error context)",
      )
    )
    ()
  })
  ()
}
