open Prelude

// The parameters passed from appsupport.js.

type t = {
  complete: list<RT.BuiltInFn.t>,
  canvasName: string,
  userContentHost: string,
  environment: string,
  csrfToken: string,
  isAdmin: bool,
  buildHash: string,
  username: string,
}

let fromString = (strJ: string): t => {
  open Json.Decode
  let j = Json.parseOrRaise(strJ)
  {
    canvasName: field("canvasName", string, j),
    complete: field("complete", list(RT.BuiltInFn.decode), j),
    userContentHost: field("userContentHost", string, j),
    environment: field("environment", string, j),
    csrfToken: field("csrfToken", string, j),
    isAdmin: field("isAdmin", bool, j),
    buildHash: field("buildHash", string, j),
    username: field("username", string, j),
  }
}
