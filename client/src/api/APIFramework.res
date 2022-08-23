// APIFramework is just calling conventions to use APIs, so that the actual calls can
// be put in components. This differs from the old way of doing it (which is in
// API.res), where all the API calls are defined in one place (which makes it hard to
// have separation both there and in other parts of the codebase)

type apiShape<'a> = {
  payload: 'a,
  canvasName: string,
}

// Global data about the client, which is stored in the model
type clientData = {csrfToken: string, canvasName: string, version: string, origin: string}

module Callable = {
  @ppx.deriving(show)
  type rec t<'msg> = clientData => Tea.Cmd.t<'msg>

  let map = (c: t<'msg1>, f: 'msg1 => 'msg2): t<'msg2> => {
    clientData => {
      Tea.Cmd.map(f, c(clientData))
    }
  }
}

let editorApiCall = (
  ~method: string,
  ~encoder: option<'paramtype => Js.Json.t>=?,
  ~params: 'paramtype=?,
  ~decoder: Js.Json.t => 'resulttype,
  ~callback: Tea.Result.t<'resulttype, Tea.Http.error<string>> => 'msg,
  endpoint: string,
): Callable.t<'msg> => {
  (clientData: clientData) => {
    let url = "https://editor.darklang.com/api/private/" ++ endpoint
    let body = switch encoder {
    | Some(encoder) =>
      let encodedBody = {
        open Json.Encode
        object_(list{("canvasName", string(clientData.canvasName)), ("payload", encoder(params))})
      }
      Web.XMLHttpRequest.StringBody(Json.stringify(encodedBody))
    | None => Web.XMLHttpRequest.EmptyBody
    }
    let request = Tea.Http.request({
      method': method,
      headers: list{
        Header("Content-type", "application/json"),
        Header("X-CSRF-Token", clientData.csrfToken),
        Header("x-darklang-client-version", clientData.version),
      },
      url,
      body,
      expect: Tea.Http.expectStringResponse(Decoders.wrapExpect(decoder)),
      timeout: None,
      withCredentials: true,
    })

    // If origin is https://darklang.com, then we're in prod (or ngrok, running against
    // prod) and editor.darklang.com's CORS rules will allow this request. If not,
    // we're in local, and both CORS and auth (session, canvas_id) will not work
    // against editor.darklang.com. By putting the conditional here instead of at the
    // beginning of the function, we still exercise the message and request generating
    // code locally.
    if clientData.origin == "https://darklang.com" {
      Tea.Http.send(callback, request)
    } else {
      Tea.Cmd.none
    }
  }
}

let editorGet = (
  ~decoder: Js.Json.t => 'resulttype,
  ~callback: Tea.Result.t<'resulttype, Tea.Http.error<string>> => 'msg,
  endpoint: string,
) => {
  editorApiCall(~method="GET", ~decoder, ~callback, endpoint)
}
