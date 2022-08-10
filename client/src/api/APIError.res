open Prelude

// Tea
module Cmd = Tea.Cmd
module Http = Tea.Http

let serverVersionOf = (e: apiError): option<string> =>
  switch e.originalError {
  | BadUrl(_) | Timeout | NetworkError | Aborted => None
  | BadStatus(response) | BadPayload(_, response) =>
    module StringMap = Caml.Map.Make(Tc.Caml.String)
    response.headers
    |> StringMap.find_first_opt(key => String.toLowercase(key) == "x-darklang-server-version")
    |> Option.map(~f=Tuple2.second)
  }

let urlOf = (e: apiError): option<string> =>
  switch e.originalError {
  | Http.BadUrl(url) => Some(url)
  | Http.BadStatus(response) | Http.BadPayload(_, response) => Some(response.url)
  | Http.Aborted | Http.Timeout | Http.NetworkError => None
  }

let shouldDisplayToUser = (e: apiError): bool =>
  switch e.originalError {
  | Http.BadUrl(_) | Http.BadPayload(_) => true
  | Http.Timeout | Http.NetworkError | Http.Aborted => e.importance == ImportantError
  | Http.BadStatus(response) =>
    if response.status.code == 502 {
      e.importance == ImportantError
    } else {
      true
    }
  }

let shouldRollbar = (e: apiError): bool =>
  switch e.originalError {
  | Http.BadUrl(_) | Http.Timeout | Http.BadPayload(_) => true
  | Http.NetworkError => // Don't rollbar if the internet is down
    false
  | Http.BadStatus(response) =>
    // Don't rollbar if you aren't logged in
    response.status.code != 401
  | Http.Aborted => // Don't rollbar if the client aborted the request
    false
  }

let parseResponse = (body: Http.responseBody): string => {
  let maybe = (name, m) =>
    switch m {
    | Some(s) => ", " ++ (name ++ (": " ++ s))
    | None => ""
    }

  let str = switch body {
  | NoResponse => "todo-noresponse"
  | StringResponse(str) => str
  | ArrayBufferResponse(_) => "todo-arratbufferresponse"
  | BlobResponse(_) => "todo-blobresponse"
  | DocumentResponse(_) => "todo-document-response"
  | JsonResponse(_) => "todo-jsonresponse"
  | TextResponse(str) => str
  | RawResponse(str, _) => str
  }

  str
  |> Json.Decode.decodeString(Decoders.exception_)
  |> Result.toOption
  |> Option.map(~f=({
    short,
    long,
    exceptionTipe,
    actual,
    actualType,
    expected,
    result,
    resultType,
    info,
    workarounds,
  }) =>
    " (" ++
    (exceptionTipe ++
    ("): " ++
    (short ++
    (maybe("message", long) ++
    (maybe("actual value", actual) ++
    (maybe("actual type", actualType) ++
    (maybe("result", result) ++
    (maybe("result type", resultType) ++
    (maybe("expected", expected) ++
    (if info == Map.String.empty {
      ""
    } else {
      ", info: " ++ Map.toString(info)
    } ++ if workarounds == list{} {
      ""
    } else {
      ", workarounds: [" ++ (String.join(~sep="", workarounds) ++ "]")
    }))))))))))
  )
  |> Option.unwrap(~default=str)
}

let isBadAuth = (e: apiError): bool =>
  if e.reload {
    true
  } else {
    switch e.originalError {
    | Http.BadStatus(response) => response.status.code == 401
    | _ => false
    }
  }

let msg = (e: apiError): string => {
  let (withoutContext, context) = switch e.originalError {
  | Http.BadUrl(str) => ("Bad url: " ++ str, e.context)
  | Http.Timeout => ("Timeout", e.context)
  | Http.NetworkError if e.context == "TriggerSendInviteCallback" => (
      "Network error - Please contact Dark",
      e.context,
    )
  | Http.NetworkError => ("Network error - is the server running?", e.context)
  | Http.BadStatus(response) =>
    if response.status.code == 502 && e.context == "AddOps" {
      (
        "We're sorry, but we were unable to save your most recent edit. Please refresh and try again.",
        "",
      )
    } else {
      (
        "Bad status: " ++ (response.status.message ++ (" - " ++ parseResponse(response.body))),
        e.context,
      )
    }
  | Http.BadPayload(msg, _) => ("Bad payload : " ++ msg, e.context)
  | Http.Aborted => ("Request Aborted", e.context)
  }

  if context == "" {
    withoutContext
  } else {
    withoutContext ++ (" (" ++ (context ++ ")"))
  }
}

let make = (~requestParams=?, ~reload, ~context, ~importance, originalError) => {
  requestParams: requestParams,
  importance: importance,
  originalError: originalError,
  context: context,
  reload: reload,
}

let sendRollbar = (m: AppTypes.model, e: apiError): unit => {
  let customContext = (e: apiError, state: AppTypes.CursorState.t): Js.Json.t => {
    let parameters = Option.unwrap(~default=Js.Json.null, e.requestParams)
    Json_encode_extended.object_(list{
      ("httpResponse", Encoders.httpError(e.originalError)),
      ("parameters", parameters),
      ("cursorState", AppTypes.CursorState.encode(state)),
    })
  }
  Rollbar.send(msg(e), urlOf(e), customContext(e, m.cursorState))
}

let handle = (m: AppTypes.model, apiError: Types.apiError): (AppTypes.model, AppTypes.cmd) => {
  let now = Js.Date.now() |> Js.Date.fromFloat
  let shouldReload = {
    let buildHashMismatch =
      serverVersionOf(apiError)
      |> Option.map(~f=hash => hash != m.buildHash)
      |> Option.unwrap(~default=false)

    let reloadAllowed = switch m.lastReload {
    | Some(time) =>
      // if 60 seconds have elapsed
      Js.Date.getTime(time) +. 60000.0 > Js.Date.getTime(now)
    | None => true
    }

    // Reload if it's an auth failure or the frontend is out of date
    isBadAuth(apiError) || (buildHashMismatch && reloadAllowed)
  }

  let ignore = {
    // Ignore when using Ngrok
    let usingNgrok = VariantTesting.variantIsActive(m, NgrokVariant)
    // This message is deep in the server code and hard to pull
    // out, so just ignore for now
    Js.log("Already at latest redo - ignoring server error")
    let redoError = String.includes(msg(apiError), ~substring="(client): Already at latest redo")

    redoError || usingNgrok
  }

  let cmd = if shouldReload {
    let m = {...m, lastReload: Some(now)}
    // Previously, this was two calls to Tea_task.nativeBinding. But
    // only the first got called, unclear why
    Cmd.call(_ => {
      SavedSettings.save(m)
      SavedUserSettings.save(m)
      Native.Location.reload(true)
    })
  } else if !ignore && shouldRollbar(apiError) {
    Cmd.call(_ => sendRollbar(m, apiError))
  } else {
    Cmd.none
  }

  let newM = {
    let error = if shouldDisplayToUser(apiError) && !ignore {
      Error.set(msg(apiError), m.error)
    } else {
      m.error
    }

    let lastReload = if shouldReload {
      Some(now)
    } else {
      m.lastReload
    }
    {...m, error: error, lastReload: lastReload}
  }
  (newM, cmd)
}
