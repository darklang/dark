open Prelude

// Tea
module Cmd = Tea.Cmd
module Http = Tea.Http

@ppx.deriving(show)
type rec errorImportance =
  | IgnorableError
  | ImportantError

@ppx.deriving(show)
type rec t = {
  context: string,
  originalError: httpError /* the Tea_http error */,
  requestParams: option<@opaque Js.Json.t>,
  reload: bool,
  importance: errorImportance,
}

let serverVersionOf = (e: t): option<string> =>
  switch e.originalError {
  | BadUrl(_) | Timeout | NetworkError | Aborted => None
  | BadStatus(response) | BadPayload(_, response) =>
    module StringMap = Caml.Map.Make(Tc.Caml.String)
    response.headers
    |> StringMap.find_first_opt(key => String.toLowercase(key) == "x-darklang-server-version")
    |> Option.map(~f=Tuple2.second)
  }

let urlOf = (e: t): option<string> =>
  switch e.originalError {
  | Http.BadUrl(url) => Some(url)
  | Http.BadStatus(response) | Http.BadPayload(_, response) => Some(response.url)
  | Http.Aborted | Http.Timeout | Http.NetworkError => None
  }

let shouldDisplayToUser = (e: t): bool =>
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

let shouldRollbar = (e: t): bool =>
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

let isBadAuth = (e: t): bool =>
  if e.reload {
    true
  } else {
    switch e.originalError {
    | Http.BadStatus(response) => response.status.code == 401
    | _ => false
    }
  }

let msg = (e: t): string => {
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
