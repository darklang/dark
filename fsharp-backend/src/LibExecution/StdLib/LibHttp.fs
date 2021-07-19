module LibExecution.StdLib.LibHttp

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude
open System

module Errors = LibExecution.Errors
module DvalRepr = LibExecution.DvalRepr

let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "Http" "respond" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body."
      fn =
        (function
        | _, [ dv; DInt code ] -> Value(DHttpResponse(Response(code, [], dv)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Http" "response" 0) }
    { name = fn "Http" "response" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body."
      fn =
        (function
        | _, [ dv; DInt code ] -> Value(DHttpResponse(Response(code, [], dv)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    // TODO(ian): merge Http::respond with Http::respond_with_headers
    //  -- need to figure out how to deprecate functions w/o breaking
    //  user code
    { name = fn "Http" "respondWithHeaders" 0
      parameters =
        [ Param.make "response" varA ""
          Param.make "headers" (TDict varA) ""
          Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code`, `response` body, and `headers`."
      fn =
        (function
        | _, [ dv; DObj o; DInt code ] ->
            let pairs =
              Map.toList o
              |> List.map
                   (fun (k, v) ->
                     match k, v with
                     | k, DStr v -> k, v
                     | k, v ->
                         Errors.throw (Errors.argumentWasnt "a string" "value" v))

            Value(DHttpResponse(Response(code, pairs, dv)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Http" "responseWithHeaders" 0) }
    { name = fn "Http" "responseWithHeaders" 0
      parameters =
        [ Param.make "response" varA ""
          Param.make "headers" (TDict varA) ""
          Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code`, `response` body, and `headers`."
      fn =
        (function
        | _, [ dv; DObj o; DInt code ] ->
            let pairs =
              Map.toList o
              |> List.map
                   (fun (k, v) ->
                     match k, v with
                     | k, DStr v -> k, v
                     | k, v ->
                         Errors.throw (Errors.argumentWasnt "a string" "value" v))

            Value(DHttpResponse(Response(code, pairs, dv)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "success" 0
      parameters = [ Param.make "response" varA "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status 200 and `response` body."
      fn =
        (function
        | _, [ dv ] -> Value(DHttpResponse(Response(200I, [], dv)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "respondWithHtml" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/html\"."
      fn =
        (function
        | _, [ dv; DInt code ] ->
            Value(
              DHttpResponse(Response(code, [ ("Content-Type", "text/html") ], dv))
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Http" "responseWithHtml" 0) }
    { name = fn "Http" "responseWithHtml" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/html\"."
      fn =
        (function
        | _, [ dv; DInt code ] ->
            Value(
              DHttpResponse(Response(code, [ ("Content-Type", "text/html") ], dv))
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "respondWithText" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/plain\"."
      fn =
        (function
        | _, [ dv; DInt code ] ->
            Value(
              DHttpResponse(Response(code, [ ("Content-Type", "text/plain") ], dv))
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Http" "responseWithText" 0) }
    { name = fn "Http" "responseWithText" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/plain\"."
      fn =
        (function
        | _, [ dv; DInt code ] ->
            Value(
              DHttpResponse(Response(code, [ ("Content-Type", "text/plain") ], dv))
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "respondWithJson" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"application/json\""
      fn =
        (function
        | _, [ dv; DInt code ] ->
            Value(
              DHttpResponse(
                Response(code, [ ("Content-Type", "application/json") ], dv)
              )
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Http" "responseWithJson" 0) }
    { name = fn "Http" "responseWithJson" 0
      parameters = [ Param.make "response" varA ""; Param.make "code" TInt "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"application/json\""
      fn =
        (function
        | _, [ dv; DInt code ] ->
            Value(
              DHttpResponse(
                Response(code, [ ("Content-Type", "application/json") ], dv)
              )
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "redirectTo" 0
      parameters = [ Param.make "url" TStr "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with a 302 redirect to `url`."
      fn =
        (function
        | _, [ DStr url ] -> Value(DHttpResponse(Redirect url))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "badRequest" 0
      parameters = [ Param.make "error" TStr "" ]
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with a 400 status and string `error` message."
      fn =
        (function
        | _, [ DStr _ as msg ] -> Value(DHttpResponse(Response(400I, [], msg)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "notFound" 0
      parameters = []
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with 404 Not Found."
      fn =
        (function
        | _, [] -> Value(DHttpResponse(Response(404I, [], DNull)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "unauthorized" 0
      parameters = []
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with 401 Unauthorized."
      fn =
        (function
        | _, [] -> Value(DHttpResponse(Response(401I, [], DNull)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "forbidden" 0
      parameters = []
      returnType = THttpResponse varA
      description =
        "Returns a Response that can be returned from an HTTP handler to respond with 403 Forbidden."
      fn =
        (function
        | _, [] -> Value(DHttpResponse(Response(403I, [], DNull)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "setCookie" 0
      parameters =
        [ Param.make "name" TStr ""
          Param.make "value" TStr ""
          Param.make "params" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Generate an HTTP Set-Cookie header Object suitable for Http::responseWithHeaders given a cookie name, a string value for it, and an Object of Set-Cookie parameters."
      fn =
        (function
        | _, [ DStr name; DStr value; DObj o ] ->
            o
            // Transform a DOBj into a cookie list of individual cookie params
            |> Map.toList
            |> List.map
                 (fun (x, y) ->
                   match (String.toLower x, y) with
                   // Single boolean set-cookie params
                   | "secure", DBool b
                   | "httponly", DBool b -> if b then [ x ] else []
                   // X=y set-cookie params
                   | "path", DStr str
                   | "domain", DStr str
                   | "samesite", DStr str -> [ sprintf "%s=%s" x str ]
                   | "max-age", DInt i
                   | "expires", DInt i -> [ sprintf "%s=%s" x (string i) ]
                   // Throw if there's not a good way to transform the k/v pair
                   | _ ->
                       Errors.throw
                         $"Unknown set-cookie param: {x}: {
                                                             DvalRepr.toDeveloperReprV0
                                                               y
                         }")
            // Combine it into a set-cookie header
            |> List.concat
            |> String.concat "; "
            |> sprintf
                 "%s=%s; %s"
                 (Uri.EscapeDataString name)
                 (Uri.EscapeDataString value)
            |> DStr
            |> fun x -> Map.add "Set-Cookie" x Map.empty
            |> DObj
            |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Http" "setCookie" 1) }
    { name = fn "Http" "setCookie" 1
      parameters =
        [ Param.make "name" TStr ""
          Param.make "value" TStr ""
          Param.make "params" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Generate an HTTP Set-Cookie header Object suitable for Http::responseWithHeaders given a cookie name, a string value for it, and an Object of Set-Cookie parameters."
      fn =
        (function
        | _, [ DStr name; DStr value; DObj o ] ->
            o
            // Transform a DOBj into a cookie list of individual cookie params
            |> Map.toList
            |> List.map
                 (fun (x, y) ->
                   match (String.toLower x, y) with
                   // Single boolean set-cookie params
                   | "secure", DBool b
                   | "httponly", DBool b -> if b then [ x ] else []
                   // X=y set-cookie params
                   | "path", DStr str
                   | "domain", DStr str
                   | "samesite", DStr str -> [ sprintf "%s=%s" x str ]
                   | "max-age", DInt i
                   | "expires", DInt i -> [ sprintf "%s=%s" x (string i) ]
                   // Throw if there's not a good way to transform the k/v pair
                   | _ ->
                       Errors.throw
                         $"Unknown set-cookie param: {x}: {
                                                             DvalRepr.toDeveloperReprV0
                                                               y
                         }")
            // Combine it into a set-cookie header
            |> List.concat
            |> String.concat "; "
            |> sprintf "%s=%s; %s" name value
            // DO NOT ESCAPE THESE VALUES; pctencoding is tempting (see
            // the implicit _v0, and
            // https://github.com/darklang/dark/pull/1917 for a
            // discussion of the bug), but incorrect. By the time it's
            // reached Http::setCookie_v1,  you've probably already
            // stored the cookie value as-is in a datastore somewhere, so
            // any changes will break attempts to look up the session.
            //
            // If you really want to shield against invalid
            // cookie-name/cookie-value strings, go read RFC6265 first.
            |> DStr
            |> fun x -> Map.add "Set-Cookie" x Map.empty
            |> DObj
            |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Http" "setCookie" 2) }
    { name = fn "Http" "setCookie" 2
      parameters =
        [ Param.make "name" TStr ""
          Param.make "value" TStr ""
          Param.make "params" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Returns an HTTP Set-Cookie header <type Dict> suitable for use with <fn Http::responseWithHeaders>, given a cookie <param name>, a <type String> <param value> for it, and a <type Dict> of Set-Cookie <param params> ({{Expires}}, {{Max-Age}}, {{Domain}}, {{Path}}, {{Secure}}, {{HttpOnly}}, and/or {{SameSite}})."
      fn =
        (function
        | state, [ DStr name; DStr value; DObj o ] ->

            let fold_cookie_params acc key value =
              match (String.toLower key, value) with
              // Bubble up errors for values that are invalid for all params
              | _,
                ((DIncomplete _
                | DErrorRail _
                | DError _) as dv) -> Errors.foundFakeDval dv
              // Single boolean set-cookie params
              | "secure", v
              | "httponly", v ->
                  (match v with
                   | DBool b -> if b then (key :: acc) else acc
                   | _ ->
                       Errors.throw (
                         Errors.argumentWasnt
                           "`true` or `false`"
                           "Secure or HttpOnly"
                           v
                       ))
              // key=data set-cookie params
              | "path", v
              | "domain", v ->
                  (match v with
                   | DStr str -> (sprintf "%s=%s" key str :: acc)
                   | _ ->
                       Errors.throw (
                         Errors.argumentWasnt "a string" "`Path` or `Domain`" v
                       ))
              | "samesite", v ->
                  (match v with
                   | DStr str when
                     List.contains (String.toLower str) [ "strict"; "lax"; "none" ] ->
                       (sprintf "%s=%s" key str :: acc)
                   | _ ->
                       Errors.throw (
                         Errors.argumentWasnt
                           "`Strict`, `Lax`, or `None`"
                           "SameSite"
                           v
                       ))
              | "max-age", v ->
                  (match v with
                   | DInt i -> (sprintf "%s=%s" key (string i) :: acc)
                   | _ ->
                       Errors.throw (
                         Errors.argumentWasnt
                           "a `Int` representing seconds"
                           "Max-Age"
                           v
                       ))
              | "expires", v ->
                  (match v with
                   | DDate d ->
                       (sprintf
                         "%s=%s"
                         key
                         (d.ToString("ddd, dd MMM yyyy HH':'mm':'ss 'GMT'"))
                        :: acc)
                   | _ -> Errors.throw (Errors.argumentWasnt "a date" "Expires" v))
              // Error if the set-cookie parameter is invalid
              | _ ->
                  Errors.throw (
                    $"Keys must be `Expires`, `Max-Age`, `Domain`, `Path`, `Secure`, `HttpOnly`, and/or `SameSite`, but one of the keys was {
                                                                                                                                               key
                    }"
                  )

            let nameValue = sprintf "%s=%s" name value
            // DO NOT ESCAPE THESE VALUES; pctencoding is tempting (see
            // the implicit _v0, and
            // https://github.com/darklang/dark/pull/1917 for a
            // discussion of the bug), but incorrect. By the time it's
            // reached Http::setCookie_v1,  you've probably already
            // stored the cookie value as-is in a datastore somewhere, so
            // any changes will break attempts to look up the session.
            //
            // If you really want to shield against invalid
            // cookie-name/cookie-value strings, go read RFC6265 first.
            let cookieParams = Map.fold fold_cookie_params [] o

            nameValue :: cookieParams
            |> String.concat "; "
            |> DStr
            |> fun x -> Map.add "Set-Cookie" x Map.empty
            |> DObj
            |> Value

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
