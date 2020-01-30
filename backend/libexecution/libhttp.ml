open Core_kernel
open Runtime
open Lib

let fns : Lib.shortfn list =
  [ { pns = ["Http::respond"]
    ; ins = []
    ; p = [par "response" TAny; par "code" TInt]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body."
    ; f =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp (Response (Dint.to_int_exn code, []), dv)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* TODO(ian): merge Http::respond with Http::respond_with_headers
   * -- need to figure out how to deprecate functions w/o breaking
   * user code
   *)
    { pns = ["Http::respondWithHeaders"]
    ; ins = []
    ; p = [par "response" TAny; par "headers" TObj; par "code" TInt]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code`, `response` body, and `headers`."
    ; f =
        InProcess
          (function
          | _, [dv; (DObj _ as obj); DInt code] ->
              let pairs = Dval.to_string_pairs_exn obj in
              DResp (Response (Dint.to_int_exn code, pairs), dv)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::success"]
    ; ins = []
    ; p = [par "response" TAny]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status 200 and `response` body."
    ; f =
        InProcess
          (function
          | _, [dv] -> DResp (Response (200, []), dv) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::respondWithHtml"]
    ; ins = []
    ; p = [par "response" TAny; par "code" TInt]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/html\"."
    ; f =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp
                ( Response
                    (Dint.to_int_exn code, [("Content-Type", "text/html")])
                , dv )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::respondWithText"]
    ; ins = []
    ; p = [par "response" TAny; par "code" TInt]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/plain\"."
    ; f =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp
                ( Response
                    (Dint.to_int_exn code, [("Content-Type", "text/plain")])
                , dv )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::respondWithJson"]
    ; ins = []
    ; p = [par "response" TAny; par "code" TInt]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"application/json\""
    ; f =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp
                ( Response
                    ( Dint.to_int_exn code
                    , [("Content-Type", "application/json")] )
                , dv )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::redirectTo"]
    ; ins = []
    ; p = [par "url" TStr]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with a 302 redirect to `url`."
    ; f =
        InProcess
          (function
          | _, [DStr url] ->
              DResp (Redirect (Unicode_string.to_string url), DNull)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::badRequest"]
    ; ins = []
    ; p = [par "error" TStr]
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with a 400 status and string `error` message."
    ; f =
        InProcess
          (function
          | _, [DStr msg] ->
              DResp (Response (400, []), DStr msg)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::notFound"]
    ; ins = []
    ; p = []
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with 404 Not Found."
    ; f =
        InProcess
          (function
          | _, [] -> DResp (Response (404, []), DNull) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::unauthorized"]
    ; ins = []
    ; p = []
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with 401 Unauthorized."
    ; f =
        InProcess
          (function
          | _, [] -> DResp (Response (401, []), DNull) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::forbidden"]
    ; ins = []
    ; p = []
    ; r = TResp
    ; d =
        "Returns a Response that can be returned from an HTTP handler to respond with 403 Forbidden."
    ; f =
        InProcess
          (function
          | _, [] -> DResp (Response (403, []), DNull) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Http::setCookie"]
    ; ins = []
    ; p = [par "name" TStr; par "value" TStr; par "params" TObj]
    ; r = TObj
    ; d =
        "Generate an HTTP Set-Cookie header Object suitable for Http::respondWithHeaders given a cookie name, a string value for it, and an Object of Set-Cookie parameters."
    ; f =
        InProcess
          (function
          | _, [DStr name; DStr value; DObj o] ->
              o
              (* Transform a DOBj into a cookie list of individual cookie params *)
              |> Map.to_alist
              |> List.concat_map ~f:(fun (x, y) ->
                     match (String.lowercase x, y) with
                     (* Single boolean set-cookie params *)
                     | "secure", DBool b | "httponly", DBool b ->
                         if b then [x] else []
                     (* X=y set-cookie params *)
                     | "path", DStr str
                     | "domain", DStr str
                     | "samesite", DStr str ->
                         [ Format.sprintf
                             "%s=%s"
                             x
                             (Unicode_string.to_string str) ]
                     | "max-age", DInt i | "expires", DInt i ->
                         [Format.sprintf "%s=%s" x (Dint.to_string i)]
                     (* Throw if there's not a good way to transform the k/v pair *)
                     | _ ->
                         y
                         |> Dval.to_developer_repr_v0
                         |> Format.sprintf "Unknown set-cookie param: %s: %s" x
                         |> Exception.code)
              (* Combine it into a set-cookie header *)
              |> String.concat ~sep:"; "
              |> Format.sprintf
                   "%s=%s; %s"
                   (Uri.pct_encode (Unicode_string.to_string name))
                   (Uri.pct_encode (Unicode_string.to_string value))
              |> Dval.dstr_of_string_exn
              |> fun x -> Dval.to_dobj_exn [("Set-Cookie", x)]
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Http::setCookie_v1"]
    ; ins = []
    ; p = [par "name" TStr; par "value" TStr; par "params" TObj]
    ; r = TObj
    ; d =
        "Generate an HTTP Set-Cookie header Object suitable for Http::respondWithHeaders given a cookie name, a string value for it, and an Object of Set-Cookie parameters."
    ; f =
        InProcess
          (function
          | _, [DStr name; DStr value; DObj o] ->
              o
              (* Transform a DOBj into a cookie list of individual cookie params *)
              |> Map.to_alist
              |> List.concat_map ~f:(fun (x, y) ->
                     match (String.lowercase x, y) with
                     (* Single boolean set-cookie params *)
                     | "secure", DBool b | "httponly", DBool b ->
                         if b then [x] else []
                     (* X=y set-cookie params *)
                     | "path", DStr str
                     | "domain", DStr str
                     | "samesite", DStr str ->
                         [ Format.sprintf
                             "%s=%s"
                             x
                             (Unicode_string.to_string str) ]
                     | "max-age", DInt i | "expires", DInt i ->
                         [Format.sprintf "%s=%s" x (Dint.to_string i)]
                     (* Throw if there's not a good way to transform the k/v pair *)
                     | _ ->
                         y
                         |> Dval.to_developer_repr_v0
                         |> Format.sprintf "Unknown set-cookie param: %s: %s" x
                         |> Exception.code)
              (* Combine it into a set-cookie header *)
              |> String.concat ~sep:"; "
              |> Format.sprintf
                   "%s=%s; %s"
                   (* DO NOT ESCAPE THESE VALUES; pctencoding is tempting (see
                    * the implicit _v0, and
                    * https://github.com/darklang/dark/pull/1917 for a
                    * discussion of the bug), but incorrect. By the time it's
                    * reached Http::setCookie_v1,  you've probably already
                    * stored the cookie value as-is in a datastore somewhere, so
                    * any changes will break attempts to look up the session.
                    *
                    * If you really want to shield against invalid
                    * cookie-name/cookie-value strings, go read RFC6265 first. *)
                   (Unicode_string.to_string name)
                   (Unicode_string.to_string value)
              |> Dval.dstr_of_string_exn
              |> fun x -> Dval.to_dobj_exn [("Set-Cookie", x)]
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
