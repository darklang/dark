open Core_kernel
open Runtime
open Lib

let fns : Types.RuntimeT.expr Types.RuntimeT.fn list =
  [ { prefix_names = ["Http::respond"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body."
    ; func =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp (Response (Dint.to_int_exn code, []), dv)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Http::response"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body."
    ; func =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp (Response (Dint.to_int_exn code, []), dv)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
    (* TODO(ian): merge Http::respond with Http::respond_with_headers
   * -- need to figure out how to deprecate functions w/o breaking
   * user code
   *)
  ; { prefix_names = ["Http::respondWithHeaders"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "headers" TObj; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code`, `response` body, and `headers`."
    ; func =
        InProcess
          (function
          | _, [dv; (DObj _ as obj); DInt code] ->
              let pairs = Dval.to_string_pairs_exn obj in
              DResp (Response (Dint.to_int_exn code, pairs), dv)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Http::responseWithHeaders"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "headers" TObj; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code`, `response` body, and `headers`."
    ; func =
        InProcess
          (function
          | _, [dv; (DObj _ as obj); DInt code] ->
              let pairs = Dval.to_string_pairs_exn obj in
              DResp (Response (Dint.to_int_exn code, pairs), dv)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::success"]
    ; infix_names = []
    ; parameters = [par "response" TAny]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status 200 and `response` body."
    ; func =
        InProcess
          (function
          | _, [dv] -> DResp (Response (200, []), dv) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::respondWithHtml"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/html\"."
    ; func =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp
                ( Response
                    (Dint.to_int_exn code, [("Content-Type", "text/html")])
                , dv )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Http::responseWithHtml"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/html\"."
    ; func =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp
                ( Response
                    (Dint.to_int_exn code, [("Content-Type", "text/html")])
                , dv )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::respondWithText"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/plain\"."
    ; func =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp
                ( Response
                    (Dint.to_int_exn code, [("Content-Type", "text/plain")])
                , dv )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Http::responseWithText"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"text/plain\"."
    ; func =
        InProcess
          (function
          | _, [dv; DInt code] ->
              DResp
                ( Response
                    (Dint.to_int_exn code, [("Content-Type", "text/plain")])
                , dv )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::respondWithJson"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"application/json\""
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Http::responseWithJson"]
    ; infix_names = []
    ; parameters = [par "response" TAny; par "code" TInt]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with HTTP status `code` and `response` body, with `content-type` set to \"application/json\""
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::redirectTo"]
    ; infix_names = []
    ; parameters = [par "url" TStr]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with a 302 redirect to `url`."
    ; func =
        InProcess
          (function
          | _, [DStr url] ->
              DResp (Redirect (Unicode_string.to_string url), DNull)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::badRequest"]
    ; infix_names = []
    ; parameters = [par "error" TStr]
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with a 400 status and string `error` message."
    ; func =
        InProcess
          (function
          | _, [DStr msg] ->
              DResp (Response (400, []), DStr msg)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::notFound"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with 404 Not Found."
    ; func =
        InProcess
          (function
          | _, [] -> DResp (Response (404, []), DNull) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::unauthorized"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with 401 Unauthorized."
    ; func =
        InProcess
          (function
          | _, [] -> DResp (Response (401, []), DNull) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::forbidden"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TResp
    ; description =
        "Returns a Response that can be returned from an HTTP handler to respond with 403 Forbidden."
    ; func =
        InProcess
          (function
          | _, [] -> DResp (Response (403, []), DNull) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Http::setCookie"]
    ; infix_names = []
    ; parameters = [par "name" TStr; par "value" TStr; par "params" TObj]
    ; return_type = TObj
    ; description =
        "Generate an HTTP Set-Cookie header Object suitable for Http::responseWithHeaders given a cookie name, a string value for it, and an Object of Set-Cookie parameters."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Http::setCookie_v1"]
    ; infix_names = []
    ; parameters = [par "name" TStr; par "value" TStr; par "params" TObj]
    ; return_type = TObj
    ; description =
        "Generate an HTTP Set-Cookie header Object suitable for Http::responseWithHeaders given a cookie name, a string value for it, and an Object of Set-Cookie parameters."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Http::setCookie_v2"]
    ; infix_names = []
    ; parameters = [par "name" TStr; par "value" TStr; par "params" TObj]
    ; return_type = TObj
    ; description =
        "Generate an HTTP Set-Cookie header Object suitable for Http::responseWithHeaders given a cookie name, a string value for it, and an Object of Set-Cookie parameters."
    ; func =
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
                     | "max-age", DInt i ->
                         [Format.sprintf "%s=%s" x (Dint.to_string i)]
                     | "expires", DDate d ->
                         [ Format.sprintf
                             "%s=%s"
                             x
                             (Util.http_date_string_of_date d) ]
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
    ; preview_safety = Safe
    ; deprecated = false } ]
