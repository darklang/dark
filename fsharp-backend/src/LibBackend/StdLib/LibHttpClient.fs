module LibBackend.StdLib.LibHttpClient

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

// let params =
//   [Param.make "uri" TStr ""; Param.make "body" varA ""; Param.make "query" TObj ""; Param.make "headers" TObj ""]
//
//
// let params_no_body = [Param.make "uri" TStr ""; Param.make "query" TObj ""; Param.make "headers" TObj ""]
//
// type headers = (string * string) list
//
// let has_form_header (headers : headers) : bool =
//   List.exists headers (fun (k, v) ->
//       String.lowercase k = "content-type"
//       && String.lowercase v = "application/x-www-form-urlencoded")
//
//
// let has_json_header (headers : headers) : bool =
//   List.exists headers (fun (k, v) ->
//       String.lowercase k = "content-type"
//       && v
//          |> String.lowercase
//          |> String.is_substring "application/json")
//
//
// let has_plaintext_header (headers : headers) : bool =
//   List.exists headers (fun (k, v) ->
//       String.lowercase k = "content-type"
//       && v |> String.lowercase |> String.is_substring "text/plain")
//
//
// (* Adds a default Content-Type header if one is not provided *)
// let with_default_content_type ~(ct : string) (headers : headers) : headers =
//   if List.Assoc.mem headers String.Caseless.equal "Content-Type"
//   then headers
//   else ("Content-Type", ct) :: headers
//
//
// (* Encodes [body] as a UTF-8 buffer/OCaml string, safe for sending across the internet! Uses
//  * the `Content-Type` header provided by the user in [headers] to make ~magic~ decisions about
//  * how to encode said body. Returns a tuple of the encoded body, and the passed headers that
//  * have potentially had a Content-Type added to them based on the magic decision we've made. *)
// let encode_request_body (headers : headers) (body : dval option) :
//     string option * headers =
//   match body with
//   | Some dv ->
//       let encoded_body, munged_headers =
//         match dv with
//         | DObj _ as dv when has_form_header headers ->
//             (Dval.to_form_encoding dv, headers)
//         (* TODO: DBytes? *)
//         | DStr s ->
//             (* Do nothing to strings, ever. The reasoning here is that users do not expect any
//             * magic to happen to their raw strings. It's also the only real way (barring Bytes) to support
//             * users doing their _own_ encoding (say, jsonifying themselves and passing the Content-Type header
//             * manually).
//             *
//             * See: https://www.notion.so/darklang/Httpclient-Empty-Body-2020-03-10-5fa468b5de6c4261b5dc81ff243f79d9 for
//             * more information. *)
//             ( Unicode_string.to_string s
//             , with_default_content_type "text/plain; charset=utf-8" headers
//             )
//         | dv when has_plaintext_header headers ->
//             (Dval.to_enduser_readable_text_v0 dv, headers)
//         | dv ->
//             (* Otherwise, jsonify (this is the 'easy' API afterall), regardless of headers passed. This makes a little more
//             * sense than you might think on first glance, due to the interaction with the above `DStr` case. Note that this handles
//             * all non-DStr dvals.
//             *
//             * If a user actually _wants_ to use a different Content-Type than the form/plain-text magic provided, they're responsible for
//             * encoding the value to a String first (ie. using the above DStr case) and not just giving us a random dval.
//             *
//             * TODO: Better feedback for user who explicitly provides a Content-Type expecting magic from us
//             * but we don't support it. *)
//             ( Dval.to_pretty_machine_json_v1 dv
//             , with_default_content_type
//                 "application/json; charset=utf-8"
//                 headers )
//       in
//       (* Explicitly convert the empty String to `None`, to ensure downstream we set the right bits on the outgoing cURL request. *)
//       if String.length encoded_body = 0
//       then (None, munged_headers)
//       else (Some encoded_body, munged_headers)
//   (* If we were passed an empty body, we need to ensure a Content-Type was set, or else helpful intermediary load balancers will set
//    * the Content-Type to something they've plucked out of the ether, which is distinctfully non-helpful and also non-deterministic *)
//   | None ->
//       (None, with_default_content_type "text/plain; charset=utf-8" headers)
//
//
// let send_request
//     (uri : string)
//     (verb : Httpclient.verb)
//     (request_body : dval option)
//     (query : dval)
//     (request_headers : dval) : dval =
//   let encoded_query = Dval.dval_to_query query in
//   let encoded_request_headers = Dval.to_string_pairs_exn request_headers in
//   let encoded_request_body, munged_encoded_request_headers =
//     (* We use the user-provided Content-Type headers to make ~magic~ decisions
//     * about how to encode the the outgoing request
//     *
//     * We also _munge_ the headers, specifically to add a Content-Type if none
//     * was provided. This is to ensure we're being good HTTP citizens.
//     * *)
//     encode_request_body encoded_request_headers request_body
//   in
//   match
//     Httpclient.http_call
//       uri
//       encoded_query
//       verb
//       munged_encoded_request_headers
//       encoded_request_body
//   with
//   | Ok res ->
//       let parsed_response_body =
//         if has_form_header res.headers
//         then
//           try Dval.of_form_encoding res.body
//           with _ -> DStr "form decoding error"
//         else if has_json_header res.headers
//         then
//           try Dval.of_unknown_json_v0 res.body
//           with _ -> DStr "json decoding error"
//         else
//           try DStr res.body
//           with _ -> DStr "utf-8 decoding error"
//       in
//       let parsed_response_headers =
//         res.headers
//         |> List.map (fun (k, v) ->
//                (String.strip k, DStr (String.strip v)))
//         |> List.filter (fun (k, _) -> String.length k > 0)
//         |> DvalMap.from_list
//         |> fun dm -> DObj dm
//       in
//       let obj =
//         Dval.to_dobj_exn
//           [ ("body", parsed_response_body)
//           ; ("headers", parsed_response_headers)
//           ; ( "raw"
//             , res.body
//               |> Dval.dstr_of_string
//               |> Option.value
//                    (DStr "utf-8 decoding error") )
//           ; ("code", DInt (Dint.of_int res.code))
//           ; ("error", DStr res.error) ]
//       in
//       if res.code >= 200 && res.code <= 299
//       then DResult (ResOk obj)
//       else DResult (ResError obj)
//   | Error err ->
//       DResult (ResError (DStr err.error))
//
//
// (* This is deprecated in favor of [encode_basic_auth u p]
// * due to using Unicode_string.append_broken.
// *)
// let encode_basic_auth_broken u p =
//   let input =
//     if Unicode_string.is_substring
//          (Unicode_string.of_string_exn "-")
//          u
//     then error "Username cannot contain a colon"
//     else
//       Unicode_string.append_broken
//         (Unicode_string.append_broken u (Unicode_string.of_string_exn ":"))
//         p
//   in
//   let encoded =
//     Unicode_string.of_string_exn
//       (B64.encode
//          B64.default_alphabet
//          true
//          (Unicode_string.to_string input))
//   in
//   Unicode_string.append_broken (Unicode_string.of_string_exn "Basic ") encoded
//
//
// let encode_basic_auth u p =
//   let input =
//     if Unicode_string.is_substring
//          (Unicode_string.of_string_exn "-")
//          u
//     then error "Username cannot contain a colon"
//     else
//       Unicode_string.append
//         (Unicode_string.append u (Unicode_string.of_string_exn ":"))
//         p
//   in
//   let encoded =
//     Unicode_string.of_string_exn
//       (B64.encode
//          B64.default_alphabet
//          true
//          (Unicode_string.to_string input))
//   in
//   Unicode_string.append (Unicode_string.of_string_exn "Basic ") encoded
//
//
// let call verb =
//     (function
//     | _, [DStr uri; body; query; headers] ->
//         send_request
//           (Unicode_string.to_string uri)
//           verb
//           (Some body)
//           query
//           headers
//     | _ ->
//         incorrectArgs ())
//
//
// let call_no_body verb =
//
//     (function
//     | _, [DStr uri; query; headers] ->
//         send_request (Unicode_string.to_string uri) verb None query headers
//     | _ ->
//         incorrectArgs ())
//

let fns : List<BuiltInFn> = []
// [ { name = fn "HttpClient" "post" 0
//
//   ; parameters = params
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.POST
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 0
//   ; parameters = params
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PUT
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 0
//   ; parameters = params
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP GET call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.GET
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 0
//   ; parameters = params
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.DELETE
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 0
//   ; parameters = params
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.OPTIONS
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 0
//   ; parameters = params
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.HEAD
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 0
//   ; parameters = params
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PATCH
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "post" 1
//   ; parameters = params
//   ; returnType = TObj
//   ; description = "Make blocking HTTP POST call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.POST
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 1
//   ; parameters = params
//   ; returnType = TObj
//   ; description = "Make blocking HTTP PUT call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PUT
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 1
//   ; parameters = params_no_body
//   ; returnType = TObj
//   ; description = "Make blocking HTTP GET call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call_no_body
//         Httpclient.GET
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 1
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = params_no_body
//   ; returnType = TObj
//   ; description = "Make blocking HTTP DELETE call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call_no_body
//         Httpclient.DELETE
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 1
//   ; parameters = params_no_body
//   ; returnType = TObj
//   ; description = "Make blocking HTTP OPTIONS call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call_no_body
//         Httpclient.OPTIONS
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 1
//   ; parameters = params_no_body
//   ; returnType = TObj
//   ; description = "Make blocking HTTP HEAD call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call_no_body
//         Httpclient.HEAD
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 1
//   ; parameters = params
//   ; returnType = TObj
//   ; description = "Make blocking HTTP PATCH call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PATCH
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "post" 2
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call
//         Httpclient.POST
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 2
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call
//         Httpclient.PUT
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 2
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.GET
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 2
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.DELETE
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 2
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.OPTIONS
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 2
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.HEAD
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 2
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call
//         Httpclient.PATCH
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "post" 3
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV1.call
//         Httpclient.POST
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 3
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV1.call
//         Httpclient.PUT
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 3
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV1.call_no_body
//         Httpclient.GET
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 3
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV1.call_no_body
//         Httpclient.DELETE
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 3
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV1.call_no_body
//         Httpclient.OPTIONS
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 3
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV1.call_no_body
//         Httpclient.HEAD
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 3
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV1.call
//         Httpclient.PATCH
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "post" 4
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV2.call
//         Httpclient.POST
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 4
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV2.call
//         Httpclient.PUT
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 4
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV2.call_no_body
//         Httpclient.GET
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 4
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV2.call_no_body
//         Httpclient.DELETE
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 4
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV2.call_no_body
//         Httpclient.OPTIONS
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 4
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV2.call_no_body
//         Httpclient.HEAD
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 4
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn =
//       Legacy.LibhttpclientV2.call
//         Httpclient.PATCH
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "post" 5
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call Httpclient.POST
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "put" 5
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call Httpclient.PUT
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "get" 5
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call_no_body Httpclient.GET
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "delete" 5
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call_no_body Httpclient.DELETE
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "options" 5
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call_no_body Httpclient.OPTIONS
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "head" 5
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call_no_body Httpclient.HEAD
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "patch" 5
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call Httpclient.PATCH
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "basicAuth" 0
//   ; parameters = [Param.make "username" TStr ""; Param.make "password" TStr ""]
//   ; returnType = TObj
//   ; description =
//       "Returns an object with 'Authorization' created using HTTP basic auth"
//   ; fn =
//         (function
//         | _, [DStr u; DStr p] ->
//             DObj
//               (Map.singleton
//                  "Authorization"
//                  (DStr (encode_basic_auth_broken u p)))
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) (* Deprecated due to using encode_basic_auth_broken *)
//   }
// ; { name = fn "HttpClient" "basicAuth" 1
//   ; parameters = [Param.make "username" TStr ""; Param.make "password" TStr ""]
//   ; returnType = TObj
//   ; description =
//       "Returns an object with 'Authorization' created using HTTP basic auth"
//   ; fn =
//         (function
//         | _, [DStr u; DStr p] ->
//             DObj
//               (Map.singleton
//                  "Authorization"
//                  (DStr (encode_basic_auth u p)))
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated } ]
//
