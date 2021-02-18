module LibBackend.StdLib.LibTwilio

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

let fns : List<BuiltInFn> = []
  // [ { name = fn "Twilio" "sendText" 0
  //
  //   ; parameters =
  //       [ Param.make "accountSID" TStr
  //       ; Param.make "authToken" TStr
  //       ; Param.make "fromNumber" TStr
  //       ; Param.make "toNumber" TStr
  //       ; Param.make "body" TStr ]
  //   ; returnType = TObj
  //   ; description =
  //       "Send text with `body` to phone number `toNumber` from number `fromNumber`, authenticated via `accountSID` and `authToken`"
  //   ; fn =
  //
  //         (function
  //         | ( s
  //           , [ DStr accountSID
  //             ; DStr authToken
  //             ; DStr fromNumber
  //             ; DStr toNumber
  //             ; DStr body ] ) ->
  //             let basic_auth_string =
  //               Libhttpclient.encode_basic_auth_broken accountSID authToken
  //             in
  //             let encoding = "application/x-www-form-urlencoded" in
  //             let headers =
  //               [ ("Authorization", DStr basic_auth_string)
  //               ; ("Content-Type", Libexecution.Dval.dstr_of_string_exn encoding)
  //               ]
  //               |> DvalMap.from_list
  //               |> DObj
  //             in
  //             let host_url = Canvas.url_for s.canvas_id in
  //             let body =
  //               [ ("From", DStr fromNumber)
  //               ; ("To", DStr toNumber)
  //               ; ("Body", DStr body)
  //               ; ("ValidityPeriod", Libexecution.Dval.dstr_of_string_exn "900")
  //               ; ( "StatusCallback"
  //                 , Libexecution.Dval.dstr_of_string_exn
  //                     (host_url ^ "/twilioCallback") ) ]
  //               |> DvalMap.fromList
  //               |> DObj
  //             in
  //             let twilio_uri =
  //               "https://api.twilio.com/2010-04-01/Accounts/"
  //               ^ Libexecution.Unicode_string.to_string accountSID
  //               ^ "/Messages.json"
  //             in
  //             Legacy.LibhttpclientV2.send_request
  //               twilio_uri
  //               Httpclient.POST
  //               Libexecution.Dval.to_pretty_machine_json_v1
  //               body
  //               (DObj DvalMap.empty)
  //               headers
  //         | _ ->
  //             incorrectArgs ())
  //   ; sqlSpec = NotYetImplementedTODO
  //   ; previewable = Impure
  //   ; deprecated =
  //       true
  //       (* Deprecated due to using Libhttpclient.encode_basic_auth_broken *) }
  // ; { name = fn "Twilio" "sendText" 1
  //
  //   ; parameters =
  //       [ Param.make "accountSID" TStr
  //       ; Param.make "authToken" TStr
  //       ; Param.make "fromNumber" TStr
  //       ; Param.make "toNumber" TStr
  //       ; Param.make "body" TStr ]
  //   ; returnType = TObj
  //   ; description =
  //       "Send text with `body` to phone number `toNumber` from number `fromNumber`, authenticated via `accountSID` and `authToken`"
  //   ; fn =
  //
  //         (function
  //         | ( s
  //           , [ DStr accountSID
  //             ; DStr authToken
  //             ; DStr fromNumber
  //             ; DStr toNumber
  //             ; DStr body ] ) ->
  //             let basic_auth_string =
  //               Libhttpclient.encode_basic_auth accountSID authToken
  //             in
  //             let encoding = "application/x-www-form-urlencoded" in
  //             let headers =
  //               [ ("Authorization", DStr basic_auth_string)
  //               ; ("Content-Type", Libexecution.Dval.dstr_of_string_exn encoding)
  //               ]
  //               |> DvalMap.from_list
  //               |> DObj
  //             in
  //             let host_url = Canvas.url_for s.canvas_id in
  //             let body =
  //               [ ("From", DStr fromNumber)
  //               ; ("To", DStr toNumber)
  //               ; ("Body", DStr body)
  //               ; ("ValidityPeriod", Libexecution.Dval.dstr_of_string_exn "900")
  //               ; ( "StatusCallback"
  //                 , Libexecution.Dval.dstr_of_string_exn
  //                     (host_url ^ "/twilioCallback") ) ]
  //               |> DvalMap.fromList
  //               |> DObj
  //             in
  //             let twilio_uri =
  //               "https://api.twilio.com/2010-04-01/Accounts/"
  //               ^ Libexecution.Unicode_string.to_string accountSID
  //               ^ "/Messages.json"
  //             in
  //             Legacy.LibhttpclientV2.send_request
  //               twilio_uri
  //               Httpclient.POST
  //               Libexecution.Dval.to_pretty_machine_json_v1
  //               body
  //               (DObj DvalMap.empty)
  //               headers
  //         | _ ->
  //             incorrectArgs ())
  //   ; sqlSpec = NotYetImplementedTODO
  //   ; previewable = Impure
  //   ; deprecated = NotDeprecated } ]
  //