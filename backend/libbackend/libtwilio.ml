open Core_kernel
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT

let fns : Libexecution.Types.fluid_expr fn list =
  [ { prefix_names = ["Twilio::sendText"]
    ; infix_names = []
    ; parameters =
        [ par "accountSID" TStr
        ; par "authToken" TStr
        ; par "fromNumber" TStr
        ; par "toNumber" TStr
        ; par "body" TStr ]
    ; return_type = TObj
    ; description =
        "Send text with `body` to phone number `toNumber` from number `fromNumber`, authenticated via `accountSID` and `authToken`"
    ; func =
        InProcess
          (function
          | ( s
            , [ DStr accountSID
              ; DStr authToken
              ; DStr fromNumber
              ; DStr toNumber
              ; DStr body ] ) ->
              let basic_auth_string =
                Libhttpclient.encode_basic_auth_broken accountSID authToken
              in
              let encoding = "application/x-www-form-urlencoded" in
              let headers =
                [ ("Authorization", DStr basic_auth_string)
                ; ("Content-Type", Libexecution.Dval.dstr_of_string_exn encoding)
                ]
                |> DvalMap.from_list
                |> DObj
              in
              let host_url = Canvas.url_for s.canvas_id in
              let body =
                [ ("From", DStr fromNumber)
                ; ("To", DStr toNumber)
                ; ("Body", DStr body)
                ; ("ValidityPeriod", Libexecution.Dval.dstr_of_string_exn "900")
                ; ( "StatusCallback"
                  , Libexecution.Dval.dstr_of_string_exn
                      (host_url ^ "/twilioCallback") ) ]
                |> DvalMap.fromList
                |> DObj
              in
              let twilio_uri =
                "https://api.twilio.com/2010-04-01/Accounts/"
                ^ Libexecution.Unicode_string.to_string accountSID
                ^ "/Messages.json"
              in
              Legacy.LibhttpclientV2.send_request
                twilio_uri
                Httpclient.POST
                Libexecution.Dval.to_pretty_machine_json_v1
                body
                (DObj DvalMap.empty)
                headers
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated =
        true
        (* Deprecated due to using Libhttpclient.encode_basic_auth_broken *) }
  ; { prefix_names = ["Twilio::sendText_v1"]
    ; infix_names = []
    ; parameters =
        [ par "accountSID" TStr
        ; par "authToken" TStr
        ; par "fromNumber" TStr
        ; par "toNumber" TStr
        ; par "body" TStr ]
    ; return_type = TObj
    ; description =
        "Send text with `body` to phone number `toNumber` from number `fromNumber`, authenticated via `accountSID` and `authToken`"
    ; func =
        InProcess
          (function
          | ( s
            , [ DStr accountSID
              ; DStr authToken
              ; DStr fromNumber
              ; DStr toNumber
              ; DStr body ] ) ->
              let basic_auth_string =
                Libhttpclient.encode_basic_auth accountSID authToken
              in
              let encoding = "application/x-www-form-urlencoded" in
              let headers =
                [ ("Authorization", DStr basic_auth_string)
                ; ("Content-Type", Libexecution.Dval.dstr_of_string_exn encoding)
                ]
                |> DvalMap.from_list
                |> DObj
              in
              let host_url = Canvas.url_for s.canvas_id in
              let body =
                [ ("From", DStr fromNumber)
                ; ("To", DStr toNumber)
                ; ("Body", DStr body)
                ; ("ValidityPeriod", Libexecution.Dval.dstr_of_string_exn "900")
                ; ( "StatusCallback"
                  , Libexecution.Dval.dstr_of_string_exn
                      (host_url ^ "/twilioCallback") ) ]
                |> DvalMap.fromList
                |> DObj
              in
              let twilio_uri =
                "https://api.twilio.com/2010-04-01/Accounts/"
                ^ Libexecution.Unicode_string.to_string accountSID
                ^ "/Messages.json"
              in
              Legacy.LibhttpclientV2.send_request
                twilio_uri
                Httpclient.POST
                Libexecution.Dval.to_pretty_machine_json_v1
                body
                (DObj DvalMap.empty)
                headers
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
