open Core_kernel
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT

let fns : Libexecution.Lib.shortfn list =
  [ { pns = ["Twilio::sendText"]
    ; ins = []
    ; p =
        [ par "accountSID" TStr
        ; par "authToken" TStr
        ; par "fromNumber" TStr
        ; par "toNumber" TStr
        ; par "body" TStr ]
    ; r = TObj
    ; d =
        "Send text with `body` to phone number `toNumber` from number `fromNumber`, authenticated via `accountSID` and `authToken`"
    ; f =
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
                ; ( "Content-Type"
                  , Libexecution.Dval.dstr_of_string_exn encoding ) ]
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
              Libhttpclient.send_request
                twilio_uri
                Httpclient.POST
                Libexecution.Dval.to_pretty_machine_json_v1
                body
                (DObj DvalMap.empty)
                headers
          | args ->
              fail args)
    ; ps = false
    ; dep = false } ]
