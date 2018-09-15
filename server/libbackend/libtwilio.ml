open Core_kernel

open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT

let replacements =
  [
  ( "Twilio::sendText"
  , InProcess
    (function
    | (s, [DStr accountSID; DStr authToken; DStr fromNumber; DStr toNumber; DStr body]) ->
      let basic_auth_string =
        Libhttpclient.encode_basic_auth accountSID authToken
      in
      let encoding = "application/x-www-form-urlencoded" in
      let headers =
        [("Authorization", DStr basic_auth_string)
        ;("Content-Type", DStr encoding)
        ]
        |> DvalMap.of_alist_exn
        |> DObj
      in
      let host_url =
        Canvas.url_for s.canvas_id
      in
      let body =
        [("From", DStr fromNumber)
        ;("To", DStr toNumber)
        ;("Body", DStr body)
        ;("ValidityPeriod", DStr "900")
        ;("StatusCallback", DStr (host_url ^ "/twilioCallback"))
        ]
        |> DvalMap.of_alist_exn
        |> DObj
      in
      let twilio_uri =
        "https://api.twilio.com/2010-04-01/Accounts/"
          ^ accountSID
          ^ "/Messages.json"
      in
      Libhttpclient.send_request
        twilio_uri
        Httpclient.POST
        body
        (DObj DvalMap.empty)
        headers
    | args -> fail args)
  )
  ]

