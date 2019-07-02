open Core_kernel
open Lib
open Runtime
open Types.RuntimeT

let fns : Lib.shortfn list =
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
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false } ]
