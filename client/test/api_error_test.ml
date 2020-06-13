open Prelude
open Tester
module APIError = APIError
module Http = Tea.Http
module StringMap = Map.Make (Caml.String)

let addOpsError =
  { context = "AddOps"
  ; originalError =
      Http.BadStatus
        { url = "url"
        ; headers = StringMap.empty
        ; body = StringResponse "body response"
        ; status = {code = 502; message = "Error msg"} }
  ; requestParams = None
  ; reload = false
  ; importance = IgnorableError }


let networkError =
  { context = "Network error context"
  ; originalError = Http.NetworkError
  ; requestParams = None
  ; reload = false
  ; importance = IgnorableError }


let run () =
  describe "msg" (fun () ->
      test " 502 AddOps msg" (fun () ->
          expect (APIError.msg addOpsError)
          |> toEqual
               "We're sorry, but we were unable to save your most recent edit. Please refresh and try again.") ;
      test "NetworkError msg" (fun () ->
          expect (APIError.msg networkError)
          |> toEqual
               "Network error - is the server running? (Network error context)") ;
      ()) ;
  ()
