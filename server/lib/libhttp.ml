open Core
open Runtime
open Lib

let fns : Lib.shortfn list = [
  { n = "Http::respond"
  ; o = []
  ; p = [par "code" TInt; par "response" TAny]
  ; r = TResp
  ; d = "Respond with HTTP status `code` and `response` body"
  ; f = InProcess
        (function
          | [DInt code; dv] -> DResp (Response code, dv)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }

  ;

  { n = "Http::success"
  ; o = []
  ; p = [par "response" TAny]
  ; r = TResp
  ; d = "Respond with HTTP status 200 and `response` body"
  ; f = InProcess
        (function
          | [dv] -> DResp (Response 200, dv)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }

  ;

  { n = "Http::redirect_to"
  ; o = []
  ; p = [par "url" TStr]
  ; r = TResp
  ; d = "Redirect to url"
  ; f = InProcess
        (function
          | [DStr url] -> DResp (Redirect url, DNull)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
]
