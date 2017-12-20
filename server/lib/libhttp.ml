open Core
open Runtime
open Lib

let fns : Lib.shortfn list = [
  { n = "Http::respond"
  ; o = []
  ; p = [par "response" TAny; par "code" TInt]
  ; r = TResp
  ; d = "Respond with HTTP status `code` and `response` body"
  ; f = InProcess
        (function
          | [dv; DInt code] -> DResp (Response (code, []), dv)
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
          | [dv] -> DResp (Response (200, []), dv)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }

  ;

  { n = "Http::respond_with_html"
  ; o = []
  ; p = [par "response" TAny; par "code" TInt]
  ; r = TResp
  ; d = "Respond with HTTP status `code` and `response` body, with `content-type` set to \"text/html\""
  ; f = InProcess
        (function
          | [dv; DInt code] -> DResp (Response (code, ["Content-Type", "text/html"]), dv)
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

  ;

  { n = "Http::fail"
  ; o = []
  ; p = [par "error" TStr]
  ; r = TResp
  ; d = "Respond with a 400 and an error message"
  ; f = InProcess
        (function
          | [DStr msg] -> DResp (Response (400, []), DStr msg)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }

  ;

  { n = "Http::not_found"
  ; o = []
  ; p = []
  ; r = TResp
  ; d = "Respond with 404 Not Found"
  ; f = InProcess
        (function
          | [] -> DResp (Response (404, []), DNull)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
]
