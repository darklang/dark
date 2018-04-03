open Core
open Runtime
open Lib

let fns : Lib.shortfn list = [
  { pns = ["Http::respond"]
  ; ins = []
  ; p = [par "response" TAny; par "code" TInt]
  ; r = TResp
  ; d = "Respond with HTTP status `code` and `response` body"
  ; f = InProcess
        (function
          | (_, [dv; DInt code]) -> DResp (Response (code, []), dv)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

  ;

  { pns = ["Http::success"]
  ; ins = []
  ; p = [par "response" TAny]
  ; r = TResp
  ; d = "Respond with HTTP status 200 and `response` body"
  ; f = InProcess
        (function
          | (_, [dv]) -> DResp (Response (200, []), dv)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

  ;

  { pns = ["Http::respond_with_html"]
  ; ins = []
  ; p = [par "response" TAny; par "code" TInt]
  ; r = TResp
  ; d = "Respond with HTTP status `code` and `response` body, with `content-type` set to \"text/html\""
  ; f = InProcess
        (function
          | (_, [dv; DInt code]) -> DResp (Response (code, ["Content-Type", "text/html"]), dv)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

  ;

  { pns = ["Http::redirect_to"]
  ; ins = []
  ; p = [par "url" TStr]
  ; r = TResp
  ; d = "Redirect to url"
  ; f = InProcess
        (function
          | (_, [DStr url]) -> DResp (Redirect url, DNull)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

  ;

  { pns = ["Http::bad_request"]
  ; ins = []
  ; p = [par "error" TStr]
  ; r = TResp
  ; d = "Respond with a 400 and an error message"
  ; f = InProcess
        (function
          | (_, [DStr msg]) -> DResp (Response (400, []), DStr msg)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

  ;

  { pns = ["Http::not_found"]
  ; ins = []
  ; p = []
  ; r = TResp
  ; d = "Respond with 404 Not Found"
  ; f = InProcess
        (function
          | (_, []) -> DResp (Response (404, []), DNull)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

  ;

  { pns = ["Http::unauthorized"]
  ; ins = []
  ; p = []
  ; r = TResp
  ; d = "Respond with 401 Unauthorized"
  ; f = InProcess
        (function
          | (_, []) -> DResp (Response (401, []), DNull)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

  ;

  { pns = ["Http::forbidden"]
  ; ins = []
  ; p = []
  ; r = TResp
  ; d = "Respond with 403 Forbidden"
  ; f = InProcess
        (function
          | (_, []) -> DResp (Response (403, []), DNull)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }

]
