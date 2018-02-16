open Core

module Clu = Cohttp_lwt_unix
module CRequest = Clu.Request

type t = string * CRequest.t [@@deriving sexp]

let store (host: string) (body: string) (h: Handler.handler) (req: CRequest.t) : unit =
  let filename =
    "requests/" ^ host ^ "_" ^ (string_of_int h.tlid) ^ ".request.sexp"
  in
  let s = sexp_of_t (body, req) in
  Sexplib.Sexp.save_hum filename s

let load (host: string) (h: Handler.handler) : t =
  let filename =
    "requests/" ^ host ^ "_" ^ (string_of_int h.tlid) ^ ".request.sexp"
  in
  let s = Sexplib.Sexp.load_sexp filename in
  t_of_sexp s
