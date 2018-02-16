open Core

module Clu = Cohttp_lwt_unix
module CRequest = Clu.Request

type t = string * CRequest.t [@@deriving sexp]

let store (host: string) (body: string) (id: int) (req: CRequest.t) : unit =
  let filename =
    "requests/" ^ host ^ "_" ^ (string_of_int id) ^ ".request.sexp"
  in
  let s = sexp_of_t (body, req) in
  Sexplib.Sexp.save_hum filename s

let load (host: string) (id: int) : t =
  let filename =
    "requests/" ^ host ^ "_" ^ (string_of_int id) ^ ".request.sexp"
  in
  let s = Sexplib.Sexp.load_sexp filename in
  t_of_sexp s
