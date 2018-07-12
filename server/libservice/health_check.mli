open Core_kernel

(* Spin up a HTTP server which responds to a single health check route *)
val run : unit -> unit Lwt.t
