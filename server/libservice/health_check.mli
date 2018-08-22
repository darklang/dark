open Core_kernel

(* Spin up a HTTP server which responds to a single health check route
 *
 * Pass a boolean ref to ~shutdown and the server will write `true`
 * to it at least once after receiving a request to /pkill
 *
 * *)
val run : shutdown:(bool ref) -> execution_id:Int63.t -> unit Lwt.t
