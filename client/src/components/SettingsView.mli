type t

type msg = Foo | Bar

(* `msg` is just for browser events (timers, callbacks etc.) and user interaction *)

(* The whole point of redefining effects, and not using say, `Toast.msg` is to
 * prevent this components coupling/depending on each other *)
type effect1 = ToastShow of string
             | APIError of err

type effect2 = ToastEffect of (Toast.t -> Toast.t)
             | APIError of err

val update2 : t -> msg -> t * effect list



(* toast.mli)
 *)


module Toast : sig
  type t
  val show : string -> t -> t

end
