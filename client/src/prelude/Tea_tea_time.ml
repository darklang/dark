include Tea.Time

(* TODO(alice): push to fork + upstream
  Setting the key by associating it with interval,
  will lead to rewrites of setInterval functions,
  if two or more share the same interval.
*)
let every ~key interval tagger =
  let open Vdom in
  let enableCall callbacks =
    let id =
      Web.Window.setInterval
        (fun () -> Web.Date.now () |> tagger |> callbacks.enqueue)
        interval
    in
    fun () -> Web.Window.clearTimeout id
  in
  Tea_sub.registration key enableCall
