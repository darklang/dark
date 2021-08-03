include Tea.Time

/* TODO(alice): push to fork + upstream
  Setting the key by associating it with interval,
  will lead to rewrites of setInterval functions,
  if two or more share the same interval.
*/
let every = (~key, interval, tagger) => {
  open Vdom
  let enableCall = callbacks => {
    let id = Web.Window.setInterval(() => Web.Date.now() |> tagger |> callbacks.enqueue, interval)

    () => Web.Window.clearTimeout(id)
  }

  Tea_sub.registration(key, enableCall)
}
