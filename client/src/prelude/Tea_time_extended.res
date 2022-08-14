include Tea.Time

// hmmm: this file seems duplicate of Tea_tea_time?

/* TODO: push to fork + upstream
  hmmm: we can probably remove the above comment?

  hmmm: is this?

  Note: Setting the key by associating it with interval, will lead to rewrites\
  of setInterval functions, if two or more share the same interval.
*/
let every = (~key, interval, tagger) => {
  open Vdom
  let enableCall = callbacks => {
    let id = Web.Window.setInterval(() => Web.Date.now() |> tagger |> callbacks.enqueue, interval)

    () => Web.Window.clearTimeout(id)
  }

  Tea_sub.registration(key, enableCall)
}
