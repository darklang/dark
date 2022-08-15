open Tc

let is_caching_enabled = true

/* Explanatory comments:
 * cache funcs take a function that returns an option cache key. If the cache
 * key has changed, or is None, then we run the expensive function (that renders
 * some vdom); if it is unchanged from the last time (as stored by
 * Tea_html.lazy1), then use the cached value.
 *
 * Note: one use of None as a cache key is "this TL is where the cursor is,
 * don't use the cached value". (At the time this comment is being written, we
 * only use this cache functionality to wrap TL and sidebar rendering.) */

// These functions differ from each other in that `cache1m` takes one arg,
// while `cache2m` takes two.

let cache1m = (keyFn: 'a => option<'b>, expensiveFn: 'a => Vdom.t<'msg>, arg1: 'a): Vdom.t<
  'msg,
> => {
  let keyFn = k => keyFn(k) |> Option.andThen(~f=Js.Json.stringifyAny)
  let eFn = () => expensiveFn(arg1)

  switch (is_caching_enabled, keyFn(arg1)) {
  | (true, Some(k)) => Tea_html.lazy1(k, eFn)
  | _ => eFn()
  }
}

let cache2m = (
  keyFn: ('a, 'b) => option<'c>,
  expensiveFn: ('a, 'b) => Vdom.t<'msg>,
  arg1: 'a,
  arg2: 'b,
): Vdom.t<'msg> => {
  let keyFn = (k1, k2) => keyFn(k1, k2) |> Option.andThen(~f=Js.Json.stringifyAny)
  let eFn = () => expensiveFn(arg1, arg2)

  switch (is_caching_enabled, keyFn(arg1, arg2)) {
  | (true, Some(k)) => Tea_html.lazy1(k, eFn)
  | _ => eFn()
  }
}
