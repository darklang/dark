open Tc

let is_caching_enabled = true

(* Explanatory comments:
 * cache funcs take a function that returns an option cache key. If the cache
 * key has changed, or is None, then we run the expensive function (that renders
 * some vdom); if it is unchanged from the last time (as stored by
 * Tea_html.lazy1), then use the cached value.
 *
 * Note: one use of None as a cache key is "this TL is where the cursor is,
 * don't use the cached value". (At the time this comment is being written, we
 * only use this cache functionality to wrap TL rendering.) *)

let cache1m
    (keyFn : 'a -> 'b option) (expensiveFn : 'a -> 'msg Vdom.t) (arg1 : 'a) :
    'msg Vdom.t =
  let eFn () = expensiveFn arg1 in
  let keyFn k = keyFn k |> Option.andThen ~f:Js.Json.stringifyAny in
  match (is_caching_enabled, keyFn arg1) with
  | true, Some k ->
      Tea_html.lazy1 k eFn
  | _ ->
      eFn ()


let cache1 (keyFn : 'a -> 'b) (expensiveFn : 'a -> 'msg Vdom.t) (arg1 : 'a) :
    'msg Vdom.t =
  cache1m (fun x -> Some (keyFn x)) expensiveFn arg1


let cache2m
    (keyFn : 'a -> 'b -> 'c option)
    (expensiveFn : 'a -> 'b -> 'msg Vdom.t)
    (arg1 : 'a)
    (arg2 : 'b) : 'msg Vdom.t =
  let keyFn k1 k2 = keyFn k1 k2 |> Option.andThen ~f:Js.Json.stringifyAny in
  let eFn () = expensiveFn arg1 arg2 in
  match (is_caching_enabled, keyFn arg1 arg2) with
  | true, Some k ->
      Tea_html.lazy1 k eFn
  | _ ->
      eFn ()


let cache2
    (keyFn : 'a -> 'b -> 'c)
    (expensiveFn : 'a -> 'b -> 'msg Vdom.t)
    (arg1 : 'a)
    (arg2 : 'b) : 'msg Vdom.t =
  cache2m (fun a1 a2 -> Some (keyFn a1 a2)) expensiveFn arg1 arg2
