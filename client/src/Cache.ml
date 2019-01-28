open Tc

let enabled = true

let cache1m
    (keyFn : 'a -> 'b option) (expensiveFn : 'a -> 'msg Vdom.t) (arg1 : 'a) :
    'msg Vdom.t =
  let eFn () = expensiveFn arg1 in
  let keyFn k = keyFn k |> Option.andThen ~f:Js.Json.stringifyAny in
  if not enabled
  then eFn ()
  else match keyFn arg1 with Some k -> Tea_html.lazy1 k eFn | _ -> eFn ()


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
  if not enabled
  then eFn ()
  else
    match keyFn arg1 arg2 with Some k -> Tea_html.lazy1 k eFn | _ -> eFn ()


let cache2
    (keyFn : 'a -> 'b -> 'c)
    (expensiveFn : 'a -> 'b -> 'msg Vdom.t)
    (arg1 : 'a)
    (arg2 : 'b) : 'msg Vdom.t =
  cache2m (fun a1 a2 -> Some (keyFn a1 a2)) expensiveFn arg1 arg2
