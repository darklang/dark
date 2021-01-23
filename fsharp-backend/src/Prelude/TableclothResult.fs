module Tablecloth.Result

type t<'ok, 'error> = Result<'ok, 'error>

let ok value = Ok value

let error value = Error value

let fromOption error ma =
  match ma with
  | None -> Error error
  | Some right -> Result.Ok right


let from_option e o = fromOption e o

let isOk r =
  match r with
  | Ok _ -> true
  | Error _ -> false

let is_ok r = isOk r

let isError r =
  match r with
  | Ok _ -> false
  | Error _ -> true

let is_error r = isError r

let both a b =
  match (a, b) with
  | Ok a', Ok b' -> Ok(a', b')
  | Error a', _ -> Error a'
  | _, Error b' -> Error b'

let or_ a b =
  match a with
  | Ok _ -> a
  | _ -> b

let and_ a b =
  match a with
  | Ok _ -> b
  | _ -> a

let unwrap ``default`` r =
  match r with
  | Ok v -> v
  | Error _ -> ``default``

let unwrapUnsafe r =
  match r with
  | Ok v -> v
  | Error _ -> raise (System.ArgumentException "expected OK, got result")

let unwrap_unsafe r = unwrapUnsafe r

let unwrapError ``default`` r =
  match r with
  | Ok _ -> ``default``
  | Error error -> error


let unwrap_error d r = unwrapError d r

let map f r = Result.map f r

let map2 f a b =
  match (a, b) with
  | Ok a, Ok b -> Ok(f a b)
  | Error a, _ -> Error a
  | _, Error b -> Error b


let mapError f r =
  match r with
  | Error error -> Error(f error)
  | Ok value -> Ok value


let map_error f r = mapError f r

let values r = List.fold_right (Ok []) (map2 (fun accum v -> v :: accum)) r

let toOption r =
  match r with
  | Ok v -> Some v
  | Error _ -> None

let to_option r = toOption r

let andThen f r =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let and_then f r = andThen f r

let flatten r =
  match r with
  | Ok v -> v
  | Error e -> Error e

let attempt f =
  try
    Ok(f ())
  with e -> Error e


let tap f t =
  match t with
  | Ok a -> f a
  | _ -> ()

let equal
  (equalOk : 'ok -> 'ok -> bool)
  (equalError : 'error -> 'error -> bool)
  a
  b
  =
  match (a, b) with
  | Error a', Error b' -> equalError a' b'
  | Ok a', Ok b' -> equalOk a' b'
  | _ -> false


let compare
  (compareOk : 'ok -> 'ok -> int)
  (compareError : 'error -> 'error -> int)
  (a : t<'ok, 'error>)
  (b : t<'ok, 'error>)
  =
  (match (a, b) with
   | Error a', Error b' -> compareError a' b'
   | Ok a', Ok b' -> compareOk a' b'
   | Error _, Ok _ -> -1
   | Ok _, Error _ -> 1 : int)


let (|?) r ``default`` = unwrap ``default`` r

let (>>|) r f = map f r

let (>>=) r f = andThen f r
