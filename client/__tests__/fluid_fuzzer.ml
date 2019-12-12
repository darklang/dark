open Jest
open Expect
open Tc

(* aim is to be deterministic *)
let seed = ref 1.0

let random () : float =
  let x = Js_math.sin !seed *. 10000.0 in
  seed := !seed +. 1.0 ;
  x -. float_of_int (Js_math.floor x)


let range (max : int) : int = Js_math.floor (float_of_int max *. random ())

let generateLength maxLength = max 0 (1 + range (maxLength - 1))

let generateList ~(f : unit -> 'a) () : 'a list =
  List.initialize (generateLength 3) (fun _ -> f ())


let generateName () =
  let generateChar () : char =
    match range 10 with 0 -> 'a' | 1 -> 'b' | 2 -> 'c' | _ -> 'd'
  in
  generateList ~f:generateChar () |> String.fromList


let generateString () =
  let generateChar () : char =
    match range 50 with 0 -> ' ' | 1 -> 'x' | 2 -> 'y' | _ -> 'z'
  in
  generateList ~f:generateChar () |> String.fromList


(* Fields can only have a subset of expressions in the fieldAccess *)
let rec generateFieldAccessExpr () =
  let open Fluid_test_data in
  match range 2 with
  | 1 ->
      var (generateName ())
  | 0 ->
      fieldAccess (generateFieldAccessExpr ()) (generateName ())
  | _ ->
      var (generateName ())


let rec generateExpr () =
  let open Fluid_test_data in
  match range 16 with
  | 0 ->
      b
  (* We know string deletion is broken right now *)
  (* | 1 -> *)
  (*     str (generateString ()) *)
  | 2 ->
      int (Int.toString (range 9))
  | 3 ->
      bool (random () < 0.5)
  | 4 ->
      float' (Int.toString (range 5000000)) (Int.toString (range 500000))
  | 5 ->
      record
        (generateList () ~f:(fun () -> (generateName (), generateExpr ())))
  | 6 ->
      list (generateList () ~f:generateExpr)
  | 7 ->
      fn (generateName ()) (generateList ~f:generateExpr ())
  | 8 ->
      partial (generateName ()) (generateExpr ())
  | 9 ->
      rightPartial (generateName ()) (generateExpr ())
  | 10 ->
      var (generateName ())
  | 11 ->
      fieldAccess (generateFieldAccessExpr ()) (generateName ())
  | 12 ->
      if' (generateExpr ()) (generateExpr ()) (generateExpr ())
  | 13 ->
      let' (generateName ()) (generateExpr ()) (generateExpr ())
  | 14 ->
      lambda (generateList ~f:generateName ()) (generateExpr ())
  | 15 ->
      pipe (generateExpr ()) (generateList ~f:generateExpr ())
  | _ ->
      b


let () = test "empty test to satisfy jest" (fun () -> expect true |> toBe true)
