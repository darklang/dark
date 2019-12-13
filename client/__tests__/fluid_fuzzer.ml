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
    match range 11 with
    | 0 ->
        'a'
    | 1 ->
        'b'
    | 2 ->
        'c'
    | 3 ->
        'd'
    | 4 ->
        'e'
    | 5 ->
        'f'
    | 6 ->
        'g'
    | 7 ->
        'h'
    | 8 ->
        'i'
    | 9 ->
        'j'
    | 10 ->
        'k'
    | _ ->
        'z'
  in
  generateList ~f:generateChar () |> String.fromList


let generateString () =
  let generateChar () : char =
    match range 11 with
    | 0 ->
        'A'
    | 1 ->
        'B'
    | 2 ->
        'C'
    | 3 ->
        'D'
    | 4 ->
        'E'
    | 5 ->
        'F'
    | 6 ->
        'G'
    | 7 ->
        'H'
    | 8 ->
        'I'
    | 9 ->
        'J'
    | 10 ->
        'K'
    | _ ->
        ' '
  in
  generateList ~f:generateChar () |> String.fromList


let generateInfixName () =
  match range 6 with
  | 0 ->
      "+"
  | 1 ->
      "++"
  | 2 ->
      "-"
  | 3 ->
      "*"
  | 4 ->
      "/"
  | 5 ->
      "||"
  | _ ->
      "&&"


let generateFnName () =
  match range 7 with
  | 0 ->
      "Int::add"
  | 1 ->
      "DB::set_v2"
  | 2 ->
      "HttpClient::post_v4"
  | 3 ->
      generateName ()
  | 4 ->
      "DB::getAll_v2"
  | 5 ->
      "DB::generateKey_v1"
  | 6 ->
      "Date::now_v0"
  | _ ->
      "Date::now"


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


let rec generatePattern () =
  let open Fluid_test_data in
  match range 7 with
  | 0 ->
      pInt (Int.toString (range 500))
  | 1 ->
      pBool (random () < 0.5)
  | 2 ->
      pNull
  | 3 ->
      pConstructor (generateName ()) (generateList ~f:generatePattern ())
  | 4 ->
      pVar (generateName ())
  | 5 ->
      pString (generateString ())
  | 6 ->
      pFloat (Int.toString (range 5000000)) (Int.toString (range 500000))
  | _ ->
      pBlank


let rec generatePipeArgumentExpr () =
  let open Fluid_test_data in
  match range 4 with
  | 0 ->
      lambda (generateList ~f:generateName ()) (generateExpr ())
  | 1 ->
      b
  | 2 ->
      fn (generateName ()) (pipeTarget :: generateList ~f:generateExpr ())
  | 3 ->
      binop (generateName ()) pipeTarget (generateExpr ())
  | _ ->
      b


and generateExpr () =
  let open Fluid_test_data in
  match range 17 with
  | 0 ->
      b
  | 1 ->
      str (generateString ())
  | 2 ->
      int (Int.toString (range 500))
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
      fn (generateFnName ()) (generateList ~f:generateExpr ())
  | 8 ->
      partial (generateFnName ()) (generateExpr ())
  | 9 ->
      rightPartial (generateInfixName ()) (generateExpr ())
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
  | 16 ->
      binop (generateInfixName ()) (generateExpr ()) (generateExpr ())
  | 17 ->
      null
  | 18 ->
      constructor (generateName ()) (generateList ~f:generateExpr ())
  | 19 ->
      match'
        (generateExpr ())
        (generateList () ~f:(fun () -> (generatePattern (), generateExpr ())))
  | _ ->
      b


let () = test "empty test to satisfy jest" (fun () -> expect true |> toBe true)
