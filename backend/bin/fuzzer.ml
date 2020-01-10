open Core_kernel
module Util = Libexecution.Util
open Libbackend
open Libexecution
module RTT = Types.RuntimeT
open Libcommon
open Types
open RTT.HandlerT
open Prelude
open Fluid

module Private = struct
  type success =
    | Passed
    | Failed
    | Skipped

  type t =
    { categories : string list
    ; name : string
    ; success : success
    ; actual : string option
    ; expected : string option }

  let results : t list ref = ref []

  let categories : string list ref = ref []

  let runningTest : string ref = ref ""

  type 'a expectation = Expect of 'a
end

(* ------------------ *)
(* Command line options *)
(* ------------------ *)

(* let pattern : Js.Re.t option ref = ref None *)

let verbose : bool ref = ref false

(* ------------------ *)
(* Output / results *)
(* ------------------ *)

let categoryIndent () =
  String.repeat ~count:(List.length !Private.categories) " "


let reset = {j|\x1b[0m|j}

let grey = {j|\x1b[37m|j}

let cyan = {j|\x1b[36m|j}

let green = {j|\x1b[32m|j}

let yellow = {j|\x1b[33m|j}

let brightWhite = {j|\x1b[1m\x1b[37m|j}

let categoryColor () : string =
  match List.length !Private.categories with
  | 1 ->
      brightWhite
  | 2 ->
      cyan
  | 3 ->
      yellow
  | 4 ->
      green
  | _ ->
      grey


let testIndent () = categoryIndent () ^ "  "

let print_category_start name : unit =
  print_endline
    (categoryIndent () ^ {j|↣|j} ^ categoryColor () ^ " " ^ name ^ reset) ;
  ()


let print_test_skip name : unit =
  print_endline (testIndent () ^ {j|🙅|j} ^ " " ^ name)


let print_test_end name (t : Private.t) : unit =
  let open Private in
  let shortName = String.slice ~from:0 ~to_:68 name in
  if t.success = Passed
  then print_endline @@ testIndent () ^ {j|✅|j} ^ " " ^ shortName
  else if t.success = Failed
  then (
    print_endline @@ testIndent () ^ {j|❌|j} ^ " " ^ name ;
    print_endline
    @@ testIndent ()
    ^ "Expected: "
    ^ Option.withDefault ~default:"None" t.expected ;
    print_endline
    @@ testIndent ()
    ^ "  Actual: "
    ^ Option.withDefault ~default:"None" t.actual )
  else print_test_skip name


(* ------------------ *)
(* Framework - test creation functions *)
(* ------------------ *)

let describe (name : string) (testFn : unit -> unit) : unit =
  let open Private in
  categories := name :: !categories ;
  if List.length !categories <= 1 || !verbose then print_category_start name ;
  testFn () ;
  match !categories with [] -> () | _ :: rest -> categories := rest


let runTest (name : string) (testFn : unit -> Private.t) : unit =
  let open Private in
  let shouldRun =
    true
    (* match !pattern with *)
    (* | None -> *)
    (*     true *)
    (* | Some pattern -> *)
    (*     let fullname = String.join ~sep:" " (name :: !categories) in *)
    (*     Util.Regex.contains ~re:pattern fullname *)
  in
  if shouldRun
  then runningTest := name
  else if !verbose
  then print_test_skip name ;
  let result =
    if shouldRun
    then
      try testFn ()
      with e ->
        { categories = !categories
        ; name = !runningTest
        ; success = Failed
        ; actual = Some (Exn.to_string e)
        ; expected = None }
    else
      { categories = !categories
      ; name = !runningTest
      ; success = Skipped
      ; actual = None
      ; expected = None }
  in
  if shouldRun && (result.success = Failed || !verbose)
  then print_test_end name result ;
  results := result :: !results


let testAll (name : string) (items : 'a list) (testFn : 'a -> Private.t) : unit
    =
  items
  |> List.iter ~f:(fun item ->
         let name' = {j|$name  - $item|j} in
         runTest name' (fun () -> testFn item))


(* ------------------ *)
(* Framework - test evaluation functions *)
(* ------------------ *)
let expect (value : 'a) = Private.Expect value

let toEqual (expected : 'a) (Expect actual : 'a Private.expectation) =
  let open Private in
  if actual = expected
  then
    { categories = !categories
    ; name = !runningTest
    ; success = Passed
    ; actual = None
    ; expected = None }
  else
    { categories = !categories
    ; name = !runningTest
    ; success = Failed
    ; actual = None (* Js.Json.stringifyAny actual *)
    ; expected = None (* Js.Json.stringifyAny expected *) }


let toBe = toEqual

let pass () : Private.t =
  let open Private in
  { categories = !categories
  ; name = !runningTest
  ; success = Passed
  ; actual = None
  ; expected = None }


let fail () : Private.t =
  let open Private in
  { categories = !categories
  ; name = !runningTest
  ; success = Failed
  ; actual = Some "fail was called"
  ; expected = None }


(* ------------------ *)
(* Announce completion *)
(* ------------------ *)
let finish () =
  let open Private in
  let successes = List.filter !results ~f:(fun r -> r.success = Passed) in
  let fails = List.filter !results ~f:(fun r -> r.success = Failed) in
  let skips = List.filter !results ~f:(fun r -> r.success = Skipped) in
  let successCount = List.length successes |> string_of_int in
  let failCount = List.length fails |> string_of_int in
  let skipCount = List.length skips |> string_of_int in
  print_endline "\n\n" ;
  if fails = []
  then (
    print_endline
      ("Passed " ^ successCount ^ " tests (" ^ skipCount ^ " skipped)") ;
    exit 0 )
  else (
    print_endline "Failures:" ;
    fails
    |> List.iter ~f:(fun {name; _} ->
           print_endline @@ testIndent () ^ {j|❌|j} ^ " " ^ name) ;
    print_endline "" ;
    print_endline
      ( "Failed "
      ^ failCount
      ^ " tests ("
      ^ successCount
      ^ " passed, "
      ^ skipCount
      ^ " skipped)" ) ;
    exit 1 )


let usage () =
  Format.printf
    "Usage:  <fnNames...>\n  Where <fnNames> is a space-separated list of functions to look for"
  (* Sys.argv.(0) ; *) ;
  exit 1


(* See docs/fuzzer.md for documentation on how to use this. *)

(* ------------------ *)
(* Settings *)
(* ------------------ *)

let defaultVerbosityThreshold = 20

let verbosityThreshold = ref defaultVerbosityThreshold

let defaultInitialSeed = 0

let initialSeed = ref defaultInitialSeed

let defaultCount = 3

let count = ref defaultCount

(* ------------------ *)
(* Debugging *)
(* ------------------ *)

let toText _ast = "todo"

let debugAST (length : int) (msg : string) (e : fluidExpr) : unit =
  if length < !verbosityThreshold then print_endline (msg ^ ":\n" ^ toText e)


(* ------------------ *)
(* Deterministic random number generator *)
(* ------------------ *)

(* aim is to be deterministic *)
let defaultSeed = 1.0

let state = ref defaultSeed

let setSeed (seed : int) : unit = state := float_of_int seed

let random () : float =
  state := Float.sin !state *. 10000.0 ;
  !state -. Float.round_down !state


let range (max : int) : int =
  Float.round_down (float_of_int max *. random ()) |> int_of_float


(* ------------------ *)
(* AST generators *)
(* ------------------ *)

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
  match range 2 with
  | 1 ->
      var (generateName ())
  | 0 ->
      fieldAccess (generateFieldAccessExpr ()) (generateName ())
  | _ ->
      var (generateName ())


let rec generatePattern () =
  match range 7 with
  | 0 ->
      pInt (Base.Int.to_string (range 500))
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
      pFloat
        (Base.Int.to_string (range 5000000))
        (Base.Int.to_string (range 500000))
  | _ ->
      pBlank


let rec generatePipeArgumentExpr () =
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
  match range 17 with
  | 0 ->
      b
  | 1 ->
      str (generateString ())
  | 2 ->
      int (Base.Int.to_string (range 500))
  | 3 ->
      bool (random () < 0.5)
  | 4 ->
      float'
        (Base.Int.to_string (range 5000000))
        (Base.Int.to_string (range 500000))
  | 5 ->
      record (generateList () ~f:(fun () -> (generateName (), generateExpr ())))
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
      pipe (generateExpr ()) (generateList ~f:generatePipeArgumentExpr ())
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


(* ------------------ *)
(* Fuzz Test definition *)
(* ------------------ *)
module FuzzTest = struct
  type t =
    { name : string
    ; fn : fluidExpr -> fluidExpr
    ; check : testcase:fluidExpr -> newAST:fluidExpr -> bool }
end

(* ------------------ *)
(* Test case reduction *)
(* ------------------ *)

let rec unwrap (id : id) (expr : fluidExpr) : fluidExpr =
  let f = unwrap id in
  let childOr (exprs : fluidExpr list) =
    List.find exprs ~f:(fun e -> Fluid.id e = id)
  in
  let newExpr =
    match expr with
    | ELet (_, _, rhs, next) ->
        childOr [rhs; next]
    | EIf (_, cond, ifexpr, elseexpr) ->
        childOr [cond; ifexpr; elseexpr]
    | EBinOp (_, _, lexpr, rexpr, _) ->
        childOr [lexpr; rexpr]
    | EFieldAccess (_, expr, _) ->
        childOr [expr]
    | EFnCall (_, _, exprs, _) ->
        childOr exprs
    | ELambda (_, _, body) ->
        childOr [body]
    | EList (_, exprs) ->
        childOr exprs
    | EMatch (_, mexpr, pairs) ->
        childOr (mexpr :: List.map ~f:Tuple2.second pairs)
    | ERecord (_, fields) ->
        childOr (List.map ~f:Tuple2.second fields)
    | EPipe (_, exprs) ->
        childOr exprs
    | EConstructor (_, _, exprs) ->
        childOr exprs
    | EPartial (_, _, oldExpr) ->
        childOr [oldExpr]
    | ERightPartial (_, _, oldExpr) ->
        childOr [oldExpr]
    | EFeatureFlag (_, _, cond, casea, caseb) ->
        childOr [cond; casea; caseb]
    | _ ->
        None
  in
  let newExpr = Option.withDefault ~default:expr newExpr in
  Fluid.walk ~f newExpr


let rec blankVarNames (id : id) (expr : fluidExpr) : fluidExpr =
  let f = blankVarNames id in
  let fStr strid str = if strid = id then "" else str in
  let newExpr =
    match expr with
    | ELet (id, name, rhs, next) ->
        ELet (id, fStr id name, rhs, next)
    | EFieldAccess (id, expr, fieldname) ->
        EFieldAccess (id, expr, fStr id fieldname)
    | ELambda (id, names, expr) ->
        let names =
          List.map names ~f:(fun (nid, name) ->
              if nid = id then (nid, "") else (nid, name))
        in
        ELambda (id, names, expr)
    | ERecord (rid, fields) ->
        ERecord
          ( rid
          , List.map
              ~f:(fun (name, expr) ->
                if id = Fluid.id expr then ("", expr) else (name, expr))
              fields )
    | _ ->
        expr
  in
  Fluid.walk ~f newExpr


let rec remove (id : id) (expr : fluidExpr) : fluidExpr =
  let r e = remove id e in
  let f e = if Fluid.id e = id then EBlank id else remove id e in
  let removeFromList exprs = List.filter exprs ~f:(fun e -> Fluid.id e <> id) in
  if Fluid.id expr = id
  then EBlank id
  else
    let newExpr =
      match expr with
      | EFieldAccess (faID, expr, fieldname) ->
          if id = faID then expr else EFieldAccess (faID, r expr, fieldname)
      | EFnCall (id, name, exprs, ster) ->
          EFnCall (id, name, removeFromList exprs, ster)
      | ELambda (id, names, expr) ->
          let names =
            names
            |> List.filterMap ~f:(fun (nid, name) ->
                   if nid = id then None else Some (nid, name))
            |> fun x -> if x = [] then List.take ~count:1 names else x
          in
          ELambda (id, names, f expr)
      | EList (id, exprs) ->
          EList (id, removeFromList exprs)
      | EMatch (mid, mexpr, pairs) ->
          EMatch
            ( mid
            , f mexpr
            , List.filterMap
                ~f:(fun (pattern, expr) ->
                  if Fluid.id expr = id || Fluid.patternID pattern = id
                  then None
                  else Some (pattern, expr))
                pairs )
      | ERecord (rid, fields) ->
          ERecord
            ( rid
            , List.filterMap
                ~f:(fun (name, expr) ->
                  if Fluid.id expr = id then None else Some (name, expr))
                fields )
      | EPipe (id, exprs) ->
        ( match removeFromList exprs with
        | [EBlank _; EBinOp (id, op, EPipeTarget ptid, rexpr, ster)]
        | [EBinOp (id, op, EPipeTarget ptid, rexpr, ster); EBlank _] ->
            EBinOp (id, op, EBlank ptid, rexpr, ster)
        | [EBlank _; EFnCall (id, name, EPipeTarget ptid :: tail, ster)]
        | [EFnCall (id, name, EPipeTarget ptid :: tail, ster); EBlank _] ->
            EFnCall (id, name, EBlank ptid :: tail, ster)
        | [justOne] ->
            justOne
        | [] ->
            EBlank id
        | newExprs ->
            EPipe (id, newExprs) )
      | EConstructor (id, name, exprs) ->
          EConstructor (id, name, removeFromList exprs)
      | _ ->
          expr
    in
    Fluid.walk ~f newExpr


let reduce (test : FuzzTest.t) (ast : fluidExpr) =
  let runThrough msg reducer ast =
    let ids = Fluid.allIDs ast |> List.indexedMap ~f:(fun i v -> (i, v)) in
    let length = List.length ids in
    let latestAST = ref ast in
    List.iter ids ~f:(fun (idx, id) ->
        ( try
            Log.inspecT msg (idx, length, id) ;
            let reducedAST = reducer id !latestAST in
            if !latestAST = reducedAST
            then print_endline "no change, trying next id"
            else
              let newAST = test.fn reducedAST in
              let passed = test.check ~testcase:reducedAST ~newAST in
              if passed
              then (
                print_endline "removed the good bit, trying next id" ;
                debugAST length "started with" !latestAST ;
                debugAST length "result was" reducedAST ;
                debugAST length "after testing" newAST )
              else (
                print_endline
                  "Success! We've reduced and it still fails. Let's keep going!" ;
                debugAST length "started with" !latestAST ;
                debugAST length "result was" reducedAST ;
                debugAST length "after testing" newAST ;
                latestAST := reducedAST )
          with e -> Log.inspecT "Exception, let's skip this one" e ) ;
        print_endline "\n") ;
    !latestAST
  in
  let sentinel = Fluid.int "56756756" in
  let oldAST = ref sentinel in
  let newAST = ref ast in
  while oldAST <> newAST do
    Log.inspecT "starting to reduce\n" (toText !newAST) ;
    oldAST := !newAST ;
    let latestAST =
      !newAST
      |> runThrough "unwrapping" unwrap
      |> runThrough "removing" remove
      |> runThrough "blankVarNames" blankVarNames
    in
    newAST := latestAST
  done ;
  !newAST


(* ------------------ *)
(* Driver *)
(* ------------------ *)
let runTest (test : FuzzTest.t) : unit =
  try
    for i = !initialSeed to !initialSeed + !count - 1 do
      let name = test.name ^ " #" ^ string_of_int i in
      runTest name (fun () ->
          setSeed i ;
          let testcase = generateExpr () |> Fluid.clone in
          Log.inspecT "testing: " name ;
          let newAST = test.fn testcase in
          Log.inspecT "checking: " name ;
          let passed = test.check ~testcase ~newAST in
          if passed = false
          then (
            Log.inspecT "failed: " name ;
            let reduced = reduce test testcase in
            let text = toText reduced in
            Log.inspecT "finished program:\n" text ;
            Log.inspecT "structure" (Fluid.show_fluidExpr reduced) ;
            fail () )
          else pass ())
    done
  with _ -> ()


let roundTripTest : FuzzTest.t =
  { name = "expression roundtrips to Expr correctly"
  ; check = (fun ~testcase ~newAST -> testcase = newAST)
  ; fn =
      (fun testcase ->
        testcase |> Fluid.fromFluidExpr |> Fluid.toFluidExpr ~inPipe:false) }


let () =
  verbose := true ;
  count := 30 ;
  initialSeed := 2 ;
  runTest roundTripTest ;
  ()
