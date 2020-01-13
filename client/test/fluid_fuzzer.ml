open Prelude
open Fluid
open Fluid_test_data
open Tester

(* See docs/fuzzer.md for documentation on how to use this. *)

(* ------------------ *)
(* Settings *)
(* ------------------ *)

(* At what size do we start to print more data? *)
let defaultVerbosityThreshold = 20

let verbosityThreshold = ref defaultVerbosityThreshold

(* The seed can be changed to get new test data *)
let defaultInitialSeed = 0

let initialSeed = ref defaultInitialSeed

(* How many tests do we create/run *)
let defaultCount = 3

let count = ref defaultCount

(* How big should the generated lists be *)
let defaultSize = 4

let size = ref defaultSize

(* Only run this one test *)
let only : int option ref = ref None

(* Stop after getting our first failure  *)
let stop_on_fail : bool ref = ref false

(* ------------------ *)
(* Debugging *)
(* ------------------ *)

let toText ast = FluidPrinter.eToString ast

let pointerToText (p : blankOrData) : string = Pointer.toContent p

let debugAST (length : int) (msg : string) (e : E.t) : unit =
  if length < !verbosityThreshold then Js.log (msg ^ ":\n" ^ E.show e)


(* ------------------ *)
(* Deterministic random number generator *)
(* ------------------ *)

(* aim is to be deterministic *)
let defaultSeed = 1.0

let state = ref defaultSeed

let setSeed (seed : int) : unit = state := float_of_int seed

let random () : float =
  state := Js_math.sin !state *. 10000.0 ;
  !state -. float_of_int (Js_math.floor !state)


let range (max : int) : int = Js_math.floor (float_of_int max *. random ())

(* ------------------ *)
(* AST generators *)
(* ------------------ *)

let generateLength maxLength = max 0 (1 + range (maxLength - 1))

let generateList ~(minSize : int) ~(f : unit -> 'a) () : 'a list =
  (* Lower the list lengths as we go so eventually the program converges *)
  size := !size - 1 ;
  let s = max minSize !size in
  let result = List.initialize (generateLength s) (fun _ -> f ()) in
  size := !size + 1 ;
  result


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
  generateList ~minSize:1 ~f:generateChar () |> String.fromList


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
  generateList ~minSize:0 ~f:generateChar () |> String.fromList


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
      pInt (Int.toString (range 500))
  | 1 ->
      pBool (random () < 0.5)
  | 2 ->
      pNull
  | 3 ->
      pConstructor
        (generateName ())
        (generateList ~minSize:0 ~f:generatePattern ())
  | 4 ->
      pVar (generateName ())
  | 5 ->
      pString (generateString ())
  | 6 ->
      pFloat (Int.toString (range 5000000)) (Int.toString (range 500000))
  | _ ->
      pBlank


let rec generatePipeArgumentExpr () =
  match range 4 with
  | 0 ->
      lambda (generateList ~minSize:1 ~f:generateName ()) (generateExpr ())
  | 1 ->
      b
  | 2 ->
      fn
        (generateName ())
        (pipeTarget :: generateList ~minSize:0 ~f:generateExpr ())
  | 3 ->
      binop (generateName ()) pipeTarget (generateExpr ())
  | _ ->
      b


and generateExpr () =
  match range 19 with
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
      null
  | 6 ->
      var (generateName ())
  | 7 ->
      partial (generateFnName ()) (generateExpr ())
  | 8 ->
      list (generateList () ~minSize:0 ~f:generateExpr)
  | 9 ->
      fn (generateFnName ()) (generateList ~minSize:0 ~f:generateExpr ())
  | 10 ->
      rightPartial (generateInfixName ()) (generateExpr ())
  | 11 ->
      fieldAccess (generateFieldAccessExpr ()) (generateName ())
  | 12 ->
      lambda (generateList ~minSize:1 ~f:generateName ()) (generateExpr ())
  | 13 ->
      let' (generateName ()) (generateExpr ()) (generateExpr ())
  | 14 ->
      binop (generateInfixName ()) (generateExpr ()) (generateExpr ())
  | 15 ->
      if' (generateExpr ()) (generateExpr ()) (generateExpr ())
  | 16 ->
      constructor (generateName ()) (generateList ~minSize:0 ~f:generateExpr ())
  | 17 ->
      pipe
        (generateExpr ())
        (generateList ~minSize:2 ~f:generatePipeArgumentExpr ())
  | 18 ->
      record
        (generateList ~minSize:1 () ~f:(fun () ->
             (generateName (), generateExpr ())))
  | 19 ->
      match'
        (generateExpr ())
        (generateList ~minSize:1 () ~f:(fun () ->
             (generatePattern (), generateExpr ())))
  | _ ->
      b


(* ------------------ *)
(* Fuzz Test definition *)
(* ------------------ *)
module FuzzTest = struct
  type t =
    { name : string
    ; fn : E.t -> E.t * fluidState
    ; check : testcase:E.t -> newAST:E.t -> fluidState -> bool
    ; ignore : (* Sometimes you know some things are broken *)
               E.t -> bool }
end

(* ------------------ *)
(* Test case reduction *)
(* ------------------ *)

let rec unwrap (id : id) (expr : E.t) : E.t =
  let f = unwrap id in
  let childOr (exprs : E.t list) = List.find exprs ~f:(fun e -> E.id e = id) in
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
  E.walk ~f newExpr


let rec changeStrings (id : id) ~(f : string -> string) (expr : E.t) : E.t =
  let fStr strid str = if strid = id then f str else str in
  let newExpr =
    match expr with
    | ELet (id, name, rhs, next) ->
        ELet (id, fStr id name, rhs, next)
    | EFieldAccess (id, expr, fieldname) ->
        EFieldAccess (id, expr, fStr id fieldname)
    | EPartial (id, name, expr) ->
        let newName = f name in
        if newName = "" then expr else EPartial (id, newName, expr)
    | ERightPartial (id, name, expr) ->
        let newName = f name in
        if newName = "" then expr else ERightPartial (id, newName, expr)
    | EFnCall (id, name, exprs, ster) ->
        let newName = f name in
        if newName = "" then expr else EFnCall (id, newName, exprs, ster)
    | EBinOp (id, name, lhs, rhs, ster) ->
        let newName = f name in
        if newName = "" then expr else EBinOp (id, newName, lhs, rhs, ster)
    | ELambda (id, names, expr) ->
        let names =
          List.map names ~f:(fun (nid, name) ->
              if nid = id then (nid, f name) else (nid, name))
        in
        ELambda (id, names, expr)
    | ERecord (rid, fields) ->
        ERecord
          ( rid
          , List.map
              ~f:(fun (name, expr) ->
                if id = E.id expr then (f name, expr) else (name, expr))
              fields )
    | EString (id, str) ->
        EString (id, f str)
    | _ ->
        expr
  in
  E.walk ~f:(changeStrings id ~f) newExpr


let blankVarNames (id : id) (expr : E.t) : E.t =
  changeStrings ~f:(fun _ -> "") id expr


let shortenNames (id : id) (expr : E.t) : E.t =
  changeStrings ~f:(String.dropRight ~count:1) id expr


let rec remove (id : id) (expr : E.t) : E.t =
  let r e = remove id e in
  let removeFromList exprs =
    List.filterMap exprs ~f:(fun e -> if E.id e = id then None else Some (r e))
  in
  if E.id expr = id
  then EBlank id
  else
    let f expr =
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
          ELambda (id, names, expr)
      | EList (id, exprs) ->
          EList (id, removeFromList exprs)
      | EMatch (mid, mexpr, pairs) ->
          EMatch
            ( mid
            , mexpr
            , List.filterMap
                ~f:(fun (pattern, expr) ->
                  if E.id expr = id || FluidPattern.id pattern = id
                  then None
                  else Some (pattern, expr))
                pairs )
      | ERecord (rid, fields) ->
          ERecord
            ( rid
            , List.filterMap
                ~f:(fun (name, expr) ->
                  if E.id expr = id then None else Some (name, expr))
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
    E.walk ~f expr


let reduce (test : FuzzTest.t) (ast : E.t) =
  let runThrough msg reducer ast =
    let tokenIDs =
      ast |> FluidPrinter.toTokens |> List.map ~f:(fun ti -> T.tid ti.token)
    in
    let eIDs = ast |> E.filterMap ~f:(fun e -> Some (E.id e)) in
    let ids =
      tokenIDs @ eIDs
      |> List.uniqueBy ~f:Prelude.deID
      |> List.indexedMap ~f:(fun i v -> (i, v))
    in
    let length = List.length ids in
    let latestAST = ref ast in
    List.iter ids ~f:(fun (idx, id) ->
        ( try
            Js.log2 msg (idx, length, id) ;
            let reducedAST = reducer id !latestAST in
            if !latestAST = reducedAST || test.ignore reducedAST
            then Js.log "no change, trying next id"
            else
              let newAST, newState = test.fn reducedAST in
              let passed = test.check ~testcase:reducedAST ~newAST newState in
              if passed
              then (
                Js.log "removed the good bit, trying next id" ;
                debugAST length "started with" !latestAST ;
                debugAST length "reduced" reducedAST ;
                debugAST length "after testing" newAST ;
                Js.log2 "pos is" newState.newPos )
              else (
                Js.log
                  "Success! We've reduced and it still fails. Let's keep going!" ;
                debugAST length "started with" !latestAST ;
                debugAST length "reduced" reducedAST ;
                debugAST length "after testing" newAST ;
                if length < !verbosityThreshold
                then Js.log2 "pos is" newState.newPos ;
                latestAST := reducedAST )
          with e -> Js.log2 "Exception, let's skip this one" e ) ;
        Js.log "\n") ;
    !latestAST
  in
  let sentinel = Fluid_test_data.int "56756756" in
  let oldAST = ref sentinel in
  let newAST = ref ast in
  while oldAST <> newAST do
    Js.log2 "starting to reduce\n" (toText !newAST) ;
    oldAST := !newAST ;
    let latestAST =
      !newAST
      |> runThrough "unwrapping" unwrap
      |> runThrough "removing" remove
      |> runThrough "blankVarNames" blankVarNames
      |> runThrough "shortenNames" shortenNames
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
      if !stop_on_fail && Tester.fails () <> [] then exit (-1) ;
      let name = test.name ^ " #" ^ string_of_int i in
      Tester.test name (fun () ->
          setSeed i ;
          let testcase = generateExpr () |> FluidExpression.clone in
          if !only = None || !only = Some i
          then (
            Js.log2 "testing: " name ;
            (* Js.log2 "testcase: " (E.show testcase) ; *)
            let newAST, newState = test.fn testcase in
            Js.log2 "checking: " name ;
            let passed = test.check ~testcase ~newAST newState in
            if passed = false
            then (
              Js.log2 "failed: " name ;
              let reduced = reduce test testcase in
              Js.log2 "finished program:\n  " (toText reduced) ;
              Js.log2 "as expr:\n  " (E.show reduced) ;
              Js.log2 "as testcase:\n  " (FluidPrinter.eToTestcase reduced) ;
              fail () )
            else pass () )
          else (
            Js.log2 "skipping: " name ;
            skip () ))
    done
  with _ -> ()
