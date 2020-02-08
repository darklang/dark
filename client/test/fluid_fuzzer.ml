open Prelude
open Fluid
open Fluid_test_data
open Tester
open FluidExpression
open FluidShortcuts

(* See docs/fuzzer.md for documentation on how to use this. *)

(* ------------------ *)
(* Settings *)
(* ------------------ *)

(* At what size do we start to print more data? *)
let defaultVerbosityThreshold = 20

let verbosityThreshold = ref defaultVerbosityThreshold

(* The seed can be changed to get new test data *)
let initialSeed : int ref =
  Js.Date.now ()
  |> int_of_float
  |> (fun n -> if n < 0 then n * -1 else n)
  |> ( mod ) 1000000000
  |> ref


(* How big should the generated lists be *)
let defaultSize = 4

let size = ref defaultSize

(* Continue after the first seed *)
let continue : bool ref = ref false

(* Stop after getting our first failure  *)
let stopOnFail : bool ref = ref true

(* ------------------ *)
(* Debugging *)
(* ------------------ *)

let toText ast = FluidPrinter.eToHumanString ast

let pointerToText (p : blankOrData) : string = Pointer.toContent p

let debugAST (length : int) (msg : string) (e : E.t) : unit =
  if length < !verbosityThreshold then Js.log (msg ^ ":\n" ^ E.show e)


(* ------------------ *)
(* Deterministic random number generator *)
(* ------------------ *)

(* aim is to be deterministic *)
let state = ref 1.0

let setSeed (seed : int) : unit = state := float_of_int seed

let random () : float =
  state := Js_math.sin !state *. 10000.0 ;
  !state -. float_of_int (Js_math.floor !state)


let range (max : int) : int = Js_math.floor (float_of_int max *. random ())

let oneOf (l : 'a list) : 'a =
  List.getAt ~index:(range (List.length l)) l |> Option.valueExn


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
    oneOf ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k']
  in
  generateList ~minSize:1 ~f:generateChar () |> String.fromList


let generateString () =
  let generateChar () : char =
    oneOf ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K']
  in
  generateList ~minSize:0 ~f:generateChar () |> String.fromList


let generateInfixName () = oneOf ["+"; "++"; "-"; "*"; "/"; "||"; "&&"]

let generateFnName () =
  oneOf
    [ "Int::add"
    ; "DB::set_v2"
    ; "HttpClient::post_v4"
    ; generateName ()
    ; "DB::getAll_v2"
    ; "DB::generateKey_v1"
    ; "Date::now_v0"
    ; "Date::now" ]


(* Fields can only have a subset of expressions in the fieldAccess *)
let rec generateFieldAccessExpr () =
  oneOf
    [ var (generateName ())
    ; fieldAccess (generateFieldAccessExpr ()) (generateName ()) ]


let rec generatePattern () =
  oneOf
    [ pInt (range 500)
    ; pBool (random () < 0.5)
    ; pNull
    ; pConstructor
        (generateName ())
        (generateList ~minSize:0 ~f:generatePattern ())
    ; pVar (generateName ())
    ; pString (generateString ())
    ; pFloat (Int.toString (range 5000000)) (Int.toString (range 500000))
    ; pBlank ]


let rec generatePipeArgumentExpr () : FluidExpression.t =
  oneOf
    [ lambda (generateList ~minSize:1 ~f:generateName ()) (generateExpr ())
    ; b
    ; fn
        (generateName ())
        (pipeTarget :: generateList ~minSize:0 ~f:generateExpr ())
    ; binop (generateName ()) pipeTarget (generateExpr ()) ]


and generateExpr () =
  oneOf
    [ b
    ; str (generateString ())
    ; int (range 500)
    ; bool (random () < 0.5)
    ; float' (Int.toString (range 5000000)) (Int.toString (range 500000))
    ; null
    ; var (generateName ())
    ; partial (generateFnName ()) (generateExpr ())
    ; list (generateList () ~minSize:0 ~f:generateExpr)
    ; fn (generateFnName ()) (generateList ~minSize:0 ~f:generateExpr ())
    ; rightPartial (generateInfixName ()) (generateExpr ())
    ; fieldAccess (generateFieldAccessExpr ()) (generateName ())
    ; lambda (generateList ~minSize:1 ~f:generateName ()) (generateExpr ())
    ; let' (generateName ()) (generateExpr ()) (generateExpr ())
    ; binop (generateInfixName ()) (generateExpr ()) (generateExpr ())
    ; if' (generateExpr ()) (generateExpr ()) (generateExpr ())
    ; constructor (generateName ()) (generateList ~minSize:0 ~f:generateExpr ())
    ; pipe
        (generateExpr ())
        (generateList ~minSize:2 ~f:generatePipeArgumentExpr ())
    ; record
        (generateList ~minSize:1 () ~f:(fun () ->
             (generateName (), generateExpr ())))
    ; match'
        (generateExpr ())
        (generateList ~minSize:1 () ~f:(fun () ->
             (generatePattern (), generateExpr ()))) ]


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
  let childOr (exprs : E.t list) =
    List.find exprs ~f:(fun e -> E.toID e = id)
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
  E.walk ~f newExpr


let rec changeStrings (id : id) ~(f : string -> string) (expr : E.t) : E.t =
  let open FluidExpression in
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
                if id = E.toID expr then (f name, expr) else (name, expr))
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
    List.filterMap exprs ~f:(fun e ->
        if E.toID e = id then None else Some (r e))
  in
  if E.toID expr = id
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
                  if E.toID expr = id || FluidPattern.toID pattern = id
                  then None
                  else Some (pattern, expr))
                pairs )
      | ERecord (rid, fields) ->
          ERecord
            ( rid
            , List.filterMap
                ~f:(fun (name, expr) ->
                  if E.toID expr = id then None else Some (name, expr))
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


let rec simplify (id : id) (expr : E.t) : E.t =
  if E.toID expr = id && not (E.isBlank expr)
  then EInteger (id, "5")
  else E.walk ~f:(simplify id) expr


let reduce (test : FuzzTest.t) (ast : E.t) =
  let runThrough msg reducer ast =
    let tokenIDs =
      ast |> FluidPrinter.toTokens |> List.map ~f:(fun ti -> T.tid ti.token)
    in
    let eIDs = ast |> E.filterMap ~f:(fun e -> Some (E.toID e)) in
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
  let sentinel = int 56756756 in
  let oldAST = ref sentinel in
  let newAST = ref ast in
  while oldAST <> newAST do
    Js.log2 "starting to reduce\n" (toText !newAST) ;
    oldAST := !newAST ;
    let latestAST =
      !newAST
      |> runThrough "simplify" simplify
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
    let seed = ref !initialSeed in
    let continue_loop = ref true in
    Tester.describe test.name (fun () ->
        while !continue_loop do
          let name = test.name ^ " #" ^ string_of_int !seed in
          Tester.test name (fun () ->
              setSeed !seed ;
              let testcase = generateExpr () |> FluidExpression.clone in
              Js.log2 "testing: " name ;
              (* Js.log2 "testcase: " (E.show testcase) ; *)
              let passed =
                match try Some (test.fn testcase) with _ -> None with
                | Some (newAST, newState) ->
                    Js.log2 "checking: " name ;
                    test.check ~testcase ~newAST newState
                | None ->
                    false
              in
              if passed = false
              then (
                Js.log2 "failed: " name ;
                let reduced = reduce test testcase in
                Js.log2 "finished program:\n  " (toText reduced) ;
                Js.log2 "as expr:\n  " (E.show reduced) ;
                Js.log2 "as testcase:\n  " (FluidPrinter.eToTestcase reduced) ;
                fail () )
              else pass ()) ;
          seed := !seed + 1 ;
          continue_loop := !continue ;
          if !stopOnFail && Tester.fails () <> [] then exit (-1) ;
          ()
        done)
  with _ -> ()
