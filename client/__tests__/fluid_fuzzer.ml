open Tc
open Types
open Fluid
open Fluid_test_data
open Tester

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

let toText ast = FluidPrinter.eToString ast

let pointerToText p : string =
  match p with
  | PExpr e ->
      toText (FluidExpression.fromNExpr e)
  | PField b
  | PVarBind b
  | PKey b
  | PFFMsg b
  | PFnName b
  | PFnCallName b
  | PConstructorName b ->
      Blank.toMaybe b |> Option.withDefault ~default:"blank"
  | PPattern _ ->
      "pattern TODO"
  | _ ->
      "not valid here"


let debugAST (length : int) (msg : string) (e : E.t) : unit =
  if length < !verbosityThreshold then Js.log (msg ^ ":\n" ^ toText e)


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
      int (Int.toString (range 500))
  | 3 ->
      bool (random () < 0.5)
  | 4 ->
      float' (Int.toString (range 5000000)) (Int.toString (range 500000))
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
    ; fn : E.t -> E.t * fluidState
    ; check : testcase:E.t -> newAST:E.t -> fluidState -> bool }
end

(* ------------------ *)
(* Test case reduction *)
(* ------------------ *)

let rec unwrap (id : id) (expr : E.t) : E.t =
  let f = unwrap id in
  let childOr (exprs : E.t list) = List.find exprs ~f:(fun e -> E.id e = id) in
  let newExpr =
    match expr with
    | ELet (_, _, _, rhs, next) ->
        childOr [rhs; next]
    | EIf (_, cond, ifexpr, elseexpr) ->
        childOr [cond; ifexpr; elseexpr]
    | EBinOp (_, _, lexpr, rexpr, _) ->
        childOr [lexpr; rexpr]
    | EFieldAccess (_, expr, _, _) ->
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
        childOr (List.map ~f:Tuple3.third fields)
    | EPipe (_, exprs) ->
        childOr exprs
    | EConstructor (_, _, _, exprs) ->
        childOr exprs
    | EPartial (_, _, oldExpr) ->
        childOr [oldExpr]
    | ERightPartial (_, _, oldExpr) ->
        childOr [oldExpr]
    | EFeatureFlag (_, _, _, cond, casea, caseb) ->
        childOr [cond; casea; caseb]
    | _ ->
        None
  in
  let newExpr = Option.withDefault ~default:expr newExpr in
  E.walk ~f newExpr


let rec blankVarNames (id : id) (expr : E.t) : E.t =
  let f = blankVarNames id in
  let fStr strid str = if strid = id then "" else str in
  let newExpr =
    match expr with
    | ELet (id, lhsID, name, rhs, next) ->
        ELet (id, lhsID, fStr lhsID name, rhs, next)
    | EFieldAccess (id, expr, fieldID, fieldname) ->
        EFieldAccess (id, expr, fieldID, fStr fieldID fieldname)
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
              ~f:(fun (fid, name, expr) ->
                if fid = id then (fid, "", expr) else (fid, name, expr))
              fields )
    | _ ->
        expr
  in
  E.walk ~f newExpr


let rec remove (id : id) (expr : E.t) : E.t =
  let r e = remove id e in
  let f e = if E.id e = id then EBlank id else remove id e in
  let removeFromList exprs = List.filter exprs ~f:(fun e -> E.id e <> id) in
  if E.id expr = id
  then EBlank id
  else
    let newExpr =
      match expr with
      | EFieldAccess (id, expr, fieldID, fieldname) ->
          if id = fieldID
          then expr
          else EFieldAccess (id, r expr, fieldID, fieldname)
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
                  if E.id expr = id || FluidPattern.id pattern = id
                  then None
                  else Some (pattern, expr))
                pairs )
      | ERecord (rid, fields) ->
          ERecord
            ( rid
            , List.filterMap
                ~f:(fun (fid, name, expr) ->
                  if E.id expr = id || fid = id
                  then None
                  else Some (fid, name, expr))
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
      | EConstructor (id, nameID, name, exprs) ->
          EConstructor (id, nameID, name, removeFromList exprs)
      | _ ->
          expr
    in
    E.walk ~f newExpr


let reduce (test : FuzzTest.t) (ast : E.t) =
  let runThrough msg reducer ast =
    let pointers =
      ast
      |> fun x ->
      E.toNExpr x
      |> AST.allData
      |> List.uniqueBy ~f:(Pointer.toID >> Prelude.deID)
      |> List.indexedMap ~f:(fun i v -> (i, v))
    in
    let length = List.length pointers in
    let latestAST = ref ast in
    List.iter pointers ~f:(fun (idx, pointer) ->
        ( try
            let id = Pointer.toID pointer in
            Js.log2 msg (idx, length, id) ;
            let reducedAST = reducer id !latestAST in
            if !latestAST = reducedAST
            then Js.log "no change, trying next id"
            else
              let newAST, newState = test.fn reducedAST in
              let passed = test.check ~testcase:reducedAST ~newAST newState in
              if passed
              then (
                Js.log "removed the good bit, trying next id" ;
                debugAST length "started with" !latestAST ;
                debugAST length "result was" reducedAST ;
                debugAST length "after testing" newAST ;
                Js.log2 "pos is" newState.newPos )
              else (
                Js.log
                  "Success! We've reduced and it still fails. Let's keep going!" ;
                debugAST length "started with" !latestAST ;
                debugAST length "result was" reducedAST ;
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
      Tester.test name (fun () ->
          setSeed i ;
          let testcase = generateExpr () |> Fluid.clone in
          Js.log2 "testing: " name ;
          let newAST, newState = test.fn testcase in
          Js.log2 "checking: " name ;
          let passed = test.check ~testcase ~newAST newState in
          if passed = false
          then (
            Js.log2 "failed: " name ;
            let reduced = reduce test testcase in
            let text = toText reduced in
            Js.log2 "finished program:\n" text ;
            Js.log2 "structure" (E.show reduced) ;
            fail () )
          else pass ())
    done
  with _ -> ()
