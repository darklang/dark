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

let toText ast = eToString defaultTestState ast

let pointerToText p : string =
  match p with
  | PExpr e ->
      toText (Fluid.fromExpr Fluid_test_data.defaultTestState e)
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


let debugAST (length : int) (msg : string) (e : fluidExpr) : unit =
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
    ; fn : fluidExpr -> fluidExpr * fluidState
    ; check : testcase:fluidExpr -> newAST:fluidExpr -> fluidState -> bool }
end

(* ------------------ *)
(* Test case reduction *)
(* ------------------ *)

let rec remove (id : id) (expr : fluidExpr) : fluidExpr =
  let f = remove id in
  if eid expr = id
  then
    (* If we can clearly unwrap the expression by replacing the only
     * non-blank child, then we do it. Otherwise we return the current
     * expression, and will try again on a later iteration. *)
    let firstChild children =
      match children with
      | [] ->
          newB ()
      | [first] ->
          first
      | _ ->
          (* we'll let the algorithm remove some other stuff first *)
          expr
    in
    match expr with
    | EInteger _ | EString _ | EVariable _ | EBool _ | ENull _ | EFloat _ ->
        newB ()
    | EBlank _ ->
        expr
    | EPipeTarget _ ->
        (* dont remove pipetargets from here *)
        expr
    | ELet (_, _, _, EBlank _, next) ->
        next
    | ELet (_, _, _, rhs, EBlank _) ->
        rhs
    | ELet _ ->
        expr
    | EIf (_, EBlank _, EBlank _, elseexpr) ->
        elseexpr
    | EIf (_, EBlank _, ifexpr, EBlank _) ->
        ifexpr
    | EIf (_, cond, EBlank _, EBlank _) ->
        cond
    | EIf _ ->
        expr
    | EBinOp (_, _, EBlank _, rexpr, _) ->
        rexpr
    | EBinOp (_, _, lexpr, _, _) ->
        lexpr
    | EFieldAccess (_, expr, _, _) ->
        expr
    | EFnCall (_, _, exprs, _) ->
      (match firstChild exprs with EPipeTarget _ -> newB () | other -> other)
    | ELambda (_, _, expr) ->
        expr
    | EList (_, exprs) ->
        firstChild exprs
    | EMatch (_, EBlank _, pairs) ->
        pairs |> List.map ~f:Tuple2.second |> firstChild
    | EMatch (_, mexpr, []) ->
        mexpr
    | EMatch _ ->
        expr
    | ERecord (_, fields) ->
        fields |> List.map ~f:Tuple3.third |> firstChild
    | EPipe (_, exprs) ->
        firstChild exprs
    | EConstructor (_, _, _, exprs) ->
        firstChild exprs
    | EOldExpr _ ->
        expr
    | EPartial (_, _, oldExpr) ->
        oldExpr
    | ERightPartial (_, _, oldExpr) ->
        oldExpr
    | EFeatureFlag (_, _, _, cond, EBlank _, EBlank _) ->
        cond
    | EFeatureFlag (_, _, _, EBlank _, casea, EBlank _) ->
        casea
    | EFeatureFlag (_, _, _, EBlank _, EBlank _, caseb) ->
        caseb
    | EFeatureFlag _ ->
        expr
  else
    let processList exprs =
      (* Strip blanks from lists rather than process them. We strip only blanks
     * because we want other expressions the opportunity to process themselves.
     * *)
      List.filterMap exprs ~f:(fun e ->
          match e with EBlank bid when id = bid -> None | _ -> Some (f e) )
    in
    let fStr strid str = if strid = id then "" else str in
    match expr with
    | EInteger _
    | EBlank _
    | EString _
    | EVariable _
    | EBool _
    | ENull _
    | EPipeTarget _
    | EFloat _ ->
        expr
    | ELet (id, lhsID, name, rhs, next) ->
        ELet (id, lhsID, fStr lhsID name, f rhs, f next)
    | EIf (id, cond, ifexpr, elseexpr) ->
        EIf (id, f cond, f ifexpr, f elseexpr)
    | EBinOp (id, op, lexpr, rexpr, ster) ->
        EBinOp (id, op, f lexpr, f rexpr, ster)
    | EFieldAccess (id, expr, fieldID, fieldname) ->
        EFieldAccess (id, f expr, fieldID, fStr fieldID fieldname)
    | EFnCall (id, name, exprs, ster) ->
        EFnCall (id, name, processList exprs, ster)
    | ELambda (id, names, expr) ->
        let names =
          names
          |> List.filterMap ~f:(fun (nid, name) ->
                 if nid = id then None else Some (nid, name) )
          |> fun x -> if x = [] then List.take ~count:1 names else x
        in
        ELambda (id, names, f expr)
    | EList (id, exprs) ->
        EList (id, processList exprs)
    | EMatch (mid, mexpr, pairs) ->
        EMatch
          ( mid
          , f mexpr
          , List.filterMap
              ~f:(fun (pattern, expr) ->
                if eid expr = id || Fluid.pid pattern = id
                then None
                else Some (pattern, f expr) )
              pairs )
    | ERecord (rid, fields) ->
        ERecord
          ( rid
          , List.filterMap
              ~f:(fun (fid, name, expr) ->
                if fid = id || eid expr = id
                then None
                else Some (fid, name, f expr) )
              fields )
    | EPipe (id, exprs) ->
      ( match processList exprs with
      | [EBlank _; EBinOp (id, op, EPipeTarget ptid, rexpr, ster)]
      | [EBinOp (id, op, EPipeTarget ptid, rexpr, ster); EBlank _] ->
          EBinOp (id, op, EBlank ptid, rexpr, ster)
      | [EBlank _; EFnCall (id, name, EPipeTarget ptid :: tail, ster)]
      | [EFnCall (id, name, EPipeTarget ptid :: tail, ster); EBlank _] ->
          EFnCall (id, name, EBlank ptid :: tail, ster)
      | [justOne] ->
          justOne
      | newExprs ->
          EPipe (id, newExprs) )
    | EConstructor (id, nameID, name, exprs) ->
        EConstructor (id, nameID, name, processList exprs)
    | EOldExpr _ ->
        expr
    | EPartial (id, str, oldExpr) ->
        EPartial (id, str, f oldExpr)
    | ERightPartial (id, str, oldExpr) ->
        ERightPartial (id, str, f oldExpr)
    | EFeatureFlag (id, msg, msgid, cond, casea, caseb) ->
        EFeatureFlag (id, msg, msgid, f cond, f casea, f caseb)


let reduce (test : FuzzTest.t) (ast : fluidExpr) =
  let runThrough msg reducer ast =
    let pointers =
      ast
      |> fun x ->
      Fluid.toExpr x
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
          with _ -> Js.log "Exception, let's skip this one" ) ;
        Js.log "\n" ) ;
    !latestAST
  in
  let sentinel = Fluid_test_data.int "56756756" in
  let oldAST = ref sentinel in
  let newAST = ref ast in
  while oldAST <> newAST do
    Js.log2 "starting to reduce\n" (toText !newAST) ;
    oldAST := !newAST ;
    let latestAST = !newAST |> runThrough "removing" remove in
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
          let testcase =
            generateExpr () |> Fluid.clone ~state:defaultTestState
          in
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
            Js.log2 "structure" (show_fluidExpr reduced) ;
            fail () )
          else pass () )
    done
  with _ -> ()
