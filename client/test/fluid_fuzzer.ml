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
let defaultItemSize = 4

let itemSize = ref defaultItemSize

(* Continue after the first seed *)
let continue : bool ref = ref false

(* Stop after getting our first failure  *)
let stopOnFail : bool ref = ref true

(* Don't generate test cases that are too large *)
let maxTestSize : int ref = ref 10000

let testSize : int ref = ref 0

(* ------------------ *)
(* Debugging *)
(* ------------------ *)

let toText ast = FluidPrinter.eToHumanString ast

let pointerToText (p : blankOrData) : string = Pointer.toContent p

let debugAST (_length : int) (msg : string) (e : E.t) : unit =
  Js.log (msg ^ ":\n" ^ E.show e)


(* if length < !verbosityThreshold then Js.log (msg ^ ":\n" ^ E.show e) *)

(* ------------------ *)
(* Deterministic random number generator *)
(* ------------------ *)

(* Deterministic random number generator, where the random numbers are evenly
 * distributed across the range. *)
let state = ref 1

let setSeed (seed : int) = state := seed

let random () : float =
  let xOrShiftRand () : int =
    (* Adapted from the C version at https://en.wikipedia.org/wiki/Xorshift *)
    (* Javascript treats numbers as 32 bit in the presence of bitwise ops and
     * our OCaml compiler defers to Javascript for handling numbers.
     *)
    state := !state lxor (!state lsl 13) ;
    (* Using lsr instead of asr because these shifts assume unsigned ints,
    * which JS doesn't have *)
    state := !state lxor (!state lsr 17) ;
    state := !state lxor (!state lsl 5) ;
    !state
  in
  let twoToNeg32 = 2.0 ** -32.0 in
  (* Conversion from https://www.doornik.com/research/randomdouble.pdf *)
  (float_of_int (xOrShiftRand ()) *. twoToNeg32) +. 0.5


let range (max : int) : int = truncate (float_of_int max *. random ())

let oneOf (l : 'a list) : 'a =
  List.getAt ~index:(range (List.length l)) l |> Option.valueExn


(* ------------------ *)
(* AST generators *)
(* ------------------ *)

let generateLength maxLength = max 0 (1 + range (maxLength - 1))

let generateList ~(minSize : int) ~(f : unit -> 'a) () : 'a list =
  (* Lower the list lengths as we go so eventually the program converges *)
  itemSize := !itemSize - 1 ;
  let s = max minSize !itemSize in
  let result = List.initialize (generateLength s) (fun _ -> f ()) in
  itemSize := !itemSize + 1 ;
  result


let generateName () : string =
  let generateChar () : char =
    oneOf ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k']
  in
  generateList ~minSize:1 ~f:generateChar () |> String.fromList


let generateString () : string =
  let generateChar () : char =
    oneOf ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K']
  in
  generateList ~minSize:0 ~f:generateChar () |> String.fromList


let generateInfixName () : string = oneOf ["+"; "++"; "-"; "*"; "/"; "||"; "&&"]

let generateFnName () : string =
  oneOf
    [ "Int::add"
    ; "DB::set_v2"
    ; "HttpClient::post_v4"
    ; generateName ()
    ; "DB::getAll_v2"
    ; "DB::generateKey_v1"
    ; "Date::now_v1"
    ; "Date::now" ]


let checkTestSize ~(default : 'a) (f : unit -> 'a) =
  if !testSize >= !maxTestSize
  then default
  else (
    testSize := !testSize + 1 ;
    f () )


(* Fields can only have a subset of expressions in the fieldAccess *)
let rec generateFieldAccessExpr' () : FluidExpression.t =
  oneOf
    [ lazy (var (generateName ()))
    ; lazy (fieldAccess (generateFieldAccessExpr ()) (generateName ())) ]
  |> Lazy.force


and generateFieldAccessExpr () =
  checkTestSize ~default:b generateFieldAccessExpr'


let rec generatePattern' () : FluidPattern.t =
  oneOf
    [ lazy (pInt (range 500))
    ; lazy (pBool (random () < 0.5))
    ; lazy (pNull ())
    ; lazy
        (pConstructor
           (generateName ())
           (generateList ~minSize:0 ~f:generatePattern ()))
    ; lazy (pVar (generateName ()))
    ; lazy (pString (generateString ()))
    ; lazy (pFloat (range 5000000) (range 500000))
    ; lazy (pBlank ()) ]
  |> Lazy.force


and generatePattern () = checkTestSize ~default:(pBlank ()) generatePattern'

let rec generatePipeArgumentExpr' () : FluidExpression.t =
  oneOf
    [ lazy
        (lambda (generateList ~minSize:1 ~f:generateName ()) (generateExpr ()))
    ; lazy b
    ; lazy
        (fn
           (generateName ())
           (pipeTarget :: generateList ~minSize:0 ~f:generateExpr ()))
    ; lazy (binop (generateName ()) pipeTarget (generateExpr ())) ]
  |> Lazy.force


and generatePipeArgumentExpr () =
  checkTestSize ~default:b generatePipeArgumentExpr'


and generateExpr' () =
  oneOf
    [ lazy b
    ; lazy (str (generateString ()))
    ; lazy (int (range 500))
    ; lazy (bool (random () < 0.5))
    ; lazy (float' (range 5000000) (range 500000))
    ; lazy null
    ; lazy (var (generateName ()))
    ; lazy (partial (generateFnName ()) (generateExpr ()))
    ; lazy (list (generateList () ~minSize:0 ~f:generateExpr))
    ; lazy (fn (generateFnName ()) (generateList ~minSize:0 ~f:generateExpr ()))
    ; lazy (rightPartial (generateInfixName ()) (generateExpr ()))
    ; lazy (fieldAccess (generateFieldAccessExpr ()) (generateName ()))
    ; lazy
        (lambda (generateList ~minSize:1 ~f:generateName ()) (generateExpr ()))
    ; lazy (let' (generateName ()) (generateExpr ()) (generateExpr ()))
    ; lazy (binop (generateInfixName ()) (generateExpr ()) (generateExpr ()))
    ; lazy (if' (generateExpr ()) (generateExpr ()) (generateExpr ()))
    ; lazy
        (constructor
           (generateName ())
           (generateList ~minSize:0 ~f:generateExpr ()))
    ; lazy
        (pipe
           (generateExpr ())
           (generateList ~minSize:2 ~f:generatePipeArgumentExpr ()))
    ; lazy
        (record
           (generateList ~minSize:1 () ~f:(fun () ->
                (generateName (), generateExpr ()))))
    ; lazy
        (match'
           (generateExpr ())
           (generateList ~minSize:1 () ~f:(fun () ->
                (generatePattern (), generateExpr ())))) ]
  |> Lazy.force


and generateExpr () = checkTestSize ~default:b generateExpr'

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

let unwrap (id : ID.t) (ast : E.t) : E.t =
  let childOr (exprs : E.t list) =
    List.find exprs ~f:(fun e -> E.toID e = id)
  in
  E.postTraversal ast ~f:(fun e ->
      let newExpr =
        match e with
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
      Option.withDefault ~default:e newExpr)


let changeStrings (id : ID.t) ~(f : string -> string) (ast : E.t) : E.t =
  let fStr strid str = if strid = id then f str else str in
  E.postTraversal ast ~f:(function
      | ELet (id, name, rhs, next) ->
          ELet (id, fStr id name, rhs, next)
      | EFieldAccess (id, expr, fieldname) ->
          EFieldAccess (id, expr, fStr id fieldname)
      | EPartial (id, name, expr) ->
          let newName = fStr id name in
          if newName = "" then expr else EPartial (id, newName, expr)
      | ERightPartial (id, name, expr) ->
          let newName = fStr id name in
          if newName = "" then expr else ERightPartial (id, newName, expr)
      | EFnCall (id, name, exprs, ster) as e ->
          let newName = fStr id name in
          if newName = "" then e else EFnCall (id, newName, exprs, ster)
      | EBinOp (id, name, lhs, rhs, ster) as e ->
          let newName = fStr id name in
          if newName = "" then e else EBinOp (id, newName, lhs, rhs, ster)
      | ELambda (id, names, expr) ->
          let names =
            List.map names ~f:(fun (nid, name) ->
                if nid = id then (nid, fStr id name) else (nid, name))
          in
          ELambda (id, names, expr)
      | ERecord (rid, fields) ->
          ERecord
            ( rid
            , List.map
                ~f:(fun (name, expr) ->
                  if id = E.toID expr then (fStr id name, expr) else (name, expr))
                fields )
      | EString (id, str) ->
          EString (id, fStr id str)
      | expr ->
          expr)


let blankVarNames (id : ID.t) (expr : E.t) : E.t =
  changeStrings ~f:(fun _ -> "") id expr


let shortenNames (id : ID.t) (expr : E.t) : E.t =
  changeStrings ~f:(String.dropRight ~count:1) id expr


let remove (id : ID.t) (ast : E.t) : E.t =
  let removeFromList exprs = List.filter exprs ~f:(fun e -> E.toID e <> id) in
  E.postTraversal ast ~f:(function
      | e when E.toID e = id ->
          EBlank id
      | EFnCall (id, name, exprs, ster) ->
          EFnCall (id, name, removeFromList exprs, ster)
      | ELambda (id, names, expr) ->
          let names =
            names
            |> List.filter ~f:(fun (nid, _) -> nid <> id)
            |> fun x -> if x = [] then List.take ~count:1 names else x
          in
          ELambda (id, names, expr)
      | EList (id, exprs) ->
          EList (id, removeFromList exprs)
      | EMatch (mid, mexpr, pairs) ->
          EMatch
            ( mid
            , mexpr
            , List.filter pairs ~f:(fun (pattern, expr) ->
                  E.toID expr <> id && FluidPattern.toID pattern <> id) )
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
      | expr ->
          expr)


let simplify (id : ID.t) (ast : E.t) : E.t =
  E.update id ast ~f:(function EBlank e -> EBlank e | _ -> EInteger (id, "5"))


let reduce (test : FuzzTest.t) (ast : E.t) =
  let runThrough msg reducer ast =
    let tokenIDs =
      ast |> FluidTokenizer.tokenize |> List.map ~f:(fun ti -> T.tid ti.token)
    in
    let eIDs = ast |> E.filterMap ~f:(fun e -> Some (E.toID e)) in
    let ids =
      tokenIDs @ eIDs
      |> List.uniqueBy ~f:ID.toString
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


let generateTestCase () =
  testSize := 0 ;
  generateExpr () |> FluidExpression.clone


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
              let testcase = generateTestCase () in
              if test.ignore testcase
              then (
                Js.log2 "ignoring: " name ;
                skip () )
              else (
                Js.log2 "testing: " name ;
                debugAST 0 "starting with" testcase ;
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
                else pass () )) ;
          seed := !seed + 1 ;
          continue_loop := !continue ;
          if !stopOnFail && Tester.fails () <> [] then exit (-1) ;
          ()
        done)
  with _ -> ()
