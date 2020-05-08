open Prelude
module T = FluidToken
module E = FluidExpression
module Pattern = FluidPattern
module Util = FluidUtil
open FluidTokenizer

type token = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

let tokensToString (tis : tokenInfo list) : string =
  tis |> List.map ~f:(fun ti -> T.toText ti.token) |> String.join ~sep:""


let eToTestString (e : E.t) : string =
  e
  |> tokenize
  |> List.map ~f:(fun ti -> T.toTestText ti.token)
  |> String.join ~sep:""


let eToHumanString (e : E.t) : string = e |> tokenize |> tokensToString

let eToStructure ?(includeIDs = false) (e : E.t) : string =
  e
  |> tokenize
  |> List.map ~f:(fun ti ->
         "<"
         ^ T.toTypeName ti.token
         ^ ( if includeIDs
           then "(" ^ (T.tid ti.token |> ID.toString) ^ ")"
           else "" )
         ^ ":"
         ^ T.toText ti.token
         ^ ">")
  |> String.join ~sep:""


let pToString (p : fluidPattern) : string =
  p
  |> FluidTokenizer.patternToToken ~idx:0
  |> List.map ~f:(fun t -> T.toTestText t)
  |> String.join ~sep:""


let pToStructure (p : fluidPattern) : string =
  p
  |> FluidTokenizer.patternToToken ~idx:0
  |> List.map ~f:(fun t -> "<" ^ T.toTypeName t ^ ":" ^ T.toText t ^ ">")
  |> String.join ~sep:""


(* ----------------- *)
(* Test cases *)
(* ----------------- *)
(* eToTestcase constructs testcases that we can enter in our
 * test suite. They are similar to `show` except that instead of the full code,
 * they use the shortcuts from Fluid_test_data. *)
(* ----------------- *)

let rec eToTestcase (e : E.t) : string =
  let r = eToTestcase in
  let quoted str = "\"" ^ str ^ "\"" in
  let listed elems = "[" ^ String.join ~sep:";" elems ^ "]" in
  let spaced elems = String.join ~sep:" " elems in
  let result =
    match e with
    | EBlank _ ->
        "b"
    | EString (_, str) ->
        spaced ["str"; quoted str]
    | EBool (_, true) ->
        spaced ["bool true"]
    | EBool (_, false) ->
        spaced ["bool false"]
    | EFloat (_, whole, fractional) ->
        spaced ["float'"; whole; fractional]
    | EInteger (_, int) ->
        spaced ["int"; int]
    | ENull _ ->
        "null"
    | EPipeTarget _ ->
        "pipeTarget"
    | EPartial (_, str, e) ->
        spaced ["partial"; quoted str; r e]
    | ERightPartial (_, str, e) ->
        spaced ["rightPartial"; quoted str; r e]
    | ELeftPartial (_, str, e) ->
        spaced ["prefixPartial"; quoted str; r e]
    | EFnCall (_, name, exprs, _) ->
        spaced ["fn"; quoted name; listed (List.map ~f:r exprs)]
    | EBinOp (_, name, lhs, rhs, _) ->
        spaced ["binop"; quoted name; r lhs; r rhs]
    | EVariable (_, name) ->
        spaced ["var"; quoted name]
    | EFieldAccess (_, expr, fieldname) ->
        spaced ["fieldAccess"; r expr; quoted fieldname]
    | EMatch (_, cond, matches) ->
        let rec pToTestcase (p : FluidPattern.t) : string =
          let quoted str = "\"" ^ str ^ "\"" in
          let listed elems = "[" ^ String.join ~sep:";" elems ^ "]" in
          let spaced elems = String.join ~sep:" " elems in
          match p with
          | FPBlank _ ->
              "pBlank"
          | FPString {str; _} ->
              spaced ["pString"; quoted str]
          | FPBool (_, _, true) ->
              spaced ["pBool true"]
          | FPBool (_, _, false) ->
              spaced ["pBool false"]
          | FPFloat (_, _, whole, fractional) ->
              spaced ["pFloat'"; whole; fractional]
          | FPInteger (_, _, int) ->
              spaced ["pInt"; int]
          | FPNull _ ->
              "pNull"
          | FPVariable (_, _, name) ->
              spaced ["pVar"; quoted name]
          | FPConstructor (_, _, name, args) ->
              spaced
                [ "pConstructor"
                ; quoted name
                ; listed (List.map args ~f:pToTestcase) ]
        in
        spaced
          [ "match'"
          ; r cond
          ; listed
              (List.map matches ~f:(fun (p, e) ->
                   "(" ^ pToTestcase p ^ ", " ^ r e ^ ")")) ]
    | ERecord (_, pairs) ->
        spaced
          [ "record"
          ; listed
              (List.map pairs ~f:(fun (k, v) ->
                   "(" ^ quoted k ^ ", " ^ r v ^ ")")) ]
    | EList (_, exprs) ->
        spaced ["list"; listed (List.map ~f:r exprs)]
    | EPipe (_, a :: rest) ->
        spaced ["pipe"; r a; listed (List.map ~f:r rest)]
    | EPipe (_, []) ->
        "INVALID PIPE - NO ELEMENTS"
    | EConstructor (_, name, exprs) ->
        spaced ["constructor"; quoted name; listed (List.map exprs ~f:r)]
    | EIf (_, cond, thenExpr, elseExpr) ->
        spaced ["if'"; r cond; r thenExpr; r elseExpr]
    | ELet (_, lhs, rhs, body) ->
        spaced ["let'"; quoted lhs; r rhs; r body]
    | ELambda (_, names, body) ->
        let names =
          List.map names ~f:(fun (_, name) -> quoted name) |> listed
        in
        spaced ["lambda"; names; r body]
    | EFeatureFlag (_, _, cond, oldCode, newCode) ->
        spaced ["ff"; r cond; r oldCode; r newCode]
  in
  "(" ^ result ^ ")"
