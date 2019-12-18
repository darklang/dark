(* Specs:
 * Pressing enter
   * https://trello.com/c/nyf3SyA4/1513-handle-pressing-enter-at-the-end-of-a-line
   * https://trello.com/c/27jn8uzF/1507-handle-pressing-enter-at-the-start-of-a-line
 * renaming functions:
   * https://trello.com/c/49TTRcad/973-renaming-functions
 * movement shortcuts:
   * https://trello.com/c/IK9fQZoW/1072-support-ctrl-a-ctrl-e-ctrl-d-ctrl-k
 *)

(* open Webapi.Dom *)
open Tc
open Types
open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module TL = Toplevel
module Regex = Util.Regex

(* Tea *)
module Cmd = Tea.Cmd

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module AC = FluidAutocomplete
module Token = FluidToken

type viewState = ViewUtils.viewState

(* -------------------- *)
(* Utils *)
(* -------------------- *)

let newB () = EBlank (gid ())

let removeCharAt str offset : string =
  if offset < 0
  then str
  else
    String.slice ~from:0 ~to_:offset str
    ^ String.slice ~from:(offset + 1) ~to_:(String.length str) str


let isNumber (str : string) = Js.Re.test_ [%re "/[0-9]+/"] str

let isIdentifierChar (str : string) = Js.Re.test_ [%re "/[_a-zA-Z0-9]+/"] str

let isFnNameChar str =
  Js.Re.test_ [%re "/[_:a-zA-Z0-9]/"] str && String.length str = 1


(* truncateStringTo63BitInt only supports positive numbers for now, but we should change this once fluid supports negative numbers *)
(* If it is not an int after truncation, returns an error *)
let truncateStringTo63BitInt (s : string) : (string, string) Result.t =
  let is62BitInt s =
    match Native.BigInt.asUintN ~nBits:62 s with
    | Some i ->
        Native.BigInt.toString i = s
    | None ->
        false
  in
  (* 4611686018427387903 is largest 62 bit number, which has 19 characters *)
  (* We use 62 bit checks instead of 63 because the most significanty bit is for sign in two's complement -- which is not yet handled *)
  let trunc19 = String.left ~count:19 s in
  if is62BitInt trunc19
  then Ok trunc19
  else
    let trunc18 = String.left ~count:18 s in
    if is62BitInt trunc18
    then Ok trunc18
    else Error "Invalid 63bit number even after truncate"


(* Only supports positive numbers for now, but we should change this once fluid supports negative numbers *)
let coerceStringTo63BitInt (s : string) : string =
  Result.withDefault (truncateStringTo63BitInt s) ~default:"0"


(* Only supports positive numbers for now, but we should change this once fluid supports negative numbers *)
let is63BitInt (s : string) : bool = Result.isOk (truncateStringTo63BitInt s)

(* -------------------- *)
(* Expressions *)
(* -------------------- *)

type ast = fluidExpr [@@deriving show]

type state = Types.fluidState

let rec fromExpr ?(inPipe = false) (s : state) (expr : Types.expr) : fluidExpr
    =
  let fns =
    assertFn ~f:(( <> ) []) "empty functions passed to fromExpr" s.ac.functions
  in
  let f = fromExpr s in
  let varToName var = match var with Blank _ -> "" | F (_, name) -> name in
  let parseString str :
      [> `Bool of bool
      | `Int of string
      | `Null
      | `Float of string * string
      | `Unknown ] =
    let asBool =
      if str = "true"
      then Some (`Bool true)
      else if str = "false"
      then Some (`Bool false)
      else if str = "null"
      then Some `Null
      else None
    in
    let asInt = if is63BitInt str then Some (`Int str) else None in
    let asFloat =
      try
        (* for the exception *)
        ignore (float_of_string str) ;
        match String.split ~on:"." str with
        | [whole; fraction] ->
            Some (`Float (whole, fraction))
        | _ ->
            None
      with _ -> None
    in
    let asString =
      if String.startsWith ~prefix:"\"" str && String.endsWith ~suffix:"\"" str
      then
        Some
          (`String
            (str |> String.dropLeft ~count:1 |> String.dropRight ~count:1))
      else None
    in
    asInt
    |> Option.or_ asString
    |> Option.or_ asBool
    |> Option.or_ asFloat
    |> Option.withDefault ~default:`Unknown
  in
  match expr with
  | Blank id ->
      EBlank id
  | F (id, nExpr) ->
    ( match nExpr with
    | Let (name, rhs, body) ->
        ELet (id, Blank.toID name, varToName name, f rhs, f body)
    | Variable varname ->
        EVariable (id, varname)
    | If (cond, thenExpr, elseExpr) ->
        EIf (id, f cond, f thenExpr, f elseExpr)
    | ListLiteral exprs ->
        EList (id, List.map ~f exprs)
    | ObjectLiteral pairs ->
        ERecord
          ( id
          , List.map pairs ~f:(fun (k, v) -> (Blank.toID k, varToName k, f v))
          )
    | FieldAccess (expr, field) ->
        EFieldAccess (id, f expr, Blank.toID field, varToName field)
    | FnCall (name, args, ster) ->
        let args = List.map ~f args in
        (* add a pipetarget in the front *)
        let args = if inPipe then EPipeTarget (gid ()) :: args else args in
        let fnCall = EFnCall (id, varToName name, args, ster) in
        let fn = List.find fns ~f:(fun fn -> fn.fnName = varToName name) in
        ( match fn with
        | Some fn when fn.fnInfix ->
          ( match args with
          | [a; b] ->
              EBinOp (id, varToName name, a, b, ster)
          | _ ->
              fnCall )
        | _ ->
            fnCall )
    | Thread exprs ->
      ( match exprs with
      | head :: tail ->
          EPipe (id, f head :: List.map ~f:(fromExpr s ~inPipe:true) tail)
      | _ ->
          recover "empty pipe" ~debug:expr (newB ()) )
    | Lambda (varnames, exprs) ->
        ELambda
          ( id
          , List.map varnames ~f:(fun var -> (Blank.toID var, varToName var))
          , f exprs )
    | Value str ->
      ( match parseString str with
      | `Bool b ->
          EBool (id, b)
      | `Int i ->
          EInteger (id, i)
      | `String s ->
          EString (id, s)
      | `Null ->
          ENull id
      | `Float (whole, fraction) ->
          EFloat (id, whole, fraction)
      | `Unknown ->
          recover
            "Getting old Value that we coudln't parse"
            ~debug:str
            (EOldExpr expr) )
    | Constructor (name, exprs) ->
        EConstructor (id, Blank.toID name, varToName name, List.map ~f exprs)
    | Match (mexpr, pairs) ->
        let mid = id in
        let rec fromPattern (p : pattern) : fluidPattern =
          match p with
          | Blank id ->
              FPBlank (mid, id)
          | F (id, np) ->
            ( match np with
            | PVariable name ->
                FPVariable (mid, id, name)
            | PConstructor (name, patterns) ->
                FPConstructor (mid, id, name, List.map ~f:fromPattern patterns)
            | PLiteral str ->
              ( match parseString str with
              | `Bool b ->
                  FPBool (mid, id, b)
              | `Int i ->
                  FPInteger (mid, id, i)
              | `String s ->
                  FPString (mid, id, s)
              | `Null ->
                  FPNull (mid, id)
              | `Float (whole, fraction) ->
                  FPFloat (mid, id, whole, fraction)
              | `Unknown ->
                  recover
                    "Getting old pattern literal that we couldn't parse"
                    ~debug:p
                    (FPOldPattern (mid, p)) ) )
        in
        let pairs = List.map pairs ~f:(fun (p, e) -> (fromPattern p, f e)) in
        EMatch (id, f mexpr, pairs)
    | FeatureFlag (msg, cond, casea, caseb) ->
        EFeatureFlag
          ( id
          , varToName msg
          , Blank.toID msg
          , f cond
          , fromExpr ~inPipe s casea
          , fromExpr ~inPipe s caseb )
    | FluidPartial (str, oldExpr) ->
        EPartial (id, str, fromExpr ~inPipe s oldExpr)
    | FluidRightPartial (str, oldExpr) ->
        ERightPartial (id, str, fromExpr ~inPipe s oldExpr) )


let astAndStateFromTLID (m : model) (tlid : tlid) : (ast * state) option =
  (* TODO(JULIAN): codify removeHandlerTransientState as an external function, make `fromExpr`
    accept only the info it needs, and differentiate between handler-specific and global fluid state. *)
  let removeHandlerTransientState m =
    {m with fluidState = {m.fluidState with ac = AC.reset m}}
  in
  let maybeFluidAstAndState =
    TL.get m tlid
    |> Option.andThen ~f:TL.getAST
    |> Option.map ~f:(fun genericAst ->
           let state =
             (* We need to discard transient state if the selected handler has changed *)
             if Some tlid = tlidOf m.cursorState
             then m.fluidState
             else
               let newM = removeHandlerTransientState m in
               newM.fluidState
           in
           (fromExpr state genericAst, state) )
  in
  maybeFluidAstAndState


let literalToString
    (v :
      [> `Bool of bool | `Int of string | `Null | `Float of string * string]) :
    string =
  match v with
  | `Int i ->
      i
  | `String str ->
      "\"" ^ str ^ "\""
  | `Bool b ->
      if b then "true" else "false"
  | `Null ->
      "null"
  | `Float (whole, fraction) ->
      whole ^ "." ^ fraction


let rec toPattern (p : fluidPattern) : pattern =
  match p with
  | FPVariable (_, id, var) ->
      F (id, PVariable var)
  | FPConstructor (_, id, name, patterns) ->
      F (id, PConstructor (name, List.map ~f:toPattern patterns))
  | FPInteger (_, id, i) ->
      F (id, PLiteral (literalToString (`Int i)))
  | FPBool (_, id, b) ->
      F (id, PLiteral (literalToString (`Bool b)))
  | FPString (_, id, str) ->
      F (id, PLiteral (literalToString (`String str)))
  | FPFloat (_, id, whole, fraction) ->
      F (id, PLiteral (literalToString (`Float (whole, fraction))))
  | FPNull (_, id) ->
      F (id, PLiteral (literalToString `Null))
  | FPBlank (_, id) ->
      Blank id
  | FPOldPattern (_, pattern) ->
      pattern


let rec toExpr ?(inPipe = false) (expr : fluidExpr) : Types.expr =
  (* inPipe is whether it's the immediate child of a pipe. *)
  let r = toExpr ~inPipe:false in
  match expr with
  | EInteger (id, num) ->
      F (id, Value (literalToString (`Int num)))
  | EString (id, str) ->
      F (id, Value (literalToString (`String str)))
  | EFloat (id, whole, fraction) ->
      F (id, Value (literalToString (`Float (whole, fraction))))
  | EBool (id, b) ->
      F (id, Value (literalToString (`Bool b)))
  | ENull id ->
      F (id, Value (literalToString `Null))
  | EVariable (id, var) ->
      F (id, Variable var)
  | EFieldAccess (id, obj, fieldID, "") ->
      F (id, FieldAccess (toExpr obj, Blank fieldID))
  | EFieldAccess (id, obj, fieldID, fieldname) ->
      F (id, FieldAccess (toExpr obj, F (fieldID, fieldname)))
  | EFnCall (id, name, args, ster) ->
    ( match args with
    | EPipeTarget _ :: _ when not inPipe ->
        recover "fn has a pipe target but no pipe" ~debug:expr (Blank.new_ ())
    | EPipeTarget _ :: args when inPipe ->
        F
          ( id
          , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
          )
    | _nonPipeTarget :: _ when inPipe ->
        recover "fn has a pipe but no pipe target" ~debug:expr (Blank.new_ ())
    | args ->
        F
          ( id
          , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
          ) )
  | EBinOp (id, name, arg1, arg2, ster) ->
    ( match arg1 with
    | EPipeTarget _ when not inPipe ->
        recover
          "binop has a pipe target but no pipe"
          ~debug:expr
          (Blank.new_ ())
    | EPipeTarget _ when inPipe ->
        F (id, FnCall (F (ID (deID id ^ "_name"), name), [toExpr arg2], ster))
    | _nonPipeTarget when inPipe ->
        recover
          "binop has a pipe but no pipe target"
          ~debug:expr
          (Blank.new_ ())
    | _ ->
        F
          ( id
          , FnCall
              ( F (ID (deID id ^ "_name"), name)
              , [toExpr arg1; toExpr arg2]
              , ster ) ) )
  | ELambda (id, vars, body) ->
      F
        ( id
        , Lambda
            ( List.map vars ~f:(fun (vid, var) -> Types.F (vid, var))
            , toExpr body ) )
  | EBlank id ->
      Blank id
  | ELet (id, lhsID, lhs, rhs, body) ->
      F (id, Let (F (lhsID, lhs), toExpr rhs, toExpr body))
  | EIf (id, cond, thenExpr, elseExpr) ->
      F (id, If (toExpr cond, toExpr thenExpr, toExpr elseExpr))
  | EPartial (id, str, oldVal) ->
      F (id, FluidPartial (str, toExpr ~inPipe oldVal))
  | ERightPartial (id, str, oldVal) ->
      F (id, FluidRightPartial (str, toExpr ~inPipe oldVal))
  | EList (id, exprs) ->
      F (id, ListLiteral (List.map ~f:r exprs))
  | ERecord (id, pairs) ->
      F
        ( id
        , ObjectLiteral
            (List.map pairs ~f:(fun (id, k, v) -> (Types.F (id, k), toExpr v)))
        )
  | EPipe (id, exprs) ->
    ( match exprs with
    | head :: tail ->
        F (id, Thread (r head :: List.map ~f:(toExpr ~inPipe:true) tail))
    | [] ->
        Blank id )
  | EConstructor (id, nameID, name, exprs) ->
      F (id, Constructor (F (nameID, name), List.map ~f:r exprs))
  | EMatch (id, mexpr, pairs) ->
      let pairs = List.map pairs ~f:(fun (p, e) -> (toPattern p, toExpr e)) in
      F (id, Match (toExpr mexpr, pairs))
  | EPipeTarget _ ->
      recover
        "Cant convert pipetargets back to exprs"
        ~debug:expr
        (Blank.new_ ())
  | EFeatureFlag (id, name, nameID, cond, caseA, caseB) ->
      F
        ( id
        , FeatureFlag
            ( F (nameID, name)
            , toExpr cond
            , toExpr ~inPipe caseA
            , toExpr ~inPipe caseB ) )
  | EOldExpr expr ->
      expr


let eid expr : id =
  match expr with
  | EOldExpr expr ->
      Blank.toID expr
  | EInteger (id, _)
  | EString (id, _)
  | EBool (id, _)
  | ENull id
  | EFloat (id, _, _)
  | EVariable (id, _)
  | EFieldAccess (id, _, _, _)
  | EFnCall (id, _, _, _)
  | ELambda (id, _, _)
  | EBlank id
  | ELet (id, _, _, _, _)
  | EIf (id, _, _, _)
  | EPartial (id, _, _)
  | ERightPartial (id, _, _)
  | EList (id, _)
  | ERecord (id, _)
  | EPipe (id, _)
  | EPipeTarget id
  | EBinOp (id, _, _, _, _)
  | EConstructor (id, _, _, _)
  | EFeatureFlag (id, _, _, _, _, _)
  | EMatch (id, _, _) ->
      id


let pid pattern : id =
  match pattern with
  | FPVariable (_, id, _)
  | FPConstructor (_, id, _, _)
  | FPInteger (_, id, _)
  | FPBool (_, id, _)
  | FPString (_, id, _)
  | FPFloat (_, id, _, _)
  | FPNull (_, id)
  | FPBlank (_, id) ->
      id
  | FPOldPattern (_, pattern) ->
      Blank.toID pattern


let pmid pattern : id =
  match pattern with
  | FPVariable (mid, _, _)
  | FPConstructor (mid, _, _, _)
  | FPInteger (mid, _, _)
  | FPBool (mid, _, _)
  | FPString (mid, _, _)
  | FPFloat (mid, _, _, _)
  | FPNull (mid, _)
  | FPBlank (mid, _) ->
      mid
  | FPOldPattern (mid, _) ->
      mid


(* -------------------- *)
(* Tokens *)
(* -------------------- *)
type token = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

(** patternToToken takes a match pattern `p` and converts it to a list of
    fluidTokens.

    ~idx is the zero-based index of the pattern in the enclosing match *)
let rec patternToToken (p : fluidPattern) ~(idx : int) : fluidToken list =
  match p with
  | FPVariable (mid, id, name) ->
      [TPatternVariable (mid, id, name, idx)]
  | FPConstructor (mid, id, name, args) ->
      let args =
        List.map args ~f:(fun a -> TSep id :: patternToToken a ~idx)
      in
      List.concat ([TPatternConstructorName (mid, id, name, idx)] :: args)
  | FPInteger (mid, id, i) ->
      [TPatternInteger (mid, id, i, idx)]
  | FPBool (mid, id, b) ->
      if b
      then [TPatternTrue (mid, id, idx)]
      else [TPatternFalse (mid, id, idx)]
  | FPString (mid, id, s) ->
      [TPatternString (mid, id, s, idx)]
  | FPFloat (mID, id, whole, fraction) ->
      let whole =
        if whole = "" then [] else [TPatternFloatWhole (mID, id, whole, idx)]
      in
      let fraction =
        if fraction = ""
        then []
        else [TPatternFloatFraction (mID, id, fraction, idx)]
      in
      whole @ [TPatternFloatPoint (mID, id, idx)] @ fraction
  | FPNull (mid, id) ->
      [TPatternNullToken (mid, id, idx)]
  | FPBlank (mid, id) ->
      [TPatternBlank (mid, id, idx)]
  | FPOldPattern (mid, op) ->
      [TPatternString (mid, Blank.toID op, "TODO: old pattern", idx)]


module Builder = struct
  type t =
    { tokens : fluidToken list
    ; indent : (* tracks the indent after a newline *) int
    ; xPos :
        (* tracks the indent for nesting, none indicates it's ready to go after a newline *)
        int option }

  let rec endsInNewline (b : t) : bool =
    match List.reverse b.tokens with
    | TNewline _ :: _ ->
        true
    | TIndent _ :: tail ->
        endsInNewline {b with tokens = tail}
    | _ ->
        false


  let empty = {tokens = []; xPos = Some 0; indent = 0}

  let add (token : fluidToken) (b : t) : t =
    let tokenLength = token |> Token.toText |> String.length in
    let tokens, xPos =
      if endsInNewline b
      then
        ( (if b.indent <> 0 then [TIndent b.indent; token] else [token])
        , Some (b.indent + tokenLength) )
      else
        let newXPos =
          match token with
          | TNewline _ ->
              None
          | _ ->
              let old = Option.withDefault b.xPos ~default:b.indent in
              Some (old + tokenLength)
        in
        ([token], newXPos)
    in
    {b with tokens = b.tokens @ tokens; xPos}


  let addIf (cond : bool) (token : fluidToken) (b : t) : t =
    if cond then add token b else b


  (* Take a list of 'a, and iterate through them, adding them to `b` by
   * calling `f` on them *)
  let addIter (xs : 'a list) ~(f : int -> 'a -> t -> t) (b : t) : t =
    List.foldl xs ~init:(b, 0) ~f:(fun x (b, i) -> (f i x b, i + 1))
    |> Tuple2.first


  let addMany (tokens : fluidToken list) (b : t) : t =
    List.foldl tokens ~init:b ~f:add


  let indentBy ~(indent : int) ~(f : t -> t) (b : t) : t =
    let oldIndent = b.indent in
    let b = {b with indent = b.indent + indent} in
    let newB = f b in
    {newB with indent = oldIndent}


  let addNested ~(f : t -> t) (b : t) : t =
    let oldIndent = b.indent in
    let newIndent = Option.withDefault ~default:b.indent b.xPos in
    let b = {b with indent = newIndent} in
    let newB = f b in
    {newB with indent = oldIndent}


  let addNewlineIfNeeded (nlInfo : (id * id * int option) option) (b : t) : t =
    if endsInNewline b then b else add (TNewline nlInfo) b


  let asTokens (b : t) : fluidToken list = b.tokens
end

let rec toTokens' (s : state) (e : ast) (b : Builder.t) : Builder.t =
  let fns =
    assertFn
      ~f:(( <> ) [])
      "empty functions passed to toTokens'"
      s.ac.functions
  in
  let fromExpr e b = toTokens' s e b in
  let open Builder in
  let ghostPartial id newName oldName =
    let ghostSuffix = String.dropLeft ~count:(String.length newName) oldName in
    if ghostSuffix = "" then [] else [TPartialGhost (id, ghostSuffix)]
  in
  let nest
      ?(placeholderFor : (string * int) option = None)
      ~indent
      (e : fluidExpr)
      (b : Builder.t) : Builder.t =
    let tokensFn b =
      match (e, placeholderFor) with
      | EBlank id, Some (fnname, pos) ->
          let name =
            fns
            |> List.find ~f:(fun f -> f.fnName = fnname)
            |> Option.andThen ~f:(fun fn ->
                   List.getAt ~index:pos fn.fnParameters )
            |> Option.map ~f:(fun p ->
                   (p.paramName, Runtime.tipe2str p.paramTipe) )
          in
          ( match name with
          | None ->
              fromExpr e b
          | Some placeholder ->
              add (TPlaceholder (placeholder, id)) b )
      | _ ->
          fromExpr e b
    in
    b |> indentBy ~indent ~f:(addNested ~f:tokensFn)
  in
  let addArgs (name : string) (id : id) (args : fluidExpr list) (b : Builder.t)
      : Builder.t =
    let args, offset =
      match args with EPipeTarget _ :: args -> (args, 1) | _ -> (args, 0)
    in
    let reflow =
      let tokens =
        args
        |> List.map ~f:(fun a -> fromExpr a Builder.empty)
        |> List.map ~f:Builder.asTokens
        |> List.concat
      in
      let length =
        tokens
        |> List.map ~f:(Token.toText >> String.length)
        |> List.sum
        |> ( + ) (* separators, including at the front *) (List.length args)
        |> ( + ) (Option.withDefault ~default:0 b.xPos)
      in
      let tooLong = length > 120 in
      let needsNewlineBreak =
        (* newlines aren't disruptive in the last argument *)
        args
        |> List.init
        |> Option.withDefault ~default:[]
        |> List.map ~f:(fun a -> fromExpr a Builder.empty)
        |> List.map ~f:Builder.asTokens
        |> List.concat
        |> List.any ~f:(function TNewline _ -> true | _ -> false)
      in
      tooLong || needsNewlineBreak
    in
    b
    |> addIter args ~f:(fun i e b ->
           if reflow
           then
             b
             |> addNewlineIfNeeded (Some (id, id, Some (offset + i)))
             |> nest ~indent:2 ~placeholderFor:(Some (name, offset + i)) e
           else
             b
             |> add (TSep (eid e))
             |> nest ~indent:0 ~placeholderFor:(Some (name, offset + i)) e )
  in
  match e with
  | EInteger (id, i) ->
      add (TInteger (id, i)) b
  | EBool (id, bool') ->
      add (if bool' then TTrue id else TFalse id) b
  | ENull id ->
      add (TNullToken id) b
  | EFloat (id, whole, fraction) ->
      let whole = if whole = "" then [] else [TFloatWhole (id, whole)] in
      let fraction =
        if fraction = "" then [] else [TFloatFraction (id, fraction)]
      in
      addMany (whole @ [TFloatPoint id] @ fraction) b
  | EBlank id ->
      add (TBlank id) b
  | ELet (id, varId, lhs, rhs, next) ->
      b
      |> add (TLetKeyword (id, varId))
      |> add (TLetLHS (id, varId, lhs))
      |> add (TLetAssignment (id, varId))
      |> addNested ~f:(fromExpr rhs)
      |> addNewlineIfNeeded (Some (eid next, id, None))
      |> addNested ~f:(fromExpr next)
  | EString (id, str) ->
      let size = 40 in
      let strings =
        if String.length str > size then String.segment ~size str else [str]
      in
      ( match strings with
      | [] ->
          add (TString (id, "")) b
      | starting :: rest ->
        ( match List.reverse rest with
        | [] ->
            add (TString (id, str)) b
        | ending :: revrest ->
            b
            |> addNested ~f:(fun b ->
                   let endingOffset = size * (List.length revrest + 1) in
                   b
                   |> add (TStringMLStart (id, starting, 0, str))
                   |> add (TNewline None)
                   |> addIter (List.reverse revrest) ~f:(fun i s b ->
                          b
                          |> add (TStringMLMiddle (id, s, size * (i + 1), str))
                          |> add (TNewline None) )
                   |> add (TStringMLEnd (id, ending, endingOffset, str)) ) ) )
  | EIf (id, cond, if', else') ->
      b
      |> add (TIfKeyword id)
      |> addNested ~f:(fromExpr cond)
      |> addNewlineIfNeeded None
      |> add (TIfThenKeyword id)
      |> addNewlineIfNeeded (Some (eid if', id, None))
      |> nest ~indent:2 if'
      |> addNewlineIfNeeded None
      |> add (TIfElseKeyword id)
      |> add (TNewline (Some (eid else', id, None)))
      |> nest ~indent:2 else'
  | EBinOp (id, op, lexpr, rexpr, _ster) ->
      let start b =
        match lexpr with
        | EPipeTarget _ ->
            b
        | _ ->
            b
            |> nest ~indent:0 ~placeholderFor:(Some (op, 0)) lexpr
            |> add (TSep (eid lexpr))
      in
      b
      |> start
      |> addMany [TBinOp (id, op); TSep id]
      |> nest ~indent:0 ~placeholderFor:(Some (op, 1)) rexpr
  | EPartial (id, newName, EBinOp (_, oldName, lexpr, rexpr, _ster)) ->
      let ghost = ghostPartial id newName (ViewUtils.partialName oldName) in
      let start b =
        match lexpr with
        | EPipeTarget _ ->
            b
        | _ ->
            b
            |> nest ~indent:0 ~placeholderFor:(Some (oldName, 0)) lexpr
            |> add (TSep (eid lexpr))
      in
      b
      |> start
      |> add (TPartial (id, newName))
      |> addMany ghost
      |> add (TSep id)
      |> nest ~indent:2 ~placeholderFor:(Some (oldName, 1)) rexpr
  | EFnCall (id, fnName, args, ster) ->
      let displayName = ViewUtils.fnDisplayName fnName in
      let versionDisplayName = ViewUtils.versionDisplayName fnName in
      let partialName = ViewUtils.partialName fnName in
      let versionToken =
        if versionDisplayName = ""
        then []
        else [TFnVersion (id, partialName, versionDisplayName, fnName)]
      in
      b
      |> add (TFnName (id, partialName, displayName, fnName, ster))
      |> addMany versionToken
      |> addArgs fnName id args
  | EPartial (id, newName, EFnCall (_, oldName, args, _)) ->
      let partial = TPartial (id, newName) in
      let newText = Token.toText partial in
      let oldText = ViewUtils.partialName oldName in
      let ghost = ghostPartial id newText oldText in
      b |> add partial |> addMany ghost |> addArgs oldName id args
  | EConstructor (id, _, name, exprs) ->
      b |> add (TConstructorName (id, name)) |> addArgs name id exprs
  | EPartial (id, newName, EConstructor (_, _, oldName, exprs)) ->
      let partial = TPartial (id, newName) in
      let newText = Token.toText partial in
      let ghost = ghostPartial id newText oldName in
      b |> add partial |> addMany ghost |> addArgs oldName id exprs
  | EFieldAccess (id, expr, fieldID, fieldname) ->
      b
      |> addNested ~f:(fromExpr expr)
      |> addMany
           [ TFieldOp (id, (* lhs *) eid expr)
           ; TFieldName (id, fieldID, fieldname) ]
  | EPartial
      (id, newFieldname, EFieldAccess (faID, expr, fieldID, oldFieldname)) ->
      let partial = TFieldPartial (id, faID, fieldID, newFieldname) in
      let newText = Token.toText partial in
      let ghost = ghostPartial id newText oldFieldname in
      b
      |> addNested ~f:(fromExpr expr)
      |> addMany [TFieldOp (id, eid expr); partial]
      |> addMany ghost
  | EVariable (id, name) ->
      b |> add (TVariable (id, name))
  | ELambda (id, names, body) ->
      let isLast i = i = List.length names - 1 in
      b
      |> add (TLambdaSymbol id)
      |> addIter names ~f:(fun i (aid, name) b ->
             b
             |> add (TLambdaVar (id, aid, i, name))
             |> addIf (not (isLast i)) (TLambdaSep (id, i))
             |> addIf (not (isLast i)) (TSep aid) )
      |> add (TLambdaArrow id)
      |> nest ~indent:2 body
  | EList (id, exprs) ->
      let lastIndex = List.length exprs - 1 in
      b
      |> add (TListOpen id)
      |> addIter exprs ~f:(fun i e b ->
             b
             |> addNested ~f:(fromExpr e)
             |> addIf (i <> lastIndex) (TListSep (id, i)) )
      |> add (TListClose id)
  | ERecord (id, fields) ->
      if fields = []
      then b |> addMany [TRecordOpen id; TRecordClose id]
      else
        b
        |> add (TRecordOpen id)
        |> indentBy ~indent:2 ~f:(fun b ->
               addIter fields b ~f:(fun i (fieldID, fieldName, expr) b ->
                   b
                   |> addNewlineIfNeeded (Some (id, id, Some i))
                   |> add
                        (TRecordFieldname
                           {recordID = id; fieldID; index = i; fieldName})
                   |> add (TRecordSep (id, i, fieldID))
                   |> addNested ~f:(fromExpr expr) ) )
        |> addMany
             [ TNewline (Some (id, id, Some (List.length fields)))
             ; TRecordClose id ]
  | EPipe (id, exprs) ->
      let length = List.length exprs in
      ( match exprs with
      | [] ->
          recover "Empty pipe found" ~debug:e b
      | [single] ->
          recover "pipe with single entry found" ~debug:e (fromExpr single b)
      | head :: tail ->
          b
          |> addNested ~f:(fromExpr head)
          |> addNewlineIfNeeded (Some (eid head, id, Some 0))
          |> addIter tail ~f:(fun i e b ->
                 b
                 |> add (TPipe (id, i, length))
                 |> addNested ~f:(fromExpr e)
                 |> addNewlineIfNeeded (Some (eid e, id, Some (i + 1))) )
          |> addNewlineIfNeeded (Some (id, id, Some (List.length tail))) )
  | EPipeTarget _ ->
      recover "should never be making tokens for EPipeTarget" ~debug:e b
  | EMatch (id, mexpr, pairs) ->
      b
      |> add (TMatchKeyword id)
      |> addNested ~f:(fromExpr mexpr)
      |> indentBy ~indent:2 ~f:(fun b ->
             b
             |> addIter pairs ~f:(fun i (pattern, expr) b ->
                    b
                    |> addNewlineIfNeeded (Some (id, id, Some i))
                    |> addMany (patternToToken pattern ~idx:i)
                    |> addMany
                         [ TSep id
                         ; TMatchSep (pid pattern, i)
                         ; TSep (pid pattern) ]
                    |> addNested ~f:(fromExpr expr) )
             |> addNewlineIfNeeded (Some (id, id, Some (List.length pairs))) )
  | EOldExpr expr ->
      b |> add (TPartial (Blank.toID expr, "TODO: oldExpr"))
  | EPartial (id, str, _) ->
      b |> add (TPartial (id, str))
  | ERightPartial (id, newOp, expr) ->
      b
      |> addNested ~f:(fromExpr expr)
      |> addMany [TSep id; TRightPartial (id, newOp)]
  | EFeatureFlag (_id, _msg, _msgid, _cond, casea, _caseb) ->
      b |> addNested ~f:(fromExpr casea)


let infoize ~(pos : int) tokens : tokenInfo list =
  let row, col = (ref 0, ref 0) in
  let rec makeInfo p ts =
    match ts with
    | [] ->
        []
    | token :: rest ->
        let length = String.length (Token.toText token) in
        let ti =
          { token
          ; startRow = !row
          ; startCol = !col
          ; startPos = p
          ; endPos = p + length
          ; length }
        in
        ( match token with
        | TNewline _ ->
            row := !row + 1 ;
            col := 0
        | _ ->
            col := !col + length ) ;
        ti :: makeInfo (p + length) rest
  in
  makeInfo pos tokens


let validateTokens (tokens : fluidToken list) : fluidToken list =
  List.iter tokens ~f:(fun t ->
      asserT "invalid token" (String.length (Token.toText t) > 0) ~debug:t ;
      () ) ;
  tokens


(* Remove artifacts of the token generation process *)
let tidy (tokens : fluidToken list) : fluidToken list =
  tokens |> List.filter ~f:(function TIndent 0 -> false | _ -> true)


let toTokens (s : state) (e : ast) : tokenInfo list =
  toTokens' s e Builder.empty
  |> Builder.asTokens
  |> tidy
  |> validateTokens
  |> infoize ~pos:0


let eToString (s : state) (e : ast) : string =
  e
  |> toTokens s
  |> List.map ~f:(fun ti -> Token.toTestText ti.token)
  |> String.join ~sep:""


let tokensToString (tis : tokenInfo list) : string =
  tis |> List.map ~f:(fun ti -> Token.toText ti.token) |> String.join ~sep:""


let eToStructure ?(includeIDs = false) (s : state) (e : fluidExpr) : string =
  e
  |> toTokens s
  |> List.map ~f:(fun ti ->
         "<"
         ^ Token.toTypeName ti.token
         ^ (if includeIDs then "(" ^ (Token.tid ti.token |> deID) ^ ")" else "")
         ^ ":"
         ^ Token.toText ti.token
         ^ ">" )
  |> String.join ~sep:""


(* -------------------- *)
(* Patterns *)
(* -------------------- *)
let pToString (p : fluidPattern) : string =
  p
  |> patternToToken ~idx:0
  |> List.map ~f:(fun t -> Token.toTestText t)
  |> String.join ~sep:""


let pToStructure (p : fluidPattern) : string =
  p
  |> patternToToken ~idx:0
  |> infoize ~pos:0
  |> List.map ~f:(fun ti ->
         "<" ^ Token.toTypeName ti.token ^ ":" ^ Token.toText ti.token ^ ">" )
  |> String.join ~sep:""


(* -------------------- *)
(* Direct canvas interaction *)
(* -------------------- *)

let editorID = "fluid-editor"

(* -------------------- *)
(* Update fluid state *)
(* -------------------- *)
let tiSentinel : tokenInfo =
  { token = TSep (ID "sentinel-token")
  ; startPos = -1000
  ; startRow = -1000
  ; startCol = -1000
  ; endPos = -1000
  ; length = -1000 }


(* Returns a new state with the arbitrary string "action" recorded for debugging.
 * If a ~pos or ~ti (token info) is passed, it will be added to the action. *)
let recordAction
    ?(pos = -1000) ?(ti = tiSentinel) (action : string) (s : state) : state =
  let action =
    if pos = -1000 then action else action ^ " " ^ string_of_int pos
  in
  let action =
    if ti = tiSentinel then action else action ^ " " ^ show_fluidToken ti.token
  in
  {s with actions = s.actions @ [action]}


let setPosition ?(resetUD = false) (s : state) (pos : int) : state =
  let s = recordAction ~pos "setPosition" s in
  let upDownCol = if resetUD then None else s.upDownCol in
  {s with newPos = pos; selectionStart = None; upDownCol}


let report (e : string) (s : state) =
  let s = recordAction "report" s in
  {s with error = Some e}


(* -------------------- *)
(* Update *)
(* -------------------- *)

let length (tokens : token list) : int =
  tokens |> List.map ~f:Token.toText |> List.map ~f:String.length |> List.sum


(* Returns the token to the left and to the right. Ignores indent tokens *)

let getLeftTokenAt (newPos : int) (tis : tokenInfo list) : tokenInfo option =
  List.find ~f:(fun ti -> newPos <= ti.endPos && newPos >= ti.startPos) tis


type neighbour =
  | L of token * tokenInfo
  | R of token * tokenInfo
  | No

let rec getTokensAtPosition
    ?(prev = None) ~(pos : int) (tokens : tokenInfo list) :
    tokenInfo option * tokenInfo option * tokenInfo option =
  (* Get the next token and the remaining tokens, skipping indents. *)
  let rec getNextToken (infos : tokenInfo list) :
      tokenInfo option * tokenInfo list =
    match infos with
    | ti :: rest ->
        if Token.isSkippable ti.token
        then getNextToken rest
        else (Some ti, rest)
    | [] ->
        (None, [])
  in
  match getNextToken tokens with
  | None, _remaining ->
      (prev, None, None)
  | Some current, remaining ->
      if current.endPos > pos
      then
        let next, _ = getNextToken remaining in
        (prev, Some current, next)
      else getTokensAtPosition ~prev:(Some current) ~pos remaining


let getNeighbours ~(pos : int) (tokens : tokenInfo list) :
    neighbour * neighbour * tokenInfo option =
  let mPrev, mCurrent, mNext = getTokensAtPosition ~pos tokens in
  let toTheRight =
    match mCurrent with Some current -> R (current.token, current) | _ -> No
  in
  (* The token directly before the cursor (skipping whitespace) *)
  let toTheLeft =
    match (mPrev, mCurrent) with
    | Some prev, _ when prev.endPos >= pos ->
        L (prev.token, prev)
    (* The left might be separated by whitespace *)
    | Some prev, Some current when current.startPos >= pos ->
        L (prev.token, prev)
    | None, Some current when current.startPos < pos ->
        (* We could be in the middle of a token *)
        L (current.token, current)
    | None, _ ->
        No
    | _, Some current ->
        L (current.token, current)
    | Some prev, None ->
        (* Last position in the ast *)
        L (prev.token, prev)
  in
  (toTheLeft, toTheRight, mNext)


type gridPos =
  { row : int
  ; col : int }

(* Result will definitely be a valid position. *)
let gridFor ~(pos : int) (tokens : tokenInfo list) : gridPos =
  let ti =
    List.find tokens ~f:(fun ti -> ti.startPos <= pos && ti.endPos >= pos)
  in
  match ti with
  | Some ti ->
      if FluidToken.isNewline ti.token
      then {row = ti.startRow + 1; col = 0}
      else {row = ti.startRow; col = ti.startCol + (pos - ti.startPos)}
  | None ->
      {row = 0; col = 0}


(* Result will definitely be a valid position. *)
let posFor ~(row : int) ~(col : int) (tokens : tokenInfo list) : int =
  if row < 0 || col < 0
  then 0
  else
    let ti =
      List.find tokens ~f:(fun ti ->
          ti.startRow = row
          && ti.startCol <= col
          && ti.startCol + ti.length >= col )
    in
    match ti with
    | Some ti ->
        ti.startPos + (col - ti.startCol)
    | None ->
      (match List.last tokens with None -> 0 | Some last -> last.endPos)


(* Ensures that the position places will be valid within the code. When placed
 * in an indent, will be moved to the next char. Empty lines get moved to the
 * start. When placed past the end of a line, will go on the end of the last
 * item in it. *)
let adjustedPosFor ~(row : int) ~(col : int) (tokens : tokenInfo list) : int =
  if row < 0 || col < 0
  then 0
  else
    let thisRow = List.filter tokens ~f:(fun ti -> ti.startRow = row) in
    let ti =
      List.find thisRow ~f:(fun ti ->
          ti.startCol <= col && ti.startCol + ti.length > col )
    in
    match ti with
    | Some ti ->
        ti.startPos + (col - ti.startCol)
    | None ->
      ( match (List.head thisRow, List.last thisRow) with
      | Some h, _ when col < h.startCol ->
          h.startPos
      | _, Some l when col >= l.startCol ->
        ( match l.token with
        | TNewline _ ->
            l.startPos
        | _ ->
            l.startPos + l.length )
      | None, None ->
          posFor ~row ~col:0 tokens
      | _, _ ->
          recover "unexpected adjustedPosFor" ~debug:(row, col) 0 )


(* ------------- *)
(* Getting expressions *)
(* ------------- *)
let rec findExpr (id : id) (expr : fluidExpr) : fluidExpr option =
  let fe = findExpr id in
  if eid expr = id
  then Some expr
  else
    match expr with
    | EInteger _
    | EBlank _
    | EString _
    | EVariable _
    | EBool _
    | ENull _
    | EPipeTarget _
    | EFloat _ ->
        None
    | ELet (_, _, _, rhs, next) ->
        fe rhs |> Option.orElse (fe next)
    | EIf (_, cond, ifexpr, elseexpr) ->
        fe cond |> Option.orElse (fe ifexpr) |> Option.orElse (fe elseexpr)
    | EBinOp (_, _, lexpr, rexpr, _) ->
        fe lexpr |> Option.orElse (fe rexpr)
    | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
        fe expr
    | ERecord (_, fields) ->
        fields |> List.map ~f:Tuple3.third |> List.filterMap ~f:fe |> List.head
    | EMatch (_, expr, pairs) ->
        fe expr
        |> Option.orElse
             ( pairs
             |> List.map ~f:Tuple2.second
             |> List.filterMap ~f:fe
             |> List.head )
    | EFnCall (_, _, exprs, _)
    | EList (_, exprs)
    | EConstructor (_, _, _, exprs)
    | EPipe (_, exprs) ->
        List.filterMap ~f:fe exprs |> List.head
    | EOldExpr _ ->
        None
    | EPartial (_, _, oldExpr) | ERightPartial (_, _, oldExpr) ->
        fe oldExpr
    | EFeatureFlag (_, _, _, cond, casea, caseb) ->
        fe cond |> Option.orElse (fe casea) |> Option.orElse (fe caseb)


let isEmpty (e : fluidExpr) : bool =
  let isBlank e = match e with EBlank _ -> true | _ -> false in
  match e with
  | EBlank _ ->
      true
  | ERecord (_, []) ->
      true
  | ERecord (_, l) ->
      l
      |> List.filter ~f:(fun (_, k, v) -> k = "" && not (isBlank v))
      |> List.isEmpty
  | EList (_, l) ->
      l |> List.filter ~f:(not << isBlank) |> List.isEmpty
  | _ ->
      false


let exprIsEmpty (id : id) (ast : ast) : bool =
  match findExpr id ast with Some e -> isEmpty e | _ -> false


let findParent (id : id) (ast : ast) : fluidExpr option =
  let rec findParent' ~(parent : fluidExpr option) (id : id) (expr : fluidExpr)
      : fluidExpr option =
    let fp = findParent' ~parent:(Some expr) id in
    if eid expr = id
    then parent
    else
      match expr with
      | EInteger _
      | EBlank _
      | EString _
      | EVariable _
      | EBool _
      | ENull _
      | EPipeTarget _
      | EFloat _ ->
          None
      | ELet (_, _, _, rhs, next) ->
          fp rhs |> Option.orElse (fp next)
      | EIf (_, cond, ifexpr, elseexpr) ->
          fp cond |> Option.orElse (fp ifexpr) |> Option.orElse (fp elseexpr)
      | EBinOp (_, _, lexpr, rexpr, _) ->
          fp lexpr |> Option.orElse (fp rexpr)
      | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
          fp expr
      | EMatch (_, _, pairs) ->
          pairs
          |> List.map ~f:Tuple2.second
          |> List.filterMap ~f:fp
          |> List.head
      | ERecord (_, fields) ->
          fields
          |> List.map ~f:Tuple3.third
          |> List.filterMap ~f:fp
          |> List.head
      | EFnCall (_, _, exprs, _)
      | EList (_, exprs)
      | EConstructor (_, _, _, exprs)
      | EPipe (_, exprs) ->
          List.filterMap ~f:fp exprs |> List.head
      | EOldExpr _ ->
          None
      | EPartial (_, _, expr) ->
          fp expr
      | ERightPartial (_, _, expr) ->
          fp expr
      | EFeatureFlag (_, _, _, cond, casea, caseb) ->
          fp cond |> Option.orElse (fp casea) |> Option.orElse (fp caseb)
  in
  findParent' ~parent:None id ast


(* ------------- *)
(* Replacing expressions *)
(* ------------- *)
(* f needs to call recurse or it won't go far *)
let recurse ~(f : fluidExpr -> fluidExpr) (expr : fluidExpr) : fluidExpr =
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
      ELet (id, lhsID, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) ->
      EIf (id, f cond, f ifexpr, f elseexpr)
  | EBinOp (id, op, lexpr, rexpr, ster) ->
      EBinOp (id, op, f lexpr, f rexpr, ster)
  | EFieldAccess (id, expr, fieldID, fieldname) ->
      EFieldAccess (id, f expr, fieldID, fieldname)
  | EFnCall (id, name, exprs, ster) ->
      EFnCall (id, name, List.map ~f exprs, ster)
  | ELambda (id, names, expr) ->
      ELambda (id, names, f expr)
  | EList (id, exprs) ->
      EList (id, List.map ~f exprs)
  | EMatch (id, mexpr, pairs) ->
      EMatch
        (id, f mexpr, List.map ~f:(fun (name, expr) -> (name, f expr)) pairs)
  | ERecord (id, fields) ->
      ERecord
        (id, List.map ~f:(fun (id, name, expr) -> (id, name, f expr)) fields)
  | EPipe (id, exprs) ->
      EPipe (id, List.map ~f exprs)
  | EConstructor (id, nameID, name, exprs) ->
      EConstructor (id, nameID, name, List.map ~f exprs)
  | EOldExpr _ ->
      expr
  | EPartial (id, str, oldExpr) ->
      EPartial (id, str, f oldExpr)
  | ERightPartial (id, str, oldExpr) ->
      ERightPartial (id, str, f oldExpr)
  | EFeatureFlag (id, msg, msgid, cond, casea, caseb) ->
      EFeatureFlag (id, msg, msgid, f cond, f casea, f caseb)


(* Slightly modified version of `AST.uses` (pre-fluid code) *)
let rec updateVariableUses (oldVarName : string) ~(f : ast -> ast) (ast : ast)
    : ast =
  let u = updateVariableUses oldVarName ~f in
  match ast with
  | EVariable (_, varName) ->
      if varName = oldVarName then f ast else ast
  | ELet (id, id', lhs, rhs, body) ->
      if oldVarName = lhs (* if variable name is rebound *)
      then ast
      else ELet (id, id', lhs, u rhs, u body)
  | ELambda (id, vars, lexpr) ->
      if List.map ~f:Tuple2.second vars |> List.member ~value:oldVarName
         (* if variable name is rebound *)
      then ast
      else ELambda (id, vars, u lexpr)
  | EMatch (id, cond, pairs) ->
      let pairs =
        List.map
          ~f:(fun (pat, expr) ->
            if Pattern.hasVariableNamed oldVarName (toPattern pat)
            then (pat, expr)
            else (pat, u expr) )
          pairs
      in
      EMatch (id, u cond, pairs)
  | _ ->
      recurse ~f:u ast


let renameVariableUses (oldVarName : string) (newVarName : string) (ast : ast)
    : ast =
  let f expr =
    match expr with
    | EVariable (id, _) ->
        EVariable (id, newVarName)
    | _ ->
        expr
  in
  updateVariableUses oldVarName ~f ast


let removeVariableUse (oldVarName : string) (ast : ast) : ast =
  let f _ = EBlank (gid ()) in
  updateVariableUses oldVarName ~f ast


(* updateExpr searches `ast` for an expression `e` with the given `id` and when
 * found replaces `e` with `f e`. *)
let updateExpr ~(f : fluidExpr -> fluidExpr) (id : id) (ast : ast) : ast =
  let found = ref false in
  let rec run e =
    if id = eid e
    then (
      found := true ;
      f e )
    else recurse ~f:run e
  in
  let finished = run ast in
  asserT
    ~debug:(id, ast)
    "didn't find the id in the expression to update"
    !found ;
  finished


let replaceExpr ~(newExpr : fluidExpr) (id : id) (ast : ast) : ast =
  (* If we're putting a pipe into another pipe, fix it up *)
  let id, newExpr =
    match (findParent id ast, newExpr) with
    | Some (EPipe (parentID, oldExprs)), EPipe (newID, newExprs) ->
        let before, elemAndAfter =
          List.splitWhen ~f:(fun nested -> eid nested = id) oldExprs
        in
        let after = List.tail elemAndAfter |> Option.withDefault ~default:[] in
        (parentID, EPipe (newID, before @ newExprs @ after))
    | _ ->
        (id, newExpr)
  in
  updateExpr id ast ~f:(fun _ -> newExpr)


(* ---------------- *)
(* Movement *)
(* ---------------- *)
let moveToPrevNonWhitespaceToken ~pos (ast : ast) (s : state) : state =
  let s = recordAction ~pos "moveToPrevNonWhitespaceToken" s in
  let rec getNextWS tokens =
    match tokens with
    | [] ->
        pos
    | ti :: rest ->
      ( match ti.token with
      | TSep _ | TNewline _ | TIndent _ ->
          getNextWS rest
      | _ ->
          if pos < ti.startPos then getNextWS rest else ti.startPos )
  in
  let newPos = getNextWS (List.reverse (toTokens s ast)) in
  setPosition ~resetUD:true s newPos


let moveToNextNonWhitespaceToken ~pos (ast : ast) (s : state) : state =
  let s = recordAction ~pos "moveToNextNonWhitespaceToken" s in
  let rec getNextWS tokens =
    match tokens with
    | [] ->
        pos
    | ti :: rest ->
      ( match ti.token with
      | TSep _ | TNewline _ | TIndent _ ->
          getNextWS rest
      | _ ->
          if pos > ti.startPos then getNextWS rest else ti.startPos )
  in
  let newPos = getNextWS (toTokens s ast) in
  setPosition ~resetUD:true s newPos


(* getStartOfLineCaretPos returns the first desirable (excluding indents, pipes, and newline tokens)
 caret pos at the start of the line containing the given tokenInfo *)
let getStartOfLineCaretPos (ast : ast) (ti : tokenInfo) (s : state) : int =
  let token =
    toTokens s ast
    |> List.find ~f:(fun info ->
           if info.startRow == ti.startRow
           then
             match info.token with
             (* To prevent the result pos from being set inside TPipe or TIndent tokens *)
             | TPipe _ | TIndent _ ->
                 false
             | _ ->
                 true
           else false )
    |> Option.withDefault ~default:ti
  in
  token.startPos


(* getBegOfWordInStrCaretPos returns the closest whitespace position before the
 * current caret position in a string  *)
let getBegOfWordInStrCaretPos ~(pos : int) (ti : tokenInfo) : int =
  let posInString = pos - ti.startPos in
  let nextPos : int ref = ref ti.length in
  let _ =
    Token.toText ti.token
    |> String.split ~on:""
    |> List.reverse
    |> List.find ~f:(fun a ->
           if (a == " " || a = "\"" || a = "\n" || a = "\t")
              && !nextPos < posInString
           then true
           else (
             nextPos := !nextPos - 1 ;
             false ) )
  in
  ti.startPos + !nextPos


(* getEndOfWordInStrCaretPos returns the closest whitespace position after the
 * current caret position in a string  *)
let getEndOfWordInStrCaretPos ~(pos : int) (ti : tokenInfo) : int =
  let posInString = pos - ti.startPos in
  let nextPos : int ref = ref 0 in
  let _ =
    Token.toText ti.token
    |> String.split ~on:""
    |> List.find ~f:(fun a ->
           if (a == " " || (a = "\"" && !nextPos > 0) || a = "\n" || a = "\t")
              && !nextPos > posInString
           then true
           else (
             nextPos := !nextPos + 1 ;
             false ) )
  in
  ti.startPos + !nextPos


(* getEndOfLineCaretPos returns the last desirable (excluding indents and newline tokens)
 caret pos at the end of the line containing the given tokenInfo *)
let getEndOfLineCaretPos (ast : ast) (ti : tokenInfo) (s : state) : int =
  let token =
    toTokens s ast
    |> List.reverse
    |> List.find ~f:(fun info -> info.startRow == ti.startRow)
    |> Option.withDefault ~default:ti
  in
  let pos =
    match token.token with
    (* To prevent the result pos from being set to the end of an indent or to a new line *)
    | TNewline _ | TIndent _ ->
        token.startPos
    | _ ->
        token.endPos
  in
  pos


(* moveToStartOfLine moves the caret to the first desirable (excluding indents, pipes, and newline tokens)
 caret pos at the start of the line containing the given tokenInfo *)
let moveToStartOfLine (ast : ast) (ti : tokenInfo) (s : state) : state =
  let s = recordAction "moveToStartOfLine" s in
  setPosition s (getStartOfLineCaretPos ast ti s)


(* moveToEndOfLine moves the caret to the last desirable (excluding indents and newline tokens)
 caret pos at the end of the line containing the given tokenInfo *)
let moveToEndOfLine (ast : ast) (ti : tokenInfo) (s : state) : state =
  let s = recordAction "moveToEndOfLine" s in
  setPosition s (getEndOfLineCaretPos ast ti s)


let goToStartOfWord ~(pos : int) (ast : ast) (ti : tokenInfo) (s : state) :
    state =
  let s = recordAction "goToStartOfWord" s in
  (* We want to find the closest editable token that is before the current cursor position
  * so the cursor always lands in a position where a user is able to type *)
  let previousToken =
    toTokens s ast
    |> List.reverse
    |> List.find ~f:(fun t -> Token.isTextToken t.token && pos > t.startPos)
  in
  let newPos =
    let tokenInfo = previousToken |> Option.withDefault ~default:ti in
    if Token.isStringToken tokenInfo.token && pos != tokenInfo.startPos
    then getBegOfWordInStrCaretPos ~pos tokenInfo
    else tokenInfo.startPos
  in
  setPosition s newPos


let goToEndOfWord ~(pos : int) (ast : ast) (ti : tokenInfo) (s : state) : state
    =
  let s = recordAction "goToEndOfWord" s in
  (* We want to find the closest editable token that is after the current cursor position
  * so the cursor always lands in a position where a user is able to type *)
  let nextToken =
    toTokens s ast
    |> List.find ~f:(fun t -> Token.isTextToken t.token && pos < t.endPos)
  in
  let newPos =
    let tokenInfo = nextToken |> Option.withDefault ~default:ti in
    if Token.isStringToken tokenInfo.token && pos != tokenInfo.endPos
    then getEndOfWordInStrCaretPos ~pos tokenInfo
    else tokenInfo.endPos
  in
  setPosition s newPos


let moveToEnd (ti : tokenInfo) (s : state) : state =
  let s = recordAction ~ti "moveToEnd" s in
  setPosition ~resetUD:true s (ti.endPos - 1)


let moveToStart (ti : tokenInfo) (s : state) : state =
  let s = recordAction ~ti ~pos:ti.startPos "moveToStart" s in
  setPosition ~resetUD:true s ti.startPos


let moveToAfter (ti : tokenInfo) (s : state) : state =
  let s = recordAction ~ti ~pos:ti.endPos "moveToAfter" s in
  setPosition ~resetUD:true s ti.endPos


let moveOneLeft (pos : int) (s : state) : state =
  let s = recordAction ~pos "moveOneLeft" s in
  setPosition ~resetUD:true s (max 0 (pos - 1))


let moveOneRight (pos : int) (s : state) : state =
  let s = recordAction ~pos "moveOneRight" s in
  setPosition ~resetUD:true s (pos + 1)


let moveTo (newPos : int) (s : state) : state =
  let s = recordAction ~pos:newPos "moveTo" s in
  setPosition s newPos


(* Find first `target` expression (starting at the back), and return a state
 * with its location. If blank, will go to the start of the blank *)
let moveToEndOfTarget (target : id) (ast : ast) (s : state) : state =
  let s = recordAction "moveToEndOfTarget" s in
  let tokens = toTokens s ast in
  match
    List.find (List.reverse tokens) ~f:(fun ti ->
        FluidToken.tid ti.token = target )
  with
  | None ->
      recover "cannot find token to moveToEndOfTarget" ~debug:(target, ast) s
  | Some lastToken ->
      let newPos =
        if FluidToken.isBlank lastToken.token
        then lastToken.startPos
        else lastToken.endPos
      in
      moveTo newPos s


let rec getNextBlank (pos : int) (tokens : tokenInfo list) : tokenInfo option =
  tokens
  |> List.find ~f:(fun ti -> Token.isBlank ti.token && ti.startPos > pos)
  |> Option.orElseLazy (fun () ->
         if pos = 0 then None else getNextBlank 0 tokens )


let getNextBlankPos (pos : int) (tokens : tokenInfo list) : int =
  tokens
  |> getNextBlank pos
  |> Option.map ~f:(fun ti -> ti.startPos)
  |> Option.withDefault ~default:pos


let moveToNextBlank ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction ~pos "moveToNextBlank" s in
  let tokens = toTokens s ast in
  let newPos = getNextBlankPos pos tokens in
  setPosition ~resetUD:true s newPos


let rec getPrevBlank (pos : int) (tokens : tokenInfo list) : tokenInfo option =
  tokens
  |> List.filter ~f:(fun ti -> Token.isBlank ti.token && ti.endPos < pos)
  |> List.last
  |> Option.orElseLazy (fun () ->
         let lastPos =
           List.last tokens
           |> Option.map ~f:(fun ti -> ti.endPos)
           |> Option.withDefault ~default:0
         in
         if pos = lastPos then None else getPrevBlank lastPos tokens )


let getPrevBlankPos (pos : int) (tokens : tokenInfo list) : int =
  tokens
  |> getPrevBlank pos
  |> Option.map ~f:(fun ti -> ti.startPos)
  |> Option.withDefault ~default:pos


let moveToPrevBlank ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction ~pos "moveToPrevBlank" s in
  let tokens = toTokens s ast in
  let newPos = getPrevBlankPos pos tokens in
  setPosition ~resetUD:true s newPos


let doLeft ~(pos : int) (ti : tokenInfo) (s : state) : state =
  let s = recordAction ~ti ~pos "doLeft" s in
  if Token.isAtom ti.token
  then moveToStart ti s
  else moveOneLeft (min pos ti.endPos) s


let selectAll ~(pos : int) (ast : ast) (s : state) : state =
  let tokens = toTokens s ast in
  let last = List.last tokens in
  let lastPos = match last with Some l -> l.endPos | None -> 0 in
  {s with newPos = lastPos; oldPos = pos; selectionStart = Some 0}


let doRight
    ~(pos : int) ~(next : tokenInfo option) (current : tokenInfo) (s : state) :
    state =
  let s = recordAction ~ti:current ~pos "doRight" s in
  if Token.isAtom current.token
  then
    match next with
    | None ->
        moveToAfter current s
    | Some nInfo ->
        moveToStart nInfo s
  else
    match next with
    | Some n when pos + 1 >= current.endPos ->
        moveToStart n s
    | _ ->
        (* When we're in whitespace, current is the next non-whitespace. So we
         * don't want to use pos, we want to use the startPos of current. *)
        let startingPos = max pos (current.startPos - 1) in
        moveOneRight startingPos s


let doUp ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction ~pos "doUp" s in
  let tokens = toTokens s ast in
  let {row; col} = gridFor ~pos tokens in
  let col = match s.upDownCol with None -> col | Some savedCol -> savedCol in
  if row = 0
  then moveTo 0 s
  else
    let pos = adjustedPosFor ~row:(row - 1) ~col tokens in
    moveTo pos {s with upDownCol = Some col}


let doDown ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction ~pos "doDown" s in
  let tokens = toTokens s ast in
  let {row; col} = gridFor ~pos tokens in
  let col = match s.upDownCol with None -> col | Some savedCol -> savedCol in
  let pos = adjustedPosFor ~row:(row + 1) ~col tokens in
  moveTo pos {s with upDownCol = Some col}


(******************************)
(* Movement with CaretTarget  *)
(******************************)

(* posFromCaretTarget returns the position in the token stream corresponding to
   the passed caretTarget within the passed ast. We expect to succeed in finding
   the target. If we cannot, we `recover` and return the current caret pos
   as a fallback.

   This is useful for determining the precise position to which the caret should
   jump after a transformation. *)
let posFromCaretTarget (s : fluidState) (ast : fluidExpr) (ct : caretTarget) :
    int =
  let infos = toTokens s ast in
  (* Essentially we're using List.findMap to map a function that
   * matches across astref,token combinations (exhaustively matching astref but not token)
   * to determine the corresponding caretPos.
   *
   * This is purposefully verbose, as we want to ensure we have an exhaustive
   * match. Please do not use '_' in any of the astRef match conditions or
   * refactor in any way that removes the exhaustive matching of the astRefs.
   *
   * NB: These were somewhat hastily added and are very possibly incorrect.
   * Please fix.
   *)
  
  (* posForTi is the most common way of going from a single token info to a caretPos.
    It only really makes sense for situations where there is a 1:1 correspondence
    between an ASTRef and a token. It does not make sense for ASTRefs that scan across
    multiple tokens (eg Multiline Strings) *)
  let posForTi ti : int option =
    Some (ti.startPos + min ct.offset ti.length)
  in
  let clampedPosForTi ti pos : int option =
    Some (ti.startPos + max 0 (min pos ti.length))
  in
  (* targetAndTokenInfoToMaybeCaretPos takes a caretTarget and tokenInfo and produces
     the corresponding token-stream-global caretPos within the token stream,
     or None if the passed token isn't one we care about. The function will be used below
     as part of a List.findMap
   *)
  let targetAndTokenInfoToMaybeCaretPos ((ct, ti) : caretTarget * tokenInfo) :
      int option =
    match (ct.astRef, ti.token) with
    (* Binop *)
    | ARBinOp (id, BOPOperator), TBinOp (id', _) when id = id' ->
        posForTi ti
    | ARBinOp (_, BOPOperator), _ ->
        None
    (* Blank *)
    | ARBlank id, TBlank id' when id = id' ->
        posForTi ti
    | ARBlank _, _ ->
        None
    (* Bool *)
    | ARBool id, (TTrue id' | TFalse id') when id = id' ->
        posForTi ti
    | ARBool _, _ ->
        None
    (* Constructor *)
    | ARConstructor (id, CPName), TConstructorName (id', _) when id = id' ->
        posForTi ti
    | ARConstructor (_, CPName), _ ->
        None
    (* Field Access *)
    | ARFieldAccess (id, FAPFieldname), TFieldName (id', _, _) when id = id' ->
        posForTi ti
    | ARFieldAccess (_, FAPFieldname), _ ->
        None
    (* Float *)
    | ARFloat (id, FPWhole), TFloatWhole (id', _)
    | ARFloat (id, FPDecimal), TFloatFraction (id', _)
      when id = id' ->
        posForTi ti
    | ARFloat (_, FPWhole), _ | ARFloat (_, FPDecimal), _ ->
        None
    (* FnCall *)
    | ARFnCall (id, FCPFnName), TFnName (id', _, _, _, _) when id = id' ->
        posForTi ti
    | ARFnCall (_, FCPFnName), _ ->
        None
    (* If *)
    | ARIf (id, IPIfKeyword), TIfKeyword id'
    | ARIf (id, IPThenKeyword), TIfThenKeyword id'
    | ARIf (id, IPElseKeyword), TIfElseKeyword id'
      when id = id' ->
        posForTi ti
    | ARIf (_, IPIfKeyword), _
    | ARIf (_, IPThenKeyword), _
    | ARIf (_, IPElseKeyword), _ ->
        None
    (* Integer *)
    | ARInteger id, TInteger (id', _) when id = id' ->
        posForTi ti
    | ARInteger _, _ ->
        None
    (* Let *)
    | ARLet (id, LPKeyword), TLetKeyword (id', _)
    | ARLet (id, LPVarName), TLetLHS (id', _, _)
    | ARLet (id, LPAssignment), TLetAssignment (id', _)
      when id = id' ->
        posForTi ti
    | ARLet (_, LPKeyword), _
    | ARLet (_, LPVarName), _
    | ARLet (_, LPAssignment), _ ->
        None
    (* List *)
    | ARList (id, LPOpen), TListOpen id'
    | ARList (id, LPClose), TListClose id'
      when id = id' ->
        posForTi ti
    | ARList (id, LPSeparator idx), TListSep (id', idx')
      when id = id' && idx = idx' ->
        posForTi ti
    | ARList (_, LPOpen), _
    | ARList (_, LPClose), _
    | ARList (_, LPSeparator _), _ ->
        None
    (* Match *)
    | ARMatch (id, MPKeyword), TMatchKeyword id' when id = id' ->
        posForTi ti
    | ARMatch (id, MPBranchSep idx), TMatchSep (id', idx')
      when id = id' && idx = idx' ->
        posForTi ti
    | ( ARMatch (id, MPBranchPattern idx)
      , 
        (* Seems a bit fishy? *)
        ( TPatternVariable (id', _, _, idx')
        | TPatternConstructorName (id', _, _, idx')
        | TPatternInteger (id', _, _, idx')
        | TPatternString (id', _, _, idx')
        | TPatternTrue (id', _, idx')
        | TPatternFalse (id', _, idx')
        | TPatternNullToken (id', _, idx')
        | TPatternFloatWhole (id', _, _, idx')
        | TPatternFloatPoint (id', _, idx')
        | TPatternFloatFraction (id', _, _, idx')
        | TPatternBlank (id', _, idx') ) )
      when id = id' && idx = idx' ->
        posForTi ti
    | ARMatch (_, MPKeyword), _
    | ARMatch (_, MPBranchSep _), _
    | ARMatch (_, MPBranchPattern _), _ ->
        None
    (* Null *)
    | ARNull id, TNullToken id' when id = id' ->
        posForTi ti
    | ARNull _, _ ->
        None
    (* Partial *)
    | ARPartial id, TPartial (id', _) when id = id' ->
        posForTi ti
    | ARPartial _, _ ->
        None
    (* Right Partial *)
    | ARRightPartial id, TRightPartial (id', _) when id = id' ->
        posForTi ti
    | ARRightPartial _, _ ->
        None
    (* Pipe *)
    | ARPipe (id, PPPipeKeyword idx), TPipe (id', idx', _)
      when id = id' && idx = idx' ->
        posForTi ti
    | ARPipe (_, PPPipeKeyword _), _ ->
        None
    (* Record *)
    | ARRecord (id, RPOpen), TRecordOpen id'
    | ARRecord (id, RPClose), TRecordClose id'
      when id = id' ->
        posForTi ti
    | ( ARRecord (id, RPFieldname idx)
      , TRecordFieldname {recordID = id'; index = idx'} )
    | ARRecord (id, RPFieldSep idx), TRecordSep (id', idx', _)
      when id = id' && idx = idx' ->
        posForTi ti
    | ARRecord (_, RPOpen), _
    | ARRecord (_, RPClose), _
    | ARRecord (_, RPFieldname _), _
    | ARRecord (_, RPFieldSep _), _ ->
        None
    (* 
    * Strings
    *)
    | ARString (id, SPOpenQuote), TString (id', _) when id = id' ->
        posForTi ti
    | ARString (id, SPOpenQuote), tok ->
      ( match tok with
      | TStringMLStart (id', str, _, _) when id = id' ->
          let len = String.length str + 1 (* to account for open quote *) in
          if ct.offset > len
          then (* Must be in a later token *)
            None
          else (* Within current token *)
            posForTi ti
      | TStringMLMiddle (id', str, startOffsetIntoString, _) when id = id' ->
          let len = String.length str in
          let offsetInStr =
            ct.offset + 1
            (* to account for open quote in the start *)
          in
          let endOffset = startOffsetIntoString + len in
          if offsetInStr > endOffset
          then (* Must be in later token *)
            None
          else
            (* Within current token *)
            clampedPosForTi ti (offsetInStr - startOffsetIntoString)
      | TStringMLEnd (id', _, startOffsetIntoString, _) when id = id' ->
          (* Must be in this token because it's the last token in the string *)
          let offsetInStr =
            ct.offset + 1
            (* to account for open quote in the start *)
          in
          clampedPosForTi ti (offsetInStr - startOffsetIntoString)
      | _ ->
          None )
    | ARString (id, SPText), tok ->
      ( match tok with
      | TString (id', _) when id = id' ->
          clampedPosForTi ti (ct.offset + 1)
      | TStringMLStart (id', str, _, _) when id = id' ->
          let len = String.length str in
          if ct.offset > len
          then (* Must be in a later token *)
            None
          else (* Within current token *)
            clampedPosForTi ti (ct.offset + 1)
      | TStringMLMiddle (id', str, startOffsetIntoString, _) when id = id' ->
          let len = String.length str in
          let offsetInStr = ct.offset in
          let endOffset = startOffsetIntoString + len in
          if offsetInStr > endOffset
          then (* Must be in later token *)
            None
          else
            (* Within current token *)
            clampedPosForTi ti (offsetInStr - startOffsetIntoString)
      | TStringMLEnd (id', _, startOffsetIntoString, _) when id = id' ->
          (* Must be in this token because it's the last token in the string *)
          let offsetInStr = ct.offset in
          clampedPosForTi ti (offsetInStr - startOffsetIntoString)
      | _ ->
          None )
    | ARString (id, SPCloseQuote), tok ->
      ( match tok with
      | TString (id', str) when id = id' ->
          clampedPosForTi ti (ct.offset + String.length str + 1)
      | (TStringMLStart (id', _, _, _) | TStringMLMiddle (id', _, _, _))
        when id = id' ->
          None
      | TStringMLEnd (id', str, _, _) when id = id' ->
          clampedPosForTi ti (ct.offset + String.length str)
      | _ ->
          None )
    (* Variable *)
    | ARVariable id, TVariable (id', _) when id = id' ->
        posForTi ti
    | ARVariable _, _ ->
        None
    (* Invalid *)
    | ARInvalid, _ ->
        None
  in
  match
    infos
    |> List.findMap ~f:(fun ti -> targetAndTokenInfoToMaybeCaretPos (ct, ti))
  with
  | Some newPos ->
      newPos
  | None ->
      recover
        "We expected to find the given caretTarget in the token stream but couldn't."
        ~debug:ct
        s.newPos


(** moveToCaretTarget returns a modified fluidState with newPos set to reflect
    the caretTarget. *)
let moveToCaretTarget (s : fluidState) (ast : fluidExpr) (ct : caretTarget) =
  {s with newPos = posFromCaretTarget s ast ct}


(** moveToAstRef returns a modified fluidState with newPos set to reflect
    the targeted astRef.

    If given, offset is the offset of the caretTarget, in characters. Defaults
    to 0, or the beginning of the targeted expression. *)
let moveToAstRef
    (s : fluidState) (ast : fluidExpr) ?(offset = 0) (astRef : astRef) :
    fluidState =
  moveToCaretTarget s ast {astRef; offset}


(* caretTargetForLastPartOfExpr takes an ast and produces a caretTarget
 * corresponding to the "very end" of that ast. The concept of "very end"
 * is related to an understanding of the tokenization of the ast, even though
 * this function doesn't explicitly depend on any tokenization functions. *)
let caretTargetForLastPartOfExpr (astPartId : id) (ast : ast) : caretTarget =
  let rec caretTargetForLastPartOfExpr' : fluidExpr -> caretTarget = function
    | EVariable (id, str) ->
        {astRef = ARVariable id; offset = String.length str}
    | EFieldAccess (id, _, _, fieldName) ->
        { astRef = ARFieldAccess (id, FAPFieldname)
        ; offset = String.length fieldName }
    | EInteger (id, valueStr) ->
        {astRef = ARInteger id; offset = String.length valueStr}
    | EBool (id, true) ->
        {astRef = ARBool id; offset = String.length "true"}
    | EBool (id, false) ->
        {astRef = ARBool id; offset = String.length "false"}
    | EString (id, _) ->
        {astRef = ARString (id, SPCloseQuote); offset = 1 (* end of quote *)}
    | EFloat (id, _, decimalStr) ->
        {astRef = ARFloat (id, FPDecimal); offset = String.length decimalStr}
    | ENull id ->
        {astRef = ARNull id; offset = String.length "null"}
    | EBlank id ->
        { astRef = ARBlank id
        ; offset =
            3
            (* It might be better to return offset 0 here,
              because we might not want to be in the actual end of the blank? *)
        }
    | ELet (_, _, _, _, bodyExpr) ->
        caretTargetForLastPartOfExpr' bodyExpr
    | EIf (_, _, _, elseExpr) ->
        caretTargetForLastPartOfExpr' elseExpr
    | EBinOp (_, _, _, rhsExpr, _) ->
        caretTargetForLastPartOfExpr' rhsExpr
    | ELambda (_, _, bodyExpr) ->
        caretTargetForLastPartOfExpr' bodyExpr
    | EFnCall (id, fnName, argExprs, _) ->
      ( match List.last argExprs with
      | Some lastExpr ->
          caretTargetForLastPartOfExpr' lastExpr
      | None ->
          { astRef = ARFnCall (id, FCPFnName)
          ; offset = fnName |> ViewUtils.partialName |> String.length } )
    | EPartial (id, str, _) ->
        (* Intentionally using the thing that was typed; not the existing expr *)
        {astRef = ARPartial id; offset = String.length str}
    | ERightPartial (id, str, _) ->
        (* XXX(JULIAN): Unclear if we should be differentiating with EPartial... *)
        {astRef = ARPartial id; offset = String.length str}
    | EList (id, _) ->
        {astRef = ARList (id, LPClose); offset = 1 (* End of the close ] *)}
    | ERecord (id, _) ->
        {astRef = ARRecord (id, RPClose); offset = 1 (* End of the close } *)}
    | EPipe (id, pipeExprs) ->
      ( match List.last pipeExprs with
      | Some lastExpr ->
          caretTargetForLastPartOfExpr' lastExpr
      | None ->
          {astRef = ARPipe (id, PPPipeKeyword 0); offset = String.length "|>"}
      )
    | EMatch (_, matchedExpr, matchItems) ->
      ( match List.last matchItems with
      | Some (_, branchBody) ->
          caretTargetForLastPartOfExpr' branchBody
      | None ->
          caretTargetForLastPartOfExpr' matchedExpr )
    | EConstructor (id, _, name, containedExprs) ->
      ( match List.last containedExprs with
      | Some lastExpr ->
          caretTargetForLastPartOfExpr' lastExpr
      | None ->
          {astRef = ARConstructor (id, CPName); offset = String.length name} )
    | (EFeatureFlag (_, _, _, _, _, _) | EPipeTarget _ | EOldExpr _) as expr ->
        recover
          "we don't yet support caretTargetForLastPartOfExpr for this"
          ~debug:expr
          {astRef = ARInvalid; offset = 0}
  in
  match findExpr astPartId ast with
  | Some expr ->
      caretTargetForLastPartOfExpr' expr
  | None ->
      recover
        "caretTargetForLastPartOfExpr got an id outside of the AST"
        ~debug:astPartId
        {astRef = ARInvalid; offset = 0}


(* caretTargetForBeginningOfExpr returns a caretTarget representing the
 * beginning of the expression in `ast` having the given `id`. *)
let caretTargetForBeginningOfExpr (id : id) (ast : ast) : caretTarget =
  let expr = findExpr id ast in
  match expr with
  | Some (EInteger (id, _)) ->
      {astRef = ARInteger id; offset = 0}
  | Some (EBool (id, _)) ->
      {astRef = ARBool id; offset = 0}
  | Some (EString (id, _)) ->
      {astRef = ARString (id, SPOpenQuote); offset = 0}
  | Some (EFloat (id, _, _)) ->
      {astRef = ARFloat (id, FPWhole); offset = 0}
  | Some (ENull id) ->
      {astRef = ARNull id; offset = 0}
  | Some (EBlank id) ->
      {astRef = ARBlank id; offset = 0}
  | Some (ELet (id, _, _, _, _)) ->
      {astRef = ARLet (id, LPKeyword); offset = 0}
  | Some (EIf (id, _, _, _)) ->
      {astRef = ARIf (id, IPIfKeyword); offset = 0}
  | Some (EMatch (id, _, _)) ->
      {astRef = ARMatch (id, MPKeyword); offset = 0}
  | Some (EBinOp _)
  | Some (EFnCall _)
  | Some (ELambda _)
  | Some (EFieldAccess _)
  | Some (EVariable _)
  | Some (EPartial _)
  | Some (ERightPartial _)
  | Some (EList _)
  | Some (ERecord _)
  | Some (EPipe _)
  | Some (EConstructor _)
  | Some (EPipeTarget _)
  | Some (EFeatureFlag _)
  | Some (EOldExpr _) ->
      recover
        "unhandled expr in caretTargetForBeginningOfExpr"
        ~debug:(id, expr)
        {astRef = ARInvalid; offset = 0}
  | None ->
      recover
        "expr not found in caretTargetForBeginningOfExpr"
        ~debug:(id, expr)
        {astRef = ARInvalid; offset = 0}


(* ---------------- *)
(* Patterns *)
(* ---------------- *)

let recursePattern ~(f : fluidPattern -> fluidPattern) (pat : fluidPattern) :
    fluidPattern =
  match pat with
  | FPInteger _
  | FPBlank _
  | FPString _
  | FPVariable _
  | FPBool _
  | FPNull _
  | FPFloat _ ->
      pat
  | FPConstructor (id, nameID, name, pats) ->
      FPConstructor (id, nameID, name, List.map ~f pats)
  | FPOldPattern _ ->
      pat


let updatePattern
    ~(f : fluidPattern -> fluidPattern) (matchID : id) (patID : id) (ast : ast)
    : ast =
  updateExpr matchID ast ~f:(fun m ->
      match m with
      | EMatch (matchID, expr, pairs) ->
          let rec run p =
            if patID = pid p then f p else recursePattern ~f:run p
          in
          let newPairs =
            List.map pairs ~f:(fun (pat, expr) -> (run pat, expr))
          in
          EMatch (matchID, expr, newPairs)
      | _ ->
          m )


let replacePattern
    ~(newPat : fluidPattern) (matchID : id) (patID : id) (ast : ast) : ast =
  updatePattern matchID patID ast ~f:(fun _ -> newPat)


let replaceVarInPattern
    (mID : id) (oldVarName : string) (newVarName : string) (ast : ast) : ast =
  updateExpr mID ast ~f:(fun e ->
      match e with
      | EMatch (mID, cond, cases) ->
          let rec replaceNameInPattern pat =
            match pat with
            | FPVariable (_, id, varName) when varName = oldVarName ->
                if newVarName = ""
                then FPBlank (mID, id)
                else FPVariable (mID, id, newVarName)
            | FPConstructor (mID, id, name, patterns) ->
                FPConstructor
                  (mID, id, name, List.map patterns ~f:replaceNameInPattern)
            | pattern ->
                pattern
          in
          let newCases =
            List.map cases ~f:(fun (pat, expr) ->
                ( replaceNameInPattern pat
                , renameVariableUses oldVarName newVarName expr ) )
          in
          EMatch (mID, cond, newCases)
      | _ ->
          recover "not a match in replaceVarInPattern" ~debug:e e )


let removePatternRow (mID : id) (id : id) (ast : ast) : ast =
  updateExpr mID ast ~f:(fun e ->
      match e with
      | EMatch (_, cond, patterns) ->
          let newPatterns =
            if List.length patterns = 1
            then patterns (* Don't allow there be less than 1 pattern *)
            else List.filter patterns ~f:(fun (p, _) -> pid p <> id)
          in
          EMatch (mID, cond, newPatterns)
      | _ ->
          recover "not a match in removePatternRow" ~debug:e e )


let replacePatternWithPartial
    (str : string) (matchID : id) (patID : id) (ast : ast) : ast =
  updatePattern matchID patID ast ~f:(fun p ->
      let str = String.trim str in
      match p with
      | _ when str = "" ->
          FPBlank (matchID, gid ())
      | FPVariable (mID, pID, _) ->
          FPVariable (mID, pID, str)
      | _ ->
          FPVariable (matchID, gid (), str) )


(** addMatchPatternAt adds a new match row (FPBlank, EBlank) into the EMatch
    with `matchId` at `idx`.

    Returns a new ast and fluidState with the action recorded. *)
let addMatchPatternAt (matchId : id) (idx : int) (ast : ast) (s : fluidState) :
    ast * fluidState =
  let action =
    Printf.sprintf "addMatchPatternAt(id=%s idx=%d)" (deID matchId) idx
  in
  let s = recordAction action s in
  let ast =
    updateExpr matchId ast ~f:(function
        | EMatch (_, cond, rows) ->
            let newVal = (FPBlank (matchId, gid ()), newB ()) in
            let newRows = List.insertAt rows ~index:idx ~value:newVal in
            EMatch (matchId, cond, newRows)
        | e ->
            recover "expected to find EMatch to update" ~debug:e e )
  in
  (ast, s)


(* ---------------- *)
(* Blanks *)
(* ---------------- *)

let removeEmptyExpr (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ELet (_, _, "", EBlank _, body) ->
          body
      | EIf (_, EBlank _, EBlank _, EBlank _) ->
          newB ()
      | ELambda (_, _, EBlank _) ->
          newB ()
      | EMatch (_, EBlank _, pairs)
        when List.all pairs ~f:(fun (p, e) ->
                 match (p, e) with FPBlank _, EBlank _ -> true | _ -> false )
        ->
          newB ()
      | _ ->
          e )


(* ---------------- *)
(* Fields *)
(* ---------------- *)
let replaceFieldName (str : string) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EPartial (id, _, (EFieldAccess _ as fa)) ->
          EPartial (id, str, fa)
      | EFieldAccess _ ->
          EPartial (gid (), str, e)
      | _ ->
          recover "not a field in replaceFieldName" ~debug:e e )


let exprToFieldAccess (id : id) (fieldID : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      EPartial (gid (), "", EFieldAccess (fieldID, e, gid (), "")) )


let removeField (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFieldAccess (_, faExpr, _, _) ->
          faExpr
      | EPartial (_, _, EFieldAccess (_, faExpr, _, _)) ->
          faExpr
      | _ ->
          recover "not a fieldAccess in removeField" ~debug:e e )


(* ---------------- *)
(* Lambdas *)
(* ---------------- *)
let replaceLamdaVar
    ~(index : int)
    (oldVarName : string)
    (newVarName : string)
    (id : id)
    (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ELambda (id, vars, expr) ->
          let vars =
            List.updateAt vars ~index ~f:(fun (id, _) -> (id, newVarName))
          in
          ELambda (id, vars, renameVariableUses oldVarName newVarName expr)
      | _ ->
          recover "not a lamda in replaceLamdaVar" ~debug:e e )


let removeLambdaSepToken (id : id) (ast : ast) (index : int) : fluidExpr =
  let index =
    (* remove expression in front of sep, not behind it *)
    index + 1
  in
  updateExpr id ast ~f:(fun e ->
      match e with
      | ELambda (id, vars, expr) ->
          let var =
            List.getAt ~index vars
            |> Option.map ~f:Tuple2.second
            |> Option.withDefault ~default:""
          in
          ELambda (id, List.removeAt ~index vars, removeVariableUse var expr)
      | _ ->
          e )


let insertLambdaVar ~(index : int) ~(name : string) (id : id) (ast : ast) : ast
    =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ELambda (id, vars, expr) ->
          let value = (gid (), name) in
          ELambda (id, List.insertAt ~index ~value vars, expr)
      | _ ->
          recover "not a list in insertLambdaVar" ~debug:e e )


(* ---------------- *)
(* Lets *)
(* ---------------- *)

let replaceLetLHS (newLHS : string) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ELet (id, lhsID, oldLHS, rhs, next) ->
          ELet (id, lhsID, newLHS, rhs, renameVariableUses oldLHS newLHS next)
      | _ ->
          recover "not a let in replaceLetLHS" ~debug:e e )


(** makeIntoLetBody takes the `id` of an expression, which will be made into the
    body of a new ELet.

    Returns a new ast, fluidState, and the id of the newly inserted ELet, which
    may be useful for doing caret placement. *)
let makeIntoLetBody (id : id) (ast : ast) (s : fluidState) :
    ast * fluidState * id =
  let s = recordAction (Printf.sprintf "makeIntoLetBody(%s)" (deID id)) s in
  let lid = gid () in
  let ast =
    updateExpr id ast ~f:(fun expr -> ELet (lid, gid (), "", newB (), expr))
  in
  (ast, s, lid)


(* ---------------- *)
(* Records *)
(* ---------------- *)
let replaceRecordField ~index (str : string) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          let fields =
            List.updateAt fields ~index ~f:(fun (id, _, expr) -> (id, str, expr)
            )
          in
          ERecord (id, fields)
      | _ ->
          recover "not a record in replaceRecordField" ~debug:e e )


let removeRecordField (id : id) (index : int) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, List.removeAt ~index fields)
      | _ ->
          recover "not a record field in removeRecordField" ~debug:e e )


(* Add a row to the record *)
let addRecordRowAt ?(letter = "") (index : int) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord
            (id, List.insertAt ~index ~value:(gid (), letter, newB ()) fields)
      | _ ->
          recover "Not a record in addRecordRowAt" ~debug:e e )


let addRecordRowToBack (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, fields @ [(gid (), "", newB ())])
      | _ ->
          recover "Not a record in addRecordRowToTheBack" ~debug:e e )


(* recordFieldAtIndex gets the field for the record in the ast with recordID at index,
   or None if the record has no field with that index *)
let recordFieldAtIndex (recordID : id) (index : int) (ast : ast) :
    (id * fluidName * fluidExpr) option =
  findExpr recordID ast
  |> Option.andThen ~f:(fun expr ->
         match expr with ERecord (_, fields) -> Some fields | _ -> None )
  |> Option.andThen ~f:(fun fields -> List.getAt ~index fields)


(* recordExprIdAtIndex gets the id of the field value for the record in the ast
   with recordID at index, or None if the record has no field with that index  *)
let recordExprIdAtIndex (recordID : id) (index : int) (ast : ast) : id option =
  match recordFieldAtIndex recordID index ast with
  | Some (_, _, fluidExpr) ->
      Some (eid fluidExpr)
  | _ ->
      None


(* ---------------- *)
(* Partials *)
(* ---------------- *)

let replaceWithPartial (str : string) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      let str = String.trim str in
      if String.startsWith ~prefix:"\"" str && String.endsWith ~suffix:"\"" str
      then EString (gid (), String.slice ~from:1 ~to_:(-1) str)
      else
        match e with
        | EPartial (_, _, (EFieldAccess _ as oldVal)) ->
            (* This is allowed to be the empty string. *)
            EPartial (id, str, oldVal)
        | EPartial (id, _, oldVal) ->
            asserT
              ~debug:str
              "empty partial, use deletePartial instead"
              (str <> "") ;
            EPartial (id, str, oldVal)
        | oldVal ->
            if str = "" then newB () else EPartial (gid (), str, oldVal) )


let deletePartial (ti : tokenInfo) (ast : ast) (s : state) : ast * state =
  let newState = ref (fun (_ : ast) -> s) in
  let ast =
    updateExpr (FluidToken.tid ti.token) ast ~f:(fun e ->
        match e with
        | EPartial
            ( _
            , _
            , EBinOp (_, _, EString (lhsID, lhsStr), EString (_, rhsStr), _) )
          ->
            (newState := fun _ -> moveTo (ti.startPos - 2) s) ;
            EString (lhsID, lhsStr ^ rhsStr)
        | EPartial (_, _, EBinOp (_, _, lhs, _, _)) ->
            (newState := fun ast -> moveToEndOfTarget (eid lhs) ast s) ;
            lhs
        | EPartial (_, _, _) ->
            let b = newB () in
            (newState := fun ast -> moveToEndOfTarget (eid b) ast s) ;
            b
        | _ ->
            recover "not a partial in deletePartial" ~debug:e e )
  in
  (ast, !newState ast)


let replacePartialWithArguments
    ~(newExpr : fluidExpr) (id : id) (s : state) (ast : ast) : ast =
  let fns =
    assertFn
      ~f:(( <> ) [])
      "empty functions passed to replacePartialWithArguments"
      s.ac.functions
  in
  let getFunctionParams fnname count varExprs =
    List.map (List.range 0 count) ~f:(fun index ->
        fns
        |> List.find ~f:(fun f -> f.fnName = fnname)
        |> Option.andThen ~f:(fun fn -> List.getAt ~index fn.fnParameters)
        |> Option.map ~f:(fun p ->
               ( p.paramName
               , Runtime.tipe2str p.paramTipe
               , List.getAt ~index varExprs
                 |> Option.withDefault ~default:(EBlank (gid ()))
               , index ) ) )
  in
  let rec wrapWithLets ~expr vars =
    match vars with
    (* don't wrap parameter who's value is a blank i.e. not set *)
    | [] | (_, _, EBlank _, _) :: _ ->
        expr
    (* don't wrap parameter that's set to a variable *)
    | (_, _, EVariable _, _) :: _ ->
        expr
    | (name, _, rhs, _) :: rest ->
        ELet (gid (), gid (), name, rhs, wrapWithLets ~expr rest)
  in
  let getExprs expr =
    match expr with
    | EFnCall (_, _, exprs, _) | EConstructor (_, _, _, exprs) ->
        exprs
    | EBinOp (_, _, lhs, rhs, _) ->
        [lhs; rhs]
    | _ ->
        recover "impossible" ~debug:expr []
  in
  let isAligned p1 p2 =
    match (p1, p2) with
    | Some (name, tipe, _, _), Some (name', tipe', _, _) ->
        name = name' && tipe = tipe'
    | _, _ ->
        false
  in
  updateExpr id ast ~f:(fun expr ->
      match expr with
      (* preserve partials with arguments *)
      | EPartial (_, _, (EFnCall (_, name, _, _) as inner))
      | EPartial (_, _, (EBinOp (_, name, _, _, _) as inner))
      | EPartial (_, _, (EConstructor (_, _, name, _) as inner)) ->
          let existingExprs = getExprs inner in
          let fetchParams newName placeholderExprs =
            let count =
              max (List.length existingExprs) (List.length placeholderExprs)
            in
            let newParams = getFunctionParams newName count placeholderExprs in
            let oldParams = getFunctionParams name count existingExprs in
            let matchedParams, mismatchedParams =
              List.partition oldParams ~f:(fun p ->
                  List.any newParams ~f:(isAligned p) )
              |> Tuple2.mapAll ~f:Option.values
            in
            let newParams =
              List.foldl
                matchedParams
                ~init:placeholderExprs
                ~f:(fun (_, _, expr, index) exprs ->
                  List.updateAt ~index ~f:(fun _ -> expr) exprs )
            in
            (newParams, mismatchedParams)
          in
          ( match newExpr with
          | EBinOp (id, newName, lhs, rhs, ster) ->
              let newParams, mismatchedParams =
                fetchParams newName [lhs; rhs]
              in
              let newExpr =
                match newParams with
                | [newLHS; newRHS] ->
                    EBinOp (id, newName, newLHS, newRHS, ster)
                | _ ->
                    recover
                      "wrong number of arguments"
                      ~debug:newParams
                      (EBinOp (id, newName, newB (), newB (), ster))
              in
              wrapWithLets ~expr:newExpr mismatchedParams
          | EFnCall (id, newName, newExprs, ster) ->
              let newParams, mismatchedParams = fetchParams newName newExprs in
              let newExpr = EFnCall (id, newName, newParams, ster) in
              wrapWithLets ~expr:newExpr mismatchedParams
          | EConstructor _ ->
              let oldParams =
                existingExprs
                |> List.indexedMap ~f:(fun i p ->
                       (* create ugly automatic variable name *)
                       let name = "var_" ^ string_of_int (Util.random ()) in
                       (name, Runtime.tipe2str TAny, p, i) )
              in
              wrapWithLets ~expr:newExpr oldParams
          | _ ->
              newExpr )
      | _ ->
          newExpr )


(* ---------------- *)
(* Binops (plus right partials) *)
(* ---------------- *)

let convertToBinOp (char : char option) (id : id) (ast : ast) : ast =
  match char with
  | None ->
      ast
  | Some c ->
      updateExpr id ast ~f:(fun expr ->
          ERightPartial (gid (), String.fromChar c, expr) )


let deleteRightPartial (ti : tokenInfo) (ast : ast) : ast * id =
  let id = ref FluidToken.fakeid in
  let ast =
    updateExpr (FluidToken.tid ti.token) ast ~f:(fun e ->
        match e with
        | ERightPartial (_, _, oldVal) ->
            id := eid oldVal ;
            oldVal
        | oldVal ->
            id := eid oldVal ;
            (* This uses oldval, unlike replaceWithPartial, because when a
           * partial goes to blank you're deleting it, while when a
           * rightPartial goes to blank you've only deleted the rhs *)
            oldVal )
  in
  (ast, !id)


let replaceWithRightPartial (str : string) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      let str = String.trim str in
      if str = ""
      then recover "replacing with empty right partial" ~debug:e e
      else
        match e with
        | ERightPartial (id, _, oldVal) ->
            ERightPartial (id, str, oldVal)
        | oldVal ->
            ERightPartial (gid (), str, oldVal) )


let deleteBinOp (ti : tokenInfo) (ast : ast) : ast * id =
  let id = ref FluidToken.fakeid in
  let ast =
    updateExpr (FluidToken.tid ti.token) ast ~f:(fun e ->
        match e with
        | EBinOp (_, _, EPipeTarget _, rhs, _) ->
            id := eid rhs ;
            rhs
        | EBinOp (_, _, lhs, _, _) ->
            id := eid lhs ;
            lhs
        | _ ->
            recover "not a binop in deleteBinOp" ~debug:e e )
  in
  (ast, !id)


(* ---------------- *)
(* Pipes *)
(* ---------------- *)
let removePipe (id : id) (ast : ast) (index : int) : ast =
  let index =
    (* remove expression in front of pipe, not behind it *)
    index + 1
  in
  updateExpr id ast ~f:(fun e ->
      match e with
      | EPipe (_, [e1; _]) ->
          e1
      | EPipe (id, exprs) ->
          EPipe (id, List.removeAt ~index exprs)
      | _ ->
          e )


(** addPipeExprAt adds a new EBlank into the EPipe with `pipeId` at `idx`.

    Returns a new ast, fluidState with the action recorded, and the id of the
    newly inserted EBlank, which may be useful for doing caret placement. *)
let addPipeExprAt (pipeId : id) (idx : int) (ast : ast) (s : fluidState) :
    ast * fluidState * id =
  let action =
    Printf.sprintf "addPipeExprAt(id=%s idx=%d)" (deID pipeId) idx
  in
  let s = recordAction action s in
  let bid = gid () in
  let ast =
    updateExpr pipeId ast ~f:(function
        | EPipe (_, exprs) ->
            let exprs = List.insertAt exprs ~index:idx ~value:(EBlank bid) in
            EPipe (pipeId, exprs)
        | e ->
            recover "expected to find EPipe to update" ~debug:e e )
  in
  (ast, s, bid)


(* Supports the various different tokens replacing their string contents.
 * Doesn't do movement. *)
let replaceStringToken ~(f : string -> string) (token : token) (ast : ast) :
    fluidExpr =
  match token with
  | TStringMLStart (id, _, _, str)
  | TStringMLMiddle (id, _, _, str)
  | TStringMLEnd (id, _, _, str)
  | TString (id, str) ->
      replaceExpr id ~newExpr:(EString (id, f str)) ast
  | TPatternString (mID, id, str, _) ->
      replacePattern mID id ~newPat:(FPString (mID, id, f str)) ast
  | TInteger (id, str) ->
      let str = f str in
      let newExpr =
        if str = ""
        then EBlank id
        else EInteger (id, coerceStringTo63BitInt str)
      in
      replaceExpr id ~newExpr ast
  | TPatternInteger (mID, id, str, _) ->
      let str = f str in
      let newPat =
        if str = ""
        then FPBlank (mID, id)
        else FPInteger (mID, id, coerceStringTo63BitInt str)
      in
      replacePattern mID id ~newPat ast
  | TPatternNullToken (mID, id, _) ->
      let str = f "null" in
      let newExpr = FPVariable (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternTrue (mID, id, _) ->
      let str = f "true" in
      let newExpr = FPVariable (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternFalse (mID, id, _) ->
      let str = f "false" in
      let newExpr = FPVariable (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternVariable (mID, _, str, _) ->
      replaceVarInPattern mID str (f str) ast
  | TRecordFieldname {recordID; index; fieldName} ->
      replaceRecordField ~index (f fieldName) recordID ast
  | TLetLHS (id, _, str) ->
      replaceLetLHS (f str) id ast
  | TLambdaVar (id, _, index, str) ->
      replaceLamdaVar ~index str (f str) id ast
  | TVariable (id, str) ->
      replaceWithPartial (f str) id ast
  | TPartial (id, str) ->
      replaceWithPartial (f str) id ast
  | TRightPartial (id, str) ->
      replaceWithRightPartial (f str) id ast
  | TFieldName (id, _, str) ->
      replaceFieldName (f str) id ast
  | TFieldPartial (id, _, _, str) ->
      (* replace the partial's name, not the field's *)
      replaceWithPartial (f str) id ast
  | TTrue id ->
      replaceWithPartial (f "true") id ast
  | TFalse id ->
      replaceWithPartial (f "false") id ast
  | TNullToken id ->
      replaceWithPartial (f "null") id ast
  | TBinOp (id, name) ->
      replaceWithPartial (f name) id ast
  | _ ->
      recover "not supported by replaceToken" ~debug:token ast


(* ---------------- *)
(* Floats  *)
(* ---------------- *)
let replaceFloatWhole (str : string) (id : id) (ast : ast) : fluidExpr =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (id, _, fraction) ->
          EFloat (id, str, fraction)
      | _ ->
          recover "not a float im replaceFloatWhole" ~debug:e e )


let replacePatternFloatWhole
    (str : string) (matchID : id) (patID : id) (ast : ast) : fluidExpr =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, patID, _, fraction) ->
          FPFloat (matchID, patID, str, fraction)
      | _ ->
          recover "not a float in replacePatternFloatWhole" ~debug:e e )


let replacePatternFloatFraction
    (str : string) (matchID : id) (patID : id) (ast : ast) : fluidExpr =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, patID, whole, _) ->
          FPFloat (matchID, patID, whole, str)
      | _ ->
          recover "not a float in replacePatternFloatFraction" ~debug:e e )


let removePatternPointFromFloat (matchID : id) (patID : id) (ast : ast) : ast =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, _, whole, fraction) ->
          let i = coerceStringTo63BitInt (whole ^ fraction) in
          FPInteger (matchID, gid (), i)
      | _ ->
          recover "Not an int in removePatternPointFromFloat" ~debug:e e )


let replaceFloatFraction (str : string) (id : id) (ast : ast) : fluidExpr =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (id, whole, _) ->
          EFloat (id, whole, str)
      | _ ->
          recover "not a floatin replaceFloatFraction" ~debug:e e )


let insertAtFrontOfFloatFraction (letter : string) (id : id) (ast : ast) :
    fluidExpr =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (id, whole, fraction) ->
          EFloat (id, whole, letter ^ fraction)
      | _ ->
          recover "not a float in insertAtFrontOfFloatFraction" ~debug:e e )


let insertAtFrontOfPatternFloatFraction
    (letter : string) (matchID : id) (patID : id) (ast : ast) : fluidExpr =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, patID, whole, fraction) ->
          FPFloat (matchID, patID, whole, letter ^ fraction)
      | _ ->
          recover
            "not a float in insertAtFrontOfPatternFloatFraction"
            ~debug:e
            e )


let convertIntToFloat (offset : int) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EInteger (_, i) ->
          let whole, fraction = String.splitAt ~index:offset i in
          EFloat (gid (), whole, fraction)
      | _ ->
          recover "Not an int in convertIntToFloat" ~debug:e e )


let convertPatternIntToFloat
    (offset : int) (matchID : id) (patID : id) (ast : ast) : ast =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPInteger (matchID, _, i) ->
          let whole, fraction = String.splitAt ~index:offset i in
          FPFloat (matchID, gid (), whole, fraction)
      | _ ->
          recover "Not an int in convertPatternIntToFloat" ~debug:e e )


let removePointFromFloat (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (_, whole, fraction) ->
          let i = coerceStringTo63BitInt (whole ^ fraction) in
          EInteger (gid (), i)
      | _ ->
          recover "Not an int in removePointFromFloat" ~debug:e e )


(* ---------------- *)
(* Lists *)
(* ---------------- *)
let removeListSepToken (id : id) (ast : ast) (index : int) : fluidExpr =
  let index =
    (* remove expression in front of sep, not behind it *)
    index + 1
  in
  updateExpr id ast ~f:(fun e ->
      match e with
      | EList (id, exprs) ->
          EList (id, List.removeAt ~index exprs)
      | _ ->
          e )


let insertInList ~(index : int) ~(newExpr : fluidExpr) (id : id) (ast : ast) :
    ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EList (id, exprs) ->
          EList (id, List.insertAt ~index ~value:newExpr exprs)
      | _ ->
          recover "not a list in insertInList" ~debug:e e )


(* Add a blank after the expr indicated by id, which we presume is in a list *)
let addBlankToList (id : id) (ast : ast) : ast =
  let parent = findParent id ast in
  match parent with
  | Some (EList (pID, exprs)) ->
    ( match List.findIndex ~f:(fun e -> eid e = id) exprs with
    | Some index ->
        insertInList ~index:(index + 1) ~newExpr:(EBlank (gid ())) pID ast
    | _ ->
        ast )
  | _ ->
      ast


(* -------------------- *)
(* Autocomplete *)
(* -------------------- *)

let acToExpr (entry : Types.fluidAutocompleteItem) : fluidExpr * int =
  match entry with
  | FACFunction fn ->
      let count = List.length fn.fnParameters in
      let partialName = ViewUtils.partialName fn.fnName in
      let r =
        if List.member ~value:fn.fnReturnTipe Runtime.errorRailTypes
        then Types.Rail
        else Types.NoRail
      in
      let args = List.initialize count (fun _ -> EBlank (gid ())) in
      if fn.fnInfix
      then
        match args with
        | [lhs; rhs] ->
            (EBinOp (gid (), fn.fnName, lhs, rhs, r), 0)
        | _ ->
            recover "BinOp doesn't have 2 args" ~debug:args (newB (), 0)
      else
        (* functions with arguments should place the caret into the first argument
         * while functions without should place it just after the function name
         * List::head |_list_ [vs] List::empty| *)
        let fnNameLen = String.length partialName in
        let offset = if List.isEmpty args then fnNameLen else fnNameLen + 1 in
        (EFnCall (gid (), fn.fnName, args, r), offset)
  | FACKeyword KLet ->
      (ELet (gid (), gid (), "", newB (), newB ()), 4)
  | FACKeyword KIf ->
      (EIf (gid (), newB (), newB (), newB ()), 3)
  | FACKeyword KLambda ->
      (ELambda (gid (), [(gid (), "")], newB ()), 1)
  | FACKeyword KMatch ->
      let matchID = gid () in
      (EMatch (matchID, newB (), [(FPBlank (matchID, gid ()), newB ())]), 6)
  | FACKeyword KPipe ->
      (EPipe (gid (), [newB (); newB ()]), 6)
  | FACVariable (name, _) ->
      (EVariable (gid (), name), String.length name)
  | FACLiteral "true" ->
      (EBool (gid (), true), 4)
  | FACLiteral "false" ->
      (EBool (gid (), false), 5)
  | FACLiteral "null" ->
      (ENull (gid ()), 4)
  | FACConstructorName (name, argCount) ->
      let args = List.initialize argCount (fun _ -> EBlank (gid ())) in
      let starting = if argCount = 0 then 0 else 1 in
      (EConstructor (gid (), gid (), name, args), starting + String.length name)
  | FACPattern _ ->
      recover "patterns are not supported here" ~debug:entry (newB (), 0)
  | FACField fieldname ->
      ( EFieldAccess (gid (), newB (), gid (), fieldname)
      , String.length fieldname )
  | FACLiteral _ ->
      recover "invalid literal in autocomplete" ~debug:entry (newB (), 0)


let rec extractSubexprFromPartial (expr : fluidExpr) : fluidExpr =
  match expr with
  | EPartial (_, _, subExpr) | ERightPartial (_, _, subExpr) ->
      extractSubexprFromPartial subExpr
  | _ ->
      expr


let acToPattern (entry : Types.fluidAutocompleteItem) : fluidPattern * int =
  match entry with
  | FACPattern p ->
    ( match p with
    | FPAConstructor (mID, patID, var, pats) ->
        (FPConstructor (mID, patID, var, pats), String.length var + 1)
    | FPAVariable (mID, patID, var) ->
        (FPVariable (mID, patID, var), String.length var + 1)
    | FPABool (mID, patID, var) ->
        (FPBool (mID, patID, var), String.length (string_of_bool var) + 1)
    | FPANull (mID, patID) ->
        (FPNull (mID, patID), 4) )
  | _ ->
      recover
        "got fluidAutocompleteItem of non `FACPattern` variant - this should never occur"
        ~debug:entry
        (FPBlank (gid (), gid ()), 0)


let initAC (s : state) (m : Types.model) : state = {s with ac = AC.init m}

let isAutocompleting (ti : tokenInfo) (s : state) : bool =
  Token.isAutocompletable ti.token
  && s.upDownCol = None
  && s.ac.index <> None
  && s.newPos <= ti.endPos
  && s.newPos >= ti.startPos


let acSetIndex (i : int) (s : state) : state =
  let s = recordAction "acSetIndex" s in
  {s with ac = {s.ac with index = Some i}; upDownCol = None}


let acClear (s : state) : state =
  let s = recordAction "acClear" s in
  {s with ac = {s.ac with index = None}}


let acMaybeShow (ti : tokenInfo) (s : state) : state =
  let s = recordAction "acShow" s in
  if Token.isAutocompletable ti.token && s.ac.index = None
  then {s with ac = {s.ac with index = Some 0}}
  else s


let acMoveUp (s : state) : state =
  let s = recordAction "acMoveUp" s in
  let index =
    match s.ac.index with None -> 0 | Some current -> max 0 (current - 1)
  in
  acSetIndex index s


let acMoveDown (s : state) : state =
  let s = recordAction "acMoveDown" s in
  let index =
    match s.ac.index with
    | None ->
        0
    | Some current ->
        min (current + 1) (List.length (AC.allCompletions s.ac) - 1)
  in
  acSetIndex index s


(* acMoveBasedOnKey produces a new state with the caret placed in a position that
   makes sense for the specific key that was pressed to confirm the autocomplete.
   
   It accepts:
   - the pressed key,
   - the caret position at which the autocompleted token begins,
   - an "offset" that corresponds to how many additional steps the caret should take to get
      from there to where the caret would end up with no special handling,
   - the state after the completion has been added to the ast
   - the ast after the completion has been added *)
let acMoveBasedOnKey
    (key : K.key)
    (posAtStartOfACedToken : int)
    (offset : int)
    (s : state)
    (ast : ast) : state =
  let tokens = toTokens s ast in
  let posAtEndOfACedToken = posAtStartOfACedToken + offset in
  let newPos =
    match key with
    | K.Tab ->
      ( match getNextBlank s.newPos tokens with
      | Some nextBlankTi ->
          nextBlankTi.startPos
      | None ->
          posAtEndOfACedToken )
    | K.ShiftTab ->
      ( match getPrevBlank s.newPos tokens with
      | Some prevBlankTi ->
          prevBlankTi.startPos
      | None ->
          posAtStartOfACedToken )
    | K.Enter ->
        posAtEndOfACedToken
    | K.Space ->
        let newS =
          (* TODO: consider skipping over non-whitespace separators
             as well, such as the commas in a list:
              we currently do [aced|,___]
              but could do    [aced,|___]
           *)
          moveToNextNonWhitespaceToken ~pos:posAtEndOfACedToken ast s
        in
        newS.newPos
    | _ ->
        posAtEndOfACedToken
  in
  let newState = moveTo newPos (acClear s) in
  newState


(* Used for piping and wrapping line in let.  For both we are often at the last argument of a function call. We want to perform the operation on the entire expression om the last line, not just the expression present in the token at the end of the line. This function helps us find the whole expression that we would want to perform it on.
*)
let rec findAppropriateParentToWrap (oldExpr : fluidExpr) (ast : ast) :
    fluidExpr option =
  let child = oldExpr in
  let parent = findParent (eid oldExpr) ast in
  match parent with
  | Some parent ->
    ( match parent with
    | EInteger _
    | EBlank _
    | EString _
    | EVariable _
    | EBool _
    | ENull _
    | EPipeTarget _
    | EFloat _ ->
        recover "these cant be parents" ~debug:parent None
    (* If the parent is some sort of "resetting", then we probably meant the child *)
    | ELet _
    | EIf _
    | EMatch _
    | ERecord _
    | EPipe _
    | EOldExpr _
    | ELambda _
    (* Not sure what to do here, probably nothing fancy *)
    | EFeatureFlag _ ->
        Some child
    (* These are the expressions we're trying to skip. They are "sub-line" expressions. *)
    | EBinOp _ | EFnCall _ | EList _ | EConstructor _ | EFieldAccess _ ->
        findAppropriateParentToWrap parent ast
    (* These are wrappers of the current expr. *)
    | EPartial _ | ERightPartial _ ->
        findAppropriateParentToWrap parent ast )
  | None ->
      (* If we get to the root *)
      Some child


(** createPipe makes the expression with `id` the target of a new EPipe.

    If the ~findParent flag is passed, an "appropriate" parent of the
    expression is found as the pipe target. See findAppropriatePipingParent for
    details. If the flag is not passed, the pipe target is the expression as
    given.

    Returns a new ast, fluidState, and the id of the EBlank created as the
    first pipe expression (if the pipe was successfully added).
    *)
let createPipe ~(findParent : bool) (id : id) (ast : ast) (s : state) :
    ast * state * id option =
  let action =
    Printf.sprintf "createPipe(id=%s findParent=%B)" (deID id) findParent
  in
  let s = recordAction action s in
  let exprToReplace =
    findExpr id ast
    |> Option.andThen ~f:(fun e ->
           if findParent then findAppropriateParentToWrap e ast else Some e )
    |> Option.map ~f:extractSubexprFromPartial
  in
  match exprToReplace with
  | None ->
      (ast, s, None)
  | Some expr ->
      let blankId = gid () in
      let newExpr = EPipe (gid (), [expr; EBlank blankId]) in
      let ast = replaceExpr (eid expr) ast ~newExpr in
      (ast, s, Some blankId)


let updateFromACItem
    (entry : fluidAutocompleteItem)
    (ti : tokenInfo)
    (ast : ast)
    (s : state)
    (key : K.key) : ast * state =
  let id = Token.tid ti.token in
  let newExpr, offset = acToExpr entry in
  let oldExpr = findExpr id ast in
  let parent = findParent id ast in
  let newAST, offset =
    match (ti.token, oldExpr, parent, newExpr) with
    (* since patterns have no partial but commit as variables
     * automatically, allow intermediate variables to
     * be autocompletable to other expressions *)
    | ( (TPatternBlank (mID, pID, _) | TPatternVariable (mID, pID, _, _))
      , _
      , _
      , _ ) ->
        let newPat, acOffset = acToPattern entry in
        let newAST = replacePattern ~newPat mID pID ast in
        (newAST, acOffset)
    | ( (TPartial _ | TRightPartial _)
      , Some
          ( (ERightPartial (_, _, subExpr) | EPartial (_, _, subExpr)) as
          oldExpr )
      , _
      , _ )
      when entry = FACKeyword KPipe ->
        (* The pipe operator is intended to be roughly "line-based", which
         * means tht instead of tying this to the smallest expression (which is
         * within the partial) we go back and figure out the "line", which is
         * to say the largest expression that doesn't break a line. *)
        let newAST =
          let exprToReplace =
            findAppropriateParentToWrap oldExpr ast
            |> Option.map ~f:extractSubexprFromPartial
          in
          match exprToReplace with
          | None ->
              let newExpr = EPipe (gid (), [subExpr; newB ()]) in
              replaceExpr (eid oldExpr) ast ~newExpr
          | Some expr when expr = subExpr ->
              let newExpr = EPipe (gid (), [subExpr; newB ()]) in
              replaceExpr (eid oldExpr) ast ~newExpr
          | Some expr ->
              let expr = replaceExpr (eid oldExpr) expr ~newExpr:subExpr in
              let newExpr = EPipe (gid (), [expr; newB ()]) in
              replaceExpr (eid expr) ast ~newExpr
        in
        let tokens = toTokens s newAST in
        let nextBlank = getNextBlankPos s.newPos tokens in
        (newAST, nextBlank - ti.startPos)
    | TPartial _, _, Some (EPipe _), EBinOp (bID, name, _, rhs, str) ->
        let newExpr = EBinOp (bID, name, EPipeTarget (gid ()), rhs, str) in
        let offset = String.length name + 1 in
        let newAST = replaceExpr ~newExpr id ast in
        (newAST, offset)
    | TPartial _, Some _, Some (EPipe _), EFnCall (fnID, name, _ :: args, str)
      ->
        let newExpr =
          EFnCall (fnID, name, EPipeTarget (gid ()) :: args, str)
        in
        let offset = String.length name + 1 in
        let newAST = replacePartialWithArguments ~newExpr id s ast in
        (newAST, offset)
    | ( TPartial _
      , Some (EPartial (_, _, EBinOp (_, _, lhs, rhs, _)))
      , _
      , EBinOp (bID, name, _, _, str) ) ->
        let newExpr = EBinOp (bID, name, lhs, rhs, str) in
        let newAST = replaceExpr ~newExpr id ast in
        (newAST, String.length name)
    | TPartial _, _, _, _ ->
        let newAST = replacePartialWithArguments ~newExpr id s ast in
        (newAST, offset)
    | ( TRightPartial _
      , Some (ERightPartial (_, _, oldExpr))
      , _
      , EBinOp (bID, name, _, rhs, str) ) ->
        let newExpr = EBinOp (bID, name, oldExpr, rhs, str) in
        let newAST = replaceExpr ~newExpr id ast in
        (newAST, String.length name)
    | ( (TFieldName _ | TFieldPartial _ | TBlank _)
      , Some
          ( EFieldAccess (faID, labelid, expr, _)
          | EPartial (_, _, EFieldAccess (faID, labelid, expr, _)) )
      , _
      , EFieldAccess (_, _, _, newname) ) ->
        let newExpr = EFieldAccess (faID, labelid, expr, newname) in
        let newAST = replaceExpr ~newExpr id ast in
        (newAST, offset)
    | _, _, _, _ ->
        let newAST = replaceExpr ~newExpr id ast in
        (newAST, offset)
  in
  (newAST, acMoveBasedOnKey key ti.startPos offset s newAST)


let acEnter (ti : tokenInfo) (ast : ast) (s : state) (key : K.key) :
    ast * state =
  let s = recordAction ~ti "acEnter" s in
  match AC.highlighted s.ac with
  | None ->
    ( match ti.token with
    | TPatternVariable _ ->
        (ast, moveToNextBlank ~pos:s.newPos ast s)
    | _ ->
        (ast, s) )
  | Some entry ->
      updateFromACItem entry ti ast s key


let acClick
    (entry : fluidAutocompleteItem) (ti : tokenInfo) (ast : ast) (s : state) =
  updateFromACItem entry ti ast s K.Enter


let commitIfValid (newPos : int) (ti : tokenInfo) ((ast, s) : ast * fluidState)
    : ast =
  let highlightedText = s.ac |> AC.highlighted |> Option.map ~f:AC.asName in
  let isInside = newPos >= ti.startPos && newPos <= ti.endPos in
  (* TODO: if we can't move off because it's the start/end etc of the ast, we
   * may want to commit anyway. *)
  if (not isInside) && Some (Token.toText ti.token) = highlightedText
  then
    let newAST, _ = acEnter ti ast s K.Enter in
    newAST
  else ast


let acMaybeCommit (newPos : int) (ast : ast) (s : fluidState) : ast =
  match s.ac.query with
  | Some (_, ti) ->
      commitIfValid newPos ti (ast, s)
  | None ->
      ast


(* Convert the expr ti into a FieldAccess, using the currently
 * selected Autocomplete value *)
let acStartField (ti : tokenInfo) (ast : ast) (s : state) : ast * state =
  let s = recordAction ~ti "acStartField" s in
  match (AC.highlighted s.ac, ti.token) with
  | Some (FACField fieldname as entry), TFieldName (faID, _, _)
  | Some (FACField fieldname as entry), TFieldPartial (_, faID, _, _) ->
      let ast, s = updateFromACItem entry ti ast s K.Enter in
      let newAST = exprToFieldAccess faID (gid ()) ast in
      let length = String.length fieldname + 1 in
      let newState = s |> moveTo (ti.startPos + length) |> acClear in
      (newAST, newState)
  | Some entry, _ ->
      let newExpr, length = acToExpr entry in
      let newExpr =
        EPartial (gid (), "", EFieldAccess (gid (), newExpr, gid (), ""))
      in
      let length = length + 1 in
      let newState = s |> moveTo (ti.startPos + length) |> acClear in
      let newAST = replaceExpr ~newExpr (Token.tid ti.token) ast in
      (newAST, newState)
  | _ ->
      (ast, s)


(* -------------------- *)
(* Code entering/interaction *)
(* -------------------- *)

type newPosition =
  | RightOne
  | RightTwo
  | LeftOne
  | LeftThree
  | MoveToStart
  | MoveToTokenEnd of id * (* offset *) int
  | SamePlace
  | TwoAfterEnd
  | Exactly of int
  (* The hope is that we can migrate everything to
     AtTarget and then remove this entirely *)
  | AtTarget of caretTarget

let adjustPosForReflow
    ~state
    (newAST : fluidExpr)
    (oldTI : tokenInfo)
    (oldPos : int)
    (adjustment : newPosition) : int =
  (* Reflow refers to adjusting layout for to prevent overly long lines. Any
   * character change can cause that line to be too long (and so it will
   * reflow) or no longer too long (and so it will un-reflow).
   *
   * We need to find where the cursor should be in the new AST, given the old
   * position, the old token it was in, and the new AST. We do this by finding
   * the old token in the new token stream, and then doing the appropriate
   * adjustment. There are definitely places this won't work, but I haven't
   * found them yet. *)
  let newTokens = toTokens state newAST in
  let newTI =
    List.find newTokens ~f:(fun x -> Token.matches oldTI.token x.token)
  in
  let diff =
    match newTI with
    | Some newTI ->
        newTI.startPos - oldTI.startPos
    | None ->
        0
  in
  let newPos = oldPos + diff in
  match (adjustment, newTI) with
  | SamePlace, _ ->
      newPos
  | RightOne, _ ->
      if FluidToken.isBlank oldTI.token
      then oldTI.startPos + diff + 1
      else newPos + 1
  | RightTwo, _ ->
      if FluidToken.isBlank oldTI.token
      then oldTI.startPos + diff + 2
      else newPos + 2
  | LeftOne, Some newTI ->
      if Token.isAtom newTI.token
      then newTI.startPos
      else if newTI.endPos < newPos
      then newTI.endPos
      else newPos - 1
  | LeftOne, None ->
      let newPos = newPos - 1 in
      max 0 newPos
  | LeftThree, Some newTI ->
      let newPos = min newPos newTI.endPos in
      let newPos = newPos - 3 in
      max 0 newPos
  | LeftThree, None ->
      let newPos = newPos - 3 in
      max 0 newPos
  | Exactly pos, _ ->
      pos
  | MoveToTokenEnd (id, offset), _ ->
      newTokens
      |> List.reverse
      |> List.find ~f:(fun x -> Token.tid x.token = id)
      |> Option.map ~f:(fun ti ->
             if FluidToken.isBlank ti.token
             then ti.startPos
             else ti.endPos + offset )
      |> recoverOpt "didn't find expected token in MoveToToken" ~default:newPos
  | MoveToStart, Some newTI ->
      newTI.startPos
  | MoveToStart, None ->
      oldTI.startPos
  | TwoAfterEnd, None ->
      oldTI.endPos + 2
  | TwoAfterEnd, Some newTI ->
      newTI.endPos + 2
  | AtTarget target, _ ->
      posFromCaretTarget state newAST target


let doBackspace ~(pos : int) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  let s = recordAction ~pos ~ti "doBackspace" s in
  let offset =
    match ti.token with
    | TPatternString _ | TString _ | TStringMLStart _ ->
        pos - ti.startPos - 2
    | TStringMLMiddle (_, _, strOffset, _) | TStringMLEnd (_, _, strOffset, _)
      ->
        pos - ti.startPos - 1 + strOffset
    | TFnVersion (_, partialName, _, _) ->
        (* Did this because we combine TFVersion and TFName into one partial so we need to get the startPos of the partial name *)
        let startPos = ti.endPos - String.length partialName in
        pos - startPos - 1
    | _ ->
        pos - ti.startPos - 1
  in
  let newID = gid () in
  let newAST, newPosition =
    match ti.token with
    | TIfThenKeyword _ | TIfElseKeyword _ | TLambdaArrow _ | TMatchSep _ ->
        (ast, MoveToStart)
    | TIfKeyword _ | TLetKeyword _ | TLambdaSymbol _ | TMatchKeyword _ ->
        let newAST = removeEmptyExpr (Token.tid ti.token) ast in
        if newAST = ast then (ast, SamePlace) else (newAST, MoveToStart)
    | TString (id, "") ->
        (replaceExpr id ~newExpr:(EBlank newID) ast, LeftOne)
    | TPatternString (mID, id, "", _) ->
        (replacePattern mID id ~newPat:(FPBlank (mID, newID)) ast, LeftOne)
    | TLambdaSep (id, idx) ->
        (removeLambdaSepToken id ast idx, LeftOne)
    | TListSep (id, idx) ->
        (removeListSepToken id ast idx, LeftOne)
    | (TRecordOpen id | TListOpen id) when exprIsEmpty id ast ->
        (replaceExpr id ~newExpr:(EBlank newID) ast, LeftOne)
    | TRecordFieldname {recordID; index; fieldName = ""} when pos = ti.startPos
      ->
        let newAst = removeRecordField recordID index ast in
        let maybeExprID = recordExprIdAtIndex recordID (index - 1) newAst in
        let target =
          match maybeExprID with
          | None ->
              { astRef = ARRecord (recordID, RPOpen)
              ; offset = 1 (* right after the { *) }
          | Some exprId ->
              caretTargetForLastPartOfExpr exprId newAst
        in
        (newAst, AtTarget target)
    | TPatternBlank (mID, id, _) when pos = ti.startPos ->
        (removePatternRow mID id ast, LeftThree)
    | TBlank _
    | TPlaceholder _
    | TIndent _
    | TLetAssignment _
    | TListClose _
    | TListOpen _
    | TRecordOpen _
    | TRecordClose _
    | TRecordSep _
    | TSep _
    | TParenOpen _
    | TParenClose _
    | TPatternBlank _
    | TPartialGhost _ ->
        (ast, LeftOne)
    | TNewline _ ->
        (ast, Exactly ti.startPos)
    | TFieldOp (id, lhsId) ->
        let newAst = removeField id ast in
        (newAst, AtTarget (caretTargetForLastPartOfExpr lhsId newAst))
    | TFloatPoint id ->
        (removePointFromFloat id ast, LeftOne)
    | TPatternFloatPoint (mID, id, _) ->
        (removePatternPointFromFloat mID id ast, LeftOne)
    | TConstructorName (id, str)
    (* str is the partialName: *)
    | TFnName (id, str, _, _, _)
    | TFnVersion (id, str, _, _) ->
        let f str = removeCharAt str offset in
        (replaceWithPartial (f str) id ast, LeftOne)
    | TRightPartial (_, str) when String.length str = 1 ->
        let ast, targetID = deleteRightPartial ti ast in
        (ast, MoveToTokenEnd (targetID, 0))
    | TPartial (_, str) when String.length str = 1 ->
        let newAST, newState = deletePartial ti ast s in
        (newAST, Exactly newState.newPos)
    | TBinOp (_, str) when String.length str = 1 ->
        let ast, targetID = deleteBinOp ti ast in
        (ast, MoveToTokenEnd (targetID, 0))
    | TStringMLEnd (id, thisStr, strOffset, _)
      when String.length thisStr = 1 && offset = strOffset ->
        let f str = removeCharAt str offset in
        let newAST = replaceStringToken ~f ti.token ast in
        (newAST, MoveToTokenEnd (id, -1) (* quote *))
    | TString _
    | TStringMLStart _
    | TStringMLMiddle _
    | TStringMLEnd _
    | TPatternString _
    | TRecordFieldname _
    | TInteger _
    | TTrue _
    | TFalse _
    | TPatternTrue _
    | TPatternFalse _
    | TNullToken _
    | TVariable _
    | TFieldName _
    | TFieldPartial _
    | TLetLHS _
    | TPatternInteger _
    | TPatternNullToken _
    | TPatternVariable _
    | TRightPartial _
    | TPartial _
    | TBinOp _
    | TLambdaVar _ ->
        let f str = removeCharAt str offset in
        (replaceStringToken ~f ti.token ast, LeftOne)
    | TPatternFloatWhole (mID, id, str, _) ->
        let str = removeCharAt str offset in
        (replacePatternFloatWhole str mID id ast, LeftOne)
    | TPatternFloatFraction (mID, id, str, _) ->
        let str = removeCharAt str offset in
        (replacePatternFloatFraction str mID id ast, LeftOne)
    | TFloatWhole (id, str) ->
        let str = removeCharAt str offset in
        (replaceFloatWhole str id ast, LeftOne)
    | TFloatFraction (id, str) ->
        let str = removeCharAt str offset in
        (replaceFloatFraction str id ast, LeftOne)
    | TPatternConstructorName (mID, id, str, _) ->
        let f str = removeCharAt str offset in
        (replacePatternWithPartial (f str) mID id ast, LeftOne)
    | TPipe (id, i, _) ->
        let newPosition =
          match getTokensAtPosition ~pos:ti.startPos (toTokens s ast) with
          | Some leftTI, _, _ ->
              let newState = doLeft ~pos:ti.startPos leftTI s in
              Exactly newState.newPos
          | _ ->
              recover
                "TPipe should never occur on first line of AST"
                ~debug:ti
                SamePlace
        in
        (removePipe id ast i, newPosition)
  in
  let newPos = adjustPosForReflow ~state:s newAST ti pos newPosition in
  (newAST, {s with newPos})


let doDelete ~(pos : int) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  let s = recordAction ~pos ~ti "doDelete" s in
  let left s = moveOneLeft pos s in
  let offset =
    match ti.token with
    | TFnVersion (_, partialName, _, _) ->
        (* Did this because we combine TFVersion and TFName into one partial so we need to get the startPos of the partial name *)
        let startPos = ti.endPos - String.length partialName in
        pos - startPos
    | _ ->
        pos - ti.startPos
  in
  let newID = gid () in
  let f str = removeCharAt str offset in
  match ti.token with
  | TIfThenKeyword _ | TIfElseKeyword _ | TLambdaArrow _ | TMatchSep _ ->
      (ast, s)
  | TIfKeyword _ | TLetKeyword _ | TLambdaSymbol _ | TMatchKeyword _ ->
      (removeEmptyExpr (Token.tid ti.token) ast, s)
  | (TListOpen id | TRecordOpen id) when exprIsEmpty id ast ->
      (replaceExpr id ~newExpr:(newB ()) ast, s)
  | TLambdaSep (id, idx) ->
      (removeLambdaSepToken id ast idx, s)
  | TListSep (id, idx) ->
      (removeListSepToken id ast idx, s)
  | TBlank _
  | TPlaceholder _
  | TIndent _
  | TLetAssignment _
  | TListClose _
  | TListOpen _
  | TNewline _
  | TRecordClose _
  | TRecordOpen _
  | TRecordSep _
  | TSep _
  | TParenOpen _
  | TParenClose _
  | TPartialGhost _ ->
      (ast, s)
  | TConstructorName (id, str)
  (* str is the partialName: *)
  | TFnName (id, str, _, _, _)
  | TFnVersion (id, str, _, _) ->
      let f str = removeCharAt str offset in
      (replaceWithPartial (f str) id ast, s)
  | TFieldOp (id, _) ->
      (removeField id ast, s)
  | TString (id, str) ->
      let target s =
        (* if we're in front of the quotes vs within it *)
        if offset == 0 then s else left s
      in
      if str = ""
      then (ast |> replaceExpr id ~newExpr:(EBlank newID), target s)
      else
        let str = removeCharAt str (offset - 1) in
        (replaceExpr id ~newExpr:(EString (newID, str)) ast, s)
  | TStringMLStart (id, _, _, str) ->
      let str = removeCharAt str (offset - 1) in
      (replaceExpr id ~newExpr:(EString (newID, str)) ast, s)
  | TStringMLMiddle (id, _, strOffset, str) ->
      let offset = offset + strOffset in
      let str = removeCharAt str offset in
      (replaceExpr id ~newExpr:(EString (newID, str)) ast, s)
  | TStringMLEnd (id, endStr, strOffset, _) ->
      let f str = removeCharAt str (offset + strOffset) in
      let newAST = replaceStringToken ~f ti.token ast in
      let newState =
        if String.length endStr = 1 && offset = 0
        then
          let moved = moveToEndOfTarget id newAST s in
          {moved with newPos = moved.newPos - 1 (* quote *)}
        else s
      in
      (newAST, newState)
  | TPatternString (mID, id, str, _) ->
      let target s =
        (* if we're in front of the quotes vs within it *)
        if offset == 0 then s else left s
      in
      if str = ""
      then
        (ast |> replacePattern mID id ~newPat:(FPBlank (mID, newID)), target s)
      else
        let str = removeCharAt str (offset - 1) in
        (replacePattern mID id ~newPat:(FPString (mID, newID, str)) ast, s)
  | TRightPartial (_, str) when String.length str = 1 ->
      let ast, targetID = deleteRightPartial ti ast in
      (ast, moveToEndOfTarget targetID ast s)
  | TPartial (_, str) when String.length str = 1 ->
      deletePartial ti ast s
  | TBinOp (_, str) when String.length str = 1 ->
      let ast, targetID = deleteBinOp ti ast in
      (ast, moveToEndOfTarget targetID ast s)
  | TRecordFieldname _
  | TInteger _
  | TPatternInteger _
  | TTrue _
  | TFalse _
  | TNullToken _
  | TVariable _
  | TPartial _
  | TRightPartial _
  | TFieldName _
  | TFieldPartial _
  | TLetLHS _
  | TPatternNullToken _
  | TPatternTrue _
  | TPatternFalse _
  | TPatternVariable _
  | TBinOp _
  | TLambdaVar _ ->
      (replaceStringToken ~f ti.token ast, s)
  | TFloatPoint id ->
      (removePointFromFloat id ast, s)
  | TFloatWhole (id, str) ->
      (replaceFloatWhole (f str) id ast, s)
  | TFloatFraction (id, str) ->
      (replaceFloatFraction (f str) id ast, s)
  | TPatternFloatPoint (mID, id, _) ->
      (removePatternPointFromFloat mID id ast, s)
  | TPatternFloatFraction (mID, id, str, _) ->
      (replacePatternFloatFraction (f str) mID id ast, s)
  | TPatternFloatWhole (mID, id, str, _) ->
      (replacePatternFloatWhole (f str) mID id ast, s)
  | TPatternConstructorName (mID, id, str, _) ->
      let f str = removeCharAt str offset in
      (replacePatternWithPartial (f str) mID id ast, s)
  | TPatternBlank _ ->
      (ast, s)
  | TPipe (id, i, length) ->
      let newAST = removePipe id ast i in
      let s =
        (* index goes from zero and doesn't include first element, while length
         * does. So + 2 correct for both those *)
        if i + 2 = length
        then
          (* when deleting the last element, go to the end of the previous element *)
          match getTokensAtPosition ~pos:ti.startPos (toTokens s ast) with
          | Some leftTI, _, _ ->
              doLeft ~pos:ti.startPos leftTI s
          | _ ->
              recover
                "TPipe should never occur on first line of AST"
                ~debug:ti
                s
        else s
      in
      (newAST, s)


let doInsert' ~pos (letter : char) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  let s = recordAction ~ti ~pos "doInsert" s in
  let s = {s with upDownCol = None} in
  let letterStr = String.fromChar letter in
  let offset =
    match ti.token with
    | TString _ | TPatternString _ | TStringMLStart (_, _, _, _) ->
        (* account for the quote *)
        pos - ti.startPos - 1
    | TStringMLMiddle (_, _, strOffset, _) | TStringMLEnd (_, _, strOffset, _)
      ->
        (* no quote here, unlike TStringMLStart *)
        pos - ti.startPos + strOffset
    | _ ->
        pos - ti.startPos
  in
  let f str = String.insertAt ~index:offset ~insert:letterStr str in
  let newID = gid () in
  let lambdaArgs ti =
    let placeholderName =
      match ti.token with
      | TPlaceholder ((name, _), _) ->
          Some name
      | _ ->
          None
    in
    let fnname =
      let id = FluidToken.tid ti.token in
      match findParent id ast with
      | Some (EFnCall (_, name, _, _)) ->
          Some name
      | _ ->
          None
    in
    s.ac.functions
    |> List.find ~f:(fun f -> Some f.fnName = fnname)
    |> Option.andThen ~f:(fun fn ->
           List.find
             ~f:(fun {paramName; _} -> Some paramName = placeholderName)
             fn.fnParameters )
    |> Option.map ~f:(fun p -> p.paramBlock_args)
    |> Option.withDefault ~default:[""]
    |> List.map ~f:(fun str -> (gid (), str))
  in
  let newExpr =
    if letter = '"'
    then EString (newID, "")
    else if letter = '['
    then EList (newID, [])
    else if letter = '{'
    then ERecord (newID, [])
    else if letter = '\\'
    then ELambda (newID, lambdaArgs ti, EBlank (gid ()))
    else if letter = ','
    then EBlank newID (* new separators *)
    else if isNumber letterStr
    then EInteger (newID, letterStr |> coerceStringTo63BitInt)
    else EPartial (newID, letterStr, EBlank (gid ()))
  in
  let newAST, newPosition =
    match ti.token with
    | (TFieldName (id, _, _) | TVariable (id, _))
      when pos = ti.endPos && letter = '.' ->
        let fieldID = gid () in
        (exprToFieldAccess id fieldID ast, RightOne)
    (* Dont add space to blanks *)
    | ti when FluidToken.isBlank ti && letterStr == " " ->
        (ast, SamePlace)
    (* replace blank *)
    | TBlank id | TPlaceholder (_, id) ->
        (replaceExpr id ~newExpr ast, RightOne)
    (* lists *)
    | TListOpen id when letter = ',' ->
        (insertInList ~index:0 id ~newExpr:(newB ()) ast, SamePlace)
    | TListOpen id ->
        (insertInList ~index:0 id ~newExpr ast, RightOne)
    (* lambda *)
    | TLambdaSymbol id when letter = ',' ->
        (insertLambdaVar ~index:0 id ~name:"" ast, SamePlace)
    | TLambdaVar (id, _, index, _) when letter = ',' ->
        (insertLambdaVar ~index:(index + 1) id ~name:"" ast, TwoAfterEnd)
    (* Ignore invalid situations *)
    | (TString _ | TPatternString _ | TStringMLStart _) when offset < 0 ->
        (ast, SamePlace)
    | TInteger _
    | TPatternInteger _
    | TFloatFraction _
    | TFloatWhole _
    | TPatternFloatWhole _
    | TPatternFloatFraction _
      when not (isNumber letterStr) ->
        (ast, SamePlace)
    | (TInteger _ | TPatternInteger _ | TFloatWhole _ | TPatternFloatWhole _)
      when '0' = letter && offset = 0 ->
        (ast, SamePlace)
    | TVariable _
    | TPatternVariable _
    | TLetLHS _
    | TFieldName _
    | TFieldPartial _
    | TLambdaVar _
    | TRecordFieldname _
      when not (isIdentifierChar letterStr) ->
        (ast, SamePlace)
    | TVariable _
    | TPatternVariable _
    | TLetLHS _
    | TFieldName _
    | TFieldPartial _
    | TLambdaVar _
    | TRecordFieldname _
      when isNumber letterStr && (offset = 0 || FluidToken.isBlank ti.token) ->
        (ast, SamePlace)
    | (TFnVersion _ | TFnName _) when not (isFnNameChar letterStr) ->
        (ast, SamePlace)
    (* Do the insert *)
    | (TString (_, str) | TStringMLEnd (_, str, _, _))
      when pos = ti.endPos - 1 && String.length str = 40 ->
        (* Strings with end quotes *)
        let s = recordAction ~pos ~ti "string to mlstring" s in
        (* Inserting at the end of an multi-line segment goes to next segment *)
        let newAST = replaceStringToken ~f ti.token ast in
        let newState = moveToNextNonWhitespaceToken ~pos newAST s in
        (newAST, Exactly (newState.newPos + 1))
    | (TStringMLStart (_, str, _, _) | TStringMLMiddle (_, str, _, _))
      when pos = ti.endPos && String.length str = 40 ->
        (* Strings without end quotes *)
        let s = recordAction ~pos ~ti "extend multiline string" s in
        (* Inserting at the end of an multi-line segment goes to next segment *)
        let newAST = replaceStringToken ~f ti.token ast in
        let newState = moveToNextNonWhitespaceToken ~pos newAST s in
        (newAST, Exactly (newState.newPos + 1))
    | TRecordFieldname _
    | TFieldName _
    | TFieldPartial _
    | TVariable _
    | TPartial _
    | TRightPartial _
    | TString _
    | TStringMLStart _
    | TStringMLMiddle _
    | TStringMLEnd _
    | TPatternString _
    | TLetLHS _
    | TTrue _
    | TFalse _
    | TPatternTrue _
    | TPatternFalse _
    | TNullToken _
    | TPatternNullToken _
    | TPatternVariable _
    | TBinOp _
    | TLambdaVar _ ->
        (replaceStringToken ~f ti.token ast, RightOne)
    | TPatternInteger (_, _, i, _) | TInteger (_, i) ->
        let newLength = f i |> coerceStringTo63BitInt |> String.length in
        let move = if newLength > offset then RightOne else SamePlace in
        (replaceStringToken ~f ti.token ast, move)
    | TFloatWhole (id, str) ->
        (replaceFloatWhole (f str) id ast, RightOne)
    | TFloatFraction (id, str) ->
        (replaceFloatFraction (f str) id ast, RightOne)
    | TFloatPoint id ->
        (insertAtFrontOfFloatFraction letterStr id ast, RightOne)
    | TPatternFloatWhole (mID, id, str, _) ->
        (replacePatternFloatWhole (f str) mID id ast, RightOne)
    | TPatternFloatFraction (mID, id, str, _) ->
        (replacePatternFloatFraction (f str) mID id ast, RightOne)
    | TPatternFloatPoint (mID, id, _) ->
        (insertAtFrontOfPatternFloatFraction letterStr mID id ast, RightOne)
    | TPatternConstructorName _ ->
        (ast, SamePlace)
    | TPatternBlank (mID, pID, _) ->
        let newPat =
          if letter = '"'
          then FPString (mID, newID, "")
          else if isNumber letterStr
          then FPInteger (mID, newID, letterStr |> coerceStringTo63BitInt)
          else FPVariable (mID, newID, letterStr)
        in
        (replacePattern mID pID ~newPat ast, RightOne)
    (* do nothing *)
    | TNewline _
    | TIfKeyword _
    | TIfThenKeyword _
    | TIfElseKeyword _
    | TFieldOp _
    | TFnName _
    | TFnVersion _
    | TLetKeyword _
    | TLetAssignment _
    | TSep _
    | TListClose _
    | TListSep _
    | TIndent _
    | TRecordOpen _
    | TRecordClose _
    | TRecordSep _
    | TPipe _
    | TLambdaSymbol _
    | TLambdaArrow _
    | TConstructorName _
    | TLambdaSep _
    | TMatchSep _
    | TMatchKeyword _
    | TPartialGhost _
    | TParenOpen _
    | TParenClose _ ->
        (ast, SamePlace)
  in
  let newPos = adjustPosForReflow ~state:s newAST ti pos newPosition in
  (newAST, {s with newPos})


let doInsert
    ~pos (letter : char option) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  match letter with
  | None ->
      (ast, s)
  | Some letter ->
      doInsert' ~pos letter ti ast s


let wrapInLet (ti : tokenInfo) (ast : ast) (s : state) : ast * fluidState =
  let s = recordAction "wrapInLet" s in
  let id = Token.tid ti.token in
  match findExpr id ast with
  | Some expr ->
      let bodyId = gid () in
      let exprToWrap =
        match findAppropriateParentToWrap expr ast with
        | Some e ->
            e
        | None ->
            expr
      in
      let eid = eid exprToWrap in
      let newExpr = ELet (gid (), gid (), "_", exprToWrap, EBlank bodyId) in
      let newAST = replaceExpr ~newExpr eid ast in
      let newPos =
        posFromCaretTarget s newAST {astRef = ARBlank bodyId; offset = 0}
      in
      (newAST, {s with newPos})
  | None ->
      (ast, s)


let maybeOpenCmd (m : Types.model) : Types.modification =
  let getCurrentToken tokens =
    let _, mCurrent, _ = getTokensAtPosition tokens ~pos:m.fluidState.newPos in
    mCurrent
  in
  Toplevel.selected m
  |> Option.andThen ~f:(fun tl ->
         TL.rootExpr tl
         |> Option.andThen ~f:(fun ast -> Some (fromExpr m.fluidState ast))
         |> Option.andThen ~f:(fun ast -> Some (toTokens m.fluidState ast))
         |> Option.withDefault ~default:[]
         |> getCurrentToken
         |> Option.andThen ~f:(fun ti ->
                Some (FluidCommandsShow (TL.id tl, ti.token)) ) )
  |> Option.withDefault ~default:NoChange


let orderRangeFromSmallToBig ((rangeBegin, rangeEnd) : int * int) : int * int =
  if rangeBegin > rangeEnd
  then (rangeEnd, rangeBegin)
  else (rangeBegin, rangeEnd)


(* Always returns a selection represented as two ints with the smaller int first.
   The numbers are identical if there is no selection. *)
let fluidGetSelectionRange (s : fluidState) : int * int =
  match s.selectionStart with
  | Some beginIdx when beginIdx < s.newPos ->
      (beginIdx, s.newPos)
  | Some endIdx ->
      (s.newPos, endIdx)
  | None ->
      (s.newPos, s.newPos)


let fluidGetCollapsedSelectionStart (s : fluidState) : int =
  fluidGetSelectionRange s |> orderRangeFromSmallToBig |> Tuple2.first


let fluidGetOptionalSelectionRange (s : fluidState) : (int * int) option =
  let endIdx = s.newPos in
  match s.selectionStart with
  | Some beginIdx ->
      Some (beginIdx, endIdx)
  | None ->
      None


let tokensInRange selStartPos selEndPos ~state ast : fluidTokenInfo list =
  toTokens state ast
  (* this condition is a little flaky, sometimes selects wrong tokens *)
  |> List.filter ~f:(fun t ->
         (* selectionStart within token *)
         (t.startPos <= selStartPos && selStartPos < t.endPos)
         (* selectionEnd within token *)
         || (t.startPos < selEndPos && selEndPos <= t.endPos)
         (* tokenStart within selection  *)
         || (selStartPos <= t.startPos && t.startPos < selEndPos)
         (* tokenEnd within selection  *)
         || (selStartPos < t.endPos && t.endPos <= selEndPos) )


let getTopmostSelectionID startPos endPos ~state ast : id option =
  let asExpr = toExpr ast in
  (* TODO: if there's multiple topmost IDs, return parent of those IDs *)
  tokensInRange startPos endPos ~state ast
  |> List.filter ~f:(fun ti -> not (Token.isNewline ti.token))
  |> List.foldl ~init:(None, 0) ~f:(fun ti (topmostID, topmostDepth) ->
         let curID = Token.parentExprID ti.token in
         let curDepth = AST.ancestors curID asExpr |> List.length in
         if (* check if current token is higher in the AST than the last token,
             * or if there's no topmost ID yet *)
            (curDepth < topmostDepth || topmostID = None)
            (* account for tokens that don't have ancestors (depth = 0)
             * but are not the topmost expression in the AST *)
            && not (curDepth = 0 && findExpr curID ast != Some ast)
         then (Some curID, curDepth)
         else (topmostID, topmostDepth) )
  |> Tuple2.first


let rec updateKey ?(recursing = false) (key : K.key) (ast : ast) (s : state) :
    ast * state =
  let pos = s.newPos in
  let keyChar = K.toChar key in
  let tokens = toTokens s ast in
  (* These might be the same token *)
  let toTheLeft, toTheRight, mNext = getNeighbours ~pos tokens in
  let onEdge =
    match (toTheLeft, toTheRight) with
    | L (lt, lti), R (rt, rti) ->
        (lt, lti) <> (rt, rti)
    | _ ->
        true
  in
  (* This expresses whether or not the expression to the left of
   * the insert should be wrapped in a binary operator, and determines
   * that fact based on the _next_ token *)
  let wrappableInBinop rightNeighbour =
    match rightNeighbour with
    | L _ ->
        (* This function is only defined in terms of right + no lookahead, so say false if we were accidentally passed an `L` *)
        false
    | No ->
        (* Assume that if we're in a blank and there's nothing to our
         * right then we must be in an expression blank that can
         * be safely wrapped *)
        true
    | R (token, _) ->
      (* This almost certainly doesn't catch all cases,
         * if you find a bug please add a case + test *)
      ( match token with
      | TLetLHS _
      | TLetAssignment _
      | TFieldName _
      | TRecordFieldname _
      | TRecordSep _
      | TLambdaSep _
      | TLambdaArrow _
      | TLambdaVar _ ->
          false
      | _ ->
          true )
  in
  let infixKeys =
    [ K.Plus
    ; K.Percent
    ; K.Minus
    ; K.Multiply
    ; K.ForwardSlash
    ; K.LessThan
    ; K.GreaterThan
    ; K.Ampersand
    ; K.ExclamationMark
    ; K.Caret
    ; K.Equals
    ; K.Pipe ]
  in
  let keyIsInfix = List.member ~value:key infixKeys in
  (* TODO: When changing TVariable and TFieldName and probably TFnName we
     * should convert them to a partial which retains the old object *)
  (* Checks to see if the token is within an if-condition statement *)
  let isInIfCondition token =
    let rec recurseUp maybeExpr prevId =
      match maybeExpr with
      | Some (EIf (_, cond, _, _)) when eid cond = prevId ->
          true
      | Some e ->
          let id = eid e in
          recurseUp (findParent id ast) id
      | None ->
          false
    in
    let tid = Token.tid token in
    recurseUp (findParent tid ast) tid
  in
  let newAST, newState =
    (* This match drives a big chunk of the change operations, but is
     * inconsistent about whether it looks left/right and also about what
     * conditions it applies to each of the tokens.
     *
     * The largest inconsistency is whether or not the case expresses "in this
     * exact case, do this exact thing" or "in this very general case, do this
     * thing". The mixing and matching of these two means the cases are very
     * sensitive to ordering. If you're adding a case that's sensitive to
     * ordering ADD A TEST, even if it's otherwise redundant from a product
     * POV. *)
    match (key, toTheLeft, toTheRight) with
    (* Moving through a lambda arrow with '->' *)
    | K.Minus, L (TLambdaVar _, _), R (TLambdaArrow _, ti) ->
        (ast, moveOneRight (ti.startPos + 1) s)
    | K.Minus, L (TLambdaArrow _, _), R (TLambdaArrow _, ti)
      when pos = ti.startPos + 1 ->
        (ast, moveOneRight (ti.startPos + 1) s)
    | K.GreaterThan, L (TLambdaArrow _, _), R (TLambdaArrow _, ti)
      when pos = ti.startPos + 2 ->
        (ast, moveToNextNonWhitespaceToken ~pos ast s)
    (* Deleting *)
    | (K.Delete, _, _ | K.Backspace, _, _) when Option.isSome s.selectionStart
      ->
        deleteSelection ~state:s ~ast
    | K.Backspace, L (TPatternString _, ti), _
    | K.Backspace, L (TString _, ti), _
      when pos = ti.endPos ->
        (* Backspace should move into a string, not delete it *)
        (ast, moveOneLeft pos s)
    | K.Backspace, _, R (TRecordFieldname {fieldName = ""}, ti) ->
        doBackspace ~pos ti ast s
    | K.Backspace, _, R (TPatternBlank _, ti) ->
        doBackspace ~pos ti ast s
    | K.Backspace, L (_, ti), _ ->
        doBackspace ~pos ti ast s
    | K.Delete, _, R (_, ti) ->
        doDelete ~pos ti ast s
    (* Autocomplete menu *)
    (* Note that these are spelt out explicitly on purpose, else they'll
     * trigger on the wrong element sometimes. *)
    | K.Escape, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acClear s)
    | K.Escape, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acClear s)
    | K.Up, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acMoveUp s)
    | K.Up, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acMoveUp s)
    | K.Down, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acMoveDown s)
    | K.Down, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acMoveDown s)
    (* Autocomplete finish *)
    | _, L (_, ti), _
      when isAutocompleting ti s
           && [K.Enter; K.Tab; K.ShiftTab; K.Space] |> List.member ~value:key
      ->
        acEnter ti ast s key
    | _, _, R (_, ti)
      when isAutocompleting ti s
           && [K.Enter; K.Tab; K.ShiftTab; K.Space] |> List.member ~value:key
      ->
        acEnter ti ast s key
    (* When we type a letter/number after an infix operator, complete and
     * then enter the number/letter. *)
    | K.Number _, L (TRightPartial (_, _), ti), _
    | K.Letter _, L (TRightPartial (_, _), ti), _
      when onEdge ->
        let ast, s = acEnter ti ast s K.Tab in
        getLeftTokenAt s.newPos (toTokens s ast |> List.reverse)
        |> Option.map ~f:(fun ti -> doInsert ~pos:s.newPos keyChar ti ast s)
        |> Option.withDefault ~default:(ast, s)
    | K.ShiftEnter, left, _ ->
        let doPipeline ast s =
          let startPos, endPos = fluidGetSelectionRange s in
          let findParent = startPos = endPos in
          let topmostID = getTopmostSelectionID startPos endPos ~state:s ast in
          Option.map topmostID ~f:(fun id ->
              let ast, s, blankId = createPipe ~findParent id ast s in
              match blankId with
              | None ->
                  (ast, s)
              | Some id ->
                  let s = moveToAstRef s ast (ARBlank id) in
                  (ast, s) )
          |> Option.withDefault ~default:(ast, s)
        in
        ( match left with
        | L (TPartial _, ti) when Option.is_some (AC.highlighted s.ac) ->
            let ast, s = acEnter ti ast s K.Enter in
            doPipeline ast s
        | _ ->
            doPipeline ast s )
    (* Special autocomplete entries *)
    (* press dot while in a variable entry *)
    | K.Period, L (TPartial _, ti), _
      when Option.map ~f:AC.isVariable (AC.highlighted s.ac) = Some true ->
        acStartField ti ast s
    | K.Period, L (TFieldPartial _, ti), _
    | K.Period, _, R (TFieldPartial _, ti)
      when Option.map ~f:AC.isField (AC.highlighted s.ac) = Some true ->
        acStartField ti ast s
    (* Tab to next blank *)
    | K.Tab, _, R (_, _) | K.Tab, L (_, _), _ ->
        (ast, moveToNextBlank ~pos ast s)
    | K.ShiftTab, _, R (_, _) | K.ShiftTab, L (_, _), _ ->
        (ast, moveToPrevBlank ~pos ast s)
    | K.SelectAll, _, R (_, _) | K.SelectAll, L (_, _), _ ->
        (ast, selectAll ~pos ast s)
    (* TODO: press comma while in an expr in a list *)
    (* TODO: press comma while in an expr in a record *)
    (* TODO: press equals when in a let *)
    (* TODO: press colon when in a record field *)
    (* Left/Right movement *)
    | K.GoToEndOfWord, _, R (_, ti) | K.GoToEndOfWord, L (_, ti), _ ->
        (ast, goToEndOfWord ~pos ast ti s)
    | K.GoToStartOfWord, _, R (_, ti) | K.GoToStartOfWord, L (_, ti), _ ->
        (ast, goToStartOfWord ~pos ast ti s)
    | K.Left, L (_, ti), _ ->
        (ast, doLeft ~pos ti s |> acMaybeShow ti)
    | K.Right, _, R (_, ti) ->
        (ast, doRight ~pos ~next:mNext ti s |> acMaybeShow ti)
    | K.GoToStartOfLine, _, R (_, ti) | K.GoToStartOfLine, L (_, ti), _ ->
        (ast, moveToStartOfLine ast ti s)
    | K.GoToEndOfLine, _, R (_, ti) ->
        (ast, moveToEndOfLine ast ti s)
    | K.DeleteToStartOfLine, _, R (_, ti) | K.DeleteToStartOfLine, L (_, ti), _
  ->
      (* The behavior of this action is not well specified -- every editor we've seen has slightly different behavior.
           The behavior we use here is: if there is a selection, delete it instead of deleting to start of line (like XCode but not VSCode).
           For expedience, delete to the visual start of line rather than the "real" start of line. This is symmetric with
           K.DeleteToEndOfLine but does not match any code editors we've seen. It does match many non-code text editors. *)
      ( match fluidGetOptionalSelectionRange s with
      | Some selRange ->
          deleteCaretRange ~state:s ~ast selRange
      | None ->
          deleteCaretRange
            ~state:s
            ~ast
            (s.newPos, getStartOfLineCaretPos ast ti s) )
    | K.DeleteToEndOfLine, _, R (_, ti) | K.DeleteToEndOfLine, L (_, ti), _ ->
      (* The behavior of this action is not well specified -- every editor we've seen has slightly different behavior.
           The behavior we use here is: if there is a selection, delete it instead of deleting to end of line (like XCode and VSCode).
           For expedience, in the presence of wrapping, delete to the visual end of line rather than the "real" end of line.
           This matches the behavior of XCode and VSCode. Most standard non-code text editors do not implement this command. *)
      ( match fluidGetOptionalSelectionRange s with
      | Some selRange ->
          deleteCaretRange ~state:s ~ast selRange
      | None ->
          deleteCaretRange
            ~state:s
            ~ast
            (s.newPos, getEndOfLineCaretPos ast ti s) )
    | K.DeleteNextWord, _, R (_, ti) ->
      ( match fluidGetOptionalSelectionRange s with
      | Some selRange ->
          deleteCaretRange ~state:s ~ast selRange
      | None ->
          let movedState = goToEndOfWord ~pos ast ti s in
          let newAst, newState =
            deleteCaretRange ~state:s ~ast (pos, movedState.newPos)
          in
          if newAst = ast && newState.newPos = pos
          then (newAst, movedState)
          else (newAst, newState) )
    | K.DeletePrevWord, L (_, ti), _ ->
      ( match fluidGetOptionalSelectionRange s with
      | Some selRange ->
          deleteCaretRange ~state:s ~ast selRange
      | None ->
          let rangeStart =
            if Token.isStringToken ti.token && pos != ti.startPos
            then getBegOfWordInStrCaretPos ~pos ti
            else ti.startPos
          in
          deleteCaretRange ~state:s ~ast (rangeStart, pos) )
    | K.Up, _, _ ->
        (ast, doUp ~pos ast s)
    | K.Down, _, _ ->
        (ast, doDown ~pos ast s)
    | K.Space, _, R (TSep _, _) ->
        (ast, moveOneRight pos s)
    (* comma - add another of the thing *)
    | K.Comma, L (TListOpen _, toTheLeft), _
    | K.Comma, L (TLambdaSymbol _, toTheLeft), _
    | K.Comma, L (TLambdaVar _, toTheLeft), _
      when onEdge ->
        doInsert ~pos keyChar toTheLeft ast s
    | K.Comma, _, R (TLambdaVar (id, _, index, _), _) when onEdge ->
        (insertLambdaVar ~index id ~name:"" ast, s)
    | K.Comma, L (t, ti), _ ->
        if onEdge
        then (addBlankToList (Token.tid t) ast, moveOneRight ti.endPos s)
        else doInsert ~pos keyChar ti ast s
    (* list-specific insertions *)
    | K.RightCurlyBrace, _, R (TRecordClose _, ti) when pos = ti.endPos - 1 ->
        (* Allow pressing close curly to go over the last curly *)
        (ast, moveOneRight pos s)
    | K.RightSquareBracket, _, R (TListClose _, ti) when pos = ti.endPos - 1 ->
        (* Allow pressing close square to go over the last square *)
        (ast, moveOneRight pos s)
    (* String-specific insertions *)
    | K.DoubleQuote, _, R (TPatternString _, ti)
    | K.DoubleQuote, _, R (TString _, ti)
    | K.DoubleQuote, _, R (TStringMLEnd _, ti)
      when pos = ti.endPos - 1 ->
        (* Allow pressing quote to go over the last quote *)
        (ast, moveOneRight pos s)
    (* Field access *)
    | K.Period, L (TVariable _, toTheLeft), _
    | K.Period, L (TFieldName _, toTheLeft), _
      when onEdge ->
        doInsert ~pos keyChar toTheLeft ast s
    (***********)
    (* K.Enter *)
    (***********)
    (*
     * Caret to right of record open {
     * Add new initial record row and move caret to it. *)
    | K.Enter, L (TRecordOpen id, _), _ ->
        let ast = addRecordRowAt 0 id ast in
        let s = moveToAstRef s ast (ARRecord (id, RPFieldname 0)) in
        (ast, s)
    (*
     * Caret to left of record close }
     * Add new final record but leave caret to left of } *)
    | K.Enter, _, R (TRecordClose id, _) ->
        let s = recordAction "addRecordRowToBack" s in
        let ast = addRecordRowToBack id ast in
        let s = moveToAstRef s ast (ARRecord (id, RPClose)) in
        (ast, s)
    (*
     * Caret between pipe symbol |> and following expression.
     * Move current pipe expr down by adding new expr above it.
     * Keep caret "the same", only moved down by 1 column. *)
    | K.Enter, L (TPipe (id, idx, _), _), R _ ->
        let ast, s, _ = addPipeExprAt id (idx + 1) ast s in
        let s =
          moveToAstRef s ast (ARPipe (id, PPPipeKeyword (idx + 1))) ~offset:2
        in
        (ast, s)
    (*
     * Caret on end-of-line.
     * Following newline contains a parent and index, meaning we're inside some
     * special construct. Special-case each of those. *)
    | K.Enter, _, R (TNewline (Some (_, parentId, Some idx)), ti) ->
      ( match findExpr parentId ast with
      | Some (EPipe _) ->
          let ast, s, blankId = addPipeExprAt parentId (idx + 1) ast s in
          let s =
            moveToCaretTarget s ast (caretTargetForBeginningOfExpr blankId ast)
          in
          (ast, s)
      | Some (ERecord _) ->
          let ast = addRecordRowAt idx parentId ast in
          let s = moveToAstRef s ast (ARRecord (parentId, RPFieldname idx)) in
          (ast, s)
      | Some (EMatch _) ->
          let ast, s = addMatchPatternAt parentId idx ast s in
          let s =
            moveToAstRef s ast (ARMatch (parentId, MPBranchPattern idx))
          in
          (ast, s)
      | Some (EFnCall _) ->
          (* Pressing enter at the end of an FnCall's expression should just
           * move right. We don't know what's next, so we just want to
           * literally go to whatever's on the other side of the newline.
           * *)
          (ast, doRight ~pos ~next:mNext ti s)
      | _ ->
          (ast, s) )
    (*
     * Caret at end of line with nothing in newline. *)
    | K.Enter, L (lt, lti), R (TNewline None, rti)
      when not (Token.isLet lt || isAutocompleting rti s || isInIfCondition lt)
      ->
        wrapInLet lti ast s
    (*
     * Caret at end-of-line with no data in the TNewline.
     * Just move right (ie, * to beginning of next line) *)
    | K.Enter, L _, R (TNewline None, ti) ->
        (ast, doRight ~pos ~next:mNext ti s)
    (*
     * Caret at end-of-line generally adds a let on the line below,
     * unless the next line starts with a blank, in which case we go to it. *)
    | K.Enter, L _, R (TNewline (Some (id, _, _)), ti) ->
        if mNext
           |> Option.map ~f:(fun n ->
                  match n.token with TBlank _ -> true | _ -> false )
           |> Option.withDefault ~default:false
        then (ast, doRight ~pos ~next:mNext ti s)
        else
          let ast, s, letId = makeIntoLetBody id ast s in
          let s = moveToAstRef s ast (ARLet (letId, LPVarName)) in
          (ast, s)
    (*
     * Caret at beginning of special line.
     * Preceding newline contains a parent and index, meaning we're inside some
     * special construct. Special-case each of those.
     *
     * In the special case of the special case where we're actually at the
     * beginning of the next line _following_ the construct, then we actually
     * want to add a let, not continue the construct. These are the index vs
     * length checks in each case.
     *
     * Keep in mind this is the newline that _ends the previous line_, which means
     * in each case the idx is one more than the number of elements in the construct.
     * Eg, a match with 2 rows will have idx=3 here.
     *)
    | K.Enter, L (TNewline (Some (_, parentId, Some idx)), _), R (rTok, _) ->
        let addLetAboveRightToken () : ast * state =
          let id = FluidToken.tid rTok in
          let ast, s, _ = makeIntoLetBody id ast s in
          let s =
            moveToCaretTarget s ast (caretTargetForBeginningOfExpr id ast)
          in
          (ast, s)
        in
        ( match findExpr parentId ast with
        | Some (EMatch (_, _, exprs)) ->
            (* if a match has n rows, the last newline has idx=(n+1) *)
            if idx = List.length exprs
            then addLetAboveRightToken ()
            else
              let ast, s = addMatchPatternAt parentId idx ast s in
              let ref = ARMatch (parentId, MPBranchPattern (idx + 1)) in
              let s = moveToAstRef s ast ref in
              (ast, s)
        | Some (EPipe (_, exprs)) ->
            (* exprs[0] is the initial value of the pipeline, but the indexing
             * is zero-based starting at exprs[1] (it indexes the _pipes
             * only_), so need idx+1 here to counteract. *)
            if idx + 1 = List.length exprs
            then addLetAboveRightToken ()
            else
              let ast, s, _ = addPipeExprAt parentId (idx + 1) ast s in
              let s =
                moveToAstRef s ast (ARPipe (parentId, PPPipeKeyword (idx + 1)))
              in
              (ast, s)
        | Some (ERecord _) ->
            (* No length special-case needed because records do not emit a
             * TNewline with index after the final '}'. In this case, we
             * actually hit the next match case instead. *)
            let ast = addRecordRowAt idx parentId ast in
            let s =
              moveToAstRef s ast (ARRecord (parentId, RPFieldname (idx + 1)))
            in
            (ast, s)
        | _ ->
            (ast, s) )
    (*
     * Caret at very beginning of tokens or at beginning of non-special line. *)
    | K.Enter, No, R (t, _) | K.Enter, L (TNewline _, _), R (t, _) ->
        let id = FluidToken.tid t in
        let ast, s, _ = makeIntoLetBody id ast s in
        let s =
          moveToCaretTarget s ast (caretTargetForBeginningOfExpr id ast)
        in
        (ast, s)
    (*
     * Caret at very end of tokens where last line is non-let expression. *)
    | K.Enter, L (token, ti), No when not (Token.isLet token) ->
        wrapInLet ti ast s
    (****************)
    (* Int to float *)
    (****************)
    | K.Period, L (TInteger (id, _), ti), _ ->
        let offset = pos - ti.startPos in
        (convertIntToFloat offset id ast, moveOneRight pos s)
    | K.Period, L (TPatternInteger (mID, id, _, _), ti), _ ->
        let offset = pos - ti.startPos in
        (convertPatternIntToFloat offset mID id ast, moveOneRight pos s)
    (* Skipping over specific characters, this must come before the
     * more general binop cases or we lose the jumping behaviour *)
    | K.Equals, _, R (TLetAssignment _, toTheRight) ->
        (ast, moveTo toTheRight.endPos s)
    | K.Colon, _, R (TRecordSep _, toTheRight) ->
        (ast, moveTo toTheRight.endPos s)
    (* Binop specific, all of the specific cases must come before the
     * big general `key, L (_, toTheLeft), _` case. *)
    | _, L (TPartial _, toTheLeft), _
    | _, L (TRightPartial _, toTheLeft), _
    | _, L (TBinOp _, toTheLeft), _
      when keyIsInfix ->
        doInsert ~pos keyChar toTheLeft ast s
    | _, _, R (TBlank _, toTheRight) when keyIsInfix ->
        doInsert ~pos keyChar toTheRight ast s
    | _, L (_, toTheLeft), _
      when onEdge && keyIsInfix && wrappableInBinop toTheRight ->
        ( convertToBinOp keyChar (Token.tid toTheLeft.token) ast
        , s |> moveTo (pos + 2) )
    (* Rest of Insertions *)
    | _, L (TListOpen _, toTheLeft), R (TListClose _, _) ->
        doInsert ~pos keyChar toTheLeft ast s
    (*
     * Caret between empty record symbols {}
     * Adds new initial record row with the typed
     * value as the key (if value entered is valid),
     * then move caret to end of key *)
    | _, L (TRecordOpen id, _), R (TRecordClose _, _) ->
      ( match keyChar with
      | Some keyCharStr when isIdentifierChar (String.fromChar keyCharStr) ->
          let letterSTr = String.fromChar keyCharStr in
          let ast = addRecordRowAt ~letter:letterSTr 0 id ast in
          let s =
            moveToAstRef s ast (ARRecord (id, RPFieldname 0)) ~offset:1
          in
          (ast, s)
      | _ ->
          (ast, s) )
    | _, L (_, toTheLeft), _ when Token.isAppendable toTheLeft.token ->
        doInsert ~pos keyChar toTheLeft ast s
    | _, _, R (TListOpen _, _) ->
        (ast, s)
    | _, _, R (TRecordOpen _, _) ->
        (ast, s)
    | _, _, R (_, toTheRight) ->
        doInsert ~pos keyChar toTheRight ast s
    | _ ->
        (* Unknown *)
        (ast, report ("Unknown action: " ^ K.toName key) s)
  in
  let newAST, newState =
    (* This is a hack to make Enter create a new entry in matches and pipes
     * at the end of an AST. Matches/Pipes generate newlines at the end of
     * the canvas: we don't want those newlines to appear in editor, however,
     * we also can't get rid of them because it's a significant challenge to
     * know what to do in those cases without that information. So instead, we
     * allow them to be created and deal with the consequences.
     *
     * They don't appear in the browser, so we can ignore that.
     *
     * The major consequence is that there is an extra space at the end of the
     * AST (the one after the newline). Users can put their cursor all the way
     * to the end of the AST, and then they press left and the cursor doesn't
     * move (since the browser doesn't display the final newline, the cursor
     * goes to the same spot).
     *
     * We handle this by checking if we're in that situation and moving the
     * cursor back to the right place if so.
     *
     * TODO: there may be ways of getting the cursor to the end without going
     * through this code, if so we need to move it. *)
    let tokens = toTokens newState newAST in
    let text = tokensToString tokens in
    let last = List.last tokens in
    match last with
    | Some {token = TNewline _} when String.length text = newState.newPos ->
        (newAST, {newState with newPos = newState.newPos - 1})
    | _ ->
        (newAST, newState)
  in
  (* If we were on a partial and have moved off it, we may want to commit that
   * partial. This is done here because the logic is different that clicking.
   *
   * We "commit the partial" using the old state, and then we do the action
   * again to make sure we go to the right place for the new canvas. *)
  if recursing
  then (newAST, newState)
  else
    match (toTheLeft, toTheRight) with
    | L (TPartial (_, str), ti), _
    | L (TFieldPartial (_, _, _, str), ti), _
    | _, R (TPartial (_, str), ti)
    | _, R (TFieldPartial (_, _, _, str), ti)
    (* When pressing an infix character, it's hard to tell whether to commit or
     * not.  If the partial is an int, or a function that returns one, pressing
     * +, -, etc  should lead to committing and then doing the action.
     *
     * However, if the partial is a valid function such as +, then pressing +
     * again could be an attempt to make ++, not `+ + ___`.
     *
     * So if the new function _could_ be valid, don't commit. *)
      when key = K.Right || key = K.Left || keyIsInfix ->
        let shouldCommit =
          match keyChar with
          | None ->
              true
          | Some keyChar ->
              let newQueryString = str ^ String.fromChar keyChar in
              s.ac.allCompletions
              |> List.filter ~f:(fun aci ->
                     String.contains ~substring:newQueryString (AC.asName aci)
                 )
              |> ( == ) []
        in
        if shouldCommit
        then
          let committedAST = commitIfValid newState.newPos ti (newAST, s) in
          updateKey
            ~recursing:true
            key
            committedAST
            (* keep the actions for debugging *)
            {s with actions = newState.actions}
        else (newAST, newState)
    | _ ->
        (newAST, newState)


(* deleteCaretRange is equivalent to pressing backspace starting from the larger of the two caret positions
   until the caret reaches the smaller of the caret positions or can no longer move.
   XXX(JULIAN): This actually moves the caret to the larger side of the range and backspaces until the
   beginning, which means this hijacks the caret in the state. *)
and deleteCaretRange ~state ~ast (caretRange : int * int) : ast * fluidState =
  let rangeStart, rangeEnd = caretRange |> orderRangeFromSmallToBig in
  let state =
    {state with newPos = rangeEnd; oldPos = state.newPos; selectionStart = None}
  in
  let currAst, currState = (ref ast, ref state) in
  let nothingChanged = ref false in
  while (not !nothingChanged) && !currState.newPos > rangeStart do
    let newAst, newState = updateKey K.Backspace !currAst !currState in
    if newState.newPos = !currState.newPos && newAst = !currAst
    then
      (* stop if nothing changed--guarantees loop termination *)
      nothingChanged := true
    else (
      currAst := newAst ;
      currState := newState )
  done ;
  (!currAst, !currState)


(* deleteSelection is equivalent to pressing backspace starting from the larger of the two caret positions
   forming the selection until the caret reaches the smaller of the caret positions or can no longer move. *)
and deleteSelection ~state ~ast : ast * fluidState =
  fluidGetSelectionRange state |> deleteCaretRange ~state ~ast


let getToken (s : fluidState) (ast : fluidExpr) : tokenInfo option =
  let tokens = toTokens s ast in
  let toTheLeft, toTheRight, _ = getNeighbours ~pos:s.newPos tokens in
  (* The algorithm that decides what token on when a certain key is pressed is
   * in updateKey. It's pretty complex and it tells us what token a keystroke
   * should apply to. For all other places that need to know what token we're
   * on, this attemps to approximate that.

   * The cursor at newPos is either in a token (eg 3 chars into "myFunction"),
   * or between two tokens (eg 1 char into "4 + 2").

   * If we're between two tokens, we decide by looking at whether the left
   * token is a text token. If it is, it's likely that we're just typing.
   * Otherwise, the important token is probably the right token.
   *
   * Example: `4 + 2`, when the cursor is at position: 0): 4 is to the right,
   * nothing to the left. Choose 4 1): 4 is a text token to the left, choose 4
   * 2): the token to the left is not a text token (it's a TSep), so choose +
   * 3): + is a text token to the left, choose + 4): 2 is to the right, nothing
   * to the left. Choose 2 5): 2 is a text token to the left, choose 2
   *
   * Reminder that this is an approximation. If we find bugs we may need to go
   * much deeper.
   *)
  match (toTheLeft, toTheRight) with
  | L (_, ti), _ when Token.isTextToken ti.token ->
      Some ti
  | _, R (_, ti) ->
      Some ti
  | L (_, ti), _ ->
      Some ti
  | _ ->
      None


let updateAutocomplete m tlid ast s : fluidState =
  match getToken s ast with
  | Some ti when Token.isAutocompletable ti.token ->
      let m = TL.withAST m tlid (toExpr ast) in
      let newAC = AC.regenerate m s.ac (tlid, ti) in
      {s with ac = newAC}
  | _ ->
      s


let updateMouseClick (newPos : int) (ast : ast) (s : fluidState) :
    ast * fluidState =
  let tokens = toTokens s ast in
  let lastPos =
    tokens
    |> List.last
    |> Option.map ~f:(fun ti -> ti.endPos)
    |> Option.withDefault ~default:0
  in
  let newPos = if newPos > lastPos then lastPos else newPos in
  let newPos =
    (* TODO: add tests for clicking in the middle of a pipe (or blank) *)
    match getLeftTokenAt newPos tokens with
    | Some current when Token.isBlank current.token ->
        current.startPos
    | Some ({token = TPipe _} as current) ->
        current.endPos
    | _ ->
        newPos
  in
  let newAST = acMaybeCommit newPos ast s in
  (newAST, setPosition s newPos)


let shouldDoDefaultAction (key : K.key) : bool =
  match key with
  | K.GoToStartOfLine
  | K.GoToEndOfLine
  | K.Delete
  | K.SelectAll
  | K.DeleteToEndOfLine
  | K.DeleteToStartOfLine
  | K.GoToStartOfWord
  | K.GoToEndOfWord
  | K.DeletePrevWord
  | K.DeleteNextWord ->
      false
  | _ ->
      true


let exprRangeInAst ~state ~ast (exprID : id) : (int * int) option =
  (* get the beginning and end of the range from
    * the expression's first and last token
    * by cross-referencing the tokens it evaluates to via toTokens
    * with the tokens for the whole ast.
    *
    * This is preferred to just getting all the tokens with the same exprID
    * because the last expression in a token range
    * (e.g. a FnCall `Int::add 1 2`) might be for a sub-expression and have a
    * different ID, (in the above case the last token TInt(2) belongs to the
    * second sub-expr of the FnCall) *)
  let astTokens = toTokens state ast in
  let exprTokens =
    findExpr exprID ast
    |> Option.map ~f:(toTokens state)
    |> Option.withDefault ~default:[]
  in
  let exprStartToken, exprEndToken =
    (List.head exprTokens, List.last exprTokens)
    |> Tuple2.mapAll ~f:(function
           | Some exprTok ->
               List.find astTokens ~f:(fun astTok ->
                   exprTok.token = astTok.token )
           | _ ->
               None )
  in
  match (exprStartToken, exprEndToken) with
  (* range is from startPos of first token in expr to
    * endPos of last token in expr *)
  | Some {startPos}, Some {endPos} ->
      Some (startPos, endPos)
  | _ ->
      None


let getTokenRangeAtCaret (state : fluidState) (ast : ast) : (int * int) option
    =
  getToken state ast |> Option.map ~f:(fun t -> (t.startPos, t.endPos))


let getExpressionRangeAtCaret (state : fluidState) (ast : ast) :
    (int * int) option =
  getToken state ast
  (* get token that the cursor is currently on *)
  |> Option.andThen ~f:(fun t ->
         (* get expression that the token belongs to *)
         let exprID = Token.tid t.token in
         exprRangeInAst ~state ~ast exprID )
  |> Option.map ~f:(fun (eStartPos, eEndPos) -> (eStartPos, eEndPos))


let trimQuotes s : string =
  let open String in
  s
  |> fun v ->
  (if endsWith ~suffix:"\"" v then dropRight ~count:1 v else v)
  |> fun v -> if startsWith ~prefix:"\"" v then dropLeft ~count:1 v else v


let clone ~(state : state) (ast : fluidExpr) : fluidExpr =
  ast |> toExpr ~inPipe:false |> AST.clone |> fromExpr state


let reconstructExprFromRange ~state ~ast (range : int * int) : fluidExpr option
    =
  (* prevent duplicates *)
  let ast = clone ~state ast in
  (* a few helpers *)
  let toBool_ s =
    if s = "true"
    then true
    else if s = "false"
    then false
    else
      recover
        "string bool token should always be convertable to bool"
        ~debug:s
        false
  in
  let findTokenValue tokens tID typeName =
    List.find tokens ~f:(fun (tID', _, typeName') ->
        tID = tID' && typeName = typeName' )
    |> Option.map ~f:Tuple3.second
  in
  let startPos, endPos = orderRangeFromSmallToBig range in
  (* main main recursive algorithm *)
  (* algo:
    * find topmost expression by ID and
    * reconstruct full/subset of expression
    * recurse into children (that remain in subset) to reconstruct those too *)
  let rec reconstruct ~topmostID (startPos, endPos) : fluidExpr option =
    let topmostExpr =
      topmostID
      |> Option.andThen ~f:(fun id -> findExpr id ast)
      |> Option.withDefault ~default:(EBlank (gid ()))
    in
    let simplifiedTokens =
      (* simplify tokens to make them homogenous, easier to parse *)
      tokensInRange startPos endPos ~state ast
      |> List.map ~f:(fun ti ->
             let t = ti.token in
             let text =
               (* trim tokens if they're on the edge of the range *)
               Token.toText t
               |> String.dropLeft
                    ~count:
                      ( if ti.startPos < startPos
                      then startPos - ti.startPos
                      else 0 )
               |> String.dropRight
                    ~count:
                      (if ti.endPos > endPos then ti.endPos - endPos else 0)
               |> fun text ->
               (* if string, do extra trim to account for quotes, then re-append quotes *)
               if Token.toTypeName ti.token = "string"
               then "\"" ^ trimQuotes text ^ "\""
               else text
             in
             Token.(tid t, text, toTypeName t) )
    in
    let reconstructExpr expr : fluidExpr option =
      let exprID =
        match expr with EPipeTarget _ -> None | _ -> Some (eid expr)
      in
      exprID
      |> Option.andThen ~f:(exprRangeInAst ~state ~ast)
      |> Option.andThen ~f:(fun (exprStartPos, exprEndPos) ->
             (* ensure expression range is not totally outside selection range *)
             if exprStartPos > endPos || exprEndPos < startPos
             then None
             else Some (max exprStartPos startPos, min exprEndPos endPos) )
      |> Option.andThen ~f:(reconstruct ~topmostID:exprID)
    in
    let orDefaultExpr : fluidExpr option -> fluidExpr =
      Option.withDefault ~default:(EBlank (gid ()))
    in
    let id = gid () in
    match (topmostExpr, simplifiedTokens) with
    | _, [] ->
        None
    (* basic, single/fixed-token expressions *)
    | EInteger (eID, _), tokens ->
        findTokenValue tokens eID "integer"
        |> Option.map ~f:coerceStringTo63BitInt
        |> Option.map ~f:(fun v -> EInteger (gid (), v))
    | EBool (eID, value), tokens ->
        Option.or_
          (findTokenValue tokens eID "true")
          (findTokenValue tokens eID "false")
        |> Option.andThen ~f:(fun newValue ->
               if newValue = ""
               then None
               else if newValue <> string_of_bool value
               then Some (EPartial (gid (), newValue, EBool (id, value)))
               else Some (EBool (id, value)) )
    | ENull eID, tokens ->
        findTokenValue tokens eID "null"
        |> Option.map ~f:(fun newValue ->
               if newValue = "null"
               then ENull id
               else EPartial (gid (), newValue, ENull id) )
    | EString (eID, _), tokens ->
        let merged =
          tokens
          |> List.filter ~f:(fun (_, _, type_) ->
                 type_ <> "newline" && type_ <> "indent" )
          |> List.map ~f:Tuple3.second
          |> String.join ~sep:""
        in
        if merged = "" then None else Some (EString (eID, trimQuotes merged))
    | EFloat (eID, _, _), tokens ->
        let newWhole = findTokenValue tokens eID "float-whole" in
        let pointSelected = findTokenValue tokens eID "float-point" <> None in
        let newFraction = findTokenValue tokens eID "float-fraction" in
        ( match (newWhole, pointSelected, newFraction) with
        | Some value, true, None ->
            Some (EFloat (id, value, "0"))
        | Some value, false, None | None, false, Some value ->
            Some (EInteger (id, coerceStringTo63BitInt value))
        | None, true, Some value ->
            Some (EFloat (id, "0", value))
        | Some whole, true, Some fraction ->
            Some (EFloat (id, whole, fraction))
        | None, true, None ->
            Some (EFloat (id, "0", "0"))
        | _, _, _ ->
            None )
    | EBlank _, _ ->
        Some (EBlank id)
    (* empty let expr and subsets *)
    | ELet (eID, lhsID, _lhs, rhs, body), tokens ->
        let letKeywordSelected =
          findTokenValue tokens eID "let-keyword" <> None
        in
        let newLhs =
          findTokenValue tokens eID "let-lhs" |> Option.withDefault ~default:""
        in
        ( match (reconstructExpr rhs, reconstructExpr body) with
        | None, None when newLhs <> "" ->
            Some (EPartial (gid (), newLhs, EVariable (gid (), newLhs)))
        | None, Some e ->
            Some e
        | Some newRhs, None ->
            Some (ELet (id, lhsID, newLhs, newRhs, EBlank (gid ())))
        | Some newRhs, Some newBody ->
            Some (ELet (id, lhsID, newLhs, newRhs, newBody))
        | None, None when letKeywordSelected ->
            Some (ELet (id, lhsID, newLhs, EBlank (gid ()), EBlank (gid ())))
        | _, _ ->
            None )
    | EIf (eID, cond, thenBody, elseBody), tokens ->
        let ifKeywordSelected =
          findTokenValue tokens eID "if-keyword" <> None
        in
        let thenKeywordSelected =
          findTokenValue tokens eID "if-then-keyword" <> None
        in
        let elseKeywordSelected =
          findTokenValue tokens eID "if-else-keyword" <> None
        in
        ( match
            ( reconstructExpr cond
            , reconstructExpr thenBody
            , reconstructExpr elseBody )
          with
        | newCond, newThenBody, newElseBody
          when ifKeywordSelected || thenKeywordSelected || elseKeywordSelected
          ->
            Some
              (EIf
                 ( id
                 , newCond |> orDefaultExpr
                 , newThenBody |> orDefaultExpr
                 , newElseBody |> orDefaultExpr ))
        | Some e, None, None | None, Some e, None | None, None, Some e ->
            Some e
        | _ ->
            None )
    | EBinOp (eID, name, expr1, expr2, ster), tokens ->
        let newName =
          findTokenValue tokens eID "binop" |> Option.withDefault ~default:""
        in
        ( match (reconstructExpr expr1, reconstructExpr expr2) with
        | Some newExpr1, Some newExpr2 when newName = "" ->
          (* since we don't allow empty partials, reconstruct the binop as we would when
           * the binop is manually deleted
           * (by elevating the argument expressions into ELets provided they aren't blanks) *)
          ( match (newExpr1, newExpr2) with
          | EBlank _, EBlank _ ->
              None
          | EBlank _, e | e, EBlank _ ->
              Some (ELet (gid (), gid (), "", e, EBlank (gid ())))
          | e1, e2 ->
              Some
                (ELet
                   ( gid ()
                   , gid ()
                   , ""
                   , e1
                   , ELet (gid (), gid (), "", e2, EBlank (gid ())) )) )
        | None, Some e ->
            let e = EBinOp (id, name, EBlank (gid ()), e, ster) in
            if newName = ""
            then None
            else if name <> newName
            then Some (EPartial (gid (), newName, e))
            else Some e
        | Some e, None ->
            let e = EBinOp (id, name, e, EBlank (gid ()), ster) in
            if newName = ""
            then None
            else if name <> newName
            then Some (EPartial (gid (), newName, e))
            else Some e
        | Some newExpr1, Some newExpr2 ->
            let e = EBinOp (id, name, newExpr1, newExpr2, ster) in
            if newName = ""
            then None
            else if name <> newName
            then Some (EPartial (gid (), newName, e))
            else Some e
        | None, None when newName <> "" ->
            let e =
              EBinOp (id, name, EBlank (gid ()), EBlank (gid ()), ster)
            in
            if newName = ""
            then None
            else if name <> newName
            then Some (EPartial (gid (), newName, e))
            else Some e
        | _, _ ->
            None )
    | ELambda (eID, _, body), tokens ->
        (* might be an edge case here where one of the vars is not (fully) selected but
         * is still bound in the body, would be worth turning the EVars in the body to partials somehow *)
        let newVars =
          (* get lambda-var tokens that belong to this expression
           * out of the list of tokens in the selection range *)
          tokens
          |> List.filterMap ~f:(function
                 | vID, value, "lambda-var" when vID = eID ->
                     Some (gid (), value)
                 | _ ->
                     None )
        in
        Some (ELambda (id, newVars, reconstructExpr body |> orDefaultExpr))
    | EFieldAccess (eID, e, _, _), tokens ->
        let newFieldName =
          findTokenValue tokens eID "field-name"
          |> Option.withDefault ~default:""
        in
        let fieldOpSelected = findTokenValue tokens eID "field-op" <> None in
        let e = reconstructExpr e in
        ( match (e, fieldOpSelected, newFieldName) with
        | None, false, newFieldName when newFieldName != "" ->
            Some
              (EPartial (gid (), newFieldName, EVariable (gid (), newFieldName)))
        | None, true, newFieldName when newFieldName != "" ->
            Some (EFieldAccess (id, EBlank (gid ()), gid (), newFieldName))
        | Some e, true, _ ->
            Some (EFieldAccess (id, e, gid (), newFieldName))
        | _ ->
            e )
    | EVariable (eID, value), tokens ->
        let newValue =
          findTokenValue tokens eID "variable"
          |> Option.withDefault ~default:""
        in
        let e = EVariable (id, value) in
        if newValue = ""
        then None
        else if value <> newValue
        then Some (EPartial (gid (), newValue, e))
        else Some e
    | EFnCall (eID, fnName, args, ster), tokens ->
        let newArgs =
          match args with
          | EPipeTarget _ :: args ->
              EPipeTarget (gid ())
              :: List.map args ~f:(reconstructExpr >> orDefaultExpr)
          | _ ->
              List.map args ~f:(reconstructExpr >> orDefaultExpr)
        in
        let newFnName =
          findTokenValue tokens eID "fn-name" |> Option.withDefault ~default:""
        in
        let newFnVersion =
          findTokenValue tokens eID "fn-version"
          |> Option.withDefault ~default:""
        in
        let newFnName =
          if newFnVersion = ""
          then newFnName
          else newFnName ^ "_" ^ newFnVersion
        in
        let e = EFnCall (id, fnName, newArgs, ster) in
        if newFnName = ""
        then None
        else if fnName <> newFnName
        then Some (EPartial (gid (), newFnName, e))
        else Some e
    | EPartial (eID, _, expr), tokens ->
        let expr = reconstructExpr expr |> orDefaultExpr in
        let newName =
          findTokenValue tokens eID "partial" |> Option.withDefault ~default:""
        in
        Some (EPartial (id, newName, expr))
    | ERightPartial (eID, _, expr), tokens ->
        let expr = reconstructExpr expr |> orDefaultExpr in
        let newName =
          findTokenValue tokens eID "partial-right"
          |> Option.withDefault ~default:""
        in
        Some (ERightPartial (id, newName, expr))
    | EList (_, exprs), _ ->
        let newExprs = List.map exprs ~f:reconstructExpr |> Option.values in
        Some (EList (id, newExprs))
    | ERecord (id, entries), _ ->
        let newEntries =
          (* looping through original set of tokens (before transforming them into tuples)
           * so we can get the index field *)
          tokensInRange startPos endPos ~state ast
          |> List.filterMap ~f:(fun ti ->
                 match ti.token with
                 | TRecordFieldname {recordID; index; fieldName = newKey}
                   when recordID = id (* watch out for nested records *) ->
                     List.getAt ~index entries
                     |> Option.map
                          ~f:
                            (Tuple3.mapEach
                               ~f:identity (* ID stays the same *)
                               ~g:(fun _ -> newKey) (* replace key *)
                               ~h:
                                 (reconstructExpr >> orDefaultExpr)
                                 (* reconstruct value expression *))
                 | _ ->
                     None )
        in
        Some (ERecord (id, newEntries))
    | EPipe (_, exprs), _ ->
        let newExprs =
          List.map exprs ~f:reconstructExpr
          |> Option.values
          |> function
          | [] ->
              [EBlank (gid ()); EBlank (gid ())]
          | [expr] ->
              [expr; EBlank (gid ())]
          | exprs ->
              exprs
        in
        Some (EPipe (id, newExprs))
    | EConstructor (eID, _, name, exprs), tokens ->
        let newName =
          findTokenValue tokens eID "constructor-name"
          |> Option.withDefault ~default:""
        in
        let newExprs = List.map exprs ~f:(reconstructExpr >> orDefaultExpr) in
        let e = EConstructor (id, gid (), name, newExprs) in
        if newName = ""
        then None
        else if name <> newName
        then Some (EPartial (gid (), newName, e))
        else Some e
    | EMatch (mID, cond, patternsAndExprs), tokens ->
        let newPatternAndExprs =
          List.map patternsAndExprs ~f:(fun (pattern, expr) ->
              let toksToPattern tokens pID =
                match
                  tokens |> List.filter ~f:(fun (pID', _, _) -> pID = pID')
                with
                | [(id, _, "pattern-blank")] ->
                    FPBlank (mID, id)
                | [(id, value, "pattern-integer")] ->
                    FPInteger (mID, id, coerceStringTo63BitInt value)
                | [(id, value, "pattern-variable")] ->
                    FPVariable (mID, id, value)
                | (id, value, "pattern-constructor-name") :: _subPatternTokens
                  ->
                    (* temporarily assuming that FPConstructor's sub-pattern tokens are always copied as well*)
                    FPConstructor
                      ( mID
                      , id
                      , value
                      , match pattern with
                        | FPConstructor (_, _, _, ps) ->
                            ps
                        | _ ->
                            [] )
                | [(id, "pattern-string", value)] ->
                    FPString (mID, id, value)
                | [(id, value, "pattern-true")] | [(id, value, "pattern-false")]
                  ->
                    FPBool (mID, id, toBool_ value)
                | [(id, _, "pattern-null")] ->
                    FPNull (mID, id)
                | [ (id, whole, "pattern-float-whole")
                  ; (_, _, "pattern-float-point")
                  ; (_, fraction, "pattern-float-fraction") ] ->
                    FPFloat (mID, id, whole, fraction)
                | [ (id, value, "pattern-float-whole")
                  ; (_, _, "pattern-float-point") ]
                | [(id, value, "pattern-float-whole")] ->
                    FPInteger (mID, id, coerceStringTo63BitInt value)
                | [ (_, _, "pattern-float-point")
                  ; (id, value, "pattern-float-fraction") ]
                | [(id, value, "pattern-float-fraction")] ->
                    FPInteger (mID, id, coerceStringTo63BitInt value)
                | _ ->
                    FPBlank (mID, gid ())
              in
              let newPattern = toksToPattern tokens (pid pattern) in
              (newPattern, reconstructExpr expr |> orDefaultExpr) )
        in
        Some
          (EMatch
             (id, reconstructExpr cond |> orDefaultExpr, newPatternAndExprs))
    | EFeatureFlag (_, name, nameID, cond, thenBody, elseBody), _ ->
        (* since we don't have any tokens associated with feature flags yet *)
        Some
          (EFeatureFlag
             ( id
             , (* should probably do some stuff about if the name token isn't fully selected *)
               name
             , nameID
             , reconstructExpr cond |> orDefaultExpr
             , reconstructExpr thenBody |> orDefaultExpr
             , reconstructExpr elseBody |> orDefaultExpr ))
    (* Unknowns:
     * - EPipeTarget: assuming it can't be selected since it doesn't produce tokens
     * - EOldExpr: going to ignore the "TODO: oldExpr" and assume it's a blank *)
    | _, _ ->
        Some (EBlank (gid ()))
  in
  let topmostID = getTopmostSelectionID startPos endPos ~state ast in
  reconstruct ~topmostID (startPos, endPos)


let exprToClipboardContents (ast : fluidExpr) : clipboardContents =
  match ast with
  | EString (_, str) ->
      `Text str
  | _ ->
      `Json (Encoders.pointerData (PExpr (toExpr ast)))


let jsonToExpr (jsonStr : string) : fluidExpr =
  let open Js.Json in
  let rec jsJsonToExpr (j : t) : fluidExpr =
    match classify j with
    | JSONString str ->
        EString (gid (), str)
    | JSONFalse ->
        EBool (gid (), false)
    | JSONTrue ->
        EBool (gid (), true)
    | JSONNull ->
        ENull (gid ())
    | JSONNumber float ->
        let str = Js.Float.toString float in
        if is63BitInt str
        then EInteger (gid (), str)
        else if Regex.exactly ~re:"[0-9]+\\.[0-9]+" str
        then
          match String.split ~on:"." str with
          | [whole; fraction] ->
              EFloat (gid (), whole, fraction)
          | _ ->
              recover
                "invalid float passed the regex"
                ~debug:str
                (EInteger (gid (), "0"))
        else
          (* TODO: support floats in the format 3.4e5 *)
          recover
            "unsupported float in json"
            ~debug:str
            (EInteger (gid (), "0"))
    | JSONObject dict ->
        dict
        |> Js_dict.entries
        |> Array.toList
        |> List.map ~f:(fun (k, json) -> (gid (), k, jsJsonToExpr json))
        |> fun fields -> ERecord (gid (), fields)
    | JSONArray arr ->
        arr
        |> Array.toList
        |> List.map ~f:jsJsonToExpr
        |> fun exprs -> EList (gid (), exprs)
  in
  try
    let j = Json.parseOrRaise jsonStr in
    jsJsonToExpr j
  with _ -> EString (gid (), jsonStr)


let clipboardContentsToExpr ~state (data : clipboardContents) :
    fluidExpr option =
  match data with
  | `Json json ->
    ( try
        let data = Decoders.pointerData json |> TL.clonePointerData in
        match data with
        | PExpr expr ->
            Some (fromExpr state expr)
        | _ ->
            (* We could support more but don't yet *)
            recover "not a pexpr" ~debug:data None
      with _ -> recover "could not decode" ~debug:json None )
  | `Text text ->
      Some (jsonToExpr text)
  | `None ->
      None


let clipboardContentsToString ~state (data : clipboardContents) : string =
  match data with
  | `Json _ ->
      data
      |> clipboardContentsToExpr ~state
      |> Option.map ~f:(eToString state)
      |> Option.withDefault ~default:""
      |> trimQuotes
  | `Text text ->
      text
  | `None ->
      ""


let getStringIndex ti pos : int =
  match ti.token with
  | TString (_, _) ->
      pos - ti.startPos - 1
  | TStringMLStart (_, _, offset, _) ->
      pos - ti.startPos + offset - 1
  | TStringMLMiddle (_, _, offset, _) | TStringMLEnd (_, _, offset, _) ->
      pos - ti.startPos + offset
  | _ ->
      recover "getting index of non-string" ~debug:(ti.token, pos) 0


let pasteOverSelection ~state ~ast data : ast * fluidState =
  let ast, state = deleteSelection ~state ~ast in
  let token = getToken state ast in
  let exprID = token |> Option.map ~f:(fun ti -> ti.token |> Token.tid) in
  let expr = Option.andThen exprID ~f:(fun id -> findExpr id ast) in
  let collapsedSelStart = fluidGetCollapsedSelectionStart state in
  let clipboardExpr = clipboardContentsToExpr ~state data in
  let text = clipboardContentsToString ~state data in
  match (clipboardExpr, expr, token) with
  | Some clipboardExpr, Some (EBlank exprID), _ ->
      let newPos =
        (clipboardExpr |> eToString state |> String.length) + collapsedSelStart
      in
      (replaceExpr ~newExpr:clipboardExpr exprID ast, {state with newPos})
  (* inserting record key (record expression with single key and no value) into string *)
  | ( Some (ERecord (_, [(_, insert, EBlank _)]))
    , Some (EString (_, str))
    , Some {startPos} ) ->
      let index = state.newPos - startPos - 1 in
      let newExpr = EString (gid (), String.insertAt ~insert ~index str) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length insert})
  (* inserting other kinds of expressions into string *)
  | _, Some (EString (_, str)), Some ti ->
      let index = getStringIndex ti state.newPos in
      let newExpr =
        EString (gid (), String.insertAt ~insert:text ~index str)
      in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (* TODO: needs reflow: if the string becomes multi-line, we end up in the wrong place. *)
      let newPos = state.newPos + String.length text in
      (newAST, {state with newPos})
  (* inserting integer into another integer *)
  | ( Some (EInteger (_, clippedInt))
    , Some (EInteger (_, pasting))
    , Some {startPos} ) ->
      let index = state.newPos - startPos in
      let insert = clippedInt in
      let newVal =
        String.insertAt ~insert ~index pasting |> coerceStringTo63BitInt
      in
      let newExpr = EInteger (gid (), newVal) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length insert})
  (* inserting float into an integer *)
  | ( Some (EFloat (_, whole, fraction))
    , Some (EInteger (_, pasting))
    , Some {startPos} ) ->
      let whole', fraction' =
        let str = pasting in
        String.
          ( slice ~from:0 ~to_:(state.newPos - startPos) str
          , slice ~from:(state.newPos - startPos) ~to_:(String.length str) str
          )
      in
      let newExpr = EFloat (gid (), whole' ^ whole, fraction ^ fraction') in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      ( newAST
      , { state with
          newPos = collapsedSelStart + (whole ^ "." ^ fraction |> String.length)
        } )
  (* inserting variable into an integer *)
  | Some (EVariable (_, varName)), Some (EInteger (_, intVal)), Some {startPos}
    ->
      let index = state.newPos - startPos in
      let newVal = String.insertAt ~insert:varName ~index intVal in
      let newExpr = EPartial (gid (), newVal, EVariable (gid (), newVal)) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length varName})
  (* inserting int-only string into an integer *)
  | Some (EString (_, insert)), Some (EInteger (_, pasting)), Some {startPos}
    when String.toInt insert |> Result.toOption <> None ->
      let index = state.newPos - startPos in
      let newVal =
        String.insertAt ~insert ~index pasting |> coerceStringTo63BitInt
      in
      let newExpr = EInteger (gid (), newVal) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length insert})
  (* inserting integer into a float whole *)
  | ( Some (EInteger (_, intVal))
    , Some (EFloat (_, whole, fraction))
    , Some {startPos; token = TFloatWhole _} ) ->
      let index = state.newPos - startPos in
      let newExpr =
        EFloat (gid (), String.insertAt ~index ~insert:intVal whole, fraction)
      in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length intVal})
  (* inserting integer into a float fraction *)
  | ( Some (EInteger (_, intVal))
    , Some (EFloat (_, whole, fraction))
    , Some {startPos; token = TFloatFraction _} ) ->
      let index = state.newPos - startPos in
      let newExpr =
        EFloat (gid (), whole, String.insertAt ~index ~insert:intVal fraction)
      in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length intVal})
  (* inserting integer after float point *)
  | ( Some (EInteger (_, intVal))
    , Some (EFloat (_, whole, fraction))
    , Some {token = TFloatPoint _} ) ->
      let newExpr = EFloat (gid (), whole, intVal ^ fraction) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length intVal})
  (* inserting variable into let LHS *)
  | ( Some (EVariable (_, varName))
    , Some (ELet (_, _, lhs, rhs, body))
    , Some {startPos; token = TLetLHS _} ) ->
      let index = state.newPos - startPos in
      let newLhs =
        if lhs <> ""
        then String.insertAt ~insert:varName ~index lhs
        else varName
      in
      let newExpr = ELet (gid (), gid (), newLhs, rhs, body) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (newAST, {state with newPos = collapsedSelStart + String.length varName})
  (* inserting list expression into another list at separator *)
  | ( Some (EList (_, itemsToPaste) as exprToPaste)
    , Some (EList (_, items))
    , Some {token = TListSep (_, index)} ) ->
      let newItems =
        let front, back = List.splitAt ~index items in
        front @ itemsToPaste @ back
      in
      let newExpr = EList (gid (), newItems) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      ( newAST
      , { state with
          newPos =
            collapsedSelStart + String.length (eToString state exprToPaste) }
      )
  (* inserting other expressions into list *)
  | ( Some exprToPaste
    , Some (EList (_, items))
    , Some {token = TListSep (_, index)} ) ->
      let newItems = List.insertAt ~value:exprToPaste ~index items in
      let newExpr = EList (gid (), newItems) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      ( newAST
      , { state with
          newPos =
            collapsedSelStart + String.length (eToString state exprToPaste) }
      )
  (* TODO:
   * - inserting pipe after expression
   * - *)
  | _ ->
      (ast, state)


let fluidDataFromModel m : (fluidState * fluidExpr) option =
  match Toplevel.selectedAST m with
  | Some expr ->
      let s = m.fluidState in
      Some (s, fromExpr s expr)
  | None ->
      None


let getCopySelection (m : model) : clipboardContents =
  match fluidDataFromModel m with
  | Some (state, ast) ->
      fluidGetOptionalSelectionRange state
      |> Option.andThen ~f:(reconstructExprFromRange ~state ~ast)
      |> Option.map ~f:exprToClipboardContents
      |> Option.withDefault ~default:`None
  | None ->
      `None


let updateMouseUp (s : state) (ast : ast) (selection : (int * int) option) =
  let s = {s with midClick = false} in
  let selection =
    Option.orElseLazy (fun () -> Entry.getFluidSelectionRange ()) selection
  in
  let ast, s =
    match selection with
    (* if range width is 0, just change pos *)
    | Some (selBegin, selEnd) when selBegin = selEnd ->
        updateMouseClick selBegin ast s
    | Some (selBegin, selEnd) ->
        ( ast
        , { s with
            selectionStart = Some selBegin; oldPos = s.newPos; newPos = selEnd
          } )
    | None ->
        (ast, {s with selectionStart = None})
  in
  (* We reset the fluidState to prevent the selection and/or cursor
         * position from persisting when a user switched handlers *)
  (ast, acClear s)


let updateMsg m tlid (ast : ast) (msg : Types.fluidMsg) (s : fluidState) :
    ast * fluidState =
  (* TODO: The state should be updated from the last request, and so this
   * shouldn't be necessary, but the tests don't work without it *)
  let s = updateAutocomplete m tlid ast s in
  let newAST, newState =
    match msg with
    | FluidUpdateAutocomplete ->
        (* updateAutocomplete has already been run, so nothing more to do *)
        (ast, s)
    | FluidMouseUp (_, selection) ->
        updateMouseUp s ast selection
    | FluidCut ->
        deleteSelection ~state:s ~ast
    | FluidPaste data ->
        let ast, state = pasteOverSelection ~state:s ~ast data in
        (ast, updateAutocomplete m tlid ast state)
    (* handle selection with direction key cases *)
    (* - moving/selecting over expressions or tokens with shift-/alt-direction or shift-/ctrl-direction *)
    | FluidKeyPress {key; (* altKey; ctrlKey; metaKey; *) shiftKey = true}
      when key = K.Right || key = K.Left || key = K.Up || key = K.Down ->
        (* Ultimately, all we want is for shift to move the end of the selection to where the caret would have been if shift were not held.
         * Since the caret is tracked the same for end of selection and movement, we actually just want to store the start position in selection
         * if there is no selection yet.
         *)
        (* TODO(JULIAN): We need to refactor updateKey and key handling in general so that modifiers compose more easily with shift *)
        (* XXX(JULIAN): We need to be able to use alt and ctrl and meta to change selection! *)
        let ast, newS = updateKey key ast s in
        ( match s.selectionStart with
        | None ->
            ( ast
            , {newS with newPos = newS.newPos; selectionStart = Some s.newPos}
            )
        | Some pos ->
            (ast, {newS with newPos = newS.newPos; selectionStart = Some pos})
        )
    | FluidKeyPress {key; (* altKey; ctrlKey; metaKey; *) shiftKey = false}
      when s.selectionStart <> None && (key = K.Right || key = K.Left) ->
        (* Aborting a selection using the left and right arrows should
         place the caret on the side of the selection in the direction
         of the pressed arrow key *)
        let newPos =
          let left, right =
            fluidGetSelectionRange s |> orderRangeFromSmallToBig
          in
          if key = K.Left then left else right
        in
        (ast, {s with lastKey = key; newPos; selectionStart = None})
    | FluidKeyPress {key; altKey; metaKey; ctrlKey}
      when (altKey || metaKey || ctrlKey) && shouldDoDefaultAction key ->
        (* To make sure no letters are entered if user is doing a browser default action *)
        (ast, s)
    | FluidKeyPress {key; shiftKey} ->
        let s = {s with lastKey = key} in
        let newAST, newState = updateKey key ast s in
        let selectionStart =
          if key = K.SelectAll
          then newState.selectionStart
          else if shiftKey && not (key = K.ShiftEnter)
                  (* We dont want to persist selection on ShiftEnter *)
          then s.selectionStart
          else None
        in
        (newAST, {newState with selectionStart})
    | FluidAutocompleteClick entry ->
        Option.map (getToken s ast) ~f:(fun ti -> acClick entry ti ast s)
        |> Option.withDefault ~default:(ast, s)
    | _ ->
        (ast, s)
  in
  let newState = updateAutocomplete m tlid newAST newState in
  (* Js.log2 "ast" (show_ast newAST) ; *)
  (* Js.log2 "tokens" (eToStructure s newAST) ; *)
  (newAST, newState)


let update (m : Types.model) (msg : Types.fluidMsg) : Types.modification =
  let s = m.fluidState in
  let s = {s with error = None; oldPos = s.newPos; actions = []} in
  match msg with
  | FluidUpdateDropdownIndex index when FluidCommands.isOpened m.fluidState.cp
    ->
      FluidCommands.cpSetIndex m index s
  | FluidUpdateDropdownIndex index ->
      let newState = acSetIndex index s in
      Types.TweakModel (fun m -> {m with fluidState = newState})
  | FluidKeyPress {key} when key = K.Undo ->
      KeyPress.undo_redo m false
  | FluidKeyPress {key} when key = K.Redo ->
      KeyPress.undo_redo m true
  | FluidKeyPress {key; altKey} when altKey && key = K.Letter 'x' ->
      maybeOpenCmd m
  | FluidKeyPress {key; metaKey; ctrlKey}
    when (metaKey || ctrlKey) && key = K.Letter 'k' ->
      KeyPress.openOmnibox m
  | FluidKeyPress ke when FluidCommands.isOpened m.fluidState.cp ->
      FluidCommands.updateCmds m ke
  | FluidClearErrorDvSrc ->
      FluidSetState {m.fluidState with errorDvSrc = SourceNone}
  | FluidFocusOnToken id ->
      (* Spec for Show token of expression: https://docs.google.com/document/d/13-jcP5xKe_Du-TMF7m4aPuDNKYjExAUZZ_Dk3MDSUtg/edit#heading=h.h1l570vp6wch *)
      tlidOf m.cursorState
      |> Option.andThen ~f:(fun tlid -> TL.get m tlid)
      |> Option.andThen ~f:(fun tl ->
             match TL.getAST tl with
             | Some expr ->
                 Some (tl, expr)
             | None ->
                 None )
      |> Option.map ~f:(fun (tl, expr) ->
             let ast = fromExpr s expr in
             let fluidState =
               let fs = moveToEndOfTarget id ast s in
               {fs with errorDvSrc = SourceId id}
             in
             let moveMod =
               match Viewport.moveToToken id tl with
               | Some dx, Some dy ->
                   MoveCanvasTo ({x = dx; y = dy}, AnimateTransition)
               | Some dx, None ->
                   MoveCanvasTo
                     ({x = dx; y = m.canvasProps.offset.y}, AnimateTransition)
               | None, Some dy ->
                   MoveCanvasTo
                     ({x = m.canvasProps.offset.x; y = dy}, AnimateTransition)
               | None, None ->
                   NoChange
             in
             if moveMod = NoChange
             then FluidSetState fluidState
             else Many [moveMod; FluidSetState fluidState] )
      |> Option.withDefault ~default:NoChange
  | FluidMouseDown _
  | FluidKeyPress _
  | FluidCopy
  | FluidPaste _
  | FluidCut
  | FluidCommandsFilter _
  | FluidCommandsClick _
  | FluidAutocompleteClick _
  | FluidUpdateAutocomplete
  | FluidMouseUp _ ->
      let tlid =
        match msg with
        | FluidMouseUp (tlid, _) ->
            Some tlid
        | _ ->
            tlidOf m.cursorState
      in
      let tl : toplevel option = Option.andThen tlid ~f:(Toplevel.get m) in
      let ast = Option.andThen tl ~f:TL.getAST in
      ( match (tl, ast) with
      | Some tl, Some ast ->
          let tlid = TL.id tl in
          let ast = fromExpr s ast in
          let newAST, newState = updateMsg m tlid ast msg s in
          let eventSpecMod, newAST, newState =
            let enter id = Enter (Filling (tlid, id)) in
            (* if tab is wrapping... *)
            if newState.lastKey = K.Tab && newState.newPos <= newState.oldPos
            then
              (* get the first blank spec header, or fall back to NoChange *)
              match tl with
              | TLHandler {spec; _} ->
                ( match SpecHeaders.firstBlank spec with
                | Some id ->
                    (enter id, ast, s)
                | None ->
                    (NoChange, newAST, newState) )
              | _ ->
                  (NoChange, newAST, newState)
            else if newState.lastKey = K.ShiftTab
                    && newState.newPos >= newState.oldPos
            then
              (* get the last blank spec header, or fall back to NoChange *)
              match tl with
              | TLHandler {spec; _} ->
                ( match SpecHeaders.lastBlank spec with
                | Some id ->
                    (enter id, ast, s)
                | None ->
                    (NoChange, newAST, newState) )
              | _ ->
                  (NoChange, newAST, newState)
            else (NoChange, newAST, newState)
          in
          let cmd =
            match newState.ac.index with
            | Some index ->
                FluidAutocomplete.focusItem index
            | None ->
                Cmd.none
          in
          let astMod =
            if ast <> newAST
            then
              let asExpr = toExpr newAST in
              let requestAnalysis =
                match Analysis.getSelectedTraceID m tlid with
                | Some traceID ->
                    let m = TL.withAST m tlid asExpr in
                    MakeCmd (Analysis.requestAnalysis m tlid traceID)
                | None ->
                    NoChange
              in
              Many
                [ Types.TweakModel (fun m -> TL.withAST m tlid asExpr)
                ; Toplevel.setSelectedAST m asExpr
                ; requestAnalysis
                ; UpdateASTCache (tlid, eToString s newAST) ]
            else Types.NoChange
          in
          Types.Many
            [ Types.TweakModel (fun m -> {m with fluidState = newState})
            ; astMod
            ; eventSpecMod
            ; Types.MakeCmd cmd ]
      | _ ->
          NoChange )


(* -------------------- *)
(* View *)
(* -------------------- *)
let viewAutocomplete (ac : Types.fluidAutocompleteState) : Types.msg Html.html
    =
  let toList acis class' index =
    List.indexedMap
      ~f:(fun i item ->
        let highlighted = index = i in
        let name = AC.asName item in
        let fnDisplayName = ViewUtils.fnDisplayName name in
        let versionDisplayName = ViewUtils.versionDisplayName name in
        let versionView =
          if String.length versionDisplayName > 0
          then Html.span [Html.class' "version"] [Html.text versionDisplayName]
          else Vdom.noNode
        in
        Html.li
          [ Attrs.classList
              [ ("autocomplete-item", true)
              ; ("fluid-selected", highlighted)
              ; (class', true) ]
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; ViewEntry.defaultPasteHandler
          ; ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.eventNoPropagation ~key:("ac-" ^ name) "click" (fun _ ->
                FluidMsg (FluidAutocompleteClick item) )
          ; ViewUtils.eventBoth
              ~key:("ac-mousemove" ^ name)
              "mousemove"
              (fun _ -> FluidMsg (FluidUpdateDropdownIndex i) ) ]
          [ Html.text fnDisplayName
          ; versionView
          ; Html.span [Html.class' "types"] [Html.text <| AC.asTypeString item]
          ] )
      acis
  in
  let index = ac.index |> Option.withDefault ~default:(-1) in
  let invalidIndex = index - List.length ac.completions in
  let autocompleteList =
    toList ac.completions "valid" index
    @ toList ac.invalidCompletions "invalid" invalidIndex
  in
  Html.div [Attrs.id "fluid-dropdown"] [Html.ul [] autocompleteList]


let submitAutocomplete (_m : model) : modification = NoChange

let viewCopyButton tlid value : msg Html.html =
  Html.div
    [ Html.class' "copy-value"
    ; Html.title "Copy this expression's value to the clipboard"
    ; ViewUtils.eventNoPropagation
        "click"
        ~key:("copylivevalue-" ^ value ^ showTLID tlid)
        (fun m -> ClipboardCopyLivevalue (value, m.mePos)) ]
    [ViewUtils.fontAwesome "copy"]


let viewErrorIndicator ~analysisStore ~state ti : Types.msg Html.html =
  let returnTipe name =
    let fn = Functions.findByNameInList name state.ac.functions in
    Runtime.tipe2str fn.fnReturnTipe
  in
  let sentToRail id =
    let dv = Analysis.getLiveValue' analysisStore id in
    match dv with
    | Some (DErrorRail (DResult (ResError _)))
    | Some (DErrorRail (DOption OptNothing)) ->
        "ErrorRail"
    | Some (DIncomplete _) | Some (DError _) ->
        "EvalFail"
    | _ ->
        ""
  in
  match ti.token with
  | TFnName (id, _, _, fnName, Rail) ->
      let offset = string_of_int ti.startRow ^ "rem" in
      let cls = ["error-indicator"; returnTipe fnName; sentToRail id] in
      let event =
        Vdom.noProp
        (* TEMPORARY DISABLE
          ViewUtils.eventNoPropagation	
            ~key:("er-" ^ show_id id)	
            "click"	
            (fun _ -> TakeOffErrorRail (tlid, id)) *)
      in
      Html.div
        [ Html.class' (String.join ~sep:" " cls)
        ; Html.styles [("top", offset)]
        ; event ]
        []
  | _ ->
      Vdom.noNode


let fnForToken state token : function_ option =
  match token with
  | TBinOp (_, fnName)
  | TFnVersion (_, _, _, fnName)
  | TFnName (_, _, _, fnName, _) ->
      Some (Functions.findByNameInList fnName state.ac.functions)
  | _ ->
      None


let fnArgExprs (token : token) (ast : fluidExpr) (state : state) :
    fluidExpr list =
  let id = Token.tid token in
  let previous =
    toExpr ast
    |> AST.threadPrevious id
    |> Option.toList
    |> List.map ~f:(fromExpr state)
  in
  let exprs =
    match findExpr id ast with
    | Some (EFnCall (_, _, exprs, _)) ->
        exprs
    | Some (EBinOp (_, _, lhs, rhs, _)) ->
        [lhs; rhs]
    | _ ->
        []
  in
  previous @ exprs


let viewPlayIcon
    ~(vs : ViewUtils.viewState) ~state (ast : ast) (ti : tokenInfo) :
    Types.msg Html.html =
  match fnForToken state ti.token with
  | Some fn ->
      let allExprs = fnArgExprs ti.token ast state in
      let argIDs = List.map ~f:eid allExprs in
      ( match ti.token with
      | TFnVersion (id, _, _, _) ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | TFnName (id, _, displayName, fnName, _) when displayName = fnName ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | _ ->
          Vdom.noNode )
  | None ->
      Vdom.noNode


let toHtml ~(vs : ViewUtils.viewState) ~tlid ~state (ast : ast) :
    Types.msg Html.html list =
  let l = ast |> toTokens state in
  (* Gets the source of a DIncomplete given an expr id *)
  let sourceOfExprValue id =
    if FluidToken.validID id
    then
      match Analysis.getLiveValueLoadable vs.analysisStore id with
      | LoadableSuccess (DIncomplete (SourceId id)) ->
          (Some id, "dark-incomplete")
      | LoadableSuccess (DError (SourceId id, _)) ->
          (Some id, "dark-error")
      | _ ->
          (None, "")
    else (None, "")
  in
  let currentTokenInfo = getToken state ast in
  let sourceOfCurrentToken onTi =
    currentTokenInfo
    |> Option.andThen ~f:(fun ti ->
           if Token.isBlank ti.token || onTi.startRow = ti.startRow
           then None
           else
             let someId, _ = Token.analysisID ti.token |> sourceOfExprValue in
             someId )
  in
  let nesting = ref 0 in
  List.map l ~f:(fun ti ->
      let dropdown () =
        match state.cp.location with
        | Some (ltlid, token) when tlid = ltlid && ti.token = token ->
            FluidCommands.viewCommandPalette state.cp
        | _ ->
            if isAutocompleting ti state
            then viewAutocomplete state.ac
            else Vdom.noNode
      in
      let element nested =
        let tokenId = Token.tid ti.token in
        let idStr = deID tokenId in
        let content = Token.toText ti.token in
        let analysisId = Token.analysisID ti.token in
        (* Apply CSS classes to token *)
        let tokenClasses = Token.toCssClasses ti.token in
        let backingNestingClass, innerNestingClass =
          let tokenBackingPrecedence, tokenInnerPrecedence =
            let currNesting = !nesting in
            match ti.token with
            | TParenOpen _ ->
                nesting := !nesting + 1 ;
                (currNesting, Some !nesting)
            | TParenClose _ ->
                nesting := !nesting - 1 ;
                (!nesting, Some currNesting)
            | _ ->
                (currNesting, None)
          in
          (* We want 0 precedence to only show up at the AST root and not in any wraparounds, so this goes 0123412341234... *)
          let wraparoundPrecedenceClass ~ext n =
            let wraparoundPrecedence =
              if n > 0 then ((n - 1) mod 4) + 1 else n
            in
            ["precedence-" ^ (wraparoundPrecedence |> string_of_int)] @ ext
          in
          ( tokenBackingPrecedence |> wraparoundPrecedenceClass ~ext:[]
          , tokenInnerPrecedence
            |> Option.map ~f:(wraparoundPrecedenceClass ~ext:["fluid-inner"])
          )
        in
        let cls =
          "fluid-entry"
          :: ("id-" ^ idStr)
          :: (backingNestingClass @ tokenClasses)
          |> List.map ~f:(fun s -> ViewUtils.strToBoolType ~condition:true s)
        in
        let conditionalClasses =
          let sourceId, errorType = sourceOfExprValue analysisId in
          let isError =
            (* Only apply to text tokens (not TSep, TNewlines, etc.) *)
            Token.isErrorDisplayable ti.token
            && (* This expression is the source of its own incompleteness. We only draw underlines under sources of incompletes, not all propagated occurrences. *)
               sourceId |> Option.isSomeEqualTo ~value:analysisId
          in
          [ ("related-change", List.member ~value:tokenId vs.hoveringRefs)
          ; ("cursor-on", currentTokenInfo |> Option.isSomeEqualTo ~value:ti)
          ; ("fluid-error", isError)
          ; (errorType, errorType <> "")
          ; (* This expression is the source of an incomplete propogated into another   expression, where the cursor is currently on *)
            ( "is-origin"
            , sourceOfCurrentToken ti |> Option.isSomeEqualTo ~value:analysisId
            )
          ; ( "jumped-to"
            , match state.errorDvSrc with
              | SourceNone ->
                  false
              | SourceId id ->
                  id = tokenId ) ]
        in
        let clickHandlers =
          [ ViewUtils.eventNeither
              ~key:("fluid-selection-dbl-click" ^ idStr)
              "dblclick"
              (fun ev ->
                match Entry.getFluidCaretPos () with
                | Some pos ->
                    let state =
                      {state with newPos = pos; oldPos = state.newPos}
                    in
                    ( match ev with
                    | {detail = 2; altKey = true} ->
                        FluidMsg
                          (FluidMouseUp
                             (tlid, getExpressionRangeAtCaret state ast))
                    | {detail = 2; altKey = false} ->
                        FluidMsg
                          (FluidMouseUp (tlid, getTokenRangeAtCaret state ast))
                    | _ ->
                        recover
                          "detail was not 2 in the doubleclick event"
                          ~debug:ev
                          (FluidMsg (FluidMouseUp (tlid, None))) )
                | None ->
                    recover
                      "found no caret pos in the doubleclick handler"
                      ~debug:ev
                      (FluidMsg (FluidMouseUp (tlid, None))) )
          ; ViewUtils.eventNoPropagation
              ~key:("fluid-selection-mousedown" ^ idStr)
              "mousedown"
              (fun _ -> FluidMsg (FluidMouseDown tlid) )
          ; ViewUtils.eventNoPropagation
              ~key:("fluid-selection-mouseup" ^ idStr)
              "mouseup"
              (fun _ -> FluidMsg (FluidMouseUp (tlid, None)) )
          ; ViewUtils.onAnimationEnd
              ~key:("anim-end" ^ idStr)
              ~listener:(fun msg ->
                if msg = "flashError" || msg = "flashIncomplete"
                then FluidMsg FluidClearErrorDvSrc
                else IgnoreMsg ) ]
        in
        let innerNode =
          match innerNestingClass with
          | Some cls ->
              [ Html.span
                  ([Attrs.class' (cls |> String.join ~sep:" ")] @ clickHandlers)
                  [Html.text content] ]
          | None ->
              [Html.text content]
        in
        Html.span
          (Html.classList (cls @ conditionalClasses) :: clickHandlers)
          (innerNode @ nested)
      in
      if vs.permission = Some ReadWrite
      then [element [dropdown (); viewPlayIcon ast ti ~vs ~state]]
      else [element []] )
  |> List.flatten


let viewLiveValue
    ~(tlid : tlid) ~(ast : fluidExpr) ~(vs : viewState) ~(state : fluidState) :
    Types.msg Html.html =
  (* Renders dval*)
  let renderDval dval canCopy =
    let text = Runtime.toRepr dval in
    [ Html.text text
    ; (if canCopy then viewCopyButton tlid text else Vdom.noNode) ]
  in
  (* Renders live value for token *)
  let renderTokenLv token id =
    let fnLoading =
      (* If fn needs to be manually executed, check status *)
      let fn = fnForToken state token in
      let args = fnArgExprs token ast state |> List.map ~f:eid in
      Option.andThen fn ~f:(fun fn ->
          if fn.fnPreviewExecutionSafe
          then None
          else
            let id = Token.tid token in
            ViewFnExecution.fnExecutionStatus vs fn id args
            |> ViewFnExecution.executionError
            |> Option.some )
    in
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess (DIncomplete _) when Option.isSome fnLoading ->
        [Html.text (Option.withDefault ~default:"" fnLoading)]
    | LoadableSuccess (DIncomplete (SourceId srcId) as dv)
    | LoadableSuccess (DError (SourceId srcId, _) as dv)
      when srcId <> id ->
        let msg =
          "<"
          ^ (dv |> Runtime.typeOf |> Runtime.tipe2str)
          ^ "> Click to locate source"
        in
        [ Html.div
            [ ViewUtils.eventNoPropagation
                ~key:("lv-src-" ^ deID srcId)
                "click"
                (fun _ -> FluidMsg (FluidFocusOnToken srcId))
            ; Html.class' "jump-src" ]
            [Html.text msg] ]
    | LoadableSuccess (DError _ as dv) | LoadableSuccess (DIncomplete _ as dv)
      ->
        renderDval dv false
    | LoadableSuccess dval ->
        renderDval dval true
    | LoadableNotInitialized | LoadableLoading _ ->
        [ViewUtils.fontAwesome "spinner"]
    | LoadableError err ->
        [Html.text ("Error loading live value: " ^ err)]
  in
  getToken state ast
  |> Option.andThen ~f:(fun ti ->
         let row = ti.startRow in
         let content =
           match AC.highlighted state.ac with
           | Some (FACVariable (_, Some dv)) ->
               (* If autocomplete is open and a variable is highlighted, then show its dval *)
               Some (renderDval dv true)
           | _ ->
               (* Else show live value of current token *)
               let token = ti.token in
               let id = Token.analysisID token in
               if Token.validID id then Some (renderTokenLv token id) else None
         in
         Option.pair content (Some row) )
  (* Render live value to the side *)
  |> Option.map ~f:(fun (content, row) ->
         let offset = float_of_int row +. 1.5 in
         Html.div
           [ Html.classList [("live-values", true)]
           ; Html.styles [("top", Js.Float.toString offset ^ "rem")]
           ; Attrs.autofocus false
           ; Vdom.attribute "" "spellcheck" "false" ]
           content )
  (* If there's a failure at any point, we don't render the live-value wrapper *)
  |> Option.withDefault ~default:Vdom.noNode


let viewAST ~(vs : ViewUtils.viewState) (ast : ast) : Types.msg Html.html list
    =
  let ({analysisStore; tlid} : ViewUtils.viewState) = vs in
  let state = vs.fluidState in
  let cmdOpen = FluidCommands.isOpenOnTL state.cp tlid in
  let event ~(key : string) (event : string) : Types.msg Vdom.property =
    let decodeNothing =
      let open Tea.Json.Decoder in
      succeed Types.IgnoreMsg
    in
    (* There is a check to preventDefault() in the appsupport file *)
    Html.on ~key event decodeNothing
  in
  let eventKey = "keydown" ^ show_tlid tlid ^ string_of_bool cmdOpen in
  let tokenInfos = ast |> toTokens state in
  let errorRail =
    let indicators =
      tokenInfos
      |> List.map ~f:(fun ti -> viewErrorIndicator ~analysisStore ~state ti)
    in
    let hasMaybeErrors = List.any ~f:(fun e -> e <> Vdom.noNode) indicators in
    Html.div
      [Html.classList [("fluid-error-rail", true); ("show", hasMaybeErrors)]]
      indicators
  in
  let liveValue =
    if vs.cursorState = FluidEntering tlid
    then viewLiveValue ~tlid ~ast ~vs ~state
    else Vdom.noNode
  in
  [ liveValue
  ; Html.div
      [ Attrs.id editorID
      ; Vdom.prop "contentEditable" "true"
      ; Attrs.autofocus true
      ; Vdom.attribute "" "spellcheck" "false"
      ; event ~key:eventKey "keydown" ]
      (toHtml ast ~vs ~tlid ~state)
  ; errorRail ]


let viewStatus (ast : ast) (s : state) : Types.msg Html.html =
  let tokens = toTokens s ast in
  let ddText txt = Html.dd [] [Html.text txt] in
  let dtText txt = Html.dt [] [Html.text txt] in
  let posData =
    let oldGrid = gridFor ~pos:s.oldPos tokens in
    let newGrid = gridFor ~pos:s.newPos tokens in
    [ dtText "pos"
    ; Html.dd
        []
        [ Html.text (string_of_int s.oldPos)
        ; Html.text " -> "
        ; Html.text (string_of_int s.newPos) ]
    ; dtText "grid"
    ; Html.dd
        []
        [ Html.text (oldGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (oldGrid.row |> string_of_int)
        ; Html.text " -> "
        ; Html.text (newGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (newGrid.row |> string_of_int) ]
    ; dtText "acIndex"
    ; Html.dd
        []
        [ Html.text
            ( s.ac.index
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "acEntryCount"
    ; Html.dd [] [Html.text (s.ac.completions |> List.length |> string_of_int)]
    ; dtText "upDownCol"
    ; Html.dd
        []
        [ Html.text
            ( s.upDownCol
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "lastKey"
    ; Html.dd
        []
        [ Html.text
            ( K.toName s.lastKey
            ^ ", "
            ^ ( K.toChar s.lastKey
              |> Option.map ~f:String.fromChar
              |> Option.withDefault ~default:"" ) ) ]
    ; dtText "selection"
    ; Html.dd
        []
        [ Html.text
            ( s.selectionStart
            |> Option.map ~f:(fun selStart ->
                   string_of_int selStart ^ "->" ^ string_of_int s.newPos )
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "midClick"
    ; Html.dd [] [Html.text (string_of_bool s.midClick)] ]
  in
  let error =
    [dtText "error"; ddText (Option.withDefault s.error ~default:"None")]
  in
  let tokenData =
    let left, right, next = getNeighbours tokens ~pos:s.newPos in
    let tokenInfo tkn =
      Html.dd [Attrs.class' "tokenInfo"] [Token.show_tokenInfo tkn]
    in
    let ddLeft =
      match left with
      | L (_, left) ->
          tokenInfo left
      | R (_, _) ->
          ddText "Right"
      | No ->
          ddText "None"
    in
    let ddRight =
      match right with
      | L (_, _) ->
          ddText "Left"
      | R (_, right) ->
          tokenInfo right
      | No ->
          ddText "None"
    in
    let ddNext =
      match next with Some next -> tokenInfo next | None -> ddText "None"
    in
    [dtText "left"; ddLeft; dtText "right"; ddRight; dtText "next"; ddNext]
  in
  let actions =
    [ dtText "actions"
    ; Html.dd
        [Attrs.class' "actions"]
        [ Html.ul
            []
            (List.map s.actions ~f:(fun txt -> Html.li [] [Html.text txt])) ]
    ]
  in
  let status = List.concat [posData; error; tokenData; actions] in
  Html.div [Attrs.id "fluid-status"] [Html.dl [] status]


(* -------------------- *)
(* Scaffolidng *)
(* -------------------- *)

let selectedASTAsText (m : model) : string option =
  let s = m.fluidState in
  TL.selectedAST m |> Option.map ~f:(fromExpr s) |> Option.map ~f:(eToString s)


let renderCallback (m : model) : unit =
  match m.cursorState with
  | FluidEntering _ when m.fluidState.midClick = false ->
      if FluidCommands.isOpened m.fluidState.cp
      then ()
      else (
        (* This for two different purposes:
         * 1. When a key press mutates the text in the content editable, the browser resets the caret position to the * beginnning of the content editable. Here we set the caret to the correct position from the fluidState
         * 2. We intercept all keyboard caret movement, therefore we need to set the caret to the correct position
         * from the fluidState

         * We do this after a render(not waiting til the next frame) so that the developer does not see the caret
         * flicker to default browser position
         *)
        match m.fluidState.selectionStart with
        | Some selStart ->
            (* Updates the browser selection range for 2 in the context of selections *)
            Entry.setFluidSelectionRange (selStart, m.fluidState.newPos)
        | None ->
            Entry.setFluidCaret m.fluidState.newPos )
  | _ ->
      ()


(* Some things aren't in non-fluid, and if you switch from one to the other you'll be in trouble. Just allow them to be removed. *)

let stripConstructs (ast : expr) : expr =
  let rec f e =
    match e with
    | F (_, FluidPartial (_, oldExpr)) ->
        f oldExpr
    | F (_, FluidRightPartial (_, oldExpr)) ->
        f oldExpr
    | _ ->
        AST.traverse f e
  in
  f ast


let stripFluidConstructsFromFunctions (ufs : userFunction list) :
    userFunction list =
  List.map ufs ~f:(fun uf -> {uf with ufAST = stripConstructs uf.ufAST})


let stripFluidConstructsFromHandlers (hs : handler list) : handler list =
  List.map hs ~f:(fun h -> {h with ast = stripConstructs h.ast})


let exprToStr (s : state) (e : expr) : string = e |> fromExpr s |> eToString s
