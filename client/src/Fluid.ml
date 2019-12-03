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
  let f = fromExpr s in
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
        let fn =
          List.find s.ac.functions ~f:(fun fn -> fn.fnName = varToName name)
        in
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
          recover "empty pipe" expr (newB ()) )
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
            str
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
                    p
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
        recover "fn has a pipe target but no pipe" expr (Blank.new_ ())
    | EPipeTarget _ :: args when inPipe ->
        F
          ( id
          , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
          )
    | _nonPipeTarget :: _ when inPipe ->
        recover "fn has a pipe but no pipe target" expr (Blank.new_ ())
    | args ->
        F
          ( id
          , FnCall (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster)
          ) )
  | EBinOp (id, name, arg1, arg2, ster) ->
    ( match arg1 with
    | EPipeTarget _ when not inPipe ->
        recover "binop has a pipe target but no pipe" expr (Blank.new_ ())
    | EPipeTarget _ when inPipe ->
        F (id, FnCall (F (ID (deID id ^ "_name"), name), [toExpr arg2], ster))
    | _nonPipeTarget when inPipe ->
        recover "binop has a pipe but no pipe target" expr (Blank.new_ ())
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
      recover "Cant convert pipetargets back to exprs" expr (Blank.new_ ())
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

let rec patternToToken (p : fluidPattern) : fluidToken list =
  match p with
  | FPVariable (mid, id, name) ->
      [TPatternVariable (mid, id, name)]
  | FPConstructor (mid, id, name, args) ->
      let args = List.map args ~f:(fun a -> TSep id :: patternToToken a) in
      List.concat ([TPatternConstructorName (mid, id, name)] :: args)
  | FPInteger (mid, id, i) ->
      [TPatternInteger (mid, id, i)]
  | FPBool (mid, id, b) ->
      if b then [TPatternTrue (mid, id)] else [TPatternFalse (mid, id)]
  | FPString (mid, id, s) ->
      [TPatternString (mid, id, s)]
  | FPFloat (mID, id, whole, fraction) ->
      let whole =
        if whole = "" then [] else [TPatternFloatWhole (mID, id, whole)]
      in
      let fraction =
        if fraction = ""
        then []
        else [TPatternFloatFraction (mID, id, fraction)]
      in
      whole @ [TPatternFloatPoint (mID, id)] @ fraction
  | FPNull (mid, id) ->
      [TPatternNullToken (mid, id)]
  | FPBlank (mid, id) ->
      [TPatternBlank (mid, id)]
  | FPOldPattern (mid, op) ->
      [TPatternString (mid, Blank.toID op, "TODO: old pattern")]


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
  let fromExpr e b = toTokens' s e b in
  let open Builder in
  let ghostPartial id newName oldName =
    let oldName = ViewUtils.partialName oldName in
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
            s.ac.functions
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
             |> addNewlineIfNeeded (Some (id, id, None))
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
      let ghost = ghostPartial id newName oldName in
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
      let ghost = ghostPartial id newName oldName in
      b
      |> add (TPartial (id, newName))
      |> addMany ghost
      |> addArgs oldName id args
  | EConstructor (id, _, name, exprs) ->
      b |> add (TConstructorName (id, name)) |> addArgs name id exprs
  | EPartial (id, newName, EConstructor (_, _, oldName, exprs)) ->
      let ghost = ghostPartial id newName oldName in
      b
      |> add (TPartial (id, newName))
      |> addMany ghost
      |> addArgs oldName id exprs
  | EFieldAccess (id, expr, fieldID, fieldname) ->
      b
      |> addNested ~f:(fromExpr expr)
      |> addMany [TFieldOp id; TFieldName (id, fieldID, fieldname)]
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
               addIter fields b ~f:(fun i (aid, fname, expr) b ->
                   b
                   |> addNewlineIfNeeded (Some (id, id, Some i))
                   |> add (TRecordFieldname (id, aid, i, fname))
                   |> add (TRecordSep (id, i, aid))
                   |> addNested ~f:(fromExpr expr) ) )
        |> addMany
             [ TNewline (Some (id, id, Some (List.length fields)))
             ; TRecordClose id ]
  | EPipe (id, exprs) ->
      let length = List.length exprs in
      ( match exprs with
      | [] ->
          recover "Empty pipe found" e b
      | [single] ->
          recover "pipe with single entry found" e (fromExpr single b)
      | head :: tail ->
          b
          |> addNested ~f:(fromExpr head)
          |> addNewlineIfNeeded (Some (id, id, Some 0))
          |> addIter tail ~f:(fun i e b ->
                 b
                 |> add (TPipe (id, i, length))
                 |> addNested ~f:(fromExpr e)
                 |> addNewlineIfNeeded (Some (id, id, Some (i + 1))) )
          |> addNewlineIfNeeded (Some (id, id, Some (List.length tail))) )
  | EPipeTarget _ ->
      recover "should never be making tokens for EPipeTarget" e b
  | EMatch (id, mexpr, pairs) ->
      b
      |> add (TMatchKeyword id)
      |> addNested ~f:(fromExpr mexpr)
      |> indentBy ~indent:2 ~f:(fun b ->
             b
             |> addIter pairs ~f:(fun i (pattern, expr) b ->
                    b
                    |> addNewlineIfNeeded (Some (id, id, Some i))
                    |> addMany (patternToToken pattern)
                    |> addMany
                         [TSep id; TMatchSep (pid pattern); TSep (pid pattern)]
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
      asserT "invalid token" (String.length (Token.toText t) > 0) t ;
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


let eToStructure (s : state) (e : fluidExpr) : string =
  e
  |> toTokens s
  |> List.map ~f:(fun ti ->
         "<" ^ Token.toTypeName ti.token ^ ":" ^ Token.toText ti.token ^ ">" )
  |> String.join ~sep:""


(* -------------------- *)
(* Patterns *)
(* -------------------- *)
let pToString (p : fluidPattern) : string =
  p
  |> patternToToken
  |> List.map ~f:(fun t -> Token.toTestText t)
  |> String.join ~sep:""


let pToStructure (p : fluidPattern) : string =
  p
  |> patternToToken
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
          recover "unexpected adjustedPosFor" (row, col) 0 )


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


let updateExpr ~(f : fluidExpr -> fluidExpr) (id : id) (ast : ast) : ast =
  let rec run e = if id = eid e then f e else recurse ~f:run e in
  run ast


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


let moveToStartOfLine (ast : ast) (ti : tokenInfo) (s : state) : state =
  let s = recordAction "moveToStartOfLine" s in
  let token =
    toTokens s ast
    |> List.find ~f:(fun info ->
           if info.startRow == ti.startRow
           then
             match info.token with
             (* To prevent the cursor from being put in TPipes or TIndents token *)
             | TPipe _ | TIndent _ ->
                 false
             | _ ->
                 true
           else false )
    |> Option.withDefault ~default:ti
  in
  let newPos = token.startPos in
  setPosition s newPos


let moveToEndOfLine (ast : ast) (ti : tokenInfo) (s : state) : state =
  let s = recordAction "moveToEndOfLine" s in
  let token =
    toTokens s ast
    |> List.reverse
    |> List.find ~f:(fun info -> info.startRow == ti.startRow)
    |> Option.withDefault ~default:ti
  in
  let newPos =
    match token.token with
    (* To prevent the cursor from going to the end of an indent or to a new line *)
    | TNewline _ | TIndent _ ->
        token.startPos
    | _ ->
        token.endPos
  in
  setPosition s newPos


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
  (* Finds how many moves to get to previous whitespace in a string *)
  let findPosOffsetToNextWhiteSpaceInStr (tokenInfo : fluidTokenInfo) : int =
    let posInToken = pos - tokenInfo.startPos in
    let offset : int ref = ref tokenInfo.length in
    let _ =
      Token.toText tokenInfo.token
      |> String.split ~on:""
      |> List.reverse
      |> List.find ~f:(fun a ->
             if ( a == " "
                || (a = "\"" && !offset != tokenInfo.length)
                || a = "\n"
                || a = "\t" )
                && !offset < posInToken
             then true
             else (
               offset := !offset - 1 ;
               false ) )
    in
    !offset
  in
  let newPos =
    let tokenInfo = previousToken |> Option.withDefault ~default:ti in
    if Token.isStringToken tokenInfo.token && pos != tokenInfo.startPos
    then
      let offset = findPosOffsetToNextWhiteSpaceInStr tokenInfo in
      tokenInfo.startPos + offset
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
  (* Finds how many moves to get to next whitespace in a string *)
  let findPosOffsetToNextWhiteSpaceInStr (tokenInfo : fluidTokenInfo) : int =
    let posInToken = pos - tokenInfo.startPos in
    let offset : int ref = ref 0 in
    let _ =
      Token.toText tokenInfo.token
      |> String.split ~on:""
      |> List.find ~f:(fun a ->
             if (a == " " || (a = "\"" && !offset > 0) || a = "\n" || a = "\t")
                && !offset > posInToken
             then true
             else (
               offset := !offset + 1 ;
               false ) )
    in
    !offset
  in
  let newPos =
    let tokenInfo = nextToken |> Option.withDefault ~default:ti in
    if Token.isStringToken tokenInfo.token && pos != tokenInfo.endPos
    then
      let offset = findPosOffsetToNextWhiteSpaceInStr tokenInfo in
      tokenInfo.startPos + offset
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
      recover "cannot find token to moveToEndOfTarget" (target, ast) s
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
          recover "not a match in replaceVarInPattern" e e )


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
          recover "not a match in removePatternRow" e e )


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
      | EFieldAccess (id, expr, fieldID, _) ->
          EFieldAccess (id, expr, fieldID, str)
      | _ ->
          recover "not a field in replaceFieldName" e e )


let exprToFieldAccess (id : id) (fieldID : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e -> EFieldAccess (fieldID, e, gid (), ""))


let removeField (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFieldAccess (_, faExpr, _, _) ->
          faExpr
      | _ ->
          recover "not a fieldAccess in removeField" e e )


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
          recover "not a lamda in replaceLamdaVar" e e )


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
          recover "not a list in insertLambdaVar" e e )


(* ---------------- *)
(* Lets *)
(* ---------------- *)

let replaceLetLHS (newLHS : string) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ELet (id, lhsID, oldLHS, rhs, next) ->
          ELet (id, lhsID, newLHS, rhs, renameVariableUses oldLHS newLHS next)
      | _ ->
          recover "not a let in replaceLetLHS" e e )


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
          recover "not a record in replaceRecordField" e e )


let removeRecordField (id : id) (index : int) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, List.removeAt ~index fields)
      | _ ->
          recover "not a record field in removeRecordField" e e )


(* Add a row to the record *)
let addRecordRowAt (index : int) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, List.insertAt ~index ~value:(gid (), "", newB ()) fields)
      | _ ->
          recover "Not a record in addRecordRowAt" e e )


let addRecordRowToBack (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, fields @ [(gid (), "", newB ())])
      | _ ->
          recover "Not a record in addRecordRowToTheBack" e e )


(* ---------------- *)
(* Partials *)
(* ---------------- *)

let replaceWithPartial (str : string) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      let str = String.trim str in
      match e with
      | EPartial (id, _, oldVal) ->
          asserT "empty partial, use deletePartial instead" (str <> "") str ;
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
            recover "not a partial in deletePartial" e e )
  in
  (ast, !newState ast)


let replacePartialWithArguments
    ~(newExpr : fluidExpr) (id : id) (s : state) (ast : ast) : ast =
  let getFunctionParams fnname count varExprs =
    List.map (List.range 0 count) ~f:(fun index ->
        s.ac.functions
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
        recover "impossible" expr []
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
                      newParams
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
      then recover "replacing with empty right partial" e e
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
            recover "not a binop in deleteBinOp" e e )
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
  | TPatternString (mID, id, str) ->
      replacePattern mID id ~newPat:(FPString (mID, id, f str)) ast
  | TInteger (id, str) ->
      let str = f str in
      let newExpr =
        if str = ""
        then EBlank id
        else EInteger (id, coerceStringTo63BitInt str)
      in
      replaceExpr id ~newExpr ast
  | TPatternInteger (mID, id, str) ->
      let str = f str in
      let newPat =
        if str = ""
        then FPBlank (mID, id)
        else FPInteger (mID, id, coerceStringTo63BitInt str)
      in
      replacePattern mID id ~newPat ast
  | TPatternNullToken (mID, id) ->
      let str = f "null" in
      let newExpr = FPVariable (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternTrue (mID, id) ->
      let str = f "true" in
      let newExpr = FPVariable (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternFalse (mID, id) ->
      let str = f "false" in
      let newExpr = FPVariable (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternVariable (mID, _, str) ->
      replaceVarInPattern mID str (f str) ast
  | TRecordFieldname (id, _, index, str) ->
      replaceRecordField ~index (f str) id ast
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
  | TTrue id ->
      replaceWithPartial (f "true") id ast
  | TFalse id ->
      replaceWithPartial (f "false") id ast
  | TNullToken id ->
      replaceWithPartial (f "null") id ast
  | TBinOp (id, name) ->
      replaceWithPartial (f name) id ast
  | _ ->
      recover "not supported by replaceToken" token ast


(* ---------------- *)
(* Floats  *)
(* ---------------- *)
let replaceFloatWhole (str : string) (id : id) (ast : ast) : fluidExpr =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (id, _, fraction) ->
          EFloat (id, str, fraction)
      | _ ->
          recover "not a float im replaceFloatWhole" e e )


let replacePatternFloatWhole
    (str : string) (matchID : id) (patID : id) (ast : ast) : fluidExpr =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, patID, _, fraction) ->
          FPFloat (matchID, patID, str, fraction)
      | _ ->
          recover "not a float in replacePatternFloatWhole" e e )


let replacePatternFloatFraction
    (str : string) (matchID : id) (patID : id) (ast : ast) : fluidExpr =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, patID, whole, _) ->
          FPFloat (matchID, patID, whole, str)
      | _ ->
          recover "not a float in replacePatternFloatFraction" e e )


let removePatternPointFromFloat (matchID : id) (patID : id) (ast : ast) : ast =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, _, whole, fraction) ->
          let i = coerceStringTo63BitInt (whole ^ fraction) in
          FPInteger (matchID, gid (), i)
      | _ ->
          recover "Not an int in removePatternPointFromFloat" e e )


let replaceFloatFraction (str : string) (id : id) (ast : ast) : fluidExpr =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (id, whole, _) ->
          EFloat (id, whole, str)
      | _ ->
          recover "not a floatin replaceFloatFraction" e e )


let insertAtFrontOfFloatFraction (letter : string) (id : id) (ast : ast) :
    fluidExpr =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (id, whole, fraction) ->
          EFloat (id, whole, letter ^ fraction)
      | _ ->
          recover "not a float in insertAtFrontOfFloatFraction" e e )


let insertAtFrontOfPatternFloatFraction
    (letter : string) (matchID : id) (patID : id) (ast : ast) : fluidExpr =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPFloat (matchID, patID, whole, fraction) ->
          FPFloat (matchID, patID, whole, letter ^ fraction)
      | _ ->
          recover "not a float in insertAtFrontOfPatternFloatFraction" e e )


let convertIntToFloat (offset : int) (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EInteger (_, i) ->
          let whole, fraction = String.splitAt ~index:offset i in
          EFloat (gid (), whole, fraction)
      | _ ->
          recover "Not an int in convertIntToFloat" e e )


let convertPatternIntToFloat
    (offset : int) (matchID : id) (patID : id) (ast : ast) : ast =
  updatePattern matchID patID ast ~f:(fun e ->
      match e with
      | FPInteger (matchID, _, i) ->
          let whole, fraction = String.splitAt ~index:offset i in
          FPFloat (matchID, gid (), whole, fraction)
      | _ ->
          recover "Not an int in convertPatternIntToFloat" e e )


let removePointFromFloat (id : id) (ast : ast) : ast =
  updateExpr id ast ~f:(fun e ->
      match e with
      | EFloat (_, whole, fraction) ->
          let i = coerceStringTo63BitInt (whole ^ fraction) in
          EInteger (gid (), i)
      | _ ->
          recover "Not an int in removePointFromFloat" e e )


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
          recover "not a list in insertInList" e e )


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


(* ---------------- *)
(* General stuff *)
(* ---------------- *)
let addEntryBelow
    (id : id)
    (index : int option)
    (ast : ast)
    (s : fluidState)
    (f : fluidState -> fluidState) : ast * fluidState =
  let s = recordAction "addEntryBelow" s in
  let cursor = ref `NextBlank in
  let newAST =
    updateExpr id ast ~f:(fun e ->
        match (index, e) with
        | None, EBlank _ ->
            cursor := `NextToken ;
            e
        | None, e ->
            ELet (gid (), gid (), "", newB (), e)
        | Some index, ERecord (id, fields) ->
            ERecord
              (id, List.insertAt fields ~index ~value:(gid (), "", newB ()))
        | Some index, EMatch (id, cond, rows) ->
            (* TODO: this doesn't work on the last row, due to how matches are
             * created, which is hard to fix. *)
            EMatch
              ( id
              , cond
              , List.insertAt
                  rows
                  ~index
                  ~value:(FPBlank (gid (), gid ()), newB ()) )
        | Some index, EPipe (id, exprs) ->
            EPipe (id, List.insertAt exprs ~index:(index + 1) ~value:(newB ()))
        | _ ->
            cursor := `NextToken ;
            e )
  in
  let newState =
    match !cursor with
    | `NextToken ->
        f s
    | `NextBlank ->
        moveToNextBlank ~pos:s.newPos newAST s
  in
  (newAST, newState)


let addEntryAbove (id : id) (index : int option) (ast : ast) (s : fluidState) :
    ast * fluidState =
  let s = recordAction "addEntryAbove" s in
  let nextIndex = ref None in
  let newAST =
    updateExpr id ast ~f:(fun e ->
        match (index, e) with
        | Some index, ERecord (id, fields) ->
            nextIndex := Some (index + 1) ;
            ERecord
              (id, List.insertAt fields ~index ~value:(gid (), "", newB ()))
        | Some index, EMatch (id, cond, rows) ->
            nextIndex := Some (index + 1) ;
            EMatch
              ( id
              , cond
              , List.insertAt
                  rows
                  ~index
                  ~value:(FPBlank (gid (), gid ()), newB ()) )
        | Some index, EPipe (id, exprs) ->
            (* TODO: this should move to the inside of the |> *)
            nextIndex := Some (index + 1) ;
            EPipe (id, List.insertAt exprs ~index:(index + 1) ~value:(newB ()))
        | None, e ->
            ELet (gid (), gid (), "", newB (), e)
        | _ ->
            e )
  in
  let newState =
    let tokens = toTokens s newAST in
    let newToken =
      List.find tokens ~f:(fun ti ->
          match ti.token with
          | TNewline (Some (tid, _, tindex))
            when id = tid && tindex = !nextIndex ->
              true
          | _ ->
              false )
    in
    let newPos =
      Option.map newToken ~f:(fun ti -> ti.startPos)
      |> Option.withDefault ~default:s.newPos
    in
    moveToNextNonWhitespaceToken ~pos:newPos newAST {s with newPos}
  in
  (newAST, newState)


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
            recover "BinOp doesn't have 2 args" args (newB (), 0)
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
      recover "patterns are not supported here" entry (newB (), 0)
  | FACField fieldname ->
      ( EFieldAccess (gid (), newB (), gid (), fieldname)
      , String.length fieldname )
  | FACLiteral _ ->
      recover "invalid literal in autocomplete" entry (newB (), 0)


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
        entry
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


let acMoveBasedOnKey
    (key : K.key) (startPos : int) (offset : int) (s : state) (ast : ast) :
    state =
  let tokens = toTokens s ast in
  let nextBlank = getNextBlankPos s.newPos tokens in
  let prevBlank = getPrevBlankPos s.newPos tokens in
  let newPos =
    match key with
    | K.Tab ->
        nextBlank
    | K.ShiftTab ->
        prevBlank
    | K.Enter ->
        startPos + offset
    | K.Space ->
        let thisTi =
          List.find ~f:(fun ti -> ti.startPos = startPos + offset) tokens
        in
        ( match thisTi with
        (* Only move forward to skip over a separator *)
        (* TODO: are there more separators we should consider here? *)
        | Some {token = TSep _} ->
            min nextBlank (startPos + offset + 1)
        | _ ->
            (* if new position is after next blank, stay in next blank *)
            startPos + offset )
    | _ ->
        s.newPos
  in
  let newState = moveTo newPos (acClear s) in
  newState


let rec findAppropriatePipingParent (oldExpr : fluidExpr) (ast : ast) :
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
        recover "these cant be parents" parent None
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
        findAppropriatePipingParent parent ast
    (* These are wrappers of the current expr. *)
    | EPartial _ | ERightPartial _ ->
        findAppropriatePipingParent parent ast )
  | None ->
      (* If we get to the root *)
      Some child


let doShiftEnter ~(findParent : bool) (id : id) (ast : ast) (s : state) :
    ast * state =
  let exprToReplace =
    findExpr id ast
    |> Option.andThen ~f:(fun e ->
           if findParent then findAppropriatePipingParent e ast else Some e )
    |> Option.map ~f:extractSubexprFromPartial
  in
  match exprToReplace with
  | None ->
      (ast, s)
  | Some expr ->
      let pipeChild = newB () in
      let newExpr = EPipe (gid (), [expr; pipeChild]) in
      let newAST = replaceExpr (eid expr) ast ~newExpr in
      (newAST, moveToEndOfTarget (eid pipeChild) newAST s)


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
    | (TPatternBlank (mID, pID) | TPatternVariable (mID, pID, _)), _, _, _ ->
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
            findAppropriatePipingParent oldExpr ast
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
    | ( TFieldName _
      , Some (EFieldAccess (faID, labelid, expr, _))
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


(* Convert the expression at ti into a FieldAccess, using the currently
 * selected Autocomplete value *)
let acStartField (ti : tokenInfo) (ast : ast) (s : state) : ast * state =
  let s = recordAction ~ti "acStartField" s in
  match AC.highlighted s.ac with
  | None ->
      (ast, s)
  | Some entry ->
      let newExpr, length = acToExpr entry in
      let newExpr = EFieldAccess (gid (), newExpr, gid (), "") in
      let length = length + 1 in
      let newState = s |> moveTo (ti.startPos + length) |> acClear in
      let newAST = replaceExpr ~newExpr (Token.tid ti.token) ast in
      (newAST, newState)


(* -------------------- *)
(* Code entering/interaction *)
(* -------------------- *)

(* posFromCaretTarget returns the position in the token stream corresponding to
   the passed caretTarget within the passed ast. We expect to succeed in finding
   the target. If we cannot, we `recover` and return the current caret pos
   as a fallback.
   
   This is useful for determining the precise position to which the caret should
   jump after a transformation. *)
let posFromCaretTarget (s : state) (ast : ast) (ct : caretTarget) : int =
  (* TODO(JULIAN): we may want to consider passing an old and new AST eventually, if
     that makes the mapping easier. *)
  let tokens = toTokens s ast in
  let tokenInfo =
    List.find tokens ~f:(fun ti ->
        match (ti.token, ct.astRef) with
        | ( TRecordFieldname (id, _, tIndex, _)
          , ARRecordFieldname (targetId, fieldIndex) )
          when id = targetId && tIndex = fieldIndex ->
            true
        | _ ->
            false )
  in
  match tokenInfo with
  | Some ti ->
      ti.startPos + min ct.offset ti.length
  | None ->
      recover
        "We expected to find the given caretTarget in the token stream but couldn't."
        ct
        s.newPos


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
    | TPatternString (mID, id, "") ->
        (replacePattern mID id ~newPat:(FPBlank (mID, newID)) ast, LeftOne)
    | TLambdaSep (id, idx) ->
        (removeLambdaSepToken id ast idx, LeftOne)
    | TListSep (id, idx) ->
        (removeListSepToken id ast idx, LeftOne)
    | (TRecordOpen id | TListOpen id) when exprIsEmpty id ast ->
        (replaceExpr id ~newExpr:(EBlank newID) ast, LeftOne)
    | TRecordFieldname (id, _, i, "") when pos = ti.startPos ->
        (removeRecordField id i ast, LeftThree)
    | TPatternBlank (mID, id) when pos = ti.startPos ->
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
    | TFieldOp id ->
        (removeField id ast, LeftOne)
    | TFloatPoint id ->
        (removePointFromFloat id ast, LeftOne)
    | TPatternFloatPoint (mID, id) ->
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
    | TPatternFloatWhole (mID, id, str) ->
        let str = removeCharAt str offset in
        (replacePatternFloatWhole str mID id ast, LeftOne)
    | TPatternFloatFraction (mID, id, str) ->
        let str = removeCharAt str offset in
        (replacePatternFloatFraction str mID id ast, LeftOne)
    | TFloatWhole (id, str) ->
        let str = removeCharAt str offset in
        (replaceFloatWhole str id ast, LeftOne)
    | TFloatFraction (id, str) ->
        let str = removeCharAt str offset in
        (replaceFloatFraction str id ast, LeftOne)
    | TPatternConstructorName (mID, id, str) ->
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
                ti
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
  | TFieldOp id ->
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
  | TPatternString (mID, id, str) ->
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
  | TPatternFloatPoint (mID, id) ->
      (removePatternPointFromFloat mID id ast, s)
  | TPatternFloatFraction (mID, id, str) ->
      (replacePatternFloatFraction (f str) mID id ast, s)
  | TPatternFloatWhole (mID, id, str) ->
      (replacePatternFloatWhole (f str) mID id ast, s)
  | TPatternConstructorName (mID, id, str) ->
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
              recover "TPipe should never occur on first line of AST" ti s
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
    | TLambdaVar _
    | TRecordFieldname _
      when not (isIdentifierChar letterStr) ->
        (ast, SamePlace)
    | TVariable _
    | TPatternVariable _
    | TLetLHS _
    | TFieldName _
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
    | TPatternInteger (_, _, i) | TInteger (_, i) ->
        let newLength = f i |> coerceStringTo63BitInt |> String.length in
        let move = if newLength > offset then RightOne else SamePlace in
        (replaceStringToken ~f ti.token ast, move)
    | TFloatWhole (id, str) ->
        (replaceFloatWhole (f str) id ast, RightOne)
    | TFloatFraction (id, str) ->
        (replaceFloatFraction (f str) id ast, RightOne)
    | TFloatPoint id ->
        (insertAtFrontOfFloatFraction letterStr id ast, RightOne)
    | TPatternFloatWhole (mID, id, str) ->
        (replacePatternFloatWhole (f str) mID id ast, RightOne)
    | TPatternFloatFraction (mID, id, str) ->
        (replacePatternFloatFraction (f str) mID id ast, RightOne)
    | TPatternFloatPoint (mID, id) ->
        (insertAtFrontOfPatternFloatFraction letterStr mID id ast, RightOne)
    | TPatternConstructorName _ ->
        (ast, SamePlace)
    | TPatternBlank (mID, pID) ->
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
   * that face based on the _next_ token *)
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
    | K.Backspace, L (TPatternString _, ti), _
    | K.Backspace, L (TString _, ti), _
      when pos = ti.endPos ->
        (* Backspace should move into a string, not delete it *)
        (ast, moveOneLeft pos s)
    | K.Backspace, _, R (TRecordFieldname (_, _, _, ""), _)
      when Option.isSome s.selectionStart ->
        deleteSelection ~state:s ~ast
    | K.Backspace, _, R (TRecordFieldname (_, _, _, ""), ti) ->
        doBackspace ~pos ti ast s
    | K.Backspace, _, R (TPatternBlank (_, _), _)
      when Option.isSome s.selectionStart ->
        deleteSelection ~state:s ~ast
    | K.Backspace, _, R (TPatternBlank (_, _), ti) ->
        doBackspace ~pos ti ast s
    | (K.Delete, _, _ | K.Backspace, _, _) when Option.isSome s.selectionStart
      ->
        deleteSelection ~state:s ~ast
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
          Option.map topmostID ~f:(fun id -> doShiftEnter ~findParent id ast s)
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
    (* Record-specific insertions *)
    | K.Enter, L (TRecordOpen id, _), _ ->
        let newAST = addRecordRowAt 0 id ast in
        let newPos =
          posFromCaretTarget
            s
            newAST
            {astRef = ARRecordFieldname (id, 0); offset = 0}
        in
        (newAST, {s with newPos})
    | K.Enter, _, R (TRecordClose id, _) ->
        let newAST = addRecordRowToBack id ast in
        (newAST, moveToNextNonWhitespaceToken ~pos newAST s)
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
    (* End of line *)
    | K.Enter, _, R (TNewline (Some (id, _, index)), ti) ->
        addEntryBelow id index ast s (doRight ~pos ~next:mNext ti)
    | K.Enter, _, R (TNewline None, ti) ->
        (ast, doRight ~pos ~next:mNext ti s)
    | K.Enter, L (TPipe (id, index, _), _), _ ->
        let newAST, newState = addEntryAbove id (Some index) ast s in
        (newAST, {newState with newPos = newState.newPos + 2})
    | K.Enter, L (TNewline (Some (id, _, index)), _), _ ->
        addEntryAbove id index ast s
    | K.Enter, No, R (t, _) ->
        addEntryAbove (FluidToken.tid t) None ast s
    (* Int to float *)
    | K.Period, L (TInteger (id, _), ti), _ ->
        let offset = pos - ti.startPos in
        (convertIntToFloat offset id ast, moveOneRight pos s)
    | K.Period, L (TPatternInteger (mID, id, _), ti), _ ->
        let offset = pos - ti.startPos in
        (convertPatternIntToFloat offset mID id ast, moveOneRight pos s)
    (* Skipping over specific characters, this must come before the
     * more general binop cases or we lose the jumping behaviour *)
    | K.Equals, _, R (TLetAssignment _, toTheRight) ->
        (ast, moveTo toTheRight.endPos s)
    | K.Colon, _, R (TRecordSep _, toTheRight) ->
        (ast, moveTo toTheRight.endPos s)
    (* Binop specific, all of the specific cases must come before the
     * big general `key, L (_, toTheLeft), _` case.  *)
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
    | _, L (_, toTheLeft), _ when Token.isAppendable toTheLeft.token ->
        doInsert ~pos keyChar toTheLeft ast s
    | _, _, R (TListOpen _, _) ->
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
     * through this code, if so we need to move it.  *)
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
   * again to make sure we go to the right place for the new canvas.  *)
  if recursing
  then (newAST, newState)
  else
    match (toTheLeft, toTheRight) with
    | L (TPartial (_, str), ti), _
    | _, R (TPartial (_, str), ti)
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


and deleteSelection ~state ~ast : ast * fluidState =
  let rangeStart, rangeEnd =
    fluidGetSelectionRange state |> orderRangeFromSmallToBig
  in
  let state =
    {state with newPos = rangeEnd; oldPos = state.newPos; selectionStart = None}
  in
  (* repeat deletion operation over range, starting from last position till first *)
  Array.range ~from:rangeStart rangeEnd
  |> Array.toList
  |> List.foldl ~init:(true, ast, state) ~f:(fun _ (continue, ast, state) ->
         let newAst, newState = updateKey K.Backspace ast state in
         if not continue
         then (false, ast, state)
         else if (* stop deleting if newPos doesn't change to prevent infinite recursion*)
                 newState.newPos = state.newPos
         then (false, ast, state)
         else if (* stop deleting if we reach range start*)
                 newState.newPos < rangeStart
         then (false, ast, state)
         else (true, newAst, newState) )
  |> fun (_, ast, state) -> (ast, state)


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
  | K.GoToEndOfWord ->
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
      recover "string bool token should always be convertable to bool" s false
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
    | ERecord (_, entries), _ ->
        let newEntries =
          (* looping through original set of tokens (before transforming them into tuples)
           * so we can get the index field *)
          tokensInRange startPos endPos ~state ast
          |> List.filterMap ~f:(fun ti ->
                 match ti.token with
                 | TRecordFieldname (_, _, index, newKey) ->
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
            recover "not a pexpr" data None
      with _ -> recover "could not decode" json None )
  | `Text text ->
      (* TODO: This is an OK first solution, but it doesn't allow us paste
         * into things like variable or key names. *)
      Some (EString (gid (), text))
  | `None ->
      None


let getStringIndex ti pos : int =
  match ti.token with
  | TString (_, _) ->
      pos - ti.startPos - 1
  | TStringMLStart (_, _, offset, _) ->
      pos - ti.startPos + offset - 1
  | TStringMLMiddle (_, _, offset, _) | TStringMLEnd (_, _, offset, _) ->
      pos - ti.startPos + offset
  | _ ->
      recover "getting index of non-string" (ti.token, pos) 0


let pasteOverSelection ~state ~ast data : ast * fluidState =
  let ast, state = deleteSelection ~state ~ast in
  let token = getToken state ast in
  let exprID = token |> Option.map ~f:(fun ti -> ti.token |> Token.tid) in
  let expr = Option.andThen exprID ~f:(fun id -> findExpr id ast) in
  let collapsedSelStart = fluidGetCollapsedSelectionStart state in
  let clipboardExpr = clipboardContentsToExpr ~state data in
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
  | Some clipboardExpr, Some (EString (_, str)), Some ti ->
      let insert = eToString state clipboardExpr |> trimQuotes in
      let index = getStringIndex ti state.newPos in
      let newExpr = EString (gid (), String.insertAt ~insert ~index str) in
      let newAST =
        exprID
        |> Option.map ~f:(fun id -> replaceExpr ~newExpr id ast)
        |> Option.withDefault ~default:ast
      in
      (* TODO: needs reflow: if the string becomes multi-line, we end up in the wrong place. *)
      let newPos = state.newPos + String.length insert in
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


let updateMsg m tlid (ast : ast) (msg : Types.fluidMsg) (s : fluidState) :
    ast * fluidState =
  (* TODO: The state should be updated from the last request, and so this
   * shouldn't be necessary, but the tests don't work without it *)
  let s = updateAutocomplete m tlid ast s in
  let newAST, newState =
    match msg with
    | FluidMouseClick _ ->
      (* TODO: if mouseclick on blank put cursor at beginning of it *)
      ( match Entry.getFluidCaretPos () with
      | Some newPos ->
          updateMouseClick newPos ast s
      | None ->
          (* We reset the fluidState to prevent the selection and/or cursor position from persisting
           * when a user switched handlers *)
          (ast, s |> acClear) )
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
    | FluidKeyPress {key; metaKey; ctrlKey}
      when (metaKey || ctrlKey) && shouldDoDefaultAction key ->
        (* To make sure no letters are entered if user is doing a browser default action *)
        (ast, s)
    | FluidKeyPress {key; shiftKey} ->
        let s = {s with lastKey = key} in
        let newAST, newState = updateKey key ast s in
        let selectionStart =
          if key = K.SelectAll
          then newState.selectionStart
          else if shiftKey
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
  | FluidStartSelection _
  | FluidKeyPress _
  | FluidCopy
  | FluidPaste _
  | FluidCut
  | FluidCommandsFilter _
  | FluidCommandsClick _
  | FluidMouseClick _
  | FluidAutocompleteClick _
  | FluidUpdateSelection _ ->
      let tlid =
        match msg with
        | FluidMouseClick tlid ->
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
              (* toggle through spec headers *)
              (* if on first spec header that is blank
               * set cursor to select that *)
              match tl with
              | TLHandler {spec = {name = Blank id; _}; _} ->
                  (enter id, ast, s)
              | TLHandler {spec = {space = Blank id; _}; _} ->
                  (enter id, ast, s)
              | TLHandler {spec = {modifier = Blank id; _}; _} ->
                  (enter id, ast, s)
              | _ ->
                  (NoChange, newAST, newState)
            else (NoChange, newAST, newState)
            (* the above logic is slightly duplicated from Selection.toggleBlankTypes *)
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
                FluidMsg (FluidAutocompleteClick item) ) ]
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


let viewErrorIndicator ~tlid ~analysisStore ~state ti : Types.msg Html.html =
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
      Html.div
        [ Html.class' (String.join ~sep:" " cls)
        ; Html.styles [("top", offset)]
        ; ViewUtils.eventNoPropagation
            ~key:("er-" ^ show_id id)
            "click"
            (fun _ -> TakeOffErrorRail (tlid, id)) ]
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
            ) ]
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
                          (FluidUpdateSelection
                             (tlid, getExpressionRangeAtCaret state ast))
                    | {detail = 2; altKey = false} ->
                        FluidMsg
                          (FluidUpdateSelection
                             (tlid, getTokenRangeAtCaret state ast))
                    | _ ->
                        (* We expect that this doesn't happen *)
                        FluidMsg (FluidUpdateSelection (tlid, None)) )
                | None ->
                    (* We expect that this doesn't happen *)
                    FluidMsg (FluidUpdateSelection (tlid, None)) )
          ; ViewUtils.eventNoPropagation
              ~key:("fluid-selection-mousedown" ^ idStr)
              "mousedown"
              (fun _ -> FluidMsg (FluidStartSelection tlid) )
          ; ViewUtils.eventNoPropagation
              ~key:("fluid-selection-mouseup" ^ idStr)
              "mouseup"
              (fun _ ->
                match Entry.getFluidSelectionRange () with
                | Some range ->
                    FluidMsg (FluidUpdateSelection (tlid, Some range))
                | None ->
                    (* This will happen if it gets a selection and there is no
                 focused node (weird browser problem?) *)
                    IgnoreMsg )
          ; ViewUtils.eventNoPropagation
              ~key:
                ( "fluid-selection-click-"
                ^ idStr
                ^ "-"
                ^
                match state.selectionStart with
                | Some x ->
                    string_of_int x
                | None ->
                    "nosel" )
              "click"
              (fun ev ->
                match Entry.getFluidSelectionRange () with
                | Some range ->
                    if ev.shiftKey
                    then FluidMsg (FluidUpdateSelection (tlid, Some range))
                    else FluidMsg (FluidUpdateSelection (tlid, None))
                | None ->
                    (* This will happen if it gets a selection and there is no
                     focused node (weird browser problem?) *)
                    IgnoreMsg ) ]
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
  let liveValue, show, offset =
    let none = ([Vdom.noNode], false, 0) in
    getToken state ast
    |> Option.map ~f:(fun ti ->
           let fn = fnForToken state ti.token in
           let args = fnArgExprs ti.token ast state |> List.map ~f:eid in
           let row = ti.startRow in
           let id = Token.analysisID ti.token in
           let fnText =
             Option.and_then fn ~f:(fun fn ->
                 if fn.fnPreviewExecutionSafe
                 then None
                 else
                   let id = Token.tid ti.token in
                   ViewFnExecution.fnExecutionStatus vs fn id args
                   |> ViewFnExecution.executionError
                   |> Option.some )
           in
           if FluidToken.validID id
           then
             let loadable =
               Analysis.getLiveValueLoadable vs.analysisStore id
             in
             match (AC.highlighted state.ac, loadable) with
             | None, LoadableSuccess (DIncomplete _) when Option.isSome fnText
               ->
                 let text = Option.withDefault ~default:"" fnText in
                 ([Html.text text], true, ti.startRow)
             | Some (FACVariable (_, Some dval)), _
             | None, LoadableSuccess dval ->
                 let text = Runtime.toRepr dval in
                 let copyBtn =
                   match dval with
                   | DIncomplete _ | DError _ ->
                       Vdom.noNode
                   | _ ->
                       viewCopyButton tlid text
                 in
                 ([Html.text text; copyBtn], true, ti.startRow)
             | Some _, _ ->
                 none
             | None, LoadableNotInitialized | None, LoadableLoading _ ->
                 ([ViewUtils.fontAwesome "spinner"], true, row)
             | None, LoadableError err ->
                 ([Html.text ("Error loading live value: " ^ err)], true, row)
           else none )
    |> Option.withDefault ~default:none
  in
  let offset = float_of_int offset +. 1.5 in
  Html.div
    [ Html.classList [("live-values", true); ("show", show)]
    ; Html.styles [("top", Js.Float.toString offset ^ "rem")]
    ; Attrs.autofocus false
    ; Vdom.attribute "" "spellcheck" "false" ]
    liveValue


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
      |> List.map ~f:(fun ti ->
             viewErrorIndicator ~tlid ~analysisStore ~state ti )
    in
    let hasMaybeErrors = List.any ~f:(fun e -> e <> Vdom.noNode) indicators in
    Html.div
      [Html.classList [("fluid-error-rail", true); ("show", hasMaybeErrors)]]
      indicators
  in
  let liveValue =
    if tlidOf vs.cursorState = Some tlid
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
  let posDiv =
    let oldGrid = gridFor ~pos:s.oldPos tokens in
    let newGrid = gridFor ~pos:s.newPos tokens in
    [ Html.div
        []
        [ Html.text (string_of_int s.oldPos)
        ; Html.text " -> "
        ; Html.text (string_of_int s.newPos) ]
    ; Html.div
        []
        [ Html.text (oldGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (oldGrid.row |> string_of_int)
        ; Html.text " -> "
        ; Html.text (newGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (newGrid.row |> string_of_int) ]
    ; Html.div
        []
        [ Html.text "acIndex: "
        ; Html.text
            ( s.ac.index
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; Html.div
        []
        [ Html.text "upDownCol: "
        ; Html.text
            ( s.upDownCol
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; Html.div
        []
        [ Html.text "lastKey: "
        ; Html.text
            ( K.toName s.lastKey
            ^ ", "
            ^ ( K.toChar s.lastKey
              |> Option.map ~f:String.fromChar
              |> Option.withDefault ~default:"" ) ) ]
    ; Html.div
        []
        [ Html.text "selection: "
        ; Html.text
            ( s.selectionStart
            |> Option.map ~f:(fun selStart ->
                   string_of_int selStart ^ "->" ^ string_of_int s.newPos )
            |> Option.withDefault ~default:"" ) ] ]
  in
  let tokenDiv =
    let left, right, next = getNeighbours tokens ~pos:s.newPos in
    let l =
      match left with
      | L (_, left) ->
          Token.show_tokenInfo left
      | R (_, _) ->
          "right"
      | No ->
          "none"
    in
    let r =
      match right with
      | L (_, _) ->
          "left"
      | R (_, right) ->
          Token.show_tokenInfo right
      | No ->
          "none"
    in
    let n =
      match next with Some next -> Token.show_tokenInfo next | None -> "none"
    in
    [ Html.text ("left: " ^ l)
    ; Html.br []
    ; Html.text ("right: " ^ r)
    ; Html.br []
    ; Html.text ("next: " ^ n) ]
  in
  let actions =
    [ Html.div
        []
        ( [Html.text "Actions: "]
        @ ( s.actions
          |> List.map ~f:(fun action -> [action; ", "])
          |> List.concat
          |> List.dropRight ~count:1
          |> List.map ~f:Html.text ) ) ]
  in
  let error =
    [ Html.div
        []
        [ Html.text
            ( Option.map s.error ~f:(fun e -> "Errors: " ^ e)
            |> Option.withDefault ~default:"none" ) ] ]
  in
  let status = List.concat [posDiv; tokenDiv; actions; error] in
  Html.div [Attrs.id "fluid-status"] status


(* -------------------- *)
(* Scaffolidng *)
(* -------------------- *)

let selectedASTAsText (m : model) : string option =
  let s = m.fluidState in
  TL.selectedAST m |> Option.map ~f:(fromExpr s) |> Option.map ~f:(eToString s)


let renderCallback (m : model) : unit =
  match m.cursorState with
  | FluidEntering _ ->
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
