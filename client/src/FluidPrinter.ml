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
module T = FluidToken
module E = FluidExpression
module Pattern = FluidPattern
module Util = FluidUtil

type token = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

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
    let tokenLength = token |> T.toText |> String.length in
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

(** patternToToken takes a match pattern `p` and converts it to a list of
    fluidTokens.

    ~idx is the zero-based index of the pattern in the enclosing match *)
let rec patternToToken (p : fluidPattern) ~(idx : int) : fluidToken list =
  match p with
  | FPVariable (mid, id, name) ->
      [TPatternVariable (mid, id, name, idx)]
  | FPConstructor (mid, id, name, args) ->
      let args = List.map args ~f:(fun a -> TSep id :: patternToToken a ~idx) in
      List.concat ([TPatternConstructorName (mid, id, name, idx)] :: args)
  | FPInteger (mid, id, i) ->
      [TPatternInteger (mid, id, i, idx)]
  | FPBool (mid, id, b) ->
      if b then [TPatternTrue (mid, id, idx)] else [TPatternFalse (mid, id, idx)]
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


let rec toTokens' (e : E.t) (b : Builder.t) : Builder.t =
  let fromExpr e b = toTokens' e b in
  let open Builder in
  let ghostPartial id newName oldName =
    let ghostSuffix = String.dropLeft ~count:(String.length newName) oldName in
    if ghostSuffix = "" then [] else [TPartialGhost (id, ghostSuffix)]
  in
  let nest
      ?(placeholderFor : (string * int) option = None)
      ~indent
      (e : E.t)
      (b : Builder.t) : Builder.t =
    let tokensFn b =
      match (e, placeholderFor) with
      | EBlank id, Some (fnname, pos) ->
          let name =
            !E.functions
            |> List.find ~f:(fun f -> f.fnName = fnname)
            |> Option.andThen ~f:(fun fn ->
                   List.getAt ~index:pos fn.fnParameters)
            |> Option.map ~f:(fun p ->
                   (p.paramName, Runtime.tipe2str p.paramTipe))
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
  let addArgs (name : string) (id : id) (args : E.t list) (b : Builder.t) :
      Builder.t =
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
        |> List.map ~f:(T.toText >> String.length)
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
             |> add (TSep (E.id e))
             |> nest ~indent:0 ~placeholderFor:(Some (name, offset + i)) e)
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
      |> addNewlineIfNeeded (Some (E.id next, id, None))
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
                          |> add (TNewline None))
                   |> add (TStringMLEnd (id, ending, endingOffset, str))) ) )
  | EIf (id, cond, if', else') ->
      b
      |> add (TIfKeyword id)
      |> addNested ~f:(fromExpr cond)
      |> addNewlineIfNeeded None
      |> add (TIfThenKeyword id)
      |> addNewlineIfNeeded (Some (E.id if', id, None))
      |> nest ~indent:2 if'
      |> addNewlineIfNeeded None
      |> add (TIfElseKeyword id)
      |> add (TNewline (Some (E.id else', id, None)))
      |> nest ~indent:2 else'
  | EBinOp (id, op, lexpr, rexpr, _ster) ->
      let start b =
        match lexpr with
        | EPipeTarget _ ->
            b
        | _ ->
            b
            |> nest ~indent:0 ~placeholderFor:(Some (op, 0)) lexpr
            |> add (TSep (E.id lexpr))
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
            |> add (TSep (E.id lexpr))
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
      let newText = T.toText partial in
      let oldText = ViewUtils.partialName oldName in
      let ghost = ghostPartial id newText oldText in
      b |> add partial |> addMany ghost |> addArgs oldName id args
  | EConstructor (id, _, name, exprs) ->
      b |> add (TConstructorName (id, name)) |> addArgs name id exprs
  | EPartial (id, newName, EConstructor (_, _, oldName, exprs)) ->
      let partial = TPartial (id, newName) in
      let newText = T.toText partial in
      let ghost = ghostPartial id newText oldName in
      b |> add partial |> addMany ghost |> addArgs oldName id exprs
  | EFieldAccess (id, expr, fieldID, fieldname) ->
      b
      |> addNested ~f:(fromExpr expr)
      |> addMany
           [ TFieldOp (id, (* lhs *) E.id expr)
           ; TFieldName (id, fieldID, fieldname) ]
  | EPartial (id, newFieldname, EFieldAccess (faID, expr, fieldID, oldFieldname))
    ->
      let partial = TFieldPartial (id, faID, fieldID, newFieldname) in
      let newText = T.toText partial in
      let ghost = ghostPartial id newText oldFieldname in
      b
      |> addNested ~f:(fromExpr expr)
      |> addMany [TFieldOp (id, E.id expr); partial]
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
             |> addIf (not (isLast i)) (TSep aid))
      |> add (TLambdaArrow id)
      |> nest ~indent:2 body
  | EList (id, exprs) ->
      let lastIndex = List.length exprs - 1 in
      b
      |> add (TListOpen id)
      |> addIter exprs ~f:(fun i e b ->
             b
             |> addNested ~f:(fromExpr e)
             |> addIf (i <> lastIndex) (TListSep (id, i)))
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
                   |> addNested ~f:(fromExpr expr)))
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
          |> addNewlineIfNeeded (Some (E.id head, id, Some 0))
          |> addIter tail ~f:(fun i e b ->
                 b
                 |> add (TPipe (id, i, length))
                 |> addNested ~f:(fromExpr e)
                 |> addNewlineIfNeeded (Some (E.id e, id, Some (i + 1))))
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
                         ; TMatchSep (Pattern.id pattern, i)
                         ; TSep (Pattern.id pattern) ]
                    |> addNested ~f:(fromExpr expr))
             |> addNewlineIfNeeded (Some (id, id, Some (List.length pairs))))
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
        let length = String.length (T.toText token) in
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
      asserT "invalid token" (String.length (T.toText t) > 0) ~debug:t ;
      ()) ;
  tokens


(* Remove artifacts of the token generation process *)
let tidy (tokens : fluidToken list) : fluidToken list =
  tokens |> List.filter ~f:(function TIndent 0 -> false | _ -> true)


let toTokens (e : E.t) : tokenInfo list =
  toTokens' e Builder.empty
  |> Builder.asTokens
  |> tidy
  |> validateTokens
  |> infoize ~pos:0


let eToString (e : E.t) : string =
  e
  |> toTokens
  |> List.map ~f:(fun ti -> T.toTestText ti.token)
  |> String.join ~sep:""


let tokensToString (tis : tokenInfo list) : string =
  tis |> List.map ~f:(fun ti -> T.toText ti.token) |> String.join ~sep:""


let eToStructure ?(includeIDs = false) (e : E.t) : string =
  e
  |> toTokens
  |> List.map ~f:(fun ti ->
         "<"
         ^ T.toTypeName ti.token
         ^ (if includeIDs then "(" ^ (T.tid ti.token |> deID) ^ ")" else "")
         ^ ":"
         ^ T.toText ti.token
         ^ ">")
  |> String.join ~sep:""


let pToString (p : fluidPattern) : string =
  p
  |> patternToToken ~idx:0
  |> List.map ~f:(fun t -> T.toTestText t)
  |> String.join ~sep:""


let pToStructure (p : fluidPattern) : string =
  p
  |> patternToToken ~idx:0
  |> infoize ~pos:0
  |> List.map ~f:(fun ti ->
         "<" ^ T.toTypeName ti.token ^ ":" ^ T.toText ti.token ^ ">")
  |> String.join ~sep:""


let nexprToString (e : Types.expr) : string =
  e |> FluidExpression.fromNExpr |> eToString
