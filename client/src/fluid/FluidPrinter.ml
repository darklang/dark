open Prelude
module T = FluidToken
module E = FluidExpression
module Pattern = FluidPattern
module Util = FluidUtil

type token = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

type featureFlagTokenization =
  | FeatureFlagOnlyDisabled
      (** FeatureFlagOnlyDisabled is used in the main editor panel to only
          * show the flag's old code *)
  | FeatureFlagConditionAndEnabled
      (** FeatureFlagConditionAndEnabled is used in the secondary editor
          * panel for editing a flag's condition and new code *)

module Builder = struct
  type t =
    { tokens : fluidToken list
          (** [tokens] list is kept reversed while being built up, as adding
            * things to the front of the list is an order of magnitude faster.
            * We were having large slowdowns on large handlers before this. *)
    ; indent : int  (** [indent] tracks the indent after a newline *)
    ; xPos : int option
          (** [xPos] tracks the indent for nesting.
            * `None` indicates it's ready to go after a newline *)
    ; ffTokenization : featureFlagTokenization }

  let rec endsInNewline (b : t) : bool =
    (* The latest token is on the front *)
    match b.tokens with
    | TNewline _ :: _ ->
        true
    | TIndent _ :: tail ->
        endsInNewline {b with tokens = tail}
    | _ ->
        false


  let empty =
    { tokens = []
    ; xPos = Some 0
    ; indent = 0
    ; ffTokenization = FeatureFlagOnlyDisabled }


  (** id is the id of the first token in the builder or None if the builder is empty. *)
  let id (b : t) : ID.t option = List.last b.tokens |> Option.map ~f:T.tid

  let lineLimit = 120

  let strLimit = 40

  let listLimit = 60

  let add (token : fluidToken) (b : t) : t =
    let tokenLength = token |> T.toText |> String.length in
    let newTokens, xPos =
      (* Add new tokens on the front *)
      if endsInNewline b
      then
        ( ( if b.indent <> 0
          then token :: TIndent b.indent :: b.tokens
          else token :: b.tokens )
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
        (token :: b.tokens, newXPos)
    in
    {b with tokens = newTokens; xPos}


  let addIf (cond : bool) (token : fluidToken) (b : t) : t =
    if cond then add token b else b


  (* Take a list of 'a, and iterate through them, adding them to `b` by
   * calling `f` on them *)
  let addIter (xs : 'a list) ~(f : int -> 'a -> t -> t) (b : t) : t =
    List.foldl xs ~init:(b, 0) ~f:(fun x (b, i) -> (f i x b, i + 1))
    |> Tuple2.first


  let addMany (tokens : fluidToken list) (b : t) : t =
    List.foldl tokens ~init:b ~f:add


  (** [indentBy ~ident ~f b] calls [f] with a modified [b] having additional
    * indentation by [indent] characters (that is, b.indent + ~indent), then
    * returns the result of that [f b] invocation with the original indent of
    * [b] restored. *)
  let indentBy ~(indent : int) ~(f : t -> t) (b : t) : t =
    let oldIndent = b.indent in
    {b with indent = b.indent + indent}
    |> f
    |> fun b -> {b with indent = oldIndent}


  let addNested ~(f : t -> t) (b : t) : t =
    let oldIndent = b.indent in
    let newIndent = Option.withDefault ~default:b.indent b.xPos in
    {b with indent = newIndent} |> f |> fun b -> {b with indent = oldIndent}


  let addNewlineIfNeeded (nlInfo : (ID.t * ID.t * int option) option) (b : t) :
      t =
    if endsInNewline b then b else add (TNewline nlInfo) b


  let asTokens (b : t) : fluidToken list =
    (* Tokens are stored reversed *)
    List.reverse b.tokens
end

(** patternToToken takes a match pattern `p` and converts it to a list of
    fluidTokens.

    ~idx is the zero-based index of the pattern in the enclosing match *)
let rec patternToToken (p : FluidPattern.t) ~(idx : int) : fluidToken list =
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
  | FPString {matchID = mid; patternID = id; str} ->
      [TPatternString {matchID = mid; patternID = id; str; branchIdx = idx}]
  | FPFloat (mID, id, whole, fraction) ->
      let whole =
        if whole = "" then [] else [TPatternFloatWhole (mID, id, whole, idx)]
      in
      let fraction =
        if fraction = ""
        then []
        else [TPatternFloatFractional (mID, id, fraction, idx)]
      in
      whole @ [TPatternFloatPoint (mID, id, idx)] @ fraction
  | FPNull (mid, id) ->
      [TPatternNullToken (mid, id, idx)]
  | FPBlank (mid, id) ->
      [TPatternBlank (mid, id, idx)]


let rec toTokens' (e : E.t) (b : Builder.t) : Builder.t =
  let open Builder in
  let ghostPartial id newName oldName =
    let ghostSuffix = String.dropLeft ~count:(String.length newName) oldName in
    if ghostSuffix = "" then [] else [TPartialGhost (id, ghostSuffix)]
  in
  (* placeholderFor = (id * string * int)
   * id: id of the placeholder-containing expr
   * string: name of the placeholder-containing expr
   * int: index of the placeholder within the expr's parameters
   *)
  let nest
      ?(placeholderFor : (ID.t * string * int) option = None)
      ~indent
      (e : E.t)
      (b : Builder.t) : Builder.t =
    let tokensFn b =
      match (e, placeholderFor) with
      | EBlank id, Some (fnID, fnname, pos) ->
          let name =
            !OldExpr.functions
            |> List.find ~f:(fun f -> f.fnName = fnname)
            |> Option.andThen ~f:(fun fn ->
                   List.getAt ~index:pos fn.fnParameters)
            |> Option.map ~f:(fun p ->
                   {name = p.paramName; tipe = tipe2str p.paramTipe})
          in
          ( match name with
          | None ->
              toTokens' e b
          | Some placeholder ->
              add (TPlaceholder {blankID = id; placeholder; fnID}) b )
      | _ ->
          toTokens' e b
    in
    b |> indentBy ~indent ~f:(addNested ~f:tokensFn)
  in
  let addArgs (name : string) (id : ID.t) (args : E.t list) (b : Builder.t) :
      Builder.t =
    let args, offset =
      match args with EPipeTarget _ :: args -> (args, 1) | _ -> (args, 0)
    in
    let reflow =
      let tokens =
        args
        |> List.map ~f:(fun a -> toTokens' a Builder.empty)
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
      let tooLong = length > lineLimit in
      let needsNewlineBreak =
        (* newlines aren't disruptive in the last argument *)
        args
        |> List.init
        |> Option.withDefault ~default:[]
        |> List.map ~f:(fun a -> toTokens' a Builder.empty)
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
             |> nest ~indent:2 ~placeholderFor:(Some (id, name, offset + i)) e
           else
             b
             |> add (TSep (E.toID e))
             |> nest ~indent:0 ~placeholderFor:(Some (id, name, offset + i)) e)
  in
  match e with
  | EInteger (id, i) ->
      b |> add (TInteger (id, i))
  | EBool (id, bool') ->
      b |> add (if bool' then TTrue id else TFalse id)
  | ENull id ->
      b |> add (TNullToken id)
  | EFloat (id, whole, fraction) ->
      let whole = if whole = "" then [] else [TFloatWhole (id, whole)] in
      let fraction =
        if fraction = "" then [] else [TFloatFractional (id, fraction)]
      in
      b |> addMany (whole @ [TFloatPoint id] @ fraction)
  | EBlank id ->
      b |> add (TBlank id)
  | ELet (id, lhs, rhs, next) ->
      let rhsID = E.toID rhs in
      b
      |> add (TLetKeyword (id, rhsID))
      |> add (TLetVarName (id, rhsID, lhs))
      |> add (TLetAssignment (id, rhsID))
      |> addNested ~f:(toTokens' rhs)
      |> addNewlineIfNeeded (Some (E.toID next, id, None))
      |> addNested ~f:(toTokens' next)
  | EString (id, str) ->
      let strings =
        if String.length str > strLimit
        then String.segment ~size:strLimit str
        else [str]
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
                   let endingOffset = strLimit * (List.length revrest + 1) in
                   b
                   |> add (TStringMLStart (id, starting, 0, str))
                   |> add (TNewline None)
                   |> addIter (List.reverse revrest) ~f:(fun i s b ->
                          b
                          |> add
                               (TStringMLMiddle (id, s, strLimit * (i + 1), str))
                          |> add (TNewline None))
                   |> add (TStringMLEnd (id, ending, endingOffset, str))) ) )
  | EIf (id, cond, if', else') ->
      b
      |> add (TIfKeyword id)
      |> addNested ~f:(toTokens' cond)
      |> addNewlineIfNeeded None
      |> add (TIfThenKeyword id)
      |> addNewlineIfNeeded (Some (E.toID if', id, None))
      |> nest ~indent:2 if'
      |> addNewlineIfNeeded None
      |> add (TIfElseKeyword id)
      |> add (TNewline (Some (E.toID else', id, None)))
      |> nest ~indent:2 else'
  | EBinOp (id, op, lexpr, rexpr, _ster) ->
      let start b =
        match lexpr with
        | EPipeTarget _ ->
            b
        | _ ->
            b
            |> nest ~indent:0 ~placeholderFor:(Some (id, op, 0)) lexpr
            |> add (TSep (E.toID lexpr))
      in
      b
      |> start
      |> addMany [TBinOp (id, op); TSep id]
      |> nest ~indent:0 ~placeholderFor:(Some (id, op, 1)) rexpr
  | EPartial (id, newName, EBinOp (_, oldName, lexpr, rexpr, _ster)) ->
      let ghost =
        ghostPartial id newName (FluidUtil.ghostPartialName oldName)
      in
      let start b =
        match lexpr with
        | EPipeTarget _ ->
            b
        | _ ->
            b
            |> nest ~indent:0 ~placeholderFor:(Some (id, oldName, 0)) lexpr
            |> add (TSep (E.toID lexpr))
      in
      b
      |> start
      |> add (TPartial (id, newName))
      |> addMany ghost
      |> add (TSep id)
      |> nest ~indent:2 ~placeholderFor:(Some (id, oldName, 1)) rexpr
  | EFnCall (id, fnName, args, ster) ->
      let displayName = FluidUtil.fnDisplayName fnName in
      let versionDisplayName = FluidUtil.versionDisplayName fnName in
      let partialName = FluidUtil.fnDisplayNameWithVersion fnName in
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
      let oldText = FluidUtil.ghostPartialName oldName in
      let ghost = ghostPartial id newText oldText in
      b |> add partial |> addMany ghost |> addArgs oldName id args
  | EConstructor (id, name, exprs) ->
      b |> add (TConstructorName (id, name)) |> addArgs name id exprs
  | EPartial (id, newName, EConstructor (_, oldName, exprs)) ->
      let partial = TPartial (id, newName) in
      let newText = T.toText partial in
      let ghost = ghostPartial id newText oldName in
      b |> add partial |> addMany ghost |> addArgs oldName id exprs
  | EFieldAccess (id, expr, fieldname) ->
      let lhsid = E.toID expr in
      b
      |> addNested ~f:(toTokens' expr)
      |> addMany [TFieldOp (id, lhsid); TFieldName (id, lhsid, fieldname)]
  | EPartial (id, newFieldname, EFieldAccess (faID, expr, oldFieldname)) ->
      let lhsid = E.toID expr in
      let partial = TFieldPartial (id, faID, lhsid, newFieldname) in
      let newText = T.toText partial in
      let ghost = ghostPartial id newText oldFieldname in
      b
      |> addNested ~f:(toTokens' expr)
      |> addMany [TFieldOp (id, E.toID expr); partial]
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
             |> addIf (not (isLast i)) (TLambdaComma (id, i))
             |> addIf (not (isLast i)) (TSep aid))
      |> add (TLambdaArrow id)
      |> nest ~indent:2 body
  | EList (id, exprs) ->
      (*
         With each iteration of the list, we calculate the new line length, if we were to add this new item. If the new line length exceeds the limit, then we add a new line token and an indent by 1 first, before adding the tokenized item to the builder.
      *)
      let lastIndex = List.length exprs - 1 in
      let xOffset = b.xPos |> Option.withDefault ~default:0 in
      b
      |> add (TListOpen id)
      |> addIter exprs ~f:(fun i e b' ->
             let currentLineLength =
               let commaWidth = if i <> lastIndex then 1 else 0 in
               (toTokens' e b').xPos
               |> Option.map ~f:(fun x -> x - xOffset + commaWidth)
               |> Option.withDefault ~default:commaWidth
             in
             (* Even if first element overflows, don't put it in a new line *)
             let isOverLimit = i > 0 && currentLineLength > listLimit in
             (* Indent after newlines to match the '[ ' *)
             let indent = if isOverLimit then 1 else 0 in
             b'
             |> addIf isOverLimit (TNewline None)
             |> indentBy ~indent ~f:(fun b' ->
                    b'
                    |> addNested ~f:(toTokens' e)
                    |> addIf (i <> lastIndex) (TListComma (id, i))))
      |> add (TListClose id)
  | ERecord (id, fields) ->
      if fields = []
      then b |> addMany [TRecordOpen id; TRecordClose id]
      else
        b
        |> add (TRecordOpen id)
        |> indentBy ~indent:2 ~f:(fun b ->
               addIter fields b ~f:(fun i (fieldName, expr) b ->
                   let exprID = E.toID expr in
                   b
                   |> addNewlineIfNeeded (Some (id, id, Some i))
                   |> add
                        (TRecordFieldname
                           {recordID = id; exprID; index = i; fieldName})
                   |> add (TRecordSep (id, i, exprID))
                   |> addNested ~f:(toTokens' expr)))
        |> addMany
             [ TNewline (Some (id, id, Some (List.length fields)))
             ; TRecordClose id ]
  | EPipe (id, exprs) ->
      let length = List.length exprs in
      ( match exprs with
      | [] ->
          recover "Empty pipe found" ~debug:e b
      | [single] ->
          recover "pipe with single entry found" ~debug:e (toTokens' single b)
      | head :: tail ->
          b
          |> addNested ~f:(toTokens' head)
          |> addNewlineIfNeeded (Some (E.toID head, id, Some 0))
          |> addIter tail ~f:(fun i e b ->
                 b
                 |> add (TPipe (id, i, length))
                 |> addNested ~f:(toTokens' e)
                 |> addNewlineIfNeeded (Some (E.toID e, id, Some (i + 1))))
          |> addNewlineIfNeeded (Some (id, id, Some (List.length tail))) )
  | EPipeTarget _ ->
      recover "should never be making tokens for EPipeTarget" ~debug:e b
  | EMatch (id, mexpr, pairs) ->
      b
      |> add (TMatchKeyword id)
      |> addNested ~f:(toTokens' mexpr)
      |> indentBy ~indent:2 ~f:(fun b ->
             b
             |> addIter pairs ~f:(fun i (pattern, expr) b ->
                    b
                    |> addNewlineIfNeeded (Some (id, id, Some i))
                    |> addMany (patternToToken pattern ~idx:i)
                    |> add
                         (TMatchBranchArrow
                            { matchID = id
                            ; patternID = Pattern.toID pattern
                            ; index = i })
                    |> addNested ~f:(toTokens' expr))
             |> addNewlineIfNeeded (Some (id, id, Some (List.length pairs))))
  | EPartial (id, str, _) ->
      b |> add (TPartial (id, str))
  | ERightPartial (id, newOp, expr) ->
      b
      |> addNested ~f:(toTokens' expr)
      |> addMany [TSep id; TRightPartial (id, newOp)]
  | EFeatureFlag (id, _name, cond, disabled, enabled) ->
    (* Feature flag tokens are displayed in two different editor panels, so
     * they are built differently depending on the current builder option. *)
    ( match b.ffTokenization with
    | FeatureFlagOnlyDisabled ->
        b |> addNested ~f:(toTokens' disabled)
    | FeatureFlagConditionAndEnabled ->
        b
        |> add (TFlagWhenKeyword id)
        |> addNested ~f:(toTokens' cond)
        |> addNewlineIfNeeded None
        |> add (TFlagEnabledKeyword id)
        |> addNewlineIfNeeded (Some (E.toID enabled, id, None))
        |> nest ~indent:2 enabled )


let infoize tokens : tokenInfo list =
  let row, col, pos = (ref 0, ref 0, ref 0) in
  List.map tokens ~f:(fun token ->
      let length = String.length (T.toText token) in
      let ti =
        { token
        ; startRow = !row
        ; startCol = !col
        ; startPos = !pos
        ; endPos = !pos + length
        ; length }
      in
      ( match token with
      | TNewline _ ->
          row := !row + 1 ;
          col := 0
      | _ ->
          col := !col + length ) ;
      pos := !pos + length ;
      ti)


let validateTokens (tokens : fluidToken list) : fluidToken list =
  List.iter tokens ~f:(fun t ->
      asserT "invalid token" (String.length (T.toText t) > 0) ~debug:t ;
      ()) ;
  tokens


(* Remove artifacts of the token generation process *)
let tidy (tokens : fluidToken list) : fluidToken list =
  tokens |> List.filter ~f:(function TIndent 0 -> false | _ -> true)


let tokenizeWithFFTokenization
    (ffTokenization : featureFlagTokenization) (e : FluidExpression.t) :
    tokenInfo list =
  {Builder.empty with ffTokenization}
  |> toTokens' e
  |> Builder.asTokens
  |> tidy
  |> validateTokens
  |> infoize


let tokenize : E.t -> FluidToken.tokenInfo list =
  tokenizeWithFFTokenization FeatureFlagOnlyDisabled


let tokenizeForPanel (k : FluidPanel.kind) (expr : FluidExpression.t) :
    FluidToken.tokenInfo list =
  match k with
  | FluidPanel.FeatureFlag ->
      tokenizeWithFFTokenization FeatureFlagConditionAndEnabled expr


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
  |> patternToToken ~idx:0
  |> List.map ~f:(fun t -> T.toTestText t)
  |> String.join ~sep:""


let pToStructure (p : fluidPattern) : string =
  p
  |> patternToToken ~idx:0
  |> infoize
  |> List.map ~f:(fun ti ->
         "<" ^ T.toTypeName ti.token ^ ":" ^ T.toText ti.token ^ ">")
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
    | EFeatureFlag _ ->
        "todo: feature flag: " ^ eToHumanString e
  in
  "(" ^ result ^ ")"
