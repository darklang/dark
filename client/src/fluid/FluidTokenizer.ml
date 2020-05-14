open Prelude
module T = FluidToken
module E = FluidExpression
module Pattern = FluidPattern

type tokenInfo = Types.fluidTokenInfo

type featureFlagTokenization =
  | FeatureFlagOnlyDisabled
      (** FeatureFlagOnlyDisabled is used in the main editor panel to only
          * show the flag's old code *)
  | FeatureFlagConditionAndEnabled
      (** FeatureFlagConditionAndEnabled is used in the secondary editor
          * panel for editing a flag's condition and new code *)

(* -------------------------------------- *)
(* Convert FluidExpressions to tokenInfos *)
(* -------------------------------------- *)

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
      let args =
        List.map args ~f:(fun a -> TSep (id, None) :: patternToToken a ~idx)
      in
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


let rec toTokens' ?(parentID = None) (e : E.t) (b : Builder.t) : Builder.t =
  let open Builder in
  let ghostPartial id newName oldName =
    let ghostSuffix = String.dropLeft ~count:(String.length newName) oldName in
    if ghostSuffix = "" then [] else [TPartialGhost (id, ghostSuffix, None)]
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
            Functions.global ()
            |> Functions.find fnname
            |> Option.andThen ~f:(fun fn ->
                   List.getAt ~index:pos fn.fnParameters)
            |> Option.map ~f:(fun p ->
                   {name = p.paramName; tipe = tipe2str p.paramTipe})
          in
          ( match name with
          | None ->
              toTokens' e b
          | Some placeholder ->
              add
                (TPlaceholder
                   {blankID = id; parentBlockID = Some fnID; placeholder; fnID})
                b )
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
             |> add (TSep (E.toID e, None))
             |> nest ~indent:0 ~placeholderFor:(Some (id, name, offset + i)) e)
  in
  match e with
  | EInteger (id, i) ->
      b |> add (TInteger (id, i, parentID))
  | EBool (id, bool') ->
      b |> add (if bool' then TTrue (id, parentID) else TFalse (id, parentID))
  | ENull id ->
      b |> add (TNullToken (id, parentID))
  | EFloat (id, whole, fraction) ->
      let whole =
        if whole = "" then [] else [TFloatWhole (id, whole, parentID)]
      in
      let fraction =
        if fraction = "" then [] else [TFloatFractional (id, fraction, parentID)]
      in
      b |> addMany (whole @ [TFloatPoint (id, parentID)] @ fraction)
  | EBlank id ->
      b |> add (TBlank (id, parentID))
  | ELet (id, lhs, rhs, next) ->
      let rhsID = E.toID rhs in
      b
      |> add (TLetKeyword (id, rhsID, parentID))
      |> add (TLetVarName (id, rhsID, lhs, parentID))
      |> add (TLetAssignment (id, rhsID, parentID))
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
          add (TString (id, "", parentID)) b
      | starting :: rest ->
        ( match List.reverse rest with
        | [] ->
            add (TString (id, str, parentID)) b
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
      |> add (TIfKeyword (id, parentID))
      |> addNested ~f:(toTokens' cond)
      |> addNewlineIfNeeded None
      |> add (TIfThenKeyword (id, parentID))
      |> addNewlineIfNeeded (Some (E.toID if', id, None))
      |> nest ~indent:2 if'
      |> addNewlineIfNeeded None
      |> add (TIfElseKeyword (id, parentID))
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
            |> add (TSep (E.toID lexpr, parentID))
      in
      b
      |> start
      |> addMany [TBinOp (id, op, parentID); TSep (id, parentID)]
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
            |> add (TSep (E.toID lexpr, parentID))
      in
      b
      |> start
      |> add (TPartial (id, newName, parentID))
      |> addMany ghost
      |> add (TSep (id, parentID))
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
      let partial = TPartial (id, newName, parentID) in
      let newText = T.toText partial in
      let oldText = FluidUtil.ghostPartialName oldName in
      let ghost = ghostPartial id newText oldText in
      b |> add partial |> addMany ghost |> addArgs oldName id args
  | EConstructor (id, name, exprs) ->
      b |> add (TConstructorName (id, name)) |> addArgs name id exprs
  | EPartial (id, newName, EConstructor (_, oldName, exprs)) ->
      let partial = TPartial (id, newName, parentID) in
      let newText = T.toText partial in
      let ghost = ghostPartial id newText oldName in
      b |> add partial |> addMany ghost |> addArgs oldName id exprs
  | EFieldAccess (id, expr, fieldname) ->
      let lhsid = E.toID expr in
      b
      |> addNested ~f:(toTokens' expr ~parentID)
      |> addMany
           [ TFieldOp (id, lhsid, parentID)
           ; TFieldName (id, lhsid, fieldname, parentID) ]
  | EPartial (id, newFieldname, EFieldAccess (faID, expr, oldFieldname)) ->
      let lhsid = E.toID expr in
      let partial = TFieldPartial (id, faID, lhsid, newFieldname, parentID) in
      let newText = T.toText partial in
      let ghost = ghostPartial id newText oldFieldname in
      b
      |> addNested ~f:(toTokens' expr)
      |> addMany [TFieldOp (id, E.toID expr, parentID); partial]
      |> addMany ghost
  | EVariable (id, name) ->
      b |> add (TVariable (id, name, parentID))
  | ELambda (id, names, body) ->
      let isLast i = i = List.length names - 1 in
      b
      |> add (TLambdaSymbol (id, parentID))
      |> addIter names ~f:(fun i (aid, name) b ->
             b
             |> add (TLambdaVar (id, aid, i, name, parentID))
             |> addIf (not (isLast i)) (TLambdaComma (id, i, parentID))
             |> addIf (not (isLast i)) (TSep (aid, parentID)))
      |> add (TLambdaArrow (id, parentID))
      |> nest ~indent:2 body
  | EList (id, exprs) ->
      (*
         With each iteration of the list, we calculate the new line length, if we were to add this new item. If the new line length exceeds the limit, then we add a new line token and an indent by 1 first, before adding the tokenized item to the builder.
      *)
      let lastIndex = List.length exprs - 1 in
      let xOffset = b.xPos |> Option.withDefault ~default:0 in
      let pid = if lastIndex = -1 then None else Some id in
      b
      |> add (TListOpen (id, pid))
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
                    |> addNested ~f:(toTokens' ~parentID:(Some id) e)
                    |> addIf (i <> lastIndex) (TListComma (id, i))))
      |> add (TListClose (id, pid))
  | ERecord (id, fields) ->
      if fields = []
      then b |> addMany [TRecordOpen (id, None); TRecordClose (id, None)]
      else
        let parentBlockID = Some id in
        b
        |> add (TRecordOpen (id, parentBlockID))
        |> indentBy ~indent:2 ~f:(fun b ->
               addIter fields b ~f:(fun i (fieldName, expr) b ->
                   let exprID = E.toID expr in
                   b
                   |> addNewlineIfNeeded (Some (id, id, Some i))
                   |> add
                        (TRecordFieldname
                           { recordID = id
                           ; exprID
                           ; index = i
                           ; fieldName
                           ; parentBlockID })
                   |> add (TRecordSep (id, i, exprID))
                   |> addNested ~f:(toTokens' ~parentID:(Some id) expr)))
        |> addMany
             [ TNewline (Some (id, id, Some (List.length fields)))
             ; TRecordClose (id, parentBlockID) ]
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
                 |> add (TPipe (id, i, length, parentID))
                 |> addNested ~f:(toTokens' ~parentID e)
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
      b |> add (TPartial (id, str, parentID))
  | ERightPartial (id, newOp, expr) ->
      b
      |> addNested ~f:(toTokens' expr)
      |> addMany [TSep (id, parentID); TRightPartial (id, newOp, parentID)]
  | ELeftPartial (id, str, expr) ->
      b
      |> add (TLeftPartial (id, str, parentID))
      |> addNested ~f:(toTokens' expr)
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


(** returns the tokens that should populate the given editor *)
let tokensForEditor (e : fluidEditor) (ast : FluidAST.t) :
    FluidToken.tokenInfo list =
  match e with
  | NoEditor ->
      []
  | MainEditor _ ->
      tokenize (FluidAST.toExpr ast)
  | FeatureFlagEditor (_, id) ->
      FluidAST.find id ast
      |> Option.map
           ~f:(tokenizeWithFFTokenization FeatureFlagConditionAndEnabled)
      |> recoverOpt
           ( "could not find expression id = "
           ^ ID.toString id
           ^ " when tokenizing FF editor" )
           ~default:[]


(** returns the given expression, tokenized with the rules of the specified editor *)
let tokenizeForEditor (e : fluidEditor) (expr : FluidExpression.t) :
    FluidToken.tokenInfo list =
  match e with
  | NoEditor ->
      []
  | MainEditor _ ->
      tokenize expr
  | FeatureFlagEditor _ ->
      tokenizeWithFFTokenization FeatureFlagConditionAndEnabled expr


(* -------------------------------------- *)
(* ASTInfo *)
(* -------------------------------------- *)
type tokenInfos = T.tokenInfo list

type neighbour =
  | L of T.t * T.tokenInfo
  | R of T.t * T.tokenInfo
  | No

let rec getTokensAtPosition ?(prev = None) ~(pos : int) (tokens : tokenInfos) :
    T.tokenInfo option * T.tokenInfo option * T.tokenInfo option =
  (* Get the next token and the remaining tokens, skipping indents. *)
  let rec getNextToken (infos : tokenInfos) : T.tokenInfo option * tokenInfos =
    match infos with
    | ti :: rest ->
        if T.isSkippable ti.token then getNextToken rest else (Some ti, rest)
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


let getNeighbours ~(pos : int) (tokens : tokenInfos) :
    neighbour * neighbour * T.tokenInfo option =
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


(* This is slightly different from getToken. Here we simply want the token
 * closest to the caret that is a not TNewline nor TSep. It is used for
 * figuring out where your caret is, to determine whether certain rendering
 * behavior should be applicable *)
let getTokenNotWhitespace (tokens : tokenInfos) (s : fluidState) :
    T.tokenInfo option =
  let left, right, _ = getNeighbours ~pos:s.newPos tokens in
  match (left, right) with
  | L (_, lti), R (TNewline _, _) ->
      Some lti
  | L (lt, lti), _ when T.isTextToken lt ->
      Some lti
  | _, R (_, rti) ->
      Some rti
  | L (_, lti), _ ->
      Some lti
  | _ ->
      None


let getToken' (tokens : tokenInfos) (s : fluidState) : T.tokenInfo option =
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
  | L (_, ti), _ when T.isTextToken ti.token ->
      Some ti
  | _, R (_, ti) ->
      Some ti
  | L (_, ti), _ ->
      Some ti
  | _ ->
      None


module ASTInfo = struct
  type t =
    { ast : FluidAST.t
    ; state : fluidState
    ; mainTokenInfos : tokenInfos
    ; featureFlagTokenInfos : (ID.t * tokenInfos) list
    ; props : fluidProps }

  let setAST (ast : FluidAST.t) (astInfo : t) : t =
    if astInfo.ast = ast
    then astInfo
    else
      let mainTokenInfos = tokenize (FluidAST.toExpr ast) in
      let featureFlagTokenInfos =
        ast
        |> FluidAST.getFeatureFlags
        |> List.map ~f:(fun expr ->
               ( E.toID expr
               , tokenizeWithFFTokenization FeatureFlagConditionAndEnabled expr
               ))
      in
      {astInfo with ast; mainTokenInfos; featureFlagTokenInfos}


  let ffTokenInfosFor (ffid : ID.t) (astInfo : t) : tokenInfos option =
    List.find astInfo.featureFlagTokenInfos ~f:(fun (id, _) -> ffid = id)
    |> Option.map ~f:Tuple2.second


  (* Get the correct tokenInfos for the activeEditor *)
  let activeTokenInfos (astInfo : t) : tokenInfos =
    match astInfo.state.activeEditor with
    | NoEditor ->
        []
    | MainEditor _ ->
        astInfo.mainTokenInfos
    | FeatureFlagEditor (_, ffid) ->
        ffTokenInfosFor ffid astInfo |> Option.withDefault ~default:[]


  let modifyState ~(f : fluidState -> fluidState) (astInfo : t) : t =
    {astInfo with state = f astInfo.state}


  let getToken (astInfo : t) : T.tokenInfo option =
    getToken' (activeTokenInfos astInfo) astInfo.state


  let getTokenNotWhitespace (astInfo : t) : T.tokenInfo option =
    getTokenNotWhitespace (activeTokenInfos astInfo) astInfo.state


  let emptyFor (props : fluidProps) (state : fluidState) : t =
    { ast = FluidAST.ofExpr (E.EBlank (gid ()))
    ; state
    ; mainTokenInfos = []
    ; featureFlagTokenInfos = []
    ; props }


  let make (props : fluidProps) (ast : FluidAST.t) (s : fluidState) : t =
    emptyFor props s |> setAST ast


  let exprOfActiveEditor (astInfo : t) : FluidExpression.t =
    match astInfo.state.activeEditor with
    | NoEditor ->
        recover "exprOfActiveEditor - none exists" (FluidAST.toExpr astInfo.ast)
    | MainEditor _ ->
        FluidAST.toExpr astInfo.ast
    | FeatureFlagEditor (_, id) ->
        FluidAST.find id astInfo.ast
        |> recoverOpt
             "exprOfActiveEditor - cannot find expression for editor"
             ~default:(FluidAST.toExpr astInfo.ast)
end
