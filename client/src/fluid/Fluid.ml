(* Specs:
 * Pressing enter
   * https://trello.com/c/nyf3SyA4/1513-handle-pressing-enter-at-the-end-of-a-line
   * https://trello.com/c/27jn8uzF/1507-handle-pressing-enter-at-the-start-of-a-line
 * renaming functions:
   * https://trello.com/c/49TTRcad/973-renaming-functions
 * movement shortcuts:
   * https://trello.com/c/IK9fQZoW/1072-support-ctrl-a-ctrl-e-ctrl-d-ctrl-k
 *)

open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module TL = Toplevel
module Regex = Util.Regex
module DUtil = Util
module AC = FluidAutocomplete
module Commands = FluidCommands
module T = FluidToken
module E = FluidExpression
module P = FluidPattern
module Printer = FluidPrinter
module Util = FluidUtil
module Clipboard = FluidClipboard
module CT = CaretTarget

(* -------------------- *)
(* Utils *)
(* -------------------- *)

type token = T.t

type state = Types.fluidState

let resetFluidState (s : fluidState) : fluidState =
  let editors = FluidEditor.State.hideAll s.editors in
  { s with
    oldPos = 0
  ; newPos = 0
  ; upDownCol = None
  ; activeEditorId = None
  ; editors }


let getStringIndexMaybe ti pos : int option =
  match ti.token with
  | TString (_, _) ->
      Some (pos - ti.startPos - 1)
  | TStringMLStart (_, _, offset, _) ->
      Some (pos - ti.startPos + offset - 1)
  | TStringMLMiddle (_, _, offset, _) | TStringMLEnd (_, _, offset, _) ->
      Some (pos - ti.startPos + offset)
  | _ ->
      None


let getStringIndex ti pos : int =
  match getStringIndexMaybe ti pos with
  | Some i ->
      i
  | None ->
      recover "getting index of non-string" ~debug:(ti.token, pos) 0


type neighbour =
  | L of T.t * T.tokenInfo
  | R of T.t * T.tokenInfo
  | No

let rec getTokensAtPosition
    ?(prev = None) ~(pos : int) (tokens : T.tokenInfo list) :
    T.tokenInfo option * T.tokenInfo option * T.tokenInfo option =
  (* Get the next token and the remaining tokens, skipping indents. *)
  let rec getNextToken (infos : T.tokenInfo list) :
      T.tokenInfo option * T.tokenInfo list =
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


let focusedEditor (s : fluidState) : FluidEditor.t option =
  s.activeEditorId
  |> Option.andThen ~f:(fun eid -> FluidEditor.State.get s.editors ~id:eid)


let exprOfFocusedEditor (ast : FluidAST.t) (s : fluidState) : FluidExpression.t
    =
  match s.activeEditorId with
  | None ->
      FluidAST.toExpr ast
  | Some _ ->
      focusedEditor s
      |> Option.andThen ~f:(fun (e : FluidEditor.t) ->
             FluidAST.find e.expressionId ast)
      |> recoverOpt
           "cannot find expression for editor"
           ~default:(FluidAST.toExpr ast)


let tokenizeForFocusedEditor (s : fluidState) (expr : FluidExpression.t) :
    FluidToken.tokenInfo list =
  match focusedEditor s with
  | None ->
      Printer.tokenize expr (* main editor, default tokenization *)
  | Some {kind; _} ->
      Printer.tokenizeForViewKind kind expr


let tokensOfFocusedEditor (ast : FluidAST.t) (s : fluidState) :
    FluidToken.tokenInfo list =
  exprOfFocusedEditor ast s |> tokenizeForFocusedEditor s


(* -------------------- *)
(* Nearby tokens *)
(* -------------------- *)
let getNeighbours ~(pos : int) (tokens : T.tokenInfo list) :
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


let getToken' (s : fluidState) (tokens : T.tokenInfo list) : T.tokenInfo option
    =
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


let getToken (ast : FluidAST.t) (s : fluidState) : T.tokenInfo option =
  tokensOfFocusedEditor ast s |> getToken' s


(* -------------------- *)
(* Update fluid state *)
(* -------------------- *)
let tiSentinel : T.tokenInfo =
  { token = TSep (ID "sentinel-token")
  ; startPos = -1000
  ; startRow = -1000
  ; startCol = -1000
  ; endPos = -1000
  ; length = -1000 }


(* Returns a new state with the arbitrary string "action" recorded for debugging.
 * If a ~pos or ~ti (token info) is passed, it will be added to the action. *)
let recordAction ?(pos = -1000) ?(ti = tiSentinel) (action : string) (s : state)
    : state =
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
  tokens |> List.map ~f:T.toText |> List.map ~f:String.length |> List.sum


(* Returns the token to the left and to the right. Ignores indent tokens *)

let getLeftTokenAt (newPos : int) (tis : T.tokenInfo list) : T.tokenInfo option
    =
  List.find ~f:(fun ti -> newPos <= ti.endPos && newPos >= ti.startPos) tis


type gridPos =
  { row : int
  ; col : int }

(* Result will definitely be a valid position. *)
let gridFor ~(pos : int) (tokens : T.tokenInfo list) : gridPos =
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
let posFor ~(row : int) ~(col : int) (tokens : T.tokenInfo list) : int =
  if row < 0 || col < 0
  then 0
  else
    let ti =
      List.find tokens ~f:(fun ti ->
          ti.startRow = row
          && ti.startCol <= col
          && ti.startCol + ti.length >= col)
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
let adjustedPosFor ~(row : int) ~(col : int) (tokens : T.tokenInfo list) : int =
  if row < 0 || col < 0
  then 0
  else
    let thisRow = List.filter tokens ~f:(fun ti -> ti.startRow = row) in
    let ti =
      List.find thisRow ~f:(fun ti ->
          ti.startCol <= col && ti.startCol + ti.length > col)
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


(* ---------------- *)
(* Movement *)
(* ---------------- *)

let moveToNextNonWhitespaceToken ~pos (ast : FluidAST.t) (s : state) : state =
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
  let newPos = tokensOfFocusedEditor ast s |> getNextWS in
  setPosition ~resetUD:true s newPos


(* getStartOfLineCaretPos returns the first desirable (excluding indents, pipes, and newline tokens)
 caret pos at the start of the line containing the given T.tokenInfo *)
let getStartOfLineCaretPos
    (ast : FluidAST.t) (s : fluidState) (ti : T.tokenInfo) : int =
  let token =
    tokensOfFocusedEditor ast s
    |> List.find ~f:(fun info ->
           if info.startRow == ti.startRow
           then
             match info.token with
             (* To prevent the result pos from being set inside TPipe or TIndent tokens *)
             | TPipe _ | TIndent _ ->
                 false
             | _ ->
                 true
           else false)
    |> Option.withDefault ~default:ti
  in
  token.startPos


(* getBegOfWordInStrCaretPos returns the closest whitespace position before the
 * current caret position in a string *)
let getBegOfWordInStrCaretPos ~(pos : int) (ti : T.tokenInfo) : int =
  let posInString = pos - ti.startPos in
  let nextPos : int ref = ref ti.length in
  let _ =
    T.toText ti.token
    |> String.split ~on:""
    |> List.reverse
    |> List.find ~f:(fun a ->
           if (a == " " || a = "\"" || a = "\n" || a = "\t")
              && !nextPos < posInString
           then true
           else (
             nextPos := !nextPos - 1 ;
             false ))
  in
  ti.startPos + !nextPos


(* getEndOfWordInStrCaretPos returns the closest whitespace position after the
 * current caret position in a string *)
let getEndOfWordInStrCaretPos ~(pos : int) (ti : T.tokenInfo) : int =
  let posInString = pos - ti.startPos in
  let nextPos : int ref = ref 0 in
  let _ =
    T.toText ti.token
    |> String.split ~on:""
    |> List.find ~f:(fun a ->
           if (a == " " || (a = "\"" && !nextPos > 0) || a = "\n" || a = "\t")
              && !nextPos > posInString
           then true
           else (
             nextPos := !nextPos + 1 ;
             false ))
  in
  ti.startPos + !nextPos


(* getEndOfLineCaretPos returns the last desirable (excluding indents and newline tokens)
 caret pos at the end of the line containing the given tokenInfo *)
let getEndOfLineCaretPos (ast : FluidAST.t) (s : fluidState) (ti : T.tokenInfo)
    : int =
  let token =
    tokensOfFocusedEditor ast s
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
let moveToStartOfLine (ast : FluidAST.t) (s : state) (ti : T.tokenInfo) : state
    =
  let s = recordAction "moveToStartOfLine" s in
  setPosition s (getStartOfLineCaretPos ast s ti)


(* moveToEndOfLine moves the caret to the last desirable (excluding indents and newline tokens)
 caret pos at the end of the line containing the given tokenInfo *)
let moveToEndOfLine (ast : FluidAST.t) (s : state) (ti : T.tokenInfo) : state =
  let s = recordAction "moveToEndOfLine" s in
  setPosition s (getEndOfLineCaretPos ast s ti)


(* We want to find the closest editable token that is before the current cursor position
  * so the cursor always lands in a position where a user is able to type *)
let getStartOfWordPos
    ~(pos : int) (ast : FluidAST.t) (s : fluidState) (ti : T.tokenInfo) : int =
  let previousToken =
    tokensOfFocusedEditor ast s
    |> List.reverse
    |> List.find ~f:(fun t -> T.isTextToken t.token && pos > t.startPos)
  in
  let tokenInfo = previousToken |> Option.withDefault ~default:ti in
  if T.isStringToken tokenInfo.token && pos != tokenInfo.startPos
  then getBegOfWordInStrCaretPos ~pos tokenInfo
  else tokenInfo.startPos


let goToStartOfWord
    ~(pos : int) (ast : FluidAST.t) (s : state) (ti : T.tokenInfo) : state =
  let s = recordAction "goToStartOfWord" s in
  setPosition s (getStartOfWordPos ~pos ast s ti)


(* We want to find the closest editable token that is after the current cursor position
  * so the cursor always lands in a position where a user is able to type *)
let getEndOfWordPos
    ~(pos : int) (ast : FluidAST.t) (s : fluidState) (ti : T.tokenInfo) : int =
  let tokenInfo =
    tokensOfFocusedEditor ast s
    |> List.find ~f:(fun t -> T.isTextToken t.token && pos < t.endPos)
    |> Option.withDefault ~default:ti
  in
  if T.isStringToken tokenInfo.token && pos != tokenInfo.endPos
  then getEndOfWordInStrCaretPos ~pos tokenInfo
  else tokenInfo.endPos


let goToEndOfWord ~(pos : int) (ast : FluidAST.t) (ti : T.tokenInfo) (s : state)
    : state =
  let s = recordAction "goToEndOfWord" s in
  setPosition s (getEndOfWordPos ~pos ast s ti)


let moveToEnd (ti : T.tokenInfo) (s : state) : state =
  let s = recordAction ~ti "moveToEnd" s in
  setPosition ~resetUD:true s (ti.endPos - 1)


let moveToStart (ti : T.tokenInfo) (s : state) : state =
  let s = recordAction ~ti ~pos:ti.startPos "moveToStart" s in
  setPosition ~resetUD:true s ti.startPos


let moveToAfter (ti : T.tokenInfo) (s : state) : state =
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
let moveToEndOfTarget (ast : FluidAST.t) (s : state) (target : ID.t) : state =
  let s = recordAction "moveToEndOfTarget" s in
  let tokens = tokensOfFocusedEditor ast s in
  match
    List.find (List.reverse tokens) ~f:(fun ti ->
        FluidToken.tid ti.token = target)
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


let rec getNextBlank (pos : int) (tokens : T.tokenInfo list) :
    T.tokenInfo option =
  tokens
  |> List.find ~f:(fun ti -> T.isBlank ti.token && ti.startPos > pos)
  |> Option.orElseLazy (fun () ->
         if pos = 0 then None else getNextBlank 0 tokens)


let getNextBlankPos (pos : int) (tokens : T.tokenInfo list) : int =
  tokens
  |> getNextBlank pos
  |> Option.map ~f:(fun ti -> ti.startPos)
  |> Option.withDefault ~default:pos


let moveToNextBlank ~(pos : int) (ast : FluidAST.t) (s : state) : state =
  recordAction ~pos "moveToNextBlank" s
  |> tokensOfFocusedEditor ast
  |> getNextBlankPos pos
  |> setPosition ~resetUD:true s


let rec getPrevBlank (pos : int) (tokens : T.tokenInfo list) :
    T.tokenInfo option =
  tokens
  |> List.filter ~f:(fun ti -> T.isBlank ti.token && ti.endPos <= pos)
  |> List.last
  |> Option.orElseLazy (fun () ->
         let lastPos =
           List.last tokens
           |> Option.map ~f:(fun ti -> ti.endPos)
           |> Option.withDefault ~default:0
         in
         if pos = lastPos then None else getPrevBlank lastPos tokens)


let getPrevBlankPos (pos : int) (tokens : T.tokenInfo list) : int =
  tokens
  |> getPrevBlank pos
  |> Option.map ~f:(fun ti -> ti.startPos)
  |> Option.withDefault ~default:pos


let moveToPrevBlank ~(pos : int) (ast : FluidAST.t) (s : state) : state =
  recordAction ~pos "moveToPrevBlank" s
  |> tokensOfFocusedEditor ast
  |> getPrevBlankPos pos
  |> setPosition ~resetUD:true s


let doLeft ~(pos : int) (ti : T.tokenInfo) (s : state) : state =
  let s = recordAction ~ti ~pos "doLeft" s in
  if T.isAtom ti.token
  then moveToStart ti s
  else moveOneLeft (min pos ti.endPos) s


let selectAll ~(pos : int) (ast : FluidAST.t) (s : state) : state =
  let lastPos =
    tokensOfFocusedEditor ast s
    |> List.last
    |> function Some l -> l.endPos | None -> 0
  in
  {s with newPos = lastPos; oldPos = pos; selectionStart = Some 0}


let doRight
    ~(pos : int)
    ~(next : T.tokenInfo option)
    (current : T.tokenInfo)
    (s : state) : state =
  let s = recordAction ~ti:current ~pos "doRight" s in
  if T.isAtom current.token
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


let doUp ~(pos : int) (ast : FluidAST.t) (s : state) : state =
  let s = recordAction ~pos "doUp" s in
  let tokens = tokensOfFocusedEditor ast s in
  let {row; col} = gridFor ~pos tokens in
  let col = match s.upDownCol with None -> col | Some savedCol -> savedCol in
  if row = 0
  then moveTo 0 s
  else
    let pos = adjustedPosFor ~row:(row - 1) ~col tokens in
    moveTo pos {s with upDownCol = Some col}


let doDown ~(pos : int) (ast : FluidAST.t) (s : state) : state =
  let s = recordAction ~pos "doDown" s in
  let tokens = tokensOfFocusedEditor ast s in
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
let posFromCaretTarget (ast : FluidAST.t) (s : fluidState) (ct : caretTarget) :
    int =
  let infos = tokensOfFocusedEditor ast s in
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
  let posForTi ti : int option = Some (ti.startPos + min ct.offset ti.length) in
  let clampedPosForTi ti pos : int option =
    Some (ti.startPos + max 0 (min pos ti.length))
  in
  (* targetAndTokenInfoToMaybeCaretPos takes a caretTarget and tokenInfo and produces
     the corresponding token-stream-global caretPos within the token stream,
     or None if the passed token isn't one we care about. The function will be used below
     as part of a List.findMap
   *)
  let targetAndTokenInfoToMaybeCaretPos ((ct, ti) : caretTarget * T.tokenInfo) :
      int option =
    match (ct.astRef, ti.token) with
    | ARBinOp id, TBinOp (id', _)
    | ARBlank id, (TBlank id' | TPlaceholder {blankID = id'; _})
    | ARBool id, (TTrue id' | TFalse id')
    | ARConstructor id, TConstructorName (id', _)
    | ARFieldAccess (id, FAPFieldname), TFieldName (id', _, _)
    | ARFieldAccess (id, FAPFieldOp), TFieldOp (id', _)
    | ARIf (id, IPIfKeyword), TIfKeyword id'
    | ARIf (id, IPThenKeyword), TIfThenKeyword id'
    | ARIf (id, IPElseKeyword), TIfElseKeyword id'
    | ARInteger id, TInteger (id', _)
    | ARLet (id, LPKeyword), TLetKeyword (id', _)
    | ARLet (id, LPVarName), TLetVarName (id', _, _)
    | ARLet (id, LPAssignment), TLetAssignment (id', _)
    | ARList (id, LPOpen), TListOpen id'
    | ARList (id, LPClose), TListClose id'
    | ARMatch (id, MPKeyword), TMatchKeyword id'
    | ARNull id, TNullToken id'
    | ARPartial id, TPartial (id', _)
    | ARPartial id, TFieldPartial (id', _, _, _)
    | ARRightPartial id, TRightPartial (id', _)
    | ARRecord (id, RPOpen), TRecordOpen id'
    | ARRecord (id, RPClose), TRecordClose id'
    | ARVariable id, TVariable (id', _)
    | ARLambda (id, LBPSymbol), TLambdaSymbol id'
    | ARLambda (id, LBPArrow), TLambdaArrow id'
    | ARPattern (id, PPVariable), TPatternVariable (_, id', _, _)
    | ARPattern (id, PPConstructor), TPatternConstructorName (_, id', _, _)
    | ARPattern (id, PPInteger), TPatternInteger (_, id', _, _)
    | ( ARPattern (id, PPBool)
      , (TPatternTrue (_, id', _) | TPatternFalse (_, id', _)) )
    | ARPattern (id, PPBlank), TPatternBlank (_, id', _)
    | ARPattern (id, PPNull), TPatternNullToken (_, id', _)
    | ARFlag (id, FPWhenKeyword), TFlagWhenKeyword id'
    | ARFlag (id, FPEnabledKeyword), TFlagEnabledKeyword id'
      when id = id' ->
        posForTi ti
    | ARList (id, LPComma idx), TListComma (id', idx')
    | ( ARMatch (id, MPBranchArrow idx)
      , TMatchBranchArrow {matchID = id'; index = idx'; _} )
    | ARPipe (id, idx), TPipe (id', idx', _)
    | ( ARRecord (id, RPFieldname idx)
      , TRecordFieldname {recordID = id'; index = idx'; _} )
    | ARRecord (id, RPFieldSep idx), TRecordSep (id', idx', _)
    | ARLambda (id, LBPVarName idx), TLambdaVar (id', _, idx', _)
    | ARLambda (id, LBPComma idx), TLambdaComma (id', idx')
      when id = id' && idx = idx' ->
        posForTi ti
    (*
     * Floats
     *)
    | ARPattern (id, PPFloat FPPoint), TPatternFloatPoint (_, id', _)
    | ARPattern (id, PPFloat FPWhole), TPatternFloatWhole (_, id', _, _)
    | ARFloat (id, FPPoint), TFloatPoint id'
    | ARFloat (id, FPWhole), TFloatWhole (id', _)
      when id = id' ->
        posForTi ti
    | ARPattern (id, PPFloat FPWhole), TPatternFloatPoint (_, id', _)
    | ARFloat (id, FPWhole), TFloatPoint id'
      when id = id' ->
        (* This accounts for situations like `|.45`, where the float doesn't have a whole part but
           we're still targeting it (perhaps due to deletion).
           Because the 'findMap' below scans from left to right and we try to match the whole first,
           we can still find positions like `1|2.54` *)
        Some ti.startPos
    | ARPattern (id, PPFloat FPFractional), TPatternFloatPoint (_, id', _)
    | ARFloat (id, FPFractional), TFloatPoint id'
      when id = id' && ct.offset = 0 ->
        (* This accounts for situations like `12.|`, where the float doesn't have a decimal part but
           we're still targeting it (perhaps due to deletion). *)
        Some ti.endPos
    | ( ARPattern (id, PPFloat FPFractional)
      , TPatternFloatFractional (_, id', _, _) )
    | ARFloat (id, FPFractional), TFloatFractional (id', _)
      when id = id' ->
        posForTi ti
    (*
     * Function calls
     *)
    | ARFnCall id, TFnName (id', partialName, displayName, _, _) when id = id'
      ->
        let dispLen = String.length displayName in
        if ct.offset > dispLen && String.length partialName > dispLen
        then (* A version token exists and we must be there instead *)
          None
        else (* Within current token *)
          clampedPosForTi ti ct.offset
    | ARFnCall id, TFnVersion (id', _, _, backendFnName) when id = id' ->
        let nameWithoutVersion = FluidUtil.fnDisplayName backendFnName in
        clampedPosForTi ti (ct.offset - String.length nameWithoutVersion)
    (*
    * Single-line Strings
    *)
    | ARString (id, SPOpenQuote), TString (id', _)
    | ARPattern (id, PPString SPOpenQuote), TPatternString {patternID = id'; _}
      when id = id' ->
        clampedPosForTi ti ct.offset
    (*
    * Multi-line Strings
    *)
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
            ct.offset - 1
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
            ct.offset - 1
            (* to account for open quote in the start *)
          in
          clampedPosForTi ti (offsetInStr - startOffsetIntoString)
      | _ ->
          None )
    (*
    * Exhaustiveness satisfaction for astRefs
    *)
    | ARBinOp _, _
    | ARBlank _, _
    | ARBool _, _
    | ARConstructor _, _
    | ARFieldAccess (_, FAPFieldname), _
    | ARFieldAccess (_, FAPFieldOp), _
    | ARFloat (_, FPWhole), _
    | ARFloat (_, FPPoint), _
    | ARFloat (_, FPFractional), _
    | ARFnCall _, _
    | ARIf (_, IPIfKeyword), _
    | ARIf (_, IPThenKeyword), _
    | ARIf (_, IPElseKeyword), _
    | ARInteger _, _
    | ARLet (_, LPKeyword), _
    | ARLet (_, LPVarName), _
    | ARLet (_, LPAssignment), _
    | ARList (_, LPOpen), _
    | ARList (_, LPClose), _
    | ARList (_, LPComma _), _
    | ARMatch (_, MPKeyword), _
    | ARMatch (_, MPBranchArrow _), _
    | ARNull _, _
    | ARPartial _, _
    | ARRightPartial _, _
    | ARPipe (_, _), _
    | ARRecord (_, RPOpen), _
    | ARRecord (_, RPClose), _
    | ARRecord (_, RPFieldname _), _
    | ARRecord (_, RPFieldSep _), _
    | ARVariable _, _
    | ARLambda (_, LBPSymbol), _
    | ARLambda (_, LBPArrow), _
    | ARLambda (_, LBPVarName _), _
    | ARLambda (_, LBPComma _), _
    | ARPattern (_, PPVariable), _
    | ARPattern (_, PPConstructor), _
    | ARPattern (_, PPInteger), _
    | ARPattern (_, PPBool), _
    | ARPattern (_, PPFloat FPPoint), _
    | ARPattern (_, PPFloat FPWhole), _
    | ARPattern (_, PPFloat FPFractional), _
    | ARPattern (_, PPBlank), _
    | ARPattern (_, PPNull), _
    | ARPattern (_, PPString SPOpenQuote), _
    | ARFlag (_, FPWhenKeyword), _
    | ARFlag (_, FPEnabledKeyword), _ ->
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
      (*
        NOTE: This is very useful for fixing issues in dev, but much too large for Rollbar:
        (Debug.loG ((show_caretTarget ct)^(show_fluidExpr ast)^(Printer.eToStructure ~incluID.toStrings:true ast)) ());
      *)
      recover
        "We expected to find the given caretTarget in the token stream but couldn't."
        ~debug:(show_caretTarget ct)
        s.newPos


(** caretTargetFromTokenInfo returns Some caretTarget corresponding to
  * the given top-level-global caret `pos`, with the precondition that
  * the pos is within the passed tokenInfo `ti`.
  * There are a few tokens that have no corresponding caretTarget.
  * In such cases, we return None instead.

  * We attempt to ensure that a single caret target uniquely
  * identifies each pos.  *)
let caretTargetFromTokenInfo (pos : int) (ti : T.tokenInfo) : caretTarget option
    =
  let offset = pos - ti.startPos in
  match ti.token with
  | TString (id, _) | TStringMLStart (id, _, _, _) ->
      Some (CT.forARStringOpenQuote id offset)
  | TStringMLMiddle (id, _, startOffset, _)
  | TStringMLEnd (id, _, startOffset, _) ->
      Some (CT.forARStringText id (startOffset + pos - ti.startPos))
  | TInteger (id, _) ->
      Some {astRef = ARInteger id; offset}
  | TBlank id | TPlaceholder {blankID = id; _} ->
      Some {astRef = ARBlank id; offset}
  | TTrue id | TFalse id ->
      Some {astRef = ARBool id; offset}
  | TNullToken id ->
      Some {astRef = ARNull id; offset}
  | TFloatWhole (id, _) ->
      Some {astRef = ARFloat (id, FPWhole); offset}
  | TFloatPoint id ->
      Some {astRef = ARFloat (id, FPPoint); offset}
  | TFloatFractional (id, _) ->
      Some {astRef = ARFloat (id, FPFractional); offset}
  | TPartial (id, _) ->
      Some {astRef = ARPartial id; offset}
  | TFieldPartial (id, _, _, _) ->
      Some {astRef = ARPartial id; offset}
  | TRightPartial (id, _) ->
      Some {astRef = ARRightPartial id; offset}
  | TLetKeyword (id, _) ->
      Some {astRef = ARLet (id, LPKeyword); offset}
  | TLetVarName (id, _, _) ->
      Some {astRef = ARLet (id, LPVarName); offset}
  | TLetAssignment (id, _) ->
      Some {astRef = ARLet (id, LPAssignment); offset}
  | TIfKeyword id ->
      Some {astRef = ARIf (id, IPIfKeyword); offset}
  | TIfThenKeyword id ->
      Some {astRef = ARIf (id, IPThenKeyword); offset}
  | TIfElseKeyword id ->
      Some {astRef = ARIf (id, IPElseKeyword); offset}
  | TBinOp (id, _) ->
      Some {astRef = ARBinOp id; offset}
  | TFieldName (id, _, _) ->
      Some {astRef = ARFieldAccess (id, FAPFieldname); offset}
  | TFieldOp (id, _) ->
      Some {astRef = ARFieldAccess (id, FAPFieldOp); offset}
  | TVariable (id, _) ->
      Some {astRef = ARVariable id; offset}
  | TFnName (id, _, _, _, _) ->
      Some {astRef = ARFnCall id; offset}
  | TFnVersion (id, _, versionName, backendFnName) ->
      (* TODO: This is very brittle and should probably be moved into a function responsible
         for grabbing the appropriate bits of functions *)
      Some
        { astRef = ARFnCall id
        ; offset =
            offset + String.length backendFnName - String.length versionName - 1
        }
  | TLambdaComma (id, idx) ->
      Some {astRef = ARLambda (id, LBPComma idx); offset}
  | TLambdaArrow id ->
      Some {astRef = ARLambda (id, LBPArrow); offset}
  | TLambdaSymbol id ->
      Some {astRef = ARLambda (id, LBPSymbol); offset}
  | TLambdaVar (id, _, idx, _) ->
      Some {astRef = ARLambda (id, LBPVarName idx); offset}
  | TListOpen id ->
      Some {astRef = ARList (id, LPOpen); offset}
  | TListClose id ->
      Some {astRef = ARList (id, LPClose); offset}
  | TListComma (id, idx) ->
      Some {astRef = ARList (id, LPComma idx); offset}
  | TPipe (id, idx, _) ->
      Some {astRef = ARPipe (id, idx); offset}
  | TRecordOpen id ->
      Some {astRef = ARRecord (id, RPOpen); offset}
  | TRecordFieldname {recordID = id; index = idx; _} ->
      Some {astRef = ARRecord (id, RPFieldname idx); offset}
  | TRecordSep (id, idx, _) ->
      Some {astRef = ARRecord (id, RPFieldSep idx); offset}
  | TRecordClose id ->
      Some {astRef = ARRecord (id, RPClose); offset}
  | TMatchKeyword id ->
      Some {astRef = ARMatch (id, MPKeyword); offset}
  | TMatchBranchArrow {matchID = id; index = idx; _} ->
      Some {astRef = ARMatch (id, MPBranchArrow idx); offset}
  | TPatternVariable (_, id, _, _) ->
      Some {astRef = ARPattern (id, PPVariable); offset}
  | TPatternConstructorName (_, id, _, _) ->
      Some {astRef = ARPattern (id, PPConstructor); offset}
  | TPatternInteger (_, id, _, _) ->
      Some {astRef = ARPattern (id, PPInteger); offset}
  | TPatternString {patternID = id; _} ->
      Some (CT.forPPStringOpenQuote id offset)
  | TPatternTrue (_, id, _) | TPatternFalse (_, id, _) ->
      Some {astRef = ARPattern (id, PPBool); offset}
  | TPatternNullToken (_, id, _) ->
      Some {astRef = ARPattern (id, PPNull); offset}
  | TPatternFloatWhole (_, id, _, _) ->
      Some {astRef = ARPattern (id, PPFloat FPWhole); offset}
  | TPatternFloatPoint (_, id, _) ->
      Some {astRef = ARPattern (id, PPFloat FPPoint); offset}
  | TPatternFloatFractional (_, id, _, _) ->
      Some {astRef = ARPattern (id, PPFloat FPFractional); offset}
  | TPatternBlank (_, id, _) ->
      Some {astRef = ARPattern (id, PPBlank); offset}
  | TConstructorName (id, _) ->
      Some {astRef = ARConstructor id; offset}
  | TFlagWhenKeyword id ->
      Some {astRef = ARFlag (id, FPWhenKeyword); offset}
  | TFlagEnabledKeyword id ->
      Some {astRef = ARFlag (id, FPEnabledKeyword); offset}
  (*
    These have no valid caretTarget because they are not
    strictly part of the AST.
   *)
  | TPartialGhost _
  | TNewline _
  | TSep _
  | TIndent _
  | TParenOpen _
  | TParenClose _ ->
      None


let caretTargetForNextNonWhitespaceToken
    ~pos (ast : FluidAST.t) (s : fluidState) : caretTarget option =
  let rec getNextWS tokens =
    match tokens with
    | [] ->
        None
    | ti :: rest ->
      ( match ti.token with
      | TSep _ | TNewline _ | TIndent _ ->
          getNextWS rest
      | _ ->
          if pos > ti.startPos
          then getNextWS rest
          else caretTargetFromTokenInfo ti.startPos ti )
  in
  tokensOfFocusedEditor ast s |> getNextWS


(** moveToCaretTarget returns a modified fluidState with newPos set to reflect
    the caretTarget. *)
let moveToCaretTarget (s : fluidState) (ast : FluidAST.t) (ct : caretTarget) :
    fluidState =
  setPosition s (posFromCaretTarget ast s ct)


(** moveToAstRef returns a modified fluidState with newPos set to reflect
    the targeted astRef.

    If given, offset is the offset of the caretTarget, in characters. Defaults
    to 0, or the beginning of the targeted expression. *)
let moveToAstRef
    (s : fluidState) (ast : FluidAST.t) ?(offset = 0) (astRef : astRef) :
    fluidState =
  moveToCaretTarget s ast {astRef; offset}


(** [caretTargetForEndOfExpr' expr] produces a caretTarget corresponding
 * to the very end of the expr. The concept of "very end" is related to an
 * understanding of the tokenization of the expr, even though this function
 * doesn't explicitly depend on any tokenization functions. *)
let rec caretTargetForEndOfExpr' : fluidExpr -> caretTarget = function
  | EVariable (id, str) ->
      {astRef = ARVariable id; offset = String.length str}
  | EFieldAccess (id, _, fieldName) ->
      { astRef = ARFieldAccess (id, FAPFieldname)
      ; offset = String.length fieldName }
  | EInteger (id, valueStr) ->
      {astRef = ARInteger id; offset = String.length valueStr}
  | EBool (id, true) ->
      {astRef = ARBool id; offset = String.length "true"}
  | EBool (id, false) ->
      {astRef = ARBool id; offset = String.length "false"}
  | EString (id, str) ->
      CT.forARStringCloseQuote id 1 str
  | EFloat (id, _, decimalStr) ->
      {astRef = ARFloat (id, FPFractional); offset = String.length decimalStr}
  | ENull id ->
      {astRef = ARNull id; offset = String.length "null"}
  | EBlank id ->
      {astRef = ARBlank id; offset = 0}
  | ELet (_, _, _, bodyExpr) ->
      caretTargetForEndOfExpr' bodyExpr
  | EIf (_, _, _, elseExpr) ->
      caretTargetForEndOfExpr' elseExpr
  | EBinOp (_, _, _, rhsExpr, _) ->
      caretTargetForEndOfExpr' rhsExpr
  | ELambda (_, _, bodyExpr) ->
      caretTargetForEndOfExpr' bodyExpr
  | EFnCall (id, fnName, argExprs, _) ->
      (* Caret targets don't make sense for EPipeTargets, so we
       * return a caret target for the end of the last fn argument
       * that isn't an EPipeTarget, or the end of the extended
       * function name, if there are no non-EPipeTargets. *)
      argExprs
      |> List.reverse
      |> List.find ~f:(fun e ->
             match e with E.EPipeTarget _ -> false | _ -> true)
      |> Option.map ~f:(fun lastNonPipeTarget ->
             caretTargetForEndOfExpr' lastNonPipeTarget)
      |> Option.withDefault
           ~default:
             { astRef = ARFnCall id
             ; offset =
                 fnName |> FluidUtil.fnDisplayNameWithVersion |> String.length
             }
  | EPartial (_, _, EBinOp (_, _, _, rhsExpr, _)) ->
      (* We need this so that (for example) when we backspace a binop containing a binop within a partial,
       * we can keep hitting backspace to delete the whole thing. This isn't (currently) needed for
       * other types of partials because deleting non-binop partials deletes their args,
       * whereas deleting binop partials merges and hoists the args. *)
      caretTargetForEndOfExpr' rhsExpr
  | EPartial (id, str, _) ->
      (* Intentionally using the thing that was typed; not the existing expr *)
      {astRef = ARPartial id; offset = String.length str}
  | ERightPartial (id, str, _) ->
      (* Intentionally using the thing that was typed; not the existing expr *)
      {astRef = ARRightPartial id; offset = String.length str}
  | EList (id, _) ->
      {astRef = ARList (id, LPClose); offset = 1 (* End of the close ] *)}
  | ERecord (id, _) ->
      {astRef = ARRecord (id, RPClose); offset = 1 (* End of the close } *)}
  | EPipe (id, pipeExprs) ->
    ( match List.last pipeExprs with
    | Some lastExpr ->
        caretTargetForEndOfExpr' lastExpr
    | None ->
        {astRef = ARPipe (id, 0); offset = String.length "|>"} )
  | EMatch (_, matchedExpr, matchItems) ->
    ( match List.last matchItems with
    | Some (_, branchBody) ->
        caretTargetForEndOfExpr' branchBody
    | None ->
        caretTargetForEndOfExpr' matchedExpr )
  | EConstructor (id, name, containedExprs) ->
    ( match List.last containedExprs with
    | Some lastExpr ->
        caretTargetForEndOfExpr' lastExpr
    | None ->
        {astRef = ARConstructor id; offset = String.length name} )
  | (EFeatureFlag (_, _, _, _, _) | EPipeTarget _) as expr ->
      recover
        "we don't yet support caretTargetForEndOfExpr' for this"
        ~debug:(show_fluidExpr expr)
        {astRef = ARInvalid; offset = 0}


(* [caretTargetForEndOfExpr id ast] produces a caretTarget corresponding
 * to the "very end" of the expr identified by id within the [ast].
 * The concept of "very end" depends on caretTargetForEndOfExpr'.
 *)
let caretTargetForEndOfExpr (astPartId : ID.t) (ast : FluidAST.t) : caretTarget
    =
  match FluidAST.find astPartId ast with
  | Some expr ->
      caretTargetForEndOfExpr' expr
  | None ->
      recover
        "caretTargetForEndOfExpr got an id outside of the AST"
        ~debug:astPartId
        {astRef = ARInvalid; offset = 0}


(* [caretTargetForStartOfExpr' expr] produces a caretTarget corresponding
 * to the very beginning of the [expr]. The concept of "very beginning" is related to an
 * understanding of the tokenization of the expr, even though this function
 * doesn't explicitly depend on any tokenization functions. *)
let rec caretTargetForStartOfExpr' : fluidExpr -> caretTarget = function
  | EInteger (id, _) ->
      {astRef = ARInteger id; offset = 0}
  | EBool (id, _) ->
      {astRef = ARBool id; offset = 0}
  | EString (id, _) ->
      CT.forARStringOpenQuote id 0
  | EFloat (id, _, _) ->
      {astRef = ARFloat (id, FPWhole); offset = 0}
  | ENull id ->
      {astRef = ARNull id; offset = 0}
  | EBlank id ->
      {astRef = ARBlank id; offset = 0}
  | ELet (id, _, _, _) ->
      {astRef = ARLet (id, LPKeyword); offset = 0}
  | EIf (id, _, _, _) ->
      {astRef = ARIf (id, IPIfKeyword); offset = 0}
  | EMatch (id, _, _) ->
      {astRef = ARMatch (id, MPKeyword); offset = 0}
  | EBinOp (_, _, lhsExpr, _, _) ->
      caretTargetForStartOfExpr' lhsExpr
  | EFnCall (id, _, _, _) ->
      {astRef = ARFnCall id; offset = 0}
  | ELambda (id, _, _) ->
      {astRef = ARLambda (id, LBPSymbol); offset = 0}
  | EFieldAccess (_, expr, _) ->
      caretTargetForStartOfExpr' expr
  | EVariable (id, _) ->
      {astRef = ARVariable id; offset = 0}
  | EPartial (id, _, _) ->
      {astRef = ARPartial id; offset = 0}
  | ERightPartial (id, _, _) ->
      {astRef = ARRightPartial id; offset = 0}
  | EList (id, _) ->
      {astRef = ARList (id, LPOpen); offset = 0}
  | ERecord (id, _) ->
      {astRef = ARRecord (id, RPOpen); offset = 0}
  | EPipe (id, exprChain) ->
      List.getAt ~index:0 exprChain
      |> Option.map ~f:(fun expr -> caretTargetForStartOfExpr' expr)
      |> recoverOpt
           "caretTargetForStartOfExpr' - EPipe"
           ~default:{astRef = ARPipe (id, 0); offset = 0}
  | EConstructor (id, _, _) ->
      {astRef = ARConstructor id; offset = 0}
  | (EFeatureFlag _ | EPipeTarget _) as expr ->
      recover
        "unhandled expr in caretTargetForStartOfExpr'"
        ~debug:(show_fluidExpr expr)
        {astRef = ARInvalid; offset = 0}


(* [caretTargetForStartOfExpr id ast] produces a caretTarget corresponding
 * to the "very beginning" of the expr identified by [id] within the [ast].
 * The concept of "very beginning" depends on caretTargetForStartOfExpr'.
 *)
let caretTargetForStartOfExpr (astPartId : ID.t) (ast : FluidAST.t) :
    caretTarget =
  match FluidAST.find astPartId ast with
  | Some expr ->
      caretTargetForStartOfExpr' expr
  | None ->
      recover
        "caretTargetForStartOfExpr got an id outside of the AST"
        ~debug:astPartId
        {astRef = ARInvalid; offset = 0}


(* caretTargetForStartOfPattern returns a caretTarget representing caret
   placement at the very start of the expression in `pattern` *)
let caretTargetForStartOfPattern (pattern : fluidPattern) : caretTarget =
  match pattern with
  | FPVariable (_, id, _) ->
      {astRef = ARPattern (id, PPVariable); offset = 0}
  | FPConstructor (_, id, _, _) ->
      {astRef = ARPattern (id, PPConstructor); offset = 0}
  | FPInteger (_, id, _) ->
      {astRef = ARPattern (id, PPInteger); offset = 0}
  | FPBool (_, id, _) ->
      {astRef = ARPattern (id, PPBool); offset = 0}
  | FPString {patternID = id; _} ->
      CT.forPPStringOpenQuote id 0
  | FPFloat (_, id, _, _) ->
      {astRef = ARPattern (id, PPFloat FPWhole); offset = 0}
  | FPNull (_, id) ->
      {astRef = ARPattern (id, PPNull); offset = 0}
  | FPBlank (_, id) ->
      {astRef = ARPattern (id, PPBlank); offset = 0}


(* caretTargetForEndOfPattern returns a caretTarget representing caret
 * placement at the very end of the expression in `pattern`.
 *
 * The concept of "very end" is related to an understanding of the
 * tokenization of the ast, even though this function doesn't explicitly depend
 * on any tokenization functions. *)
let rec caretTargetForEndOfPattern (pattern : fluidPattern) : caretTarget =
  match pattern with
  | FPVariable (_, id, varName) ->
      {astRef = ARPattern (id, PPVariable); offset = String.length varName}
  | FPConstructor (_, id, name, containedPatterns) ->
    ( match List.last containedPatterns with
    | Some lastPattern ->
        caretTargetForEndOfPattern lastPattern
    | None ->
        {astRef = ARPattern (id, PPConstructor); offset = String.length name} )
  | FPInteger (_, id, valueStr) ->
      {astRef = ARPattern (id, PPInteger); offset = String.length valueStr}
  | FPBool (_, id, true) ->
      {astRef = ARPattern (id, PPBool); offset = String.length "true"}
  | FPBool (_, id, false) ->
      {astRef = ARPattern (id, PPBool); offset = String.length "false"}
  | FPString {patternID = id; str; _} ->
      CT.forPPStringCloseQuote id 1 str (* end of close quote *)
  | FPFloat (_, id, _, frac) ->
      { astRef = ARPattern (id, PPFloat FPFractional)
      ; offset = String.length frac }
  | FPNull (_, id) ->
      {astRef = ARPattern (id, PPNull); offset = String.length "null"}
  | FPBlank (_, id) ->
      (* Consider changing this from 3 to 0 if we don't want blanks to have two spots *)
      {astRef = ARPattern (id, PPBlank); offset = 3}


(* caretTargetForBeginningOfMatchBranch returns a caretTarget representing caret
 * placement at the very start of the match branch identified by `matchID` and `index`
 * within the `ast`.
 * It is an error to pass an id of a non-match or an index outside the match.
 *
 * "very start" is based on the definition of caretTargetForStartOfPattern
 *)
let caretTargetForBeginningOfMatchBranch
    (matchID : ID.t) (index : int) (ast : FluidAST.t) : caretTarget =
  let maybeTarget =
    match FluidAST.find matchID ast with
    | Some (EMatch (_, _, branches)) ->
        branches
        |> List.getAt ~index
        |> Option.map ~f:(fun (pattern, _) ->
               caretTargetForStartOfPattern pattern)
    | _ ->
        None
  in
  maybeTarget
  |> recoverOpt
       "caretTargetForBeginningOfMatchBranch got an invalid id/idx"
       ~debug:(matchID, index)
       ~default:{astRef = ARInvalid; offset = 0}


(* caretTargetForEndOfMatchPattern returns a caretTarget representing caret
 * placement at the end of the match pattern in the branch identified by `matchID` and `index`
 * within the `ast`.
 * It is an error to pass an id of a non-match or an index outside the match.
 *
 * "end" is based on the definition of caretTargetForEndOfPattern
 *)
let caretTargetForEndOfMatchPattern
    (ast : FluidAST.t) (matchID : ID.t) (index : int) : caretTarget =
  let maybeTarget =
    match FluidAST.find matchID ast with
    | Some (EMatch (_, _, branches)) ->
        branches
        |> List.getAt ~index
        |> Option.map ~f:(fun (pattern, _) ->
               caretTargetForEndOfPattern pattern)
    | _ ->
        None
  in
  maybeTarget
  |> recoverOpt
       "caretTargetForEndOfMatchPattern got an invalid id/index"
       ~debug:(matchID, index)
       ~default:{astRef = ARInvalid; offset = 0}


let loadTL (tlid : TLID.t) (m : model) : model =
  let m = {m with cursorState = FluidEntering tlid} in
  TL.get m tlid
  |> Option.andThen ~f:TL.getAST
  |> Option.map ~f:(fun ast ->
         let editors = FluidEditor.State.init tlid ast in
         let ac = AC.reset m in
         {m with fluidState = {m.fluidState with editors; ac}})
  |> Option.withDefault ~default:m


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


let updatePattern
    ~(f : fluidPattern -> fluidPattern)
    (matchID : ID.t)
    (patID : ID.t)
    (ast : FluidAST.t) : FluidAST.t =
  FluidAST.update matchID ast ~f:(fun m ->
      match m with
      | EMatch (matchID, expr, pairs) ->
          let rec run p =
            if patID = P.toID p then f p else recursePattern ~f:run p
          in
          let newPairs =
            List.map pairs ~f:(fun (pat, expr) -> (run pat, expr))
          in
          EMatch (matchID, expr, newPairs)
      | _ ->
          m)


let replacePattern
    ~(newPat : fluidPattern) (matchID : ID.t) (patID : ID.t) (ast : FluidAST.t)
    : FluidAST.t =
  updatePattern matchID patID ast ~f:(fun _ -> newPat)


(** addMatchPatternAt adds a new match row (FPBlank, EBlank) into the EMatch
    with `matchId` at `idx`.

    Returns a new ast and fluidState with the action recorded. *)
let addMatchPatternAt
    (matchId : ID.t) (idx : int) (ast : FluidAST.t) (s : fluidState) :
    FluidAST.t * fluidState =
  let action =
    Printf.sprintf "addMatchPatternAt(id=%s idx=%d)" (ID.toString matchId) idx
  in
  let s = recordAction action s in
  let ast =
    FluidAST.update matchId ast ~f:(function
        | EMatch (_, cond, rows) ->
            let newVal = (P.FPBlank (matchId, gid ()), E.newB ()) in
            let newRows = List.insertAt rows ~index:idx ~value:newVal in
            EMatch (matchId, cond, newRows)
        | e ->
            recover "expected to find EMatch to update" ~debug:e e)
  in
  (ast, s)


(* ---------------- *)
(* Blanks *)
(* ---------------- *)

(* [insBlankOrPlaceholderHelper' ins]
 * shouldn't be called directly, only via
 * maybeInsertInBlankExpr or insertInPlaceholderExpr.
 *
 * It encodes the shared behavior of inserting text to
 * blanks or placeholders, which are identical
 * except for when creating lambdas.
 *)
let insBlankOrPlaceholderHelper' (ins : string) : (E.t * caretTarget) option =
  if ins = " " || ins = ","
  then None
  else if ins = "\\"
  then
    recover
      "insBlankOrPlaceholderHelper' - call insertInBlankExpr or insertInPlaceholderExpr instead"
      None
  else
    Some
      (let newID = gid () in
       if ins = "\""
       then (E.EString (newID, ""), CT.forARStringOpenQuote newID 1)
       else if ins = "["
       then (E.EList (newID, []), {astRef = ARList (newID, LPOpen); offset = 1})
       else if ins = "{"
       then
         (E.ERecord (newID, []), {astRef = ARRecord (newID, RPOpen); offset = 1})
       else if Util.isNumber ins
       then
         let intStr = ins |> Util.coerceStringTo63BitInt in
         ( E.EInteger (newID, intStr)
         , {astRef = ARInteger newID; offset = String.length intStr} )
       else
         ( E.EPartial (newID, ins, EBlank (gid ()))
         , {astRef = ARPartial newID; offset = String.length ins} ))


(* [maybeInsertInBlankExpr ins]
 * produces Some (newExpr, newCaretTarget) tuple that should be
 * used to replace a blank when inserting the text [ins], or
 * None if the text shouldn't replace the blank.
 *)
let maybeInsertInBlankExpr (ins : string) : (E.t * caretTarget) option =
  if ins = "\\"
  then
    let newID = gid () in
    Some
      ( E.ELambda (newID, [(gid (), "")], EBlank (gid ()))
      , {astRef = ARLambda (newID, LBPVarName 0); offset = 0} )
  else insBlankOrPlaceholderHelper' ins


(* [insertInBlankExpr ins]
 * produces the (newExpr, newCaretTarget) tuple that should be
 * used to replace a blank when inserting the text [ins].
 *)
let insertInBlankExpr (ins : string) : E.t * caretTarget =
  maybeInsertInBlankExpr ins
  |> Option.withDefault
       ~default:
         (let newID = gid () in
          (E.EBlank newID, {astRef = ARBlank newID; offset = 0}))


(* [insertInPlaceholderExpr ~fnID ~placeholder ~ins ast functions]
 * produces the (newExpr, newCaretTarget) tuple that should be
 * used to replace a placeholder when inserting the text [ins],
 * given a list of [functions] that might contain data about the
 * containing function with [fnID] in [ast].
 *
 * Placeholders are almost the same as blanks but have special behavior
 * in conjunction with lambdas.
 *)
let insertInPlaceholderExpr
    ~(fnID : ID.t)
    ~(placeholder : placeholder)
    ~(ins : string)
    (ast : FluidAST.t)
    (functions : function_ list) : E.t * caretTarget =
  let newID = gid () in
  let lambdaArgs () =
    let fnname =
      match FluidAST.find fnID ast with
      | Some (E.EFnCall (_, name, _, _)) ->
          Some name
      | _ ->
          None
    in
    functions
    |> List.find ~f:(fun f -> Some f.fnName = fnname)
    |> Option.andThen ~f:(fun fn ->
           List.find
             ~f:(fun {paramName; _} -> paramName = placeholder.name)
             fn.fnParameters)
    |> Option.map ~f:(fun p -> p.paramBlock_args)
    |> Option.withDefault ~default:[""]
    |> List.map ~f:(fun str -> (gid (), str))
  in
  let newExpr, newTarget =
    if ins = "\\"
    then
      ( E.ELambda (newID, lambdaArgs (), EBlank (gid ()))
      , (* TODO: if lambdaArgs is a populated list, place caret at the end *)
        {astRef = ARLambda (newID, LBPSymbol); offset = 1} )
    else
      insBlankOrPlaceholderHelper' ins
      (* Just replace with a new blank -- we were creating eg a new list item *)
      |> Option.withDefault
           ~default:(E.EBlank newID, {astRef = ARBlank newID; offset = 0})
  in
  (newExpr, newTarget)


(* -------------------- *)
(* Strings *)
(* -------------------- *)
let maybeCommitStringPartial
    (pos : int)
    (ti : FluidToken.tokenInfo)
    (newAST : FluidAST.t)
    (newState : fluidState) =
  let id = T.tid ti.token in
  (* Handle moving from a partial back to an EString *)
  let valid_escape_chars_alist =
    (* We use this alist for two things: knowing what chars are permitted
     * after a \, and knowing what to replace them with as we go from a
     * display string to the real thing *)
    [("n", "\n"); ("t", "\t"); ("\\", "\\")]
  in
  let valid_escape_chars = valid_escape_chars_alist |> List.map ~f:fst in
  let invalid_escapes_in_string (str : string) : string list =
    let captures =
      (* Capture any single character following a '\'. Escaping is
       * terrible here. *)
      (* takes a unit arg so we create a new re value every time we run -
       * the re value contains state *)
      (* Non-capturing group of even number (inc'l 0) of slashes handles
       * '\\\o' - recognizes that the first pair is a valid sequence
       * ('\\'), and the second is not ('\o') *)
      let re () = [%re "/(?:\\\\\\\\)*\\\\(.)/g"] in
      let rec matches (acc : Js.Re.result list) (re : Js.Re.t) :
          Js.Re.result list =
        match Regex.matches ~re str with
        | None ->
            acc
        | Some r ->
            matches (r :: acc) re
      in
      matches [] (re ())
      |> List.map ~f:Js.Re.captures
      |> List.filterMap ~f:(function
             | [|_whole_match; capture|] ->
                 Some capture
             | _ ->
                 None)
      |> List.filterMap ~f:Js.Nullable.toOption
    in
    captures
    |> List.filter ~f:(fun c -> not (List.member ~value:c valid_escape_chars))
  in
  let origExpr = FluidAST.find id newAST in
  let processStr (str : string) : string =
    valid_escape_chars_alist
    |> List.foldl ~init:str ~f:(fun (from, repl) acc ->
           (* workaround for how "\\" gets escaped *)
           let from = if from == "\\" then "\\\\" else from in
           Regex.replace ~re:(Regex.regex ("\\\\" ^ from)) ~repl acc)
  in
  let newAST =
    FluidAST.update
      ~failIfMissing:false
      ~f:(function
        | EPartial (_, str, EString _) as origExpr ->
            let processedStr = processStr str in
            let invalid_escapes = invalid_escapes_in_string str in
            if not (List.isEmpty invalid_escapes)
            then origExpr (* no-op *)
            else EString (id, processedStr)
        | origExpr ->
            origExpr (* no-op *))
      id
      newAST
  in
  let newState =
    ( match origExpr with
    | Some (EPartial (_, str, EString _)) ->
        let invalid_escapes = invalid_escapes_in_string str in
        if invalid_escapes <> [] then None (* no-op *) else Some str
    | _ ->
        None (* no-op *) )
    |> Option.map ~f:(fun oldStr ->
           let oldOffset = pos - ti.startPos in
           (* We might have shortened the string when we processed its
            * escapes - but only the ones to the left of the cursor would
            * move the cursor *)
           let oldlhs, _ = String.splitAt ~index:oldOffset oldStr in
           let newlhs = processStr oldlhs in
           let newOffset =
             oldOffset + (String.length oldlhs - String.length newlhs)
           in
           let astRef =
             match FluidAST.find id newAST with
             | Some (EString _) ->
                 ARString (id, SPOpenQuote)
             | Some (EPartial _) ->
                 ARPartial id
             | Some expr ->
                 recover
                   "need an ASTRef match for "
                   ~debug:(show_fluidExpr expr)
                   (ARPartial id)
             | _ ->
                 recover "no expr found for ID" ~debug:id (ARPartial id)
           in
           moveToAstRef newState newAST ~offset:(newOffset + 1) astRef)
    (* If origExpr wasn't an EPartial (_, _, EString _), then we didn't
     * change the AST in the updateExpr call, so leave the newState as it
     * was *)
    |> Option.withDefault ~default:newState
  in
  (newAST, newState)


let startEscapingString pos ti (s : fluidState) (ast : FluidAST.t) :
    FluidAST.t * fluidState =
  (* I think this is correct but depends on how we 'render' strings - that
   * is, it is correct because we display '"', which bumps pos by 1. *)
  let offset = getStringIndex ti pos in
  let id = T.tid ti.token in
  let newAst =
    ast
    |> FluidAST.update id ~f:(function
           | EString (_, str) as old_expr ->
               let new_str =
                 String.splitAt ~index:offset str
                 |> (fun (lhs, rhs) -> (lhs, rhs))
                 |> fun (lhs, rhs) -> lhs ^ "\\" ^ rhs
               in
               EPartial (id, new_str, old_expr)
           | e ->
               e (* TODO can't happen *))
  in
  let newState =
    let offset = offset + 1 in
    moveToAstRef s newAst ~offset (ARPartial id)
  in
  (newAst, newState)


(* ---------------- *)
(* Fields *)
(* ---------------- *)

(* [exprToFieldAccess id ~partialID ~fieldID] wraps the expression with `id` in
 * the `ast` with a partial-wrapped field access where the partial has partialID
 * and the field access has fieldID. It produces a (newASt, caretTarget) where
 * the caretTarget represents the end of the partial. *)
let exprToFieldAccess
    (id : ID.t) ~(partialID : ID.t) ~(fieldID : ID.t) (ast : FluidAST.t) :
    FluidAST.t * caretTarget =
  let newAST =
    FluidAST.update id ast ~f:(fun e ->
        EPartial (partialID, "", EFieldAccess (fieldID, e, "")))
  in
  (newAST, {astRef = ARPartial partialID; offset = 0})


(* ---------------- *)
(* Lambdas *)
(* ---------------- *)

let insertLambdaVar
    ~(index : int) ~(name : string) (id : ID.t) (ast : FluidAST.t) : FluidAST.t
    =
  FluidAST.update id ast ~f:(fun e ->
      match e with
      | ELambda (id, vars, expr) ->
          let value = (gid (), name) in
          ELambda (id, List.insertAt ~index ~value vars, expr)
      | _ ->
          recover "not a list in insertLambdaVar" ~debug:e e)


(* ---------------- *)
(* Lets *)
(* ---------------- *)

(** makeIntoLetBody takes the `id` of an expression, which will be made into the
    body of a new ELet.

    Returns a new ast, fluidState, and the id of the newly inserted ELet, which
    may be useful for doing caret placement. *)
let makeIntoLetBody (id : ID.t) (ast : FluidAST.t) (s : fluidState) :
    FluidAST.t * fluidState * ID.t =
  let s =
    recordAction (Printf.sprintf "makeIntoLetBody(%s)" (ID.toString id)) s
  in
  let lid = gid () in
  let ast =
    FluidAST.update id ast ~f:(fun expr -> ELet (lid, "", E.newB (), expr))
  in
  (ast, s, lid)


(* ---------------- *)
(* Records *)
(* ---------------- *)

(* Add a row to the record *)
let addRecordRowAt ?(letter = "") (index : int) (id : ID.t) (ast : FluidAST.t) :
    FluidAST.t =
  FluidAST.update id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, List.insertAt ~index ~value:(letter, E.newB ()) fields)
      | _ ->
          recover "Not a record in addRecordRowAt" ~debug:e e)


let addRecordRowToBack (id : ID.t) (ast : FluidAST.t) : FluidAST.t =
  FluidAST.update id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, fields @ [("", E.newB ())])
      | _ ->
          recover "Not a record in addRecordRowToTheBack" ~debug:e e)


(* recordFieldAtIndex gets the field for the record in the ast with recordID at index,
   or None if the record has no field with that index *)
let recordFieldAtIndex (recordID : ID.t) (index : int) (ast : FluidExpression.t)
    : (string * fluidExpr) option =
  E.find recordID ast
  |> Option.andThen ~f:(fun expr ->
         match expr with E.ERecord (_, fields) -> Some fields | _ -> None)
  |> Option.andThen ~f:(fun fields -> List.getAt ~index fields)


(* recordExprIdAtIndex gets the id of the field value for the record in the ast
   with recordID at index, or None if the record has no field with that index  *)
let recordExprIdAtIndex
    (recordID : ID.t) (index : int) (ast : FluidExpression.t) : ID.t option =
  match recordFieldAtIndex recordID index ast with
  | Some (_, fluidExpr) ->
      Some (E.toID fluidExpr)
  | _ ->
      None


(* [mergeExprs e1 e2] "merges" the given exprs [e1] and [e2] into a single expr
 * and returns that merged expr along with a caret target corresponding to the
 * "middle" of the merge.
 *
 * An example of where this is useful is when deleting a binary operator.
 *   Given 12 + 34, deleting the + results in the EInteger 1234.
 *)
let rec mergeExprs (e1 : fluidExpr) (e2 : fluidExpr) : fluidExpr * caretTarget =
  match (e1, e2) with
  | EPipeTarget _, e2 ->
      (e2, caretTargetForStartOfExpr' e2)
  | _, EBinOp (id, op, lhs, rhs, rail) ->
      (* Example: 1 , (2+3) -> (1|2+3) *)
      let merged, target = mergeExprs e1 lhs in
      (EBinOp (id, op, merged, rhs, rail), target)
  | e1, EBlank _ ->
      (e1, caretTargetForEndOfExpr' e1)
  | EBlank _, e2 ->
      (e2, caretTargetForStartOfExpr' e2)
  | EInteger (id, i1), EInteger (_, i2) ->
      ( EInteger (id, Util.coerceStringTo63BitInt (i1 ^ i2))
      , {astRef = ARInteger id; offset = String.length i1} )
  | EString (id, s1), EString (_, s2) ->
      (EString (id, s1 ^ s2), CT.forARStringText id (String.length s1))
  | e1, _e2 ->
      (* TODO(JULIAN): consider preserving e2 as well, by (for example) creating a partial. *)
      (* recover "mergeExprs can't handle this" ~debug:(show_fluidExpr e1, show_fluidExpr e2) (e1, caretTargetForEndOfExpr' e1) *)
      (e1, caretTargetForEndOfExpr' e1)


let replacePartialWithArguments
    ~(newExpr : E.t) (id : ID.t) (s : state) (ast : FluidAST.t) :
    FluidAST.t * caretTarget =
  let open FluidExpression in
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
               , index )))
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
        ELet (gid (), name, rhs, wrapWithLets ~expr rest)
  in
  let getArgs expr =
    match expr with
    | EFnCall (_, _, exprs, _) | EConstructor (_, _, exprs) ->
        exprs
    | EBinOp (_, _, lhs, rhs, _) ->
        [lhs; rhs]
    | _ ->
        recover "impossible" ~debug:expr []
  in
  let chooseSter ~(oldName : string) ~(oldExpr : E.t) (newAllowed : sendToRail)
      =
    (* decides whether the new function is on the rails. Note that are checking
     * if we should prefer the old setting. *)
    let oldSter =
      match oldExpr with
      | EFnCall (_, _, _, ster) | EBinOp (_, _, _, _, ster) ->
          ster
      | _ ->
          NoRail
    in
    let oldAllowed =
      s.ac.functions
      |> List.find ~f:(fun fn -> fn.fnName = oldName)
      |> Option.map ~f:(fun fn ->
             if List.member ~value:fn.fnReturnTipe Runtime.errorRailTypes
             then Rail
             else NoRail)
      |> Option.withDefault ~default:NoRail
    in
    (* The new function should be on the error rail if it was on the error rail
     * and the new function allows it, or if it wasn't on the error rail, but
     * the old function didn't allow it and the new one does *)
    if newAllowed = Rail && (oldSter = Rail || oldAllowed = NoRail)
    then Rail
    else NoRail
  in
  let isAligned p1 p2 =
    match (p1, p2) with
    | Some (name, tipe, _, _), Some (name', tipe', _, _) ->
        name = name' && tipe = tipe'
    | _, _ ->
        false
  in
  let ctForExpr (expr : fluidExpr) : caretTarget =
    match expr with
    | EBinOp (_id, _opName, lhs, _, _) ->
        caretTargetForStartOfExpr' lhs
    | EFnCall (id, fnName, argExprs, _ster) ->
        argExprs
        |> List.find ~f:(function EPipeTarget _ -> false | _ -> true)
        |> Option.map ~f:(fun arg -> caretTargetForStartOfExpr' arg)
        |> Option.withDefault
             ~default:
               { astRef = ARFnCall id
               ; offset = fnName |> FluidUtil.ghostPartialName |> String.length
               }
    | EConstructor (id, cName, argExprs) ->
        argExprs
        |> List.find ~f:(function EPipeTarget _ -> false | _ -> true)
        |> Option.map ~f:(fun arg -> caretTargetForStartOfExpr' arg)
        |> Option.withDefault
             ~default:{astRef = ARConstructor id; offset = String.length cName}
    | ELet (id, _, _, _) ->
        {astRef = ARLet (id, LPVarName); offset = 0}
    | EIf (_, condExpr, _, _) ->
        caretTargetForStartOfExpr' condExpr
    | EMatch (_, mExpr, _) ->
        caretTargetForStartOfExpr' mExpr
    | expr ->
        caretTargetForEndOfExpr' expr
  in
  let mkExprAndTarget (expr : fluidExpr) : fluidExpr * caretTarget =
    (expr, ctForExpr expr)
  in
  FluidAST.find id ast
  |> Option.map ~f:(function
         (* preserve partials with arguments *)
         | EPartial (_, _, (EFnCall (_, oldName, _, _) as oldExpr))
         | EPartial (_, _, (EBinOp (_, oldName, _, _, _) as oldExpr))
         | EPartial (_, _, (EConstructor (_, oldName, _) as oldExpr)) ->
             let existingExprs = getArgs oldExpr in
             let fetchParams newName placeholderExprs =
               let count =
                 max (List.length existingExprs) (List.length placeholderExprs)
               in
               let newParams =
                 getFunctionParams newName count placeholderExprs
               in
               let oldParams = getFunctionParams oldName count existingExprs in
               let matchedParams, mismatchedParams =
                 List.partition oldParams ~f:(fun p ->
                     List.any newParams ~f:(isAligned p))
                 |> Tuple2.mapAll ~f:Option.values
               in
               let newParams =
                 List.foldl
                   matchedParams
                   ~init:placeholderExprs
                   ~f:(fun (_, _, expr, index) exprs ->
                     List.updateAt ~index ~f:(fun _ -> expr) exprs)
               in
               (newParams, mismatchedParams)
             in
             ( match newExpr with
             | EBinOp (id, newName, lhs, rhs, newSter) ->
                 let ster = chooseSter ~oldName ~oldExpr newSter in
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
                         (EBinOp (id, newName, E.newB (), E.newB (), ster))
                 in
                 (wrapWithLets ~expr:newExpr mismatchedParams, ctForExpr newExpr)
             | EFnCall (id, newName, newExprs, newSter) ->
                 let ster = chooseSter ~oldName ~oldExpr newSter in
                 let newParams, mismatchedParams =
                   fetchParams newName newExprs
                 in
                 let newExpr = EFnCall (id, newName, newParams, ster) in
                 (wrapWithLets ~expr:newExpr mismatchedParams, ctForExpr newExpr)
             | EConstructor _ ->
                 let oldParams =
                   existingExprs
                   |> List.indexedMap ~f:(fun i p ->
                          (* create ugly automatic variable name *)
                          let name = "var_" ^ string_of_int (DUtil.random ()) in
                          (name, Runtime.tipe2str TAny, p, i))
                 in
                 (wrapWithLets ~expr:newExpr oldParams, ctForExpr newExpr)
             | newExpr ->
                 mkExprAndTarget newExpr )
         | _ ->
             mkExprAndTarget newExpr)
  |> Option.map ~f:(fun (newExpr, target) ->
         (FluidAST.replace id ~replacement:newExpr ast, target))
  |> recoverOpt
       "replacePartialWithArguments"
       ~default:(ast, {astRef = ARInvalid; offset = 0})


(* ---------------- *)
(* Partials *)
(* ---------------- *)

let rec extractSubexprFromPartial (expr : E.t) : E.t =
  match expr with
  | EPartial (_, _, subExpr) | ERightPartial (_, _, subExpr) ->
      extractSubexprFromPartial subExpr
  | _ ->
      expr


(* ---------------- *)
(* Pipes *)
(* ---------------- *)

(* Used for piping and wrapping line in let. For both we are often at the last
 * argument of a function call. We want to perform the operation on the entire
 * expression on the last line, not just the expression present in the token at
 * the end of the line. This function helps us find the whole expression that
 * we would want to perform it on.
 *)
let rec findAppropriateParentToWrap
    (oldExpr : FluidExpression.t) (ast : FluidAST.t) : FluidExpression.t option
    =
  let child = oldExpr in
  let parent = FluidAST.findParent (E.toID oldExpr) ast in
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
    | ELambda _
    (* Not sure what to do here, probably nothing fancy *)
    | EFeatureFlag _ ->
        Some child
    | EPipe _ ->
        Some parent
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
let createPipe ~(findParent : bool) (id : ID.t) (ast : FluidAST.t) (s : state) :
    FluidAST.t * state * ID.t option =
  let action =
    Printf.sprintf "createPipe(id=%s findParent=%B)" (ID.toString id) findParent
  in
  let s = recordAction action s in
  let exprToReplace =
    FluidAST.find id ast
    |> Option.andThen ~f:(fun e ->
           if findParent then findAppropriateParentToWrap e ast else Some e)
    |> Option.map ~f:extractSubexprFromPartial
  in
  match exprToReplace with
  | None ->
      (ast, s, None)
  | Some expr ->
      let blankId = gid () in
      let replacement = E.EPipe (gid (), [expr; EBlank blankId]) in
      let ast = FluidAST.replace (E.toID expr) ast ~replacement in
      (ast, s, Some blankId)


(** addPipeExprAt adds a new EBlank into the EPipe with `pipeId` at `idx`.

    Returns a new ast, fluidState with the action recorded, and the id of the
    newly inserted EBlank, which may be useful for doing caret placement. *)
let addPipeExprAt
    (pipeId : ID.t) (idx : int) (ast : FluidAST.t) (s : fluidState) :
    FluidAST.t * fluidState * ID.t =
  let open FluidExpression in
  let action =
    Printf.sprintf "addPipeExprAt(id=%s idx=%d)" (ID.toString pipeId) idx
  in
  let s = recordAction action s in
  let bid = gid () in
  let ast =
    FluidAST.update pipeId ast ~f:(function
        | EPipe (_, exprs) ->
            let exprs = List.insertAt exprs ~index:idx ~value:(EBlank bid) in
            EPipe (pipeId, exprs)
        | e ->
            recover "expected to find EPipe to update" ~debug:e e)
  in
  (ast, s, bid)


(* ---------------- *)
(* Lists *)
(* ---------------- *)

let insertInList ~(index : int) ~(newExpr : E.t) (id : ID.t) (ast : FluidAST.t)
    : FluidAST.t =
  FluidAST.update id ast ~f:(fun e ->
      match e with
      | EList (id, exprs) ->
          EList (id, List.insertAt ~index ~value:newExpr exprs)
      | _ ->
          recover "not a list in insertInList" ~debug:e e)


(* -------------------- *)
(* Autocomplete *)
(* -------------------- *)

let acToExpr (entry : Types.fluidAutocompleteItem) : (E.t * caretTarget) option
    =
  let open FluidExpression in
  let mkBlank () : E.t * caretTarget =
    let bID = gid () in
    (EBlank bID, {astRef = ARBlank bID; offset = 0})
  in
  match entry with
  | FACFunction fn ->
      let count = List.length fn.fnParameters in
      let r =
        if List.member ~value:fn.fnReturnTipe Runtime.errorRailTypes
        then Rail
        else NoRail
      in
      let args = List.initialize count (fun _ -> EBlank (gid ())) in
      if fn.fnInfix
      then
        match args with
        | [lhs; rhs] ->
            Some
              ( EBinOp (gid (), fn.fnName, lhs, rhs, r)
              , caretTargetForStartOfExpr' rhs )
        | _ ->
            recover "BinOp doesn't have 2 args" ~debug:args None
      else
        (* functions with arguments should place the caret into the first argument
         * while functions without should place it just after the function name
         * List::head |_list_ [vs] List::empty| *)
        let fID = gid () in
        let target =
          args
          |> List.find ~f:(function EPipeTarget _ -> false | _ -> true)
          |> Option.map ~f:(fun arg -> caretTargetForStartOfExpr' arg)
          |> Option.withDefault
               ~default:
                 { astRef = ARFnCall fID
                 ; offset = fn.fnName |> FluidUtil.partialName |> String.length
                 }
        in
        Some (EFnCall (fID, fn.fnName, args, r), target)
  | FACKeyword KLet ->
      let b, target = mkBlank () in
      Some (ELet (gid (), "", b, E.newB ()), target)
  | FACKeyword KIf ->
      let b, target = mkBlank () in
      Some (EIf (gid (), b, E.newB (), E.newB ()), target)
  | FACKeyword KLambda ->
      let lID = gid () in
      Some
        ( ELambda (lID, [(gid (), "")], E.newB ())
        , {astRef = ARLambda (lID, LBPVarName 0); offset = 0} )
  | FACKeyword KMatch ->
      let matchID = gid () in
      let b, target = mkBlank () in
      Some
        (EMatch (matchID, b, [(FPBlank (matchID, gid ()), E.newB ())]), target)
  | FACKeyword KPipe ->
      let b, target = mkBlank () in
      Some (EPipe (gid (), [b; E.newB ()]), target)
  | FACVariable (name, _) ->
      let vID = gid () in
      Some
        ( EVariable (vID, name)
        , {astRef = ARVariable vID; offset = String.length name} )
  | FACLiteral "true" ->
      let bID = gid () in
      Some
        (EBool (bID, true), {astRef = ARBool bID; offset = String.length "true"})
  | FACLiteral "false" ->
      let bID = gid () in
      Some
        ( EBool (bID, false)
        , {astRef = ARBool bID; offset = String.length "false"} )
  | FACLiteral "null" ->
      let nID = gid () in
      Some (ENull nID, {astRef = ARNull nID; offset = String.length "null"})
  | FACConstructorName (name, argCount) ->
      let args = List.initialize argCount (fun _ -> EBlank (gid ())) in
      let expr = EConstructor (gid (), name, args) in
      Some (expr, caretTargetForEndOfExpr' expr)
  | FACField fieldname ->
      let fID = gid () in
      Some
        ( EFieldAccess (fID, E.newB (), fieldname)
        , { astRef = ARFieldAccess (fID, FAPFieldname)
          ; offset = String.length fieldname } )
  | FACLiteral _ ->
      recover "invalid literal in autocomplete" ~debug:entry None
  | FACPattern _ ->
      (* This only works for exprs *)
      None


let acToPattern (entry : Types.fluidAutocompleteItem) :
    (fluidPattern * caretTarget) option =
  let selectedPat : P.t option =
    match entry with
    | FACPattern p ->
      ( match p with
      | FPAConstructor (mID, patID, var, pats) ->
          Some (FPConstructor (mID, patID, var, pats))
      | FPAVariable (mID, patID, var) ->
          Some (FPVariable (mID, patID, var))
      | FPABool (mID, patID, var) ->
          Some (FPBool (mID, patID, var))
      | FPANull (mID, patID) ->
          Some (FPNull (mID, patID)) )
    | _ ->
        (* This only works for patterns *)
        None
  in
  selectedPat |> Option.map ~f:(fun p -> (p, caretTargetForEndOfPattern p))


let acToPatternOrExpr (entry : Types.fluidAutocompleteItem) :
    E.fluidPatOrExpr * caretTarget =
  acToPattern entry
  |> Option.map ~f:(fun (pat, target) -> (E.Pat pat, target))
  |> Option.orElseLazy (fun () ->
         acToExpr entry
         |> Option.map ~f:(fun (expr, target) -> (E.Expr expr, target)))
  |> recoverOpt
       "acToPatternOrExpr"
       ~debug:entry
       ~default:(E.Expr (E.newB ()), {astRef = ARInvalid; offset = 0})


let initAC (s : state) (m : Types.model) : state = {s with ac = AC.init m}

let isAutocompleting (ti : T.tokenInfo) (s : state) : bool =
  T.isAutocompletable ti.token
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


let acMaybeShow (ti : T.tokenInfo) (s : state) : state =
  let s = recordAction "acShow" s in
  if T.isAutocompletable ti.token && s.ac.index = None
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


(* Check to see if we should open autocomplete at new position *)
let updatePosAndAC (ast : FluidAST.t) (newPos : int) (s : state) : state =
  (* Update newPos and reset upDownCol and reset AC *)
  let s = setPosition ~resetUD:true s newPos |> acClear in
  getToken ast s
  |> Option.map ~f:(fun ti -> acMaybeShow ti s)
  |> Option.withDefault ~default:s


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
    (key : K.key) (currCaretTarget : caretTarget) (s : state) (ast : FluidAST.t)
    : state =
  let tokens = tokensOfFocusedEditor ast s in
  let caretTarget : caretTarget =
    match key with
    | K.Tab ->
        getNextBlank s.newPos tokens
        |> Option.andThen ~f:(fun nextBlankTi ->
               caretTargetFromTokenInfo nextBlankTi.startPos nextBlankTi)
        |> Option.withDefault ~default:currCaretTarget
    | K.ShiftTab ->
        getPrevBlank s.newPos tokens
        |> Option.andThen ~f:(fun prevBlankTi ->
               caretTargetFromTokenInfo prevBlankTi.startPos prevBlankTi)
        |> Option.withDefault ~default:currCaretTarget
    | K.Enter ->
        currCaretTarget
    | K.Space ->
        (* TODO: consider skipping over non-whitespace separators
          as well, such as the commas in a list:
          we currently do [aced|,___]
          but could do    [aced,|___]
        *)
        let startPos = posFromCaretTarget ast s currCaretTarget in
        ( match getNeighbours ~pos:startPos tokens with
        | _, R (TNewline _, _), _ ->
            (* If we're on a newline, don't move forward *)
            currCaretTarget
        | _ ->
            caretTargetForNextNonWhitespaceToken ~pos:startPos ast s
            |> Option.withDefault ~default:currCaretTarget )
    | _ ->
        currCaretTarget
  in
  moveToCaretTarget (acClear s) ast caretTarget


let updateFromACItem
    (entry : fluidAutocompleteItem)
    (ti : T.tokenInfo)
    (ast : FluidAST.t)
    (s : state)
    (key : K.key) : FluidAST.t * state =
  let open FluidExpression in
  let id = T.tid ti.token in
  let newPatOrExpr, newTarget = acToPatternOrExpr entry in
  let oldExpr = FluidAST.find id ast in
  let parent = FluidAST.findParent id ast in
  let newAST, target =
    match (ti.token, oldExpr, parent, newPatOrExpr) with
    (* since patterns have no partial but commit as variables
     * automatically, allow intermediate variables to
     * be autocompletable to other expressions *)
    | ( (TPatternBlank (mID, pID, _) | TPatternVariable (mID, pID, _, _))
      , _
      , _
      , Pat newPat ) ->
        let newAST = replacePattern ~newPat mID pID ast in
        (newAST, newTarget)
    | ( (TPartial _ | TRightPartial _)
      , Some
          ((ERightPartial (_, _, subExpr) | EPartial (_, _, subExpr)) as oldExpr)
      , _
      , _ )
      when entry = FACKeyword KPipe ->
        (* The pipe operator is intended to be roughly "line-based", which
         * means tht instead of tying this to the smallest expression (which is
         * within the partial) we go back and figure out the "line", which is
         * to say the largest expression that doesn't break a line. *)
        let exprToReplace =
          findAppropriateParentToWrap oldExpr ast
          |> Option.map ~f:extractSubexprFromPartial
        in
        ( match exprToReplace with
        | None ->
            let blank = E.newB () in
            let replacement = EPipe (gid (), [subExpr; blank]) in
            ( FluidAST.replace (E.toID oldExpr) ast ~replacement
            , caretTargetForEndOfExpr' blank )
        | Some expr when expr = subExpr ->
            let blank = E.newB () in
            let replacement = EPipe (gid (), [subExpr; blank]) in
            ( FluidAST.replace (E.toID oldExpr) ast ~replacement
            , caretTargetForEndOfExpr' blank )
        | Some expr ->
            let blank = E.newB () in
            let expr = E.replace (E.toID oldExpr) expr ~replacement:subExpr in
            let replacement = EPipe (gid (), [expr; blank]) in
            ( FluidAST.replace (E.toID expr) ast ~replacement
            , caretTargetForEndOfExpr' blank ) )
    | TPartial _, _, Some (EPipe _), Expr (EBinOp (bID, name, _, rhs, str)) ->
        let replacement = EBinOp (bID, name, EPipeTarget (gid ()), rhs, str) in
        let newAST = FluidAST.replace ~replacement id ast in
        (newAST, caretTargetForEndOfExpr' replacement)
    | ( TPartial _
      , Some _
      , Some (EPipe _)
      , Expr (EFnCall (fnID, name, _ :: args, str)) ) ->
        let newExpr = EFnCall (fnID, name, EPipeTarget (gid ()) :: args, str) in
        (* We can't use the newTarget because it might point to eg a blank
         * replaced with an argument *)
        replacePartialWithArguments ~newExpr id s ast
    | ( TPartial _
      , Some (EPartial (_, _, EBinOp (_, _, lhs, rhs, _)))
      , _
      , Expr (EBinOp (bID, name, _, _, str)) ) ->
        let replacement = EBinOp (bID, name, lhs, rhs, str) in
        let newAST = FluidAST.replace ~replacement id ast in
        (newAST, caretTargetForStartOfExpr' rhs)
    | TPartial _, _, _, Expr newExpr ->
        (* We can't use the newTarget because it might point to eg a blank
         * replaced with an argument *)
        replacePartialWithArguments ~newExpr id s ast
    | ( TRightPartial _
      , Some (ERightPartial (_, _, oldExpr))
      , _
      , Expr (EBinOp (bID, name, _, rhs, str)) ) ->
        let replacement = EBinOp (bID, name, oldExpr, rhs, str) in
        let newAST = FluidAST.replace ~replacement id ast in
        (newAST, caretTargetForStartOfExpr' rhs)
    | ( (TFieldName _ | TFieldPartial _ | TBlank _)
      , Some
          ( EFieldAccess (faID, expr, _)
          | EPartial (_, _, EFieldAccess (faID, expr, _)) )
      , _
      , Expr (EFieldAccess (_, _, newname)) ) ->
        let replacement = EFieldAccess (faID, expr, newname) in
        let newAST = FluidAST.replace ~replacement id ast in
        (newAST, caretTargetForEndOfExpr' replacement)
    | _, _, _, Expr newExpr ->
        let newAST = FluidAST.replace ~replacement:newExpr id ast in
        (newAST, newTarget)
    | _, _, _, Pat _ ->
        recover
          "updateFromACItem - unhandled pattern"
          ~debug:entry
          (ast, {astRef = ARInvalid; offset = 0})
  in
  let s = {s with ac = {s.ac with query = None}} in
  (newAST, acMoveBasedOnKey key target s newAST)


let acEnter (ti : T.tokenInfo) (ast : FluidAST.t) (s : state) (key : K.key) :
    FluidAST.t * state =
  let s = recordAction ~ti "acEnter" s in
  match AC.highlighted s.ac with
  | None ->
    ( match ti.token with
    | TPatternVariable _ ->
        (ast, moveToNextBlank ~pos:s.newPos ast s)
    | TFieldPartial (partialID, _fieldAccessID, anaID, fieldname) ->
        (* Accept fieldname, even if it's not in the autocomplete *)
        FluidAST.find anaID ast
        |> Option.map ~f:(fun expr ->
               let replacement = E.EFieldAccess (gid (), expr, fieldname) in
               (FluidAST.replace ~replacement partialID ast, s))
        |> Option.withDefault ~default:(ast, s)
    | _ ->
        (ast, s) )
  | Some entry ->
      updateFromACItem entry ti ast s key


let acClick
    (entry : fluidAutocompleteItem)
    (ti : T.tokenInfo)
    (ast : FluidAST.t)
    (s : state) =
  updateFromACItem entry ti ast s K.Enter


(* If `newPos` is outside `ti`, and `ti` matches the current autocomplete entry
 * perfectly, then select and commit that autocomplete entry *)
let commitIfValid
    (newPos : int) (ti : T.tokenInfo) (ast : FluidAST.t) (s : fluidState) :
    FluidAST.t =
  let highlightedText = s.ac |> AC.highlighted |> Option.map ~f:AC.asName in
  let isInside = newPos >= ti.startPos && newPos <= ti.endPos in
  (* TODO: if we can't move off because it's the start/end etc of the ast, we
   * may want to commit anyway. *)
  if (not isInside)
     && (Some (T.toText ti.token) = highlightedText || T.isFieldPartial ti.token)
  then
    let newAST, _ = acEnter ti ast s K.Enter in
    newAST
  else ast


let acMaybeCommit (newPos : int) (ast : FluidAST.t) (s : fluidState) :
    FluidAST.t =
  match s.ac.query with
  | Some (_, ti) ->
      commitIfValid newPos ti ast s
  | None ->
      ast


(* Convert the expr ti into a FieldAccess, using the currently
 * selected Autocomplete value *)
let acStartField (ti : T.tokenInfo) (ast : FluidAST.t) (s : state) :
    FluidAST.t * state =
  let s = recordAction ~ti "acStartField" s in
  match (AC.highlighted s.ac, ti.token) with
  | Some (FACField _ as entry), TFieldName (faID, _, _)
  | Some (FACField _ as entry), TFieldPartial (_, faID, _, _) ->
      let ast, s = updateFromACItem entry ti ast s K.Enter in
      let newAST, target =
        exprToFieldAccess faID ~partialID:(gid ()) ~fieldID:(gid ()) ast
      in
      let newState = moveToCaretTarget s newAST target |> acClear in
      (newAST, newState)
  | Some entry, _ ->
      let replacement =
        match acToPatternOrExpr entry with
        | Expr newExpr, _ignoredTarget ->
            E.EPartial (gid (), "", EFieldAccess (gid (), newExpr, ""))
        | Pat _, _ ->
            recover "acStartField" (E.newB ())
      in
      let target = caretTargetForEndOfExpr' replacement in
      let newAST = FluidAST.replace ~replacement (T.tid ti.token) ast in
      let newState = moveToCaretTarget s newAST target |> acClear in
      (newAST, newState)
  | _ ->
      (ast, s)


(* -------------------- *)
(* Code entering/interaction *)
(* -------------------- *)

type newPosition =
  | SamePlace
  | Exactly of int
  (* The hope is that we can migrate everything to
     AtTarget and then remove this entirely *)
  | AtTarget of caretTarget

let adjustPosForReflow
    ~state
    (newAST : FluidAST.t)
    (oldTI : T.tokenInfo)
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
  let newTokens = tokensOfFocusedEditor newAST state in
  let newTI = List.find newTokens ~f:(fun x -> T.matches oldTI.token x.token) in
  let diff =
    match newTI with Some newTI -> newTI.startPos - oldTI.startPos | None -> 0
  in
  let newPos = oldPos + diff in
  match (adjustment, newTI) with
  | SamePlace, _ ->
      newPos
  | Exactly pos, _ ->
      pos
  | AtTarget target, _ ->
      posFromCaretTarget newAST state target


let idOfASTRef (astRef : astRef) : ID.t option =
  match astRef with
  | ARInteger id
  | ARBool id
  | ARString (id, _)
  | ARFloat (id, _)
  | ARNull id
  | ARBlank id
  | ARLet (id, _)
  | ARIf (id, _)
  | ARBinOp id
  | ARFieldAccess (id, _)
  | ARVariable id
  | ARFnCall id
  | ARPartial id
  | ARRightPartial id
  | ARList (id, _)
  | ARRecord (id, _)
  | ARPipe (id, _)
  | ARConstructor id
  | ARMatch (id, _)
  | ARLambda (id, _)
  | ARPattern (id, _)
  | ARFlag (id, _) ->
      Some id
  | ARInvalid ->
      None


(** [itemsAtCurrAndNextIndex lst idx] produces Some tuple of the
 * item at the given [idx] and at [idx + 1]. If either of the
 * indices is not present in the list, it returns None.
 *)
let rec itemsAtCurrAndNextIndex (lst : 'a list) (idx : int) : ('a * 'a) option =
  match lst with
  | [] | [_] ->
      None
  | a :: (b :: _ as rest) ->
      if idx > 0
      then (itemsAtCurrAndNextIndex [@tailcall]) rest (idx - 1)
      else if idx = 0
      then Some (a, b)
      else None


(* [doExplicitBackspace [currCaretTarget] [ast]] produces the
 * (newAST, newPosition) tuple resulting from performing
 * a backspace-style deletion at [currCaretTarget] in the
 * [ast]. Note that newPosition will be either
 * AtTarget or SamePlace -- either the caret stays in the
 * same place, or it ends up at a specific location.
 *
 * WARNING: in some cases, we may produce caret targets with
 * offsets that are outside of the targetable range.
 * In such cases, we currently rely on the behavior of
 * posFromCaretTarget to clamp the offset.
 *
 * Note also that doExplicitBackspace expects to receive
 * only "real" caret targets (via caretTargetFromTokenInfo);
 * we don't handle certain 0-offset [currCaretTarget]s because
 * we expect to receive tokens to the left of the caret instead
 * of the 0th offset of the caretTarget of the token to the right.
 *
 * A hacky exception to this is Blanks -- there are a few circumstances
 * where we obtain a zero offset for blanks even though the blank is to the
 * right of the caret instead of the left. This is to account for
 * deleting rows in eg a match (see doBackspace in updateKey).
 * Ideally we wouldn't need these hacks.
 *)
let doExplicitBackspace (currCaretTarget : caretTarget) (ast : FluidAST.t) :
    FluidAST.t * newPosition =
  let open FluidExpression in
  let {astRef = currAstRef; offset = currOffset} = currCaretTarget in
  let currCTMinusOne = {astRef = currAstRef; offset = currOffset - 1} in
  let mutation : string -> string =
   fun str -> Util.removeCharAt str (currOffset - 1)
  in
  let mutationAt (str : string) ~(index : int) : string =
    Util.removeCharAt str index
  in
  let doExprBackspace (currAstRef : astRef) (expr : fluidExpr) :
      (E.fluidPatOrExpr * caretTarget) option =
    let mkEBlank : unit -> (E.fluidPatOrExpr * caretTarget) option =
     fun () ->
      let bID = gid () in
      Some (Expr (EBlank bID), {astRef = ARBlank bID; offset = 0})
    in
    let mkPartialOrBlank ~(str : string) ~(oldExpr : fluidExpr) :
        (E.fluidPatOrExpr * caretTarget) option =
      if str = ""
      then mkEBlank ()
      else
        let parID = gid () in
        Some
          ( Expr (EPartial (parID, str, oldExpr))
          , {astRef = ARPartial parID; offset = currOffset - 1} )
    in
    match (currAstRef, expr) with
    | ARInteger _, EInteger (id, intStr) ->
        let str = mutation intStr in
        if str = ""
        then mkEBlank ()
        else
          let coerced = Util.coerceStringTo63BitInt str in
          if coerced = intStr
          then None
          else Some (Expr (EInteger (id, coerced)), currCTMinusOne)
    | ARString (_, kind), EString (id, str) ->
        let strRelOffset = match kind with SPOpenQuote -> currOffset - 1 in
        if strRelOffset = 0 && str = ""
        then mkEBlank ()
        else
          let newStr = str |> mutationAt ~index:(strRelOffset - 1) in
          Some
            ( Expr (EString (id, newStr))
            , CT.forARStringOpenQuote id strRelOffset )
    | ARFloat (_, FPWhole), EFloat (id, whole, frac) ->
        Some (Expr (EFloat (id, mutation whole, frac)), currCTMinusOne)
    | ARFloat (_, FPPoint), EFloat (_, whole, frac) ->
        (* Todo: If the float only consists of a . and has no whole or frac,
          it should become a blank. Instead, it currently becomes a 0, which is weird. *)
        let i = Util.coerceStringTo63BitInt (whole ^ frac) in
        let iID = gid () in
        Some
          ( Expr (EInteger (iID, i))
          , {astRef = ARInteger iID; offset = String.length whole} )
    | ARFloat (_, FPFractional), EFloat (id, whole, frac) ->
        Some (Expr (EFloat (id, whole, mutation frac)), currCTMinusOne)
    | ARLet (_, LPVarName), ELet (id, oldName, value, body) ->
        let newName = mutation oldName in
        let newExpr =
          ELet (id, newName, value, E.renameVariableUses ~oldName ~newName body)
        in
        if newName = ""
        then Some (Expr newExpr, {astRef = currAstRef; offset = 0})
        else Some (Expr newExpr, currCTMinusOne)
    | ARLambda (_, LBPComma varAndSepIdx), ELambda (id, oldVars, oldExpr) ->
        itemsAtCurrAndNextIndex oldVars varAndSepIdx
        |> Option.map ~f:(fun ((_, keepVarName), (_, deleteVarName)) ->
               (* remove expression in front of sep, not behind it, hence + 1 *)
               let newVars = List.removeAt ~index:(varAndSepIdx + 1) oldVars in
               let newExpr = E.removeVariableUse deleteVarName oldExpr in
               Some
                 ( Expr (ELambda (id, newVars, newExpr))
                 , { astRef = ARLambda (id, LBPVarName varAndSepIdx)
                   ; offset = String.length keepVarName } ))
        |> recoverOpt
             "doExplicitBackspace - LPComma"
             ~debug:(varAndSepIdx, oldVars)
             ~default:None
    | ARList (_, LPComma elemAndSepIdx), EList (id, exprs) ->
        let newExpr, target =
          itemsAtCurrAndNextIndex exprs elemAndSepIdx
          |> Option.map ~f:(fun (beforeComma, afterComma) ->
                 mergeExprs beforeComma afterComma)
          |> Option.withDefault
               ~default:(E.newB (), {astRef = ARList (id, LPOpen); offset = 1})
        in
        let newExprs =
          (* Considering a is the item at elemAndSepIdx and b is at elemAndSepIdx + 1,
           * we merge a and b in [...a,b...] by replacing a with ab and removing b *)
          exprs
          |> List.updateAt ~index:elemAndSepIdx ~f:(fun _ -> newExpr)
          |> List.removeAt ~index:(elemAndSepIdx + 1)
        in
        Some (Expr (EList (id, newExprs)), target)
    | (ARRecord (_, RPOpen), expr | ARList (_, LPOpen), expr)
      when E.isEmpty expr ->
        mkEBlank ()
    | ARRecord (_, RPFieldname index), ERecord (id, nameValPairs) ->
        List.getAt ~index nameValPairs
        |> Option.map ~f:(function
               | "", _ ->
                   let target =
                     match
                       recordExprIdAtIndex id (index - 1) (FluidAST.toExpr ast)
                     with
                     | None ->
                         { astRef = ARRecord (id, RPOpen)
                         ; offset = 1 (* right after the { *) }
                     | Some exprId ->
                         caretTargetForEndOfExpr exprId ast
                   in
                   ( Expr (ERecord (id, List.removeAt ~index nameValPairs))
                   , target )
               | name, _ ->
                   let nameValPairs =
                     List.updateAt nameValPairs ~index ~f:(fun (_, expr) ->
                         (mutation name, expr))
                   in
                   (Expr (ERecord (id, nameValPairs)), currCTMinusOne))
    | ARFieldAccess (_, FAPFieldOp), EFieldAccess (_, faExpr, _)
    | ARFieldAccess (_, FAPFieldOp), EPartial (_, _, EFieldAccess (_, faExpr, _))
      ->
        Some (Expr faExpr, caretTargetForEndOfExpr' faExpr)
    | ARConstructor _, (EConstructor (_, str, _) as oldExpr) ->
        mkPartialOrBlank ~str:(str |> mutation |> String.trim) ~oldExpr
    | ARFnCall _, (EFnCall (_, fnName, _, _) as oldExpr) ->
        mkPartialOrBlank
          ~str:(fnName |> FluidUtil.partialName |> mutation |> String.trim)
          ~oldExpr
    | ARBool _, (EBool (_, bool) as oldExpr) ->
        let str = if bool then "true" else "false" in
        mkPartialOrBlank ~str:(mutation str) ~oldExpr
    | ARNull _, (ENull _ as oldExpr) ->
        mkPartialOrBlank ~str:(mutation "null") ~oldExpr
    | ARVariable _, (EVariable (_, varName) as oldExpr) ->
        mkPartialOrBlank ~str:(mutation varName) ~oldExpr
    | ARBinOp _, (EBinOp (_, op, lhsExpr, rhsExpr, _) as oldExpr) ->
        let str = op |> FluidUtil.ghostPartialName |> mutation |> String.trim in
        if str = ""
        then
          (* Delete the binop *)
          let expr, target = mergeExprs lhsExpr rhsExpr in
          Some (Expr expr, target)
        else
          let newID = gid () in
          Some
            ( Expr (EPartial (newID, str, oldExpr))
            , {astRef = ARPartial newID; offset = currOffset - 1} )
    | ( ARFieldAccess (_, FAPFieldname)
      , (EFieldAccess (_, _, fieldName) as oldExpr) ) ->
        (* Note that str is allowed to be empty in partials *)
        let str = mutation fieldName in
        let newID = gid () in
        Some
          ( Expr (EPartial (newID, str, oldExpr))
          , {astRef = ARPartial newID; offset = currOffset - 1} )
    | ARPartial _, EPartial (id, oldStr, oldExpr) ->
        let str = oldStr |> mutation |> String.trim in
        if str = ""
        then
          (* inlined version of deletePartial, with appropriate exceptions added *)
          match oldExpr with
          | EFieldAccess _ ->
              (* This is allowed to be the empty string. *)
              Some (Expr (EPartial (id, str, oldExpr)), currCTMinusOne)
          | EBinOp (_, _, lhsExpr, rhsExpr, _) ->
              let expr, target = mergeExprs lhsExpr rhsExpr in
              Some (Expr expr, target)
          | _ ->
              mkEBlank ()
        else if String.startsWith ~prefix:"\"" str
                && String.endsWith ~suffix:"\"" str
        then
          let newID = gid () in
          Some
            ( Expr (EString (newID, String.slice ~from:1 ~to_:(-1) str))
            , CT.forARStringText newID (currOffset - 1) )
        else Some (Expr (EPartial (id, str, oldExpr)), currCTMinusOne)
    | ARRightPartial _, ERightPartial (id, oldStr, oldValue) ->
        let str = oldStr |> mutation |> String.trim in
        if str = ""
        then Some (Expr oldValue, caretTargetForEndOfExpr' oldValue)
        else Some (Expr (ERightPartial (id, str, oldValue)), currCTMinusOne)
    | ARLambda (_, LBPVarName index), ELambda (id, vars, expr) ->
        vars
        |> List.getAt ~index
        |> Option.map ~f:(fun (_, oldName) ->
               let newName = oldName |> mutation in
               (* Note that newName is intentionally
                        allowed to be "" with no special handling *)
               let vars =
                 List.updateAt vars ~index ~f:(fun (varId, _) ->
                     (varId, newName))
               in
               ( Expr
                   (ELambda
                      (id, vars, E.renameVariableUses ~oldName ~newName expr))
               , currCTMinusOne ))
    | ARPipe (_, idx), EPipe (id, exprChain) ->
      ( match exprChain with
      | [e1; _] ->
          Some (Expr e1, caretTargetForEndOfExpr' e1)
      | exprs ->
          exprs
          |> List.getAt ~index:idx
          |> Option.map ~f:(fun expr ->
                 (* remove expression in front of pipe, not behind it *)
                 Some
                   ( Expr (EPipe (id, List.removeAt ~index:(idx + 1) exprs))
                   , caretTargetForEndOfExpr' expr ))
          |> recoverOpt "doExplicitBackspace ARPipe" ~default:None )
    (*
     * Delete leading keywords of empty expressions
     *)
    | ARLet (_, LPKeyword), ELet (_, varName, expr, EBlank _)
    | ARLet (_, LPKeyword), ELet (_, varName, EBlank _, expr)
      when varName = "" || varName = "_" ->
        Some (Expr expr, caretTargetForStartOfExpr' expr)
    | ARIf (_, IPIfKeyword), EIf (_, EBlank _, EBlank _, EBlank _)
    | ARLambda (_, LBPSymbol), ELambda (_, _, EBlank _) ->
        (* If the expr is empty and thus can be removed *)
        mkEBlank ()
    | ARMatch (_, MPKeyword), EMatch (_, EBlank _, pairs)
      when List.all pairs ~f:(fun (p, e) ->
               match (p, e) with P.FPBlank _, EBlank _ -> true | _ -> false) ->
        (* the match has no content and can safely be deleted *)
        mkEBlank ()
    | ARMatch (_, MPKeyword), EMatch _
    | ARLet (_, LPKeyword), ELet _
    | ARIf (_, IPIfKeyword), EIf _
    | ARLambda (_, LBPSymbol), ELambda _ ->
        (* keywords of "non-empty" exprs shouldn't be deletable at all *)
        None
    (*
     * Immutable; just jump to the start
     *)
    | ARMatch (id, MPBranchArrow idx), expr ->
        Some (Expr expr, caretTargetForEndOfMatchPattern ast id idx)
    | ARIf (_, IPThenKeyword), expr
    | ARIf (_, IPElseKeyword), expr
    | ARLambda (_, LBPArrow), expr
    | ARBlank _, expr
    | ARLet (_, LPAssignment), expr
    | ARRecord (_, RPOpen), expr
    | ARRecord (_, RPClose), expr
    | ARRecord (_, RPFieldSep _), expr
    | ARList (_, LPOpen), expr
    | ARList (_, LPClose), expr
    | ARFlag (_, FPWhenKeyword), expr
    | ARFlag (_, FPEnabledKeyword), expr ->
        (* We could alternatively move by a single character instead,
           which is what the old version did with minor exceptions;
           that isn't particularly useful as typing within these
           is meaningless and we want this to bring you to a location
           where you can meaningfully type. *)
        Some (Expr expr, {astRef = currAstRef; offset = 0})
    (*****************
     * Exhaustiveness
     *)
    | ARBinOp _, _
    | ARBool _, _
    | ARConstructor _, _
    | ARFieldAccess (_, FAPFieldname), _
    | ARFieldAccess (_, FAPFieldOp), _
    | ARFloat (_, FPFractional), _
    | ARFloat (_, FPPoint), _
    | ARFloat (_, FPWhole), _
    | ARFnCall _, _
    | ARIf (_, IPIfKeyword), _
    | ARInteger _, _
    | ARLambda (_, LBPSymbol), _
    | ARLambda (_, LBPComma _), _
    | ARLambda (_, LBPVarName _), _
    | ARLet (_, LPKeyword), _
    | ARLet (_, LPVarName), _
    | ARList (_, LPComma _), _
    | ARMatch (_, MPKeyword), _
    | ARNull _, _
    | ARPartial _, _
    | ARPipe (_, _), _
    | ARRecord (_, RPFieldname _), _
    | ARRightPartial _, _
    | ARString (_, SPOpenQuote), _
    | ARVariable _, _
    (*
     * Non-exprs
     *)
    | ARPattern _, _
    | ARInvalid, _ ->
        recover
          "doExplicitBackspace - unexpected expr"
          ~debug:(show_astRef currAstRef)
          None
  in
  let doPatternBackspace
      (patContainerRef : ID.t option ref)
      (currAstRef : astRef)
      (pat : fluidPattern) : (E.fluidPatOrExpr * caretTarget) option =
    let mkPBlank (matchID : ID.t) : (E.fluidPatOrExpr * caretTarget) option =
      let bID = gid () in
      Some
        ( Pat (FPBlank (matchID, bID))
        , {astRef = ARPattern (bID, PPBlank); offset = 0} )
    in
    match (currAstRef, pat) with
    | ARPattern (_, PPBlank), FPBlank (mID, pID) ->
        if currOffset = 0
        then
          match FluidAST.find mID ast with
          | Some (EMatch (_, cond, patterns)) ->
              patContainerRef := Some mID ;
              patterns
              (* FIXME: This is super broken because the pattern id could be anywhere
                 but we only check at the pattern root *)
              |> List.findIndex ~f:(fun (p, _) -> P.toID p = pID)
              |> Option.map ~f:(fun remIdx ->
                     let newPatterns =
                       if List.length patterns = 1
                       then patterns
                       else List.removeAt patterns ~index:remIdx
                     in
                     let targetExpr =
                       patterns
                       |> List.getAt ~index:(remIdx - 1)
                       |> Option.map ~f:(fun (_, e) -> e)
                       |> Option.withDefault ~default:cond
                     in
                     let target = caretTargetForEndOfExpr' targetExpr in
                     (E.Expr (EMatch (mID, cond, newPatterns)), target))
          | _ ->
              recover "doExplicitBackspace PPBlank" None
        else Some (Pat (FPBlank (mID, pID)), {astRef = currAstRef; offset = 0})
    | ARPattern (_, PPVariable), FPVariable (mID, pID, oldName) ->
        patContainerRef := Some mID ;
        let newName = mutation oldName in
        let newPat, target =
          if newName = ""
          then
            ( P.FPBlank (mID, pID)
            , {astRef = ARPattern (pID, PPBlank); offset = 0} )
          else (P.FPVariable (mID, pID, newName), currCTMinusOne)
        in
        ( match FluidAST.find mID ast with
        | Some (EMatch (_, cond, cases)) ->
            let rec run p =
              if pID = P.toID p then newPat else recursePattern ~f:run p
            in
            let newCases =
              List.map cases ~f:(fun (pat, body) ->
                  if P.findPattern pID pat <> None
                  then (run pat, E.renameVariableUses ~oldName ~newName body)
                  else (pat, body))
            in
            Some (Expr (EMatch (mID, cond, newCases)), target)
        | _ ->
            recover "doExplicitBackspace FPVariable" None )
    | ARPattern (_, PPNull), FPNull (mID, _) ->
        let str = mutation "null" in
        let newID = gid () in
        Some
          ( Pat (FPVariable (mID, newID, str))
          , {astRef = ARPattern (newID, PPVariable); offset = currOffset - 1} )
    | ARPattern (_, PPBool), FPBool (mID, _, bool) ->
        let str = if bool then "true" else "false" in
        let newStr = mutation str in
        let newID = gid () in
        Some
          ( Pat (FPVariable (mID, newID, newStr))
          , {astRef = ARPattern (newID, PPVariable); offset = currOffset - 1} )
    | ARPattern (_, PPInteger), FPInteger (mID, pID, intStr) ->
        let str = mutation intStr in
        if str = ""
        then mkPBlank mID
        else
          let coerced = Util.coerceStringTo63BitInt str in
          if coerced = intStr
          then None
          else Some (Pat (FPInteger (mID, pID, coerced)), currCTMinusOne)
    | ARPattern (_, PPConstructor), FPConstructor (mID, _, str, _patterns) ->
        let str = str |> mutation |> String.trim in
        let newID = gid () in
        if str = ""
        then
          Some
            ( Pat (FPBlank (mID, newID))
            , {astRef = ARPattern (newID, PPBlank); offset = 0} )
        else
          Some
            ( Pat (FPVariable (mID, newID, str))
            , {astRef = ARPattern (newID, PPVariable); offset = currOffset - 1}
            )
    (*
    * Floats
    *)
    | ARPattern (_, PPFloat FPWhole), FPFloat (mID, pID, whole, frac) ->
        Some (Pat (FPFloat (mID, pID, mutation whole, frac)), currCTMinusOne)
    | ARPattern (_, PPFloat FPPoint), FPFloat (mID, _, whole, frac) ->
        (* TODO: If the float only consists of a . and has no whole or frac,
           it should become a blank. Instead, it currently becomes a 0, which is weird. *)
        let i = Util.coerceStringTo63BitInt (whole ^ frac) in
        let iID = gid () in
        Some
          ( Pat (FPInteger (mID, iID, i))
          , {astRef = ARPattern (iID, PPInteger); offset = String.length whole}
          )
    | ARPattern (_, PPFloat FPFractional), FPFloat (mID, pID, whole, frac) ->
        Some (Pat (FPFloat (mID, pID, whole, mutation frac)), currCTMinusOne)
    (*
    * Strings
    *)
    | ARPattern (_, PPString kind), FPString ({matchID; patternID; str} as data)
      ->
        let strRelOffset = match kind with SPOpenQuote -> currOffset - 1 in
        if strRelOffset = 0 && str = ""
        then mkPBlank matchID
        else
          let str = str |> mutationAt ~index:(strRelOffset - 1) in
          Some
            ( Pat (FPString {data with str})
            , CT.forPPStringOpenQuote patternID strRelOffset )
    (*****************
     * Exhaustiveness
     *)
    | ARPattern (_, PPBlank), _
    | ARPattern (_, PPBool), _
    | ARPattern (_, PPConstructor), _
    | ARPattern (_, PPFloat FPFractional), _
    | ARPattern (_, PPFloat FPPoint), _
    | ARPattern (_, PPFloat FPWhole), _
    | ARPattern (_, PPInteger), _
    | ARPattern (_, PPNull), _
    | ARPattern (_, PPString SPOpenQuote), _
    | ARPattern (_, PPVariable), _
    (*
     * non-patterns
     *)
    | ARBinOp _, _
    | ARBlank _, _
    | ARBool _, _
    | ARConstructor _, _
    | ARFieldAccess _, _
    | ARFloat _, _
    | ARFnCall _, _
    | ARIf _, _
    | ARInteger _, _
    | ARLambda _, _
    | ARLet _, _
    | ARList _, _
    | ARMatch _, _
    | ARNull _, _
    | ARPartial _, _
    | ARPipe _, _
    | ARRecord _, _
    | ARRightPartial _, _
    | ARString _, _
    | ARVariable _, _
    | ARFlag _, _
    | ARInvalid, _ ->
        recover
          "doExplicitBackspace - unexpected pat"
          ~debug:(show_astRef currAstRef)
          None
  in
  (* FIXME: This is an ugly hack so we can modify match branches when editing a pattern.
     There's probably a nice way to do this without a ref, but that's a bigger change.
   *)
  let patContainerRef : ID.t option ref = ref None in
  idOfASTRef currAstRef
  |> Option.andThen ~f:(fun patOrExprID ->
         match E.findExprOrPat patOrExprID (Expr (FluidAST.toExpr ast)) with
         | Some patOrExpr ->
             Some (patOrExprID, patOrExpr)
         | None ->
             None)
  |> Option.andThen ~f:(fun (patOrExprID, patOrExpr) ->
         let maybeTransformedExprAndCaretTarget =
           match patOrExpr with
           | E.Pat pat ->
               doPatternBackspace patContainerRef currAstRef pat
           | E.Expr expr ->
               doExprBackspace currAstRef expr
         in
         match maybeTransformedExprAndCaretTarget with
         | Some (Expr newExpr, target) ->
             let patOrExprID =
               match !patContainerRef with
               | None ->
                   patOrExprID
               | Some mID ->
                   mID
             in
             Some
               ( FluidAST.replace patOrExprID ~replacement:newExpr ast
               , AtTarget target )
         | Some (Pat newPat, target) ->
             let mID = P.toMatchID newPat in
             let newAST = replacePattern mID patOrExprID ~newPat ast in
             Some (newAST, AtTarget target)
         | None ->
             None)
  |> Option.withDefault ~default:(ast, SamePlace)


let doBackspace
    ~(pos : int) (ti : T.tokenInfo) (ast : FluidAST.t) (s : fluidState) :
    FluidAST.t * fluidState =
  let s = recordAction ~pos ~ti "doBackspace" s in
  let newAST, newPosition =
    match caretTargetFromTokenInfo pos ti with
    | Some ct ->
        doExplicitBackspace ct ast
    | None ->
        (ast, Exactly ti.startPos)
  in
  let newPos = adjustPosForReflow ~state:s newAST ti pos newPosition in
  let newS = updatePosAndAC newAST newPos s in
  (newAST, newS)


let doDelete ~(pos : int) (ti : T.tokenInfo) (ast : FluidAST.t) (s : state) :
    FluidAST.t * state =
  (* Delete is approximately the same as backspace 1 place ahead,
   * as long as we process the token after the caret.
   * The only real difference is with multi-syllable extended grapheme clusters
   * like गा, which we currently fail to support, much as we currently fail to support
   * multi-codepoint emoji (the expected behavior of multi-syllable clusters differs from emoji).
   *
   * Note that we do not handle caret affinity properly, but caret affinity should behave the
   * same for backspace and delete.
   *
   * See https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad.
   *)
  let s = recordAction ~pos ~ti "doDelete" s in
  let newAST, newPosition =
    match caretTargetFromTokenInfo pos ti with
    | Some ct ->
        doExplicitBackspace {ct with offset = ct.offset + 1} ast
    | None ->
        (ast, SamePlace)
  in
  (* Delete should only move the caret if the AST changed; that is a difference from backspace *)
  let newAST, newPosition =
    if newAST = ast then (ast, SamePlace) else (newAST, newPosition)
  in
  let newPos = adjustPosForReflow ~state:s newAST ti pos newPosition in
  (newAST, {s with newPos})


(* [doExplicitInsert [extendedGraphemeCluster] [currCaretTarget] [ast]]
 * produces the (newAST, newPosition) tuple resulting from performing
 * a text insertion at [currCaretTarget] in the [ast].
 * Note that newPosition will be either AtTarget or SamePlace --
 * either the caret stays in the same place, or it ends up at a specific location.
 *
 * Note that there are some special-case inserts that aren't handled by doExplicitInsert.
 * See doInsert and updateKey for these exceptional cases.
 *)
let doExplicitInsert
    (extendedGraphemeCluster : string)
    (currCaretTarget : caretTarget)
    (ast : FluidAST.t) : FluidAST.t * newPosition =
  let {astRef = currAstRef; offset = currOffset} = currCaretTarget in
  let caretDelta = extendedGraphemeCluster |> String.length in
  let currCTPlusLen = {astRef = currAstRef; offset = currOffset + caretDelta} in
  let mutation : string -> string =
   fun str ->
    str |> String.insertAt ~index:currOffset ~insert:extendedGraphemeCluster
  in
  let mutationAt (str : string) ~(index : int) : string =
    str |> String.insertAt ~index ~insert:extendedGraphemeCluster
  in
  let doExprInsert (currAstRef : astRef) (expr : fluidExpr) :
      (fluidExpr * caretTarget) option =
    let mkPartial (str : string) (oldExpr : fluidExpr) :
        (fluidExpr * caretTarget) option =
      let newID = gid () in
      Some
        ( EPartial (newID, str, oldExpr)
        , {astRef = ARPartial newID; offset = currOffset + caretDelta} )
    in
    match (currAstRef, expr) with
    | ARString (_, kind), EString (id, str) ->
        let len = String.length str in
        let strRelOffset = match kind with SPOpenQuote -> currOffset - 1 in
        if strRelOffset < 0 || strRelOffset > len
        then
          (* out of string bounds means you can't insert into the string *)
          None
        else
          let newStr = str |> mutationAt ~index:strRelOffset in
          Some
            ( EString (id, newStr)
            , CT.forARStringText id (strRelOffset + caretDelta) )
    | ARFloat (_, kind), EFloat (id, whole, frac) ->
        if FluidUtil.isNumber extendedGraphemeCluster
        then
          let isWhole, index =
            match kind with
            | FPWhole ->
                (true, currOffset)
            | FPFractional ->
                (false, currOffset)
            | FPPoint ->
                if currCaretTarget.offset = 0
                then (true, String.length whole)
                else (false, 0)
          in
          if isWhole
          then
            let newWhole = mutationAt whole ~index in
            (* This enables |.67 -> 0|.67 but prevents |1.0 -> 0|1.0 *)
            if String.slice ~from:0 ~to_:1 newWhole = "0"
               && String.length newWhole > 1
            then None
            else
              Some
                ( EFloat (id, newWhole, frac)
                , {astRef = ARFloat (id, FPWhole); offset = index + caretDelta}
                )
          else
            let newFrac = mutationAt frac ~index in
            Some
              ( EFloat (id, whole, newFrac)
              , { astRef = ARFloat (id, FPFractional)
                ; offset = index + caretDelta } )
        else None
    | ARLambda (_, LBPVarName index), ELambda (id, vars, expr) ->
        vars
        |> List.getAt ~index
        |> Option.andThen ~f:(fun (_, oldName) ->
               let newName = oldName |> mutation in
               if FluidUtil.isValidIdentifier newName
               then
                 let vars =
                   List.updateAt vars ~index ~f:(fun (varId, _) ->
                       (varId, newName))
                 in
                 Some
                   ( E.ELambda
                       (id, vars, E.renameVariableUses ~oldName ~newName expr)
                   , currCTPlusLen )
               else None)
    | ARPartial _, EPartial (id, oldStr, oldExpr) ->
        let str = oldStr |> mutation |> String.trim in
        if String.startsWith ~prefix:"\"" str
           && String.endsWith ~suffix:"\"" str
        then
          let newID = gid () in
          Some
            ( EString (newID, String.slice ~from:1 ~to_:(-1) str)
            , CT.forARStringOpenQuote newID (currOffset + caretDelta) )
        else Some (EPartial (id, str, oldExpr), currCTPlusLen)
    | ARRightPartial _, ERightPartial (id, oldStr, oldValue) ->
        let str = oldStr |> mutation |> String.trim in
        Some (ERightPartial (id, str, oldValue), currCTPlusLen)
    | ARBinOp _, (EBinOp (_, op, _, _, _) as oldExpr) ->
        let str = op |> FluidUtil.partialName |> mutation |> String.trim in
        mkPartial str oldExpr
    | ARInteger _, EInteger (id, intStr) ->
        if currCaretTarget.offset = 0 && extendedGraphemeCluster = "0"
        then
          (* This prevents inserting leading 0s at the beginning of the int.
           * Note that Util.coerceStringTo63BitInt currently coerces strings with
           * leading "0"s to "0"; this prevents coerceStringTo63BitInt getting
           * a leading 0 in the first place. If Util.coerceStringTo63BitInt could
           * deal with leading 0s, we would still need this special case to deal with
           * caret placement, unless Util.coerceStringTo63BitInt preserved leading 0s.
           *)
          None
        else if Util.isNumber extendedGraphemeCluster
        then
          let str = mutation intStr in
          let coerced = Util.coerceStringTo63BitInt str in
          if coerced = intStr
          then None
          else Some (EInteger (id, coerced), currCTPlusLen)
        else if extendedGraphemeCluster = "."
        then
          let newID = gid () in
          let whole, frac = String.splitAt ~index:currOffset intStr in
          Some
            ( EFloat (newID, whole, frac)
            , {astRef = ARFloat (newID, FPPoint); offset = 1} )
        else None
    | ARRecord (_, RPFieldname index), ERecord (id, nameValPairs) ->
        List.getAt ~index nameValPairs
        |> Option.andThen ~f:(fun (name, _) ->
               let newName = mutation name in
               if FluidUtil.isValidIdentifier newName
               then
                 let nameValPairs =
                   List.updateAt nameValPairs ~index ~f:(fun (_, expr) ->
                       (newName, expr))
                 in
                 Some (E.ERecord (id, nameValPairs), currCTPlusLen)
               else None)
    | ( ARFieldAccess (_, FAPFieldname)
      , (EFieldAccess (_, _, fieldName) as oldExpr) ) ->
        let newName = mutation fieldName in
        if FluidUtil.isValidIdentifier newName
        then mkPartial newName oldExpr
        else None
    | ARFieldAccess (_, FAPFieldOp), old ->
        recover
          "doExplicitInsert - ARFieldAccess-FAPFieldOp is unhandled and doesn't seem to happen in practice"
          ~debug:old
          None
    | ARVariable _, (E.EVariable (_, varName) as oldExpr) ->
        mkPartial (mutation varName) oldExpr
    | ARNull _, (E.ENull _ as oldExpr) ->
        mkPartial (mutation "null") oldExpr
    | ARBool _, (E.EBool (_, bool) as oldExpr) ->
        let str = if bool then "true" else "false" in
        mkPartial (mutation str) oldExpr
    | ARLet (_, LPVarName), ELet (id, oldName, value, body) ->
        let newName = mutation oldName in
        if FluidUtil.isValidIdentifier newName
        then
          Some
            ( ELet
                (id, newName, value, E.renameVariableUses ~oldName ~newName body)
            , currCTPlusLen )
        else None
    | ARBlank _, _ ->
        maybeInsertInBlankExpr extendedGraphemeCluster
    (*
     * Things you can't edit but probably should be able to edit
     *)
    | ARFnCall _, EFnCall _ | ARConstructor _, EConstructor _ ->
        None
    (*
     * Immutable keywords and symbols
     *)
    | ARLambda (_, LBPComma _), _
    | ARLambda (_, LBPSymbol), _
    | ARLambda (_, LBPArrow), _
    | ARRecord (_, RPOpen), _
    | ARRecord (_, RPFieldSep _), _
    | ARRecord (_, RPClose), _
    | ARList (_, LPOpen), _
    | ARList (_, LPComma _), _
    | ARList (_, LPClose), _
    | ARLet (_, LPKeyword), _
    | ARLet (_, LPAssignment), _
    | ARIf (_, IPIfKeyword), _
    | ARIf (_, IPThenKeyword), _
    | ARIf (_, IPElseKeyword), _
    | ARPipe (_, _), _
    | ARMatch (_, MPKeyword), _
    | ARMatch (_, MPBranchArrow _), _
    | ARFlag (_, FPWhenKeyword), _
    | ARFlag (_, FPEnabledKeyword), _ ->
        None
    (*****************
     * Exhaustiveness
     *)
    | ARBinOp _, _
    | ARBool _, _
    | ARConstructor _, _
    | ARFieldAccess (_, FAPFieldname), _
    | ARFloat (_, FPFractional), _
    | ARFloat (_, FPPoint), _
    | ARFloat (_, FPWhole), _
    | ARFnCall _, _
    | ARInteger _, _
    | ARLambda (_, LBPVarName _), _
    | ARLet (_, LPVarName), _
    | ARNull _, _
    | ARPartial _, _
    | ARRecord (_, RPFieldname _), _
    | ARRightPartial _, _
    | ARString (_, SPOpenQuote), _
    | ARVariable _, _
    (*
     * Non-exprs
     *)
    | ARPattern _, _
    | ARInvalid, _ ->
        recover
          "doExplicitBackspace - unexpected expr"
          ~debug:(show_astRef currAstRef)
          None
  in
  let doPatInsert
      (patContainerRef : ID.t option ref)
      (currAstRef : astRef)
      (pat : fluidPattern) : (E.fluidPatOrExpr * caretTarget) option =
    match (currAstRef, pat) with
    | ARPattern (_, PPFloat kind), FPFloat (mID, pID, whole, frac) ->
        if FluidUtil.isNumber extendedGraphemeCluster
        then
          let isWhole, index =
            match kind with
            | FPWhole ->
                (true, currOffset)
            | FPFractional ->
                (false, currOffset)
            | FPPoint ->
                if currCaretTarget.offset = 0
                then (true, String.length whole)
                else (false, 0)
          in
          if isWhole
          then
            let newWhole = mutationAt whole ~index in
            (* This enables |.67 -> 0|.67 but prevents |1.0 -> 0|1.0 *)
            if String.slice ~from:0 ~to_:1 newWhole = "0"
               && String.length newWhole > 1
            then None
            else
              Some
                ( Pat (FPFloat (mID, pID, newWhole, frac))
                , { astRef = ARPattern (pID, PPFloat FPWhole)
                  ; offset = index + caretDelta } )
          else
            let newFrac = mutationAt frac ~index in
            Some
              ( Pat (FPFloat (mID, pID, whole, newFrac))
              , { astRef = ARPattern (pID, PPFloat FPFractional)
                ; offset = index + caretDelta } )
        else None
    | ARPattern (_, PPNull), FPNull (mID, _) ->
        let str = mutation "null" in
        let newID = gid () in
        if FluidUtil.isValidIdentifier str
        then
          Some
            ( Pat (FPVariable (mID, newID, str))
            , { astRef = ARPattern (newID, PPVariable)
              ; offset = currOffset + caretDelta } )
        else None
    | ARPattern (_, PPBool), FPBool (mID, _, bool) ->
        let str = if bool then "true" else "false" in
        let newStr = mutation str in
        let newID = gid () in
        if FluidUtil.isValidIdentifier str
        then
          Some
            ( Pat (FPVariable (mID, newID, newStr))
            , { astRef = ARPattern (newID, PPVariable)
              ; offset = currOffset + caretDelta } )
        else None
    | ARPattern (_, PPInteger), FPInteger (mID, pID, intStr) ->
        if currCaretTarget.offset = 0 && extendedGraphemeCluster = "0"
        then
          (* This prevents inserting leading 0s at the beginning of the int.
           * Note that Util.coerceStringTo63BitInt currently coerces strings with
           * leading "0"s to "0"; this prevents coerceStringTo63BitInt getting
           * a leading 0 in the first place. If Util.coerceStringTo63BitInt could
           * deal with leading 0s, we would still need this special case to deal with
           * caret placement, unless Util.coerceStringTo63BitInt preserved leading 0s.
           *)
          None
        else if Util.isNumber extendedGraphemeCluster
        then
          let str = mutation intStr in
          let coerced = Util.coerceStringTo63BitInt str in
          if coerced = intStr
          then None
          else Some (Pat (FPInteger (mID, pID, coerced)), currCTPlusLen)
        else if extendedGraphemeCluster = "."
        then
          let newID = gid () in
          let whole, frac = String.splitAt ~index:currOffset intStr in
          Some
            ( Pat (FPFloat (mID, newID, whole, frac))
            , {astRef = ARPattern (newID, PPFloat FPPoint); offset = 1} )
        else None
    | ( ARPattern (_, PPString kind)
      , FPString ({matchID = _; patternID; str} as data) ) ->
        let len = String.length str in
        let strRelOffset = match kind with SPOpenQuote -> currOffset - 1 in
        if strRelOffset < 0 || strRelOffset > len
        then
          (* out of string bounds means you can't insert into the string *)
          None
        else
          let str = str |> mutationAt ~index:strRelOffset in
          Some
            ( Pat (FPString {data with str})
            , CT.forPPStringText patternID (strRelOffset + caretDelta) )
    | ARPattern (_, PPBlank), FPBlank (mID, _) ->
        let newID = gid () in
        if extendedGraphemeCluster = "\""
        then
          Some
            ( Pat (FPString {matchID = mID; patternID = newID; str = ""})
            , CT.forPPStringText newID 0 )
        else if Util.isNumber extendedGraphemeCluster
        then
          Some
            ( Pat
                (FPInteger
                   ( mID
                   , newID
                   , extendedGraphemeCluster |> Util.coerceStringTo63BitInt ))
            , {astRef = ARPattern (newID, PPInteger); offset = caretDelta} )
        else if FluidUtil.isIdentifierChar extendedGraphemeCluster
        then
          Some
            ( Pat (FPVariable (mID, newID, extendedGraphemeCluster))
            , {astRef = ARPattern (newID, PPVariable); offset = caretDelta} )
        else None
    | ARPattern (_, PPVariable), FPVariable (mID, pID, oldName) ->
        let newName = mutation oldName in
        if FluidUtil.isValidIdentifier newName
        then (
          patContainerRef := Some mID ;
          let newPat, target =
            (P.FPVariable (mID, pID, newName), currCTPlusLen)
          in
          match FluidAST.find mID ast with
          | Some (EMatch (_, cond, cases)) ->
              let rec run p =
                if pID = P.toID p then newPat else recursePattern ~f:run p
              in
              let newCases =
                List.map cases ~f:(fun (pat, body) ->
                    if P.findPattern pID pat <> None
                    then (run pat, E.renameVariableUses ~oldName ~newName body)
                    else (pat, body))
              in
              Some (Expr (EMatch (mID, cond, newCases)), target)
          | _ ->
              recover "doExplicitInsert FPVariable" None )
        else None
    (*
     * Things you can't edit but probably should be able to edit
     *)
    | ARPattern (_, PPConstructor), _ ->
        None
    (*****************
     * Exhaustiveness
     *)
    | ARPattern (_, PPBlank), _
    | ARPattern (_, PPBool), _
    | ARPattern (_, PPFloat FPFractional), _
    | ARPattern (_, PPFloat FPPoint), _
    | ARPattern (_, PPFloat FPWhole), _
    | ARPattern (_, PPInteger), _
    | ARPattern (_, PPNull), _
    | ARPattern (_, PPString SPOpenQuote), _
    | ARPattern (_, PPVariable), _
    (*
     * non-patterns
     *)
    | ARBinOp _, _
    | ARBlank _, _
    | ARBool _, _
    | ARConstructor _, _
    | ARFieldAccess _, _
    | ARFloat _, _
    | ARFnCall _, _
    | ARIf _, _
    | ARInteger _, _
    | ARLambda _, _
    | ARLet _, _
    | ARList _, _
    | ARMatch _, _
    | ARNull _, _
    | ARPartial _, _
    | ARPipe _, _
    | ARRecord _, _
    | ARRightPartial _, _
    | ARString _, _
    | ARVariable _, _
    | ARFlag _, _
    | ARInvalid, _ ->
        recover
          "doExplicitInsert - unexpected pat"
          ~debug:(show_astRef currAstRef)
          None
  in
  (* FIXME: This is an ugly hack so we can modify match branches when editing a pattern.
     There's probably a nice way to do this without a ref, but that's a bigger change.
   *)
  let patContainerRef : ID.t option ref = ref None in
  idOfASTRef currAstRef
  |> Option.andThen ~f:(fun patOrExprID ->
         match E.findExprOrPat patOrExprID (Expr (FluidAST.toExpr ast)) with
         | Some patOrExpr ->
             Some (patOrExprID, patOrExpr)
         | None ->
             None)
  |> Option.andThen ~f:(fun (patOrExprID, patOrExpr) ->
         let maybeTransformedExprAndCaretTarget =
           match patOrExpr with
           | E.Pat pat ->
               doPatInsert patContainerRef currAstRef pat
           | E.Expr expr ->
             ( match doExprInsert currAstRef expr with
             | None ->
                 None
             | Some (expr, ct) ->
                 Some (Expr expr, ct) )
         in
         match maybeTransformedExprAndCaretTarget with
         | Some (Expr newExpr, target) ->
             let patOrExprID =
               match !patContainerRef with
               | None ->
                   patOrExprID
               | Some mID ->
                   mID
             in
             Some
               ( FluidAST.replace patOrExprID ~replacement:newExpr ast
               , AtTarget target )
         | Some (Pat newPat, target) ->
             let mID = P.toMatchID newPat in
             let newAST = replacePattern mID patOrExprID ~newPat ast in
             Some (newAST, AtTarget target)
         | None ->
             None)
  |> Option.withDefault ~default:(ast, SamePlace)


let doInsert
    ~pos (letter : string) (ti : T.tokenInfo) (ast : FluidAST.t) (s : state) :
    FluidAST.t * state =
  let s = recordAction ~ti ~pos "doInsert" s in
  let s = {s with upDownCol = None} in
  let newAST, newPosition =
    match caretTargetFromTokenInfo pos ti with
    | Some ct ->
        doExplicitInsert letter ct ast
    | None ->
        (ast, SamePlace)
  in
  let newPos = adjustPosForReflow ~state:s newAST ti pos newPosition in
  (newAST, {s with newPos})


(** [doInfixInsert ~pos infixTxt ti ast s] produces the
 * (newAST, newState) tuple resulting from performing
 * the insertion of the string [infixTxt], which we know
 * conforms to FluidTextInput.isInfixSymbol, within the
 * token with token info [ti], knowing that the [ast]-relative
 * caret position is [pos] and that the current state is [s].
 *
 * In most cases, we wrap the expr indicated by the [ti]
 * in a partial, particularly if we are at the end of an expr.
 * Otherwise (especially if we are in the middle of an expr), we
 * defer to the behavior of doInsert.
 *)
let doInfixInsert
    ~pos (infixTxt : string) (ti : T.tokenInfo) (ast : FluidAST.t) (s : state) :
    FluidAST.t * state =
  caretTargetFromTokenInfo pos ti
  |> Option.andThen ~f:(fun ct ->
         idOfASTRef ct.astRef
         |> Option.andThen ~f:(fun id ->
                match FluidAST.find id ast with
                | Some expr ->
                    Some (id, ct, expr)
                | None ->
                    None))
  |> Option.andThen ~f:(fun (id, ct, expr) ->
         match (ct.astRef, expr) with
         | ARInteger _, expr
         | ARBool _, expr
         | ARFieldAccess (_, FAPFieldname), expr
         | ARString (_, SPOpenQuote), expr
         | ARFloat _, expr
         | ARNull _, expr
         | ARVariable _, expr
         | ARList _, expr
         | ARRecord _, expr
         (* This works for function calls because
          * the caretTargetForEndOfExpr' of a function
          * call will only line up with the
          * (caretTargetFromTokenInfo pos ti) if all its
          * arguments have been filled (because it has none or
          * because they're filled with pipe targets)
          *)
         | ARFnCall _, expr ->
             if caretTargetForEndOfExpr' expr = ct
             then
               let newID = gid () in
               Some
                 ( id
                 , E.ERightPartial (newID, infixTxt, expr)
                 , { astRef = ARRightPartial newID
                   ; offset = String.length infixTxt } )
             else None
         | ARBlank _, expr ->
             let newID = gid () in
             Some
               ( id
               , E.EPartial (newID, infixTxt, expr)
               , {astRef = ARPartial newID; offset = String.length infixTxt} )
         | ARPipe (_, index), E.EPipe (id, pipeExprs) ->
           ( match pipeExprs |> List.getAt ~index:(index + 1) with
           | Some pipedInto ->
               (* Note that this essentially destroys the pipedInto
                * expression, because it becomes overwritten by the
                * partial. Handling this properly would involve
                * introducing a new construct -- perhaps a left partial. *)
               let parID = gid () in
               let newExpr = E.EPartial (parID, infixTxt, pipedInto) in
               let newPipeExprs =
                 pipeExprs
                 |> List.updateAt ~index:(index + 1) ~f:(fun _ -> newExpr)
               in
               Some
                 ( id
                 , E.EPipe (id, newPipeExprs)
                 , {astRef = ARPartial parID; offset = String.length infixTxt}
                 )
           | None ->
               None )
         (* Exhaustiveness *)
         | ARPipe _, _ ->
             None
         (* Don't insert *)
         | ARFieldAccess (_, FAPFieldOp), _
         | ARLet _, _
         | ARIf _, _
         | ARBinOp _, _
         | ARPartial _, _
         | ARRightPartial _, _
         | ARConstructor _, _
         | ARMatch _, _
         | ARLambda _, _
         | ARPattern _, _
         | ARFlag _, _ ->
             None
         | ARInvalid, _ ->
             None)
  |> Option.map ~f:(fun (replaceID, newExpr, newCaretTarget) ->
         let newAST = FluidAST.replace replaceID ~replacement:newExpr ast in
         (newAST, moveToCaretTarget s newAST newCaretTarget))
  |> Option.orElseLazy (fun () -> Some (doInsert ~pos infixTxt ti ast s))
  |> recoverOpt
       "updateKey - can't return None due to lazy Some"
       ~default:(ast, s)


let wrapInLet (ti : T.tokenInfo) (ast : FluidAST.t) (s : state) :
    FluidAST.t * fluidState =
  let s = recordAction "wrapInLet" s in
  let id = T.tid ti.token in
  match FluidAST.find id ast with
  | Some expr ->
      let bodyId = gid () in
      let exprToWrap =
        match findAppropriateParentToWrap expr ast with
        | Some e ->
            e
        | None ->
            expr
      in
      let eid = E.toID exprToWrap in
      let replacement = E.ELet (gid (), "_", exprToWrap, EBlank bodyId) in
      let newAST = FluidAST.replace ~replacement eid ast in
      let newTarget = {astRef = ARBlank bodyId; offset = 0} in
      (newAST, moveToCaretTarget s newAST newTarget)
  | None ->
      (ast, s)


let orderRangeFromSmallToBig ((rangeBegin, rangeEnd) : int * int) : int * int =
  if rangeBegin > rangeEnd
  then (rangeEnd, rangeBegin)
  else (rangeBegin, rangeEnd)


(* Always returns a selection represented as two ints with the smaller int first.
   The numbers are identical if there is no selection. *)
let getSelectionRange (s : fluidState) : int * int =
  match s.selectionStart with
  | Some beginIdx when beginIdx < s.newPos ->
      (beginIdx, s.newPos)
  | Some endIdx ->
      (s.newPos, endIdx)
  | None ->
      (s.newPos, s.newPos)


let updateSelectionRange (s : fluidState) (newPos : int) : fluidState =
  { s with
    newPos
  ; selectionStart =
      Some (s.selectionStart |> Option.withDefault ~default:s.newPos) }


let getOptionalSelectionRange (s : fluidState) : (int * int) option =
  let endIdx = s.newPos in
  match s.selectionStart with
  | Some beginIdx ->
      Some (beginIdx, endIdx)
  | None ->
      None


let tokensInRange
    (ast : FluidAST.t) (s : fluidState) (selStartPos : int) (selEndPos : int) :
    fluidTokenInfo list =
  tokensOfFocusedEditor ast s
  (* this condition is a little flaky, sometimes selects wrong tokens *)
  |> List.filter ~f:(fun t ->
         (* selectionStart within token *)
         (t.startPos <= selStartPos && selStartPos < t.endPos)
         (* selectionEnd within token *)
         || (t.startPos < selEndPos && selEndPos <= t.endPos)
         (* tokenStart within selection  *)
         || (selStartPos <= t.startPos && t.startPos < selEndPos)
         (* tokenEnd within selection  *)
         || (selStartPos < t.endPos && t.endPos <= selEndPos))


let getTopmostSelectionID
    (ast : FluidAST.t) (s : fluidState) (startPos : int) (endPos : int) :
    ID.t option =
  (* TODO: if there's multiple topmost IDs, return parent of those IDs *)
  tokensInRange ast s startPos endPos
  |> List.filter ~f:(fun ti -> not (T.isNewline ti.token))
  |> List.foldl ~init:(None, 0) ~f:(fun ti (topmostID, topmostDepth) ->
         let curID = T.parentExprID ti.token in
         let curDepth = FluidAST.ancestors curID ast |> List.length in
         if (* check if current token is higher in the AST than the last token,
             * or if there's no topmost ID yet *)
            (curDepth < topmostDepth || topmostID = None)
            (* account for tokens that don't have ancestors (depth = 0)
             * but are not the topmost expression in the AST *)
            && not
                 ( curDepth = 0
                 && FluidAST.find curID ast != Some (FluidAST.toExpr ast) )
         then (Some curID, curDepth)
         else (topmostID, topmostDepth))
  |> Tuple2.first


let getSelectedExprID (ast : FluidAST.t) (s : fluidState) : ID.t option =
  getOptionalSelectionRange s
  |> Option.andThen ~f:(getTopmostSelectionID ast s |> Tuple2.uncurry)


let maybeOpenCmd (m : Types.model) : Types.modification =
  let getExprIDOnCaret (ast : FluidAST.t) (s : fluidState) (tl : toplevel) =
    match getToken ast s with
    | Some ti ->
        let id = T.tid ti.token in
        if T.validID id then Some (TL.id tl, id) else None
    | None ->
        None
  in
  TL.selected m
  |> Option.thenAlso ~f:TL.getAST
  |> Option.andThen ~f:(fun (tl, ast) ->
         getSelectedExprID ast m.fluidState
         |> Option.map ~f:(fun id -> (TL.id tl, id))
         |> Option.orElseLazy (fun _ -> getExprIDOnCaret ast m.fluidState tl))
  |> Option.map ~f:(fun (tlid, id) -> FluidCommandsShow (tlid, id))
  |> Option.withDefault ~default:NoChange


let rec updateKey
    ?(recursing = false)
    (inputEvent : fluidInputEvent)
    (ast : FluidAST.t)
    (s : fluidState) : FluidAST.t * state =
  let pos = s.newPos in
  let tokens = tokensOfFocusedEditor ast s in
  (* These might be the same token *)
  let toTheLeft, toTheRight, mNext = getNeighbours ~pos tokens in
  let onEdge =
    match (toTheLeft, toTheRight) with
    | L (lt, lti), R (rt, rti) ->
        (lt, lti) <> (rt, rti)
    | _ ->
        true
  in
  let keyIsInfix =
    match inputEvent with
    | InsertText txt when FluidTextInput.isInfixSymbol txt ->
        true
    | _ ->
        false
  in
  (* TODO: When changing TVariable and TFieldName and probably TFnName we
   * should convert them to a partial which retains the old object *)
  (* Checks to see if the token is within an if-condition statement *)
  let isInIfCondition token =
    let rec recurseUp maybeExpr prevId =
      match maybeExpr with
      | Some (E.EIf (_, cond, _, _)) when E.toID cond = prevId ->
          true
      | Some e ->
          let id = E.toID e in
          recurseUp (FluidAST.findParent id ast) id
      | None ->
          false
    in
    let tid = T.tid token in
    recurseUp (FluidAST.findParent tid ast) tid
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
    match (inputEvent, toTheLeft, toTheRight) with
    (****************)
    (* AUTOCOMPLETE *)
    (****************)
    (* Note that these are spelt out explicitly on purpose, else they'll
     * trigger on the wrong element sometimes. *)
    | Keypress {key = K.Escape; _}, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acClear s)
    | Keypress {key = K.Escape; _}, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acClear s)
    | Keypress {key = K.Up; _}, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acMoveUp s)
    | Keypress {key = K.Up; _}, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acMoveUp s)
    | Keypress {key = K.Down; _}, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acMoveDown s)
    | Keypress {key = K.Down; _}, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acMoveDown s)
    (*
     * Autocomplete finish
     *)
    | Keypress {key; _}, L (_, ti), _
      when isAutocompleting ti s
           && [K.Enter; K.Tab; K.ShiftTab; K.Space] |> List.member ~value:key ->
        acEnter ti ast s key
    | Keypress {key; _}, _, R (_, ti)
      when isAutocompleting ti s
           && [K.Enter; K.Tab; K.ShiftTab; K.Space] |> List.member ~value:key ->
        acEnter ti ast s key
    (* When we type a letter/number after an infix operator, complete and
     * then enter the number/letter. *)
    | InsertText txt, L (TRightPartial (_, _), ti), _
      when onEdge && Util.isIdentifierChar txt ->
        let ast, s = acEnter ti ast s K.Tab in
        let tokens = tokensOfFocusedEditor ast s in
        getLeftTokenAt s.newPos (List.reverse tokens)
        |> Option.map ~f:(fun ti -> doInsert ~pos:s.newPos txt ti ast s)
        |> Option.withDefault ~default:(ast, s)
    (*
     * Special autocomplete entries
     *)
    (* Piping, with and without autocomplete menu open *)
    | Keypress {key = K.ShiftEnter; _}, left, _ ->
        let doPipeline (ast : FluidAST.t) (s : fluidState) :
            FluidAST.t * fluidState =
          let startPos, endPos = getSelectionRange s in
          let topmostID = getTopmostSelectionID ast s startPos endPos in
          let findParent = startPos = endPos in
          Option.map topmostID ~f:(fun id ->
              let ast, s, blankId = createPipe ~findParent id ast s in
              match blankId with
              | None ->
                  (ast, s)
              | Some id ->
                  let s = moveToAstRef s ast (ARBlank id) in
                  (ast, s))
          |> Option.withDefault ~default:(ast, s)
        in
        ( match left with
        | (L (TPartial _, ti) | L (TFieldPartial _, ti))
          when Option.is_some (AC.highlighted s.ac) ->
            let ast, s = acEnter ti ast s K.Enter in
            doPipeline ast s
        | _ ->
            doPipeline ast s )
    (* press dot while in a variable entry *)
    | InsertText ".", L (TPartial _, ti), _
      when Option.map ~f:AC.isVariable (AC.highlighted s.ac) = Some true ->
        acStartField ti ast s
    | InsertText ".", L (TFieldPartial _, ti), _
    | InsertText ".", _, R (TFieldPartial _, ti)
      when Option.map ~f:AC.isField (AC.highlighted s.ac) = Some true ->
        acStartField ti ast s
    (********************)
    (* CARET NAVIGATION *)
    (********************)
    (* Tab to next blank *)
    | Keypress {key = K.Tab; _}, _, R (_, _)
    | Keypress {key = K.Tab; _}, L (_, _), _ ->
        (ast, moveToNextBlank ~pos ast s)
    | Keypress {key = K.ShiftTab; _}, _, R (_, _)
    | Keypress {key = K.ShiftTab; _}, L (_, _), _ ->
        (ast, moveToPrevBlank ~pos ast s)
    (* Left/Right movement *)
    | Keypress {key = K.GoToEndOfWord maintainSelection; _}, _, R (_, ti)
    | Keypress {key = K.GoToEndOfWord maintainSelection; _}, L (_, ti), _ ->
        if maintainSelection == K.KeepSelection
        then (ast, updateSelectionRange s (getEndOfWordPos ast s ~pos ti))
        else (ast, goToEndOfWord ~pos ast ti s)
    | Keypress {key = K.GoToStartOfWord maintainSelection; _}, _, R (_, ti)
    | Keypress {key = K.GoToStartOfWord maintainSelection; _}, L (_, ti), _ ->
        if maintainSelection == K.KeepSelection
        then (ast, updateSelectionRange s (getStartOfWordPos ast s ~pos ti))
        else (ast, goToStartOfWord ast s ti ~pos)
    | Keypress {key = K.Left; _}, L (_, ti), _ ->
        (ast, doLeft ~pos ti s |> acMaybeShow ti)
    | Keypress {key = K.Right; _}, _, R (_, ti) ->
        (ast, doRight ~pos ~next:mNext ti s |> acMaybeShow ti)
    | Keypress {key = K.GoToStartOfLine maintainSelection; _}, _, R (_, ti)
    | Keypress {key = K.GoToStartOfLine maintainSelection; _}, L (_, ti), _ ->
        if maintainSelection == K.KeepSelection
        then (ast, updateSelectionRange s (getStartOfLineCaretPos ast s ti))
        else (ast, moveToStartOfLine ast s ti)
    | Keypress {key = K.GoToEndOfLine maintainSelection; _}, _, R (_, ti) ->
        if maintainSelection == K.KeepSelection
        then (ast, updateSelectionRange s (getEndOfLineCaretPos ast s ti))
        else (ast, moveToEndOfLine ast s ti)
    | Keypress {key = K.Up; _}, _, _ ->
        (ast, doUp ~pos ast s)
    | Keypress {key = K.Down; _}, _, _ ->
        (ast, doDown ~pos ast s)
    (*************)
    (* SELECTION *)
    (*************)
    | Keypress {key = K.SelectAll; _}, _, R (_, _)
    | Keypress {key = K.SelectAll; _}, L (_, _), _ ->
        (ast, selectAll ~pos ast s)
    (*************)
    (* OVERWRITE *)
    (*************)
    | ReplaceText txt, _, _ ->
        replaceText ~ast ~state:s txt
    (*************)
    (* DELETION  *)
    (*************)
    (* Delete selection *)
    | (DeleteContentBackward, _, _ | DeleteContentForward, _, _)
      when Option.isSome s.selectionStart ->
        deleteSelection ~state:s ~ast
    (* Special-case hack for deleting rows of a match or record *)
    | DeleteContentBackward, _, R (TRecordFieldname {fieldName = ""; _}, ti)
    | DeleteContentBackward, L (TNewline _, _), R (TPatternBlank _, ti) ->
        doBackspace ~pos ti ast s
    | DeleteContentBackward, L (_, ti), _ ->
        doBackspace ~pos ti ast s
    | DeleteContentForward, _, R (_, ti) ->
        doDelete ~pos ti ast s
    | DeleteSoftLineBackward, _, R (_, ti)
    | DeleteSoftLineBackward, L (_, ti), _ ->
      (* The behavior of this action is not well specified -- every editor we've seen has slightly different behavior.
           The behavior we use here is: if there is a selection, delete it instead of deleting to start of line (like XCode but not VSCode).
           For expedience, delete to the visual start of line rather than the "real" start of line. This is symmetric with
           K.DeleteToEndOfLine but does not match any code editors we've seen. It does match many non-code text editors. *)
      ( match getOptionalSelectionRange s with
      | Some selRange ->
          deleteCaretRange ~state:s ~ast selRange
      | None ->
          deleteCaretRange
            ~state:s
            ~ast
            (s.newPos, getStartOfLineCaretPos ast s ti) )
    | DeleteSoftLineForward, _, R (_, ti) | DeleteSoftLineForward, L (_, ti), _
      ->
      (* The behavior of this action is not well specified -- every editor we've seen has slightly different behavior.
           The behavior we use here is: if there is a selection, delete it instead of deleting to end of line (like XCode and VSCode).
           For expedience, in the presence of wrapping, delete to the visual end of line rather than the "real" end of line.
           This matches the behavior of XCode and VSCode. Most standard non-code text editors do not implement this command. *)
      ( match getOptionalSelectionRange s with
      | Some selRange ->
          deleteCaretRange ~state:s ~ast selRange
      | None ->
          deleteCaretRange
            ~state:s
            ~ast
            (s.newPos, getEndOfLineCaretPos ast s ti) )
    | DeleteWordForward, _, R (_, ti) ->
      ( match getOptionalSelectionRange s with
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
    | DeleteWordBackward, L (_, ti), _ ->
      ( match getOptionalSelectionRange s with
      | Some selRange ->
          deleteCaretRange ~state:s ~ast selRange
      | None ->
          let rangeStart =
            if T.isStringToken ti.token && pos != ti.startPos
            then getBegOfWordInStrCaretPos ~pos ti
            else ti.startPos
          in
          deleteCaretRange ~state:s ~ast (rangeStart, pos) )
    (****************************************)
    (* SKIPPING OVER SYMBOLS BY TYPING THEM *)
    (****************************************)
    (*
     * Skipping over a lambda arrow with '->'
     *)
    | InsertText "-", L (TLambdaVar _, _), R (TLambdaArrow _, ti) ->
        (* ___| -> ___ to ___ |-> ___ *)
        (ast, moveOneRight (ti.startPos + 1) s)
    | InsertText "-", L (TLambdaArrow _, _), R (TLambdaArrow _, ti)
      when pos = ti.startPos + 1 ->
        (* ___ |-> ___ to ___ -|> ___ *)
        (ast, moveOneRight (ti.startPos + 1) s)
    | InsertText ">", L (TLambdaArrow _, _), R (TLambdaArrow _, ti)
      when pos = ti.startPos + 2 ->
        (* ___ -|> ___ to ___ -> |___ *)
        (ast, moveToNextNonWhitespaceToken ~pos ast s)
    (*
     * Skipping over specific characters
     *)
    | InsertText "=", _, R (TLetAssignment _, toTheRight) ->
        (ast, moveTo toTheRight.endPos s)
    | InsertText ":", _, R (TRecordSep _, toTheRight) ->
        (ast, moveTo toTheRight.endPos s)
    | Keypress {key = K.Space; _}, _, R (TSep _, _) ->
        (ast, moveOneRight pos s)
    (* Pressing } to go over the last } *)
    | InsertText "}", _, R (TRecordClose _, ti) when pos = ti.endPos - 1 ->
        (ast, moveOneRight pos s)
    (* Pressing ] to go over the last ] *)
    | InsertText "]", _, R (TListClose _, ti) when pos = ti.endPos - 1 ->
        (ast, moveOneRight pos s)
    (* Pressing quote to go over the last quote *)
    | InsertText "\"", _, R (TPatternString _, ti)
    | InsertText "\"", _, R (TString _, ti)
    | InsertText "\"", _, R (TStringMLEnd _, ti)
      when pos = ti.endPos - 1 ->
        (ast, moveOneRight pos s)
    (***************************)
    (* CREATING NEW CONSTRUCTS *)
    (***************************)
    (* Entering a string escape
     * TODO: Move this to doInsert *)
    | InsertText "\\", L (TString _, _), R (TString _, ti)
      when false (* disable for now *) && pos - ti.startPos != 0 ->
        startEscapingString pos ti s ast
    (* comma - add another of the thing *)
    | InsertText ",", L (TListOpen id, _), _ when onEdge ->
        let bID = gid () in
        let newExpr, target =
          (E.EBlank bID (* new separators *), {astRef = ARBlank bID; offset = 0})
        in
        let newAST = insertInList ~index:0 id ~newExpr ast in
        (newAST, moveToCaretTarget s newAST target)
    | InsertText ",", L (TLambdaSymbol id, _), _ when onEdge ->
        let newAST = insertLambdaVar ~index:0 id ~name:"" ast in
        let target = {astRef = ARLambda (id, LBPVarName 0); offset = 0} in
        (newAST, moveToCaretTarget s newAST target)
    | InsertText ",", L (TLambdaVar (id, _, index, _), _), _ when onEdge ->
        let newAST = insertLambdaVar ~index:(index + 1) id ~name:"" ast in
        let target =
          {astRef = ARLambda (id, LBPVarName (index + 1)); offset = 0}
        in
        (newAST, moveToCaretTarget s newAST target)
    | InsertText ",", _, R (TLambdaVar (id, _, index, _), _) when onEdge ->
        let target = {astRef = ARLambda (id, LBPVarName index); offset = 0} in
        let newAST = insertLambdaVar ~index id ~name:"" ast in
        let newState = moveToCaretTarget s newAST target in
        (newAST, newState)
    | InsertText ",", L (t, ti), _ ->
        if onEdge
        then
          (* If we are at the end of an expression,
           * we check if we are in a list.
           * If so, we add a blank after the current
           * index in the list and place the caret
           * in that blank. *)
          let exprID = T.tid t in
          ( match FluidAST.findParent exprID ast with
          | Some (E.EList (listID, exprs)) ->
              exprs
              |> List.findIndex ~f:(fun e -> E.toID e = exprID)
              |> Option.map ~f:(fun listIdx -> (listID, listIdx))
          | _ ->
              None )
          |> Option.map ~f:(fun (listID, listIdx) ->
                 let newExpr, target =
                   let bID = gid () in
                   (E.EBlank bID, {astRef = ARBlank bID; offset = 0})
                 in
                 (insertInList ~index:(listIdx + 1) ~newExpr listID ast, target))
          |> Option.map ~f:(fun (newAST, newTarget) ->
                 (newAST, moveToCaretTarget s newAST newTarget))
          |> Option.withDefault ~default:(ast, s)
        else doInsert ~pos "," ti ast s
    (* Field access *)
    | InsertText ".", L (TVariable (id, _), toTheLeft), _
    | InsertText ".", L (TFieldName (id, _, _), toTheLeft), _
      when onEdge && pos = toTheLeft.endPos ->
        let newAST, target =
          exprToFieldAccess id ~partialID:(gid ()) ~fieldID:(gid ()) ast
        in
        (newAST, moveToCaretTarget s newAST target)
    (* Infix symbol insertion to create partials *)
    | InsertText infixTxt, L (TPipe _, ti), _
    | InsertText infixTxt, _, R (TPlaceholder _, ti)
    | InsertText infixTxt, _, R (TBlank _, ti)
    | InsertText infixTxt, L (_, ti), _
      when keyIsInfix ->
        doInfixInsert ~pos infixTxt ti ast s
    (* Typing between empty list symbols [] *)
    | InsertText txt, L (TListOpen id, _), R (TListClose _, _) ->
        let newExpr, target = insertInBlankExpr txt in
        let newAST = insertInList ~index:0 id ~newExpr ast in
        (newAST, moveToCaretTarget s newAST target)
    (* Typing between empty record symbols {} *)
    | InsertText txt, L (TRecordOpen id, _), R (TRecordClose _, _) ->
        (* Adds new initial record row with the typed
         * value as the fieldname (if value entered is valid),
         * then move caret to end of fieldname *)
        if Util.isIdentifierChar txt
        then
          let ast = addRecordRowAt ~letter:txt 0 id ast in
          let s = moveToAstRef s ast (ARRecord (id, RPFieldname 0)) ~offset:1 in
          (ast, s)
        else (ast, s)
    (***********************************)
    (* INSERT INTO EXISTING CONSTRUCTS *)
    (***********************************)
    | InsertText ins, L (TPlaceholder {placeholder; blankID; fnID}, _), _
    | InsertText ins, _, R (TPlaceholder {placeholder; blankID; fnID}, _) ->
        (* We need this special case because by the time we get to the general
         * doInsert handling, reconstructing the difference between placeholders
         * and blanks is too challenging. ASTRefs cannot distinguish blanks and placeholders. *)
        let newExpr, newTarget =
          insertInPlaceholderExpr ~fnID ~placeholder ~ins ast s.ac.functions
        in
        let newAST = FluidAST.replace blankID ~replacement:newExpr ast in
        (newAST, moveToCaretTarget s newAST newTarget)
    | Keypress {key = K.Space; _}, _, R (_, toTheRight) ->
        doInsert ~pos " " toTheRight ast s
    | InsertText txt, L (_, toTheLeft), _ when T.isAppendable toTheLeft.token ->
        doInsert ~pos txt toTheLeft ast s
    | InsertText txt, _, R (_, toTheRight) ->
        doInsert ~pos txt toTheRight ast s
    (***********)
    (* K.Enter *)
    (***********)
    (*
     * Caret to right of record open {
     * Add new initial record row and move caret to it. *)
    | Keypress {key = K.Enter; _}, L (TRecordOpen id, _), _ ->
        let ast = addRecordRowAt 0 id ast in
        let s = moveToAstRef s ast (ARRecord (id, RPFieldname 0)) in
        (ast, s)
    (*
     * Caret to left of record close }
     * Add new final record but leave caret to left of } *)
    | Keypress {key = K.Enter; _}, _, R (TRecordClose id, _) ->
        let s = recordAction "addRecordRowToBack" s in
        let ast = addRecordRowToBack id ast in
        let s = moveToAstRef s ast (ARRecord (id, RPClose)) in
        (ast, s)
    (*
     * Caret between pipe symbol |> and following expression.
     * Move current pipe expr down by adding new expr above it.
     * Keep caret "the same", only moved down by 1 column. *)
    | Keypress {key = K.Enter; _}, L (TPipe (id, idx, _), _), R _ ->
        let ast, s, _ = addPipeExprAt id (idx + 1) ast s in
        let s = moveToAstRef s ast (ARPipe (id, idx + 1)) ~offset:2 in
        (ast, s)
    (*
     * Caret on end-of-line.
     * Following newline contains a parent and index, meaning we're inside some
     * special construct. Special-case each of those. *)
    | ( Keypress {key = K.Enter; _}
      , _
      , R (TNewline (Some (_, parentId, Some idx)), ti) ) ->
      ( match FluidAST.find parentId ast with
      | Some (EPipe _) ->
          let ast, s, blankId = addPipeExprAt parentId (idx + 1) ast s in
          let s =
            moveToCaretTarget s ast (caretTargetForStartOfExpr blankId ast)
          in
          (ast, s)
      | Some (ERecord _) ->
          let ast = addRecordRowAt idx parentId ast in
          let s = moveToAstRef s ast (ARRecord (parentId, RPFieldname idx)) in
          (ast, s)
      | Some (EMatch _) ->
          let ast, s = addMatchPatternAt parentId idx ast s in
          let target = caretTargetForBeginningOfMatchBranch parentId idx ast in
          let s = moveToCaretTarget s ast target in
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
    | Keypress {key = K.Enter; _}, L (lt, lti), R (TNewline None, rti)
      when not (T.isLet lt || isAutocompleting rti s || isInIfCondition lt) ->
        wrapInLet lti ast s
    (*
     * Caret at end-of-line with no data in the TNewline.
     * Just move right (ie, * to beginning of next line) *)
    | Keypress {key = K.Enter; _}, L _, R (TNewline None, ti) ->
        (ast, doRight ~pos ~next:mNext ti s)
    (*
     * Caret at end-of-line generally adds a let on the line below,
     * unless the next line starts with a blank, in which case we go to it. *)
    | Keypress {key = K.Enter; _}, L _, R (TNewline (Some (id, _, _)), ti) ->
        if mNext
           |> Option.map ~f:(fun n ->
                  match n.token with TBlank _ -> true | _ -> false)
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
    | ( Keypress {key = K.Enter; _}
      , L (TNewline (Some (_, parentId, Some idx)), _)
      , R (rTok, _) ) ->
        let applyToRightToken () : FluidAST.t * state =
          let parentID = T.toParentID rTok in
          let index = T.toIndex rTok in
          match
            ( parentID
            , index
            , Option.andThen parentID ~f:(fun id -> FluidAST.find id ast) )
          with
          | Some parentId, Some idx, Some (EMatch _) ->
              let ast, s = addMatchPatternAt parentId idx ast s in
              let target =
                caretTargetForBeginningOfMatchBranch parentId (idx + 1) ast
              in
              let s = moveToCaretTarget s ast target in
              (ast, s)
          | Some parentId, Some idx, Some (ERecord _) ->
              let ast = addRecordRowAt idx parentId ast in
              let target =
                {astRef = ARRecord (parentId, RPFieldname (idx + 1)); offset = 0}
              in
              let s = moveToCaretTarget s ast target in
              (ast, s)
          | _ ->
              let id = FluidToken.tid rTok in
              let ast, s, _ = makeIntoLetBody id ast s in
              let s =
                moveToCaretTarget s ast (caretTargetForStartOfExpr id ast)
              in
              (ast, s)
        in
        ( match FluidAST.find parentId ast with
        | Some (EMatch (_, _, exprs)) ->
            (* if a match has n rows, the last newline has idx=(n+1) *)
            if idx = List.length exprs
            then applyToRightToken ()
            else
              let ast, s = addMatchPatternAt parentId idx ast s in
              let target =
                caretTargetForBeginningOfMatchBranch parentId (idx + 1) ast
              in
              let s = moveToCaretTarget s ast target in
              (ast, s)
        | Some (EPipe (_, exprs)) ->
            (* exprs[0] is the initial value of the pipeline, but the indexing
             * is zero-based starting at exprs[1] (it indexes the _pipes
             * only_), so need idx+1 here to counteract. *)
            if idx + 1 = List.length exprs
            then applyToRightToken ()
            else
              let ast, s, _ = addPipeExprAt parentId (idx + 1) ast s in
              let s = moveToAstRef s ast (ARPipe (parentId, idx + 1)) in
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
    | Keypress {key = K.Enter; _}, No, R (t, _)
    | Keypress {key = K.Enter; _}, L (TNewline _, _), R (t, _) ->
        (* In some cases, like |1 + 2, we want to wrap the parent expr (in this case the binop) in a let.
         * This has to be recursive to handle variations on |1*2 + 3.
         * In other cases, we want to wrap just the subexpression, such as an if's then expression. *)
        let id = FluidToken.tid t in
        let topID =
          FluidAST.find id ast
          |> Option.andThen ~f:(fun directExpr ->
                 findAppropriateParentToWrap directExpr ast)
          |> Option.map ~f:(fun expr -> E.toID expr)
          |> Option.withDefault ~default:id
        in
        let ast, s, _ = makeIntoLetBody topID ast s in
        let s = moveToCaretTarget s ast (caretTargetForStartOfExpr topID ast) in
        (ast, s)
    (*
     * Caret at very end of tokens where last line is non-let expression. *)
    | Keypress {key = K.Enter; _}, L (token, ti), No when not (T.isLet token) ->
        wrapInLet ti ast s
    | _ ->
        (* Unknown *)
        (ast, report ("Unknown action: " ^ show_fluidInputEvent inputEvent) s)
  in
  let newState = {newState with lastInput = inputEvent} in
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
    let tokens = tokensOfFocusedEditor newAST newState in
    let text = Printer.tokensToString tokens in
    let last = List.last tokens in
    match last with
    | Some {token = TNewline _; _} when String.length text = newState.newPos ->
        (newAST, {newState with newPos = newState.newPos - 1})
    | _ ->
        (newAST, newState)
  in
  (* If we were on a partial and have moved off it, we may want to commit that
   * partial. For example, if we fully typed out "String::append", then click
   * away, we want that to become `String::append ___ ___`.
   *
   * We "commit the partial" using the old state, and then we do the action
   * again to make sure we go to the right place for the new canvas.
   *
   * This is done here because the logic is different that clicking. *)
  if recursing
  then (newAST, newState)
  else
    let key =
      match inputEvent with Keypress {key; _} -> Some key | _ -> None
    in
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
      when key = Some K.Right || key = Some K.Left || keyIsInfix ->
        let shouldCommit =
          match inputEvent with
          | Keypress {key = K.Right; _} | Keypress {key = K.Left; _} ->
              true
          | InsertText txt ->
              (* if the partial is a valid function name, don't commit *)
              let newQueryString = str ^ txt in
              s.ac.allCompletions
              |> List.filter ~f:(fun aci ->
                     String.contains ~substring:newQueryString (AC.asName aci))
              |> ( == ) []
          | _ ->
              (* unreachable due to when condition on enclosing match branch *)
              true
        in
        if shouldCommit
        then
          (* Use the new position as we may want to commit if we've moved, but
           * use the old AST so as not to double-commit *)
          let committedAST = commitIfValid newState.newPos ti ast s in
          updateKey
            ~recursing:true
            inputEvent
            committedAST
            (* keep the actions for debugging *)
            {s with actions = newState.actions}
        else (newAST, newState)
    | L (TPartial (_, _), ti), _ when false (* disable for now *) ->
        maybeCommitStringPartial pos ti newAST newState
    | _ ->
        (newAST, newState)


(* deleteCaretRange is equivalent to pressing backspace starting from the
 * larger of the two caret positions until the caret reaches the smaller of the
 * caret positions or can no longer move.
 *
 * XXX(JULIAN): This actually moves the caret to the larger side of the range
 * and backspaces until the beginning, which means this hijacks the caret in
  * the state. *)
and deleteCaretRange ~state ~(ast : FluidAST.t) (caretRange : int * int) :
    FluidAST.t * fluidState =
  let rangeStart, rangeEnd = orderRangeFromSmallToBig caretRange in
  let state =
    {state with newPos = rangeEnd; oldPos = state.newPos; selectionStart = None}
  in
  let currAst, currState = (ref ast, ref state) in
  let nothingChanged = ref false in
  while (not !nothingChanged) && !currState.newPos > rangeStart do
    let newAst, newState =
      updateKey DeleteContentBackward !currAst !currState
    in
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
and deleteSelection ~state ~(ast : FluidAST.t) : FluidAST.t * fluidState =
  getSelectionRange state |> deleteCaretRange ~state ~ast


and replaceText ~(ast : FluidAST.t) ~state (str : string) :
    FluidAST.t * fluidState =
  let newAST, newState =
    getSelectionRange state |> deleteCaretRange ~state ~ast
  in
  updateKey (InsertText str) newAST newState


let updateAutocomplete
    (m : model) (tlid : TLID.t) (ast : FluidAST.t) (s : fluidState) : fluidState
    =
  match getToken ast s with
  | Some ti when T.isAutocompletable ti.token ->
      let m = TL.withAST m tlid ast in
      let newAC = AC.regenerate m s.ac (tlid, ti) in
      {s with ac = newAC}
  | _ ->
      s


let updateMouseClick (newPos : int) (ast : FluidAST.t) (s : fluidState) :
    FluidAST.t * fluidState =
  let tokens = tokensOfFocusedEditor ast s in
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
    | Some current when T.isBlank current.token ->
        current.startPos
    | Some ({token = TPipe _; _} as current) ->
        current.endPos
    | _ ->
        newPos
  in
  let ast = acMaybeCommit newPos ast s in
  let s = updatePosAndAC ast newPos s in
  (ast, s)


let shouldDoDefaultAction (key : K.key) : bool =
  match key with
  | K.GoToStartOfLine _
  | K.GoToEndOfLine _
  | K.SelectAll
  | K.GoToStartOfWord _
  | K.GoToEndOfWord _ ->
      false
  | _ ->
      true


let shouldSelect (key : K.key) : bool =
  match key with
  | K.GoToStartOfWord K.KeepSelection
  | K.GoToEndOfWord K.KeepSelection
  | K.GoToStartOfLine K.KeepSelection
  | K.GoToEndOfLine K.KeepSelection
  | K.SelectAll ->
      true
  | _ ->
      false


(** [expressionRange e target] returns the beginning and end of the range
  * from the expression's first and last token by cross-referencing the
  * tokens for the expression with the tokens for the whole editor's expr.
  *
  * This is preferred to just getting all the tokens with the same exprID
  * because the last expression in a token range
  * (e.g. a FnCall `Int::add 1 2`) might be for a sub-expression and have a
  * different ID, (in the above case the last token TInt(2) belongs to the
  * second sub-expr of the FnCall) *)
let expressionRange (ast : FluidAST.t) (s : fluidState) (exprID : ID.t) :
    (int * int) option =
  let containingTokens = tokensOfFocusedEditor ast s in
  let exprTokens =
    FluidAST.find exprID ast
    |> Option.map ~f:(tokenizeForFocusedEditor s)
    |> Option.withDefault ~default:[]
  in
  let exprStartToken, exprEndToken =
    (List.head exprTokens, List.last exprTokens)
    |> Tuple2.mapAll ~f:(function
           | Some exprTok ->
               List.find containingTokens ~f:(fun tk ->
                   exprTok.token = tk.token)
           | _ ->
               None)
  in
  match (exprStartToken, exprEndToken) with
  (* range is from startPos of first token in expr to
    * endPos of last token in expr *)
  | Some {startPos; _}, Some {endPos; _} ->
      Some (startPos, endPos)
  | _ ->
      None


let getTokenRangeAtCaret (ast : FluidAST.t) (s : fluidState) :
    (int * int) option =
  getToken ast s |> Option.map ~f:(fun t -> (t.startPos, t.endPos))


let getExpressionRangeAtCaret (ast : FluidAST.t) (s : fluidState) :
    (int * int) option =
  getToken ast s
  (* get token that the cursor is currently on *)
  |> Option.andThen ~f:(fun t ->
         (* get expression that the token belongs to *)
         let exprID = T.tid t.token in
         expressionRange ast s exprID)
  |> Option.map ~f:(fun (eStartPos, eEndPos) -> (eStartPos, eEndPos))


let reconstructExprFromRange
    (ast : FluidAST.t) (s : fluidState) (range : int * int) :
    FluidExpression.t option =
  (* prevent duplicates *)
  let open FluidExpression in
  let ast = FluidAST.clone ast in
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
        tID = tID' && typeName = typeName')
    |> Option.map ~f:Tuple3.second
  in
  let startPos, endPos = orderRangeFromSmallToBig range in
  (* main main recursive algorithm *)
  (* algo:
    * find topmost expression by ID and
    * reconstruct full/subset of expression
    * recurse into children (that remain in subset) to reconstruct those too *)
  let rec reconstruct ~topmostID (startPos, endPos) : E.t option =
    let topmostExpr =
      topmostID
      |> Option.andThen ~f:(fun id -> FluidAST.find id ast)
      |> Option.withDefault ~default:(EBlank (gid ()))
    in
    let tokens =
      (* simplify tokens to make them homogenous, easier to parse *)
      tokensInRange ast s startPos endPos
      |> List.map ~f:(fun ti ->
             let t = ti.token in
             let text =
               (* trim tokens if they're on the edge of the range *)
               T.toText t
               |> String.dropLeft
                    ~count:
                      ( if ti.startPos < startPos
                      then startPos - ti.startPos
                      else 0 )
               |> String.dropRight
                    ~count:(if ti.endPos > endPos then ti.endPos - endPos else 0)
               |> fun text ->
               (* if string, do extra trim to account for quotes, then re-append quotes *)
               if T.toTypeName ti.token = "string"
               then "\"" ^ Util.trimQuotes text ^ "\""
               else text
             in
             let open T in
             (tid t, text, toTypeName t))
    in
    let reconstructExpr expr : E.t option =
      match expr with
      | EPipeTarget _ ->
          Some expr
      | _ ->
          let exprID = E.toID expr in
          exprID
          |> expressionRange ast s
          |> Option.andThen ~f:(fun (exprStartPos, exprEndPos) ->
                 (* ensure expression range is not totally outside selection range *)
                 if exprStartPos > endPos || exprEndPos < startPos
                 then None
                 else Some (max exprStartPos startPos, min exprEndPos endPos))
          |> Option.andThen ~f:(reconstruct ~topmostID:(Some exprID))
    in
    let orDefaultExpr : E.t option -> E.t =
      Option.withDefault ~default:(EBlank (gid ()))
    in
    let id = gid () in
    match topmostExpr with
    | _ when tokens = [] ->
        None
    (* basic, single/fixed-token expressions *)
    | EInteger (eID, _) ->
        findTokenValue tokens eID "integer"
        |> Option.map ~f:Util.coerceStringTo63BitInt
        |> Option.map ~f:(fun v -> EInteger (gid (), v))
    | EBool (eID, value) ->
        Option.or_
          (findTokenValue tokens eID "true")
          (findTokenValue tokens eID "false")
        |> Option.andThen ~f:(fun newValue ->
               if newValue = ""
               then None
               else if newValue <> string_of_bool value
               then Some (EPartial (gid (), newValue, EBool (id, value)))
               else Some (EBool (id, value)))
    | ENull eID ->
        findTokenValue tokens eID "null"
        |> Option.map ~f:(fun newValue ->
               if newValue = "null"
               then ENull id
               else EPartial (gid (), newValue, ENull id))
    | EString (eID, _) ->
        let merged =
          tokens
          |> List.filter ~f:(fun (_, _, type_) ->
                 type_ <> "newline" && type_ <> "indent")
          |> List.map ~f:Tuple3.second
          |> String.join ~sep:""
        in
        if merged = ""
        then None
        else Some (EString (eID, Util.trimQuotes merged))
    | EFloat (eID, _, _) ->
        let newWhole = findTokenValue tokens eID "float-whole" in
        let pointSelected = findTokenValue tokens eID "float-point" <> None in
        let newFraction = findTokenValue tokens eID "float-fractional" in
        ( match (newWhole, pointSelected, newFraction) with
        | Some value, true, None ->
            Some (EFloat (id, value, "0"))
        | Some value, false, None | None, false, Some value ->
            Some (EInteger (id, Util.coerceStringTo63BitInt value))
        | None, true, Some value ->
            Some (EFloat (id, "0", value))
        | Some whole, true, Some fraction ->
            Some (EFloat (id, whole, fraction))
        | None, true, None ->
            Some (EFloat (id, "0", "0"))
        | _, _, _ ->
            None )
    | EBlank _ ->
        Some (EBlank id)
    (* empty let expr and subsets *)
    | ELet (eID, _lhs, rhs, body) ->
        let letKeywordSelected =
          findTokenValue tokens eID "let-keyword" <> None
        in
        let newLhs =
          findTokenValue tokens eID "let-var-name"
          |> Option.withDefault ~default:""
        in
        ( match (reconstructExpr rhs, reconstructExpr body) with
        | None, None when newLhs <> "" ->
            Some (EPartial (gid (), newLhs, EVariable (gid (), newLhs)))
        | None, Some e ->
            Some e
        | Some newRhs, None ->
            Some (ELet (id, newLhs, newRhs, EBlank (gid ())))
        | Some newRhs, Some newBody ->
            Some (ELet (id, newLhs, newRhs, newBody))
        | None, None when letKeywordSelected ->
            Some (ELet (id, newLhs, EBlank (gid ()), EBlank (gid ())))
        | _, _ ->
            None )
    | EIf (eID, cond, thenBody, elseBody) ->
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
    | EBinOp (eID, name, expr1, expr2, ster) ->
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
              Some (ELet (gid (), "", e, EBlank (gid ())))
          | e1, e2 ->
              Some
                (ELet (gid (), "", e1, ELet (gid (), "", e2, EBlank (gid ()))))
          )
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
            let e = EBinOp (id, name, EBlank (gid ()), EBlank (gid ()), ster) in
            if newName = ""
            then None
            else if name <> newName
            then Some (EPartial (gid (), newName, e))
            else Some e
        | _, _ ->
            None )
    | ELambda (eID, _, body) ->
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
                     None)
        in
        Some (ELambda (id, newVars, reconstructExpr body |> orDefaultExpr))
    | EFieldAccess (eID, e, _) ->
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
            Some (EFieldAccess (id, EBlank (gid ()), newFieldName))
        | Some e, true, _ ->
            Some (EFieldAccess (id, e, newFieldName))
        | _ ->
            e )
    | EVariable (eID, value) ->
        let newValue =
          findTokenValue tokens eID "variable" |> Option.withDefault ~default:""
        in
        let e = EVariable (id, value) in
        if newValue = ""
        then None
        else if value <> newValue
        then Some (EPartial (gid (), newValue, e))
        else Some e
    | EFnCall (eID, fnName, args, ster) ->
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
    | EPartial (eID, _, expr) ->
        (* What should we do with the expr? Some of the name is covered by
         * the partial name which breaks the reconstruction algorithm. In
         * addtion, copying a partial without the old expr breaks the whole
         * concept of a partial. So it makes more sense to copy the whole
         * thing. *)
        let newName =
          findTokenValue tokens eID "partial" |> Option.withDefault ~default:""
        in
        Some (EPartial (id, newName, expr))
    | ERightPartial (eID, _, expr) ->
        let expr = reconstructExpr expr |> orDefaultExpr in
        let newName =
          findTokenValue tokens eID "partial-right"
          |> Option.withDefault ~default:""
        in
        Some (ERightPartial (id, newName, expr))
    | EList (_, exprs) ->
        let newExprs = List.map exprs ~f:reconstructExpr |> Option.values in
        Some (EList (id, newExprs))
    | ERecord (id, entries) ->
        let newEntries =
          (* looping through original set of tokens (before transforming them into tuples)
           * so we can get the index field *)
          tokensInRange ast s startPos endPos
          |> List.filterMap ~f:(fun ti ->
                 match ti.token with
                 | TRecordFieldname
                     {recordID; index; fieldName = newKey; exprID = _}
                   when recordID = id (* watch out for nested records *) ->
                     List.getAt ~index entries
                     |> Option.map
                          ~f:
                            (Tuple2.mapEach
                               ~f:(fun _ -> newKey) (* replace key *)
                               ~g:
                                 (reconstructExpr >> orDefaultExpr)
                                 (* reconstruct value expression *))
                 | _ ->
                     None)
        in
        Some (ERecord (id, newEntries))
    | EPipe (_, exprs) ->
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
    | EConstructor (eID, name, exprs) ->
        let newName =
          findTokenValue tokens eID "constructor-name"
          |> Option.withDefault ~default:""
        in
        let newExprs = List.map exprs ~f:(reconstructExpr >> orDefaultExpr) in
        let e = EConstructor (id, name, newExprs) in
        if newName = ""
        then None
        else if name <> newName
        then Some (EPartial (gid (), newName, e))
        else Some e
    | EMatch (mID, cond, patternsAndExprs) ->
        let open FluidPattern in
        let newPatternAndExprs =
          List.map patternsAndExprs ~f:(fun (pattern, expr) ->
              let toksToPattern tokens pID =
                match
                  tokens |> List.filter ~f:(fun (pID', _, _) -> pID = pID')
                with
                | [(id, _, "pattern-blank")] ->
                    FPBlank (mID, id)
                | [(id, value, "pattern-integer")] ->
                    FPInteger (mID, id, Util.coerceStringTo63BitInt value)
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
                | [(id, value, "pattern-string")] ->
                    FPString
                      { matchID = mID
                      ; patternID = id
                      ; str = Util.trimQuotes value }
                | [(id, value, "pattern-true")] | [(id, value, "pattern-false")]
                  ->
                    FPBool (mID, id, toBool_ value)
                | [(id, _, "pattern-null")] ->
                    FPNull (mID, id)
                | [ (id, whole, "pattern-float-whole")
                  ; (_, _, "pattern-float-point")
                  ; (_, fraction, "pattern-float-fractional") ] ->
                    FPFloat (mID, id, whole, fraction)
                | [ (id, value, "pattern-float-whole")
                  ; (_, _, "pattern-float-point") ]
                | [(id, value, "pattern-float-whole")] ->
                    FPInteger (mID, id, Util.coerceStringTo63BitInt value)
                | [ (_, _, "pattern-float-point")
                  ; (id, value, "pattern-float-fractional") ]
                | [(id, value, "pattern-float-fractional")] ->
                    FPInteger (mID, id, Util.coerceStringTo63BitInt value)
                | _ ->
                    FPBlank (mID, gid ())
              in
              let newPattern = toksToPattern tokens (P.toID pattern) in
              (newPattern, reconstructExpr expr |> orDefaultExpr))
        in
        Some
          (EMatch (id, reconstructExpr cond |> orDefaultExpr, newPatternAndExprs))
    | EFeatureFlag (_, name, cond, disabled, enabled) ->
        (* since we don't have any tokens associated with feature flags yet *)
        Some
          (EFeatureFlag
             ( id
             , (* should probably do some stuff about if the name token isn't fully selected *)
               name
             , reconstructExpr cond |> orDefaultExpr
             , reconstructExpr enabled |> orDefaultExpr
             , reconstructExpr disabled |> orDefaultExpr ))
    | EPipeTarget _ ->
        Some (EPipeTarget (gid ()))
  in
  let topmostID = getTopmostSelectionID ast s startPos endPos in
  reconstruct ~topmostID (startPos, endPos)


let pasteOverSelection ~state ~(ast : FluidAST.t) data : FluidAST.t * state =
  let ast, state = deleteSelection ~state ~ast in
  let mTi = getToken ast state in
  let exprID = mTi |> Option.map ~f:(fun ti -> ti.token |> T.tid) in
  let expr = Option.andThen exprID ~f:(fun id -> FluidAST.find id ast) in
  let clipboardExpr = Clipboard.clipboardContentsToExpr data in
  let text = Clipboard.clipboardContentsToString data in
  match expr with
  | Some expr ->
    ( match (expr, clipboardExpr, mTi) with
    | EBlank id, Some cp, _ ->
        (* Paste into a blank *)
        let newAST = FluidAST.replace ~replacement:cp id ast in
        let caretTarget = caretTargetForEndOfExpr (E.toID cp) newAST in
        (newAST, moveToCaretTarget state newAST caretTarget)
    | EString (id, str), _, Some ti ->
        (* Paste into a string, to take care of newlines *)
        let index = getStringIndex ti state.newPos in
        let replacement =
          E.EString (id, String.insertAt ~insert:text ~index str)
        in
        let newAST = FluidAST.replace ~replacement id ast in
        let caretTarget = CT.forARStringText id (index + String.length text) in
        (newAST, moveToCaretTarget state newAST caretTarget)
    | _ ->
        text
        |> String.split ~on:""
        |> List.foldl ~init:(ast, state) ~f:(fun str (newAST, s) ->
               let space : FluidKeyboard.keyEvent =
                 { key = K.Space
                 ; shiftKey = false
                 ; altKey = false
                 ; metaKey = false
                 ; ctrlKey = false }
               in
               let enter = {space with key = K.Enter} in
               let action =
                 if str = " "
                 then Keypress space
                 else if str = "\n"
                 then Keypress enter
                 else InsertText str
               in
               updateKey action newAST s) )
  | _ ->
      recover "pasting over non-existant handler" (ast, state)


let fluidDataFromModel m : (fluidState * FluidAST.t) option =
  match Toplevel.selectedAST m with
  | Some expr ->
      let s = m.fluidState in
      Some (s, expr)
  | None ->
      None


let getCopySelection (m : model) : clipboardContents =
  fluidDataFromModel m
  |> Option.andThen ~f:(fun (state, ast) ->
         let from, to_ = getSelectionRange state in
         let text =
           exprOfFocusedEditor ast state
           |> FluidPrinter.eToHumanString
           |> String.slice ~from ~to_
         in
         let json =
           reconstructExprFromRange ast m.fluidState (from, to_)
           |> Option.map ~f:Clipboard.exprToClipboardContents
         in
         Some (text, json))
  |> Option.withDefault ~default:("", None)


let updateMouseUp (m : model) (ast : FluidAST.t) (eventData : fluidMouseUp) :
    FluidAST.t * fluidState =
  let s =
    {m.fluidState with midClick = false; activeEditorId = eventData.editorId}
  in
  let selection =
    eventData.selection |> Option.orElseLazy Entry.getFluidSelectionRange
  in
  let ast, s =
    match selection with
    (* if range width is 0, just change pos *)
    | Some (selBegin, selEnd) when selBegin = selEnd ->
        updateMouseClick selBegin ast s
    | Some (selBegin, selEnd) ->
        ( ast
        , { s with
            selectionStart = Some selBegin
          ; oldPos = s.newPos
          ; newPos = selEnd }
          |> acClear )
    | None ->
        (* We reset the fluidState to prevent the selection and/or cursor
         * position from persisting when a user switched handlers *)
        (ast, {s with selectionStart = None} |> acClear)
  in
  (ast, s)


let updateMsg m tlid (ast : FluidAST.t) (msg : Types.fluidMsg) (s : fluidState)
    : FluidAST.t * fluidState =
  (* TODO: The state should be updated from the last request, and so this
   * shouldn't be necessary, but the tests don't work without it *)
  let s = updateAutocomplete m tlid ast s in
  let newAST, newState =
    match msg with
    | FluidCloseCmdPalette | FluidUpdateAutocomplete ->
        (* updateAutocomplete has already been run, so nothing more to do *)
        (ast, s)
    | FluidMouseUp eventData ->
        updateMouseUp m ast eventData
    | FluidCut ->
        deleteSelection ~state:s ~ast
    | FluidPaste data ->
        let ast, s = pasteOverSelection ~state:s ~ast data in
        let s = updateAutocomplete m tlid ast s in
        (ast, s)
    (* handle selection with direction key cases *)
    (* moving/selecting over expressions or tokens with shift-/alt-direction
     * or shift-/ctrl-direction *)
    | FluidInputEvent
        ( Keypress {key; shiftKey = true; altKey = _; ctrlKey = _; metaKey = _}
        as ievt )
      when key = K.Right || key = K.Left || key = K.Up || key = K.Down ->
        (* Ultimately, all we want is for shift to move the end of the
         * selection to where the caret would have been if shift were not held.
         * Since the caret is tracked the same for end of selection and
         * movement, we actually just want to store the start position in
         * selection if there is no selection yet.
         *
         * TODO(JULIAN): We need to refactor updateKey and key handling in
         * general so that modifiers compose more easily with shift
         *
         * XXX(JULIAN): We need to be able to use alt and ctrl and meta to
         * change selection! *)
        let ast, newS = updateKey ievt ast s in
        ( match s.selectionStart with
        | None ->
            ( ast
            , {newS with newPos = newS.newPos; selectionStart = Some s.newPos}
            )
        | Some pos ->
            (ast, {newS with newPos = newS.newPos; selectionStart = Some pos})
        )
    | FluidInputEvent
        ( Keypress {key; shiftKey = false; altKey = _; ctrlKey = _; metaKey = _}
        as ievt )
      when s.selectionStart <> None && (key = K.Right || key = K.Left) ->
        (* Aborting a selection using the left and right arrows should
         place the caret on the side of the selection in the direction
         of the pressed arrow key *)
        let newPos =
          let left, right = getSelectionRange s |> orderRangeFromSmallToBig in
          if key = K.Left then left else right
        in
        (ast, {s with lastInput = ievt; newPos; selectionStart = None})
    | FluidInputEvent (Keypress {key; altKey; metaKey; ctrlKey; shiftKey = _})
      when (altKey || metaKey || ctrlKey) && shouldDoDefaultAction key ->
        (* To make sure no letters are entered if user is doing a browser default action *)
        (ast, s)
    | FluidInputEvent (Keypress {key; shiftKey; _} as ievt) ->
        let s = {s with lastInput = ievt} in
        let newAST, newState = updateKey ievt ast s in
        let selectionStart =
          if shouldSelect key
          then newState.selectionStart
          else if shiftKey && not (key = K.ShiftEnter)
                  (* We dont want to persist selection on ShiftEnter *)
          then s.selectionStart
          else None
        in
        (newAST, {newState with selectionStart})
    | FluidInputEvent (InsertText str as ievt)
      when Option.is_some s.selectionStart ->
        let s = {s with lastInput = ievt} in
        updateKey (ReplaceText str) ast s
    | FluidInputEvent ievt ->
        updateKey ievt ast s
    | FluidAutocompleteClick entry ->
        Option.map (getToken ast s) ~f:(fun ti -> acClick entry ti ast s)
        |> Option.withDefault ~default:(ast, s)
    | FluidClearErrorDvSrc
    | FluidMouseDown _
    | FluidCommandsFilter _
    | FluidCommandsClick _
    | FluidFocusOnToken _
    | FluidUpdateDropdownIndex _ ->
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
      FluidCommands.cpSetIndex m index
  | FluidUpdateDropdownIndex index ->
      ReplaceAllModificationsWithThisOne
        (fun m ->
          let fluidState = acSetIndex index m.fluidState in
          ({m with fluidState}, Tea.Cmd.none))
  | FluidInputEvent (Keypress {key = K.Undo; _}) ->
      KeyPress.undo_redo m false
  | FluidInputEvent (Keypress {key = K.Redo; _}) ->
      KeyPress.undo_redo m true
  | FluidInputEvent (Keypress {key = K.CommandPalette; _}) ->
      maybeOpenCmd m
  | FluidInputEvent (Keypress {key = K.Omnibox; _}) ->
      KeyPress.openOmnibox m
  | FluidInputEvent (Keypress ke) when FluidCommands.isOpened m.fluidState.cp ->
      FluidCommands.updateCmds m ke
  | FluidClearErrorDvSrc ->
      FluidSetState {m.fluidState with errorDvSrc = SourceNone}
  | FluidFocusOnToken id ->
      (* Spec for Show token of expression: https://docs.google.com/document/d/13-jcP5xKe_Du-TMF7m4aPuDNKYjExAUZZ_Dk3MDSUtg/edit#heading=h.h1l570vp6wch *)
      CursorState.tlidOf m.cursorState
      |> Option.andThen ~f:(fun tlid -> TL.get m tlid)
      |> Option.andThen ~f:(fun tl ->
             match TL.getAST tl with
             | Some expr ->
                 Some (tl, expr)
             | None ->
                 None)
      |> Option.map ~f:(fun (tl, ast) ->
             let fluidState =
               let fs = moveToEndOfTarget ast s id in
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
             else Many [moveMod; FluidSetState fluidState])
      |> Option.withDefault ~default:NoChange
  | FluidCloseCmdPalette ->
      FluidCommandsClose
  | FluidMouseDown _
  | FluidInputEvent _
  | FluidPaste _
  | FluidCut
  | FluidCommandsFilter _
  | FluidCommandsClick _
  | FluidAutocompleteClick _
  | FluidUpdateAutocomplete
  | FluidMouseUp _ ->
      let tlid =
        match msg with
        | FluidMouseUp {tlid; _} ->
            Some tlid
        | _ ->
            CursorState.tlidOf m.cursorState
      in
      let tl : toplevel option = Option.andThen tlid ~f:(Toplevel.get m) in
      let ast = Option.andThen tl ~f:TL.getAST in
      ( match (tl, ast) with
      | Some tl, Some ast ->
          let tlid = TL.id tl in
          let newAST, newState = updateMsg m tlid ast msg s in
          let eventSpecMod, newAST, newState =
            let isFluidEntering =
              (* Only fire Tab controls if the state is currently in
               * entering, as some keypresses fire in both editors. *)
              match m.cursorState with FluidEntering _ -> true | _ -> false
            in
            let enter id = Enter (Filling (tlid, id)) in
            (* if tab is wrapping... *)
            let lastKey =
              match newState.lastInput with
              | Keypress {key; _} ->
                  Some key
              | _ ->
                  None
            in
            if isFluidEntering
               && lastKey = Some K.Tab
               && newState.newPos <= newState.oldPos
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
            else if isFluidEntering
                    && lastKey = Some K.ShiftTab
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
                Tea.Cmd.none
          in
          let astMod =
            if ast <> newAST
            then
              let requestAnalysis =
                match Analysis.getSelectedTraceID m tlid with
                | Some traceID ->
                    let m = TL.withAST m tlid newAST in
                    MakeCmd (Analysis.requestAnalysis m tlid traceID)
                | None ->
                    NoChange
              in
              Many
                [ ReplaceAllModificationsWithThisOne
                    (fun m -> (TL.withAST m tlid newAST, Tea.Cmd.none))
                ; Toplevel.setSelectedAST m newAST
                ; requestAnalysis
                ; UpdateASTCache
                    (tlid, Printer.eToHumanString (FluidAST.toExpr newAST)) ]
            else Types.NoChange
          in
          Types.Many
            [ ReplaceAllModificationsWithThisOne
                (fun m -> ({m with fluidState = newState}, Tea.Cmd.none))
            ; astMod
            ; eventSpecMod
            ; Types.MakeCmd cmd ]
      | _ ->
          NoChange )


(* -------------------- *)
(* Scaffolidng *)
(* -------------------- *)

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
            Entry.setFluidSelectionRange selStart m.fluidState.newPos
        | None ->
            Entry.setFluidCaret m.fluidState.newPos )
  | _ ->
      ()


let cleanUp (m : model) (tlid : TLID.t option) : model * modification =
  let state = m.fluidState in
  let rmPartialsMod =
    tlid
    |> Option.andThen ~f:(TL.get m)
    |> Option.thenAlso ~f:TL.getAST
    |> Option.andThen ~f:(fun (tl, ast) ->
           let newAST =
             acMaybeCommit 0 ast state |> FluidAST.map ~f:AST.removePartials
           in
           if newAST <> ast then Some (TL.setASTMod tl newAST) else None)
    |> Option.withDefault ~default:NoChange
  in
  let acVisibilityModel =
    if AC.isOpened state.ac
    then AC.updateAutocompleteVisibility m
    else if Commands.isOpened state.cp
    then Commands.updateCommandPaletteVisibility m
    else m
  in
  (acVisibilityModel, rmPartialsMod)
