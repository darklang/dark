open Prelude
module T = FluidToken
module E = FluidExpression
module P = FluidPattern
module AC = FluidAutocomplete
module Printer = FluidPrinter
module TL = Toplevel

type props = Types.fluidProps

let propsFromModel (m : model) : props =
  {functions = m.functions; variants = m.tests}


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


type t =
  { ast : FluidAST.t
  ; state : fluidState
  ; mainTokenInfos : tokenInfos
  ; featureFlagTokenInfos : (ID.t * tokenInfos) list
  ; props : props }

let setAST (ast : FluidAST.t) (astInfo : t) : t =
  if astInfo.ast = ast
  then astInfo
  else
    let mainTokenInfos = Printer.tokenize (FluidAST.toExpr ast) in
    let featureFlagTokenInfos =
      ast
      |> FluidAST.getFeatureFlags
      |> List.map ~f:(fun expr ->
             ( E.toID expr
             , Printer.tokenizeWithFFTokenization
                 FeatureFlagConditionAndEnabled
                 expr ))
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


let emptyFor (props : props) (state : fluidState) : t =
  { ast = FluidAST.ofExpr (E.EBlank (gid ()))
  ; state
  ; mainTokenInfos = []
  ; featureFlagTokenInfos = []
  ; props }


let make (props : props) (ast : FluidAST.t) (s : fluidState) : t =
  emptyFor props s |> setAST ast


let fromModelAndTLID ?(removeTransientState = true) (m : model) (tlid : TLID.t)
    : t option =
  (* TODO(JULIAN): codify removeHandlerTransientState as an external function,
   * make `fromExpr` accept only the info it needs, and differentiate between
   * handler-specific and global fluid state. *)
  let removeHandlerTransientState m =
    if removeTransientState
    then {m with fluidState = {m.fluidState with ac = AC.init}}
    else m
  in
  TL.get m tlid
  |> Option.andThen ~f:TL.getAST
  |> Option.map ~f:(fun ast ->
         let state =
           (* We need to discard transient state if the selected handler has changed *)
           if Some tlid = CursorState.tlidOf m.cursorState
           then m.fluidState
           else
             let newM = removeHandlerTransientState m in
             newM.fluidState
         in
         make (propsFromModel m) ast state)


let fromModel (m : model) : t option =
  CursorState.tlidOf m.cursorState |> Option.andThen ~f:(fromModelAndTLID m)


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
