open Prelude

type props =
  { analysisStore : analysisStore
  ; ast : FluidAST.t
  ; functions : functionsType
  ; executingFunctions : ID.t list
  ; editor : fluidEditor
  ; hoveringRefs : ID.t list
  ; fluidState : fluidState
  ; permission : permission option
  ; tlid : TLID.t
  ; tokens : FluidToken.tokenInfo list }

type executionFlow =
  | CodeExecuted
  | CodeNotExecuted
  | UnknownExecution

let isActiveEditor (p : props) = p.fluidState.activeEditor = p.editor

let propsToFnExecutionProps (p : props) : ViewFnExecution.props =
  { analysisStore = p.analysisStore
  ; ast = p.ast
  ; executingFunctions = p.executingFunctions
  ; permission = p.permission
  ; tlid = p.tlid }


let viewPlayIcon (p : props) (ti : FluidToken.tokenInfo) : Types.msg Html.html =
  match ViewUtils.fnForToken p.functions ti.token with
  | Some ({fnOrigin = UserFunction; _} as fn)
  (* HACK: UserFunctions need to be executable so that the user can get a value
   * into the trace. Otherwise, when they edit the function they won't have any
   * live values. *)
  | Some ({fnPreviewSafety = Unsafe; _} as fn) ->
      (* Looking these up can be slow, so the fnPreviewSafety check
       * above is very important.
       *
       * Note that fnPreviewSafety is calculated dynamically by
       * FluidAutocomplete. *)
      let allExprs = AST.getArguments (FluidToken.tid ti.token) p.ast in
      let argIDs = List.map ~f:FluidExpression.toID allExprs in
      ( match ti.token with
      | TFnVersion (id, _, _, _) ->
          ViewFnExecution.fnExecutionButton
            (propsToFnExecutionProps p)
            fn
            id
            argIDs
      | TFnName (id, _, displayName, fnName, _)
      (* If fn is unversioned or is v0 *)
        when displayName = fnName || displayName ^ "_v0" = fnName ->
          ViewFnExecution.fnExecutionButton
            (propsToFnExecutionProps p)
            fn
            id
            argIDs
      | _ ->
          Vdom.noNode )
  | Some {fnPreviewSafety = Safe; _} | None ->
      Vdom.noNode


let toHtml (p : props) : Types.msg Html.html list =
  let exeFlow ti =
    let id = FluidToken.analysisID ti.token in
    match Analysis.getLiveValueLoadable p.analysisStore id with
    | LoadableSuccess (ExecutedResult _) ->
        CodeExecuted
    | LoadableSuccess (NonExecutedResult _) ->
        CodeNotExecuted
    | _ ->
        UnknownExecution
  in
  let caretToken = FluidTokenizer.getTokenNotWhitespace p.tokens p.fluidState in
  let caretParentBlockID =
    caretToken
    |> Option.andThen ~f:(fun ti ->
           (* We only care to check further if caret is in a faded region. *)
           if exeFlow ti = CodeNotExecuted
           then FluidToken.parentBlockID ti.token
           else None)
  in
  (* Returns true if token is in the same block as the caret or on the same row *)
  let isNearCaret ti =
    match caretParentBlockID with
    | Some pid ->
        FluidToken.parentBlockID ti.token = Some pid
        && exeFlow ti = CodeNotExecuted
    | None ->
        let isNotInBlock =
          (* So we don't unfade tokens in the same row, but belong to another block *)
          FluidToken.parentBlockID ti.token = None
        in
        let caretRow = Option.map ~f:(fun ti -> ti.startRow) caretToken in
        Some ti.startRow = caretRow
        && isNotInBlock
        && exeFlow ti = CodeNotExecuted
  in
  (* Returns true if token is part of the expr the opened command palette will act on *)
  let isInCPExpr ti =
    match p.fluidState.cp.location with
    | Some (_, id) when id = FluidToken.tid ti.token ->
        true
    | _ ->
        false
  in
  (* Gets the source of a DIncomplete given an expr id *)
  let sourceOfExprValue id =
    if FluidToken.validID id
    then
      (* Only highlight incompletes and errors on executed paths *)
      match Analysis.getLiveValueLoadable p.analysisStore id with
      | LoadableSuccess (ExecutedResult (DIncomplete (SourceId (tlid, id)))) ->
          (Some (tlid, id), "dark-incomplete")
      | LoadableSuccess (ExecutedResult (DError (SourceId (tlid, id), _))) ->
          (Some (tlid, id), "dark-error")
      | _ ->
          (None, "")
    else (None, "")
  in
  let currentTokenInfo = FluidTokenizer.getToken' p.tokens p.fluidState in
  let sourceOfCurrentToken onTi =
    currentTokenInfo
    |> Option.andThen ~f:(fun ti ->
           if FluidToken.isBlank ti.token || onTi.startRow = ti.startRow
           then None
           else
             let someId, _ =
               FluidToken.analysisID ti.token |> sourceOfExprValue
             in
             someId)
  in
  let nesting = ref 0 in
  let cmdToken =
    match p.fluidState.cp.location with
    | Some (ltlid, id) when p.tlid = ltlid ->
        (* Reversing list will get us the last token visually rendered with matching expression ID, so we don't have to keep track of max pos *)
        p.tokens
        |> List.reverse
        |> List.getBy ~f:(fun ti -> FluidToken.tid ti.token = id)
    | _ ->
        None
  in
  let dropdown ti =
    match cmdToken with
    | Some onTi when onTi = ti ->
        FluidCommands.viewCommandPalette p.fluidState.cp
    | _ ->
        if Fluid.isAutocompleting ti p.fluidState && isActiveEditor p
        then FluidAutocompleteView.view p.fluidState.ac
        else Vdom.noNode
  in
  let isSelected tokenStart tokenEnd =
    let selStart, selEnd = FluidUtil.getSelectionRange p.fluidState in
    isActiveEditor p && selStart <= tokenStart && tokenEnd <= selEnd
  in
  let idsInAFlag =
    (* If we're in the main editor, find all the FF expressions, then build a
     * set of all the IDs of them and their children. This is used below to
     * apply a CSS class to highlight tokens contained in a FF. *)
    match p.editor with
    | NoEditor ->
        ID.Set.empty
    | FeatureFlagEditor _ ->
        ID.Set.empty
    | MainEditor _ ->
        FluidAST.filter p.ast ~f:(function
            | EFeatureFlag _ ->
                true
            | _ ->
                false)
        |> List.foldl ~init:ID.Set.empty ~f:(fun e acc ->
               match e with
               | FluidExpression.EFeatureFlag (_, _, _, oldCode, _) ->
                   ID.Set.addMany
                     acc
                     ~values:(FluidExpression.decendants oldCode)
               | _ ->
                   acc)
  in
  (* We want to highlight all tokens that are in the old-code of a feature
   * flag. to do this, we highlight any token with an ID in idsInAFlag, which
   * has the ID of every expression within that old code. But we also need to
   * highlight the indents, which don't have an ID.
   *
   * So, we toggle this flag on the first time we see a token within the flag
   * IDs above, then toggle it off as soon as we see a non-whitespace token
   * that's not contained in the set. *)
  let withinFlag = ref false in
  List.map p.tokens ~f:(fun ti ->
      let element nested =
        let tokenId = FluidToken.tid ti.token in
        let idStr = ID.toString tokenId in
        let content = FluidToken.toText ti.token in
        let analysisId = FluidToken.analysisID ti.token in
        (* Toggle withinFlag if we've crossed a flag boundary.
         * See above comment where withinFlag is defined *)
        if (not !withinFlag) && ID.Set.member idsInAFlag ~value:tokenId
        then withinFlag := true
        else if FluidToken.validID tokenId
                && !withinFlag
                && not (ID.Set.member idsInAFlag ~value:tokenId)
        then withinFlag := false ;
        (* Apply CSS classes to token *)
        let tokenClasses = FluidToken.toCssClasses ti.token in
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
          (* We want 0 precedence to only show up at the AST root and not in
           * any wraparounds, so this goes 0123412341234... *)
          let wraparoundPrecedenceClass ~ext n =
            let wraparoundPrecedence =
              if n > 0 then ((n - 1) mod 4) + 1 else n
            in
            ["precedence-" ^ (wraparoundPrecedence |> string_of_int)] @ ext
          in
          ( tokenBackingPrecedence |> wraparoundPrecedenceClass ~ext:[]
          , tokenInnerPrecedence
            |> Option.map ~f:(wraparoundPrecedenceClass ~ext:["fluid-inner"]) )
        in
        let cls =
          "fluid-entry"
          :: ("id-" ^ idStr)
          :: (backingNestingClass @ tokenClasses)
          |> List.map ~f:(fun s -> (s, true))
        in
        let conditionalClasses =
          let sourceId, errorType = sourceOfExprValue analysisId in
          let isError =
            (* Only apply to text tokens (not TSep, TNewlines, etc.) *)
            FluidToken.isErrorDisplayable ti.token
            && (* This expression is the source of its own incompleteness. We
            only draw underlines under sources of incompletes, not all
            propagated occurrences. *)
            sourceId = Some (p.tlid, analysisId)
          in
          let isNotExecuted = exeFlow ti = CodeNotExecuted in
          (* Unfade non-executed code if the caret is in it,
           * so auto-complete and command-palette will render at full opacity. *)
          let isInFocus = isNotExecuted && (isNearCaret ti || isInCPExpr ti) in
          [ ("related-change", List.member ~value:tokenId p.hoveringRefs)
          ; ("cursor-on", currentTokenInfo = Some ti)
          ; ("in-flag", !withinFlag)
          ; ("fluid-error", isError)
          ; ("fluid-not-executed", isNotExecuted)
          ; ("fluid-code-focus", isInFocus)
          ; (errorType, errorType <> "")
          ; (* This expression is the source of an incomplete propogated
             * into another, where the cursor is currently on *)
            ("is-origin", sourceOfCurrentToken ti = Some (p.tlid, analysisId))
          ; ( "jumped-to"
            , match p.fluidState.errorDvSrc with
              | SourceNone ->
                  false
              | SourceId (tlid, id) ->
                  id = tokenId && p.tlid = tlid )
          ; ("selected", isSelected ti.startPos ti.endPos) ]
        in
        let innerNode =
          match innerNestingClass with
          | Some cls ->
              [ Html.span
                  [Html.class' (cls |> String.join ~sep:" ")]
                  [Html.text content] ]
          | None ->
              [Html.text content]
        in
        Html.span
          [Html.classList (cls @ conditionalClasses)]
          (innerNode @ nested)
      in
      if p.permission = Some ReadWrite
      then [element [dropdown ti; viewPlayIcon p ti]]
      else [element []])
  |> List.flatten


let tokensView (p : props) : Types.msg Html.html =
  let tlidStr = TLID.toString p.tlid in
  let textInputListeners =
    (* the command palette is inside div.fluid-editor but has it's own input
     * handling, so don't do normal fluid input stuff if it's open *)
    if FluidCommands.isOpened p.fluidState.cp
    then (Html.noProp, Html.noProp, Html.noProp)
    else
      ( Html.onCB
          "keydown"
          ("keydown" ^ tlidStr)
          (FluidKeyboard.onKeydown (fun x ->
               FluidMsg (FluidInputEvent (Keypress x))))
      , Html.onCB
          "beforeinput"
          ("beforeinput" ^ tlidStr)
          FluidTextInput.fromInputEvent
      , Html.onCB
          "compositionend"
          ("compositionend" ^ tlidStr)
          FluidTextInput.fromCompositionEndEvent )
  in
  let clickHandlers =
    [ ViewUtils.eventNeither
        ~key:("fluid-selection-dbl-click" ^ tlidStr)
        "dblclick"
        (fun {altKey; _} ->
          match Entry.getFluidSelectionRange () with
          | Some (startPos, endPos) ->
              let selection =
                if altKey
                then SelectExpressionAt startPos
                else SelectTokenAt (startPos, endPos)
              in
              FluidMsg
                (FluidMouseDoubleClick
                   {tlid = p.tlid; editor = p.editor; selection})
          | None ->
              IgnoreMsg "fluid-dblclick-noselection")
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mousedown" ^ tlidStr)
        "mousedown"
        (fun _ -> FluidMsg (FluidMouseDown p.tlid))
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mouseup" ^ tlidStr)
        "mouseup"
        (fun _ ->
          match Entry.getFluidSelectionRange () with
          | Some (startPos, endPos) ->
              let selection =
                if startPos = endPos
                then ClickAt startPos
                else SelectText (startPos, endPos)
              in
              FluidMsg
                (FluidMouseUp {tlid = p.tlid; editor = p.editor; selection})
          | None ->
              (* Select the handler, if not selected *)
              FluidMsg
                (FluidMouseUp
                   {tlid = p.tlid; editor = p.editor; selection = ClickAt 0}))
    ; ViewUtils.onAnimationEnd ~key:("anim-end" ^ tlidStr) ~listener:(fun msg ->
          if msg = "flashError" || msg = "flashIncomplete"
          then FluidMsg FluidClearErrorDvSrc
          else IgnoreMsg "fluid-animation-end") ]
  in
  let idAttr =
    if p.fluidState.activeEditor = p.editor
    then Html.id "active-editor"
    else Html.noProp
  in
  Html.div
    ( [ idAttr
      ; Html.class' "fluid-tokens"
      ; Vdom.prop "contentEditable" "true"
      ; Html.autofocus true
      ; Vdom.attribute "" "spellcheck" "false"
      ; Vdom.attribute "" (* disable grammarly crashes *) "data-gramm" "false"
      ]
    @ clickHandlers
    @ Tuple3.toList textInputListeners )
    (toHtml p)


let viewErrorIndicator (p : props) (ti : FluidToken.tokenInfo) :
    Types.msg Html.html =
  let returnTipe (name : string) =
    Functions.find name p.functions
    |> Option.map ~f:(fun fn -> fn.fnReturnTipe)
    |> Option.withDefault ~default:TAny
  in
  let liveValue (id : ID.t) = Analysis.getLiveValue' p.analysisStore id in
  let isEvalSuccess dv =
    match dv with
    | Some (DIncomplete _) | Some (DError _) ->
        false
    | Some _ ->
        true
    | _ ->
        false
  in
  match ti.token with
  | TFnName (id, _, _, fnName, Rail) ->
      let offset = string_of_int ti.startRow ^ "rem" in
      let icon =
        match (returnTipe fnName, liveValue id) with
        | TResult, Some (DErrorRail (DResult (ResError _))) ->
            ViewUtils.darkIcon "result-error"
        | TResult, v when isEvalSuccess v ->
            ViewUtils.darkIcon "result-ok"
        | TOption, Some (DErrorRail (DOption OptNothing)) ->
            ViewUtils.darkIcon "option-nothing"
        | TOption, v when isEvalSuccess v ->
            ViewUtils.darkIcon "option-just"
        | _ ->
            Vdom.noNode
      in
      let event =
        Vdom.noProp
        (* TEMPORARY DISABLE
          ViewUtils.eventNoPropagation
            ~key:("er-" ^ show_id id)
            "click"
            (fun _ -> TakeOffErrorRail (tlid, id)) *)
      in
      Html.div
        [Html.class' "error-indicator"; Html.styles [("top", offset)]; event]
        [icon]
  | _ ->
      Vdom.noNode


let errorRailView (p : props) : Types.msg Html.html =
  let indicators = List.map p.tokens ~f:(viewErrorIndicator p) in
  let hasMaybeErrors = List.any ~f:(fun e -> e <> Vdom.noNode) indicators in
  Html.div
    [Html.classList [("fluid-error-rail", true); ("show", hasMaybeErrors)]]
    indicators


(** [view] builds a fluid editor *)
let view (p : props) : Types.msg Html.html =
  Html.div [Html.class' "fluid-editor"] [tokensView p; errorRailView p]
