open Prelude

let viewPlayIcon ~(vs : ViewUtils.viewState) (ti : FluidToken.tokenInfo) :
    Types.msg Html.html =
  match ViewUtils.fnForToken vs.fluidState ti.token with
  | Some fn when not fn.fnPreviewExecutionSafe ->
      (* Looking these up can be slow, so the fnPreviewExecutionSafe check
       * above is very important *)
      let allExprs = AST.getArguments (FluidToken.tid ti.token) vs.ast in
      let argIDs = List.map ~f:FluidExpression.toID allExprs in
      ( match ti.token with
      | TFnVersion (id, _, _, _) ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | TFnName (id, _, displayName, fnName, _)
      (* If fn is unversioned or is v0 *)
        when displayName = fnName || displayName ^ "_v0" = fnName ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | _ ->
          Vdom.noNode )
  | Some _ | None ->
      Vdom.noNode


let toHtml (vs : ViewUtils.viewState) (editor : ViewUtils.editorViewState) :
    Types.msg Html.html list =
  (* Gets the source of a DIncomplete given an expr id *)
  let sourceOfExprValue id =
    if FluidToken.validID id
    then
      (* Only highlight incompletes and errors on executed paths *)
      match Analysis.getLiveValueLoadable vs.analysisStore id with
      | LoadableSuccess (ExecutedResult (DIncomplete (SourceId id))) ->
          (Some id, "dark-incomplete")
      | LoadableSuccess (ExecutedResult (DError (SourceId id, _))) ->
          (Some id, "dark-error")
      | _ ->
          (None, "")
    else (None, "")
  in
  let wasExecuted id : bool option =
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess (ExecutedResult _) ->
        Some true
    | LoadableSuccess (NonExecutedResult _) ->
        Some false
    | _ ->
        None
  in
  let currentTokenInfo = Fluid.getToken vs.ast vs.fluidState in
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
    match vs.fluidState.cp.location with
    | Some (ltlid, id) when editor.tlid = ltlid ->
        (* Reversing list will get us the last token visually rendered with matching expression ID, so we don't have to keep track of max pos *)
        editor.tokens
        |> List.reverse
        |> List.getBy ~f:(fun ti -> FluidToken.tid ti.token = id)
    | _ ->
        None
  in
  let dropdown ti =
    match cmdToken with
    | Some onTi when onTi = ti ->
        FluidCommands.viewCommandPalette vs.fluidState.cp
    | _ ->
        if Fluid.isAutocompleting ti vs.fluidState
        then FluidAutocompleteView.view vs.fluidState.ac
        else Vdom.noNode
  in
  let isSelected tokenStart tokenEnd =
    let selStart, selEnd = Fluid.getSelectionRange vs.fluidState in
    selStart <= tokenStart && tokenEnd <= selEnd
  in
  List.map editor.tokens ~f:(fun ti ->
      let element nested =
        let tokenId = FluidToken.tid ti.token in
        let idStr = ID.toString tokenId in
        let content = FluidToken.toText ti.token in
        let analysisId = FluidToken.analysisID ti.token in
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
          (* We want 0 precedence to only show up at the AST root and not in any wraparounds, so this goes 0123412341234... *)
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
            && (* This expression is the source of its own incompleteness. We only draw underlines under sources of incompletes, not all propagated occurrences. *)
            sourceId = Some analysisId
          in
          [ ("related-change", List.member ~value:tokenId vs.hoveringRefs)
          ; ("cursor-on", currentTokenInfo = Some ti)
          ; ("fluid-error", isError)
          ; ( "fluid-executed"
            , wasExecuted tokenId |> Option.withDefault ~default:false )
          ; ( "fluid-not-executed"
            , not (wasExecuted tokenId |> Option.withDefault ~default:true) )
          ; (errorType, errorType <> "")
          ; (* This expression is the source of an incomplete propogated
             * into another, where the cursor is currently on *)
            ("is-origin", sourceOfCurrentToken ti = Some analysisId)
          ; ( "jumped-to"
            , match vs.fluidState.errorDvSrc with
              | SourceNone ->
                  false
              | SourceId id ->
                  id = tokenId )
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
      if vs.permission = Some ReadWrite
      then [element [dropdown ti; viewPlayIcon ti ~vs]]
      else [element []])
  |> List.flatten


(** [view] builds a fluid editor panel for the given [editor]. *)
let view (vs : ViewUtils.viewState) (editor : ViewUtils.editorViewState) :
    Types.msg Html.html =
  let ({tlid; fluidState = state; _} : ViewUtils.viewState) = vs in
  let tlidStr = TLID.toString tlid in
  let textInputListeners =
    (* the command palette is inside div.fluid-editor but has it's own input
     * handling, so don't do normal fluid input stuff if it's open *)
    if FluidCommands.isOpened vs.fluidState.cp
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
        (fun ev ->
          match Entry.getFluidCaretPos () with
          | Some pos ->
              let state = {state with newPos = pos; oldPos = state.newPos} in
              ( match ev with
              | {detail = 2; altKey = true; _} ->
                  FluidMsg
                    (FluidMouseUp
                       { tlid
                       ; editorId = editor.editorId
                       ; selection =
                           Fluid.getExpressionRangeAtCaret vs.ast state })
              | {detail = 2; altKey = false; _} ->
                  FluidMsg
                    (FluidMouseUp
                       { tlid
                       ; editorId = editor.editorId
                       ; selection = Fluid.getTokenRangeAtCaret vs.ast state })
              | _ ->
                  recover
                    "detail was not 2 in the doubleclick event"
                    ~debug:ev
                    (FluidMsg
                       (FluidMouseUp
                          {tlid; editorId = editor.editorId; selection = None}))
              )
          | None ->
              recover
                "found no caret pos in the doubleclick handler"
                ~debug:ev
                (FluidMsg
                   (FluidMouseUp
                      {tlid; editorId = editor.editorId; selection = None})))
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mousedown" ^ tlidStr)
        "mousedown"
        (fun _ -> FluidMsg (FluidMouseDown tlid))
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mouseup" ^ tlidStr)
        "mouseup"
        (fun _ ->
          FluidMsg
            (FluidMouseUp {tlid; editorId = editor.editorId; selection = None}))
    ; ViewUtils.onAnimationEnd ~key:("anim-end" ^ tlidStr) ~listener:(fun msg ->
          if msg = "flashError" || msg = "flashIncomplete"
          then FluidMsg FluidClearErrorDvSrc
          else IgnoreMsg) ]
  in
  let idAttr =
    if vs.fluidState.activeEditorId = editor.editorId
    then Html.id "active-editor"
    else Html.noProp
  in
  Html.div
    ( [ idAttr
      ; Html.class' "fluid-editor"
      ; Vdom.prop "contentEditable" "true"
      ; Html.autofocus true
      ; Vdom.attribute "" "spellcheck" "false"
      ; Vdom.attribute "" (* disable grammarly crashes *) "data-gramm" "false"
      ]
    @ clickHandlers
    @ Tuple3.toList textInputListeners )
    (toHtml vs editor)
