open Prelude

type state =
  { analysisStore : analysisStore
  ; ast : FluidAST.t
  ; executingFunctions : ID.t list
  ; editor : fluidEditor
  ; hoveringRefs : ID.t list
  ; fluidState : fluidState
  ; permission : permission option
  ; tlid : TLID.t
  ; tokens : FluidToken.tokenInfo list }

let isActiveEditor (s : state) = s.fluidState.activeEditor = s.editor

let stateToFnExecutionState (s : state) : ViewFnExecution.state =
  { analysisStore = s.analysisStore
  ; ast = s.ast
  ; executingFunctions = s.executingFunctions
  ; permission = s.permission
  ; tlid = s.tlid }


let viewPlayIcon (s : state) (ti : FluidToken.tokenInfo) : Types.msg Html.html =
  match ViewUtils.fnForToken s.fluidState ti.token with
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
      let allExprs = AST.getArguments (FluidToken.tid ti.token) s.ast in
      let argIDs = List.map ~f:FluidExpression.toID allExprs in
      ( match ti.token with
      | TFnVersion (id, _, _, _) ->
          ViewFnExecution.fnExecutionButton
            (stateToFnExecutionState s)
            fn
            id
            argIDs
      | TFnName (id, _, displayName, fnName, _)
      (* If fn is unversioned or is v0 *)
        when displayName = fnName || displayName ^ "_v0" = fnName ->
          ViewFnExecution.fnExecutionButton
            (stateToFnExecutionState s)
            fn
            id
            argIDs
      | _ ->
          Vdom.noNode )
  | Some {fnPreviewSafety = Safe; _} | None ->
      Vdom.noNode


let toHtml (s : state) : Types.msg Html.html list =
  (* Gets the source of a DIncomplete given an expr id *)
  let sourceOfExprValue id =
    if FluidToken.validID id
    then
      (* Only highlight incompletes and errors on executed paths *)
      match Analysis.getLiveValueLoadable s.analysisStore id with
      | LoadableSuccess (ExecutedResult (DIncomplete (SourceId (tlid, id)))) ->
          (Some (tlid, id), "dark-incomplete")
      | LoadableSuccess (ExecutedResult (DError (SourceId (tlid, id), _))) ->
          (Some (tlid, id), "dark-error")
      | _ ->
          (None, "")
    else (None, "")
  in
  let wasExecuted id : bool option =
    match Analysis.getLiveValueLoadable s.analysisStore id with
    | LoadableSuccess (ExecutedResult _) ->
        Some true
    | LoadableSuccess (NonExecutedResult _) ->
        Some false
    | _ ->
        None
  in
  let currentTokenInfo = Fluid.getToken s.ast s.fluidState in
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
    match s.fluidState.cp.location with
    | Some (ltlid, id) when s.tlid = ltlid ->
        (* Reversing list will get us the last token visually rendered with matching expression ID, so we don't have to keep track of max pos *)
        s.tokens
        |> List.reverse
        |> List.getBy ~f:(fun ti -> FluidToken.tid ti.token = id)
    | _ ->
        None
  in
  let dropdown ti =
    match cmdToken with
    | Some onTi when onTi = ti ->
        FluidCommands.viewCommandPalette s.fluidState.cp
    | _ ->
        if Fluid.isAutocompleting ti s.fluidState && isActiveEditor s
        then FluidAutocompleteView.view s.fluidState.ac
        else Vdom.noNode
  in
  let isSelected tokenStart tokenEnd =
    let selStart, selEnd = Fluid.getSelectionRange s.fluidState in
    isActiveEditor s && selStart <= tokenStart && tokenEnd <= selEnd
  in
  let idsInAFlag =
    (* If we're in the main editor, find all the FF expressions, then build a
     * set of all the IDs of them and their children. This is used below to
     * apply a CSS class to highlight tokens contained in a FF. *)
    match s.editor with
    | FeatureFlagEditor _ ->
        ID.Set.empty
    | MainEditor ->
        FluidAST.filter s.ast ~f:(function
            | EFeatureFlag _ ->
                true
            | _ ->
                false)
        |> List.foldl ~init:ID.Set.empty ~f:(fun e acc ->
               match e with
               | FluidExpression.EFeatureFlag (_, _, _, oldCode, _) ->
                   let values =
                     FluidExpression.children oldCode
                     |> List.map ~f:FluidExpression.toID
                   in
                   ID.Set.add ~value:(FluidExpression.toID oldCode) acc
                   |> ID.Set.addMany ~values
               | _ ->
                   acc)
  in
  List.map s.tokens ~f:(fun ti ->
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
            sourceId = Some (s.tlid, analysisId)
          in
          [ ("related-change", List.member ~value:tokenId s.hoveringRefs)
          ; ("cursor-on", currentTokenInfo = Some ti)
          ; ("in-flag", ID.Set.member idsInAFlag ~value:tokenId)
          ; ("fluid-error", isError)
          ; ( "fluid-executed"
            , wasExecuted tokenId |> Option.withDefault ~default:false )
          ; ( "fluid-not-executed"
            , not (wasExecuted tokenId |> Option.withDefault ~default:true) )
          ; (errorType, errorType <> "")
          ; (* This expression is the source of an incomplete propogated
             * into another, where the cursor is currently on *)
            ("is-origin", sourceOfCurrentToken ti = Some (s.tlid, analysisId))
          ; ( "jumped-to"
            , match s.fluidState.errorDvSrc with
              | SourceNone ->
                  false
              | SourceId (tlid, id) ->
                  id = tokenId && s.tlid = tlid )
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
      if s.permission = Some ReadWrite
      then [element [dropdown ti; viewPlayIcon s ti]]
      else [element []])
  |> List.flatten


let tokensView (s : state) : Types.msg Html.html =
  let tlidStr = TLID.toString s.tlid in
  let textInputListeners =
    (* the command palette is inside div.fluid-editor but has it's own input
     * handling, so don't do normal fluid input stuff if it's open *)
    if FluidCommands.isOpened s.fluidState.cp
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
              let fstate =
                {s.fluidState with newPos = pos; oldPos = s.fluidState.newPos}
              in
              ( match ev with
              | {detail = 2; altKey = true; _} ->
                  FluidMsg
                    (FluidMouseUp
                       { tlid = s.tlid
                       ; editor = s.editor
                       ; selection =
                           Fluid.getExpressionRangeAtCaret s.ast fstate })
              | {detail = 2; altKey = false; _} ->
                  FluidMsg
                    (FluidMouseUp
                       { tlid = s.tlid
                       ; editor = s.editor
                       ; selection = Fluid.getTokenRangeAtCaret s.ast fstate })
              | _ ->
                  recover
                    "detail was not 2 in the doubleclick event"
                    ~debug:ev
                    (FluidMsg
                       (FluidMouseUp
                          {tlid = s.tlid; editor = s.editor; selection = None}))
              )
          | None ->
              recover
                "found no caret pos in the doubleclick handler"
                ~debug:ev
                (FluidMsg
                   (FluidMouseUp
                      {tlid = s.tlid; editor = s.editor; selection = None})))
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mousedown" ^ tlidStr)
        "mousedown"
        (fun _ -> FluidMsg (FluidMouseDown s.tlid))
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mouseup" ^ tlidStr)
        "mouseup"
        (fun _ ->
          FluidMsg
            (FluidMouseUp {tlid = s.tlid; editor = s.editor; selection = None}))
    ; ViewUtils.onAnimationEnd ~key:("anim-end" ^ tlidStr) ~listener:(fun msg ->
          if msg = "flashError" || msg = "flashIncomplete"
          then FluidMsg FluidClearErrorDvSrc
          else IgnoreMsg) ]
  in
  let idAttr =
    if s.fluidState.activeEditor = s.editor
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
    (toHtml s)


let viewErrorIndicator (s : state) (ti : FluidToken.tokenInfo) :
    Types.msg Html.html =
  let returnTipe (name : string) =
    Functions.findByNameInList name s.fluidState.ac.functions
    |> Option.map ~f:(fun fn -> fn.fnReturnTipe)
    |> Option.withDefault ~default:TAny
  in
  let liveValue (id : ID.t) = Analysis.getLiveValue' s.analysisStore id in
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


let errorRailView (s : state) : 'msg Html.html =
  let indicators = List.map s.tokens ~f:(viewErrorIndicator s) in
  let hasMaybeErrors = List.any ~f:(fun e -> e <> Vdom.noNode) indicators in
  Html.div
    [Html.classList [("fluid-error-rail", true); ("show", hasMaybeErrors)]]
    indicators


(** [view] builds a fluid editor *)
let view (s : state) : Types.msg Html.html =
  Html.div [Html.class' "fluid-editor"] [tokensView s; errorRailView s]
