open Prelude

(* Tea *)
module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events

(* Fluid *)
module K = FluidKeyboard
module AC = FluidAutocomplete
module T = FluidToken
module E = FluidExpression
module P = FluidPattern
module Printer = FluidPrinter
module Util = FluidUtil

(* Tea *)

type viewState = ViewUtils.viewState

type ast = E.t

type token = T.t

let viewAutocomplete (ac : Types.fluidAutocompleteState) : Types.msg Html.html =
  let toList acis class' index =
    List.indexedMap
      ~f:(fun i item ->
        let highlighted = index = i in
        let name = AC.asName item in
        let fnDisplayName = FluidUtil.fnDisplayName name in
        let versionDisplayName = FluidUtil.versionDisplayName name in
        let versionView =
          if String.length versionDisplayName > 0
          then Html.span [Html.class' "version"] [Html.text versionDisplayName]
          else Vdom.noNode
        in
        Html.li
          ~unique:name
          [ Attrs.classList
              [ ("autocomplete-item", true)
              ; ("fluid-selected", highlighted)
              ; (class', true) ]
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; ViewEntry.defaultPasteHandler
          ; ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.eventNoPropagation ~key:("ac-" ^ name) "click" (fun _ ->
                FluidMsg (FluidAutocompleteClick item))
          ; ViewUtils.eventBoth
              ~key:("ac-mousemove" ^ name)
              "mousemove"
              (fun _ -> FluidMsg (FluidUpdateDropdownIndex i)) ]
          [ Html.text fnDisplayName
          ; versionView
          ; Html.span [Html.class' "types"] [Html.text <| AC.asTypeString item]
          ])
      acis
  in
  let index = ac.index |> Option.withDefault ~default:(-1) in
  let invalidIndex = index - List.length ac.completions in
  let autocompleteList =
    toList ac.completions "valid" index
    @ toList ac.invalidCompletions "invalid" invalidIndex
  in
  Html.div [Attrs.id "fluid-dropdown"] [Html.ul [] autocompleteList]


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


let fnArgExprs (token : token) (ast : ast) : E.t list =
  let id = T.tid token in
  let exprs =
    match E.find id ast with
    | Some (EFnCall (_, _, exprs, _)) ->
        exprs
    | Some (EBinOp (_, _, lhs, rhs, _)) ->
        [lhs; rhs]
    | _ ->
        []
  in
  match exprs with
  | EPipeTarget _ :: rest ->
      (* It's a little slow to look this up, so only look when we know we're
       * in a thread. *)
      let previous = ast |> AST.threadPrevious id |> Option.toList in
      previous @ rest
  | exprs ->
      exprs


let viewPlayIcon
    ~(vs : ViewUtils.viewState) ~state (ast : ast) (ti : T.tokenInfo) :
    Types.msg Html.html =
  match fnForToken state ti.token with
  | Some fn when not fn.fnPreviewExecutionSafe ->
      (* Looking these up can be slow, so the fnPreviewExecutionSafe check
       * above is very important *)
      let allExprs = fnArgExprs ti.token ast in
      let argIDs = List.map ~f:E.toID allExprs in
      ( match ti.token with
      | TFnVersion (id, _, _, _) ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | TFnName (id, _, displayName, fnName, _) when displayName = fnName ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | _ ->
          Vdom.noNode )
  | Some _ | None ->
      Vdom.noNode


let toHtml ~(vs : ViewUtils.viewState) ~tokenInfos ~tlid ~state (ast : ast) :
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
  let currentTokenInfo = Fluid.getToken' state tokenInfos in
  let sourceOfCurrentToken onTi =
    currentTokenInfo
    |> Option.andThen ~f:(fun ti ->
           if T.isBlank ti.token || onTi.startRow = ti.startRow
           then None
           else
             let someId, _ = T.analysisID ti.token |> sourceOfExprValue in
             someId)
  in
  let nesting = ref 0 in
  let cmdToken =
    match state.cp.location with
    | Some (ltlid, id) when tlid = ltlid ->
        (* Reversing list will get us the last token visually rendered with matching expression ID, so we don't have to keep track of max pos *)
        tokenInfos
        |> List.reverse
        |> List.getBy ~f:(fun ti -> FluidToken.tid ti.token = id)
    | _ ->
        None
  in
  let dropdown ti =
    match cmdToken with
    | Some onTi when onTi = ti ->
        FluidCommands.viewCommandPalette state.cp
    | _ ->
        if Fluid.isAutocompleting ti state
        then viewAutocomplete state.ac
        else Vdom.noNode
  in
  let isSelected tokenStart tokenEnd =
    let selStart, selEnd = Fluid.getSelectionRange state in
    selStart <= tokenStart && tokenEnd <= selEnd
  in
  List.map tokenInfos ~f:(fun ti ->
      let element nested =
        let tokenId = T.tid ti.token in
        let idStr = deID tokenId in
        let content = T.toText ti.token in
        let analysisId = T.analysisID ti.token in
        (* Apply CSS classes to token *)
        let tokenClasses = T.toCssClasses ti.token in
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
            T.isErrorDisplayable ti.token
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
            , match state.errorDvSrc with
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
                  [Attrs.class' (cls |> String.join ~sep:" ")]
                  [Html.text content] ]
          | None ->
              [Html.text content]
        in
        Html.span
          [Html.classList (cls @ conditionalClasses)]
          (innerNode @ nested)
      in
      if vs.permission = Some ReadWrite
      then [element [dropdown ti; viewPlayIcon ast ti ~vs ~state]]
      else [element []])
  |> List.flatten


let viewArrow (curID : id) (srcID : id) : Types.msg Html.html =
  let curSelector = ".id-" ^ deID curID in
  let srcSelector = ".id-" ^ deID srcID in
  match
    (Native.Ext.querySelector curSelector, Native.Ext.querySelector srcSelector)
  with
  | Some curElem, Some srcElem ->
      let curRect = Native.Ext.getBoundingClient curElem curSelector in
      let srcRect = Native.Ext.getBoundingClient srcElem srcSelector in
      let height = curRect.bottom - srcRect.top in
      Html.div
        [ Html.class' "src-arrow"
        ; Html.styles [("height", string_of_int height ^ "px")] ]
        []
  | _ ->
      Vdom.noNode


let viewDval tlid dval ~(canCopy : bool) =
  let text = Runtime.toRepr dval in
  [Html.text text; (if canCopy then viewCopyButton tlid text else Vdom.noNode)]


let viewLiveValue ~(tlid : tlid) ~(ast : ast) ~(vs : viewState) :
    Types.msg Html.html =
  (* isLoaded will be set to false later if we are in the middle of loading
   * results. All other states are considered loaded. This is used to apply
   * a class ".loaded" purely for integration tests being able to know when
   * the live value content is ready and can be asserted on *)
  let isLoaded = ref true in
  (* Renders dval*)
  let renderDval = viewDval tlid in
  (* Renders live value for token *)
  let renderTokenLv token id =
    let fnLoading =
      (* If fn needs to be manually executed, check status *)
      fnForToken vs.fluidState token
      |> Option.andThen ~f:(fun fn ->
             if fn.fnPreviewExecutionSafe
             then None
             else
               let id = T.tid token in
               let args = fnArgExprs token ast |> List.map ~f:E.toID in
               ViewFnExecution.fnExecutionStatus vs fn id args
               |> ViewFnExecution.executionError
               |> Option.some)
    in
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess (ExecutedResult (DIncomplete _))
      when Option.isSome fnLoading ->
        [Html.text (Option.withDefault ~default:"" fnLoading)]
    | LoadableSuccess (ExecutedResult (DIncomplete (SourceId srcId) as dv))
    | LoadableSuccess (ExecutedResult (DError (SourceId srcId, _) as dv))
      when srcId <> id ->
        let errType = dv |> Runtime.typeOf |> Runtime.tipe2str in
        let msg = "<" ^ errType ^ ">" in
        [ viewArrow id srcId
        ; Html.div
            [ ViewUtils.eventNoPropagation
                ~key:("lv-src-" ^ deID srcId)
                "click"
                (fun _ -> FluidMsg (FluidFocusOnToken srcId))
            ; Html.class' "jump-src"
            ; Html.title ("Click here to go to the source of " ^ errType) ]
            [Html.text msg; ViewUtils.fontAwesome "arrow-alt-circle-up"] ]
    | LoadableSuccess (ExecutedResult (DError _ as dval))
    | LoadableSuccess (ExecutedResult (DIncomplete _ as dval)) ->
        renderDval dval ~canCopy:false
    | LoadableSuccess (ExecutedResult dval) ->
        renderDval dval ~canCopy:true
    | LoadableNotInitialized | LoadableLoading _ ->
        isLoaded := false ;
        [ViewUtils.fontAwesome "spinner"]
    | LoadableSuccess (NonExecutedResult (DError _ as dval))
    | LoadableSuccess (NonExecutedResult (DIncomplete _ as dval)) ->
        [ Html.div
            []
            ( Html.text "This code was not executed in this trace\n\n"
            :: renderDval dval ~canCopy:false ) ]
    | LoadableSuccess (NonExecutedResult dval) ->
        [ Html.div
            []
            ( Html.text "This code was not executed in this trace.\n\n"
            :: renderDval dval ~canCopy:true ) ]
    | LoadableError err ->
        [Html.text ("Error loading live value: " ^ err)]
  in
  let tokens =
    List.getAt ~index:vs.fluidState.activeEditorPanelIdx vs.tokenSplits
    |> Option.map ~f:(fun (p : FluidPrinter.split) -> p.tokens)
    |> Option.withDefault ~default:[]
  in
  Fluid.getToken' vs.fluidState tokens
  |> Option.andThen ~f:(fun ti ->
         let row = ti.startRow in
         let content =
           match AC.highlighted vs.fluidState.ac with
           | Some (FACVariable (_, Some dv)) ->
               (* If autocomplete is open and a variable is highlighted,
                * then show its dval *)
               Some (renderDval dv ~canCopy:true)
           | _ ->
               (* Else show live value of current token *)
               let token = ti.token in
               let id = T.analysisID token in
               if T.validID id then Some (renderTokenLv token id) else None
         in
         Option.pair content (Some row))
  (* Render live value to the side *)
  |> Option.map ~f:(fun (content, row) ->
         let offset = float_of_int row in
         Html.div
           [ Html.classList [("live-value", true); ("loaded", !isLoaded)]
           ; Html.styles [("top", Js.Float.toString offset ^ "rem")]
           ; Attrs.autofocus false
           ; Vdom.attribute "" "spellcheck" "false" ]
           content)
  (* If there's a failure at any point, we don't render the live-value wrapper *)
  |> Option.withDefault ~default:Vdom.noNode


let viewReturnValue (vs : ViewUtils.viewState) (ast : ast) : Types.msg Html.html
    =
  if tlidOf vs.cursorState = Some vs.tlid
  then
    let id = E.toID ast in
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess (ExecutedResult dval) ->
        let isRefreshed =
          match vs.handlerProp with
          | Some {execution = Complete; _} ->
              true
          | _ ->
              false
        in
        let isIncomplete =
          (* Since HTTP and userFunctions are the case where Incomplete return is likely to case and error, we only want to highlight those cases.  *)
          if Toplevel.isHTTPHandler vs.tl || Toplevel.isUserFunction vs.tl
          then match dval with DIncomplete _ -> true | _ -> false
          else false
        in
        let auxText =
          if isIncomplete
          then
            [ Html.span
                [Html.class' "msg"]
                [ Html.text
                    "Your code needs to return a value in the last expression"
                ] ]
          else [Vdom.noNode]
        in
        Html.div
          [ Html.classList
              [ ("return-value", true)
              ; ("refreshed", isRefreshed)
              ; ("incomplete", isIncomplete) ] ]
          (viewDval vs.tlid dval ~canCopy:true @ auxText)
    | _ ->
        Vdom.noNode
  else Vdom.noNode


(** [fluidEditorView] builds a fluid editor panel for the given [tokenInfos].
  * [~idx] is the index of the editor panel, with 0 being the "main" editor
  * and >0 being any sub-editors (like for a feature flag). This [~idx]
  * corresponds to the index of the tokenInfo list return from
  * tokenizeWithSplits, and is used to ensure caret placement/focus/editing act
  * against the correct token stream (that is, clicking on panel idx=1 means
  * you are now editing index 1 of the splits returned from
  * FluidPrinter.tokenizeWithSplits). *)
let fluidEditorView
    ~(idx : int)
    (vs : ViewUtils.viewState)
    (tokenInfos : FluidPrinter.tokenInfo list)
    (ast : ast) : Types.msg Html.html =
  let ({tlid; fluidState = state; _} : ViewUtils.viewState) = vs in
  let textInputListeners =
    (* the command palette is inside div.fluid-editor but has it's own input
     * handling, so don't do normal fluid input stuff if it's open *)
    if FluidCommands.isOpened vs.fluidState.cp
    then (Html.noProp, Html.noProp, Html.noProp)
    else
      ( Html.onCB
          "keydown"
          ("keydown" ^ show_tlid tlid)
          (FluidKeyboard.onKeydown (fun x ->
               FluidMsg (FluidInputEvent (Keypress x))))
      , Html.onCB
          "beforeinput"
          ("beforeinput" ^ show_tlid tlid)
          FluidTextInput.fromInputEvent
      , Html.onCB
          "compositionend"
          ("compositionend" ^ show_tlid tlid)
          FluidTextInput.fromCompositionEndEvent )
  in
  let clickHandlers =
    let idStr = deTLID tlid in
    [ ViewUtils.eventNeither
        ~key:("fluid-selection-dbl-click" ^ idStr)
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
                       ; selection = Fluid.getExpressionRangeAtCaret state ast
                       ; editorIdx = idx })
              | {detail = 2; altKey = false; _} ->
                  FluidMsg
                    (FluidMouseUp
                       { tlid
                       ; selection = Fluid.getTokenRangeAtCaret state ast
                       ; editorIdx = idx })
              | _ ->
                  recover
                    "detail was not 2 in the doubleclick event"
                    ~debug:ev
                    (FluidMsg
                       (FluidMouseUp {tlid; selection = None; editorIdx = idx}))
              )
          | None ->
              recover
                "found no caret pos in the doubleclick handler"
                ~debug:ev
                (FluidMsg
                   (FluidMouseUp {tlid; selection = None; editorIdx = idx})))
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mousedown" ^ idStr)
        "mousedown"
        (fun _ -> FluidMsg (FluidMouseDown tlid))
    ; ViewUtils.eventNoPropagation
        ~key:("fluid-selection-mouseup" ^ idStr)
        "mouseup"
        (fun _ ->
          FluidMsg (FluidMouseUp {tlid; selection = None; editorIdx = idx}))
    ; ViewUtils.onAnimationEnd ~key:("anim-end" ^ idStr) ~listener:(fun msg ->
          if msg = "flashError" || msg = "flashIncomplete"
          then FluidMsg FluidClearErrorDvSrc
          else IgnoreMsg) ]
  in
  let idAttr =
    if vs.fluidState.activeEditorPanelIdx = idx
    then Attrs.id "active-editor"
    else Attrs.noProp
  in
  Html.div
    ( [ idAttr
      ; Html.class' "fluid-editor"
      ; Vdom.prop "contentEditable" "true"
      ; Attrs.autofocus true
      ; Vdom.attribute "" "spellcheck" "false"
      ; Vdom.attribute "" (* disable grammarly crashes *) "data-gramm" "false"
      ]
    @ clickHandlers
    @ Tuple3.toList textInputListeners )
    (toHtml ast ~tokenInfos ~vs ~tlid ~state)


let viewAST ~(vs : ViewUtils.viewState) (ast : ast) : Types.msg Html.html list =
  let ({analysisStore; tlid; fluidState = state; _} : ViewUtils.viewState) =
    vs
  in
  let mainTokenInfos = ViewUtils.getMainTokens vs in
  let errorRail =
    let indicators =
      mainTokenInfos |> List.map ~f:(viewErrorIndicator ~analysisStore ~state)
    in
    let hasMaybeErrors = List.any ~f:(fun e -> e <> Vdom.noNode) indicators in
    Html.div
      [Html.classList [("fluid-error-rail", true); ("show", hasMaybeErrors)]]
      indicators
  in
  let liveValue =
    if vs.cursorState = FluidEntering tlid
    then viewLiveValue ~tlid ~ast ~vs
    else Vdom.noNode
  in
  let mainEditor = fluidEditorView ~idx:0 vs mainTokenInfos ast in
  let returnValue = viewReturnValue vs ast in
  let secondaryEditors =
    let findRowOffestOfMainTokenWithId (target : id) : int option =
      (* FIXME(ds) this is a giant hack to find the row offset of the corresponding
       * token in the main view for each secondary editor. This works by getting
       * the id of the split (ie, the id of the first token in the split)
       * and then looking through the main tokens [O(N)] to find one with a
       * corresponding id. This is brittle and will likely break at some point. We
       * should do something better. *)
      List.find mainTokenInfos ~f:(fun ti -> target = T.tid ti.token)
      |> Option.map ~f:(fun ti -> ti.startRow)
    in
    if List.member vs.testVariants ~value:FeatureFlagVariant
    then
      List.tail vs.tokenSplits
      |> Option.withDefault ~default:[]
      |> List.mapi ~f:(fun (i : int) (s : Printer.split) ->
             let errorRail =
               Html.div
                 [Html.classList [("fluid-error-rail", true); ("show", true)]]
                 []
             in
             let rowOffset =
               findRowOffestOfMainTokenWithId s.id
               |> Option.withDefault ~default:0
             in
             Html.div
               [ Html.class' "fluid-secondary-editor"
               ; Html.styles [("top", string_of_int rowOffset ^ "rem")] ]
               [fluidEditorView ~idx:(i + 1) vs s.tokens ast; errorRail])
    else []
  in
  mainEditor :: liveValue :: returnValue :: errorRail :: secondaryEditors


let viewStatus (m : model) (ast : ast) : Types.msg Html.html =
  let s = m.fluidState in
  let tokens = Printer.tokensForSplit ~index:s.activeEditorPanelIdx ast in
  let ddText txt = Html.dd [] [Html.text txt] in
  let dtText txt = Html.dt [] [Html.text txt] in
  let posData =
    let oldGrid = Fluid.gridFor ~pos:s.oldPos tokens in
    let newGrid = Fluid.gridFor ~pos:s.newPos tokens in
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
    ; dtText "lastInput"
    ; Html.dd [] [Html.text (show_fluidInputEvent s.lastInput)]
    ; dtText "selection"
    ; Html.dd
        []
        [ Html.text
            ( s.selectionStart
            |> Option.map ~f:(fun selStart ->
                   string_of_int selStart ^ "->" ^ string_of_int s.newPos)
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "midClick"
    ; Html.dd [] [Html.text (string_of_bool s.midClick)] ]
  in
  let error =
    [dtText "error"; ddText (Option.withDefault s.error ~default:"None")]
  in
  let tokenData =
    let left, right, next = Fluid.getNeighbours tokens ~pos:s.newPos in
    let ddNoProp1 txt = Html.dd [Html.noProp] [Html.text txt] in
    let tokenInfo tkn =
      Html.dd [Attrs.class' "tokenInfo"] [T.show_tokenInfo tkn]
    in
    let ddLeft =
      match left with
      | L (_, left) ->
          tokenInfo left
      | R (_, _) ->
          ddNoProp1 "Right"
      | No ->
          ddNoProp1 "None"
    in
    let ddRight =
      match right with
      | L (_, _) ->
          ddNoProp1 "Left"
      | R (_, right) ->
          tokenInfo right
      | No ->
          ddNoProp1 "None"
    in
    let ddNext =
      match next with Some next -> tokenInfo next | None -> ddNoProp1 "None"
    in
    [dtText "left"; ddLeft; dtText "right"; ddRight; dtText "next"; ddNext]
  in
  let actions =
    [ dtText "actions"
    ; Html.dd
        [Attrs.class' "actions"]
        [ Html.ul
            []
            (List.map s.actions ~f:(fun txt -> Html.li [] [Html.text txt])) ] ]
  in
  let cursorState =
    [dtText "cursorState"; ddText (show_cursorState m.cursorState)]
  in
  let lastMod = [dtText "lastMod"; ddText (show_modification m.lastMod)] in
  let status =
    List.concat [posData; error; tokenData; actions; cursorState; lastMod]
  in
  Html.div [Attrs.id "fluid-status"] [Html.dl [] status]


let view (vs : viewState) (e : fluidExpr) =
  [Html.div [Html.class' "fluid-ast"] (viewAST ~vs e)]
