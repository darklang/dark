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

type state = Types.fluidState

let toTokens = Printer.toTokens

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
      let argIDs = List.map ~f:E.id allExprs in
      ( match ti.token with
      | TFnVersion (id, _, _, _) ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | TFnName (id, _, displayName, fnName, _) when displayName = fnName ->
          ViewFnExecution.fnExecutionButton vs fn id argIDs
      | _ ->
          Vdom.noNode )
  | Some _ | None ->
      Vdom.noNode


let toHtml ~(vs : ViewUtils.viewState) ~tlid ~state (ast : ast) :
    Types.msg Html.html list =
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
  let currentTokenInfo = Fluid.getToken' state vs.tokens in
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
  let isCmdDrawn = ref false in
  List.map vs.tokens ~f:(fun ti ->
      let dropdown () =
        match state.cp.onExpr with
        | Some id when id = T.tid ti.token && not !isCmdDrawn ->
            isCmdDrawn := true ;
            FluidCommands.viewCommandPalette state.cp
        | _ ->
            if Fluid.isAutocompleting ti state
            then viewAutocomplete state.ac
            else Vdom.noNode
      in
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
          ; (errorType, errorType <> "")
          ; (* This expression is the source of an incomplete propogated
             * into another, where the cursor is currently on *)
            ("is-origin", sourceOfCurrentToken ti = Some analysisId)
          ; ( "jumped-to"
            , match state.errorDvSrc with
              | SourceNone ->
                  false
              | SourceId id ->
                  id = tokenId ) ]
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
                    | {detail = 2; altKey = true; _} ->
                        FluidMsg
                          (FluidMouseUp
                             (tlid, Fluid.getExpressionRangeAtCaret state ast))
                    | {detail = 2; altKey = false; _} ->
                        FluidMsg
                          (FluidMouseUp
                             (tlid, Fluid.getTokenRangeAtCaret state ast))
                    | _ ->
                        recover
                          "detail was not 2 in the doubleclick event"
                          ~debug:ev
                          (FluidMsg (FluidMouseUp (tlid, None))) )
                | None ->
                    recover
                      "found no caret pos in the doubleclick handler"
                      ~debug:ev
                      (FluidMsg (FluidMouseUp (tlid, None))))
          ; ViewUtils.eventNoPropagation
              ~key:("fluid-selection-mousedown" ^ idStr)
              "mousedown"
              (fun _ -> FluidMsg (FluidMouseDown tlid))
          ; ViewUtils.eventNoPropagation
              ~key:("fluid-selection-mouseup" ^ idStr)
              "mouseup"
              (fun _ -> FluidMsg (FluidMouseUp (tlid, None)))
          ; ViewUtils.onAnimationEnd
              ~key:("anim-end" ^ idStr)
              ~listener:(fun msg ->
                if msg = "flashError" || msg = "flashIncomplete"
                then FluidMsg FluidClearErrorDvSrc
                else IgnoreMsg) ]
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
      else [element []])
  |> List.flatten


let viewDval tlid dval ~(canCopy : bool) =
  let text = Runtime.toRepr dval in
  [Html.text text; (if canCopy then viewCopyButton tlid text else Vdom.noNode)]


let viewLiveValue
    ~(tlid : tlid) ~(ast : ast) ~(vs : viewState) ~(state : fluidState) :
    Types.msg Html.html =
  (* Renders dval*)
  let renderDval = viewDval tlid in
  (* Renders live value for token *)
  let renderTokenLv token id =
    let fnLoading =
      (* If fn needs to be manually executed, check status *)
      let fn = fnForToken state token in
      let args = fnArgExprs token ast |> List.map ~f:E.id in
      Option.andThen fn ~f:(fun fn ->
          if fn.fnPreviewExecutionSafe
          then None
          else
            let id = T.tid token in
            ViewFnExecution.fnExecutionStatus vs fn id args
            |> ViewFnExecution.executionError
            |> Option.some)
    in
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess (DIncomplete _) when Option.isSome fnLoading ->
        [Html.text (Option.withDefault ~default:"" fnLoading)]
    | LoadableSuccess (DIncomplete (SourceId srcId) as dv)
    | LoadableSuccess (DError (SourceId srcId, _) as dv)
      when srcId <> id ->
        let msg =
          "<"
          ^ (dv |> Runtime.typeOf |> Runtime.tipe2str)
          ^ "> Click to locate source"
        in
        [ Html.div
            [ ViewUtils.eventNoPropagation
                ~key:("lv-src-" ^ deID srcId)
                "click"
                (fun _ -> FluidMsg (FluidFocusOnToken srcId))
            ; Html.class' "jump-src" ]
            [Html.text msg] ]
    | LoadableSuccess (DError _ as dv) | LoadableSuccess (DIncomplete _ as dv)
      ->
        renderDval dv ~canCopy:false
    | LoadableSuccess dval ->
        renderDval dval ~canCopy:true
    | LoadableNotInitialized | LoadableLoading _ ->
        [ViewUtils.fontAwesome "spinner"]
    | LoadableError err ->
        [Html.text ("Error loading live value: " ^ err)]
  in
  Fluid.getToken' state vs.tokens
  |> Option.andThen ~f:(fun ti ->
         let row = ti.startRow in
         let content =
           match AC.highlighted state.ac with
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
         let offset = float_of_int row +. 1.5 in
         Html.div
           [ Html.class' "live-value"
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
    let id = E.id ast in
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess dval ->
        Html.div
          [ Html.classList
              [ ("return-value", true)
              ; ( "refreshed"
                , match vs.handlerProp with
                  | Some {execution = Complete; _} ->
                      true
                  | _ ->
                      false ) ] ]
          (viewDval vs.tlid dval ~canCopy:true)
    | _ ->
        Vdom.noNode
  else Vdom.noNode


let viewAST ~(vs : ViewUtils.viewState) (ast : ast) : Types.msg Html.html list =
  let ({analysisStore; tlid; _} : ViewUtils.viewState) = vs in
  let state = vs.fluidState in
  let tokenInfos = vs.tokens in
  let errorRail =
    let indicators =
      tokenInfos
      |> List.map ~f:(fun ti -> viewErrorIndicator ~analysisStore ~state ti)
    in
    let hasMaybeErrors = List.any ~f:(fun e -> e <> Vdom.noNode) indicators in
    Html.div
      [Html.classList [("fluid-error-rail", true); ("show", hasMaybeErrors)]]
      indicators
  in
  let liveValue =
    if vs.cursorState = FluidEntering tlid
    then viewLiveValue ~tlid ~ast ~vs ~state
    else Vdom.noNode
  in
  let returnValue = viewReturnValue vs ast in
  let onEvt
      (constructor : Web.Node.event -> FluidTextInput.t option)
      (evt : Web.Node.event) : 'a option =
    constructor evt
    |> Option.map ~f:(fun t -> Types.FluidMsg (Types.FluidTextInput t))
  in
  [ liveValue
  ; Html.div
      [ Attrs.id Fluid.editorID
      ; Vdom.prop "contentEditable" "true"
      ; Attrs.autofocus true
      ; Vdom.attribute "" "spellcheck" "false"
      ; Html.onCB
          "input"
          ("input" ^ show_tlid tlid)
          (onEvt FluidTextInput.fromInputEvent)
      ; Html.onCB
          "compositionend"
          ("compositionend" ^ show_tlid tlid)
          (onEvt FluidTextInput.fromCompositionEndEvent) ]
      (toHtml ast ~vs ~tlid ~state)
  ; returnValue
  ; errorRail ]


let viewStatus (m : model) (ast : ast) (s : state) : Types.msg Html.html =
  let tokens = toTokens ast in
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
    ; dtText "lastKey"
    ; Html.dd
        []
        [ Html.text
            ( K.toName s.lastKey
            ^ ", "
            ^ ( K.toChar s.lastKey
              |> Option.map ~f:String.fromChar
              |> Option.withDefault ~default:"" ) ) ]
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
    let tokenInfo tkn =
      Html.dd [Attrs.class' "tokenInfo"] [T.show_tokenInfo tkn]
    in
    let ddLeft =
      match left with
      | L (_, left) ->
          tokenInfo left
      | R (_, _) ->
          ddText "Right"
      | No ->
          ddText "None"
    in
    let ddRight =
      match right with
      | L (_, _) ->
          ddText "Left"
      | R (_, right) ->
          tokenInfo right
      | No ->
          ddText "None"
    in
    let ddNext =
      match next with Some next -> tokenInfo next | None -> ddText "None"
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
