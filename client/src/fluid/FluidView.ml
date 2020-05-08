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
module Printer = FluidTokenizer
module Util = FluidUtil

(* Tea *)

type viewState = ViewUtils.viewState

type token = T.t

let viewCopyButton tlid value : msg Html.html =
  Html.div
    [ Html.class' "copy-value"
    ; Html.title "Copy this expression's value to the clipboard"
    ; ViewUtils.eventNoPropagation
        "click"
        ~key:("copylivevalue-" ^ value ^ TLID.toString tlid)
        (fun m -> ClipboardCopyLivevalue (value, m.mePos)) ]
    [ViewUtils.fontAwesome "copy"]


let viewArrow (curID : ID.t) (srcID : ID.t) : Types.msg Html.html =
  let curSelector = ".id-" ^ ID.toString curID in
  let srcSelector = ".id-" ^ ID.toString srcID in
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


let viewLiveValue (vs : viewState) : Types.msg Html.html =
  (* isLoaded will be set to false later if we are in the middle of loading
   * results. All other states are considered loaded. This is used to apply
   * a class ".loaded" purely for integration tests being able to know when
   * the live value content is ready and can be asserted on *)
  let isLoaded = ref true in
  (* Renders dval*)
  let renderDval = viewDval vs.tlid in
  (* Renders live value for token *)
  let renderTokenLv token id =
    let fnLoading =
      (* If fn needs to be manually executed, check status *)
      ViewUtils.fnForToken vs.functions token
      |> Option.andThen ~f:(fun fn ->
             match fn.fnPreviewSafety with
             | Safe ->
                 None
             | Unsafe ->
                 let id = T.tid token in
                 let args =
                   vs.astInfo.ast
                   |> AST.getArguments (T.tid token)
                   |> List.map ~f:E.toID
                 in
                 let s = ViewFnExecution.stateFromViewState vs in
                 ViewFnExecution.fnExecutionStatus s fn id args
                 |> ViewFnExecution.executionError
                 |> Option.some)
    in
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess (ExecutedResult (DIncomplete _))
      when Option.isSome fnLoading ->
        [Html.text (Option.withDefault ~default:"" fnLoading)]
    | LoadableSuccess
        (ExecutedResult (DIncomplete (SourceId (srcTlid, srcId)) as dv))
    | LoadableSuccess
        (ExecutedResult (DError (SourceId (srcTlid, srcId), _) as dv))
      when srcId <> id || srcTlid <> vs.tlid ->
        let errType = dv |> Runtime.typeOf |> Runtime.tipe2str in
        let msg = "<" ^ errType ^ ">" in
        [ viewArrow id srcId
        ; Html.div
            [ ViewUtils.eventNoPropagation
                ~key:("lv-src-" ^ ID.toString srcId ^ TLID.toString srcTlid)
                "click"
                (fun _ -> FluidMsg (FluidFocusOnToken (srcTlid, srcId)))
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
  FluidTokenizer.ASTInfo.getToken vs.astInfo
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


let viewReturnValue
    (vs : ViewUtils.viewState) (dragEvents : ViewUtils.domEventList) :
    Types.msg Html.html =
  if CursorState.tlidOf vs.cursorState = Some vs.tlid
  then
    let id = FluidAST.toID vs.astInfo.ast in
    match Analysis.getLiveValueLoadable vs.analysisStore id with
    | LoadableSuccess (ExecutedResult dval) ->
        let isRefreshed =
          match vs.handlerProp with
          | Some {execution = Complete; _} ->
              true
          | _ ->
              false
        in
        let warningText =
          let onDefaultTrace tlid =
            match vs.traces with
            | [(tid, _)] when tid = Analysis.defaultTraceIDForTL ~tlid ->
                true
            | _ ->
                false
          in
          (* Since HTTP and userFunctions are the case where Incomplete return
           * is likely to case and error, we only want to highlight those
           * cases. *)
          match (dval, vs.tl) with
          | DIncomplete _, TLHandler h when SpecHeaders.spaceOf h.spec = HSHTTP
            ->
              Some "Your code needs to return a value in the last expression"
          | DIncomplete _, TLFunc f when onDefaultTrace f.ufTLID ->
              Some
                "This function has not yet been called - please call this function"
          | DIncomplete _, TLFunc _ ->
              Some "Your code needs to return a value in the last expression"
          | _, TLFunc f
            when not
                   (Runtime.isCompatible
                      (BlankOr.valueWithDefault TAny f.ufMetadata.ufmReturnTipe)
                      (Runtime.typeOf dval)) ->
              Some "wrong type"
          | _, TLPmFunc _
          | _, TLFunc _
          | _, TLHandler _
          | _, TLDB _
          | _, TLTipe _
          | _, TLGroup _ ->
              None
        in
        let auxText =
          warningText
          |> Option.map ~f:(fun txt ->
                 Html.span [Html.class' "msg"] [Html.text txt])
          |> Option.withDefault ~default:Vdom.noNode
        in
        let dvalString = Runtime.toRepr dval in
        let newLine1 =
          if String.contains ~substring:"\n" dvalString
          then Html.br []
          else Vdom.noNode
        in
        let newLine2 =
          if warningText <> None then Html.br [] else Vdom.noNode
        in
        let viewDval = viewDval vs.tlid dval ~canCopy:true in
        Html.div
          ( Html.classList
              [ ("return-value", true)
              ; ("refreshed", isRefreshed)
              ; ("warning", warningText <> None)
              ; ("draggable", dragEvents <> []) ]
          :: dragEvents )
          ( [Html.text "This trace returns: "; newLine1]
          @ viewDval
          @ [newLine2; auxText] )
    | _ ->
        Vdom.noNode
  else Vdom.noNode


let viewAST (vs : ViewUtils.viewState) (dragEvents : ViewUtils.domEventList) :
    Types.msg Html.html list =
  let liveValue =
    if vs.cursorState = FluidEntering vs.tlid
    then viewLiveValue vs
    else Vdom.noNode
  in
  let editorState =
    { FluidEditorView.analysisStore = vs.analysisStore
    ; ast = vs.astInfo.ast
    ; functions = vs.functions
    ; executingFunctions = vs.executingFunctions
    ; editor = MainEditor vs.tlid
    ; hoveringRefs = vs.hoveringRefs
    ; fluidState = vs.fluidState
    ; permission = vs.permission
    ; tlid = vs.tlid
    ; tokens = vs.astInfo.mainTokenInfos }
  in
  let mainEditor = FluidEditorView.view editorState in
  let returnValue = viewReturnValue vs dragEvents in
  let debugAST =
    if vs.showHandlerASTs
    then
      Html.div
        [Html.class' "debug-ast"]
        [Html.text (E.toHumanReadable (FluidAST.toExpr vs.astInfo.ast))]
    else Vdom.noNode
  in
  let secondaryEditors =
    let findRowOffestOfMainTokenWithId (flagID : ID.t) : int option =
      (* FIXME(ds) this is a giant hack to find the row offset of the corresponding
       * token in the main view for each secondary editor. This works by getting
       * the id of the split (ie, the id of the first token in the split)
       * and then looking through the main tokens [O(N)] to find one with a
       * corresponding id. This is brittle and will likely break at some point. We
       * should do something better. *)
      FluidAST.find flagID vs.astInfo.ast
      |> Option.andThen ~f:(function
             | E.EFeatureFlag (_, _, _, oldCode, _) ->
                 Some (E.toID oldCode)
             | _ ->
                 None)
      |> Option.andThen ~f:(fun oldCodeID ->
             List.find vs.astInfo.mainTokenInfos ~f:(fun ti ->
                 oldCodeID = T.tid ti.token))
      |> Option.map ~f:(fun ti -> ti.startRow)
    in
    Fluid.buildFeatureFlagEditors vs.tlid vs.astInfo.ast
    |> List.map ~f:(fun e ->
           match e with
           | NoEditor ->
               recover
                 "got NoEditor when building feature flag editors"
                 (Html.div [] [])
           | MainEditor _ ->
               recover
                 "got MainEditor when building feature flag editors"
                 (Html.div [] [])
           | FeatureFlagEditor (_, flagID) ->
               let flagIcon =
                 Html.div
                   [Html.class' "ff-icon"; Html.title "feature flag"]
                   [ViewUtils.fontAwesome "flag"]
               in
               let rowOffset =
                 flagID
                 |> findRowOffestOfMainTokenWithId
                 |> Option.withDefault ~default:0
               in
               let tokens =
                 FluidTokenizer.ASTInfo.ffTokenInfosFor flagID vs.astInfo
                 |> recoverOpt "can't find tokens for real flag" ~default:[]
               in
               let editorState =
                 { FluidEditorView.analysisStore = vs.analysisStore
                 ; ast = vs.astInfo.ast
                 ; functions = vs.functions
                 ; executingFunctions = vs.executingFunctions
                 ; editor = e
                 ; hoveringRefs = vs.hoveringRefs
                 ; fluidState = vs.fluidState
                 ; permission = vs.permission
                 ; tlid = vs.tlid
                 ; tokens }
               in
               Html.div
                 [ Html.class' "fluid-secondary-editor"
                 ; Html.styles [("top", string_of_int rowOffset ^ ".5rem")] ]
                 [flagIcon; FluidEditorView.view editorState])
  in
  mainEditor :: liveValue :: returnValue :: debugAST :: secondaryEditors


let view (vs : ViewUtils.viewState) (dragEvents : ViewUtils.domEventList) =
  [Html.div [Html.class' "fluid-ast"] (viewAST vs dragEvents)]
