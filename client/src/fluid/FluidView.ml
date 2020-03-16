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
      ViewUtils.fnForToken vs.fluidState token
      |> Option.andThen ~f:(fun fn ->
             match fn.fnPreviewSafety with
             | Safe ->
                 None
             | Unsafe ->
                 let id = T.tid token in
                 let args =
                   AST.getArguments (T.tid token) vs.ast |> List.map ~f:E.toID
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
    | LoadableSuccess (ExecutedResult (DIncomplete (SourceId srcId) as dv))
    | LoadableSuccess (ExecutedResult (DError (SourceId srcId, _) as dv))
      when srcId <> id ->
        let errType = dv |> Runtime.typeOf |> Runtime.tipe2str in
        let msg = "<" ^ errType ^ ">" in
        [ viewArrow id srcId
        ; Html.div
            [ ViewUtils.eventNoPropagation
                ~key:("lv-src-" ^ ID.toString srcId)
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
  Fluid.getToken vs.ast vs.fluidState
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


let viewReturnValue (vs : ViewUtils.viewState) : Types.msg Html.html =
  if CursorState.tlidOf vs.cursorState = Some vs.tlid
  then
    let id = FluidAST.toID vs.ast in
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


let viewAST (vs : ViewUtils.viewState) : Types.msg Html.html list =
  let liveValue =
    if vs.cursorState = FluidEntering vs.tlid
    then viewLiveValue vs
    else Vdom.noNode
  in
  let editorState =
    { FluidEditorView.analysisStore = vs.analysisStore
    ; ast = vs.ast
    ; executingFunctions = vs.executingFunctions
    ; editor = MainEditor
    ; hoveringRefs = vs.hoveringRefs
    ; fluidState = vs.fluidState
    ; permission = vs.permission
    ; tlid = vs.tlid
    ; tokens = vs.tokens }
  in
  let mainEditor = FluidEditorView.view editorState in
  let returnValue = viewReturnValue vs in
  let secondaryEditors =
    let findRowOffestOfMainTokenWithId (target : ID.t) : int option =
      (* FIXME(ds) this is a giant hack to find the row offset of the corresponding
       * token in the main view for each secondary editor. This works by getting
       * the id of the split (ie, the id of the first token in the split)
       * and then looking through the main tokens [O(N)] to find one with a
       * corresponding id. This is brittle and will likely break at some point. We
       * should do something better. *)
      List.find vs.tokens ~f:(fun ti -> target = T.tid ti.token)
      |> Option.map ~f:(fun ti -> ti.startRow)
    in
    Fluid.buildFeatureFlagEditors vs.ast
    |> List.map ~f:(fun e ->
           match e with
           | MainEditor ->
               recover
                 "got MainEditor when building feature flag editors"
                 (Html.div [] [])
           | FeatureFlagEditor expressionId ->
               let flagIcon =
                 Html.div [Html.class' "ff-icon"] [ViewUtils.fontAwesome "flag"]
               in
               let rowOffset =
                 expressionId
                 |> findRowOffestOfMainTokenWithId
                 |> Option.withDefault ~default:0
               in
               let tokens = FluidPrinter.tokensForEditor e vs.ast in
               let editorState =
                 { FluidEditorView.analysisStore = vs.analysisStore
                 ; ast = vs.ast
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
                 ; Html.styles [("top", string_of_int rowOffset ^ "rem")] ]
                 [flagIcon; FluidEditorView.view editorState])
  in
  mainEditor :: liveValue :: returnValue :: secondaryEditors


let view (vs : ViewUtils.viewState) =
  [Html.div [Html.class' "fluid-ast"] (viewAST vs)]
