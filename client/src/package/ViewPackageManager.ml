open Prelude

(* Dark *)
module B = BlankOr

type viewState = ViewUtils.viewState

let fontAwesome = ViewUtils.fontAwesome

let viewParam (vs : viewState) (index : int) (p : packageFnParameter) :
    msg Html.html list =
  let viewParamName =
    ViewBlankOr.viewText
      ~enterable:false
      ~classes:["name"]
      ParamName
      vs
      (BlankOr.newF p.name)
  in
  let viewParamTipe =
    ViewBlankOr.viewTipe
      ~classes:["type"]
      ~enterable:false
      ParamTipe
      vs
      (BlankOr.newF p.tipe)
  in
  [ Html.div
      ~unique:p.name
      [ Html.classList [("col param", true)]
      ; Vdom.attribute "" "data-pos" (string_of_int index) ]
      [viewParamName; viewParamTipe] ]


let viewParams (fn : packageFn) (vs : viewState) : msg Html.html list =
  let params =
    fn.parameters |> List.indexedMap ~f:(viewParam vs) |> List.flatten
  in
  params


let viewUserFnName ~(classes : string list) (vs : viewState) (v : string) :
    msg Html.html =
  let v = BlankOr.newF v in
  ViewBlankOr.viewText ~classes ~enterable:true FnName vs v


let viewMetadata (vs : viewState) (fn : packageFn) : msg Html.html =
  let titleRow =
    Html.div
      [Html.class' "spec-header"]
      [ ViewUtils.darkIcon "fn"
      ; viewUserFnName vs ~classes:["fn-name-content"] fn.fnname ]
  in
  let paramRows =
    Html.div [Html.id "fnparams"; Html.class' "params"] (viewParams fn vs)
  in
  Html.div [Html.class' "fn-header"] [titleRow; paramRows]


let view (vs : viewState) (fn : packageFn) : msg Html.html =
  let mainEditor =
    let editorState =
      { FluidEditorView.analysisStore = vs.analysisStore
      ; ast = vs.ast
      ; functions = vs.functions
      ; executingFunctions = vs.executingFunctions
      ; editor = MainEditor
      ; hoveringRefs = vs.hoveringRefs
      ; fluidState = vs.fluidState
      ; permission = Some Read
      ; tlid = vs.tlid
      ; tokens = vs.tokens }
    in
    let editor = FluidEditorView.view editorState in
    Html.div [Html.class' "fluid-ast"] [editor]
  in
  Html.div
    [Html.class' "user-fn-toplevel"]
    [ Html.div [Html.class' "metadata"] [viewMetadata vs fn]
    ; Html.div [Html.class' "function-body expand"] [mainEditor] ]
