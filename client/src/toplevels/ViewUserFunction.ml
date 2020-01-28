open Prelude

(* Dark *)
module B = BlankOr

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let viewText = ViewBlankOr.viewText

let wc = ViewBlankOr.wc

let enterable = ViewBlankOr.Enterable

let viewUserFnName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  viewText FnName vs ((enterable :: idConfigs) @ c) v


let viewParamName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  viewText ParamName vs ((enterable :: idConfigs) @ c) v


let viewParamTipe (vs : viewState) (c : htmlConfig list) (v : tipe blankOr) :
    msg Html.html =
  ViewBlankOr.viewTipe ParamTipe vs ((enterable :: idConfigs) @ c) v


let viewKillParameterBtn (uf : userFunction) (p : userFunctionParameter) :
    msg Html.html =
  let freeVariables =
    uf.ufAST |> AST.freeVariables |> List.map ~f:Tuple2.second
  in
  let canDeleteParameter pname =
    List.member ~value:pname freeVariables |> not
  in
  let buttonContent allowed =
    if allowed
    then
      Html.div
        [ Html.class' "parameter-btn allowed"
        ; ViewUtils.eventNoPropagation
            ~key:
              ( "dufp-"
              ^ showTLID uf.ufTLID
              ^ "-"
              ^ (p.ufpName |> B.toID |> showID) )
            "click"
            (fun _ -> DeleteUserFunctionParameter (uf.ufTLID, p)) ]
        [fontAwesome "times-circle"]
    else
      Html.div
        [ Html.class' "parameter-btn disallowed"
        ; Html.title
            "Can't delete parameter because it is used in the function body" ]
        [fontAwesome "times-circle"]
  in
  match p.ufpName with
  | F (_, pname) ->
      buttonContent (canDeleteParameter pname)
  | _ ->
      buttonContent true

let viewExecuteBtn (vs : viewState) (fn : userFunction) : msg Html.html =
  let readyClass = match vs.analysisStore with
    | LoadableSuccess _ -> "ready"
    | LoadableNotInitialized -> "no-data"
    | LoadableLoading _ -> "wait"
    | LoadableError _ -> "error"
  in
  let events =
    ViewUtils.eventNoPropagation
      ~key:("run-fun" ^ "-" ^ showTLID fn.ufTLID)
      "click"
      (fun _ -> Debug.loG "exe fun" vs.analysisStore; IgnoreMsg )
  in
  Html.div [Html.class' ("execution-button " ^ readyClass); events] [fontAwesome "redo"]
  (*
  if vs.permission = Some ReadWrite
  then
    let hasData =
      Analysis.selectedTrace vs.tlTraceIDs vs.traces vs.tlid
      |> Option.andThen ~f:(fun trace_id ->
              List.find ~f:(fun (id, _) -> id = trace_id) vs.traces
              |> Option.andThen ~f:(fun (_, data) -> data |> Result.toOption))
      |> Option.is_some
    in
    let classes =
      Html.classList
        [ ("handler-trigger", true)
        ; ("is-executing", handlerIsExecuting vs)
        ; ("inactive", not hasData)
        ; ("complete", handlerIsExeComplete vs)
        ; ("failed", handlerIsExeFail vs) ]
    in
    let attrs =
      if hasData
      then
        [ Html.title "Replay this execution"
        ; ViewUtils.eventNoPropagation
            ~key:("lh" ^ "-" ^ showTLID vs.tlid)
            "click"
            (fun _ -> TriggerHandler vs.tlid)
        ; ViewUtils.onAnimationEnd
            ~key:("exe" ^ "-" ^ showTLID vs.tlid)
            ~listener:(fun name ->
              if name = "fadeIn"
              then SetHandlerExeIdle vs.tlid
              else IgnoreMsg) ]
      else [Html.title "Need input data to replay execution"]
    in
    Html.div (classes :: attrs) [fontAwesome "redo"]
  else Vdom.noNode
  *)

let viewParam (fn : userFunction) (vs : viewState) (p : userFunctionParameter) :
    msg Html.html =
  Html.div
    [Html.class' "col"]
    [ ( if vs.permission = Some ReadWrite
      then viewKillParameterBtn fn p
      else Vdom.noNode )
    ; viewParamName vs [wc "name"] p.ufpName
    ; Html.div [Html.class' "param-divider"] [Html.text ":"]
    ; viewParamTipe vs [wc "type"] p.ufpTipe ]


let viewMetadata (vs : viewState) (fn : userFunction) : msg Html.html =
  let addParamBtn =
    if vs.permission = Some ReadWrite
    then
      let strTLID = showTLID fn.ufTLID in
      Html.div
        ~key:("add-param-col-" ^ strTLID)
        [ Html.class' "col new-parameter"
        ; ViewUtils.eventNoPropagation
            ~key:("aufp-" ^ strTLID)
            "click"
            (fun _ -> AddUserFunctionParameter fn.ufTLID) ]
        [ Html.div
            [Html.class' "parameter-btn allowed add"]
            [fontAwesome "plus-circle"]
        ; Html.span [Html.class' "btn-label"] [Html.text "add new parameter"] ]
    else Vdom.noNode
  in
  let menuView =
    let delAct : TLMenu.menuItem =
      let disableMsg =
        if not (List.isEmpty vs.usedInRefs)
        then
          Some
            "Cannot delete this function as it is used in your code base. Use the references on the right to find and change this function's callers, after which you'll be able to delete it."
        else None
      in
      { title = "Delete Function "
      ; key = "del-ufn-"
      ; icon = Some "times"
      ; action = (fun _ -> DeleteUserFunction fn.ufTLID)
      ; disableMsg }
    in
    Html.div [Html.class' "menu"] [TLMenu.viewMenu vs.menuState vs.tlid [delAct]]
  in
  let titleRow =
    Html.div
      [Html.class' "spec-header"]
      [ ViewUtils.svgIconFn "#7dcac0"
      ; viewUserFnName vs [wc "fn-name-content"] fn.ufMetadata.ufmName
      ; Html.div [Html.class' "fn-actions"] [viewExecuteBtn vs fn; menuView] ]
  in
  let paramRows =
    let params = fn.ufMetadata.ufmParameters |> List.map ~f:(viewParam fn vs) in
    Html.div [Html.class' "params"] (params @ [addParamBtn])
  in
  Html.div [Html.class' "fn-header"] [titleRow; paramRows]


let view (vs : viewState) (fn : userFunction) : msg Html.html =
  Html.div
    [Html.class' "user-fn-toplevel"]
    [ Html.div [Html.class' "metadata"] [viewMetadata vs fn]
    ; Html.div [Html.class' "function-body expand"] (FluidView.view vs fn.ufAST)
    ]
