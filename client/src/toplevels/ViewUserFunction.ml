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

type exeFunction =
  | CanExecute of traceID * dval list
  | CannotExecute of string
  | IsExecuting

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
  let exeStatus =
    if vs.isExecuting
    then IsExecuting
    else
      (* Attempts to get trace inputValues for this function *)
      match Analysis.selectedTrace vs.tlTraceIDs vs.traces vs.tlid with
      | Some (traceID, Ok td) ->
          let args = UserFunctions.inputToArgs fn td.input in
          (* If any of the args is Incomplete/Error then we don't want to bother allowing this function to be executed *)
          if List.any
               ~f:(fun dv -> match dv with DIncomplete _ -> true | _ -> false)
               args
          then CannotExecute "Cannot run function with incomplete arguments"
          else if List.any
                    ~f:(fun dv -> match dv with DError _ -> true | _ -> false)
                    args
          then
            CannotExecute "Cannot run function with arguments that has an error"
          else CanExecute (traceID, args)
      | _ ->
          CannotExecute "Cannot run function with no trace data"
  in
  let events =
    (* If function is ready for re-execution, attach onClick listener *)
    match (fn.ufMetadata.ufmName, exeStatus) with
    | F (_, fnName), CanExecute (traceID, args) ->
        ViewUtils.eventNoPropagation
          ~key:("run-fun" ^ "-" ^ showTLID fn.ufTLID ^ "-" ^ traceID)
          "click"
          (fun _ ->
            ExecuteFunctionFromWithin
              { efpTLID = fn.ufTLID
              ; efpCallerID = FluidExpression.id fn.ufAST
              ; efpTraceID = traceID
              ; efpFnName = fnName
              ; efpArgs = args })
    | _ ->
        Vdom.noProp
  in
  let title =
    match exeStatus with
    | CannotExecute msg ->
        msg
    | CanExecute _ ->
        "Click to execute function"
    | IsExecuting ->
        "Function is executing"
  in
  Html.div
    [ Html.classList
        [ ("execution-button", true)
        ; ( "is-ready"
          , vs.permission = Some ReadWrite
            && match exeStatus with CanExecute _ -> true | _ -> false )
        ; ("is-executing", exeStatus = IsExecuting) ]
    ; events
    ; Html.title title ]
    [fontAwesome "redo"]


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
