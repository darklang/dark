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
      Html.div
        ~key:"add-param-col"
        [ Html.class' "col new-parameter"
        ; ViewUtils.eventNoPropagation
            ~key:("aufp-" ^ showTLID fn.ufTLID)
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
            "Cannot delete because is function is used in your code base. Use references on the right to depreciate usages of this function."
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
      ; Html.div [Html.class' "fn-actions"] [menuView] ]
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
