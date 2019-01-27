open Tc
open Types
open Prelude

(* Dark *)
module B = Blank

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let viewText = ViewBlankOr.viewText

let wc = ViewBlankOr.wc

let text = ViewBlankOr.text

let div = ViewBlankOr.div

let nested = ViewBlankOr.nested

let atom = ViewBlankOr.atom

let keyword = ViewBlankOr.keyword

let enterable = ViewBlankOr.Enterable

let viewUserFnName (vs : viewState) (c : htmlConfig list) (v : string blankOr)
    : msg Html.html =
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
    AST.freeVariables uf.ufAST |> List.map ~f:Tuple2.second
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
            (fun _ -> DeleteUserFunctionParameter (uf, p)) ]
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


let viewMetadata (vs : viewState) (fn : userFunction) : msg Html.html =
  let namediv =
    Html.div
      [Html.class' "ufn-name"]
      [viewUserFnName vs [wc "fn-name-content"] fn.ufMetadata.ufmName]
  in
  let coldivs =
    fn.ufMetadata.ufmParameters
    |> List.map ~f:(fun p ->
           Html.div
             [Html.class' "col"]
             [ viewParamName vs [wc "name"] p.ufpName
             ; viewParamTipe vs [wc "type"] p.ufpTipe
             ; viewKillParameterBtn fn p ] )
  in
  Html.div [Html.class' "user-fn"] (namediv :: coldivs)


let viewFunction (vs : viewState) (fn : userFunction) : msg Html.html =
  Html.div
    [Html.class' "user-fn-toplevel"]
    [ Html.div [Html.class' "metadata"] [viewMetadata vs fn]
    ; Html.div [Html.class' "ast"] [ViewCode.viewExpr 0 vs [] fn.ufAST] ]
