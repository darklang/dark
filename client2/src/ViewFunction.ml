open Tea
open! Porting
module Attrs = Html.Attributes
open Types
open ViewBlankOr
open ViewUtils

let viewFunction (vs : viewState) (fn : userFunction) : msg Html.html =
  Html.div
    [Html.class' "user-fn-toplevel"]
    [ Html.div [Html.class' "metadata"] [viewMetadata vs fn]
    ; Html.div [Html.class' "ast"] [ViewCode.viewExpr 0 vs [] fn.ufAST] ]

let viewUserFnName (vs : viewState) (c : htmlConfig list) (v : string blankOr)
    : msg Html.html =
  viewText FnName vs (idConfigs ^ c) v

let viewParamName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  viewText ParamName vs (idConfigs ^ c) v

let viewParamTipe (vs : viewState) (c : htmlConfig list) (v : tipe blankOr) :
    msg Html.html =
  viewTipe ParamTipe vs (idConfigs ^ c) v

let viewKillParameterBtn (vs : viewState) (uf : userFunction)
    (p : userFunctionParameter) : msg Html.html =
  let freeVariables = AST.freeVariables uf.ufAST |> List.map Tuple.second in
  let canDeleteParameter pname = List.member pname freeVariables |> not in
  let buttonContent allowed =
    if allowed then
      Html.div
        [ Html.class' "parameter-btn allowed"
        ; eventNoPropagation "click" (fun _ ->
              DeleteUserFunctionParameter (uf, p) ) ]
        [fontAwesome "times-circle"]
    else
      Html.div
        [ Html.class' "parameter-btn disallowed"
        ; Attrs.title
            "Can't delete parameter because it is used in the function body" ]
        [fontAwesome "times-circle"]
  in
  match p.ufpName with
  | F (_, pname) -> buttonContent (canDeleteParameter pname)
  | _ -> buttonContent true

let viewMetadata (vs : viewState) (fn : userFunction) : msg Html.html =
  let namediv =
    Html.div [Html.class' "ufn-name"]
      [viewUserFnName vs [wc "fn-name-content"] fn.ufMetadata.ufmName]
  in
  let coldivs =
    fn.ufMetadata.ufmParameters
    |> List.map (fun p ->
           Html.div [Html.class' "col"]
             [ viewParamName vs [wc "name"] p.ufpName
             ; viewParamTipe vs [wc "type"] p.ufpTipe
             ; viewKillParameterBtn vs fn p ] )
  in
  Html.div [Html.class' "user-fn"] (namediv :: coldivs)
