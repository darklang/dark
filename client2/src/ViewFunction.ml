open Tea
open! Porting
module Attrs = Html.Attributes
open Types
open ViewBlankOr
open ViewUtils

let viewFunction (vs : viewState) (fn : userFunction) : msg Html.html =
  Html.div
    [Attrs.class_ "user-fn-toplevel"]
    [ Html.div [Attrs.class_ "metadata"] [viewMetadata vs fn]
    ; Html.div [Attrs.class_ "ast"] [ViewCode.viewExpr 0 vs [] fn.ast] ]

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
  let freeVariables = AST.freeVariables uf.ast |> List.map Tuple.second in
  let canDeleteParameter pname = List.member pname freeVariables |> not in
  let buttonContent allowed =
    if allowed then
      Html.div
        [ Attrs.class_ "parameter-btn allowed"
        ; eventNoPropagation "click" (fun _ ->
              DeleteUserFunctionParameter (uf, p) ) ]
        [fontAwesome "times-circle"]
    else
      Html.div
        [ Attrs.class_ "parameter-btn disallowed"
        ; Attrs.title
            "Can't delete parameter because it is used in the function body" ]
        [fontAwesome "times-circle"]
  in
  match p.name with
  | F (_, pname) -> buttonContent (canDeleteParameter pname)
  | _ -> buttonContent true

let viewMetadata (vs : viewState) (fn : userFunction) : msg Html.html =
  let namediv =
    Html.div [Attrs.class_ "ufn-name"]
      [viewUserFnName vs [wc "fn-name-content"] fn.metadata.name]
  in
  let coldivs =
    fn.metadata.parameters
    |> List.map (fun p ->
           Html.div [Attrs.class_ "col"]
             [ viewParamName vs [wc "name"] p.name
             ; viewParamTipe vs [wc "type"] p.ufParamTipe
             ; viewKillParameterBtn vs fn p ] )
  in
  Html.div [Attrs.class_ "user-fn"] (namediv :: coldivs)
