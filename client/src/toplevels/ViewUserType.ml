open Prelude

(* Dark *)
module B = BlankOr

let fontAwesome = ViewUtils.fontAwesome

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let wc = ViewBlankOr.wc

let viewTipeName (vs : viewState) (t : userTipe) : msg Html.html =
  let nameField =
    let c = [wc "ut-name"] in
    ViewBlankOr.viewText ~enterable:true TypeName vs c t.utName
  in
  Html.div [Html.class' "typetitle"] [nameField]


let viewFieldName
    (vs : viewState) (configs : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  ViewBlankOr.viewText ~enterable:true TypeFieldName vs configs v


let viewFieldType
    (vs : viewState) (configs : htmlConfig list) (v : tipe blankOr) :
    msg Html.html =
  ViewBlankOr.viewTipe ~enterable:true TypeFieldTipe vs configs v


let viewKillFieldBtn (t : userTipe) (field : userRecordField) : msg Html.html =
  Html.div
    [ Html.class' "field-btn allowed"
    ; ViewUtils.eventNoPropagation
        ~key:
          ( "dutf-"
          ^ TLID.toString t.utTLID
          ^ "-"
          ^ (field.urfName |> B.toID |> ID.toString) )
        "click"
        (fun _ -> DeleteUserTypeField (t.utTLID, field)) ]
    [fontAwesome "times-circle"]


let viewTipeField
    (vs : viewState) (t : userTipe) (fieldCount : int) (field : userRecordField)
    : msg Html.html =
  let button =
    if fieldCount > 1 && vs.permission = Some ReadWrite
    then viewKillFieldBtn t field
    else Vdom.noNode
  in
  let row =
    [ viewFieldName vs [wc "name"] field.urfName
    ; viewFieldType vs [wc "type"] field.urfTipe
    ; button ]
  in
  Html.div [Html.class' "field"] row


let viewUserTipe (vs : viewState) (t : userTipe) : msg Html.html =
  match t.utDefinition with
  | UTRecord fields ->
      let nameDiv = viewTipeName vs t in
      let fieldDivs =
        let fieldCount = List.length fields in
        Html.div
          [Html.class' "fields"]
          (List.map ~f:(viewTipeField vs t fieldCount) fields)
      in
      Html.div [Html.class' "user-type"] [nameDiv; fieldDivs]
