open Prelude

(* Dark *)
module B = BlankOr

let fontAwesome = ViewUtils.fontAwesome

type viewState = ViewUtils.viewState

let viewTipeName (vs : viewState) (t : userTipe) : msg Html.html =
  let nameField =
    ViewBlankOr.viewText
      ~enterable:true
      ~classes:["ut-name"]
      TypeName
      vs
      t.utName
  in
  Html.div [Html.class' "typetitle"] [nameField]


let viewFieldName ~(classes : string list) (vs : viewState) (v : string blankOr)
    : msg Html.html =
  ViewBlankOr.viewText ~enterable:true ~classes TypeFieldName vs v


let viewFieldType ~(classes : string list) (vs : viewState) (v : tipe blankOr) :
    msg Html.html =
  ViewBlankOr.viewTipe ~enterable:true ~classes TypeFieldTipe vs v


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
    [ viewFieldName vs ~classes:["name"] field.urfName
    ; viewFieldType vs ~classes:["type"] field.urfTipe
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
