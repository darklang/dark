open Prelude

(* Dark *)
module B = BlankOr

let fontAwesome = ViewUtils.fontAwesome

type viewProps = ViewUtils.viewProps

let viewTipeName (vp : viewProps) (t : userTipe) : msg Html.html =
  let nameField =
    ViewBlankOr.viewText
      ~enterable:true
      ~classes:["ut-name"]
      TypeName
      vp
      t.utName
  in
  Html.div [Html.class' "typetitle"] [nameField]


let viewFieldName ~(classes : string list) (vp : viewProps) (v : string blankOr)
    : msg Html.html =
  ViewBlankOr.viewText ~enterable:true ~classes TypeFieldName vp v


let viewFieldType ~(classes : string list) (vp : viewProps) (v : tipe blankOr) :
    msg Html.html =
  ViewBlankOr.viewTipe ~enterable:true ~classes TypeFieldTipe vp v


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
    (vp : viewProps) (t : userTipe) (fieldCount : int) (field : userRecordField)
    : msg Html.html =
  let button =
    if fieldCount > 1 && vp.permission = Some ReadWrite
    then viewKillFieldBtn t field
    else Vdom.noNode
  in
  let row =
    [ viewFieldName vp ~classes:["name"] field.urfName
    ; viewFieldType vp ~classes:["type"] field.urfTipe
    ; button ]
  in
  Html.div [Html.class' "field"] row


let viewUserTipe (vp : viewProps) (t : userTipe) : msg Html.html =
  match t.utDefinition with
  | UTRecord fields ->
      let nameDiv = viewTipeName vp t in
      let fieldDivs =
        let fieldCount = List.length fields in
        Html.div
          [Html.class' "fields"]
          (List.map ~f:(viewTipeField vp t fieldCount) fields)
      in
      Html.div [Html.class' "user-type"] [nameDiv; fieldDivs]
