open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

// Dark
module B = BlankOr

module Msg = AppTypes.Msg
type msg = AppTypes.msg
let fontAwesome = Icons.fontAwesome

type viewProps = ViewUtils.viewProps

let viewTypeName = (vp: viewProps, t: PT.UserType.t): Html.html<msg> => {
  let nameField = ViewBlankOr.viewText(
    ~enterable=true,
    ~classes=list{"ut-name"},
    TypeName,
    vp,
    B.fromStringID(t.name, t.nameID),
  )

  Html.div(list{Attrs.class'("typetitle")}, list{nameField})
}

let viewFieldName = (~classes: list<string>, vp: viewProps, v: BlankOr.t<string>): Html.html<msg> =>
  ViewBlankOr.viewText(~enterable=true, ~classes, TypeFieldName, vp, v)

let viewFieldType = (~classes: list<string>, vp: viewProps, v: BlankOr.t<DType.t>): Html.html<
  msg,
> => ViewBlankOr.viewType(~enterable=true, ~classes, TypeFieldType, vp, v)

let viewKillFieldBtn = (t: PT.UserType.t, field: PT.UserType.RecordField.t): Html.html<msg> =>
  Html.div(
    list{
      Attrs.class'("field-btn allowed"),
      EventListeners.eventNoPropagation(
        ~key="dutf-" ++ TLID.toString(t.tlid) ++ "-" ++ field.nameID->ID.toString,
        "click",
        _ => Msg.DeleteUserTypeField(t.tlid, field),
      ),
    },
    list{fontAwesome("times-circle")},
  )

let viewTypeField = (
  vp: viewProps,
  t: PT.UserType.t,
  fieldCount: int,
  field: PT.UserType.RecordField.t,
): Html.html<msg> => {
  let button = if fieldCount > 1 && vp.permission == Some(ReadWrite) {
    viewKillFieldBtn(t, field)
  } else {
    Vdom.noNode
  }

  let row = list{
    viewFieldName(vp, ~classes=list{"name"}, BlankOr.fromStringID(field.name, field.nameID)),
    viewFieldType(vp, ~classes=list{"type"}, BlankOr.fromOptionID(field.typ, field.typeID)),
    button,
  }

  Html.div(list{Attrs.class'("field")}, row)
}

let viewUserType = (vp: viewProps, t: PT.UserType.t): Html.html<msg> =>
  switch t.definition {
  | Record(fields) =>
    let nameDiv = viewTypeName(vp, t)
    let fieldDivs = {
      let fieldCount = List.length(fields)
      Html.div(list{Attrs.class'("fields")}, List.map(~f=viewTypeField(vp, t, fieldCount), fields))
    }

    Html.div(list{Attrs.class'("user-type")}, list{nameDiv, fieldDivs})
  }
