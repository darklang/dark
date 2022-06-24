open Prelude

// Dark
module B = BlankOr

let fontAwesome = ViewUtils.fontAwesome

type viewProps = ViewUtils.viewProps

let viewTipeName = (vp: viewProps, t: userTipe): Html.html<msg> => {
  let nameField = ViewBlankOr.viewText(
    ~enterable=true,
    ~classes=list{"ut-name"},
    TypeName,
    vp,
    t.utName,
  )

  Html.div(list{Html.class'("typetitle")}, list{nameField})
}

let viewFieldName = (~classes: list<string>, vp: viewProps, v: blankOr<string>): Html.html<msg> =>
  ViewBlankOr.viewText(~enterable=true, ~classes, TypeFieldName, vp, v)

let viewFieldType = (~classes: list<string>, vp: viewProps, v: blankOr<tipe>): Html.html<msg> =>
  ViewBlankOr.viewTipe(~enterable=true, ~classes, TypeFieldTipe, vp, v)

let viewKillFieldBtn = (t: userTipe, field: userRecordField): Html.html<msg> =>
  Html.div(
    list{
      Html.class'("field-btn allowed"),
      ViewUtils.eventNoPropagation(
        ~key="dutf-" ++
        (TLID.toString(t.utTLID) ++
        ("-" ++ (field.urfName |> B.toID |> ID.toString))),
        "click",
        _ => DeleteUserTypeField(t.utTLID, field),
      ),
    },
    list{fontAwesome("times-circle")},
  )

let viewTipeField = (
  vp: viewProps,
  t: userTipe,
  fieldCount: int,
  field: userRecordField,
): Html.html<msg> => {
  let button = if fieldCount > 1 && vp.permission == Some(ReadWrite) {
    viewKillFieldBtn(t, field)
  } else {
    Vdom.noNode
  }

  let row = list{
    viewFieldName(vp, ~classes=list{"name"}, field.urfName),
    viewFieldType(vp, ~classes=list{"type"}, field.urfTipe),
    button,
  }

  Html.div(list{Html.class'("field")}, row)
}

let viewUserTipe = (vp: viewProps, t: userTipe): Html.html<msg> =>
  switch t.utDefinition {
  | UTRecord(fields) =>
    let nameDiv = viewTipeName(vp, t)
    let fieldDivs = {
      let fieldCount = List.length(fields)
      Html.div(list{Html.class'("fields")}, List.map(~f=viewTipeField(vp, t, fieldCount), fields))
    }

    Html.div(list{Html.class'("user-type")}, list{nameDiv, fieldDivs})
  }
