open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

// Dark
module B = BlankOr

type msg = AppTypes.msg
type viewProps = ViewUtils.viewProps

type domEventList = ViewUtils.domEventList
type dbStats = AnalysisTypes.dbStats

let fontAwesome = Icons.fontAwesome

let viewDbCount = (stats: dbStats): Html.html<msg> =>
  Html.div(
    list{Attrs.class'("db db-count")},
    list{
      Html.span(
        list{Attrs.class'("dbcount-txt")},
        list{Html.text("# of Entries: " ++ string_of_int(stats.count))},
      ),
    },
  )

let viewDbLatestEntry = (stats: dbStats): Html.html<msg> => {
  let title = Html.div(
    list{Attrs.class'("title")},
    list{
      Html.span(
        list{Attrs.classList(list{("label", true), ("show", Option.isSome(stats.example))})},
        list{Html.text("Latest Entry:")},
      ),
    },
  )

  let exampleHtml = switch stats.example {
  | Some(example, key) =>
    Html.div(
      list{Attrs.class'("dbexample")},
      list{
        Html.div(list{Attrs.class'("key")}, list{Html.text(key ++ ":")}),
        Html.div(list{Attrs.class'("value")}, list{Html.text(Runtime.toRepr(example))}),
      },
    )
  | None => Vdom.noNode
  }

  Html.div(list{Attrs.class'("db db-liveVal")}, list{title, exampleHtml})
}

let viewDBData = (vp: viewProps, db: PT.DB.t): Html.html<msg> =>
  switch Map.get(~key=TLID.toString(db.tlid), vp.dbStats) {
  | Some(stats) if CursorState.tlidOf(vp.cursorState) == Some(db.tlid) =>
    let liveVal = viewDbLatestEntry(stats)
    let count = viewDbCount(stats)
    Html.div(list{Attrs.class'("dbdata")}, list{count, liveVal})
  | _ => Vdom.noNode
  }

let viewDBHeader = (vp: viewProps, db: PT.DB.t): list<Html.html<msg>> => {
  let typeView = Html.span(
    list{Attrs.class'("toplevel-type")},
    list{fontAwesome("database"), Html.text("DB")},
  )

  let titleView = {
    let nameField = if vp.dbLocked {
      Html.text(db.name)
    } else {
      let blankOr = B.fromStringID(db.name, db.nameID)
      ViewBlankOr.viewText(~enterable=true, ~classes=list{"dbname"}, DBName, vp, blankOr)
    }

    Html.span(
      list{Attrs.class'("toplevel-name")},
      list{
        nameField,
        Html.span(
          list{Attrs.class'("version")},
          list{Html.text(".v" ++ string_of_int(db.version))},
        ),
      },
    )
  }

  let menuView = {
    let delAct: TLMenu.menuItem = {
      let disableMsg = if vp.dbLocked {
        Some("Cannot delete due to data inside")
      } else if !List.isEmpty(vp.usedInRefs) {
        Some("Cannot delete because your code refers to this DB")
      } else {
        None
      }

      {
        title: "Delete",
        key: "del-db-",
        icon: Some("times"),
        action: _ => ToplevelDelete(vp.tlid),
        disableMsg: disableMsg,
      }
    }

    Html.div(list{Attrs.class'("menu")}, list{TLMenu.viewMenu(vp.menuState, vp.tlid, list{delAct})})
  }

  list{typeView, titleView, menuView}
}

let viewDBColName = (
  ~classes: list<string>,
  vp: viewProps,
  name: option<string>,
  nameID: ID.t,
): Html.html<msg> => {
  let enterable = name == None || !vp.dbLocked
  let v = B.fromOptionID(name, nameID)
  ViewBlankOr.viewText(~enterable, ~classes, DBColName, vp, v)
}

let viewDBColType = (
  ~classes: list<string>,
  vp: viewProps,
  typ: option<DType.t>,
  typeID: ID.t,
): Html.html<msg> => {
  let enterable = typ == None || !vp.dbLocked
  let typ = Option.map(~f=DType.tipe2str, typ)
  let v = B.fromOptionID(typ, typeID)
  ViewBlankOr.viewText(~enterable, ~classes, DBColType, vp, v)
}

let viewDBCol = (vp: viewProps, isMigra: bool, tlid: TLID.t, col: PT.DB.Col.t): Html.html<msg> => {
  let deleteButton = if (
    vp.permission == Some(ReadWrite) &&
    (isMigra || !vp.dbLocked) &&
    (Option.isSome(col.name) || Option.isSome(col.typ))
  ) {
    Html.div(
      list{
        Attrs.class'("delete-col"),
        ViewUtils.eventNoPropagation(
          ~key="dcidb-" ++ (TLID.toString(tlid) ++ ("-" ++ (col.nameID |> ID.toString))),
          "click",
          _ => DeleteColInDB(tlid, col.nameID),
        ),
      },
      list{fontAwesome("minus-circle")},
    )
  } else {
    Vdom.noNode
  }

  let row = list{
    viewDBColName(vp, ~classes=list{"name"}, col.name, col.nameID),
    viewDBColType(vp, ~classes=list{"type"}, col.typ, col.typeID),
  }

  Html.div(
    list{Attrs.classList(list{("col", true), ("has-empty", deleteButton == Vdom.noNode)})},
    list{deleteButton, ...row},
  )
}

let viewDB = (vp: viewProps, db: PT.DB.t, dragEvents: domEventList): list<Html.html<msg>> => {
  let lockClass = if vp.dbLocked {
    "lock"
  } else {
    "unlock"
  }

  let cols = if !(vp.permission == Some(ReadWrite)) || vp.dbLocked {
    List.filter(~f=col => Option.isSome(col.name) && Option.isSome(col.typ), db.cols)
  } else {
    db.cols
  }

  let keyView = Html.div(
    list{Attrs.class'("col key")},
    list{Html.text("All entries are identified by a unique string `key`.")},
  )

  let coldivs = List.map(~f=viewDBCol(vp, false, db.tlid), cols)
  let data = viewDBData(vp, db)
  let headerView = Html.div(list{Attrs.class'("spec-header " ++ lockClass)}, viewDBHeader(vp, db))

  Belt.List.concatMany([
    list{Html.div(list{Attrs.class'("db"), ...dragEvents}, list{headerView, keyView, ...coldivs})},
    list{data},
  ])
}
