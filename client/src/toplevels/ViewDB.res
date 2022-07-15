open Prelude

// Dark
module B = BlankOr

type viewProps = ViewUtils.viewProps

type domEventList = ViewUtils.domEventList

let fontAwesome = ViewUtils.fontAwesome

let dbName2String = (name: blankOr<string>): string => B.valueWithDefault("", name)

let viewDbCount = (stats: dbStats): Html.html<msg> =>
  Html.div(
    list{Html.class'("db db-count")},
    list{
      Html.span(
        list{Html.class'("dbcount-txt")},
        list{Html.text("# of Entries: " ++ string_of_int(stats.count))},
      ),
    },
  )

let viewDbLatestEntry = (stats: dbStats): Html.html<msg> => {
  let title = Html.div(
    list{Html.class'("title")},
    list{
      Html.span(
        list{Html.classList(list{("label", true), ("show", Option.isSome(stats.example))})},
        list{Html.text("Latest Entry:")},
      ),
    },
  )

  let exampleHtml = switch stats.example {
  | Some(example, key) =>
    Html.div(
      list{Html.class'("dbexample")},
      list{
        Html.div(list{Html.class'("key")}, list{Html.text(key ++ ":")}),
        Html.div(list{Html.class'("value")}, list{Html.text(Runtime.toRepr(example))}),
      },
    )
  | None => Vdom.noNode
  }

  Html.div(list{Html.class'("db db-liveVal")}, list{title, exampleHtml})
}

let viewDBData = (vp: viewProps, db: db): Html.html<msg> =>
  switch Map.get(~key=TLID.toString(db.dbTLID), vp.dbStats) {
  | Some(stats) if CursorState.tlidOf(vp.cursorState) == Some(db.dbTLID) =>
    let liveVal = viewDbLatestEntry(stats)
    let count = viewDbCount(stats)
    Html.div(list{Html.class'("dbdata")}, list{count, liveVal})
  | _ => Vdom.noNode
  }

let viewDBHeader = (vp: viewProps, db: db): list<Html.html<msg>> => {
  let typeView = Html.span(
    list{Html.class'("toplevel-type")},
    list{fontAwesome("database"), Html.text("DB")},
  )

  let titleView = {
    let nameField = if vp.dbLocked {
      Html.text(dbName2String(db.dbName))
    } else {
      ViewBlankOr.viewText(~enterable=true, ~classes=list{"dbname"}, DBName, vp, db.dbName)
    }

    Html.span(
      list{Html.class'("toplevel-name")},
      list{
        nameField,
        Html.span(list{Html.class'("version")}, list{Html.text(".v" ++ string_of_int(db.version))}),
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

    Html.div(list{Html.class'("menu")}, list{TLMenu.viewMenu(vp.menuState, vp.tlid, list{delAct})})
  }

  list{typeView, titleView, menuView}
}

let viewDBColName = (~classes: list<string>, vp: viewProps, v: blankOr<string>): Html.html<msg> => {
  let enterable = B.isBlank(v) || !vp.dbLocked
  ViewBlankOr.viewText(~enterable, ~classes, DBColName, vp, v)
}

let viewDBColType = (~classes: list<string>, vp: viewProps, v: blankOr<string>): Html.html<msg> => {
  let enterable = B.isBlank(v) || !vp.dbLocked
  ViewBlankOr.viewText(~enterable, ~classes, DBColType, vp, v)
}

let viewDBCol = (vp: viewProps, isMigra: bool, tlid: TLID.t, (n, t): dbColumn): Html.html<msg> => {
  let deleteButton = if (
    vp.permission == Some(ReadWrite) && ((isMigra || !vp.dbLocked) && (B.isF(n) || B.isF(t)))
  ) {
    Html.div(
      list{
        Html.class'("delete-col"),
        ViewUtils.eventNoPropagation(
          ~key="dcidb-" ++ (TLID.toString(tlid) ++ ("-" ++ (n |> B.toID |> ID.toString))),
          "click",
          _ => DeleteColInDB(tlid, B.toID(n)),
        ),
      },
      list{fontAwesome("minus-circle")},
    )
  } else {
    Vdom.noNode
  }

  let row = list{
    viewDBColName(vp, ~classes=list{"name"}, n),
    viewDBColType(vp, ~classes=list{"type"}, t),
  }

  Html.div(
    list{Html.classList(list{("col", true), ("has-empty", deleteButton == Vdom.noNode)})},
    list{deleteButton, ...row},
  )
}

let viewMigraFuncs = (vp: viewProps, desc: string, varName: string): Html.html<msg> =>
  Html.div(
    list{Html.class'("col roll-fn")},
    list{
      Html.div(
        list{Html.class'("fn-title")},
        list{
          Html.span(list{}, list{Html.text(desc ++ " : ")}),
          Html.span(list{Html.class'("varname")}, list{Html.text(varName)}),
        },
      ),
      ...FluidView.view(vp, list{}),
    },
  )

let viewDB = (vp: viewProps, db: db, dragEvents: domEventList): list<Html.html<msg>> => {
  let lockClass = if vp.dbLocked {
    "lock"
  } else {
    "unlock"
  }

  let cols = if !(vp.permission == Some(ReadWrite)) || vp.dbLocked {
    List.filter(~f=((n, t)) => B.isF(n) && B.isF(t), db.cols)
  } else {
    db.cols
  }

  let keyView = Html.div(
    list{Html.class'("col key")},
    list{Html.text("All entries are identified by a unique string `key`.")},
  )

  let coldivs = List.map(~f=viewDBCol(vp, false, db.dbTLID), cols)
  let data = viewDBData(vp, db)
  let headerView = Html.div(list{Html.class'("spec-header " ++ lockClass)}, viewDBHeader(vp, db))

  Belt.List.concatMany([
    list{Html.div(list{Html.class'("db"), ...dragEvents}, list{headerView, keyView, ...coldivs})},
    list{data},
  ])
}
