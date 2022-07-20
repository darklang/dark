open Prelude
open ViewUtils
module B = BlankOr
module TL = Toplevel
module TD = TLID.Dict
module Cmd = Tea.Cmd

type modification = AppTypes.modification
type model = AppTypes.model
module Mod = AppTypes.Modification

let missingEventSpaceDesc: string = "Undefined"

let missingEventRouteDesc: string = "Undefined"

let delPrefix: string = "deleted-"

type identifier =
  | Tlid(TLID.t)
  | Other(string)

type onClickAction =
  | Destination(AppTypes.Page.t)
  | SendMsg(AppTypes.msg)
  | DoNothing

let tlidOfIdentifier = (identifier): option<TLID.t> =>
  switch identifier {
  | Tlid(tlid) => Some(tlid)
  | Other(_) => None
  }

let entryKeyFromIdentifier = (identifier): string =>
  switch identifier {
  | Tlid(tlid) => "entry-" ++ TLID.toString(tlid)
  | Other(s) => "entry-" ++ s
  }

type rec entry = {
  name: string,
  identifier: identifier,
  onClick: onClickAction,
  uses: option<int>,
  minusButton: option<AppTypes.msg>,
  plusButton: option<AppTypes.msg>,
  killAction: option<AppTypes.msg>,
  // if this is in the deleted section, what does minus do?
  verb: option<string>,
}

and category = {
  count: int,
  name: string,
  plusButton: option<AppTypes.msg>,
  iconAction: option<AppTypes.msg>,
  classname: string,
  tooltip: option<AppTypes.Tooltip.source>,
  entries: list<item>,
}

and item =
  | Category(category)
  | Entry(entry)

let rec count = (s: item): int =>
  switch s {
  | Entry(_) => 1
  | Category(c) => (c.entries |> List.map(~f=count))->List.sum(module(Int))
  }

let iconButton = (
  ~key: string,
  ~icon: string,
  ~classname: string,
  handler: AppTypes.msg,
): Html.html<AppTypes.msg> => {
  let event = ViewUtils.eventNeither(~key, "click", _ => handler)
  Html.div(list{event, Html.class'("icon-button " ++ classname)}, list{fontAwesome(icon)})
}

let categoryIcon_ = (name: string): list<Html.html<AppTypes.msg>> => {
  let darkIcon = ViewUtils.darkIcon
  // Deleted categories have a deleted- prefix, with which are not valid fontaweome icons
  switch name |> String.toLowercase |> Regex.replace(~re=Regex.regex(delPrefix), ~repl="") {
  | "http" => list{darkIcon("http")}
  | "dbs" => list{darkIcon("db")}
  | "fns" => list{darkIcon("fn")}
  | "deleted" => list{darkIcon("deleted")}
  | "package-manager" => list{fontAwesome("box-open")}
  | "static" => list{fontAwesome("file")}
  | "types" => list{darkIcon("types")}
  | "cron" => list{darkIcon("cron")}
  | "repl" => list{fontAwesome("terminal")}
  | "worker" => list{fontAwesome("wrench")}
  | "fof" => list{darkIcon("fof")}
  | "secrets" => list{fontAwesome("user-secret")}
  | _ if String.includes(~substring="pm-author", name) => list{fontAwesome("user")}
  | _ if String.includes(~substring="pm-package", name) => list{fontAwesome("cubes")}
  | _ => list{darkIcon("undefined")}
  }
}

let categoryButton = (~props=list{}, name: string, description: string): Html.html<AppTypes.msg> =>
  Html.div(
    list{
      Html.class'("category-icon"),
      Html.title(description),
      Vdom.attribute("", "role", "img"),
      Vdom.attribute("", "alt", description),
      ...props,
    },
    categoryIcon_(name),
  )

let setTooltips = (tooltip: AppTypes.Tooltip.source, entries: list<'a>): option<
  AppTypes.Tooltip.source,
> =>
  if entries == list{} {
    Some(tooltip)
  } else {
    None
  }

let handlerCategory = (
  filter: toplevel => bool,
  name: string,
  action: AppTypes.AutoComplete.omniAction,
  iconAction: option<AppTypes.msg>,
  tooltip: AppTypes.Tooltip.source,
  hs: list<PT.Handler.t>,
): category => {
  let handlers = hs |> List.filter(~f=h => filter(TLHandler(h)))
  {
    count: List.length(handlers),
    name: name,
    plusButton: Some(CreateRouteHandler(action)),
    classname: String.toLowercase(name),
    iconAction: iconAction,
    tooltip: setTooltips(tooltip, handlers),
    entries: List.map(handlers, ~f=h => {
      let tlid = h.tlid
      Entry({
        name: h.spec.name |> B.toOption |> Option.unwrap(~default=missingEventRouteDesc),
        uses: None,
        identifier: Tlid(tlid),
        onClick: Destination(FocusedHandler(tlid, None, true)),
        minusButton: None,
        killAction: Some(ToplevelDeleteForever(tlid)),
        plusButton: None,
        verb: if TL.isHTTPHandler(TLHandler(h)) {
          B.toOption(h.spec.modifier)
        } else {
          None
        },
      })
    }),
  }
}

let httpCategory = (handlers: list<PT.Handler.t>): category =>
  handlerCategory(
    TL.isHTTPHandler,
    "HTTP",
    NewHTTPHandler(None),
    Some(GoToArchitecturalView),
    Http,
    handlers,
  )

let cronCategory = (handlers: list<PT.Handler.t>): category =>
  handlerCategory(
    TL.isCronHandler,
    "Cron",
    NewCronHandler(None),
    Some(GoToArchitecturalView),
    Cron,
    handlers,
  )

let replCategory = (handlers: list<PT.Handler.t>): category =>
  handlerCategory(TL.isReplHandler, "REPL", NewReplHandler(None), None, Repl, handlers)

let workerCategory = (handlers: list<PT.Handler.t>): category => handlerCategory(tl =>
    TL.isWorkerHandler(tl) ||
    // Show the old workers here for now
    TL.isDeprecatedCustomHandler(tl)
  , "Worker", NewWorkerHandler(None), Some(GoToArchitecturalView), Worker, handlers)

let dbCategory = (m: AppTypes.model, dbs: list<PT.DB.t>): category => {
  let entries = List.map(dbs, ~f=db => {
    let uses = switch db.name {
    | Blank(_) => 0
    | F(_, name) => Refactor.dbUseCount(m, name)
    }

    let minusButton = None
    Entry({
      name: B.valueWithDefault("Untitled DB", db.name),
      identifier: Tlid(db.tlid),
      uses: Some(uses),
      onClick: Destination(FocusedDB(db.tlid, true)),
      minusButton: minusButton,
      killAction: Some(ToplevelDeleteForever(db.tlid)),
      verb: None,
      plusButton: None,
    })
  })

  {
    count: List.length(dbs),
    name: "Datastores",
    classname: "dbs",
    plusButton: Some(CreateDBTable),
    iconAction: Some(GoToArchitecturalView),
    tooltip: setTooltips(Datastore, entries),
    entries: entries,
  }
}

let f404Category = (m: AppTypes.model): category => {
  let f404s = {
    // Generate set of deleted handler specs, stringified
    let deletedHandlerSpecs =
      m.deletedHandlers
      |> Map.values
      |> List.map(~f=(h: PT.Handler.t) => {
        let space = B.valueWithDefault("", h.spec.space)
        let name = B.valueWithDefault("", h.spec.name)
        let modifier = B.valueWithDefault("", h.spec.modifier)
        /* Note that this concatenated string gets compared to `space ^ path ^ modifier` later.
         * h.spec.name and f404.path are the same thing, with different names. Yes this is confusing. */
        space ++ (name ++ modifier)
      })
      |> Set.String.fromList

    m.f404s
    |> List.uniqueBy(~f=f => f.space ++ (f.path ++ f.modifier))
    |> // Don't show 404s for deleted handlers
    List.filter(~f=f => !Set.member(~value=f.space ++ (f.path ++ f.modifier), deletedHandlerSpecs))
  }

  {
    count: List.length(f404s),
    name: "404s",
    plusButton: None,
    classname: "fof",
    iconAction: None,
    tooltip: setTooltips(FourOhFour, f404s),
    entries: List.map(f404s, ~f=({space, path, modifier, _} as fof) => Entry({
      name: if space == "HTTP" {
        path
      } else {
        space ++ ("::" ++ path)
      },
      uses: None,
      identifier: Other(fof.space ++ (fof.path ++ fof.modifier)),
      onClick: SendMsg(CreateHandlerFrom404(fof)),
      minusButton: Some(Delete404APICall(fof)),
      killAction: None,
      plusButton: Some(CreateHandlerFrom404(fof)),
      verb: if space == "WORKER" {
        None
      } else {
        Some(modifier)
      },
    })),
  }
}

let userFunctionCategory = (m: AppTypes.model, ufs: list<PT.UserFunction.t>): category => {
  let fns = ufs |> List.filter(~f=(fn: PT.UserFunction.t) => B.isF(fn.metadata.name))
  let entries = List.filterMap(fns, ~f=fn =>
    Option.map(B.toOption(fn.metadata.name), ~f=name => {
      let tlid = fn.tlid
      let usedIn = Introspect.allUsedIn(tlid, m)
      let minusButton = None
      Entry({
        name: name,
        identifier: Tlid(tlid),
        uses: Some(List.length(usedIn)),
        minusButton: minusButton,
        killAction: Some(DeleteUserFunctionForever(tlid)),
        onClick: Destination(FocusedFn(tlid, None)),
        plusButton: None,
        verb: None,
      })
    })
  )

  {
    count: List.length(fns),
    name: "Functions",
    classname: "fns",
    plusButton: Some(CreateFunction),
    iconAction: Some(GoToArchitecturalView),
    tooltip: setTooltips(Function, entries),
    entries: entries,
  }
}

let userTipeCategory = (m: AppTypes.model, tipes: list<PT.UserType.t>): category => {
  let tipes = tipes |> List.filter(~f=(ut: PT.UserType.t) => B.isF(ut.name))
  let entries = List.filterMap(tipes, ~f=tipe =>
    Option.map(B.toOption(tipe.name), ~f=name => {
      let minusButton = if Refactor.usedTipe(m, name) {
        None
      } else {
        Some(Msg.DeleteUserType(tipe.tlid))
      }

      Entry({
        name: name,
        identifier: Tlid(tipe.tlid),
        uses: Some(Refactor.tipeUseCount(m, name)),
        minusButton: minusButton,
        killAction: Some(DeleteUserTypeForever(tipe.tlid)),
        onClick: Destination(FocusedType(tipe.tlid)),
        plusButton: None,
        verb: None,
      })
    })
  )

  {
    count: List.length(tipes),
    name: "Types",
    classname: "types",
    plusButton: Some(CreateType),
    iconAction: None,
    tooltip: None,
    entries: entries,
  }
}

let standardCategories = (m, hs, dbs, ufns, tipes) => {
  let hs = hs |> Map.values |> List.sortBy(~f=tl => TL.sortkey(TLHandler(tl)))

  let dbs = dbs |> Map.values |> List.sortBy(~f=tl => TL.sortkey(TLDB(tl)))

  let ufns = ufns |> Map.values |> List.sortBy(~f=tl => TL.sortkey(TLFunc(tl)))

  let tipes = tipes |> Map.values |> List.sortBy(~f=tl => TL.sortkey(TLTipe(tl)))

  /* We want to hide user defined types for users who arent already using them
    since there is currently no way to use them other than as a function param.
    we should show user defined types once the user can use them more */
  let tipes = if List.length(tipes) === 0 {
    list{}
  } else {
    list{userTipeCategory(m, tipes)}
  }
  let catagories = list{
    httpCategory(hs),
    workerCategory(hs),
    cronCategory(hs),
    replCategory(hs),
    dbCategory(m, dbs),
    userFunctionCategory(m, ufns),
    ...tipes,
  }

  catagories
}

let packageManagerCategory = (pmfns: packageFns): category => {
  let getFnnameEntries = (moduleList: list<PT.Package.Fn.t>): list<item> => {
    let fnnames =
      moduleList
      |> List.sortBy(~f=(fn: PT.Package.Fn.t) => fn.name.module_)
      |> List.uniqueBy(~f=(fn: PT.Package.Fn.t) => fn.name.function)

    fnnames |> List.map(~f=(fn: PT.Package.Fn.t) => Entry({
      name: fn.name.module_ ++ "::" ++ fn.name.function ++ "_v" ++ string_of_int(fn.name.version),
      identifier: Tlid(fn.tlid),
      onClick: Destination(FocusedPackageManagerFn(fn.tlid)),
      uses: None,
      minusButton: None,
      plusButton: None,
      killAction: None,
      verb: None,
    }))
  }

  let getPackageEntries = (userList: list<PT.Package.Fn.t>): list<item> => {
    let uniquePackages =
      userList
      |> List.sortBy(~f=(fn: PT.Package.Fn.t) => fn.name.package)
      |> List.uniqueBy(~f=(fn: PT.Package.Fn.t) => fn.name.package)

    uniquePackages |> List.map(~f=(fn: PT.Package.Fn.t) => {
      let packageList =
        userList |> List.filter(~f=(f: PT.Package.Fn.t) => fn.name.package == f.name.package)

      Category({
        count: List.length(uniquePackages),
        name: fn.name.package,
        plusButton: None,
        iconAction: None,
        classname: "pm-package" ++ fn.name.package,
        tooltip: None,
        entries: getFnnameEntries(packageList),
      })
    })
  }

  let uniqueauthors =
    pmfns
    |> Map.values
    |> List.sortBy(~f=(fn: PT.Package.Fn.t) => fn.name.owner)
    |> List.uniqueBy(~f=(fn: PT.Package.Fn.t) => fn.name.owner)

  let getAuthorEntries = uniqueauthors |> List.map(~f=(fn: PT.Package.Fn.t) => {
    let authorList =
      pmfns |> Map.values |> List.filter(~f=(f: PT.Package.Fn.t) => fn.name.owner == f.name.owner)

    Category({
      count: List.length(uniqueauthors),
      name: fn.name.owner,
      plusButton: None,
      iconAction: None,
      classname: "pm-author" ++ fn.name.owner,
      tooltip: None,
      entries: getPackageEntries(authorList),
    })
  })

  {
    count: List.length(uniqueauthors),
    name: "Package Manager",
    plusButton: None,
    iconAction: None,
    classname: "package-manager",
    tooltip: Some(PackageManager),
    entries: getAuthorEntries,
  }
}

let deletedCategory = (m: AppTypes.model): category => {
  let cats = standardCategories(
    m,
    m.deletedHandlers,
    m.deletedDBs,
    m.deletedUserFunctions,
    m.deletedUserTipes,
  ) |> List.map(~f=c => {
    ...c,
    plusButton: None /* only allow new entries on the main category */,
    classname: // dont open/close in lockstep with parent
    delPrefix ++ c.classname,
    entries: List.map(c.entries, ~f=x =>
      switch x {
      | Entry(e) =>
        let actionOpt =
          e.identifier |> tlidOfIdentifier |> Option.map(~f=tlid => Msg.RestoreToplevel(tlid))

        Entry({
          ...e,
          plusButton: actionOpt,
          uses: None,
          minusButton: e.killAction,
          onClick: actionOpt
          |> Option.map(~f=msg => SendMsg(msg))
          |> Option.unwrap(~default=DoNothing),
        })
      | c => c
      }
    ),
  })

  let showTooltip = List.filter(~f=c => c.entries != list{}, cats)
  {
    count: (cats |> List.map(~f=c => count(Category(c))))->List.sum(module(Int)),
    name: "Deleted",
    plusButton: None,
    classname: "deleted",
    iconAction: None,
    tooltip: setTooltips(Deleted, showTooltip),
    entries: List.map(cats, ~f=c => Category(c)),
  }
}

let viewEmptyCategory = (c: category): Html.html<AppTypes.msg> => {
  let name = switch c.classname {
  | "http" => "HTTP handlers"
  | "cron" | "worker" | "repl" => c.name ++ "s"
  | _ => c.name
  }

  Html.div(list{Html.class'("simple-item empty")}, list{Html.text("No " ++ name)})
}

let viewEntry = (m: AppTypes.model, e: entry): Html.html<AppTypes.msg> => {
  let name = e.name
  let isSelected = tlidOfIdentifier(e.identifier) == CursorState.tlidOf(m.cursorState)

  let linkItem = {
    let verb = switch e.verb {
    | Some(v) => Html.span(list{Html.class'("verb " ++ v)}, list{Html.text(v)})
    | _ => Vdom.noNode
    }

    switch e.onClick {
    | Destination(dest) =>
      let cls = {
        let selected = if isSelected {
          " selected"
        } else {
          ""
        }
        let unused = if e.uses == Some(0) {
          " unused"
        } else {
          ""
        }
        "toplevel-link" ++ (selected ++ unused)
      }

      let path = Html.span(list{Html.class'("path")}, list{Html.text(name)})
      Html.span(list{Html.class'("toplevel-name")}, list{Url.linkFor(dest, cls, list{path, verb})})
    | SendMsg(msg) =>
      let cls = "toplevel-msg"
      let path = Html.span(list{Html.class'("path")}, list{Html.text(name)})
      let action = if m.permission == Some(ReadWrite) {
        ViewUtils.eventNeither(~key=name ++ "-clicked-msg", "click", _ => msg)
      } else {
        Vdom.noProp
      }

      Html.span(
        list{Html.class'("toplevel-name"), action},
        list{Html.span(list{Html.class'(cls)}, list{path, verb})},
      )
    | DoNothing => Html.span(list{Html.class'("toplevel-name")}, list{Html.text(name), verb})
    }
  }

  let iconspacer = Html.div(list{Html.class'("icon-spacer")}, list{})
  let minuslink = /* This prevents the delete button appearing in the hover view.
   * We'll add it back in for 404s specifically at some point */
  if m.permission == Some(Read) {
    iconspacer
  } else {
    switch e.minusButton {
    | Some(msg) =>
      iconButton(
        ~key=entryKeyFromIdentifier(e.identifier),
        ~icon="minus-circle",
        ~classname="delete-button",
        msg,
      )
    | None => iconspacer
    }
  }

  let pluslink = switch e.plusButton {
  | Some(msg) =>
    if m.permission == Some(ReadWrite) {
      iconButton(~key=e.name ++ "-plus", ~icon="plus-circle", ~classname="add-button", msg)
    } else {
      iconspacer
    }
  | None => iconspacer
  }

  Html.div(list{Html.class'("simple-item")}, list{minuslink, linkItem, pluslink})
}

let viewDeploy = (d: staticDeploy): Html.html<AppTypes.msg> => {
  let statusString = switch d.status {
  | Deployed => "Deployed"
  | Deploying => "Deploying"
  }

  let copyBtn = Html.div(
    list{
      Html.class'("icon-button copy-hash"),
      ViewUtils.eventNeither("click", ~key="hash-" ++ d.deployHash, m => ClipboardCopyLivevalue(
        "\"" ++ (d.deployHash ++ "\""),
        m.mePos,
      )),
    },
    list{fontAwesome("copy")},
  )

  Html.div(
    list{Html.class'("simple-item deploy")},
    list{
      Html.div(
        list{Html.class'("hash")},
        list{
          Html.a(list{Html.href(d.url), Html.target("_blank")}, list{Html.text(d.deployHash)}),
          copyBtn,
        },
      ),
      Html.div(
        list{
          Html.classList(list{
            ("status", true),
            (
              "success",
              switch d.status {
              | Deployed => true
              | _ => false
              },
            ),
          }),
        },
        list{Html.text(statusString)},
      ),
      Html.div(list{Html.class'("timestamp")}, list{Html.text(Js.Date.toUTCString(d.lastUpdate))}),
    },
  )
}

let categoryName = (name: string): Html.html<AppTypes.msg> =>
  Html.span(list{Html.class'("category-name")}, list{Html.text(name)})

let categoryOpenCloseHelpers = (s: AppTypes.Sidebar.State.t, classname: string, count: int): (
  Vdom.property<AppTypes.msg>,
  Vdom.property<AppTypes.msg>,
) => {
  let isOpen = Set.member(s.openedCategories, ~value=classname)
  let isDetailed = s.mode == DetailedMode
  let isSubCat = String.includes(~substring=delPrefix, classname)
  let openEventHandler = if isDetailed || isSubCat {
    ViewUtils.eventNoPropagation(
      ~key=if isOpen {
        "cheh-true-"
      } else {
        "cheh-false-"
      } ++
      classname,
      "click",
      _ => SidebarMsg(MarkCategoryOpen(!isOpen, classname)),
    )
  } else {
    Vdom.noProp
  }

  let openAttr = if isOpen && count != 0 {
    Vdom.attribute("", "open", "")
  } else {
    Vdom.noProp
  }

  (openEventHandler, openAttr)
}

let viewDeployStats = (m: AppTypes.model): Html.html<AppTypes.msg> => {
  let entries = m.staticDeploys
  let count = List.length(entries)
  let (openEventHandler, openAttr) = categoryOpenCloseHelpers(m.sidebarState, "deploys", count)

  let openAttr = if m.sidebarState.mode == AbridgedMode {
    Vdom.attribute("", "open", "")
  } else {
    openAttr
  }

  let title = categoryName("Static Assets")
  let summary = {
    let tooltip =
      Tooltips.generateContent(StaticAssets) |> Tooltips.viewToolTip(
        ~shouldShow=m.tooltipState.tooltipSource == Some(StaticAssets),
        ~tlid=None,
      )

    let openTooltip = if count == 0 {
      ViewUtils.eventNoPropagation(~key="open-tooltip-deploys", "click", _ => ToolTipMsg(
        OpenTooltip(StaticAssets),
      ))
    } else {
      Vdom.noProp
    }

    let header = Html.div(
      list{Html.class'("category-header"), openTooltip},
      list{categoryButton("static", "Static Assets"), title},
    )

    let deployLatest = if count != 0 {
      entries |> List.take(~count=1) |> List.map(~f=viewDeploy)
    } else {
      list{}
    }

    Html.summary(
      list{openEventHandler, Html.class'("category-summary")},
      list{tooltip, header, ...deployLatest},
    )
  }

  let deploys = if List.length(entries) > 0 {
    entries |> List.map(~f=viewDeploy)
  } else {
    list{Html.div(list{Html.class'("simple-item empty")}, list{Html.text("No Static deploys")})}
  }

  let content = Html.div(
    list{
      Html.class'("category-content"),
      eventNoPropagation(~key="cat-close-deploy", "mouseleave", _ => SidebarMsg(ResetSidebar)),
    },
    list{title, ...deploys},
  )

  let classes = Html.classList(list{
    ("sidebar-category", true),
    ("deploys", true),
    ("empty", count == 0),
  })

  Html.details(~unique="deploys", list{classes, openAttr}, list{summary, content})
}

let viewSecret = (s: SecretTypes.t): Html.html<AppTypes.msg> => {
  let copyBtn = Html.div(
    list{
      Html.class'("icon-button copy-secret-name"),
      ViewUtils.eventNeither(
        "click",
        ~key="copy-secret-" ++ s.secretName,
        m => ClipboardCopyLivevalue(s.secretName, m.mePos),
      ),
      Html.title("Click to copy secret name"),
    },
    list{fontAwesome("copy")},
  )

  let secretValue = Util.obscureString(s.secretValue)
  let secretValue = {
    // If str length > 16 chars, we just want to keep the last 16 chars
    let len = String.length(secretValue)
    let count = len - 16
    if count > 0 {
      String.dropLeft(~count, secretValue)
    } else {
      secretValue
    }
  }

  Html.div(
    list{Html.class'("simple-item secret")},
    list{
      Html.div(
        list{Html.class'("key-block")},
        list{
          Html.span(list{Html.class'("secret-name")}, list{Html.text(s.secretName)}),
          Html.span(list{Html.class'("secret-value")}, list{Html.text(secretValue)}),
        },
      ),
      copyBtn,
    },
  )
}

let viewSecretKeys = (m: AppTypes.model): Html.html<AppTypes.msg> => {
  let count = List.length(m.secrets)
  let (openEventHandler, openAttr) = categoryOpenCloseHelpers(m.sidebarState, "secrets", count)

  let openAttr = if m.sidebarState.mode == AbridgedMode {
    Vdom.attribute("", "open", "")
  } else {
    openAttr
  }

  let title = categoryName("Secret Keys")
  let summary = {
    let tooltip =
      Tooltips.generateContent(Secrets) |> Tooltips.viewToolTip(
        ~shouldShow=m.tooltipState.tooltipSource == Some(Secrets),
        ~tlid=None,
      )

    let openTooltip = if count == 0 {
      ViewUtils.eventNoPropagation(~key="open-tooltip-secrets", "click", _ => ToolTipMsg(
        OpenTooltip(Secrets),
      ))
    } else {
      Vdom.noProp
    }

    let plusBtn = iconButton(
      ~key="plus-secret",
      ~icon="plus-circle",
      ~classname="create-tl-icon",
      SecretMsg(OpenCreateModal),
    )

    let header = Html.div(
      list{Html.class'("category-header"), openTooltip},
      list{categoryButton("secrets", "Secret Keys"), title},
    )

    Html.summary(
      list{openEventHandler, Html.class'("category-summary")},
      list{tooltip, header, plusBtn},
    )
  }

  let entries = if count > 0 {
    List.map(m.secrets, ~f=viewSecret)
  } else {
    list{Html.div(list{Html.class'("simple-item empty")}, list{Html.text("No secret keys")})}
  }

  let content = Html.div(
    list{
      Html.class'("category-content"),
      eventNoPropagation(~key="cat-close-secret", "mouseleave", _ => SidebarMsg(ResetSidebar)),
    },
    list{title, ...entries},
  )

  let classes = Html.classList(list{
    ("sidebar-category", true),
    ("secrets", true),
    ("empty", count == 0),
  })

  Html.details(~unique="secrets", list{classes, openAttr}, list{summary, content})
}

let rec viewItem = (m: AppTypes.model, s: item): Html.html<AppTypes.msg> =>
  switch s {
  | Category(c) =>
    if c.count > 0 {
      viewCategory(m, c)
    } else {
      Vdom.noNode
    }
  | Entry(e) => viewEntry(m, e)
  }

and viewCategory = (m: AppTypes.model, c: category): Html.html<AppTypes.msg> => {
  let (openEventHandler, openAttr) = categoryOpenCloseHelpers(m.sidebarState, c.classname, c.count)

  let (openTooltip, tooltipView) = switch c.tooltip {
  | Some(tt) =>
    let view =
      Tooltips.generateContent(tt) |> Tooltips.viewToolTip(
        ~shouldShow=m.tooltipState.tooltipSource == Some(tt),
        ~tlid=None,
      )

    (
      ViewUtils.eventNoPropagation(~key="open-tooltip-" ++ c.classname, "click", _ => ToolTipMsg(
        OpenTooltip(tt),
      )),
      view,
    )
  | None => (Vdom.noProp, Vdom.noNode)
  }

  let openAttr = if m.sidebarState.mode == AbridgedMode {
    Vdom.attribute("", "open", "")
  } else {
    openAttr
  }

  let isSubCat = String.includes(~substring=delPrefix, c.classname)
  let title = categoryName(c.name)
  let summary = {
    let plusButton = switch c.plusButton {
    | Some(msg) =>
      if m.permission == Some(ReadWrite) {
        iconButton(
          ~key="plus-" ++ c.classname,
          ~icon="plus-circle",
          ~classname="create-tl-icon",
          msg,
        )
      } else {
        Vdom.noNode
      }
    | None => Vdom.noNode
    }

    let catIcon = {
      let props = switch c.iconAction {
      | Some(ev) if m.sidebarState.mode == AbridgedMode && !isSubCat => list{
          eventNeither(~key="return-to-arch", "click", _ => ev),
        }
      | Some(_) | None => list{Vdom.noProp}
      }

      categoryButton(c.classname, c.name, ~props)
    }

    let header = Html.div(list{Html.class'("category-header"), openTooltip}, list{catIcon, title})

    Html.summary(
      list{Html.class'("category-summary"), openEventHandler},
      list{tooltipView, header, plusButton},
    )
  }

  let content = {
    let entries = if c.count > 0 {
      List.map(~f=viewItem(m), c.entries)
    } else {
      list{viewEmptyCategory(c)}
    }

    Html.div(
      list{
        Html.class'("category-content"),
        eventNoPropagation(~key="cat-close-" ++ c.classname, "mouseleave", _ =>
          if !isSubCat {
            SidebarMsg(ResetSidebar)
          } else {
            Msg.IgnoreMsg("sidebar-category-close")
          }
        ),
      },
      list{categoryName(c.name), ...entries},
    )
  }

  let classes = Html.classList(list{
    ("sidebar-category", true),
    (c.classname, true),
    ("empty", c.count == 0),
  })

  Html.details(~unique=c.classname, list{classes, openAttr}, list{summary, content})
}

let viewToggleBtn = (isDetailed: bool): Html.html<AppTypes.msg> => {
  let event = ViewUtils.eventNeither(~key="toggle-sidebar", "click", _ => SidebarMsg(
    ToggleSidebarMode,
  ))

  let description = if isDetailed {
    "Collapse sidebar"
  } else {
    "Expand sidebar"
  }

  let icon = {
    let view' = iconName =>
      Html.span(list{Html.class'("icon")}, list{fontAwesome(iconName), fontAwesome(iconName)})

    if isDetailed {
      view'("chevron-left")
    } else {
      view'("chevron-right")
    }
  }

  let label = Html.span(list{Html.class'("label")}, list{Html.text(description)})
  let alt = if isDetailed {
    Vdom.noProp
  } else {
    Html.title(description)
  }
  Html.div(list{event, Html.class'("toggle-sidebar-btn"), alt}, list{label, icon})
}

let stateInfoTohtml = (key: string, value: Html.html<AppTypes.msg>): Html.html<AppTypes.msg> =>
  Html.div(
    list{Html.class'("state-info-row")},
    list{
      Html.p(list{Html.class'("key")}, list{Html.text(key)}),
      Html.p(list{Html.class'("sep")}, list{Html.text(":")}),
      Html.p(list{Html.class'("value")}, list{value}),
    },
  )

let adminDebuggerView = (m: AppTypes.model): Html.html<AppTypes.msg> => {
  let environmentName = if m.environment === "prodclone" {
    "clone"
  } else {
    m.environment
  }

  let pageToString = pg =>
    switch pg {
    | AppTypes.Page.Architecture => "Architecture"
    | FocusedPackageManagerFn(tlid) =>
      Printf.sprintf("Package Manager Fn (TLID %s)", TLID.toString(tlid))
    | FocusedFn(tlid, _) => Printf.sprintf("Fn (TLID %s)", TLID.toString(tlid))
    | FocusedHandler(tlid, _, _) => Printf.sprintf("Handler (TLID %s)", TLID.toString(tlid))
    | FocusedDB(tlid, _) => Printf.sprintf("DB (TLID %s)", TLID.toString(tlid))
    | FocusedType(tlid) => Printf.sprintf("Type (TLID %s)", TLID.toString(tlid))
    | SettingsModal(tab) =>
      Printf.sprintf("SettingsModal (tab %s)", SettingsViewTypes.settingsTabToText(tab))
    }

  let flagText =
    "[" ++ ((m.tests |> List.map(~f=show_variantTest) |> String.join(~sep=", ")) ++ "]")

  let environment = Html.div(
    list{Html.class'("environment " ++ environmentName)},
    list{Html.text(environmentName)},
  )

  let stateInfo = Html.div(
    list{Html.class'("state-info")},
    list{
      stateInfoTohtml("env", Html.text(m.environment)),
      stateInfoTohtml("flags", Html.text(flagText)),
      stateInfoTohtml("page", Html.text(pageToString(m.currentPage))),
      stateInfoTohtml("cursorState", Html.text(AppTypes.CursorState.show(m.cursorState))),
    },
  )

  let toggleTimer = Html.div(
    list{
      ViewUtils.eventNoPropagation(~key="tt", "mouseup", _ => ToggleEditorSetting(
        es => {...es, runTimers: !es.runTimers},
      )),
      Html.class'("checkbox-row"),
    },
    list{
      Html.input'(list{Html.type'("checkbox"), Html.checked(m.editorSettings.runTimers)}, list{}),
      Html.label(list{}, list{Html.text("Run Timers")}),
    },
  )

  let toggleFluidDebugger = Html.div(
    list{
      ViewUtils.eventNoPropagation(~key="tt", "mouseup", _ => ToggleEditorSetting(
        es => {...es, showFluidDebugger: !es.showFluidDebugger},
      )),
      Html.class'("checkbox-row"),
    },
    list{
      Html.input'(
        list{Html.type'("checkbox"), Html.checked(m.editorSettings.showFluidDebugger)},
        list{},
      ),
      Html.label(list{}, list{Html.text("Show Fluid Debugger")}),
    },
  )

  let toggleHandlerASTs = Html.div(
    list{
      ViewUtils.eventNoPropagation(~key="tgast", "mouseup", _ => ToggleEditorSetting(
        es => {...es, showHandlerASTs: !es.showHandlerASTs},
      )),
      Html.class'("checkbox-row"),
    },
    list{
      Html.input'(
        list{Html.type'("checkbox"), Html.checked(m.editorSettings.showHandlerASTs)},
        list{},
      ),
      Html.label(list{}, list{Html.text("Show Handler ASTs")}),
    },
  )

  let debugger = Html.a(
    list{Html.href(ViewScaffold.debuggerLinkLoc(m)), Html.class'("state-info-row debugger")},
    list{
      Html.text(
        if m.teaDebuggerEnabled {
          "Disable Debugger"
        } else {
          "Enable Debugger"
        },
      ),
    },
  )

  let variantLinks = if m.isAdmin {
    List.map(VariantTesting.availableAdminVariants, ~f=variant => {
      let name = VariantTesting.nameOf(variant)
      let enabled = VariantTesting.variantIsActive(m, variant)
      Html.a(
        list{Html.href(ViewScaffold.flagLinkLoc(name, enabled)), Html.class'("state-info-row")},
        list{
          Html.text(
            if enabled {
              "Disable " ++ (name ++ " variant")
            } else {
              "Enable " ++ (name ++ " variant")
            },
          ),
        },
      )
    })
  } else {
    list{}
  }

  let saveTestButton = Html.a(
    list{
      ViewUtils.eventNoPropagation(~key="stb", "mouseup", _ => SaveTestButton),
      Html.class'("state-info-row save-state"),
    },
    list{Html.text("SAVE STATE FOR INTEGRATION TEST")},
  )

  let hoverView = Html.div(
    list{Html.class'("category-content")},
    Belt.List.concatMany([
      list{stateInfo, toggleTimer, toggleFluidDebugger, toggleHandlerASTs, debugger},
      variantLinks,
      list{saveTestButton},
    ]),
  )

  let icon = Html.div(
    list{
      Html.class'("category-icon"),
      Html.title("Admin"),
      Vdom.attribute("", "role", "img"),
      Vdom.attribute("", "alt", "Admin"),
    },
    list{fontAwesome("cog")},
  )

  let sectionIcon = Html.div(list{Html.class'("category-summary")}, list{icon, environment})

  Html.div(list{Html.class'("sidebar-category admin")}, list{sectionIcon, hoverView})
}

let update = (msg: AppTypes.Sidebar.msg): modification =>
  switch msg {
  | ToggleSidebarMode =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let mode = switch m.sidebarState.mode {
        | DetailedMode => AppTypes.Sidebar.Mode.AbridgedMode
        | AbridgedMode => DetailedMode
        }

        ({...m, sidebarState: {...m.sidebarState, mode: mode}}, Cmd.none)
      },
    )
  | ResetSidebar => ReplaceAllModificationsWithThisOne(Viewport.enablePan(true))
  | MarkCategoryOpen(shouldOpen, key) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let openedCategories = if shouldOpen {
          Set.add(~value=key, m.sidebarState.openedCategories)
        } else {
          Set.remove(~value=key, m.sidebarState.openedCategories)
        }

        ({...m, sidebarState: {...m.sidebarState, openedCategories: openedCategories}}, Cmd.none)
      },
    )
  }

let viewSidebar_ = (m: AppTypes.model): Html.html<AppTypes.msg> => {
  let cats = Belt.List.concat(
    standardCategories(m, m.handlers, m.dbs, m.userFunctions, m.userTipes),
    list{f404Category(m), deletedCategory(m), packageManagerCategory(m.functions.packageFunctions)},
  )

  let isDetailed = switch m.sidebarState.mode {
  | DetailedMode => true
  | _ => false
  }

  let showAdminDebugger = if !isDetailed && m.isAdmin {
    adminDebuggerView(m)
  } else {
    Vdom.noNode
  }

  let secretsView = viewSecretKeys(m)
  let content = {
    let categories = Belt.List.concat(
      List.map(~f=viewCategory(m), cats),
      list{secretsView, viewDeployStats(m), showAdminDebugger},
    )

    Html.div(
      list{
        Html.classList(list{
          ("viewing-table", true),
          ("detailed", isDetailed),
          ("abridged", !isDetailed),
        }),
      },
      list{viewToggleBtn(isDetailed), ...categories},
    )
  }

  Html.div(
    list{
      Html.id("sidebar-left"),
      // Block opening the omnibox here by preventing canvas pan start
      nothingMouseEvent("mousedown"),
      ViewUtils.eventNoPropagation(~key="click-sidebar", "click", _ => ToolTipMsg(Close)),
      ViewUtils.eventNoPropagation(~key="ept", "mouseover", _ => EnablePanning(false)),
      ViewUtils.eventNoPropagation(~key="epf", "mouseout", _ => EnablePanning(true)),
    },
    list{content},
  )
}

let rtCacheKey = (m: model) =>
  (
    m.handlers |> Map.mapValues(~f=(h: PT.Handler.t) => (h.pos, TL.sortkey(TLHandler(h)))),
    m.dbs |> Map.mapValues(~f=(db: PT.DB.t) => (db.pos, TL.sortkey(TLDB(db)))),
    m.userFunctions |> Map.mapValues(~f=(uf: PT.UserFunction.t) => uf.metadata.name),
    m.userTipes |> Map.mapValues(~f=(ut: PT.UserType.t) => ut.name),
    m.f404s,
    m.sidebarState,
    m.deletedHandlers |> Map.mapValues(~f=(h: PT.Handler.t) => TL.sortkey(TLHandler(h))),
    m.deletedDBs |> Map.mapValues(~f=(db: PT.DB.t) => (db.pos, TL.sortkey(TLDB(db)))),
    m.deletedUserFunctions |> Map.mapValues(~f=(uf: PT.UserFunction.t) => uf.metadata.name),
    m.deletedUserTipes |> Map.mapValues(~f=(ut: PT.UserType.t) => ut.name),
    m.staticDeploys,
    m.unlockedDBs,
    m.usedDBs,
    m.usedFns,
    // CLEANUP do these need to be here twice
    m.userTipes |> Map.mapValues(~f=(ut: PT.UserType.t) => ut.name),
    m.deletedUserTipes |> Map.mapValues(~f=(ut: PT.UserType.t) => ut.name),
    CursorState.tlidOf(m.cursorState),
    m.environment,
    m.editorSettings,
    m.permission,
    m.currentPage,
    m.tooltipState.tooltipSource,
    m.secrets,
    m.functions.packageFunctions |> Map.mapValues(~f=(t: PT.Package.Fn.t) => t.name.owner),
  ) |> Option.some

let viewSidebar = m => ViewCache.cache1m(rtCacheKey, viewSidebar_, m)
