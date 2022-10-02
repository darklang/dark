open Prelude
open ViewUtils
module B = BlankOr
module TL = Toplevel
module TD = TLID.Dict
module Cmd = Tea.Cmd

type modification = AppTypes.modification
type model = AppTypes.model
type msg = AppTypes.msg
module Mod = AppTypes.Modification

let fontAwesome = Icons.fontAwesome

let tw = Attrs.class
let tw2 = (c1, c2) => Attrs.class(`${c1} ${c2}`)
let tw3 = (c1, c2, c3) => Attrs.class(`${c1} ${c2} ${c3}`)

let missingEventRouteDesc: string = "Undefined"

let delPrefix: string = "deleted-"

type identifier =
  | Tlid(TLID.t)
  | Other(string)

type onClickAction =
  | Destination(AppTypes.Page.t)
  | SendMsg(msg)
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
  minusButton: option<msg>,
  plusButton: option<msg>,
  killAction: option<msg>,
  // if this is in the deleted section, what does minus do?
  verb: option<string>,
}

and category = {
  count: int,
  name: string,
  nested: bool,
  plusButton: option<msg>,
  iconAction: option<msg>,
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

module Styles = {
  let categoryNameBase = %twc(
    "block text-grey8 font-bold mt-0 tracking-wide w-full font-heading -ml-4"
  )
  let contentCategoryName = %twc("text-lg text-center ") ++ categoryNameBase
  let nestedSidebarCategoryName = %twc("text-base text-left ") ++ categoryNameBase

  let plusButton = %twc("text-xs hover:text-sidebar-hover hover:cursor-pointer")

  let sidebarCategory = %twc("mb-5 px-1.25 py-0 relative group-outer")

  let categoryContent = %twc("hidden group-outer-hover:block")
}

let iconButton = (
  ~key: string,
  ~icon: string,
  ~classname: string,
  ~style: string,
  handler: msg,
): Html.html<msg> => {
  let twStyle = %twc("hover:text-sidebar-hover cursor-pointer")
  let event = EventListeners.eventNeither(~key, "click", _ => handler)
  Html.div(list{event, Attrs.class(`${style} ${twStyle} ${classname}`)}, list{fontAwesome(icon)})
}

let categoryIcon_ = (name: string): list<Html.html<msg>> => {
  let darkIcon = Icons.darkIcon
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
  | "admin" => list{fontAwesome("cog")}
  | _ if String.includes(~substring="pm-author", name) => list{fontAwesome("user")}
  | _ if String.includes(~substring="pm-package", name) => list{fontAwesome("cubes")}
  | _ => list{darkIcon("undefined")}
  }
}

let categoryButton = (~props=list{}, name: string, description: string): Html.html<msg> =>
  Html.div(
    list{
      tw(
        %twc(
          "mr-0 text-2xl text-grey5 duration-200 group-outer-hover:text-3xl w-9 h-9 text-center"
        ),
      ),
      Attrs.title(description),
      Attrs.role("img"),
      Attrs.alt(description),
      ...props,
    },
    categoryIcon_(name),
  )

let handlerCategory = (
  filter: toplevel => bool,
  name: string,
  action: AppTypes.AutoComplete.omniAction,
  iconAction: option<msg>,
  tooltip: AppTypes.Tooltip.source,
  hs: list<PT.Handler.t>,
): category => {
  let handlers = hs |> List.filter(~f=h => filter(TLHandler(h)))
  {
    count: List.length(handlers),
    name: name,
    plusButton: Some(CreateRouteHandler(action)),
    nested: false,
    classname: String.toLowercase(name),
    iconAction: iconAction,
    tooltip: Some(tooltip),
    entries: List.map(handlers, ~f=h => {
      let tlid = h.tlid
      Entry({
        name: PT.Handler.Spec.name(h.spec) |> B.valueWithDefault(missingEventRouteDesc),
        uses: None,
        identifier: Tlid(tlid),
        onClick: Destination(FocusedHandler(tlid, None, true)),
        minusButton: None,
        killAction: Some(ToplevelDeleteForever(tlid)),
        plusButton: None,
        verb: if TL.isHTTPHandler(TLHandler(h)) {
          h.spec->PT.Handler.Spec.modifier->Option.andThen(~f=B.toOption)
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
  handlerCategory(
    TL.isReplHandler,
    "REPL",
    NewReplHandler(None),
    Some(GoToArchitecturalView),
    Repl,
    handlers,
  )

let workerCategory = (handlers: list<PT.Handler.t>): category => handlerCategory(tl =>
    TL.isWorkerHandler(tl) ||
    // Show the old workers here for now
    TL.isDeprecatedCustomHandler(tl)
  , "Worker", NewWorkerHandler(None), Some(GoToArchitecturalView), Worker, handlers)

let dbCategory = (m: model, dbs: list<PT.DB.t>): category => {
  let entries = List.map(dbs, ~f=db => {
    let uses = if db.name == "" {
      0
    } else {
      Refactor.dbUseCount(m, db.name)
    }

    let minusButton = None
    Entry({
      name: if db.name == "" {
        "Untitled DB"
      } else {
        db.name
      },
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
    nested: false,
    classname: "dbs",
    plusButton: Some(CreateDBTable),
    iconAction: Some(GoToArchitecturalView),
    tooltip: Some(Datastore),
    entries: entries,
  }
}

let f404Category = (m: model): category => {
  let f404s = {
    // Generate set of deleted handler specs, stringified
    let deletedHandlerSpecs =
      m.deletedHandlers
      |> Map.values
      |> List.map(~f=(h: PT.Handler.t) => {
        let space = h.spec->PT.Handler.Spec.space->B.toString
        let name = h.spec->PT.Handler.Spec.name->B.toString
        let modifier = h.spec->PT.Handler.Spec.modifier->B.optionToString
        // Note that this concatenated string gets compared to `space ^ path ^ modifier` later.
        // h.spec.name and f404.path are the same thing, with different names. Yes this is confusing.
        space ++ name ++ modifier
      })
      |> Set.String.fromList

    m.f404s
    |> List.uniqueBy(~f=(f: AnalysisTypes.FourOhFour.t) => f.space ++ (f.path ++ f.modifier))
    |> // Don't show 404s for deleted handlers
    List.filter(~f=(f: AnalysisTypes.FourOhFour.t) =>
      !Set.member(~value=f.space ++ (f.path ++ f.modifier), deletedHandlerSpecs)
    )
  }

  {
    count: List.length(f404s),
    name: "404s",
    plusButton: None,
    nested: false,
    classname: "fof",
    iconAction: None,
    tooltip: Some(FourOhFour),
    entries: List.map(f404s, ~f=({space, path, modifier, _} as fof) => Entry({
      name: if space == "HTTP" {
        path
      } else {
        space ++ ("::" ++ path)
      },
      uses: None,
      identifier: Other(fof.space ++ fof.path ++ fof.modifier),
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

let userFunctionCategory = (m: model, ufs: list<PT.UserFunction.t>): category => {
  let fns = ufs |> List.filter(~f=(fn: PT.UserFunction.t) => fn.name != "")
  let entries = List.map(fns, ~f=fn => {
    let tlid = fn.tlid
    let usedIn = Introspect.allUsedIn(tlid, m)
    let minusButton = None
    Entry({
      name: fn.name,
      identifier: Tlid(tlid),
      uses: Some(List.length(usedIn)),
      minusButton: minusButton,
      killAction: Some(DeleteUserFunctionForever(tlid)),
      onClick: Destination(FocusedFn(tlid, None)),
      plusButton: None,
      verb: None,
    })
  })

  {
    count: List.length(fns),
    name: "Functions",
    nested: false,
    classname: "fns",
    plusButton: Some(CreateFunction),
    iconAction: Some(GoToArchitecturalView),
    tooltip: Some(Function),
    entries: entries,
  }
}

let userTipeCategory = (m: model, tipes: list<PT.UserType.t>): category => {
  let tipes = tipes |> List.filter(~f=(ut: PT.UserType.t) => ut.name != "")
  let entries = List.map(tipes, ~f=tipe => {
    let minusButton = if Refactor.usedTipe(m, tipe.name) {
      None
    } else {
      Some(Msg.DeleteUserType(tipe.tlid))
    }

    Entry({
      name: tipe.name,
      identifier: Tlid(tipe.tlid),
      uses: Some(Refactor.tipeUseCount(m, tipe.name)),
      minusButton: minusButton,
      killAction: Some(DeleteUserTypeForever(tipe.tlid)),
      onClick: Destination(FocusedType(tipe.tlid)),
      plusButton: None,
      verb: None,
    })
  })

  {
    count: List.length(tipes),
    name: "Types",
    nested: false,
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
        nested: true,
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
      nested: true,
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
    nested: false,
    plusButton: None,
    iconAction: None,
    classname: "package-manager",
    tooltip: Some(PackageManager),
    entries: getAuthorEntries,
  }
}

let deletedCategory = (m: model): category => {
  let cats = standardCategories(
    m,
    m.deletedHandlers,
    m.deletedDBs,
    m.deletedUserFunctions,
    m.deleteduserTypes,
  ) |> List.map(~f=c => {
    ...c,
    plusButton: None /* only allow new entries on the main category */,
    // dont open/close in lockstep with parent
    classname: delPrefix ++ c.classname,
    nested: true,
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

  {
    count: (cats |> List.map(~f=c => count(Category(c))))->List.sum(module(Int)),
    name: "Deleted",
    nested: false,
    plusButton: None,
    classname: "deleted",
    iconAction: None,
    tooltip: Some(Deleted),
    entries: List.map(cats, ~f=c => Category(c)),
  }
}

let viewEmptyCategory = (c: category): Html.html<msg> => {
  let name = switch c.classname {
  | "http" => "HTTP handlers"
  | "cron" | "worker" | "repl" => c.name ++ "s"
  | _ => c.name
  }

  Html.div(list{tw2(%twc("text-sidebar-secondary"), "simple-item")}, list{Html.text("No " ++ name)})
}

let viewEntry = (m: model, e: entry): Html.html<msg> => {
  let name = e.name
  let isSelected = tlidOfIdentifier(e.identifier) == CursorState.tlidOf(m.cursorState)

  let linkItem = {
    let verb = switch e.verb {
    | Some(verb) =>
      let verbStyle = switch verb {
      | "GET" => %twc("text-http-get")
      | "POST" => %twc("text-http-post")
      | "PUT" => %twc("text-http-put")
      | "DELETE" => %twc("text-http-delete")
      | "PATCH" => %twc("text-http-patch")
      | "HEAD" => %twc("text-white2") // TODO
      | "OPTIONS" => %twc("text-http-options")
      | _ => %twc("text-white2")
      }
      Html.span(list{tw2(verbStyle, "ml-4")}, list{Html.text(verb)})
    | _ => Vdom.noNode
    }

    switch e.onClick {
    | Destination(dest) =>
      let selected = {
        // font-black is same as fa-solid, font-medium produces the empty circle
        let dotStyle = if isSelected {
          %twc("inline-block text-sidebar-hover group-hover:text-orange font-black")
        } else {
          %twc("inline-block text-transparent group-hover:text-sidebar-hover font-medium")
        }
        // unclear why both align-middle and mb-[2px] are needed to make the dot center
        let baseStyle = %twc("text-xxs pl-0.25 pr-0.5 align-middle mb-[2px]")
        fontAwesome(~style=`${baseStyle} ${dotStyle}`, "circle")
      }

      let cls = {
        let unused = if e.uses == Some(0) {
          %twc("text-sidebar-secondary")
        } else {
          ""
        }
        let default = %twc(
          "flex justify-between cursor-pointer w-full text-sidebar-primary no-underline outline-none"
        )

        `${default} ${unused}`
      }

      Html.span(
        list{Attrs.class("group toplevel-name")},
        list{
          Url.linkFor(dest, cls, list{Html.span(list{}, list{selected, Html.text(name)}), verb}),
        },
      )
    | SendMsg(msg) =>
      let path = Html.span(list{}, list{Html.text(name)})
      let action = if m.permission == Some(ReadWrite) {
        EventListeners.eventNeither(~key=name ++ "-clicked-msg", "click", _ => msg)
      } else {
        Vdom.noProp
      }

      Html.span(
        list{Attrs.class'("toplevel-name"), action},
        list{
          Html.span(list{tw(%twc("flex justify-between w-full cursor-pointer"))}, list{path, verb}),
        },
      )
    | DoNothing => Html.span(list{Attrs.class'("toplevel-name")}, list{Html.text(name), verb})
    }
  }

  let iconspacer = Html.div(list{Attrs.class'("icon-spacer")}, list{})
  let minuslink = /* This prevents the delete button appearing in the hover view.
   * We'll add it back in for 404s specifically at some point */
  if m.permission == Some(Read) {
    iconspacer
  } else {
    switch e.minusButton {
    | Some(msg) =>
      iconButton(
        ~key=entryKeyFromIdentifier(e.identifier),
        ~style=%twc("mr-3 text-sidebar-secondary"),
        ~icon="minus-circle",
        ~classname="",
        msg,
      )
    | None => iconspacer
    }
  }

  let pluslink = switch e.plusButton {
  | Some(msg) =>
    if m.permission == Some(ReadWrite) {
      iconButton(
        ~key=e.name ++ "-plus",
        ~icon="plus-circle",
        ~style=%twc("ml-1.5"),
        ~classname="add-button",
        msg,
      )
    } else {
      iconspacer
    }
  | None => iconspacer
  }

  Html.div(list{Attrs.class'("simple-item")}, list{minuslink, linkItem, pluslink})
}

let viewDeploy = (d: StaticAssets.Deploy.t): Html.html<msg> => {
  let statusString = switch d.status {
  | Deployed => "Deployed"
  | Deploying => "Deploying"
  }

  let copyBtn = Html.div(
    list{
      tw2("icon-button ", %twc("text-xs absolute -top-2 -right-1.5 hover:text-sidebar-hover")),
      EventListeners.eventNeither(
        "click",
        ~key="hash-" ++ d.deployHash,
        m => Msg.ClipboardCopyLivevalue("\"" ++ d.deployHash ++ "\"", m.mePos),
      ),
    },
    list{fontAwesome("copy")},
  )

  let statusColor = switch d.status {
  | Deployed => %twc("text-green")
  | Deploying => %twc("text-sidebar-secondary")
  }

  Html.div(
    list{tw(%twc("flex flex-wrap justify-between items-center mt-4"))},
    list{
      Html.div(
        list{
          tw(
            %twc(
              "relative inline-block border border-solid border-sidebar-secondary p-0.5 rounded-sm"
            ),
          ),
        },
        list{
          Html.a(
            list{
              tw(%twc("text-sm text-sidebar-primary hover:text-sidebar-hover no-underline")),
              Attrs.href(d.url),
              Attrs.target("_blank"),
            },
            list{Html.text(d.deployHash)},
          ),
          copyBtn,
        },
      ),
      Html.div(list{tw2(statusColor, %twc("inline-block"))}, list{Html.text(statusString)}),
      Html.div(
        list{tw(%twc("block w-full text-xxs text-right"))},
        list{Html.text(Js.Date.toUTCString(d.lastUpdate))},
      ),
    },
  )
}

let viewDeployStats = (m: model): Html.html<msg> => {
  let entries = m.staticDeploys

  let summary = {
    let tooltip =
      Tooltips.generateContent(StaticAssets) |> Tooltips.viewToolTip(
        ~shouldShow=m.tooltipState.tooltipSource == Some(StaticAssets),
        ~tlid=None,
      )

    Html.div(
      list{Attrs.class'("category-summary")},
      list{
        tooltip,
        Html.div(
          list{Attrs.class'("category-header")},
          list{categoryButton("static", "Static Assets")},
        ),
      },
    )
  }

  let content = {
    let deploys = if List.length(entries) > 0 {
      entries |> List.map(~f=viewDeploy)
    } else {
      list{Html.div(list{tw(%twc("text-grey2 mt-1"))}, list{Html.text("No Static deploys")})}
    }

    Html.div(
      list{
        tw2("category-content", Styles.categoryContent),
        EventListeners.eventNoPropagation(
          ~key="cat-close-deploy",
          "mouseleave",
          _ => Msg.SidebarMsg(ResetSidebar),
        ),
      },
      list{
        Html.span(list{Attrs.class(Styles.contentCategoryName)}, list{Html.text("Static Assets")}),
        ...deploys,
      },
    )
  }

  Html.div(list{tw2("sidebar-category", Styles.sidebarCategory)}, list{summary, content})
}

let viewSecret = (s: SecretTypes.t): Html.html<msg> => {
  let copyBtn = Html.div(
    list{
      tw2("icon-button", %twc("text-base hover:text-sidebar-hover")),
      EventListeners.eventNeither(
        "click",
        ~key="copy-secret-" ++ s.secretName,
        m => Msg.ClipboardCopyLivevalue(s.secretName, m.mePos),
      ),
      Attrs.title("Click to copy secret name"),
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

  let style = %twc("text-xs no-underline box-border px-1.5 py-0 w-full overflow-hidden")

  Html.div(
    list{
      tw("flex relative justify-between items-center flex-row flex-nowrap w-72 ml-5 mr-1 mt-2.5"),
    },
    list{
      Html.div(
        list{
          tw(
            %twc(
              "group border border-solid border-sidebar-secondary pt-1 rounded-sm text-sidebar-primary w-64 hover:cursor-pointer hover:text-sidebar-hover hover:border-sidebar-hover"
            ),
          ),
        },
        list{
          Html.span(
            list{tw2(style, %twc("inline-block group-hover:hidden"))},
            list{Html.text(s.secretName)},
          ),
          Html.span(
            list{tw2(style, %twc("hidden group-hover:inline-block"))},
            list{Html.text(secretValue)},
          ),
        },
      ),
      copyBtn,
    },
  )
}

let viewSecretKeys = (m: model): Html.html<AppTypes.msg> => {
  let count = List.length(m.secrets)

  let summary = {
    let tooltip =
      Tooltips.generateContent(Secrets) |> Tooltips.viewToolTip(
        ~shouldShow=m.tooltipState.tooltipSource == Some(Secrets),
        ~tlid=None,
      )

    let plusBtn = iconButton(
      ~key="plus-secret",
      ~icon="plus-circle",
      ~style=Styles.plusButton,
      ~classname="",
      SecretMsg(OpenCreateModal),
    )

    let header = Html.div(
      list{Attrs.class'("category-header")},
      list{categoryButton("secrets", "Secret Keys")},
    )

    Html.div(list{Attrs.class'("category-summary")}, list{tooltip, header, plusBtn})
  }

  let content = {
    let title = Html.span(list{tw(Styles.contentCategoryName)}, list{Html.text("Secret Keys")})
    let entries = if count > 0 {
      List.map(m.secrets, ~f=viewSecret)
    } else {
      list{Html.div(list{Attrs.class'("simple-item")}, list{Html.text("No secret keys")})}
    }
    Html.div(
      list{
        tw2("category-content", Styles.categoryContent),
        EventListeners.eventNoPropagation(
          ~key="cat-close-secret",
          "mouseleave",
          _ => Msg.SidebarMsg(ResetSidebar),
        ),
      },
      list{title, ...entries},
    )
  }

  Html.div(list{tw2("sidebar-category", Styles.sidebarCategory)}, list{summary, content})
}

let rec viewItem = (m: model, s: item): Html.html<msg> =>
  switch s {
  | Category(c) =>
    if c.count > 0 {
      viewCategory(m, c)
    } else {
      Vdom.noNode
    }
  | Entry(e) => viewEntry(m, e)
  }

and viewCategory = (m: model, c: category): Html.html<msg> => {
  let tooltipView = switch c.tooltip {
  | Some(tt) =>
    Tooltips.generateContent(tt) |> Tooltips.viewToolTip(
      ~shouldShow=m.tooltipState.tooltipSource == Some(tt),
      ~tlid=None,
    )
  | None => Vdom.noNode
  }

  let summary = {
    let plusButton = switch c.plusButton {
    | Some(msg) =>
      if m.permission == Some(ReadWrite) {
        iconButton(
          ~key="plus-" ++ c.classname,
          ~icon="plus-circle",
          ~style=Styles.plusButton,
          ~classname="",
          msg,
        )
      } else {
        Vdom.noNode
      }
    | None => Vdom.noNode
    }

    let catIcon = {
      if c.nested {
        Vdom.noNode
      } else {
        let props = switch c.iconAction {
        | Some(ev) if !c.nested => list{
            EventListeners.eventNeither(~key="return-to-arch", "click", _ => ev),
          }
        | Some(_) | None => list{Vdom.noProp}
        }

        categoryButton(c.classname, c.name, ~props)
      }
    }

    let header = Html.div(list{Attrs.class'("category-header")}, list{catIcon})

    Html.div(list{Attrs.class'("category-summary")}, list{tooltipView, header, plusButton})
  }

  let content = {
    let style = if c.nested {
      Styles.nestedSidebarCategoryName
    } else {
      Styles.contentCategoryName
    }
    let title = Html.span(list{tw(style)}, list{Html.text(c.name)})
    let entries = if c.count > 0 {
      List.map(~f=viewItem(m), c.entries)
    } else {
      list{viewEmptyCategory(c)}
    }

    Html.div(
      list{
        tw2("category-content", Styles.categoryContent),
        EventListeners.eventNoPropagation(~key="cat-close-" ++ c.classname, "mouseleave", _ =>
          if !c.nested {
            Msg.SidebarMsg(ResetSidebar)
          } else {
            Msg.IgnoreMsg("sidebar-category-close")
          }
        ),
      },
      list{title, ...entries},
    )
  }

  Html.div(
    list{tw2("sidebar-category " ++ c.classname, Styles.sidebarCategory)},
    list{summary, content},
  )
}

let adminDebuggerView = (m: model): Html.html<msg> => {
  let rowStyle = %twc("flex h-4 items-center ml-4 m-1.5")
  let stateRowStyle = rowStyle ++ %twc(" justify-start mx-0")

  let stateInfoTohtml = (key: string, value: Html.html<msg>): Html.html<msg> =>
    Html.div(
      list{tw(stateRowStyle)},
      list{
        Html.p(list{}, list{Html.text(key)}),
        Html.p(list{tw(%twc("w-3.5"))}, list{Html.text(":")}),
        Html.p(list{tw(%twc("max-w-[210px] whitespace-nowrap"))}, list{value}),
      },
    )

  let pageToString = pg =>
    switch pg {
    | AppTypes.Page.Architecture => "Architecture"
    | FocusedPackageManagerFn(tlid) =>
      Printf.sprintf("Package Manager Fn (TLID %s)", TLID.toString(tlid))
    | FocusedFn(tlid, _) => Printf.sprintf("Fn (TLID %s)", TLID.toString(tlid))
    | FocusedHandler(tlid, _, _) => Printf.sprintf("Handler (TLID %s)", TLID.toString(tlid))
    | FocusedDB(tlid, _) => Printf.sprintf("DB (TLID %s)", TLID.toString(tlid))
    | FocusedType(tlid) => Printf.sprintf("Type (TLID %s)", TLID.toString(tlid))
    | SettingsModal(tab) => Printf.sprintf("SettingsModal (tab %s)", Settings.Tab.toText(tab))
    }

  let environment = {
    let color = switch m.environment {
    | "production" => %twc("text-black3")
    | "dev" => %twc("text-blue")
    | _ => %twc("text-magenta")
    }

    Html.div(
      list{tw2(%twc("bg-white1 text-[0.4em] rounded-sm p-0.5 -m-2.5"), color)},
      list{Html.text(m.environment)},
    )
  }

  let stateInfo = Html.div(
    list{},
    list{
      stateInfoTohtml("env", Html.text(m.environment)),
      stateInfoTohtml("page", Html.text(pageToString(m.currentPage))),
      stateInfoTohtml("cursorState", Html.text(AppTypes.CursorState.show(m.cursorState))),
    },
  )

  let input = (
    checked: bool,
    fn: AppTypes.EditorSettings.t => AppTypes.EditorSettings.t,
    label: string,
    style: string,
  ) => {
    let event = EventListeners.eventNoPropagation(
      ~key=`tt-${label}-${checked ? "checked" : "unchecked"}`,
      "mouseup",
      _ => Msg.ToggleEditorSetting(es => fn(es)),
    )

    Html.div(
      list{event, tw(rowStyle ++ " " ++ style)},
      list{
        Html.input(
          list{Attrs.type'("checkbox"), Attrs.checked(checked), tw(%twc("cursor-pointer"))},
          list{},
        ),
        Html.label(list{tw(%twc("ml-2"))}, list{Html.text(label)}),
      },
    )
  }

  let toggleTimer = input(
    m.editorSettings.runTimers,
    es => {...es, runTimers: !es.runTimers},
    "Run Timers",
    "mt-4",
  )

  let toggleFluidDebugger = input(
    m.editorSettings.showFluidDebugger,
    es => {...es, showFluidDebugger: !es.showFluidDebugger},
    "Show Fluid Debugger",
    "",
  )

  let toggleHandlerASTs = input(
    m.editorSettings.showHandlerASTs,
    es => {...es, showHandlerASTs: !es.showHandlerASTs},
    "Show Handler ASTs",
    "mb-4",
  )

  let debugger = Html.a(
    list{
      Attrs.href(ViewScaffold.debuggerLinkLoc(m)),
      tw(stateRowStyle ++ %twc(" text-grey8 hover:text-white3")),
    },
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

  let saveTestButton = Html.a(
    list{
      EventListeners.eventNoPropagation(~key="stb", "mouseup", _ => Msg.SaveTestButton),
      tw2(
        stateRowStyle,
        %twc(
          "border border-solid rounded-sm p-1 mb-5 h-2.5 w-fit text-xxs text-grey8 cursor-pointer hover:text-black2 hover:bg-grey8"
        ),
      ),
    },
    list{Html.text("SAVE STATE FOR INTEGRATION TEST")},
  )

  let hoverView = Html.div(
    list{tw2("category-content", Styles.categoryContent)},
    Belt.List.concatMany([
      list{stateInfo, toggleTimer, toggleFluidDebugger, toggleHandlerASTs, debugger},
      list{saveTestButton},
    ]),
  )

  Html.div(
    list{tw3(%twc("p-0"), Styles.sidebarCategory, "sidebar-category")},
    list{
      Html.div(
        list{tw2("category-summary", %twc("flex flex-col justify-start items-center -ml-2"))},
        list{categoryButton("admin", "Admin"), environment},
      ),
      hoverView,
    },
  )
}

let update = (msg: Sidebar.msg): modification =>
  switch msg {
  | ResetSidebar => ReplaceAllModificationsWithThisOne(Viewport.enablePan(true))
  }

let viewSidebar_ = (m: model): Html.html<msg> => {
  let cats = Belt.List.concat(
    standardCategories(m, m.handlers, m.dbs, m.userFunctions, m.userTypes),
    list{f404Category(m), deletedCategory(m), packageManagerCategory(m.functions.packageFunctions)},
  )

  let showAdminDebugger = if m.settings.contributingSettings.general.showSidebarDebuggerPanel {
    adminDebuggerView(m)
  } else {
    Vdom.noNode
  }

  let categories = Belt.List.concat(
    List.map(~f=viewCategory(m), cats),
    list{viewSecretKeys(m), viewDeployStats(m), showAdminDebugger},
  )

  Html.div(
    list{
      Attrs.id("sidebar-left"), // keep for sidebar and z-index
      tw2(
        %twc(
          "h-full top-0 left-0 p-0 fixed box-border transition-[width] duration-200 bg-sidebar-bg pt-[20px] w-14"
        ),
        "abridged",
      ),
      // Block opening the omnibox here by preventing canvas pan start
      EventListeners.nothingMouseEvent("mousedown"),
      EventListeners.eventNoPropagation(~key="ept", "mouseover", _ => Msg.EnablePanning(false)),
      EventListeners.eventNoPropagation(~key="epf", "mouseout", _ => Msg.EnablePanning(true)),
    },
    categories,
  )
}

let rtCacheKey = (m: model) =>
  (
    m.handlers |> Map.mapValues(~f=(h: PT.Handler.t) => (h.pos, TL.sortkey(TLHandler(h)))),
    m.dbs |> Map.mapValues(~f=(db: PT.DB.t) => (db.pos, TL.sortkey(TLDB(db)))),
    m.userFunctions |> Map.mapValues(~f=(uf: PT.UserFunction.t) => uf.name),
    m.userTypes |> Map.mapValues(~f=(ut: PT.UserType.t) => ut.name),
    m.f404s,
    m.deletedHandlers |> Map.mapValues(~f=(h: PT.Handler.t) => TL.sortkey(TLHandler(h))),
    m.deletedDBs |> Map.mapValues(~f=(db: PT.DB.t) => (db.pos, TL.sortkey(TLDB(db)))),
    m.deletedUserFunctions |> Map.mapValues(~f=(uf: PT.UserFunction.t) => uf.name),
    m.deleteduserTypes |> Map.mapValues(~f=(ut: PT.UserType.t) => ut.name),
    m.staticDeploys,
    m.unlockedDBs,
    m.usedDBs,
    m.usedFns,
    m.usedTipes,
    CursorState.tlidOf(m.cursorState),
    m.environment,
    m.editorSettings,
    m.permission,
    m.currentPage,
    m.tooltipState.tooltipSource,
    m.secrets,
    m.functions.packageFunctions |> Map.mapValues(~f=(t: PT.Package.Fn.t) => t.name.owner),
    m.settings.contributingSettings.general.showSidebarDebuggerPanel,
  ) |> Option.some

let viewSidebar = m => ViewCache.cache1m(rtCacheKey, viewSidebar_, m)
