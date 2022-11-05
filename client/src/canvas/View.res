open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

// Dark
module TL = Toplevel
module P = Pointer
module TD = TLID.Dict
module E = FluidExpression

open ProgramTypes.Expr

module Msg = AppTypes.Msg
type model = AppTypes.model
type msg = AppTypes.msg

let appID = "app"

let fontAwesome = Icons.fontAwesome

let docsURL = "https://docs.darklang.com"

let functionRefsURL = "https://ops-documentation.builtwithdark.com/?pretty=1"

let keyboardRefsURL = "https://docs.darklang.com/reference/keyboard-mapping"

let viewTL_ = (m: model, tl: toplevel): Html.html<msg> => {
  let tlid = TL.id(tl)
  let vs = ViewUtils.createVS(m, tl)
  let dragEvents = list{
    EventListeners.eventNoPropagation(
      ~key="tlmd-" ++ TLID.toString(tlid),
      "mousedown",
      x => Msg.TLDragRegionMouseDown(tlid, x),
    ),
    EventListeners.eventNoPropagation(
      ~key="tlmu-" ++ TLID.toString(tlid),
      "mouseup",
      x => Msg.TLDragRegionMouseUp(tlid, x),
    ),
  }

  let (body, data, draggable) = switch tl {
  | TLHandler(h) => (ViewHandler.view(vs, h, dragEvents), ViewData.viewData(vs), true)
  | TLDB(db) => (ViewDB.viewDB(vs, db, dragEvents), list{}, true)
  | TLPmFunc(f) => (list{ViewUserFunction.view(vs, PackageFn(f), false)}, list{}, false)
  | TLFunc(f) => (
      list{ViewUserFunction.view(vs, UserFunction(f), m.tooltipState.fnSpace)},
      ViewData.viewData(vs),
      false,
    )
  | TLType(t) => (list{ViewUserType.viewUserType(vs, t)}, list{}, false)
  }

  let usages = ViewIntrospect.allUsagesView(tlid, vs.usedInRefs, vs.refersToRefs)

  /* we capture and ignore mouseup here, otherwise clicking things inside the
   * toplevel (but not inside another element with a mouseup handler like
   * .fluid-editor) will bubble up into a "focus canvas" message and deselect
   * our toplevel. Since we capture mouseup, we must capture mousedown as well,
   * otherwise mousedown will begin canvas panning, but mouseup won't stop it. */
  let events = list{
    EventListeners.nothingMouseEvent("mouseup"),
    EventListeners.nothingMouseEvent("mousedown"),
  }

  /* This is a bit ugly - DBs have a larger 'margin' (not CSS margin) between
   * the encompassing toplevel div and the db div it contains, than  handlers.
   * Which leads to it being easy to hit "why won't this drag" if you click in
   * that margin. So for TLDBs, we want the whole toplevel div to be a draggable
   * region (and so we want to attach the dragEvents handlers to the that div,
   * and not just the db div).
   *
   * We do not want that for handlers, because:
   * - if you click _just_ outside of a text region, you probably want
   * click-drag-select behavior, not drag-handler behavior
   * - the 'margin' is smaller, so you are less likely to hit the "why won't
   * this drag" behavior mentioned above */
  let events = Belt.List.concat(
    events,
    switch tl {
    | TLDB(_) => dragEvents
    | _ => list{Vdom.noProp, Vdom.noProp}
    },
  )

  let avatars = Avatar.viewToplevelAvatars(m.avatarsList, tlid)
  let selected = Some(tlid) == CursorState.tlidOf(m.cursorState)
  let hovering = ViewUtils.isHoverOverTL(vs)
  let boxClasses = {
    let dragging = switch m.cursorState {
    | DraggingTL(tlid_, _, _, _) => tlid_ == tlid
    | _ => false
    }

    list{("selected", selected), ("dragging", dragging), ("hovering", hovering)}
  }

  // Need to add aditional css class to remove background color
  let classes = list{
    ("toplevel", true),
    ("tl-" ++ TLID.toString(tlid), true),
    ("selected", selected),
  }

  let id =
    FluidTokenizer.ASTInfo.getToken(vs.astInfo)
    |> Option.map(~f=(ti: FluidToken.tokenInfo) => FluidToken.tid(ti.token))
    |> Option.orElse(CursorState.idOf(m.cursorState))

  let top = {
    let p = (text: string) => Html.p(list{}, list{Html.text(text)})
    let viewDoc = desc =>
      Html.div(
        list{
          Attrs.classList(list{("documentation-box", true), ("draggable", draggable)}),
          ...dragEvents,
        },
        desc,
      )

    switch (CursorState.tlidOf(m.cursorState), id) {
    | (Some(tlid_), Some(id)) if tlid_ == tlid =>
      let acFnDocString = {
        let regular =
          m.complete
          |> Autocomplete.highlighted
          |> Option.andThen(~f=Autocomplete.documentationForItem)

        let desc =
          m.fluidState.ac
          |> FluidAutocomplete.highlightedWithValidity
          |> Option.andThen(~f=FluidAutocomplete.documentationForItem)
          |> Option.orElse(regular)

        Option.map(desc, ~f=desc => viewDoc(desc))
      }

      let selectedFnDocString = {
        let fnAndRail =
          TL.getAST(tl)
          |> Option.andThen(~f=ast => FluidAST.findExpr(id, ast))
          |> Option.andThen(~f=x =>
            switch x {
            | EFnCall(_, name, _, sendToRail) => Some(name, sendToRail)
            | EBinOp(_, name, _, _, sendToRail) =>
              Some(Stdlib(PT.InfixStdlibFnName.toStdlib(name)), sendToRail)
            | _ => None
            }
          )
          |> Option.andThen(~f=((name, sendToRail)) =>
            Functions.find(name, m.functions) |> Option.map(~f=f => (f, sendToRail))
          )

        switch fnAndRail {
        | Some(fn, sendToRail) =>
          Some(viewDoc(FluidAutocomplete.documentationForFunction(fn, Some(sendToRail))))
        | None => None
        }
      }

      let selectedParamDocString = {
        let paramAndFnDesc =
          TL.get(m, tlid)
          |> Option.andThen(~f=TL.getAST)
          |> Option.andThen(~f=AST.getParamIndex(id))
          |> Option.andThen(~f=((name, index)) =>
            m.functions
            |> Functions.findByStr(name)
            |> Option.map(~f=(f: Function.t) => {
              let param = f.parameters |> List.getAt(~index)
              (param, f)
            })
          )

        switch paramAndFnDesc {
        | Some(Some(param), f) =>
          let header = param.name ++ ": " ++ DType.type2str(param.typ)

          Some(
            viewDoc(
              Belt.List.concat(
                FluidAutocomplete.documentationForFunction(f, None),
                list{p(header), p(param.description)},
              ),
            ),
          )

        | _ => None
        }
      }

      let cmdDocString = if FluidCommands.isOpenOnTL(m.fluidState.cp, tlid) {
        FluidCommands.highlighted(m.fluidState.cp) |> Option.map(~f=(c: AppTypes.fluidCmd) =>
          viewDoc(list{p(c.doc)})
        )
      } else {
        None
      }

      acFnDocString
      |> Option.orElse(cmdDocString)
      |> Option.orElse(selectedParamDocString)
      |> Option.orElse(selectedFnDocString)
      |> Option.unwrap(~default=Vdom.noNode)
    | _ => Vdom.noNode
    }
  }

  let pos = switch m.currentPage {
  | Architecture | FocusedHandler(_) | FocusedDB(_) | SettingsModal(_) => TL.pos(tl)
  | FocusedPackageManagerFn(_) | FocusedFn(_) | FocusedType(_) => Pos.center
  }

  let hasFF = vs.astInfo.featureFlagTokenInfos != list{}
  let tooltip = switch m.tooltipState.userTutorial.step {
  | Some(step)
    if step == VerbChange || (step == ReturnValue || (step == OpenTab || step == GettingStarted)) =>
    UserTutorial.generateTutorialContent(step, m.username) |> Tooltips.viewToolTip(
      ~shouldShow=m.tooltipState.userTutorial.tlid == Some(tlid),
      ~tlid=Some(tlid),
    )
  | _ => Vdom.noNode
  }

  let html = list{
    Html.div(
      /* this unique key ensures that when switching between toplevels the entire
       * vdom node is rebuilt so that we don't get yelled at about the fact that
       * the property list or body nodes may be of differing length. Eg, DBs and
       * Fns have different property list lengths */
      ~unique=TLID.toString(tlid),
      list{Attrs.classList(classes), ...events},
      Belt.List.concatMany([list{top}, body, data]),
    ),
    avatars,
    Html.div(
      list{
        Attrs.classList(list{("use-wrapper", true), ("fade", hasFF)}),
        // Block opening the omnibox here by preventing canvas pan start
        EventListeners.nothingMouseEvent("mousedown"),
      },
      usages,
    ),
    tooltip,
  }

  ViewUtils.placeHtml(pos, boxClasses, html)
}

let tlCacheKey = (m: model, tl) => {
  let tlid = TL.id(tl)
  if Some(tlid) == CursorState.tlidOf(m.cursorState) {
    None
  } else {
    let hovered = switch List.head(m.hovering) {
    | Some(tlid_, id) if tlid_ == tlid => Some(id)
    | _ => None
    }

    let tracesLoaded = Analysis.getTraces(m, tlid) |> List.map(~f=((_, traceData)) =>
      switch traceData {
      | Ok(_) | Error(AnalysisTypes.TraceError.MaximumCallStackError) => true
      | _ => false
      }
    )

    let avatarsList = Avatar.filterAvatarsByTlid(m.avatarsList, tlid)
    let props = Map.get(~key=tlid, m.handlerProps)
    let menuIsOpen = TLMenu.isOpen(m, tlid)
    let workerSchedule = tl |> TL.asHandler |> Option.andThen(~f=Handlers.getWorkerSchedule(m))

    Some(
      tl,
      Analysis.getSelectedTraceID(m, tlid),
      hovered,
      tracesLoaded,
      avatarsList,
      props,
      menuIsOpen,
      workerSchedule,
      m.editorSettings.showHandlerASTs,
      m.tooltipState.userTutorial,
    )
  }
}

let tlCacheKeyDB = (m: model, tl) => {
  let tlid = TL.id(tl)
  if Some(tlid) == CursorState.tlidOf(m.cursorState) {
    None
  } else {
    let avatarsList = Avatar.filterAvatarsByTlid(m.avatarsList, tlid)
    let menuIsOpen = TLMenu.isOpen(m, tlid)
    Some(tl, DB.isLocked(m, tlid), avatarsList, menuIsOpen)
  }
}

let tlCacheKeyType = (m: model, tl) => {
  let tlid = TL.id(tl)
  if Some(tlid) == CursorState.tlidOf(m.cursorState) {
    None
  } else {
    let avatarsList = Avatar.filterAvatarsByTlid(m.avatarsList, tlid)
    Some(tl, avatarsList)
  }
}

let viewTL = (m, tl) =>
  switch tl {
  | TLType(_) => ViewCache.cache2m(tlCacheKeyType, viewTL_, m, tl)
  | TLDB(_) => ViewCache.cache2m(tlCacheKeyDB, viewTL_, m, tl)
  | TLPmFunc(_) | TLFunc(_) | TLHandler(_) => ViewCache.cache2m(tlCacheKey, viewTL_, m, tl)
  }

@ocaml.doc(" [zeroOutAppScrollImmediate ()] immediately forces the scroll of #app to 0,0.
 * Prefer [zeroOutAppScroll] if possible. ")
let zeroOutAppScrollImmediate = (): unit => {
  open Webapi.Dom
  Document.getElementById(document, appID)
  |> Option.map(~f=app => Element.scrollTo(app, 0.0, 0.0))
  |> recoverOpt("zeroOutAppScroll", ~default=())
}

@ocaml.doc(" [zeroOutAppScroll] returns a Tea.Cmd.t that forces the scroll of #app to 0,0.
 * We need the invariant of #app scrolled to 0,0 to be maintained in order for #canvas translate to work.
 * See https://www.notion.so/darklang/Positioning-Bug-8831a3e00a234e55856a85861512876e
 * for more information about this constraint and what happens if it is broken. ")
let zeroOutAppScroll: AppTypes.cmd = Tea.Cmd.call(_ => zeroOutAppScrollImmediate())

@ocaml.doc(" [isAppScrollZero ()] returns true if the scroll of #app is 0,0 and false otherwise.
 * We need the invariant of #app scrolled to 0,0 to be maintained in order for #canvas translate to work.
 * See https://www.notion.so/darklang/Positioning-Bug-8831a3e00a234e55856a85861512876e
 * for more information about this constraint and what happens if it is broken. ")
let isAppScrollZero = (): bool => {
  open Webapi.Dom
  Document.getElementById(document, appID)
  |> Option.map(~f=app => Element.scrollLeft(app) == 0.0 && Element.scrollTop(app) == 0.0)
  |> // Technically recoverOpt might be better here, but in some situations, #app doesn't exist yet
  Option.unwrap(~default=true)
}

let viewCanvas = (m: model): Html.html<msg> => {
  let allDivs = switch m.currentPage {
  | Architecture | FocusedHandler(_) | FocusedDB(_) | SettingsModal(_) =>
    m
    |> TL.structural
    |> Map.values
    |> /* TEA's vdom assumes lists have the same ordering, and diffs incorrectly
     * if not (though only when using our Util cache). This leads to the
     * clicks going to the wrong toplevel. Sorting solves it, though I don't
     * know exactly how. TODO: we removed the Util cache so it might work. */
    List.sortBy(~f=tl => TLID.toString(TL.id(tl)))
    |> List.map(~f=viewTL(m))
  | FocusedPackageManagerFn(tlid) =>
    switch Map.get(~key=tlid, m.functions.packageFunctions) {
    | Some(func) => list{viewTL(m, TL.pmfToTL(func))}
    | None => list{}
    }
  | FocusedFn(tlid, _) =>
    switch Map.get(~key=tlid, m.userFunctions) {
    | Some(func) => list{viewTL(m, TL.ufToTL(func))}
    | None => list{}
    }
  | FocusedType(tlid) =>
    switch Map.get(~key=tlid, m.userTypes) {
    | Some(typ) => list{viewTL(m, TL.utToTL(typ))}
    | None => list{}
    }
  }

  // BEGIN HACK
  /* This is a last-ditch effort to fix the position bug.
   * If recover doesn't happen in prod, we can remove this
   * for a performance boost. */
  if isAppScrollZero() {
    ()
  } else {
    recover("forcibly corrected position bug", zeroOutAppScrollImmediate())
  }
  // END HACK
  /* Note that the following translation is container relative,
   * so we must ensure that none of the parent elements are scrolled or otherwise moved. */
  let animationStyle = (
    "transition",
    if m.canvasProps.panAnimation == AnimateTransition {
      "transform 0.5s"
    } else {
      "unset"
    },
  )

  let (canvasStyles, overlayStyles) = {
    let offset = m.canvasProps.offset
    (
      /* The canvas is transformed to the inverse offset to move it such that the browser viewport
       * can act as a "camera" looking at the region of the nodes we've scrolled to. */
      list{
        animationStyle,
        ("transform", `translate(${Int.toString(-offset.x)}px, ${Int.toString(-offset.y)}px)`),
      },
      /* The overlay is inverse-transformed from the canvas so that it is always in the viewport.
       * We "undo" the transformation of the canvas since the overlay is a child of the canvas. */
      list{
        animationStyle,
        ("transform", `translate(${Int.toString(offset.x)}px, ${Int.toString(offset.y)}px)`),
      },
    )
  }

  let overlay = {
    let show = switch m.currentPage {
    | FocusedHandler(_) | FocusedDB(_) => true
    | Architecture =>
      switch CursorState.unwrap(m.cursorState) {
      | Omnibox(_) => true
      | _ => false
      }
    | _ => false
    }

    Html.div(
      list{
        Attrs.classList(list{("overlay", true), ("show", show)}),
        if show {
          Attrs.styles(overlayStyles)
        } else {
          Vdom.noProp
        },
      },
      list{},
    )
  }

  let pageClass = switch m.currentPage {
  | SettingsModal(_) => "settings-modal"
  | Architecture => "arch"
  | FocusedHandler(_) => "focused-handler"
  | FocusedDB(_) => "focused-db"
  | FocusedPackageManagerFn(_) => "focused-package-manager-fn"
  | FocusedFn(_) => "focused-fn"
  | FocusedType(_) => "focused-type"
  }

  Html.div(
    list{
      Attrs.id("canvas"),
      Attrs.class'("canvas " ++ pageClass),
      Attrs.styles(canvasStyles),
      EventListeners.onTransitionEnd(~key="canvas-pan-anim", ~listener=prop =>
        if prop == "transform" {
          Msg.CanvasPanAnimationEnd
        } else {
          AppTypes.Msg.IgnoreMsg("canvas-pan-end")
        }
      ),
    },
    list{overlay, ...allDivs},
  )
}

let viewBackToCanvas = (currentPage: AppTypes.Page.t, showTooltip: bool): Html.html<msg> =>
  switch currentPage {
  | FocusedFn(_) =>
    let helpIcon = Html.div(
      list{
        Attrs.class'("help-icon"),
        EventListeners.eventNoPropagation(~key="ept", "mouseenter", _ => Msg.ToolTipMsg(
          OpenFnTooltip(true),
        )),
        EventListeners.eventNoPropagation(~key="epf", "mouseleave", _ => Msg.ToolTipMsg(
          OpenFnTooltip(false),
        )),
      },
      list{fontAwesome("question-circle")},
    )
    let tooltip =
      Tooltips.generateContent(FnBackToCanvas) |> Tooltips.viewToolTip(
        ~shouldShow=showTooltip,
        ~tlid=None,
      )

    Html.div(
      list{Attrs.id("back-to-canvas"), Attrs.class'("back-to-canvas")},
      list{
        tooltip,
        Html.div(
          list{
            Attrs.class'("back-to-canvas-content"),
            Vdom.prop("alt", "architecture preview"),
            EventListeners.eventNoPropagation(~key="return-to-arch", "click", _ =>
              Msg.GoToArchitecturalView
            ),
          },
          list{
            helpIcon,
            Html.a(list{Attrs.class'("content")}, list{Vdom.text("Return to main canvas")}),
          },
        ),
      },
    )
  | _ => Vdom.noNode
  }

let viewToast = (t: AppTypes.Toast.t): Html.html<msg> => {
  let msg = Option.unwrap(~default="", t.message)
  let classes = if Option.isSome(t.message) {
    "toast show"
  } else {
    "toast"
  }

  let styleOverrides = switch t.pos {
  | Some({vx, vy}) =>
    Attrs.styles(list{
      ("top", string_of_int(vy - 10) ++ "px"),
      ("left", string_of_int(vx + 10) ++ "px"),
    })
  | None => Vdom.noProp
  }

  Html.div(
    list{
      Attrs.class'(classes),
      EventListeners.onAnimationEnd(~key="toast", ~listener=_ => Msg.ResetToast),
      styleOverrides,
    },
    list{Html.text(msg)},
  )
}

let accountView = (m: model): Html.html<msg> => {
  let logout = Html.a(
    list{Attrs.class'("account-action-btn"), Attrs.href("https://login.darklang.com/logout")},
    list{Html.text("Logout")},
  )

  let docs = Html.a(
    list{
      Attrs.class'("account-action-btn"),
      Attrs.href(docsURL),
      Attrs.target("_blank"),
      EventListeners.eventNoPropagation(~key="account-doc", "click", _ => Msg.UpdateHeapio(
        OpenDocs,
      )),
    },
    list{Html.text("Documentation")},
  )

  let functionRefs = Html.a(
    list{
      Attrs.class'("account-action-btn"),
      Attrs.href(functionRefsURL),
      Attrs.target("_blank"),
      EventListeners.eventNoPropagation(~key="account-fn-ref", "click", _ => Msg.UpdateHeapio(
        OpenFnRef,
      )),
    },
    list{Html.text("Function Reference")},
  )

  let keyboardRefs = Html.a(
    list{
      Attrs.class'("account-action-btn"),
      Attrs.href(keyboardRefsURL),
      Attrs.target("_blank"),
      EventListeners.eventNoPropagation(~key="account-fn-ref", "click", _ => Msg.UpdateHeapio(
        OpenKeyboardRef,
      )),
    },
    list{Html.text("Keyboard Reference")},
  )

  let discordRef = Html.a(
    list{
      Attrs.class'("account-action-btn"),
      Attrs.href("https://darklang.com/discord-invite"),
      Attrs.target("_blank"),
      EventListeners.eventNoPropagation(~key="discord-invite-ref", "click", _ => Msg.UpdateHeapio(
        OpenKeyboardRef,
      )),
    },
    list{Html.text("Discord Community")},
  )

  let contributeRef = Html.a(
    list{
      Attrs.class'("account-action-btn"),
      Attrs.href("https://docs.darklang.com/contributing/getting-started"),
      Attrs.target("_blank"),
      EventListeners.eventNoPropagation(~key="contributor-ref", "click", _ => Msg.UpdateHeapio(
        OpenKeyboardRef,
      )),
    },
    list{Html.text("Contributor Docs")},
  )

  let tutorial = Html.p(
    list{
      Attrs.class'("account-action-btn"),
      EventListeners.eventNoPropagation(~key="tutorial", "click", _ => Msg.ToolTipMsg(
        UpdateTutorial(ReopenTutorial),
      )),
    },
    list{Html.text("Hello World tutorial")},
  )

  let spacer = Html.div(list{Attrs.class'("account-action-spacer")}, list{})
  let settings = Html.p(
    list{
      Attrs.class'("account-action-btn"),
      EventListeners.eventNoPropagation(~key="open-settings", "click", _ => Msg.SettingsMsg(
        Open(Canvases),
      )),
    },
    list{Html.text("Settings")},
  )

  let share = Html.p(
    list{
      Attrs.class'("account-action-btn invite"),
      EventListeners.eventNoPropagation(~key="open-invite", "click", _ => Msg.SettingsMsg(
        Open(Invite),
      )),
    },
    list{Html.text("Share Dark")},
  )

  let tooltip = {
    let (shouldShow, ttContent) = if (
      m.firstVisitToThisCanvas &&
      UserTutorial.isTutorialCanvas(~username=m.username, ~canvasname=m.canvasName)
    ) {
      (true, UserTutorial.generateCRUDContent)
    } else {
      (
        m.tooltipState.userTutorial.step == Some(Welcome),
        UserTutorial.generateTutorialContent(Welcome, m.username),
      )
    }

    ttContent |> Tooltips.viewToolTip(~shouldShow, ~tlid=m.tooltipState.userTutorial.tlid)
  }

  Html.div(
    list{
      Attrs.class'("my-account"),
      // Block opening the omnibox here by preventing canvas pan start
      EventListeners.nothingMouseEvent("mousedown"),
    },
    list{
      m->Avatar.myAvatar->Avatar.avatarDiv(~style=Avatar.Styles.main),
      tooltip,
      Html.div(
        list{Attrs.class'("account-actions")},
        list{
          settings,
          share,
          logout,
          spacer,
          docs,
          functionRefs,
          keyboardRefs,
          discordRef,
          contributeRef,
          tutorial,
        },
      ),
    },
  )
}

let view = (m: model): Html.html<msg> => {
  let eventListeners = /* We don't want propagation because we don't want to double-handle these events and
   * window has its own listeners. */
  list{
    EventListeners.eventNeither(~key="app-md", "mousedown", mouseEvent => Msg.AppMouseDown(
      mouseEvent,
    )),
    EventListeners.eventNeither(~key="app-mu", "mouseup", mouseEvent => Msg.AppMouseUp(mouseEvent)),
    EventListeners.scrollEventNeither(~key="app-scroll", "scroll", _ => Msg.AppScroll),
  }

  let attributes = list{Attrs.id(appID), ...eventListeners}

  let footer = list{
    ViewScaffold.viewIntegrationTestButton(m.integrationTestState),
    ViewScaffold.readOnlyMessage(m),
    viewBackToCanvas(m.currentPage, m.tooltipState.fnSpace),
    ViewScaffold.viewError(m.error),
  }

  let sidebar = ViewSidebar.viewSidebar(m)
  let body = viewCanvas(m)
  let entry = ViewEntry.viewEntry(m)
  let activeAvatars = Avatar.viewAllAvatars(m.avatarsList)
  let ast = TL.selectedAST(m) |> Option.unwrap(~default=FluidAST.ofExpr(EBlank(gid())))

  let fluidStatus = if m.editorSettings.showFluidDebugger {
    list{FluidDebugger.view(m, ast)}
  } else {
    list{Vdom.noNode}
  }

  let viewDocs = list{
    Html.a(
      list{
        Attrs.class'("doc-container"),
        Attrs.href(docsURL),
        Attrs.target("_blank"),
        // Block opening the omnibox here by preventing canvas pan start
        EventListeners.nothingMouseEvent("mousedown"),
        EventListeners.eventNoPropagation(~key="doc", "click", _ => Msg.UpdateHeapio(OpenDocs)),
      },
      list{fontAwesome("book"), Html.text("Docs")},
    ),
  }

  let modal = ViewModal.unsupportedBrowser(
    ~show=m.integrationTestState == NoIntegrationTest && m.unsupportedBrowser,
  )

  let settingsModal = if m.settings.opened {
    SettingsView.html(m)
  } else {
    Vdom.noNode
  }

  let content = Belt.List.concatMany([
    list{SettingsPrivacyView.viewTopbar(m.settings.privacySettings)},
    ViewTopbar.html(m),
    list{
      sidebar,
      body,
      activeAvatars,
      accountView(m),
      viewToast(m.toast),
      entry,
      modal,
      settingsModal,
      InsertSecret.view(m.insertSecretModal),
    },
    fluidStatus,
    footer,
    viewDocs,
  ])

  Html.div(attributes, content)
}
