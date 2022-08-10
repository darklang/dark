module PT = ProgramTypes

let opaque = Types.opaque

module Page = {
  @ppx.deriving(show({with_path: false}))
  type rec center = bool

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | Architecture
    | FocusedPackageManagerFn(TLID.t)
    | FocusedFn(TLID.t, option<Types.traceID>)
    | FocusedHandler(TLID.t, option<Types.traceID>, center)
    | FocusedDB(TLID.t, center)
    | FocusedType(TLID.t)
    | SettingsModal(SettingsState.Tab.t)
}

module VPos = {
  // There are two coordinate systems. Pos is an absolute position in the
  // canvas. Nodes and Edges have Pos'. VPos is the viewport: clicks occur
  // within the viewport and we map Absolute positions back to the
  // viewport to display in the browser.
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    vx: int,
    vy: int,
  }

  let encode = (vp: t) => {
    open Json_encode_extended
    object_(list{("vx", int(vp.vx)), ("vy", int(vp.vy))})
  }

  let decode = (j): t => {
    open Json_decode_extended
    {vx: field("vx", int, j), vy: field("vy", int, j)}
  }
  let default: t = {vx: 475, vy: 200}
}

module Toast = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    // CLEANUP: remove prefixes
    message: option<string>,
    pos: option<VPos.t>,
  }
  let default: t = {message: None, pos: None}
}

module SyncState = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Tc.Set.String.t
}

module CanvasProps = {
  @ppx.deriving(show({with_path: false}))
  type rec isTransitionAnimated =
    | AnimateTransition
    | DontAnimateTransition

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    offset: Pos.t,
    enablePan: bool,
    lastOffset: option<Pos.t>,
    panAnimation: isTransitionAnimated,
  }

  let default: t = {
    offset: Pos.origin,
    enablePan: true,
    lastOffset: None,
    panAnimation: DontAnimateTransition,
  }
}

module PageVisibility = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | Hidden
    | Visible
}

module AutoComplete = {
  // Some AC items needs to be dynamically added to the list, while others can be
  // filtered in and out of the list.  For example: Goto will take you to focus on a
  // toplevel.  In the case of "Jump to", results are filtered by name, and do not need
  // to be dynamically generated.  But in the case of "Found in", results are
  // dynamically generated, based on the content that is inside.
  @ppx.deriving(show({with_path: false}))
  type rec isDynamic = bool

  @ppx.deriving(show({with_path: false}))
  type rec omniAction =
    | NewDB(option<string>)
    | NewFunction(option<string>)
    | NewHTTPHandler(option<string>)
    | NewWorkerHandler(option<string>)
    | NewCronHandler(option<string>)
    | NewReplHandler(option<string>)
    | Goto(Page.t, TLID.t, string, isDynamic)

  @ppx.deriving(show({with_path: false}))
  type rec item =
    | ACOmniAction(omniAction)
    // HTTP
    | ACHTTPModifier(string)
    | ACHTTPRoute(string)
    // Workers
    | ACWorkerName(string)
    | ACEventSpace(string)
    | ACEventModifier(string)
    // Repl
    | ACReplName(string)
    // CRON
    | ACCronName(string)
    | ACCronTiming(string)
    // DBs
    | ACDBName(string)
    | ACDBColType(string)
    | ACDBColName(string)
    // User functions
    | ACFnName(string)
    | ACParamName(string)
    | ACParamTipe(DType.t)
    | ACReturnTipe(DType.t)
    // User types
    | ACTypeFieldTipe(DType.t)
    | ACTypeName(string)
    | ACTypeFieldName(string)

  @ppx.deriving(show({with_path: false}))
  type rec target = (TLID.t, Types.blankOrData)

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    completions: list<item>,
    allCompletions: list<item>,
    index: int,
    value: string,
    prevValue: string,
    target: option<target>,
    visible: bool,
  }

  let default: t = {
    completions: list{},
    target: None,
    allCompletions: list{},
    index: -1,
    value: "",
    prevValue: "",
    visible: true,
  }

  @ppx.deriving(show({with_path: false}))
  type rec mod =
    | ACSetQuery(string)
    | ACReset
    | ACSelectDown
    | ACSelectUp
    | ACSetTarget(option<target>)
    | ACRegenerate
    | ACSetVisible(bool)
}

module CursorState = {
  @ppx.deriving(show({with_path: false}))
  type rec hasMoved = bool

  /* CursorState represents what the user is focussed on and where their actions
   * (notably keypresses, but also things like autocomplete and refactoring) are
   * intended to work on */
  @ppx.deriving(show({with_path: false}))
  type rec t =
    /* Show the onmibox. If we know the position the user wants the handler
     * to be at (presumably because they clicked there to get the omnibox),
     * then use it. Otherwise, if there's no position, we'll pick one for them
     * later */
    | Omnibox(option<Pos.t>)
    /* Partially deprecated. This used to indicate when you had selected a
     * blankOr, but were not "entering" it. However, this mostly only made
     * sense for code, and now that code is fluid this is just annoying and
     * weird. */
    | Selecting(TLID.t, option<ID.t>)
    // When we're editing a blankOr
    | Entering(TLID.t, ID.t)
    // When we're editing code (in the fluid editor)
    | FluidEntering(TLID.t)
    // When we're dragging toplevels - the old state is stored and reset after moving
    // is done
    | DraggingTL(TLID.t, VPos.t, hasMoved, t)
    // For dragging the canvas. The old state is stored and reset later
    | PanningCanvas({viewportStart: VPos.t, viewportCurr: VPos.t, prevCursorState: t})
    // Doing nothing
    | Deselected

  let rec encode = (cs: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch cs {
    | Selecting(tlid, mId) => ev("Selecting", list{TLID.encode(tlid), nullable(ID.encode, mId)})
    | Omnibox(maybePos) => ev("OmniBox", list{nullable(Pos.encode, maybePos)})
    | Entering(tlid, id) => ev("Entering", list{TLID.encode(tlid), ID.encode(id)})
    | DraggingTL(tlid_, vpos_, hasMoved, cursor) =>
      ev("DraggingTL", list{TLID.encode(tlid_), VPos.encode(vpos_), bool(hasMoved), encode(cursor)})
    | PanningCanvas({viewportStart, viewportCurr, prevCursorState}) =>
      ev(
        "PanningCanvas",
        list{VPos.encode(viewportStart), VPos.encode(viewportCurr), encode(prevCursorState)},
      )
    | Deselected => ev("Deselected", list{})
    | FluidEntering(tlid) => ev("FluidEntering", list{TLID.encode(tlid)})
    }
  }

  let rec decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    let dv0 = variant0
    let dv1 = variant1
    let dv2 = variant2
    let dv3 = variant3
    let dv4 = variant4
    variants(
      list{
        ("Selecting", dv2((a, b) => Selecting(a, b), TLID.decode, optional(ID.decode))),
        ("Omnibox", dv1(x => Omnibox(x), optional(Pos.decode))),
        ("Entering", dv2((x, y) => Entering(x, y), TLID.decode, ID.decode)),
        (
          "Dragging",
          dv4((a, b, c, d) => DraggingTL(a, b, c, d), TLID.decode, VPos.decode, bool, decode),
        ), // Deprecated via DraggingTL
        (
          "DraggingTL",
          dv4((a, b, c, d) => DraggingTL(a, b, c, d), TLID.decode, VPos.decode, bool, decode),
        ),
        ("PanningCanvas", dv3((viewportStart, viewportCurr, prevCursorState) => PanningCanvas({
            viewportStart: viewportStart,
            viewportCurr: viewportCurr,
            prevCursorState: prevCursorState,
          }), VPos.decode, VPos.decode, decode)),
        ("Deselected", dv0(Deselected)), // Old value
        ("SelectingCommand", dv2((a, b) => Selecting(a, Some(b)), TLID.decode, ID.decode)),
        ("FluidEntering", dv1(a => FluidEntering(a), TLID.decode)),
        ("FluidMouseSelecting", dv1(a => FluidEntering(a), TLID.decode)),
      },
      j,
    )
  }
}

module FunctionParams = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    draggingParamIndex: option<int>,
    dragOverSpaceIndex: option<int>,
    justMovedParam: option<ID.t>,
  }

  @ppx.deriving(show({with_path: false}))
  type rec msg =
    | ParamDragStart(int)
    | ParamDragDone
    | ParamEntersSpace(int)
    | ParamLeavesSpace
    | ParamDropIntoSpace(int)
    | Reset

  let default: t = {
    draggingParamIndex: None,
    dragOverSpaceIndex: None,
    justMovedParam: None,
  }
}

module Focus = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | FocusNothing
    | FocusExact(TLID.t, ID.t)
    | FocusNext(TLID.t, option<ID.t>)
    | FocusPageAndCursor(Page.t, CursorState.t)
    | FocusSame
    // unchanged
    | FocusNoChange
}

module ScrollEvent = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {timestamp: float}
  let decode = (j: Js.Json.t): t => {
    open Json.Decode
    {
      timestamp: field("timeStamp", Json.Decode.float, j),
    }
  }
}

module MouseEvent = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    mePos: VPos.t,
    button: int,
    altKey: bool,
    ctrlKey: bool,
    shiftKey: bool,
    detail: int,
  }
  let decode = (j: Js.Json.t): t => {
    open Json.Decode
    {
      // We decode floats b/c newer Chromes may use floats instead of ints; we
      // when truncate rather than moving to floats everywhere due to concerns
      // about sending data back to browsers whose DOMs don't support float
      // positions - see https://github.com/darklang/dark/pull/2016 for
      // discussion, and
      // https://drafts.csswg.org/cssom-view/#extensions-to-the-window-interface
      // for the spec
      mePos: {
        vx: field("pageX", Json.Decode.float, j) |> truncate,
        vy: field("pageY", Json.Decode.float, j) |> truncate,
      },
      button: field("button", int, j),
      ctrlKey: field("ctrlKey", bool, j),
      shiftKey: field("shiftKey", bool, j),
      altKey: field("altKey", bool, j),
      detail: field("detail", int, j),
    }
  }
}

module Sidebar = {
  module Mode = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | DetailedMode
      | AbridgedMode

    let encode = (s: t): Js.Json.t => {
      open Json_encode_extended
      switch s {
      | DetailedMode => variant("DetailedMode", list{})
      | AbridgedMode => variant("AbridgedMode", list{})
      }
    }

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      j |> variants(list{
        ("DetailedMode", variant0(DetailedMode)),
        ("AbridgedMode", variant0(AbridgedMode)),
      })
    }
  }

  module State = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      mode: Mode.t,
      openedCategories: Tc.Set.String.t,
    }

    let encode = (s: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("mode", Mode.encode(s.mode)), ("openedCategories", strSet(s.openedCategories))})
    }

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      {
        mode: field("mode", Mode.decode, j),
        openedCategories: withDefault(Tc.Set.String.empty, field("openedCategories", strSet), j),
      }
    }

    let default = {mode: DetailedMode, openedCategories: Tc.Set.String.empty}
  }
  @ppx.deriving(show({with_path: false}))
  type rec msg =
    | ToggleSidebarMode
    | ResetSidebar
    | MarkCategoryOpen(bool, string)
}

module Tutorial = {
  module Step = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | Welcome
      | VerbChange
      | ReturnValue
      | OpenTab
      | GettingStarted

    let encode = (t: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch t {
      | Welcome => ev("Welcome", list{})
      | VerbChange => ev("VerbChange", list{})
      | ReturnValue => ev("ReturnValue", list{})
      | OpenTab => ev("OpenTab", list{})
      | GettingStarted => ev("GettingStarted", list{})
      }
    }
    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      j |> variants(list{
        ("Welcome", variant0(Welcome)),
        ("VerbChange", variant0(VerbChange)),
        ("ReturnValue", variant0(ReturnValue)),
        ("OpenTab", variant0(OpenTab)),
        ("GettingStarted", variant0(GettingStarted)),
      })
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec msg =
    | NextStep
    | PrevStep
    | CloseTutorial
    | ReopenTutorial

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    step: option<Step.t>,
    tlid: option<TLID.t>,
  }
  let default: t = {step: Some(Welcome), tlid: None}
}

module Avatar = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    canvasId: string,
    canvasName: string,
    @opaque serverTime: Js.Date.t,
    tlid: option<string>,
    username: string,
    email: string,
    fullname: option<string>,
    browserId: string,
  }
  let decode = (j): t => {
    open Json_decode_extended
    {
      canvasId: field("canvasId", string, j),
      canvasName: field("canvasName", string, j),
      tlid: field("tlid", optional(string), j),
      username: field("username", string, j),
      serverTime: field("serverTime", field("value", date), j),
      email: field("email", string, j),
      fullname: field("name", optional(string), j),
      browserId: field("browserId", string, j),
    }
  }
}

module Menu = {
  @ppx.deriving(show({with_path: false}))
  type rec msg =
    | OpenMenu
    | CloseMenu

  @ppx.deriving(show({with_path: false}))
  type rec t = {isOpen: bool}

  let default: t = {isOpen: false}
}

module Tooltip = {
  @ppx.deriving(show({with_path: false}))
  type rec source =
    | Http
    | Worker
    | Cron
    | Repl
    | Datastore
    | Function
    | FourOhFour
    | Deleted
    | PackageManager
    | StaticAssets
    | FnParam
    | FnBackToCanvas
    | Secrets

  @ppx.deriving(show({with_path: false}))
  type rec msg =
    | OpenTooltip(source)
    | Close
    | OpenLink(string)
    | OpenFnTooltip(bool)
    | UpdateTutorial(Tutorial.msg)

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    tooltipSource: option<source>,
    fnSpace: bool,
    userTutorial: Tutorial.t,
  }

  let default: t = {
    tooltipSource: None,
    fnSpace: false,
    userTutorial: {step: Some(Welcome), tlid: None},
  }
}

module HandlerProperty = {
  module ExecutionState = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | Idle
      | Executing
      | Complete

    let decode = (j): t => {
      open Json_decode_extended
      j |> variants(list{
        ("Idle", variant0(Idle)),
        ("Executing", variant0(Executing)),
        ("Complete", variant0(Complete)),
      })
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    // When hovering over a reference, this is the list of ID.ts that refer to
    // the reference
    hoveringReferences: list<ID.t>,
    execution: ExecutionState.t,
  }

  // None of these properties makes sense to save on a refresh
  let encode = (_p: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{})
  }

  let decode = (j): t => {
    open Json_decode_extended
    {
      hoveringReferences: field("hoveringReferences", list(ID.decode), j),
      execution: field("executing", ExecutionState.decode, j),
    }
  }

  let default: t = {hoveringReferences: list{}, execution: Idle}
}

module IntegrationTests = {
  @ppx.deriving(show({with_path: false}))
  type rec testResult = Tc.Result.t<unit, string>

  @ppx.deriving(show({with_path: false}))
  type rec t<'model> =
    | IntegrationTestExpectation('model => testResult)
    | IntegrationTestFinished(testResult)
    | NoIntegrationTest
}

module EditorSettings = {
  @ppx.deriving(show({with_path: false}))
  type rec ffIsExpanded = bool

  @ppx.deriving(show({with_path: false}))
  type rec flagsVS = Tc.Map.String.t<ffIsExpanded>

  // Editor settings are global settings on the editor. Initially, these are only
  // things that admins use for debugging - in the future they could be extended to
  // actual editor settings
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    showFluidDebugger: bool,
    showHandlerASTs: bool,
    runTimers: bool,
  }

  let encode = (es: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("runTimers", bool(es.runTimers)),
      ("showHandlerASTs", bool(es.showHandlerASTs)),
      ("showFluidDebugger", bool(es.showFluidDebugger)),
    })
  }
  let default: t = {runTimers: true, showHandlerASTs: false, showFluidDebugger: false}
}

module SavedSettings = {
  module User = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      firstVisitToDark: bool,
      recordConsent: option<bool>,
    }

    let encode = (se: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("showUserWelcomeModal", bool(se.firstVisitToDark)),
        ("firstVisitToDark", bool(se.firstVisitToDark)),
        ("recordConsent", nullable(bool, se.recordConsent)),
      })
    }

    let default: t = {
      firstVisitToDark: true,
      recordConsent: None,
    }

    let decode = (j: Js.Json.t): t => {
      // It's very important that this is generous in parsing old values and missing
      // values and in general never failing.
      open Json_decode_extended
      let oldFirstVisitToDark = withDefault(
        default.firstVisitToDark,
        field("showUserWelcomeModal", bool),
        j,
      )

      let newFirstVisitToDark = withDefault(
        default.firstVisitToDark,
        field("firstVisitToDark", bool),
        j,
      )

      {
        firstVisitToDark: oldFirstVisitToDark || newFirstVisitToDark,
        recordConsent: withDefault(None, field("recordConsent", optional(bool)), j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    editorSettings: EditorSettings.t,
    cursorState: CursorState.t,
    tlTraceIDs: Types.tlTraceIDs,
    handlerProps: TLID.Dict.t<HandlerProperty.t>,
    canvasPos: Pos.t,
    lastReload: option<@opaque Js.Date.t>,
    sidebarState: Sidebar.State.t,
    showTopbar: bool,
    firstVisitToThisCanvas: bool,
    userTutorial: option<Tutorial.Step.t>,
    userTutorialTLID: option<TLID.t>,
  }
  let default: t = {
    editorSettings: EditorSettings.default,
    cursorState: Deselected,
    tlTraceIDs: TLID.Dict.empty,
    handlerProps: TLID.Dict.empty,
    canvasPos: Pos.origin,
    lastReload: None,
    sidebarState: Sidebar.State.default,
    showTopbar: false,
    firstVisitToThisCanvas: true,
    userTutorial: None,
    userTutorialTLID: None,
  }

  let encode = (se: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("editorSettings", EditorSettings.encode(se.editorSettings)),
      ("cursorState", CursorState.encode(se.cursorState)),
      ("tlTraceIDs", TLID.Dict.encode(string, se.tlTraceIDs)),
      ("handlerProps", TLID.Dict.encode(HandlerProperty.encode, se.handlerProps)),
      ("canvasPos", Pos.encode(se.canvasPos)),
      ("lastReload", nullable(date, se.lastReload)),
      ("sidebarState", Sidebar.State.encode(se.sidebarState)),
      ("showTopbar1", bool(se.showTopbar)),
      ("firstVisitToThisCanvas", bool(se.firstVisitToThisCanvas)),
      ("userTutorial", nullable(Tutorial.Step.encode, se.userTutorial)),
      ("userTutorialTLID", nullable(TLID.encode, se.userTutorialTLID)),
    })
  }
  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    // always use withDefault or optional because the field might be missing due
    // to old editors or new fields.
    {
      editorSettings: {
        runTimers: withDefault(true, field("editorSettings", field("runTimers", bool)), j),
        showHandlerASTs: withDefault(
          false,
          field("editorSettings", field("showHandlerASTs", bool)),
          j,
        ),
        showFluidDebugger: withDefault(
          false,
          field("editorSettings", field("showFluidDebugger", bool)),
          j,
        ),
      },
      cursorState: withDefault(CursorState.Deselected, field("cursorState", CursorState.decode), j),
      tlTraceIDs: withDefault(TLID.Dict.empty, field("tlTraceIDs", TLID.Dict.decode(string)), j),
      handlerProps: withDefault(
        TLID.Dict.empty,
        field("handlerProps", TLID.Dict.decode(HandlerProperty.decode)),
        j,
      ),
      canvasPos: withDefault(Pos.origin, field("canvasPos", Pos.decode), j),
      lastReload: optional(field("lastReload", date), j),
      sidebarState: withDefault(
        Sidebar.State.default,
        field("sidebarState", Sidebar.State.decode),
        j,
      ),
      showTopbar: withDefault(default.showTopbar, field("showTopbar1", bool), j),
      firstVisitToThisCanvas: withDefault(
        default.firstVisitToThisCanvas,
        field("firstVisitToThisCanvas", bool),
        j,
      ),
      userTutorial: withDefault(
        default.userTutorial,
        field("userTutorial", optional(Tutorial.Step.decode)),
        j,
      ),
      userTutorialTLID: withDefault(
        default.userTutorialTLID,
        field("userTutorialTLID", optional(TLID.decode)),
        j,
      ),
    }
  }
}

module Msg = {
  @ppx.deriving(show({with_path: false}))
  type rec t<'model, 'modification> =
    | IgnoreMsg(/* debug string so you know where it came from */ string)
    | IgnoreMouseUp // for nothingMouseEvent
    | FluidMsg(FluidTypes.Msg.t<'model, 'modification>)
    | AppMouseDown(MouseEvent.t)
    | @printer(opaque("AppMouseDrag")) AppMouseDrag(Tea.Mouse.position)
    | AppMouseUp(MouseEvent.t)
    | AppScroll
    | WindowMouseUp(MouseEvent.t)
    | TLDragRegionMouseDown(TLID.t, MouseEvent.t)
    // we have the actual node when TLDragRegionMouseUp is created,
    // but by the time we use it the proper node will be changed
    | TLDragRegionMouseUp(TLID.t, MouseEvent.t)
    | ToplevelDelete(TLID.t)
    | ToplevelDeleteForever(TLID.t)
    | @printer(opaque("DragToplevel")) DragToplevel(TLID.t, Tea.Mouse.position)
    | EntryInputMsg(string)
    | EntrySubmitMsg
    | GlobalKeyPress(Keyboard.keyEvent)
    | AutocompleteClick(int)
    | @printer(opaque("AddOpsAPICallback"))
    AddOpsAPICallback(Focus.t, APIAddOps.Params.t, Tea.Result.t<APIAddOps.t, Types.httpError>)
    | AddOpsPusherMsg(PusherTypes.AddOps.t)
    | @printer(opaque("SavetestAPICallback"))
    SaveTestAPICallback(Tea.Result.t<APISaveTest.t, Types.httpError>)
    | @printer(opaque("GetUnlockedDBsAPICallback"))
    GetUnlockedDBsAPICallback(Tea.Result.t<APIDBs.UnlockedDBs.t, Types.httpError>)
    | @printer(opaque("Get404sAPICallback"))
    Get404sAPICallback(Tea.Result.t<API404.List.t, Types.httpError>)
    | NewTracePush(AnalysisTypes.NewTrace.t)
    | New404Push(AnalysisTypes.FourOhFour.t)
    | NewStaticDeployPush(StaticAssets.Deploy.t)
    | WorkerStatePush(Tc.Map.String.t<AnalysisTypes.WorkerState.t>)
    | @printer(opaque("Delete404APICallback"))
    Delete404APICallback(
        AnalysisTypes.FourOhFour.t,
        API404.Delete.Params.t,
        Tea.Result.t<unit, Types.httpError>,
      )
    | @printer(opaque("DeleteToplevelForeverAPICallback"))
    DeleteToplevelForeverAPICallback(
        APIToplevels.DeleteForever.Params.t,
        Tea.Result.t<unit, Types.httpError>,
      )
    | @printer(opaque("InitialLoadAPICallback"))
    InitialLoadAPICallback(Focus.t, 'modification, Tea.Result.t<APIInitialLoad.t, Types.httpError>)
    | @printer(opaque("FetchAllTracesAPICallback"))
    FetchAllTracesAPICallback(Tea.Result.t<APITraces.AllTraces.t, Types.httpError>)
    | @printer(opaque("ExecuteFunctionAPICallback"))
    ExecuteFunctionAPICallback(
        APIExecution.Function.Params.t,
        Tea.Result.t<APIExecution.Function.t, Types.httpError>,
      )
    | @printer(opaque("UploadFunctionAPICallback"))
    UploadFnAPICallback(APIPackages.UploadFn.Params.t, Tea.Result.t<unit, Types.httpError>)
    | @printer(opaque("TriggerHandlerAPICallback"))
    TriggerHandlerAPICallback(
        APIExecution.Handler.Params.t,
        Tea.Result.t<APIExecution.Handler.t, Types.httpError>,
      )
    | @printer(opaque("LoadPackagesAPICallback"))
    LoadPackagesAPICallback(Tea.Result.t<APIPackages.AllPackages.t, Types.httpError>)
    | @printer(opaque("InsertSecretCallback"))
    InsertSecretCallback(Tea.Result.t<list<SecretTypes.t>, Types.httpError>)
    | @printer(opaque("LogoutAPICallback")) LogoutAPICallback
    | Delete404APICall(AnalysisTypes.FourOhFour.t)
    | NewPresencePush(list<Avatar.t>)
    | @printer(opaque("LocationChange")) LocationChange(Web.Location.location)
    | FinishIntegrationTest
    | SaveTestButton
    | ToggleEditorSetting(EditorSettings.t => EditorSettings.t)
    | ExecuteFunctionButton(TLID.t, ID.t, string)
    | ExecuteFunctionFromWithin(APIExecution.Function.Params.t)
    | CreateHandlerFrom404(AnalysisTypes.FourOhFour.t)
    | @printer(opaque("TimerFire")) TimerFire(Types.timerAction, Tea.Time.t)
    | JSError(string)
    | PageVisibilityChange(PageVisibility.t)
    | DeleteUserFunctionParameter(TLID.t, PT.UserFunction.Parameter.t)
    | AddUserFunctionParameter(TLID.t)
    | UploadFn(TLID.t)
    | DeleteUserTypeField(TLID.t, PT.UserType.RecordField.t)
    | BlankOrClick(TLID.t, ID.t, MouseEvent.t)
    | BlankOrDoubleClick(TLID.t, ID.t, MouseEvent.t)
    | BlankOrMouseEnter(TLID.t, ID.t, MouseEvent.t)
    | BlankOrMouseLeave(TLID.t, ID.t, MouseEvent.t)
    | MouseWheel(int, int)
    | TraceClick(TLID.t, Types.traceID, MouseEvent.t)
    | TraceMouseEnter(TLID.t, Types.traceID, MouseEvent.t)
    | TraceMouseLeave(TLID.t, Types.traceID, MouseEvent.t)
    | TriggerHandler(TLID.t)
    | CreateRouteHandler(AutoComplete.omniAction)
    | SidebarMsg(Sidebar.msg)
    | CreateFunction
    | ExtractFunction
    | CreateType
    | DeleteUserFunction(TLID.t)
    | DeleteUserFunctionForever(TLID.t)
    | DeleteUserType(TLID.t)
    | DeleteUserTypeForever(TLID.t)
    | RestoreToplevel(TLID.t)
    | ReceiveAnalysis(AnalysisTypes.PerformAnalysis.t)
    | ReceiveFetch(APITypes.fetchResult)
    | EnablePanning(bool)
    | DeleteColInDB(TLID.t, ID.t)
    | CreateDBTable
    | ClipboardCopyEvent(Types.clipboardEvent)
    | ClipboardCutEvent(Types.clipboardEvent)
    | ClipboardPasteEvent(Types.clipboardEvent)
    | ClipboardCopyLivevalue(string, VPos.t)
    | EventDecoderError(string, string, string)
    | CanvasPanAnimationEnd
    | GoTo(Page.t)
    | SetHoveringReferences(TLID.t, list<ID.t>)
    | @printer(opaque("TriggerSendPresenceCallback"))
    TriggerSendPresenceCallback(Tea.Result.t<unit, Types.httpError>)
    | TakeOffErrorRail(TLID.t, ID.t)
    | SetHandlerExeIdle(TLID.t)
    | CopyCurl(TLID.t, VPos.t)
    | TLMenuMsg(TLID.t, Menu.msg)
    | ResetToast
    | GoToArchitecturalView
    | HideTopbar
    | DismissErrorBar
    | PauseWorker(string)
    | RunWorker(string)
    | @printer(opaque("UpdateWorkerScheduleCallback"))
    UpdateWorkerScheduleCallback(
        Tea.Result.t<Tc.Map.String.t<AnalysisTypes.WorkerState.t>, Types.httpError>,
      )
    | NewTabFromTLMenu(string, TLID.t)
    | FnParamMsg(FunctionParams.msg)
    | ToolTipMsg(Tooltip.msg)
    | UpdateHeapio(Types.heapioTrack)
    | SettingsMsg(SettingsState.msg)
    | SecretMsg(SecretTypes.msg)
}

module Modification = {
  /* tlidSelectTarget represents a target inside a TLID for use
     by the `Select` modification.

     In Fluid, we should probably use STCaret in all cases -- knowing the id of an ast
     node (via STID) is insufficient to know where to place the caret within that node.
     In non-fluid, the concept of a caret doesn't really exist; we select nodes at any
     nesting level as a whole, so STID is sufficient.

     If we want to select a toplevel as a whole but don't have a specific id *in mind,
     we use STTopLevelRoot. There's a few places where we do this as a fallback when we
     expected to find an id but couldn't (they used to use Some(id) with an implicit
     fallback to None). */
  @ppx.deriving(show({with_path: false}))
  type rec tlidSelectTarget =
    | STCaret(FluidCursorTypes.CaretTarget.t)
    | STID(ID.t)
    | STTopLevelRoot

  @ppx.deriving(show({with_path: false}))
  type rec t<'model> =
    | @ocaml.doc(" ReplaceAllModificationsWithThisOne is a migration path away from modifications. It
        * takes in a model and directly returns a (model * msg Cmd.t) just like
        * The update function.
        *
        * This modification should be used in all new code.
        *
        * The intent is to completely replace all existing modifications with
        * this one, then remove the modification type entirely, directly
        * returning the (model * Cmd.t) from the update function ")
    @printer(opaque("ReplaceAllModificationsWithThisOne"))
    ReplaceAllModificationsWithThisOne('model => ('model, Tea.Cmd.t<Msg.t<'model, t<'model>>>))

    // API Calls
    | AddOps((list<PT.Op.t>, Focus.t))
    | HandleAPIError(APIError.t)
    | GetUnlockedDBsAPICall
    | Get404sAPICall
    | GetWorkerStatsAPICall(TLID.t)
    | ExecutingFunctionAPICall(TLID.t, ID.t, string)
    | TriggerHandlerAPICall(TLID.t)
    | UpdateDBStatsAPICall(TLID.t)
    | DeleteToplevelForeverAPICall(TLID.t)
    // End API Calls
    | Select(TLID.t, tlidSelectTarget)
    | SetHover(TLID.t, Types.idOrTraceID)
    | ClearHover(TLID.t, Types.idOrTraceID)
    | Deselect
    | RemoveToplevel(Types.toplevel)
    | SetToplevels(list<PT.Handler.t>, list<PT.DB.t>, bool)
    | UpdateToplevels(list<PT.Handler.t>, list<PT.DB.t>, bool)
    | SetDeletedToplevels(list<PT.Handler.t>, list<PT.DB.t>)
    | UpdateDeletedToplevels(list<PT.Handler.t>, list<PT.DB.t>)
    | UpdateAnalysis(AnalysisTypes.PerformAnalysis.Envelope.t)
    | SetUserFunctions(list<PT.UserFunction.t>, list<PT.UserFunction.t>, bool)
    | SetUnlockedDBs(AnalysisTypes.unlockedDBs)
    | AppendUnlockedDBs(AnalysisTypes.unlockedDBs)
    | Append404s(list<AnalysisTypes.FourOhFour.t>)
    | Delete404(AnalysisTypes.FourOhFour.t) // Save the entire 404 in case we need to roll back
    | Enter(TLID.t, ID.t) // Enter a blankOr
    | EnterWithOffset(TLID.t, ID.t, int) // Entering a blankOr with a desired caret offset
    | OpenOmnibox(option<Pos.t>) // Open the omnibox
    | UpdateWorkerSchedules(Tc.Map.String.t<AnalysisTypes.WorkerState.t>)
    | NoChange
    | @printer(opaque("MakeCmd")) MakeCmd(Tea.Cmd.t<Msg.t<'model, t<'model>>>)
    | AutocompleteMod(AutoComplete.mod)
    | Many(list<t<'model>>)
    | PanCanvas({viewportStart: VPos.t, viewportCurr: VPos.t, prevCursorState: CursorState.t})
    | DragTL(TLID.t, VPos.t, CursorState.hasMoved, CursorState.t)
    | TriggerIntegrationTest(string)
    | EndIntegrationTest
    | SetPage(Page.t)
    | SetTLTraceID(TLID.t, TraceID.t)
    | ExecutingFunctionBegan(TLID.t, ID.t)
    | ExecutingFunctionComplete(list<(TLID.t, ID.t)>)
    | MoveCanvasTo(Pos.t, CanvasProps.isTransitionAnimated)
    | UpdateTraces(AnalysisTypes.Traces.t)
    | OverrideTraces(AnalysisTypes.Traces.t)
    | UpdateTraceFunctionResult(TLID.t, TraceID.t, ID.t, string, string, int, RuntimeTypes.Dval.t)
    | AppendStaticDeploy(list<StaticAssets.Deploy.t>)
    // designed for one-off small changes
    | Apply(
        /* It can be tempting to call a function which returns
         * modifications. However, this can have a bug - the model
         * used to create those modifications can be wrong (if the
         * model was changed by previous modifications). Apply can
         * be used to call the functions and apply the modifications,
         * so that the latest model is use. */
        'model => t<'model>,
      )
    | SetTypes(list<PT.UserType.t>, list<PT.UserType.t>, bool)
    | SetPermission(option<AccountTypes.Permission.t>)
    | CenterCanvasOn(TLID.t)
    | InitIntrospect(list<Types.toplevel>)
    | RefreshUsages(list<TLID.t>)
    | FluidCommandsShow(TLID.t, ID.t)
    | FluidCommandsClose
    /* We need to track clicks so that we don't mess with the caret while a
     * click is happening. */
    | FluidStartClick
    | FluidEndClick
    | UpdateAvatarList(list<Avatar.t>)
    | ExpireAvatars
    | SetClipboardContents(Types.clipboardContents, Types.clipboardEvent)
    | UpdateASTCache(TLID.t, string)
    | InitASTCache(list<PT.Handler.t>, list<PT.UserFunction.t>)
    | FluidSetState(FluidTypes.State.t<'model, t<'model>>)
    | TLMenuUpdate(TLID.t, Menu.msg)
}

module Model = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    error: Error.t,
    lastMsg: Msg.t<t, Modification.t<t>>,
    tests: list<Types.variantTest>,
    functions: Types.functionsType,
    complete: AutoComplete.t,
    cursorState: CursorState.t,
    currentPage: Page.t,
    hovering: list<(TLID.t, Types.idOrTraceID)>,
    handlers: TLID.Dict.t<PT.Handler.t>,
    deletedHandlers: TLID.Dict.t<PT.Handler.t>,
    dbs: TLID.Dict.t<PT.DB.t>,
    deletedDBs: TLID.Dict.t<PT.DB.t>,
    userFunctions: TLID.Dict.t<PT.UserFunction.t>,
    deletedUserFunctions: TLID.Dict.t<PT.UserFunction.t>,
    userTypes: TLID.Dict.t<PT.UserType.t>,
    deleteduserTypes: TLID.Dict.t<PT.UserType.t>,
    traces: AnalysisTypes.Traces.t,
    analyses: AnalysisTypes.analyses,
    f404s: list<AnalysisTypes.FourOhFour.t>,
    unlockedDBs: AnalysisTypes.unlockedDBs,
    // State of individual integration tests
    integrationTestState: IntegrationTests.t<t>,
    visibility: PageVisibility.t,
    syncState: SyncState.t,
    executingFunctions: list<(TLID.t, ID.t)>,
    tlTraceIDs: Types.tlTraceIDs, // This is TLID id to traceID map
    canvasProps: CanvasProps.t,
    canvasName: string,
    userContentHost: string,
    origin: string,
    environment: string,
    csrfToken: string,
    usedDBs: Tc.Map.String.t<int>,
    usedFns: Tc.Map.String.t<int>,
    usedTipes: Tc.Map.String.t<int>,
    handlerProps: TLID.Dict.t<HandlerProperty.t>,
    staticDeploys: list<StaticAssets.Deploy.t>,
    /* tlRefersTo : to answer the question "what TLs does this TL refer to". eg
     * if myFunc was called in Repl2 at id, then the dict would be:
     *
     *   { repl2.tlid { (myFunc.tlid, id) } }
     *
     * which you can read as "repl2 refersTo myfunc". So a TLID.t points to the TLs
     * it uses. */
    tlRefersTo: TLID.Dict.t<list<(TLID.t, ID.t)>>,
    /* tlUsedIn: to answer the question "what TLs is this TL's name used in".  eg
     * if myFunc was called in Repl2, the dict would
     *
     *   { myfunc.tlid: { repl2.tlid }}
     *
     * which you can read as "myfunc is used in repl2". */
    tlUsedIn: TLID.Dict.t<TLID.Set.t>,
    fluidState: FluidTypes.State.t<t, Modification.t<t>>,
    dbStats: AnalysisTypes.dbStatsStore,
    workerStats: TLID.Dict.t<AnalysisTypes.WorkerStats.t>,
    avatarsList: list<Avatar.t>,
    browserId: string,
    sidebarState: Sidebar.State.t,
    isAdmin: bool,
    buildHash: string,
    lastReload: option<@opaque Js.Date.t>,
    opCtrs: Tc.Map.String.t<int>,
    clientOpCtrId: string,
    permission: option<AccountTypes.Permission.t>,
    showTopbar: bool,
    toast: Toast.t,
    username: string,
    account: AccountTypes.Account.t,
    workerSchedules: Tc.Map.String.t<AnalysisTypes.WorkerState.t>,
    searchCache: TLID.Dict.t<string>,
    editorSettings: EditorSettings.t,
    teaDebuggerEnabled: bool,
    unsupportedBrowser: bool,
    tlMenus: TLID.Dict.t<Menu.t>,
    firstVisitToDark: bool,
    // indicates if it is the users first time visiting any dark canvas
    tooltipState: Tooltip.t,
    currentUserFn: FunctionParams.t,
    settingsView: SettingsState.t,
    firstVisitToThisCanvas: bool,
    // indicates if it is the users first time this canvas
    secrets: list<SecretTypes.t>,
    insertSecretModal: SecretTypes.insertModal,
  }
  let default: t = {
    error: Error.default,
    functions: Defaults.defaultFunctionsType,
    lastMsg: Msg.IgnoreMsg("default"),
    opCtrs: Tc.Map.String.empty,
    clientOpCtrId: "",
    complete: AutoComplete.default,
    currentPage: Architecture,
    hovering: list{},
    tests: list{},
    handlers: TLID.Dict.empty,
    deletedHandlers: TLID.Dict.empty,
    dbs: TLID.Dict.empty,
    deletedDBs: TLID.Dict.empty,
    userFunctions: TLID.Dict.empty,
    deletedUserFunctions: TLID.Dict.empty,
    userTypes: TLID.Dict.empty,
    deleteduserTypes: TLID.Dict.empty,
    analyses: Tc.Map.String.empty,
    traces: TLID.Dict.empty,
    f404s: list{},
    unlockedDBs: TLID.Set.empty,
    integrationTestState: NoIntegrationTest,
    visibility: PageVisibility.Visible, // partially saved in editor
    syncState: Tc.Set.String.empty,
    cursorState: Deselected,
    executingFunctions: list{},
    tlTraceIDs: TLID.Dict.empty,
    canvasProps: CanvasProps.default,
    canvasName: "builtwithdark",
    userContentHost: "builtwithdark.com",
    origin: "",
    environment: "none",
    csrfToken: Defaults.unsetCSRF,
    usedDBs: Tc.Map.String.empty,
    usedFns: Tc.Map.String.empty,
    usedTipes: Tc.Map.String.empty,
    handlerProps: TLID.Dict.empty,
    staticDeploys: list{},
    tlRefersTo: TLID.Dict.empty,
    tlUsedIn: TLID.Dict.empty,
    fluidState: FluidTypes.State.default,
    dbStats: Tc.Map.String.empty,
    workerStats: TLID.Dict.empty,
    avatarsList: list{},
    browserId: "",
    sidebarState: Sidebar.State.default,
    isAdmin: false,
    buildHash: "",
    username: "defaultUsername",
    lastReload: None,
    permission: None,
    showTopbar: true,
    toast: Toast.default,
    account: AccountTypes.Account.default,
    workerSchedules: Tc.Map.String.empty,
    searchCache: TLID.Dict.empty,
    editorSettings: EditorSettings.default,
    teaDebuggerEnabled: false,
    unsupportedBrowser: false,
    tlMenus: TLID.Dict.empty,
    firstVisitToDark: true,
    tooltipState: Tooltip.default,
    currentUserFn: FunctionParams.default,
    firstVisitToThisCanvas: true,
    secrets: list{},
    settingsView: SettingsState.default,
    insertSecretModal: SecretTypes.defaultInsertModal,
  }
}

// Some concrete definitions to make things easier for other files
@ppx.deriving(show({with_path: false}))
type rec msg = Msg.t<model, modification>
and modification = Modification.t<model>
and model = Model.t
and fluidState = FluidTypes.State.t<model, modification>
and fluidCmd = FluidTypes.Command.t<model, modification>
and fluidMsg = FluidTypes.Msg.t<model, modification>
and fluidCmdState = FluidTypes.Command.state<model, modification>

type cmd = Tea.Cmd.t<msg>
