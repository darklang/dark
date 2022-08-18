open Prelude

module Attrs = Tea.Html.Attributes

module TL = Toplevel
module TD = TLID.Dict
module E = FluidExpression
module ASTInfo = FluidTokenizer.ASTInfo
module Msg = AppTypes.Msg
type msg = AppTypes.msg

type viewProps = {
  tl: toplevel,
  functions: Functions.t,
  astInfo: ASTInfo.t,
  cursorState: AppTypes.CursorState.t,
  tlid: TLID.t,
  isAdmin: bool,
  hovering: option<(TLID.t, idOrTraceID)>,
  ac: AppTypes.AutoComplete.t,
  showEntry: bool,
  showLivevalue: bool,
  dbLocked: bool,
  analysisStore: AnalysisTypes.analysisStore /* for current selected trace */,
  traces: list<AnalysisTypes.Trace.t>,
  dbStats: AnalysisTypes.dbStatsStore,
  executingFunctions: list<id>,
  tlTraceIDs: tlTraceIDs,
  testVariants: list<variantTest>,
  handlerProp: option<AppTypes.HandlerProperty.t>,
  canvasName: string,
  userContentHost: string,
  refersToRefs: list<(toplevel, list<id>)>,
  usedInRefs: list<toplevel>,
  hoveringRefs: list<id>,
  fluidState: AppTypes.fluidState,
  avatarsList: list<AppTypes.Avatar.t>,
  permission: option<AccountTypes.Permission.t>,
  workerStats: option<AnalysisTypes.WorkerStats.t>,
  menuState: AppTypes.Menu.t,
  isExecuting: bool,
  fnProps: AppTypes.FunctionParams.t,
  showHandlerASTs: bool,
  secretValues: list<string>,
}

// -----------------------------
// Events
// -----------------------------
type domEvent = Vdom.property<msg>

type domEventList = list<domEvent>

let createVS = (m: AppTypes.model, tl: toplevel): viewProps => {
  let tlid = TL.id(tl)
  let hp = switch tl {
  | TLHandler(_) => Map.get(~key=tlid, m.handlerProps)
  | _ => None
  }

  let traceID = Analysis.getSelectedTraceID(m, tlid)
  let ast = TL.getAST(tl) |> Option.unwrap(~default=FluidAST.ofExpr(E.newB()))

  let analysisStore =
    Option.map(traceID, ~f=Analysis.getStoredAnalysis(m)) |> Option.unwrap(
      ~default=LoadableNotInitialized,
    )

  let props = FluidUtil.propsFromModel(m)
  let astInfo = ASTInfo.make(props, ast, m.fluidState)
  {
    tl: tl,
    astInfo: astInfo,
    tlid: tlid,
    cursorState: CursorState.unwrap(m.cursorState),
    hovering: m.hovering
    |> List.filter(~f=((thisTLID, _)) => thisTLID == tlid)
    |> List.head
    |> Option.andThen(~f=((_, i) as res) =>
      switch CursorState.idOf(m.cursorState) {
      | Some(cur) =>
        if AnID(cur) == i {
          None
        } else {
          Some(res)
        }
      | _ => Some(res)
      }
    ),
    ac: m.complete,
    showEntry: true,
    showLivevalue: false,
    isAdmin: m.isAdmin,
    dbLocked: DB.isLocked(m, tlid),
    functions: m.functions,
    analysisStore: analysisStore,
    traces: Analysis.getTraces(m, tlid),
    dbStats: m.dbStats,
    executingFunctions: List.filter(
      ~f=((tlid_, _)) => tlid_ == tlid,
      m.executingFunctions,
    ) |> List.map(~f=((_, id)) => id),
    tlTraceIDs: m.tlTraceIDs,
    testVariants: m.tests,
    handlerProp: hp,
    canvasName: m.canvasName,
    userContentHost: m.userContentHost,
    refersToRefs: if CursorState.tlidOf(m.cursorState) == Some(tlid) {
      Introspect.allRefersTo(tlid, m)
    } else {
      list{}
    },
    usedInRefs: if CursorState.tlidOf(m.cursorState) == Some(tlid) {
      Introspect.allUsedIn(tlid, m)
    } else {
      list{}
    },
    hoveringRefs: Map.get(~key=tlid, m.handlerProps)
    |> Option.map(~f=(hp: AppTypes.HandlerProperty.t) => hp.hoveringReferences)
    |> Option.unwrap(~default=list{}),
    fluidState: m.fluidState,
    avatarsList: switch m.currentPage {
    | FocusedHandler(tlid_, _, _)
    | FocusedType(tlid_)
    | FocusedFn(tlid_, _)
    | FocusedDB(tlid_, _) if tlid_ == tlid =>
      m.avatarsList
    | _ => list{}
    },
    permission: m.permission,
    workerStats: // Right now we patch because worker execution link depends on name instead of TLID. When we fix our worker association to depend on TLID instead of name, then we will get rid of this patchy hack.

    {
      let count = Map.get(~key=tlid, m.workerStats)
      let asWorkerSchedule = Handlers.getWorkerSchedule(m)
      let schedule = tl |> TL.asHandler |> Option.andThen(~f=asWorkerSchedule)

      switch (count, schedule) {
      | (None, None) => None
      | (Some(c), None) => Some(c)
      | (None, Some(_)) => Some({...AnalysisTypes.WorkerStats.default, schedule: schedule})
      | (Some(c), Some(_)) => Some({...c, schedule: schedule})
      }
    },
    menuState: Map.get(~key=tlid, m.tlMenus) |> Option.unwrap(~default=AppTypes.Menu.default),
    isExecuting: // Converge can execute for functions & handlers
    switch tl {
    | TLFunc(_) => List.any(~f=((fTLID, _)) => fTLID == tlid, m.executingFunctions)
    | TLHandler(_) =>
      // Doing explicit match here just to be safe, even though we can probably assume you can't have handlerProp without it being a handler from code above.
      switch hp {
      | Some(p) => p.execution == Executing
      | _ => false
      }
    | TLPmFunc(_) | TLDB(_) | TLTipe(_) => false
    },
    fnProps: m.currentUserFn,
    showHandlerASTs: m.editorSettings.showHandlerASTs,
    secretValues: m.secrets |> List.map(~f=SecretTypes.getSecretValue),
  }
}

let fontAwesome = (name: string): Html.html<msg> =>
  Html.i(list{Attrs.class'("fa fa-" ++ name)}, list{})

let darkIcon = (name: string): Html.html<msg> =>
  Html.i(list{Attrs.class'("di di-" ++ name)}, list{})

let decodeTransEvent = (fn: string => 'a, j): 'a => {
  open Json.Decode
  fn(field("propertyName", string, j))
}

let decodeAnimEvent = (fn: string => 'a, j): 'a => {
  open Json.Decode
  fn(field("animationName", string, j))
}

// Generic event, the the listener handle and do what it wants with the event object
let onEvent = (
  ~event: string,
  ~key: string,
  ~preventDefault=true,
  listener: Web.Node.event => msg,
): Vdom.property<msg> =>
  Html.Events.onCB(event, key, evt => {
    if preventDefault {
      evt["preventDefault"]()
    }
    Some(listener(evt))
  })

let eventBoth = (
  ~key: string,
  event: string,
  constructor: AppTypes.MouseEvent.t => msg,
): Vdom.property<msg> =>
  Tea.Html.onWithOptions(
    ~key,
    event,
    {stopPropagation: false, preventDefault: false},
    Decoders.wrapDecoder(Json.Decode.map(constructor, AppTypes.MouseEvent.decode)),
  )

let eventPreventDefault = (
  ~key: string,
  event: string,
  constructor: AppTypes.MouseEvent.t => msg,
): Vdom.property<msg> =>
  Tea.Html.onWithOptions(
    ~key,
    event,
    {stopPropagation: false, preventDefault: true},
    Decoders.wrapDecoder(Json.Decode.map(constructor, AppTypes.MouseEvent.decode)),
  )

let eventNeither = (
  ~key: string,
  event: string,
  constructor: AppTypes.MouseEvent.t => msg,
): Vdom.property<msg> =>
  Tea.Html.onWithOptions(
    ~key,
    event,
    {stopPropagation: true, preventDefault: true},
    Decoders.wrapDecoder(Json.Decode.map(constructor, AppTypes.MouseEvent.decode)),
  )

let scrollEventNeither = (
  ~key: string,
  event: string,
  constructor: AppTypes.ScrollEvent.t => msg,
): Vdom.property<msg> =>
  Tea.Html.onWithOptions(
    ~key,
    event,
    {stopPropagation: true, preventDefault: true},
    Decoders.wrapDecoder(Json.Decode.map(constructor, AppTypes.ScrollEvent.decode)),
  )

let eventNoPropagation = (
  ~key: string,
  event: string,
  constructor: AppTypes.MouseEvent.t => msg,
): Vdom.property<msg> =>
  Tea.Html.onWithOptions(
    ~key,
    event,
    {stopPropagation: true, preventDefault: false},
    Decoders.wrapDecoder(Json.Decode.map(constructor, AppTypes.MouseEvent.decode)),
  )

let onTransitionEnd = (~key: string, ~listener: string => msg): Vdom.property<msg> =>
  Tea.Html.onWithOptions(
    ~key,
    "transitionend",
    {stopPropagation: false, preventDefault: true},
    Decoders.wrapDecoder(decodeTransEvent(listener)),
  )

let onAnimationEnd = (~key: string, ~listener: string => msg): Vdom.property<msg> =>
  Tea.Html.onWithOptions(
    ~key,
    "animationend",
    {stopPropagation: false, preventDefault: true},
    Decoders.wrapDecoder(decodeAnimEvent(listener)),
  )

let nothingMouseEvent = (name: string): Vdom.property<msg> =>
  eventNoPropagation(~key="", name, _ =>
    // For fluid, we need to know about most (all?) mouseups
    if name == "mouseup" {
      IgnoreMouseUp
    } else {
      Msg.IgnoreMsg(name)
    }
  )

let placeHtml = (pos: Pos.t, classes: list<'a>, html: list<Html.html<msg>>): Html.html<msg> => {
  let styles = Html.Attributes.styles(list{
    ("left", string_of_int(pos.x) ++ "px"),
    ("top", string_of_int(pos.y) ++ "px"),
  })

  Html.div(list{Attrs.classList(list{("node", true), ...classes}), styles}, html)
}

let inCh = (w: int): string => w |> string_of_int |> (s => s ++ "ch")

let widthInCh = (w: int): Vdom.property<msg> => w |> inCh |> Html.style("width")

let createHandlerProp = (hs: list<PT.Handler.t>): TD.t<AppTypes.HandlerProperty.t> =>
  hs |> List.map(~f=(h: PT.Handler.t) => (h.tlid, AppTypes.HandlerProperty.default)) |> TD.fromList

let isHoverOverTL = (vp: viewProps): bool =>
  switch vp.hovering {
  | Some(tlid, _id) if tlid == TL.id(vp.tl) => true
  | _ => false
  }

let intAsUnit = (i: int, u: string): string => string_of_int(i) ++ u

let fnForToken = (functions: Functions.t, token): option<function_> =>
  switch token {
  | FluidTypes.Token.TBinOp(_, fnName, _)
  | TFnVersion(_, _, _, fnName)
  | TFnName(_, _, _, fnName, _) =>
    Functions.findByStr(fnName, functions)
  | _ => None
  }
