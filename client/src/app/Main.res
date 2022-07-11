open Prelude

// Tea
module Cmd = Tea.Cmd
module Http = Tea.Http

// Dark
module AC = Autocomplete
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Key = Keyboard
module Regex = Util.Regex
module TD = TLIDDict

let incOpCtr = (m: model): model => {
  ...m,
  opCtrs: Map.update(m.opCtrs, ~key=m.clientOpCtrId, ~f=x =>
    switch x {
    | Some(v) => Some(v + 1)
    | None => Some(1)
    }
  ),
}

let opCtr = (m: model): int =>
  switch Map.get(~key=m.clientOpCtrId, m.opCtrs) {
  | Some(ctr) => ctr
  | None => 0
  }

let expireAvatars = (avatars: list<Types.avatar>): list<Types.avatar> => {
  let fiveMinsAgo: float = Js.Date.now() -. 5.0 *. 60.0 *. 1000.0
  List.filter(~f=av => av.serverTime |> Js.Date.valueOf > fiveMinsAgo, avatars)
}

let createBrowserId: string = BsUuid.Uuid.V4.create() |> BsUuid.Uuid.V4.toString

let createClientOpCtrId: string = BsUuid.Uuid.V4.create() |> BsUuid.Uuid.V4.toString

let manageBrowserId = (): string =>
  // Setting the browser id in session storage so it is stored per tab
  switch Dom.Storage.getItem("browserId", Dom.Storage.sessionStorage) {
  | Some(browserId) => browserId
  | None =>
    let newBrowserId = createBrowserId
    Dom.Storage.setItem("browserId", newBrowserId, Dom.Storage.sessionStorage)
    newBrowserId
  }

let init = (encodedParamString: string, location: Web.Location.location) => {
  let {
    canvasName,
    complete,
    userContentHost,
    environment,
    csrfToken,
    isAdmin,
    buildHash,
    username,
  }: InitialParameters.t = InitialParameters.fromString(encodedParamString)

  let variants = VariantTesting.enabledVariantTests(isAdmin)
  let m = SavedSettings.load(canvasName) |> SavedSettings.toModel
  let m = SavedUserSettings.load(username) |> SavedUserSettings.toModel(m)
  let userTutorial = if m.firstVisitToDark && m.tooltipState.userTutorial.step == None {
    UserTutorial.defaultTutorial
  } else {
    m.tooltipState.userTutorial
  }

  let page =
    Url.parseLocation(location) |> Option.unwrap(~default=Defaults.defaultModel.currentPage)

  // these saved values may not be valid yet
  let savedCursorState = m.cursorState
  let m = {
    ...m,
    cursorState: Deselected,
    // deselect for now as the selected blank isn't available yet
    currentPage: Architecture,
    functions: Functions.empty |> Functions.setBuiltins(
      complete,
      {usedFns: m.usedFns, userFunctions: m.userFunctions},
    ),
    complete: AC.init(m),
    tests: variants,
    handlers: TLIDDict.empty,
    dbs: TLIDDict.empty,
    canvasName: canvasName,
    userContentHost: userContentHost,
    origin: location.origin,
    environment: environment,
    csrfToken: csrfToken,
    browserId: manageBrowserId(),
    clientOpCtrId: createClientOpCtrId,
    isAdmin: isAdmin,
    buildHash: buildHash,
    username: username,
    teaDebuggerEnabled: Url.isDebugging(),
    unsupportedBrowser: Entry.unsupportedBrowser(),
    fluidState: Fluid.initAC(m.fluidState),
    tooltipState: {...m.tooltipState, userTutorial: userTutorial},
  }

  let timeStamp = Js.Date.now() /. 1000.0
  let avMessage: avatarModelMessage = {
    canvasName: m.canvasName,
    browserId: m.browserId,
    tlid: None,
    timestamp: timeStamp,
  }

  if Url.isIntegrationTest() {
    (m, Cmd.batch(list{API.integration(m, m.canvasName), API.loadPackages(m)}))
  } else {
    (
      m,
      Cmd.batch(list{
        API.loadPackages(m),
        API.initialLoad(m, FocusPageAndCursor(page, savedCursorState)),
        API.sendPresence(m, avMessage),
      }),
    )
  }
}

let processFocus = (m: model, focus: focus): modification =>
  switch focus {
  | FocusNext(tlid, pred) =>
    switch TL.get(m, tlid) {
    | None => NoChange
    | Some(tl) =>
      let next =
        pred |> Option.map(~f=TL.getNextBlank(tl)) |> Option.unwrap(~default=TL.firstBlank(tl))

      switch next {
      | Some(id) => Enter(tlid, id)
      | None =>
        switch pred {
        | Some(id) => Select(tlid, STID(id))
        | None => Select(tlid, STTopLevelRoot)
        }
      }
    }
  | FocusExact(tlid, id) =>
    switch TL.getPD(m, tlid, id) {
    | Some(pd) =>
      if P.isBlank(pd) || P.toContent(pd) == "" {
        Enter(tlid, id)
      } else {
        Select(tlid, STID(id))
      }
    | _ => NoChange
    }
  | FocusSame =>
    switch CursorState.unwrap(m.cursorState) {
    | Selecting(tlid, mId) =>
      switch (TL.get(m, tlid), mId) {
      | (Some(tl), Some(id)) =>
        if TL.isValidBlankOrID(tl, id) {
          NoChange
        } else {
          Select(tlid, STTopLevelRoot)
        }
      | (Some(_), None) => Select(tlid, STTopLevelRoot)
      | _ => Deselect
      }
    | Entering(tlid, id) =>
      switch TL.get(m, tlid) {
      | Some(tl) =>
        if TL.isValidBlankOrID(tl, id) {
          NoChange
        } else {
          Select(tlid, STTopLevelRoot)
        }
      | _ => Deselect
      }
    | _ => NoChange
    }
  | FocusPageAndCursor(page, cs) =>
    let useCS = Page.tlidOf(page) == CursorState.tlidOf(cs)
    let (tlid, mID) = // If they don't match, the URL wins
    if useCS {
      (CursorState.tlidOf(cs), CursorState.idOf(cs))
    } else {
      (Page.tlidOf(page), None)
    }

    let mTl = Option.andThen(tlid, ~f=TL.get(m))
    let pd = Option.map2(mTl, mID, ~f=(tl, id) => TL.find(tl, id))
    switch (mTl, pd) {
    | (Some(tl), Some(Some(pd))) if TL.isValidBlankOrID(tl, P.toID(pd)) =>
      let query = AutocompleteMod(ACSetQuery(P.toContent(pd)))
      Many(list{
        SetPage(page),
        ReplaceAllModificationsWithThisOne(CursorState.setCursorState(cs)),
        query,
      })
    | (Some(_), Some(None)) | (Some(_), None) =>
      Many(list{
        SetPage(page),
        ReplaceAllModificationsWithThisOne(CursorState.setCursorState(cs)),
        AutocompleteMod(ACSetQuery("")),
      })
    | (_, _) =>
      switch page {
      | SettingsModal(tab) => Many(SettingsView.getModifications(m, OpenSettingsView(tab)))
      | _ => NoChange
      }
    }
  | FocusNothing => Deselect
  // used instead of focussame when we've already done the focus
  | FocusNoChange => NoChange
  }

let processAutocompleteMods = (m: model, mods: list<autocompleteMod>): (model, Cmd.t<msg>) => {
  if m.integrationTestState != NoIntegrationTest {
    Debug.loG("autocompletemod update", show_list(~f=show_autocompleteMod, mods))
  }
  let complete = List.fold(
    ~f=(complete_, mod_) => AC.update(m, mod_, complete_),
    ~initial=m.complete,
    mods,
  )

  let focus = switch CursorState.unwrap(m.cursorState) {
  | Entering(_) => AC.focusItem(complete.index)
  | _ => Cmd.none
  }

  if m.integrationTestState != NoIntegrationTest {
    let val_ = AC.getValue(complete)
    Debug.loG(
      "autocompletemod result: ",
      string_of_int(complete.index) ++ (" => '" ++ (val_ ++ "'")),
    )
  }
  ({...m, complete: complete}, focus)
}

let applyOpsToClient = (updateCurrent, p: addOpAPIParams, r: addOpAPIResult): list<
  Types.modification,
> => list{
  UpdateToplevels(r.handlers, r.dbs, updateCurrent),
  UpdateDeletedToplevels(r.deletedHandlers, r.deletedDBs),
  SetUserFunctions(r.userFunctions, r.deletedUserFunctions, updateCurrent),
  SetTypes(r.userTipes, r.deletedUserTipes, updateCurrent),
  RefreshUsages(Introspect.tlidsToUpdateUsage(p.ops)),
}

let isACOpened = (m: model): bool =>
  FluidAutocomplete.isOpened(m.fluidState.ac) ||
  (FluidCommands.isOpened(m.fluidState.cp) ||
  AC.isOpened(m.complete))

let rec updateMod = (mod_: modification, (m, cmd): (model, Cmd.t<msg>)): (model, Cmd.t<msg>) => {
  if m.integrationTestState != NoIntegrationTest {
    Debug.loG("mod update", show_modification(mod_))
  }
  let (newm, newcmd) = {
    let bringBackCurrentTL = (oldM: model, newM: model): model =>
      /* used with updateCurrent - if updateCurrent is false, we want to restore
       * the current TL so we don't lose local changes made since the API call */
      switch CursorState.tlidOf(oldM.cursorState) {
      | Some(tlid) =>
        let tl = TL.get(oldM, tlid)
        switch tl {
        | Some(TLDB(db)) => DB.upsert(newM, db)
        | Some(TLHandler(h)) => Handlers.upsert(newM, h)
        | Some(TLFunc(func)) => UserFunctions.upsert(newM, func)
        | Some(TLPmFunc(_)) | Some(TLTipe(_)) | None => newM
        }
      | None => newM
      }

    let handleAPI = (params, focus) => {
      /* immediately update the model based on SetHandler and focus, if
       possible */
      let m = m |> incOpCtr
      let hasNonHandlers = List.any(~f=c =>
        switch c {
        | SetHandler(_, _, _) => false
        | SetFunction(_) => false
        | SetType(_) => false
        | _ => true
        }
      , params.ops)

      if hasNonHandlers {
        (m, API.addOp(m, focus, params))
      } else {
        let localM = List.fold(~f=(m, call) =>
          switch call {
          | SetHandler(_tlid, _pos, h) => Handlers.upsert(m, h)
          | SetFunction(f) => UserFunctions.upsert(m, f)
          | SetType(t) => UserTypes.upsert(m, t)
          | _ => m
          }
        , ~initial=m, params.ops)

        let (withFocus, wfCmd) = updateMod(
          Many(list{AutocompleteMod(ACReset), processFocus(localM, focus)}),
          (localM, Cmd.none),
        )

        (withFocus, Cmd.batch(list{wfCmd, API.addOp(withFocus, FocusNoChange, params)}))
      }
    }

    switch mod_ {
    | ReplaceAllModificationsWithThisOne(f) => f(m)
    | HandleAPIError(apiError) =>
      let now = Js.Date.now() |> Js.Date.fromFloat
      let shouldReload = {
        let buildHashMismatch =
          APIError.serverVersionOf(apiError)
          |> Option.map(~f=hash => hash != m.buildHash)
          |> Option.unwrap(~default=false)

        let reloadAllowed = switch m.lastReload {
        | Some(time) =>
          // if 60 seconds have elapsed
          Js.Date.getTime(time) +. 60000.0 > Js.Date.getTime(now)
        | None => true
        }

        // Reload if it's an auth failure or the frontend is out of date
        APIError.isBadAuth(apiError) || (buildHashMismatch && reloadAllowed)
      }

      let ignore = {
        // Ignore when using Ngrok
        let usingNgrok = VariantTesting.variantIsActive(m, NgrokVariant)
        /* This message is deep in the server code and hard to pull
         * out, so just ignore for now */
        Js.log("Already at latest redo - ignoring server error")
        let redoError = String.includes(
          APIError.msg(apiError),
          ~substring="(client): Already at latest redo",
        )

        redoError || usingNgrok
      }

      let cmd = if shouldReload {
        let m = {...m, lastReload: Some(now)}
        /* Previously, this was two calls to Tea_task.nativeBinding. But
         * only the first got called, unclear why. */
        Cmd.call(_ => {
          SavedSettings.save(m)
          SavedUserSettings.save(m)
          Native.Location.reload(true)
        })
      } else if !ignore && APIError.shouldRollbar(apiError) {
        Cmd.call(_ => Rollbar.sendAPIError(m, apiError))
      } else {
        Cmd.none
      }

      let newM = {
        let error = if APIError.shouldDisplayToUser(apiError) && !ignore {
          Error.set(APIError.msg(apiError), m.error)
        } else {
          m.error
        }

        let lastReload = if shouldReload {
          Some(now)
        } else {
          m.lastReload
        }
        {...m, error: error, lastReload: lastReload}
      }

      (newM, cmd)
    | AddOps(ops, focus) => handleAPI(API.opsParams(ops, (m |> opCtr) + 1, m.clientOpCtrId), focus)
    | GetUnlockedDBsAPICall => Sync.attempt(~key="unlocked", m, API.getUnlockedDBs(m))
    | Get404sAPICall => (m, API.get404s(m))
    | UpdateDBStatsAPICall(tlid) => Analysis.updateDBStats(m, tlid)
    | GetWorkerStatsAPICall(tlid) => Analysis.getWorkerStats(m, tlid)
    | DeleteToplevelForeverAPICall(tlid) => (m, API.deleteToplevelForever(m, {dtfTLID: tlid}))
    | NoChange => (m, Cmd.none)
    | TriggerIntegrationTest(name) =>
      let expect = IntegrationTest.trigger(name)
      ({...m, integrationTestState: expect}, Cmd.none)
    | EndIntegrationTest =>
      let expectationFn = switch m.integrationTestState {
      | IntegrationTestExpectation(fn) => fn
      | IntegrationTestFinished(_) =>
        recover(
          "Attempted to end integration test but one ran + was already finished",
          ~debug=m.integrationTestState,
          _ => IntegrationTest.fail("Already finished"),
        )
      | NoIntegrationTest =>
        recover(
          "Attempted to end integration test but none was running",
          ~debug=m.integrationTestState,
          _ => IntegrationTest.fail("Not running"),
        )
      }

      let result = expectationFn(m)
      ({...m, integrationTestState: IntegrationTestFinished(result)}, Cmd.none)
    | MakeCmd(cmd) => (m, cmd)
    | SetPage(page) =>
      let pagePresent = switch Page.tlidOf(page) {
      | None => true
      | Some(tlid) => TL.get(m, tlid) != None
      }

      if pagePresent {
        let avMessage: avatarModelMessage = {
          canvasName: m.canvasName,
          browserId: m.browserId,
          tlid: Page.tlidOf(page),
          timestamp: Js.Date.now() /. 1000.0,
        }

        let (m, afCmd) = Page.updatePossibleTrace(m, page)
        let cmds = Cmd.batch(list{API.sendPresence(m, avMessage), afCmd})

        (Page.setPage(m, m.currentPage, page), cmds)
      } else {
        (Page.setPage(m, m.currentPage, Architecture), Url.updateUrl(Architecture))
      }
    | Select(tlid, p) =>
      let (cursorState: cursorState, maybeNewFluidState: option<fluidState>) = switch p {
      | STTopLevelRoot =>
        switch TL.get(m, tlid) {
        | Some(TLDB(_)) | Some(TLTipe(_)) => (Selecting(tlid, None), None)
        | Some(TLFunc(_)) | Some(TLHandler(_)) => (FluidEntering(tlid), None)
        | Some(TLPmFunc(_)) => (Selecting(tlid, None), None)
        | None => (Deselected, None)
        }
      | STID(id) =>
        switch TL.getPD(m, tlid, id) {
        | Some(_) => (Selecting(tlid, Some(id)), None)
        | None => (FluidEntering(tlid), None)
        }
      | STCaret(caretTarget) =>
        let maybeNewFluidState =
          Fluid.astInfoFromModelAndTLID(m, tlid) |> Option.map(~f=astInfo =>
            astInfo
            |> Fluid.setPosition(Fluid.posFromCaretTarget(caretTarget, astInfo))
            |> (astInfo => astInfo.state)
          )

        (FluidEntering(tlid), maybeNewFluidState)
      }

      let (m, hashcmd) = switch TL.get(m, tlid) {
      | Some(tl) if CursorState.tlidOf(m.cursorState) != Some(tlid) =>
        let page = TL.asPage(tl, false)
        let m = Page.setPage(m, m.currentPage, page)
        (m, Url.updateUrl(page))
      | _ => (m, Cmd.none)
      }

      let m = {
        ...m,
        cursorState: cursorState,
        fluidState: maybeNewFluidState |> Option.unwrap(~default=m.fluidState),
      }

      let (
        m,
        acCmd,
      ) = /* Note that we want to ensure that when we click out of an entry box that the AC is
       * reset, else we can't scroll. */
      switch p {
      | STTopLevelRoot => processAutocompleteMods(m, list{ACReset})
      | STID(_) => processAutocompleteMods(m, list{})
      | STCaret(_) => processAutocompleteMods(m, list{ACReset})
      }

      let (m, afCmd) = Analysis.analyzeFocused(m)
      let timeStamp = Js.Date.now() /. 1000.0
      let avMessage: avatarModelMessage = {
        canvasName: m.canvasName,
        browserId: m.browserId,
        tlid: Some(tlid),
        timestamp: timeStamp,
      }

      let commands = list{hashcmd, acCmd, afCmd, API.sendPresence(m, avMessage)}
      (m, Cmd.batch(commands))
    | Deselect =>
      if m.cursorState != Deselected {
        let m = TLMenu.closeMenu(m)
        let hashcmd = Url.updateUrl(Architecture)
        let m = Page.setPage(m, m.currentPage, Architecture)
        let (m, acCmd) = processAutocompleteMods(m, list{ACReset})
        let m = {...m, cursorState: Deselected}
        let m = {...m, fluidState: Fluid.deselectFluidEditor(m.fluidState)}

        let timeStamp = Js.Date.now() /. 1000.0
        let avMessage: avatarModelMessage = {
          canvasName: m.canvasName,
          browserId: m.browserId,
          tlid: None,
          timestamp: timeStamp,
        }

        let commands = list{hashcmd, acCmd, API.sendPresence(m, avMessage)}
        (m, Cmd.batch(commands))
      } else {
        (m, Cmd.none)
      }
    | OpenOmnibox(pos) =>
      let (cursorState, target) = (Omnibox(pos), None)
      let (m, acCmd) = processAutocompleteMods(m, list{ACSetTarget(target)})
      let m = {...m, cursorState: cursorState}
      (m, Cmd.batch(list{acCmd, CursorState.focusEntry(m)}))
    | Enter(tlid, id) =>
      let (cursorState, target) = switch TL.getPD(m, tlid, id) {
      | Some(pd) => (Entering(tlid, id), Some(tlid, pd))
      | None => (FluidEntering(tlid), None)
      }

      let (m, acCmd) = processAutocompleteMods(m, list{ACSetTarget(target)})
      let m = {...m, cursorState: cursorState}
      let (m, afCmd) = Analysis.analyzeFocused(m)
      (m, Cmd.batch(list{afCmd, acCmd, CursorState.focusEntry(m)}))
    | EnterWithOffset(tlid, id, offset) =>
      let (cursorState, target) = switch TL.getPD(m, tlid, id) {
      | Some(pd) => (Entering(tlid, id), Some(tlid, pd))
      | None => (FluidEntering(tlid), None)
      }

      let (m, acCmd) = processAutocompleteMods(m, list{ACSetTarget(target)})
      let m = {...m, cursorState: cursorState}
      let (m, afCmd) = Analysis.analyzeFocused(m)
      (m, Cmd.batch(list{afCmd, acCmd, CursorState.focusEntryWithOffset(m, offset)}))
    | RemoveToplevel(tl) => (Toplevel.remove(m, tl), Cmd.none)
    | SetToplevels(handlers, dbs, updateCurrent) =>
      let oldM = m
      let m = {...m, handlers: Handlers.fromList(handlers), dbs: DB.fromList(dbs)}

      /* If updateCurrent = false, bring back the TL being edited, so we don't lose work done since the
       API call */
      let m = if updateCurrent {
        m
      } else {
        bringBackCurrentTL(oldM, m)
      }
      let m = {
        let hTLIDs = List.map(~f=h => h.hTLID, handlers)
        let dbTLIDs = List.map(~f=db => db.dbTLID, dbs)
        {
          ...m,
          deletedHandlers: Map.removeMany(m.deletedHandlers, ~keys=hTLIDs),
          deletedDBs: Map.removeMany(m.deletedDBs, ~keys=dbTLIDs),
        }
      }

      let m = Refactor.updateUsageCounts(m)
      processAutocompleteMods(m, list{ACRegenerate})
    | UpdateToplevels(handlers, dbs, updateCurrent) =>
      let oldM = m
      let m = {
        ...m,
        handlers: Map.mergeRight(m.handlers, Handlers.fromList(handlers)),
        dbs: Map.mergeRight(m.dbs, DB.fromList(dbs)),
      }

      let (m, acCmd) = processAutocompleteMods(m, list{ACRegenerate})
      /* If updateCurrent = false, bring back the TL being edited, so we don't lose work done since the
       API call */
      let m = if updateCurrent {
        m
      } else {
        bringBackCurrentTL(oldM, m)
      }
      updateMod(
        SetToplevels(Map.values(m.handlers), Map.values(m.dbs), updateCurrent),
        (m, Cmd.batch(list{cmd, acCmd})),
      )
    | UpdateDeletedToplevels(dhandlers, ddbs) =>
      let dhandlers = Map.mergeRight(m.deletedHandlers, Handlers.fromList(dhandlers))

      let ddbs = Map.mergeRight(m.deletedDBs, DB.fromList(ddbs))
      updateMod(SetDeletedToplevels(Map.values(dhandlers), Map.values(ddbs)), (m, cmd))
    | SetDeletedToplevels(dhandlers, ddbs) =>
      let hTLIDs = List.map(~f=h => h.hTLID, dhandlers)
      let dbTLIDs = List.map(~f=db => db.dbTLID, ddbs)
      let m = {
        ...m,
        deletedHandlers: Handlers.fromList(dhandlers),
        deletedDBs: DB.fromList(ddbs),
        handlers: Map.removeMany(m.handlers, ~keys=hTLIDs),
        dbs: Map.removeMany(m.dbs, ~keys=dbTLIDs),
      }

      processAutocompleteMods(m, list{ACRegenerate})
    | UpdateAnalysis(traceID, dvals) =>
      let m = {
        ...m,
        analyses: Analysis.record(m.analyses, traceID, LoadableSuccess(dvals)),
      }

      let (m, _) = updateMod(Fluid.update(m, FluidUpdateAutocomplete), (m, Cmd.none))

      processAutocompleteMods(m, list{ACRegenerate})
    | UpdateWorkerSchedules(schedules) =>
      let m = {...m, workerSchedules: schedules}
      (m, Cmd.none)
    | UpdateTraces(traces) =>
      let m = Analysis.updateTraces(m, traces)
      let (m, afCmd) = Analysis.analyzeFocused(m)
      let (m, acCmd) = processAutocompleteMods(m, list{ACRegenerate})
      (m, Cmd.batch(list{afCmd, acCmd}))
    | OverrideTraces(traces) =>
      /* OverrideTraces takes a set of traces and merges it with the model, but if
       * the (tlid, traceID) pair occurs in both, the result will have its data
       * blown away.
       *
       * Use OverrideTraces when a set of traceIDs has been received by the client but
       * some/all of them might represent _mutated_ traces. (ie. a trace where if
       * you re-fetch the trace data you'd get a different set of input values or
       * stored function results */
      let newTraces = Analysis.mergeTraces(
        ~selectedTraceIDs=m.tlTraceIDs,
        ~onConflict=(_old, (newID, _)) => (newID, Error(NoneYet)),
        ~oldTraces=m.traces,
        ~newTraces=traces,
      )

      let m = {...m, traces: newTraces}
      let (m, afCmd) = Analysis.analyzeFocused(m)
      let (m, acCmd) = processAutocompleteMods(m, list{ACRegenerate})
      (m, Cmd.batch(list{afCmd, acCmd}))
    | UpdateTraceFunctionResult(tlid, traceID, callerID, fnName, hash, hashVersion, dval) =>
      let m = Analysis.replaceFunctionResult(
        m,
        tlid,
        traceID,
        callerID,
        fnName,
        hash,
        hashVersion,
        dval,
      )

      // traces could be missing
      let (m, afCmd) = Analysis.analyzeFocused(m)
      /* make sure we run the analysis even if the analyzeFocused conditions
       * don't hold, as we have a new result to be analyzed */
      let reExeCmd = Analysis.requestAnalysis(m, tlid, traceID)
      let (m, acCmd) = processAutocompleteMods(m, list{ACRegenerate})
      (m, Cmd.batch(list{afCmd, acCmd, reExeCmd}))
    | SetUserFunctions(userFuncs, deletedUserFuncs, updateCurrent) =>
      if userFuncs == list{} && deletedUserFuncs == list{} {
        // no need to do this if nothing changed
        (m, Cmd.none)
      } else {
        let oldM = m
        let m = {
          ...m,
          userFunctions: Map.mergeRight(
            m.userFunctions,
            UserFunctions.fromList(userFuncs),
          ) |> Map.removeMany(~keys=List.map(~f=UserFunctions.toID, deletedUserFuncs)),
          deletedUserFunctions: Map.mergeRight(
            m.deletedUserFunctions,
            UserFunctions.fromList(deletedUserFuncs),
          ) |> Map.removeMany(~keys=List.map(~f=UserFunctions.toID, userFuncs)),
        }

        /* Bring back the TL being edited, so we don't lose work done since the
         API call */
        let m = if updateCurrent {
          m
        } else {
          bringBackCurrentTL(oldM, m)
        }
        let m = Refactor.updateUsageCounts(m)
        let props = {usedFns: m.usedFns, userFunctions: m.userFunctions}
        let m = {...m, functions: Functions.update(props, m.functions)}
        processAutocompleteMods(m, list{ACRegenerate})
      }
    | SetTypes(userTipes, deletedUserTipes, updateCurrent) =>
      let m2 = {
        ...m,
        userTipes: Map.mergeRight(m.userTipes, UserTypes.fromList(userTipes)) |> Map.removeMany(
          ~keys=List.map(~f=UserTypes.toID, deletedUserTipes),
        ),
        deletedUserTipes: Map.mergeRight(
          m.deletedUserTipes,
          UserTypes.fromList(deletedUserTipes),
        ) |> Map.removeMany(~keys=List.map(~f=UserTypes.toID, userTipes)),
      }

      /* Bring back the TL being edited, so we don't lose work done since the
       API call */
      let m3 = switch CursorState.tlidOf(m.cursorState) {
      | Some(tlid) =>
        if updateCurrent {
          m2
        } else {
          TL.get(m, tlid)
          |> Option.andThen(~f=TL.asUserTipe)
          |> Option.map(~f=UserTypes.upsert(m2))
          |> Option.unwrap(~default=m2)
        }
      | None => m2
      }

      let m4 = Refactor.updateUsageCounts(m3)
      processAutocompleteMods(m4, list{ACRegenerate})
    | SetPermission(permission) => ({...m, permission: permission}, Cmd.none)
    | SetUnlockedDBs(unlockedDBs) => ({...m, unlockedDBs: unlockedDBs}, Cmd.none)
    | AppendUnlockedDBs(
        newDBs,
      ) => /* You probably don't want to use this, you probably want to wait for the
       * regular unlockedDBs API timer to do a full SetUnlockedDBs, but this
       * can be useful if you've done an operation to a DB and you know 100%
       * that it will be in the next unlockedDBs set.
       *
       * If your assumption is wrong, it'll be blown away by the next SetUnlockedDBs
       * operation regardless -- so as long as that interim period doesn't result
       * in the potential for dangerous operations, you should be fine.
       */
      ({...m, unlockedDBs: Set.union(m.unlockedDBs, newDBs)}, Cmd.none)
    | Delete404(f404) => (
        {
          ...m,
          f404s: List.filter(
            ~f=f => f.space ++ (f.path ++ f.modifier) != f404.space ++ (f404.path ++ f404.modifier),
            m.f404s,
          ),
        },
        Cmd.none,
      )
    | Append404s(f404s) =>
      let new404s =
        Belt.List.concat(f404s, m.f404s) |> List.uniqueBy(~f=f404 =>
          f404.space ++ (f404.path ++ (f404.modifier ++ (f404.timestamp ++ f404.traceID)))
        )

      ({...m, f404s: new404s}, Cmd.none)
    | ExpireAvatars => ({...m, avatarsList: m.avatarsList |> expireAvatars}, Cmd.none)
    | UpdateAvatarList(avatarsList) =>
      let updatedAvatars =
        avatarsList
        |> List.filter(~f=(avatar: Types.avatar) => avatar.browserId !== m.browserId)
        |> expireAvatars

      ({...m, avatarsList: updatedAvatars}, Cmd.none)
    | AppendStaticDeploy(d) => (
        {...m, staticDeploys: DarkStorage.appendDeploy(d, m.staticDeploys)},
        Cmd.none,
      )
    | SetHover(tlid, id) =>
      let nhovering = list{(tlid, id), ...m.hovering}
      ({...m, hovering: nhovering}, Cmd.none)
    | ClearHover(tlid, id) =>
      let nhovering = List.filter(~f=m => m != (tlid, id), m.hovering)
      ({...m, hovering: nhovering}, Cmd.none)
    | SetTLTraceID(tlid, traceID) =>
      let m = Analysis.setSelectedTraceID(m, tlid, traceID)
      let (m, afCmd) = Analysis.analyzeFocused(m)
      let newPage = Page.setPageTraceID(m.currentPage, traceID)
      let m = Page.setPage(m, m.currentPage, newPage)
      let navCmd = Url.navigateTo(newPage)
      let commands = list{afCmd, navCmd}
      (m, Cmd.batch(commands))
    | DragTL(
        tlid,
        offset,
        hasMoved,
        state,
      ) => /* Because mouseEvents are not perfectly reliable, we can end up in
       * weird dragging states. If we start dragging, make sure the state
       * we're in before isnt also dragging. */
      (
        {
          ...m,
          cursorState: DraggingTL(tlid, offset, hasMoved, CursorState.unwrap(state)),
        },
        Cmd.none,
      )
    | PanCanvas({viewportStart, viewportCurr, prevCursorState}) => (
        {
          ...m,
          cursorState: PanningCanvas({
            viewportStart: viewportStart,
            viewportCurr: viewportCurr,
            prevCursorState: prevCursorState,
          }),
        },
        Cmd.none,
      )
    | ExecutingFunctionBegan(tlid, id) =>
      let nexecutingFunctions = Belt.List.concat(m.executingFunctions, list{(tlid, id)})
      ({...m, executingFunctions: nexecutingFunctions}, Cmd.none)
    | ExecutingFunctionAPICall(tlid, id, name) =>
      switch TL.get(m, tlid) {
      | Some(tl) =>
        let traceID = Analysis.getSelectedTraceID(m, tlid)
        switch Option.andThen(traceID, ~f=Analysis.getTrace(m, tlid)) {
        | Some(traceID, _) =>
          switch Analysis.getArguments(m, tl, id, traceID) {
          | Some(args) =>
            let params = {
              efpTLID: tlid,
              efpCallerID: id,
              efpTraceID: traceID,
              efpFnName: name,
              efpArgs: args,
            }

            (m, API.executeFunction(m, params))
          | None =>
            (m, Cmd.none)
            |> Model.updateError(Error.set("Traces are not loaded for this handler"))
            |> updateMod(ExecutingFunctionComplete(list{(tlid, id)}))
          }
        | None =>
          (m, Cmd.none)
          |> Model.updateError(Error.set("Traces are not loaded for this handler"))
          |> updateMod(ExecutingFunctionComplete(list{(tlid, id)}))
        }
      | None =>
        // Attempted to execute a function in a toplevel that we just deleted!
        (m, Cmd.none) |> updateMod(ExecutingFunctionComplete(list{(tlid, id)}))
      }
    | ExecutingFunctionComplete(targets) =>
      let isComplete = target => \"<|"(not, List.member(~value=target, targets))
      let nexecutingFunctions = List.filter(~f=isComplete, m.executingFunctions)

      ({...m, executingFunctions: nexecutingFunctions}, Cmd.none)
    | MoveCanvasTo(offset, panAnimation) =>
      let newCanvasProps = {
        ...m.canvasProps,
        offset: offset,
        panAnimation: panAnimation,
        lastOffset: Some(m.canvasProps.offset),
      }

      ({...m, canvasProps: newCanvasProps}, Cmd.none)
    | CenterCanvasOn(tlid) =>
      switch TL.get(m, tlid) {
      | Some(tl) => (
          {
            ...m,
            canvasProps: {
              ...m.canvasProps,
              offset: Viewport.centerCanvasOn(tl),
              panAnimation: AnimateTransition,
            },
          },
          Cmd.none,
        )
      | None => (m, Cmd.none)
      }
    | TriggerHandlerAPICall(tlid) =>
      let traceID = Analysis.getSelectedTraceID(m, tlid)
      switch Option.andThen(traceID, ~f=Analysis.getTrace(m, tlid)) {
      | Some(traceID, Ok(traceData)) =>
        let handlerProps = RT.setHandlerExeState(tlid, Executing, m.handlerProps)

        (
          {...m, handlerProps: handlerProps},
          API.triggerHandler(m, {thTLID: tlid, thTraceID: traceID, thInput: traceData.input}),
        )
      | _ => (m, Cmd.none)
      }
    | InitIntrospect(tls) => (Introspect.refreshUsages(m, List.map(~f=TL.id, tls)), Cmd.none)
    | RefreshUsages(tlids) => (Introspect.refreshUsages(m, tlids), Cmd.none)
    | FluidCommandsShow(tlid, id) => (
        FluidCommands.show(m, tlid, id),
        Tea_html_cmds.focus(FluidCommands.filterInputID),
      )
    | FluidCommandsClose =>
      let cp = FluidCommands.reset(m)
      ({...m, fluidState: {...m.fluidState, cp: cp, selectionStart: None}}, Cmd.none)
    | FluidStartClick => ({...m, fluidState: {...m.fluidState, midClick: true}}, Cmd.none)
    | FluidEndClick => ({...m, fluidState: {...m.fluidState, midClick: false}}, Cmd.none)
    | Apply(fn) =>
      let mod_ = fn(m)
      updateMod(mod_, (m, cmd))
    | AutocompleteMod(mod_) => processAutocompleteMods(m, list{mod_})
    | SetClipboardContents(data, e) =>
      Clipboard.setData(data, e)
      (m, Cmd.none)
    | UpdateASTCache(tlid, str) =>
      let searchCache = m.searchCache |> Map.update(~key=tlid, ~f=_ => Some(str))

      ({...m, searchCache: searchCache}, Cmd.none)
    | InitASTCache(handlers, userFunctions) =>
      let hcache = handlers |> List.fold(~initial=m.searchCache, ~f=(cache, h) => {
        let value = FluidPrinter.eToHumanString(FluidAST.toExpr(h.ast))

        cache |> Map.add(~key=h.hTLID, ~value)
      })

      let searchCache = userFunctions |> List.fold(~initial=hcache, ~f=(cache, f) => {
        let value = FluidPrinter.eToHumanString(FluidAST.toExpr(f.ufAST))

        cache |> Map.add(~key=f.ufTLID, ~value)
      })

      ({...m, searchCache: searchCache}, Cmd.none)
    | FluidSetState(fluidState) => ({...m, fluidState: fluidState}, Cmd.none)
    | TLMenuUpdate(tlid, msg) => (TLMenu.update(m, tlid, msg), Cmd.none)
    | SettingsViewUpdate(msg) =>
      let settingsView = SettingsView.update(m.settingsView, msg)
      ({...m, settingsView: settingsView}, cmd)
    // applied from left to right
    | Many(mods) =>
      List.fold(~f=(model, mod') => updateMod(mod', model), ~initial=(m, Cmd.none), mods)
    }
  }

  let cmds = Cmd.batch(list{cmd, newcmd})
  let (newm, modi) = {
    let mTLID = switch m.cursorState {
    | FluidEntering(tlid) if Some(tlid) != CursorState.tlidOf(newm.cursorState) => Some(tlid)
    | _ => None
    }

    Fluid.cleanUp(newm, mTLID)
  }

  switch modi {
  | NoChange => (newm, cmds)
  | _ => updateMod(modi, (newm, cmds))
  }
}

let update_ = (msg: msg, m: model): modification => {
  if m.integrationTestState != NoIntegrationTest {
    Debug.loG("msg update", show_msg(msg))
  }
  switch msg {
  | GlobalKeyPress(event) => KeyPress.handler(event, m)
  | EntryInputMsg(query) =>
    Many(list{
      AutocompleteMod(ACSetQuery(query)),
      AutocompleteMod(ACSetVisible(true)),
      MakeCmd(CursorState.focusEntry(m)),
    })
  | EntrySubmitMsg => NoChange
  | AutocompleteClick(index) =>
    switch CursorState.unwrap(m.cursorState) {
    | Entering(tlid, id) =>
      let newcomplete = {...m.complete, index: index}
      let newm = {...m, complete: newcomplete}
      Entry.submit(newm, tlid, id, Entry.StayHere)
    | _ => NoChange
    }
  | FluidMsg(FluidUpdateDropdownIndex(index)) => Fluid.update(m, FluidUpdateDropdownIndex(index))
  | FluidMsg(FluidAutocompleteClick(item)) =>
    switch CursorState.unwrap(m.cursorState) {
    | FluidEntering(_) => Fluid.update(m, FluidAutocompleteClick(item))
    | _ => NoChange
    }
  | AppMouseDown(event) =>
    if event.button == Defaults.leftButton {
      switch m.cursorState {
      | PanningCanvas({prevCursorState, _}) =>
        /* In case we are already panning, we got into a weird state;
         * we should stop panning. */
        ReplaceAllModificationsWithThisOne(CursorState.setCursorState(prevCursorState))
      | _ =>
        PanCanvas({
          viewportStart: event.mePos,
          viewportCurr: event.mePos,
          prevCursorState: m.cursorState,
        })
      }
    } else {
      NoChange
    }
  | AppMouseDrag(mousePos) =>
    switch m.cursorState {
    | PanningCanvas({viewportStart, viewportCurr, prevCursorState}) =>
      let viewportNext = {vx: mousePos.x, vy: mousePos.y}
      let dx = viewportCurr.vx - viewportNext.vx
      let dy = viewportCurr.vy - viewportNext.vy
      Many(list{
        Viewport.moveCanvasBy(m, dx, dy),
        PanCanvas({
          viewportStart: viewportStart,
          viewportCurr: viewportNext,
          prevCursorState: prevCursorState,
        }),
      })
    | _ => NoChange
    }
  | AppScroll =>
    /* This is needed to ensure that when we
     * translate the canvas, it moves in absolute space (see docstring for more). */
    ReplaceAllModificationsWithThisOne(m => (m, View.zeroOutAppScroll))
  | WindowMouseUp(event) | AppMouseUp(event) =>
    let clickBehavior = switch m.currentPage {
    | FocusedFn(tlid, _) | FocusedType(tlid) | FocusedPackageManagerFn(tlid) =>
      // Clicking on the raw canvas should keep you selected to functions/types in their space
      let defaultBehaviour = Select(tlid, STTopLevelRoot)
      switch CursorState.unwrap(m.cursorState) {
      | Entering(
          tlid,
          id,
        ) => // If we click away from an entry box, commit it before doing the default behaviour
        list{Entry.commit(m, tlid, id), defaultBehaviour}
      | _ => list{defaultBehaviour}
      }
    | Architecture | FocusedDB(_) | FocusedHandler(_) =>
      if event.button == Defaults.leftButton {
        // Clicking on the canvas should deselect the current selection on the main canvas
        let defaultBehaviour = Deselect
        switch CursorState.unwrap(m.cursorState) {
        | Deselected =>
          let openAt = Some(Viewport.toAbsolute(m, event.mePos))
          list{AutocompleteMod(ACReset), Entry.openOmnibox(~openAt, ())}
        | Entering(
            tlid,
            id,
          ) => // If we click away from an entry box, commit it before doing the default behaviour
          list{Entry.commit(m, tlid, id), defaultBehaviour}
        | _ => list{defaultBehaviour}
        }
      } else {
        list{}
      }
    | SettingsModal(_) => // Click handled in component
      list{}
    }

    let clickBehavior = if Option.isSome(m.tooltipState.tooltipSource) {
      list{Tooltips.update(m.tooltipState, Close), ...clickBehavior}
    } else {
      clickBehavior
    }

    switch m.cursorState {
    | PanningCanvas({viewportStart, viewportCurr, prevCursorState}) =>
      let distSquared = (a: vPos, b: vPos): int => {
        let dx = b.vx - a.vx
        let dy = b.vy - a.vy
        dx * dx + dy * dy
      }

      let maxSquareDistToConsiderAsClick = 16
      if distSquared(viewportStart, viewportCurr) <= maxSquareDistToConsiderAsClick {
        /* {m with cursorState = prevCursorState} bypasses the focus part of
         * CursorState.setCursorState to avoid focusing any elements that
         * clickBehavior might dismiss (ex closing the omnibox). */
        Many(list{
          ReplaceAllModificationsWithThisOne(
            m => ({...m, cursorState: prevCursorState}, Tea.Cmd.none),
          ),
          ...clickBehavior,
        })
      } else {
        ReplaceAllModificationsWithThisOne(CursorState.setCursorState(prevCursorState))
      }
    | DraggingTL(draggingTLID, _, hasMoved, origCursorState) =>
      switch TL.get(m, draggingTLID) {
      | Some(tl) =>
        if hasMoved {
          // We've been updating tl.pos as mouse moves,
          // now want to report last pos to server
          Many(list{
            ReplaceAllModificationsWithThisOne(CursorState.setCursorState(origCursorState)),
            AddOps(list{MoveTL(draggingTLID, TL.pos(tl))}, FocusNoChange),
          })
        } else {
          // if we haven't moved, treat this as a single click and not a attempted drag
          let defaultBehaviour = Select(draggingTLID, STTopLevelRoot)
          switch origCursorState {
          | Entering(tlid, id) => Many(list{Entry.commit(m, tlid, id), defaultBehaviour})
          | _ => defaultBehaviour
          }
        }
      | None => ReplaceAllModificationsWithThisOne(CursorState.setCursorState(origCursorState))
      }
    | FluidEntering(_) => Fluid.update(m, FluidMouseUpExternal)
    | _ => NoChange
    }
  | IgnoreMouseUp => Fluid.update(m, FluidMouseUpExternal)
  | BlankOrMouseEnter(tlid, id, _) => SetHover(tlid, id)
  | BlankOrMouseLeave(tlid, id, _) => ClearHover(tlid, id)
  | MouseWheel(x, y) => Viewport.moveCanvasBy(m, x, y)
  | TraceMouseEnter(tlid, traceID, _) =>
    let traceCmd = switch Analysis.getTrace(m, tlid, traceID) {
    | Some(_, Error(_)) =>
      let (m, cmd) = Analysis.requestTrace(m, tlid, traceID)
      list{ReplaceAllModificationsWithThisOne(old => ({...old, syncState: m.syncState}, cmd))}
    | _ => list{}
    }

    Many(Belt.List.concat(traceCmd, list{SetHover(tlid, ID(traceID))}))
  | TraceMouseLeave(tlid, traceID, _) => ClearHover(tlid, ID(traceID))
  | TriggerHandler(tlid) => TriggerHandlerAPICall(tlid)
  | DragToplevel(_, mousePos) =>
    switch m.cursorState {
    | DraggingTL(draggingTLID, startVPos, _, origCursorState) =>
      let xDiff = mousePos.x - startVPos.vx
      let yDiff = mousePos.y - startVPos.vy
      let m2 = TL.move(draggingTLID, xDiff, yDiff, m)
      Many(list{
        SetToplevels(Map.values(m2.handlers), Map.values(m2.dbs), true),
        DragTL(draggingTLID, {vx: mousePos.x, vy: mousePos.y}, true, origCursorState),
      })
    | _ => NoChange
    }
  | TLDragRegionMouseDown(targetTLID, event) =>
    if event.button == Defaults.leftButton {
      let tl = TL.get(m, targetTLID)
      switch tl {
      | Some(TLPmFunc(_)) | Some(TLFunc(_)) | Some(TLTipe(_)) | None => NoChange
      | Some(TLHandler(_)) | Some(TLDB(_)) => DragTL(targetTLID, event.mePos, false, m.cursorState)
      }
    } else {
      NoChange
    }
  | TLDragRegionMouseUp(tlid, event) =>
    if event.button == Defaults.leftButton {
      switch m.cursorState {
      | DraggingTL(draggingTLID, _, hasMoved, origCursorState) =>
        switch TL.get(m, draggingTLID) {
        | Some(tl) =>
          if hasMoved {
            // We've been updating tl.pos as mouse moves,
            // now want to report last pos to server
            // the SetCursorState here isn't always necessary
            // because in the happy case we'll also receive
            // a ToplevelClick event, but it seems that sometimes
            // we don't, perhaps due to overlapping click handlers
            // There doesn't seem to be any harm in stopping dragging
            // here though
            Many(list{
              ReplaceAllModificationsWithThisOne(CursorState.setCursorState(origCursorState)),
              AddOps(list{MoveTL(draggingTLID, TL.pos(tl))}, FocusNoChange),
            })
          } else {
            // if we haven't moved, treat this as a single click and not a attempted drag
            let defaultBehaviour = Select(draggingTLID, STTopLevelRoot)
            switch origCursorState {
            | Entering(tlid, id) => Many(list{Entry.commit(m, tlid, id), defaultBehaviour})
            | _ => defaultBehaviour
            }
          }
        | None => ReplaceAllModificationsWithThisOne(CursorState.setCursorState(origCursorState))
        }
      | Entering(tlid, id) =>
        Many(list{Entry.commit(m, tlid, id), Select(tlid, STTopLevelRoot), FluidEndClick})
      | _ => Many(list{Select(tlid, STTopLevelRoot), FluidEndClick})
      }
    } else {
      NoChange
    }
  | BlankOrClick(targetExnID, targetID, event) =>
    let select = id => {
      let offset = Native.OffsetEstimator.estimateClickOffset(ID.toString(id), event)

      /* If we're in the Fluid world, we should treat clicking legacy BlankOr inputs
       * as double clicks to automatically enter them. */
      Selection.dblclick(m, targetExnID, targetID, offset)
    }

    switch m.cursorState {
    | Deselected => select(targetID)
    | DraggingTL(_, _, _, prevCursorState)
    | PanningCanvas({prevCursorState, _}) =>
      ReplaceAllModificationsWithThisOne(CursorState.setCursorState(prevCursorState))
    | Entering(tlid, fillingID) =>
      let defaultBehaviour = select(targetID)
      if fillingID == targetID {
        NoChange
      } else {
        // If we click away from an entry box, commit it before doing the default behaviour
        Many(list{Entry.commit(m, tlid, fillingID), defaultBehaviour})
      }
    | Omnibox(_) => select(targetID)
    | Selecting(_, _) => select(targetID)
    | FluidEntering(_) => select(targetID)
    }
  | BlankOrDoubleClick(targetExnID, targetID, event) =>
    let offset = Native.OffsetEstimator.estimateClickOffset(ID.toString(targetID), event)

    Selection.dblclick(m, targetExnID, targetID, offset)
  | ExecuteFunctionButton(tlid, id, name) =>
    let selectionTarget: tlidSelectTarget = /* Note that the intent here is to make the live value visible, which
     * is a side-effect of placing the caret right after the function name
     * in the handler where the function is being called.  We're relying on
     * the length of the function name representing the offset into the
     * tokenized function call node corresponding to this location. Eg:
     * foo|v1 a b */
    STCaret({astRef: ARFnCall(id), offset: String.length(name)})

    Many(list{
      ExecutingFunctionBegan(tlid, id),
      ExecutingFunctionAPICall(tlid, id, name),
      Select(tlid, selectionTarget),
    })
  | ExecuteFunctionFromWithin(p) =>
    Many(list{ExecutingFunctionBegan(p.efpTLID, p.efpCallerID), MakeCmd(API.executeFunction(m, p))})
  | TraceClick(tlid, traceID, _) =>
    switch m.cursorState {
    | DraggingTL(_, _, _, origCursorState) =>
      ReplaceAllModificationsWithThisOne(CursorState.setCursorState(origCursorState))
    | Deselected => Many(list{Select(tlid, STTopLevelRoot), SetTLTraceID(tlid, traceID)})
    | _ => SetTLTraceID(tlid, traceID)
    }
  | StartMigration(tlid) =>
    let mdb = tlid |> TL.get(m) |> Option.andThen(~f=TL.asDB)
    switch mdb {
    | Some(db) => DB.startMigration(tlid, db.cols)
    | None => NoChange
    }
  | AbandonMigration(tlid) => AddOps(list{AbandonDBMigration(tlid)}, FocusNothing)
  | DeleteColInDB(tlid, nameId) =>
    let mdb = tlid |> TL.get(m) |> Option.andThen(~f=TL.asDB)
    switch mdb {
    | Some(db) =>
      if DB.isMigrationCol(db, nameId) {
        AddOps(list{DeleteColInDBMigration(tlid, nameId)}, FocusNothing)
      } else {
        AddOps(list{DeleteDBCol(tlid, nameId)}, FocusNothing)
      }
    | None => NoChange
    }
  | ToggleEditorSetting(fn) =>
    ReplaceAllModificationsWithThisOne(
      m => ({...m, editorSettings: fn(m.editorSettings)}, Cmd.none),
    )
  | SaveTestButton => MakeCmd(API.saveTest(m))
  | FinishIntegrationTest => EndIntegrationTest
  | ExtractFunction =>
    switch m.cursorState {
    | Selecting(tlid, mId) =>
      switch (TL.get(m, tlid), mId) {
      | (Some(tl), Some(id)) => Refactor.extractFunction(m, tl, id)
      | _ => NoChange
      }
    | _ => NoChange
    }
  | DeleteUserFunctionParameter(uftlid, upf) =>
    switch TL.get(m, uftlid) |> Option.andThen(~f=TL.asUserFunction) {
    | Some(uf) =>
      let replacement = UserFunctions.removeParameter(uf, upf)
      let newCalls = Refactor.removeFunctionParameter(m, uf, upf)
      AddOps(list{SetFunction(replacement), ...newCalls}, FocusNext(uf.ufTLID, None))
    | None => NoChange
    }
  | AddUserFunctionParameter(uftlid) =>
    switch TL.get(m, uftlid) |> Option.andThen(~f=TL.asUserFunction) {
    | Some(uf) =>
      let nextId = UserFunctions.idOfLastBlankor(uf)
      Refactor.addFunctionParameter(m, uf, nextId)
    | None => NoChange
    }
  | DeleteUserTypeField(tipetlid, field) =>
    switch TL.get(m, tipetlid) |> Option.andThen(~f=TL.asUserTipe) {
    | Some(tipe) =>
      let replacement = UserTypes.removeField(tipe, field)
      AddOps(list{SetType(replacement)}, FocusNext(tipe.utTLID, None))
    | None => NoChange
    }
  | ToplevelDelete(tlid) =>
    let resetMenu = // So menu doesn't stay at opened state when TL is restored
    ReplaceAllModificationsWithThisOne(m => (TLMenu.resetMenu(tlid, m), Cmd.none))

    TL.get(m, tlid)
    |> Option.map(~f=tl => Many(list{
      RemoveToplevel(tl),
      AddOps(list{DeleteTL(TL.id(tl))}, FocusNothing),
      resetMenu,
    }))
    |> Option.unwrap(~default=NoChange)
  | ToplevelDeleteForever(tlid) =>
    Many(list{
      DeleteToplevelForeverAPICall(tlid),
      ReplaceAllModificationsWithThisOne(
        m => (
          {
            ...m,
            deletedHandlers: Map.remove(~key=tlid, m.deletedHandlers),
            deletedDBs: Map.remove(~key=tlid, m.deletedDBs),
          },
          Cmd.none,
        ),
      ),
    })
  | DeleteUserFunction(tlid) =>
    let page = Page.maybeChangeFromPage(tlid, m.currentPage)
    Many(list{AddOps(list{DeleteFunction(tlid)}, FocusSame), ...page})
  | RestoreToplevel(tlid) =>
    let page = Page.getPageFromTLID(m, tlid)
    AddOps(list{UndoTL(tlid)}, FocusPageAndCursor(page, Selecting(tlid, None)))
  | DeleteUserFunctionForever(tlid) =>
    Many(list{
      DeleteToplevelForeverAPICall(tlid),
      ReplaceAllModificationsWithThisOne(
        m => (
          {
            ...m,
            deletedUserFunctions: Map.remove(~key=tlid, m.deletedUserFunctions),
          },
          Cmd.none,
        ),
      ),
    })
  | DeleteUserType(tlid) =>
    let page = Page.maybeChangeFromPage(tlid, m.currentPage)
    Many(list{AddOps(list{DeleteType(tlid)}, FocusSame), ...page})
  | UpdateHeapio(msg) =>
    Entry.sendHeapioMessage(msg)
    NoChange
  | ToolTipMsg(msg) =>
    if msg == UpdateTutorial(CloseTutorial) && m.firstVisitToDark {
      Entry.sendHeapioMessage(WelcomeModal)
    }
    Tooltips.update(m.tooltipState, msg)
  | DeleteUserTypeForever(tlid) =>
    Many(list{
      DeleteToplevelForeverAPICall(tlid),
      ReplaceAllModificationsWithThisOne(
        m => (
          {
            ...m,
            deletedUserTipes: Map.remove(~key=tlid, m.deletedUserTipes),
          },
          Cmd.none,
        ),
      ),
    })
  | AddOpsAPICallback(focus, params, Ok(r: addOpAPIResponse)) =>
    let (m, newOps, _) = API.filterOpsAndResult(m, params, None)
    let params = {...params, ops: newOps}
    let initialMods = applyOpsToClient(focus !== FocusNoChange, params, r.result)

    let focusMods = if focus == FocusNoChange {
      list{}
    } else {
      let m = {
        ...m,
        opCtrs: Map.add(m.opCtrs, ~key=params.clientOpCtrId, ~value=params.opCtr),
        handlers: Map.mergeRight(m.handlers, Handlers.fromList(r.result.handlers)),
        dbs: Map.mergeRight(m.dbs, DB.fromList(r.result.dbs)),
        userFunctions: Map.mergeRight(
          m.userFunctions,
          UserFunctions.fromList(r.result.userFunctions),
        ),
        userTipes: Map.mergeRight(m.userTipes, UserTypes.fromList(r.result.userTipes)),
      }

      let newState = processFocus(m, focus)
      list{AutocompleteMod(ACReset), Model.updateErrorMod(Error.clear), newState}
    }

    Many(Belt.List.concat(initialMods, focusMods))
  | AddOpsPusherMsg(msg) =>
    if msg.params.clientOpCtrId == m.clientOpCtrId {
      NoChange
    } else {
      /* msg was sent from this client, we've already handled it
       in AddOpsAPICallback */

      let (m, newOps, result) = API.filterOpsAndResult(m, msg.params, Some(msg.result))

      let params = {...msg.params, ops: newOps}
      let initialMods = applyOpsToClient(false, params, result |> Option.unwrapUnsafe)

      Many(Belt.List.concat(initialMods, list{MakeCmd(CursorState.focusEntry(m))}))
    }
  | FetchAllTracesAPICallback(Ok(x)) =>
    let traces = List.fold(x.traces, ~initial=Map.String.empty, ~f=(dict, (tlid, traceid)) => {
      let trace = (traceid, Error(NoneYet))
      Map.update(dict, ~key=TLID.toString(tlid), ~f=x =>
        switch x {
        | Some(existing) => Some(Belt.List.concat(existing, list{trace}))
        | None => Some(list{trace})
        }
      )
    })

    UpdateTraces(traces)
  | FetchAllTracesAPICallback(Error(x)) =>
    Model.updateErrorMod(Error.set("Failed to load traces: " ++ Tea_http.string_of_error(x)))
  | InitialLoadAPICallback(focus, extraMod /* for integration tests, maybe more */, Ok(r)) =>
    let pfM = {
      ...m,
      opCtrs: r.opCtrs,
      handlers: Handlers.fromList(r.handlers),
      dbs: DB.fromList(r.dbs),
      userFunctions: UserFunctions.fromList(r.userFunctions),
      userTipes: UserTypes.fromList(r.userTipes),
      handlerProps: ViewUtils.createHandlerProp(r.handlers),
    }

    let newState = processFocus(pfM, focus)
    let allTLs = TL.all(pfM)
    Many(list{
      ReplaceAllModificationsWithThisOne(
        m => {
          let settingsView = SettingsView.update(
            m.settingsView,
            SetSettingsView(r.canvasList, m.username, r.orgs, r.orgCanvasList),
          )

          (
            {
              ...m,
              opCtrs: r.opCtrs,
              account: r.account,
              settingsView: settingsView,
              secrets: r.secrets,
            },
            Cmd.none,
          )
        },
      ),
      SetToplevels(r.handlers, r.dbs, true),
      SetDeletedToplevels(r.deletedHandlers, r.deletedDBs),
      SetUserFunctions(r.userFunctions, r.deletedUserFunctions, true),
      SetTypes(r.userTipes, r.deletedUserTipes, true),
      SetUnlockedDBs(r.unlockedDBs),
      UpdateWorkerSchedules(r.workerSchedules),
      SetPermission(r.permission),
      Get404sAPICall,
      AppendStaticDeploy(r.staticDeploys),
      AutocompleteMod(ACReset),
      Model.updateErrorMod(Error.clear),
      extraMod,
      newState,
      MakeCmd(API.fetchAllTraces(m)),
      InitIntrospect(Map.values(allTLs)),
      InitASTCache(r.handlers, r.userFunctions),
    })
  | SaveTestAPICallback(Ok(msg)) => Model.updateErrorMod(Error.set("Success! " ++ msg))
  | ExecuteFunctionAPICallback(params, Ok(dval, hash, hashVersion, tlids, unlockedDBs)) =>
    let traces = List.map(
      ~f=tlid => (TLID.toString(tlid), list{(params.efpTraceID, Error(NoneYet))}),
      tlids,
    )

    Many(list{
      UpdateTraceFunctionResult(
        params.efpTLID,
        params.efpTraceID,
        params.efpCallerID,
        params.efpFnName,
        hash,
        hashVersion,
        dval,
      ),
      ExecutingFunctionComplete(list{(params.efpTLID, params.efpCallerID)}),
      OverrideTraces(Map.String.fromList(traces)),
      SetUnlockedDBs(unlockedDBs),
    })
  | TriggerHandlerAPICallback(params, Ok(tlids)) =>
    let traces: Prelude.traces =
      List.map(
        ~f=tlid => (TLID.toString(tlid), list{(params.thTraceID, Error(NoneYet))}),
        tlids,
      ) |> Map.String.fromList

    Many(list{
      OverrideTraces(traces),
      ReplaceAllModificationsWithThisOne(
        m => {
          let handlerProps = RT.setHandlerExeState(params.thTLID, Complete, m.handlerProps)

          ({...m, handlerProps: handlerProps}, Cmd.none)
        },
      ),
    })
  | GetUnlockedDBsAPICallback(Ok(unlockedDBs)) =>
    Many(list{
      ReplaceAllModificationsWithThisOne(
        m => (Sync.markResponseInModel(m, ~key="unlocked"), Cmd.none),
      ),
      SetUnlockedDBs(unlockedDBs),
    })
  | Get404sAPICallback(Ok(f404s)) => Append404s(f404s)
  | LoadPackagesAPICallback(Ok(loadedPackages)) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let pkgs = loadedPackages |> List.map(~f=pkg => (pkg.pfTLID, pkg)) |> TLIDDict.fromList

        let props = {usedFns: m.usedFns, userFunctions: m.userFunctions}
        let m = {...m, functions: Functions.setPackages(pkgs, props, m.functions)}

        /* We need to update the list of usages due to package manager functions.
         * Ideally we would make this dependency more explicit. */
        let m = Introspect.refreshUsages(m, Map.keys(m.handlers))
        (m, Cmd.none)
      },
    )
  | InsertSecretCallback(Ok(secrets)) =>
    ReplaceAllModificationsWithThisOne(m => ({...m, secrets: secrets}, Cmd.none))
  | NewTracePush(traceID, tlids) =>
    let traces = List.map(~f=tlid => (TLID.toString(tlid), list{(traceID, Error(NoneYet))}), tlids)

    UpdateTraces(Map.String.fromList(traces))
  | New404Push(f404) => Append404s(list{f404})
  | NewPresencePush(avatarsList) => UpdateAvatarList(avatarsList)
  | NewStaticDeployPush(asset) => AppendStaticDeploy(list{asset})
  | WorkerStatePush(ws) => UpdateWorkerSchedules(ws)
  | Delete404APICall(f404) =>
    Many(list{
      // This deletion is speculative
      Delete404(f404),
      MakeCmd(API.delete404(m, f404)),
    })
  | Delete404APICallback(params, result) =>
    switch result {
    | Ok(_) => NoChange
    | Error(err) =>
      Many(list{
        Append404s(list{params}) /* Rollback the speculative deletion */,
        HandleAPIError(
          APIError.make(
            ~context="Delete404",
            ~importance=ImportantError,
            ~requestParams=Encoders.fof(params),
            ~reload=false,
            err,
          ),
        ),
      })
    }
  | DeleteToplevelForeverAPICallback(params, result) =>
    switch result {
    | Ok(_) => NoChange
    | Error(err) =>
      Many(list{
        // Rollback the intended deletion
        HandleAPIError(
          APIError.make(
            ~context="DeleteToplevelForeverAPICallback",
            ~importance=ImportantError,
            ~requestParams=Encoders.deleteToplevelForeverAPIParams(params),
            ~reload=false,
            err,
          ),
        ),
      })
    }
  | ReceiveAnalysis(result) =>
    switch result {
    | Ok(id, analysisResults) => UpdateAnalysis(id, analysisResults)
    | Error(AnalysisExecutionError(_, str)) => Model.updateErrorMod(Error.set(str))
    | Error(AnalysisParseError(str)) => Model.updateErrorMod(Error.set(str))
    }
  | ReceiveFetch(TraceFetchFailure(params, _, "Bad credentials")) =>
    HandleAPIError(
      APIError.make(
        ~context="TraceFetch",
        ~importance=ImportantError,
        ~reload=true,
        ~requestParams=Encoders.getTraceDataAPIParams(params),
        /* not a great error ... but this is an api error without a
         * corresponding actual http error */
        Tea.Http.Aborted,
      ),
    )
  | ReceiveFetch(TraceFetchFailure(params, url, error))
    if error == "Selected trace too large for the editor to load, maybe try another?" =>
    let traces = Map.String.fromList(list{
      (TLID.toString(params.gtdrpTlid), list{(params.gtdrpTraceID, Error(MaximumCallStackError))}),
    })

    Many(list{
      ReplaceAllModificationsWithThisOne(
        m => {
          let key = "tracefetch-" ++ params.gtdrpTraceID
          let m = Sync.markResponseInModel(m, ~key)
          Rollbar.displayAndReportError(m, "Error fetching trace", Some(url), Some(error))
        },
      ),
      UpdateTraces(traces),
    })
  | ReceiveFetch(TraceFetchFailure(params, url, error)) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let key = "tracefetch-" ++ params.gtdrpTraceID
        let m = Sync.markResponseInModel(m, ~key)
        Rollbar.displayAndReportError(m, "Error fetching trace", Some(url), Some(error))
      },
    )
  | ReceiveFetch(TraceFetchSuccess(params, result)) =>
    let traces = Map.String.fromList(list{(TLID.toString(params.gtdrpTlid), list{result.trace})})

    Many(list{
      ReplaceAllModificationsWithThisOne(
        m => {
          let key = "tracefetch-" ++ params.gtdrpTraceID
          (Sync.markResponseInModel(m, ~key), Cmd.none)
        },
      ),
      UpdateTraces(traces),
    })
  | ReceiveFetch(TraceFetchMissing(params)) =>
    // We'll force it so no need to update syncState
    let (_, cmd) = Analysis.requestTrace(~force=true, m, params.gtdrpTlid, params.gtdrpTraceID)

    MakeCmd(cmd)
  | ReceiveFetch(DbStatsFetchFailure(params, _, "Bad credentials")) =>
    HandleAPIError(
      APIError.make(
        ~context="DbStatsFetch",
        ~importance=ImportantError,
        ~reload=true,
        ~requestParams=Encoders.dbStatsAPIParams(params),
        /* not a great error ... but this is an api error without a
         * corresponding actual http error */
        Tea.Http.Aborted,
      ),
    )
  | ReceiveFetch(DbStatsFetchFailure(params, url, error)) =>
    let key = params.dbStatsTlids |> List.map(~f=TLID.toString) |> String.join(~sep=",")

    ReplaceAllModificationsWithThisOne(
      m => {
        let key = "update-db-stats-" ++ key
        let m = Sync.markResponseInModel(m, ~key)
        Rollbar.displayAndReportError(m, "Error fetching db stats", Some(url), Some(error))
      },
    )
  | ReceiveFetch(DbStatsFetchMissing(params)) =>
    let key = params.dbStatsTlids |> List.map(~f=TLID.toString) |> String.join(~sep=",")

    ReplaceAllModificationsWithThisOne(
      m => {
        let key = "update-db-stats-" ++ key
        (Sync.markResponseInModel(m, ~key), Cmd.none)
      },
    )
  | ReceiveFetch(DbStatsFetchSuccess(params, result)) =>
    let key = params.dbStatsTlids |> List.map(~f=TLID.toString) |> String.join(~sep=",")

    ReplaceAllModificationsWithThisOne(
      m => {
        let m = Sync.markResponseInModel(m, ~key="update-db-stats-" ++ key)
        let newStore = Map.merge(~f=(_k, v1, v2) =>
          switch (v1, v2) {
          | (None, None) => None
          | (Some(l), None) => Some(l)
          | (None, Some(r)) => Some(r)
          | (Some(_), Some(r)) => Some(r)
          }
        , m.dbStats, result)

        let m = {...m, dbStats: newStore}
        (m, Cmd.none)
      },
    )
  | ReceiveFetch(WorkerStatsFetchFailure(params, _, "Bad credentials")) =>
    HandleAPIError(
      APIError.make(
        ~context="WorkerStatsFetch",
        ~importance=ImportantError,
        ~reload=true,
        ~requestParams=Encoders.workerStatsAPIParams(params),
        /* not a great error ... but this is an api error without a
         * corresponding actual http error */
        Tea.Http.Aborted,
      ),
    )
  | ReceiveFetch(WorkerStatsFetchFailure(params, url, error)) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let key = "get-worker-stats-" ++ TLID.toString(params.workerStatsTlid)

        let m = Sync.markResponseInModel(m, ~key)
        Rollbar.displayAndReportError(m, "Error fetching db stats", Some(url), Some(error))
      },
    )
  | ReceiveFetch(WorkerStatsFetchMissing(params)) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let key = "get-worker-stats-" ++ TLID.toString(params.workerStatsTlid)

        (Sync.markResponseInModel(m, ~key), Cmd.none)
      },
    )
  | ReceiveFetch(WorkerStatsFetchSuccess(params, result)) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let key = "get-worker-stats-" ++ TLID.toString(params.workerStatsTlid)

        let m = Sync.markResponseInModel(m, ~key)
        let tlid = params.workerStatsTlid
        let workerStats = Map.add(~key=tlid, ~value=result, m.workerStats)
        ({...m, workerStats: workerStats}, Cmd.none)
      },
    )
  | AddOpsAPICallback(_, params, Error(err)) =>
    HandleAPIError(
      APIError.make(
        ~context="AddOps",
        ~importance=ImportantError,
        ~requestParams=Encoders.addOpAPIParams(params),
        ~reload=false,
        err,
      ),
    )
  | SaveTestAPICallback(Error(err)) =>
    Model.updateErrorMod(Error.set("Error: " ++ Tea_http.string_of_error(err)))
  | ExecuteFunctionAPICallback(params, Error(err)) =>
    HandleAPIError(
      APIError.make(
        ~context="ExecuteFunction",
        ~importance=ImportantError,
        ~requestParams=Encoders.executeFunctionAPIParams(params),
        ~reload=false,
        err,
      ),
    )
  | TriggerHandlerAPICallback(_, Error(err)) =>
    HandleAPIError(
      APIError.make(~context="TriggerHandler", ~importance=ImportantError, err, ~reload=false),
    )
  | InitialLoadAPICallback(_, _, Error(err)) =>
    HandleAPIError(
      APIError.make(~context="InitialLoad", ~importance=ImportantError, err, ~reload=false),
    )
  | GetUnlockedDBsAPICallback(Error(err)) =>
    Many(list{
      ReplaceAllModificationsWithThisOne(
        m => (Sync.markResponseInModel(m, ~key="unlocked"), Cmd.none),
      ),
      HandleAPIError(
        APIError.make(~context="GetUnlockedDBs", ~importance=IgnorableError, ~reload=false, err),
      ),
    })
  | Get404sAPICallback(Error(err)) =>
    HandleAPIError(
      APIError.make(~context="Get404s", ~importance=ImportantError, ~reload=false, err),
    )
  | LoadPackagesAPICallback(Error(err)) =>
    HandleAPIError(
      APIError.make(~context="LoadPackages", ~importance=ImportantError, ~reload=false, err),
    )
  | InsertSecretCallback(Error(err)) =>
    HandleAPIError(
      APIError.make(~context="InsertSecrets", ~importance=ImportantError, ~reload=false, err),
    )
  | JSError(msg) => Model.updateErrorMod(Error.set("Error in JS: " ++ msg))
  | LocationChange(loc) => Url.changeLocation(loc)
  | TimerFire(action, _) =>
    switch action {
    | RefreshAnalysis =>
      let getUnlockedDBs = // Small optimization
      if Map.length(m.dbs) > 0 {
        GetUnlockedDBsAPICall
      } else {
        NoChange
      }

      switch Toplevel.selected(m) {
      | Some(tl) if Toplevel.isDB(tl) =>
        Many(list{
          /* DB stats can be very slow, which makes users unsure of whether
           * it's working at all. Commenting this out is enough to disable
           * it, as the UI does not appear if the DB stats API call isn't
           * run. */
          // UpdateDBStatsAPICall (TL.id tl);
          getUnlockedDBs,
        })
      | Some(tl) if Toplevel.isWorkerHandler(tl) =>
        Many(list{GetWorkerStatsAPICall(TL.id(tl)), getUnlockedDBs})
      | _ => getUnlockedDBs
      }
    | RefreshAvatars => ExpireAvatars
    | _ => NoChange
    }
  | IgnoreMsg(_) =>
    /* Many times we have to receive a Msg and we don't actually do anything.
     * To lower to conceptual load, we send an IgnoreMsg, rather than a
     * different msg each time that we have to understand. */
    NoChange
  | PageVisibilityChange(vis) =>
    ReplaceAllModificationsWithThisOne(m => ({...m, visibility: vis}, Cmd.none))
  | CreateHandlerFrom404({space, path, modifier, _} as fof) =>
    let center = Viewport.findNewPos(m)
    let tlid = gtlid()
    let pos = center
    let ast = ProgramTypes.Expr.EBlank(gid())
    let aHandler = {
      ast: FluidAST.ofExpr(ast),
      spec: {
        space: B.newF(space),
        name: B.newF(path),
        modifier: B.newF(modifier),
      },
      hTLID: tlid,
      pos: pos,
    }

    let traces = List.fold(
      ~initial=list{},
      ~f=(acc, search) =>
        if (
          search.space == fof.space && (search.path == fof.path && search.modifier == fof.modifier)
        ) {
          list{(search.traceID, Error(NoneYet)), ...acc}
        } else {
          acc
        },
      m.f404s,
    )

    let traceMods = switch List.head(traces) {
    | Some(first, _) =>
      let traceDict = Map.String.fromList(list{(TLID.toString(tlid), traces)})

      list{UpdateTraces(traceDict), SetTLTraceID(tlid, first)}
    | None => list{}
    }

    /* It's important that we update the traces + set the cursor _first_
     * -- otherwise both of them will attempt to 'analyzeFocused' on the new
     * TLID which might not have made it to the server yet.
     *
     * By doing them first they'll fire the analysis on whatever the user is
     * currently focused on, which is safe.
     */
    Many(
      Belt.List.concat(
        traceMods,
        list{
          AddOps(
            list{SetHandler(tlid, pos, aHandler)},
            FocusPageAndCursor(FocusedHandler(tlid, None, true), FluidEntering(tlid)),
          ),
          Delete404(fof),
        },
      ),
    )
  | SidebarMsg(msg) => ViewSidebar.update(msg)
  | CreateRouteHandler(action) =>
    let center = Viewport.findNewPos(m)
    Entry.submitOmniAction(m, center, action)
  | CreateDBTable =>
    let center = Viewport.findNewPos(m)
    Refactor.createNewDB(m, None, center)
  | CreateFunction => Refactor.createNewFunction(m, None)
  | CreateType =>
    let tipe = Refactor.generateEmptyUserType()
    Many(list{
      AddOps(list{SetType(tipe)}, FocusNothing),
      MakeCmd(Url.navigateTo(FocusedType(tipe.utTLID))),
    })
  | EnablePanning(pan) => ReplaceAllModificationsWithThisOne(Viewport.enablePan(pan))
  | ClipboardCopyEvent(e) =>
    let toast = ReplaceAllModificationsWithThisOne(
      m => ({...m, toast: {...m.toast, toastMessage: Some("Copied!")}}, Cmd.none),
    )

    let clipboardData = Fluid.getCopySelection(m)
    Many(list{SetClipboardContents(clipboardData, e), toast})
  | ClipboardPasteEvent(e) =>
    let data = Clipboard.getData(e)
    Fluid.update(m, FluidPaste(data))
  | ClipboardCutEvent(e) =>
    let toast = ReplaceAllModificationsWithThisOne(
      m => ({...m, toast: {...m.toast, toastMessage: Some("Copied!")}}, Cmd.none),
    )

    let (copyData, mod_) = (Fluid.getCopySelection(m), Apply(m => Fluid.update(m, FluidCut)))

    Many(list{SetClipboardContents(copyData, e), mod_, toast})
  | ClipboardCopyLivevalue(lv, pos) =>
    let lv = lv |> Regex.replace(~re=Regex.regex("(^\")|(\"$)"), ~repl="")
    Native.Clipboard.copyToClipboard(lv)
    ReplaceAllModificationsWithThisOne(
      m => ({...m, toast: {toastMessage: Some("Copied!"), toastPos: Some(pos)}}, Cmd.none),
    )
  | EventDecoderError(name, key, error) =>
    /* Consider rollbar'ing here, but consider the following before doing so:
     *    - old clients after a deploy
     *    - lots of events using a bad decoder
     *    - rollbar token exhaustion */
    Model.updateErrorMod(
      Error.set(
        "INTERNAL: Error decoding js event " ++
        (name ++
        (" with key " ++ (key ++ (" got error: \"" ++ (error ++ "\""))))),
      ),
    )
  | CanvasPanAnimationEnd =>
    ReplaceAllModificationsWithThisOne(
      m => (
        {
          ...m,
          canvasProps: {...m.canvasProps, panAnimation: DontAnimateTransition},
        },
        Cmd.none,
      ),
    )
  | GoTo(page) => MakeCmd(Url.navigateTo(page))
  | SetHoveringReferences(tlid, ids) => Introspect.setHoveringReferences(tlid, ids)
  | TriggerSendPresenceCallback(Ok(_)) => NoChange
  | TriggerSendPresenceCallback(Error(err)) =>
    HandleAPIError(
      APIError.make(
        ~context="TriggerSendPresenceCallback",
        ~importance=IgnorableError,
        ~reload=false,
        err,
      ),
    )
  | FluidMsg(FluidCut) | FluidMsg(FluidPaste(_)) =>
    recover("Fluid functions should not happen here", ~debug=msg, NoChange)
  | FluidMsg(FluidCommandsFilter(query)) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let cp = FluidCommands.filter(m, query, m.fluidState.cp)
        ({...m, fluidState: {...m.fluidState, cp: cp}}, Cmd.none)
      },
    )
  | FluidMsg(FluidCommandsClick(cmd)) =>
    Many(list{FluidCommands.runCommand(m, cmd), FluidCommandsClose})
  | TakeOffErrorRail(tlid, id) =>
    switch TL.get(m, tlid) {
    | Some(tl) => Refactor.takeOffRail(m, tl, id)
    | _ => NoChange
    }
  | UploadFn(tlid) =>
    TL.get(m, tlid)
    |> Option.andThen(~f=TL.asUserFunction)
    |> Option.map(~f=uplFn => API.uploadFn(m, {uplFn: uplFn}))
    |> Option.map(~f=cmd => MakeCmd(cmd))
    |> Option.unwrap(~default=Model.updateErrorMod(Error.set("No function to upload")))
  | SetHandlerExeIdle(tlid) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let handlerProps = RT.setHandlerExeState(tlid, Idle, m.handlerProps)
        ({...m, handlerProps: handlerProps}, Cmd.none)
      },
    )
  | CopyCurl(tlid, pos) => CurlCommand.copyCurlMod(m, tlid, pos)
  | TLMenuMsg(tlid, msg) => TLMenuUpdate(tlid, msg)
  | FluidMsg(FluidMouseDown(targetExnID)) =>
    let defaultBehaviour = list{FluidStartClick, Select(targetExnID, STTopLevelRoot)}

    switch m.cursorState {
    | Entering(tlid, id) =>
      Many(list{
        // If we click away from an entry box, commit it before doing the default behaviour
        Entry.commit(m, tlid, id),
        ...defaultBehaviour,
      })
    | _ => Many(defaultBehaviour)
    }
  | FluidMsg(FluidMouseUp({tlid: targetExnID, _}) as msg) =>
    Many(list{Select(targetExnID, STTopLevelRoot), Apply(m => Fluid.update(m, msg))})
  | FluidMsg(msg) =>
    // Handle all other messages
    Fluid.update(m, msg)
  | ResetToast =>
    ReplaceAllModificationsWithThisOne(m => ({...m, toast: Defaults.defaultToast}, Cmd.none))
  | HideTopbar => ReplaceAllModificationsWithThisOne(m => ({...m, showTopbar: false}, Cmd.none))
  | LogoutOfDark =>
    ReplaceAllModificationsWithThisOne(
      m => ({...m, editorSettings: {...m.editorSettings, runTimers: false}}, API.logout(m)),
    )
  | LogoutAPICallback =>
    // For some reason the Tea.Navigation.modifyUrl and .newUrl doesn't work
    Native.Ext.redirect("/login")
    NoChange
  | GoToArchitecturalView => Many(list{Deselect, MakeCmd(Url.navigateTo(Architecture))})
  | DismissErrorBar => Model.updateErrorMod(Error.clear)
  | PauseWorker(workerName) =>
    MakeCmd(API.updateWorkerSchedule(m, {workerName: workerName, schedule: "pause"}))
  | RunWorker(workerName) =>
    MakeCmd(API.updateWorkerSchedule(m, {workerName: workerName, schedule: "run"}))
  | UpdateWorkerScheduleCallback(Ok(schedules)) => UpdateWorkerSchedules(schedules)
  | UpdateWorkerScheduleCallback(Error(_)) =>
    Model.updateErrorMod(Error.set("Failed to update worker schedule"))
  | NewTabFromTLMenu(url, tlid) =>
    Native.Window.openUrl(url, "_blank")
    TLMenuUpdate(tlid, CloseMenu)
  | SettingsViewMsg(msg) =>
    let mods = SettingsView.getModifications(m, msg)
    Many(mods)
  | FnParamMsg(msg) => FnParams.update(m, msg)
  | UploadFnAPICallback(_, Error(err)) =>
    HandleAPIError(
      APIError.make(~context="UploadFnAPICallback", ~importance=IgnorableError, ~reload=false, err),
    )
  | UploadFnAPICallback(_, Ok(_)) =>
    Model.updateErrorMod(Error.set("Successfully uploaded function"))
  | SecretMsg(msg) => InsertSecret.update(msg)
  }
}

let rec filter_read_only = (m: model, modification: modification) =>
  if m.permission == Some(ReadWrite) {
    modification
  } else {
    switch modification {
    | Enter(_) | EnterWithOffset(_) | AddOps(_) => NoChange
    | Many(ms) => Many(List.map(~f=filter_read_only(m), ms))
    | _ => modification
    }
  }

// Checks to see if AST has changed, if so make requestAnalysis command.
let maybeRequestAnalysis = (oldM: model, newM: model, otherCommands: Cmd.t<msg>): Cmd.t<msg> =>
  switch (TL.selected(oldM), TL.selected(newM)) {
  | (Some(prevTL), Some(newTL)) if TL.id(prevTL) == TL.id(newTL) =>
    switch (TL.getAST(prevTL), TL.getAST(newTL)) {
    | (Some(oldAST), Some(newAST)) if oldAST != newAST =>
      let tlid = TL.id(newTL)
      Analysis.getSelectedTraceID(newM, tlid)
      |> Option.map(~f=traceID =>
        Cmd.batch(list{otherCommands, Analysis.requestAnalysis(newM, tlid, traceID)})
      )
      |> Option.unwrap(~default=otherCommands)
    | (_, _) => otherCommands
    }
  | (_, _) => otherCommands
  }

let update = (m: model, msg: msg): (model, Cmd.t<msg>) => {
  let mods = update_(msg, m) |> filter_read_only(m)
  let (newm, newc) = updateMod(mods, (m, Cmd.none))
  let newc = maybeRequestAnalysis(m, newm, newc)
  /* BEGIN HACK
   * Patch up the activeEditor to match the toplevel if
   * there is a selected toplevel. Instead, we should deprecate
   * activeEditor and modify cursorState
   * and make fluidState be per-handler */
  let activeEditor = switch (CursorState.tlidOf(newm.cursorState), newm.fluidState.activeEditor) {
  | (None, _) => NoEditor
  | (Some(tl), NoEditor) => MainEditor(tl)
  | (Some(_), ed) => ed
  }

  // END HACK
  SavedSettings.save(m)
  SavedUserSettings.save(m)
  ({...newm, lastMsg: msg, fluidState: {...newm.fluidState, activeEditor: activeEditor}}, newc)
}

let subscriptions = (m: model): Tea.Sub.t<msg> => {
  let keySubs = list{Keyboard.downs(x => GlobalKeyPress(x))}
  let dragSubs = switch m.cursorState {
  // we use IDs here because the node will change
  // before they're triggered
  | DraggingTL(id, _, _, _) =>
    let listenerKey = "mouse_moves_" ++ TLID.toString(id)
    list{BrowserListeners.DarkMouse.moves(~key=listenerKey, event => DragToplevel(id, event))}
  | PanningCanvas(_) =>
    let listenerKey = "mouse_drag"
    list{BrowserListeners.DarkMouse.moves(~key=listenerKey, event => AppMouseDrag(event))}
  | _ => list{}
  }

  let windowMouseSubs = list{
    BrowserListeners.Window.Mouse.ups(~key="win_mouse_up", event => WindowMouseUp(event)),
  }

  let timers = if m.editorSettings.runTimers {
    switch m.visibility {
    | Hidden => list{}
    | Visible => list{
        Tea.Time.every(~key="refresh_analysis", Tea.Time.second, f => TimerFire(
          RefreshAnalysis,
          f,
        )),
        Tea.Time.every(~key="refresh_avatars", Tea.Time.second, f => TimerFire(RefreshAvatars, f)),
      }
    }
  } else {
    list{}
  }

  let onError = list{
    BrowserListeners.DisplayClientError.listen(~key="display_client_error", s => JSError(s)),
  }

  let visibility = list{
    BrowserListeners.Window.OnFocusChange.listen(~key="window_on_focus_change", v =>
      if v {
        PageVisibilityChange(Visible)
      } else {
        PageVisibilityChange(Hidden)
      }
    ),
  }

  let mousewheelSubs = if m.canvasProps.enablePan && !isACOpened(m) {
    list{BrowserListeners.OnWheel.listen(~key="on_wheel", ((dx, dy)) => MouseWheel(dx, dy))}
  } else {
    list{}
  }

  let analysisSubs = list{
    Analysis.ReceiveAnalysis.listen(~key="receive_analysis", s => ReceiveAnalysis(s)),
    Analysis.NewTracePush.listen(~key="new_trace_push", s => NewTracePush(s)),
    Analysis.New404Push.listen(~key="new_404_push", s => New404Push(s)),
    DarkStorage.NewStaticDeployPush.listen(~key="new_static_deploy", s => NewStaticDeployPush(s)),
    Analysis.ReceiveFetch.listen(~key="receive_fetch", s => ReceiveFetch(s)),
    Analysis.NewPresencePush.listen(~key="new_presence_push", s => NewPresencePush(s)),
    Analysis.AddOps.listen(~key="add_op", s => AddOpsPusherMsg(s)),
    Analysis.WorkerStatePush.listen(~key="worker_state_push", s => WorkerStatePush(s)),
  }

  let clipboardSubs = // We want the default copy/paste behaviors on the settings modal
  if m.settingsView.opened || m.insertSecretModal.visible {
    list{}
  } else {
    list{
      BrowserListeners.Clipboard.copyListener(~key="copy_event", e => ClipboardCopyEvent(e)),
      BrowserListeners.Clipboard.cutListener(~key="cut_event", e => ClipboardCutEvent(e)),
      BrowserListeners.Clipboard.pasteListener(~key="paste_event", e => {
        e["preventDefault"]()
        ClipboardPasteEvent(e)
      }),
    }
  }

  Tea.Sub.batch(
    List.flatten(list{
      windowMouseSubs,
      keySubs,
      clipboardSubs,
      dragSubs,
      timers,
      visibility,
      onError,
      mousewheelSubs,
      analysisSubs,
    }),
  )
}

let debugging = {
  let prog = Tea.Debug.debug(
    show_msg,
    {
      init: a => init(a, Tea.Navigation.getLocation()),
      view: View.view,
      renderCallback: Fluid.renderCallback,
      update: update,
      subscriptions: subscriptions,
      shutdown: _ => Cmd.none,
    },
  )

  let myInit = (flag, _) => prog.init(flag)
  Tea.Navigation.navigationProgram(
    x => Tea.Debug.ClientMsg(LocationChange(x)),
    {
      init: myInit,
      update: prog.update,
      view: prog.view,
      renderCallback: prog.renderCallback,
      subscriptions: prog.subscriptions,
      shutdown: prog.shutdown,
    },
  )
}

let normal = {
  let program: Tea.Navigation.navigationProgram<string, model, msg> = {
    init: init,
    view: View.view,
    update: update,
    renderCallback: Fluid.renderCallback,
    subscriptions: subscriptions,
    shutdown: _ => Cmd.none,
  }

  Tea.Navigation.navigationProgram(x => LocationChange(x), program)
}

let main = normal
