open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TL = Toplevel

type modification = AppTypes.modification
type model = AppTypes.model
module Mod = AppTypes.Modification

// hmmm: a 'selection' is...

// -------------------------------
// Traces
// These used to have keyboard shortcuts to move between traces. When we
// reintroduce shortcuts, it would likely be nice to have them again.
// -------------------------------
let moveToOlderTrace = (m: AppTypes.model, tlid: TLID.t): modification => {
  let traceIDs = Analysis.getTraces(m, tlid) |> List.map(~f=Tuple2.first)
  let traceID = switch Analysis.getSelectedTraceID(m, tlid) {
  | None => List.head(traceIDs)
  | Some(current) => Util.listNext(~value=current, traceIDs)
  }

  traceID |> Option.map(~f=t => Mod.SetTLTraceID(tlid, t)) |> Option.unwrap(~default=Mod.NoChange)
}

let moveToNewerTrace = (m: AppTypes.model, tlid: TLID.t): modification => {
  let traceIDs = Analysis.getTraces(m, tlid) |> List.map(~f=Tuple2.first)
  let traceID = switch Analysis.getSelectedTraceID(m, tlid) {
  | None => List.head(traceIDs)
  | Some(current) => Util.listPrevious(~value=current, traceIDs)
  }

  traceID |> Option.map(~f=t => Mod.SetTLTraceID(tlid, t)) |> Option.unwrap(~default=Mod.NoChange)
}

// -------------------------------
// Entering
// -------------------------------

let enterDB = (m: AppTypes.model, tl: toplevel, id: id): modification => {
  let tlid = TL.id(tl)
  let isLocked = DB.isLocked(m, tlid)
  let pd = TL.find(tl, id)
  let enterField = Mod.Many(list{
    Enter(tlid, id),
    AutocompleteMod(ACSetQuery(pd |> Option.map(~f=P.toContent) |> Option.unwrap(~default=""))),
  })

  switch pd {
  | Some(PDBName(_)) =>
    if isLocked {
      NoChange
    } else {
      enterField
    }
  | Some(PDBColName(_)) =>
    if isLocked {
      NoChange
    } else {
      enterField
    }
  | Some(PDBColType(_)) =>
    if isLocked {
      NoChange
    } else {
      enterField
    }
  | _ => NoChange
  }
}

let enterWithOffset = (
  m: AppTypes.model,
  tlid: TLID.t,
  id: id,
  offset: option<int>,
): modification =>
  switch TL.get(m, tlid) {
  | Some(TLDB(_) as tl) => enterDB(m, tl, id)
  | Some(tl) =>
    switch TL.find(tl, id) {
    | Some(pd) =>
      let enterMod = switch offset {
      | None => Mod.Enter(tlid, id)
      | Some(offset) => Mod.EnterWithOffset(tlid, id, offset)
      }

      Many(list{enterMod, AutocompleteMod(ACSetQuery(P.toContent(pd)))})
    | None => recover("id not found in enterWithOffset", ~debug=(tlid, id), Mod.NoChange)
    }
  | _ => recover("Entering invalid tl", ~debug=(tlid, id), Mod.NoChange)
  }

let enter = (m: model, tlid: TLID.t, id: id): modification => enterWithOffset(m, tlid, id, None)

let dblclick = (m: model, tlid: TLID.t, id: id, offset: option<int>): modification =>
  enterWithOffset(m, tlid, id, offset)

// -------------------------------
// Blanks
// -------------------------------
/* the name here is _awful_, but going to rip all of the glue
 * out soon so i pinky promise that it'll go away */
let fluidEnteringMod = tlid => Mod.ReplaceAllModificationsWithThisOne(
  (m: model) =>
    {...m, fluidState: {...m.fluidState, newPos: 0}} |> CursorState.setCursorState(
      FluidEntering(tlid),
    ),
)

let maybeEnterFluid = (
  ~nonFluidCursorMod: modification,
  tl: toplevel,
  newPD: option<id>,
): modification => {
  let tlid = TL.id(tl)
  switch newPD {
  | None => fluidEnteringMod(tlid)
  | Some(id) =>
    if TL.isValidBlankOrID(tl, id) {
      nonFluidCursorMod
    } else {
      fluidEnteringMod(tlid)
    }
  }
}

let enterNextBlank = (m: model, tlid: TLID.t, cur: id): modification =>
  switch TL.get(m, tlid) {
  | None => recover("entering no TL", ~debug=(tlid, cur), Mod.NoChange)
  | Some(tl) =>
    let nextBlank = TL.getNextBlank(tl, cur)
    let target =
      nextBlank
      |> Option.map(~f=id => Mod.Enter(tlid, id))
      |> Option.unwrap(~default=fluidEnteringMod(tlid))

    maybeEnterFluid(~nonFluidCursorMod=target, tl, nextBlank)
  }

let enterPrevBlank = (m: model, tlid: TLID.t, cur: id): modification =>
  switch TL.get(m, tlid) {
  | None => recover("entering no TL", ~debug=(tlid, cur), Mod.NoChange)
  | Some(tl) =>
    let prevBlank = TL.getPrevBlank(tl, cur)
    let target =
      prevBlank
      |> Option.map(~f=id => Mod.Enter(tlid, id))
      |> Option.unwrap(~default=fluidEnteringMod(tlid))

    maybeEnterFluid(~nonFluidCursorMod=target, tl, prevBlank)
  }
