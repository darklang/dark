open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TL = Toplevel

(* ------------------------------- *)
(* Traces *)
(* These used to have keyboard shortcuts to move between traces. When we
 * reintroduce shortcuts, it would likely be nice to have them again. *)
(* ------------------------------- *)
let moveToOlderTrace (m : model) (tlid : tlid) : modification =
  let traceIDs = Analysis.getTraces m tlid |> List.map ~f:Tuple2.first in
  let traceID =
    match Analysis.getSelectedTraceID m tlid with
    | None ->
        List.head traceIDs
    | Some current ->
        Util.listNext ~value:current traceIDs
  in
  traceID
  |> Option.map ~f:(fun t -> SetTLTraceID (tlid, t))
  |> Option.withDefault ~default:NoChange


let moveToNewerTrace (m : model) (tlid : tlid) : modification =
  let traceIDs = Analysis.getTraces m tlid |> List.map ~f:Tuple2.first in
  let traceID =
    match Analysis.getSelectedTraceID m tlid with
    | None ->
        List.head traceIDs
    | Some current ->
        Util.listPrevious ~value:current traceIDs
  in
  traceID
  |> Option.map ~f:(fun t -> SetTLTraceID (tlid, t))
  |> Option.withDefault ~default:NoChange


(* ------------------------------- *)
(* Toplevels *)
(* ------------------------------- *)
let selectNextToplevel (m : model) (cur : tlid option) : modification =
  let tls = TLIDDict.tlids (TL.structural m) in
  let next =
    cur |> Option.andThen ~f:(fun value -> Util.listNextWrap ~value tls)
  in
  match next with
  | Some nextId ->
      Select (nextId, STTopLevelRoot)
  | None ->
      Deselect


let enterDB (m : model) (db : db) (tl : toplevel) (id : id) : modification =
  let tlid = TL.id tl in
  let isLocked = DB.isLocked m tlid in
  let isMigrationCol = DB.isMigrationCol db id in
  let pd = TL.find tl id in
  let enterField =
    Many
      [ Enter (Filling (tlid, id))
      ; AutocompleteMod
          (ACSetQuery
             (pd |> Option.map ~f:P.toContent |> Option.withDefault ~default:""))
      ]
  in
  match pd with
  | Some (PDBName _) ->
      if isLocked then NoChange else enterField
  | Some (PDBColName _) ->
      if isLocked && not isMigrationCol then NoChange else enterField
  | Some (PDBColType _) ->
      if isLocked && not isMigrationCol then NoChange else enterField
  (* TODO validate ex.id is in either rollback or rollforward function if there's a migration in progress *)
  | _ ->
      NoChange


let enterWithOffset (m : model) (tlid : tlid) (id : id) (offset : int option) :
    modification =
  match TL.get m tlid with
  | Some (TLDB db as tl) ->
      enterDB m db tl id
  | Some tl ->
    ( match TL.find tl id with
    | Some pd ->
        let enterMod =
          match offset with
          | None ->
              Enter (Filling (tlid, id))
          | Some offset ->
              EnterWithOffset (Filling (tlid, id), offset)
        in
        Many [enterMod; AutocompleteMod (ACSetQuery (P.toContent pd))]
    | None ->
        recover "id not found in enterWithOffset" ~debug:(tlid, id) NoChange )
  | _ ->
      recover "Entering invalid tl" ~debug:(tlid, id) NoChange


let enter (m : model) (tlid : tlid) (id : id) : modification =
  enterWithOffset m tlid id None


let dblclick (m : model) (tlid : tlid) (id : id) (offset : int option) :
    modification =
  enterWithOffset m tlid id offset


(* ------------------------------- *)
(* Blanks *)
(* ------------------------------- *)
(* the name here is _awful_, but going to rip all of the glue
 * out soon so i pinky promise that it'll go away *)
let fluidEnteringMod tlid =
  Many
    [ SetCursorState (FluidEntering tlid)
    ; TweakModel (fun m -> {m with fluidState = {m.fluidState with newPos = 0}})
    ]


let maybeEnterFluid
    ~(nonFluidCursorMod : modification) (tl : toplevel) (newPD : id option) :
    modification =
  let tlid = TL.id tl in
  match newPD with
  | None ->
      fluidEnteringMod tlid
  | Some id ->
      if TL.isValidBlankOrID tl id
      then nonFluidCursorMod
      else fluidEnteringMod tlid


let selectNextBlank (m : model) (tlid : tlid) (cur : id) : modification =
  match TL.get m tlid with
  | None ->
      recover "selecting no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let nextBlank = TL.getNextBlank tl cur in
      let target =
        nextBlank
        |> Option.map ~f:(fun id -> Select (tlid, STID id))
        |> Option.withDefault ~default:(fluidEnteringMod tlid)
      in
      maybeEnterFluid ~nonFluidCursorMod:target tl nextBlank


let enterNextBlank (m : model) (tlid : tlid) (cur : id) : modification =
  match TL.get m tlid with
  | None ->
      recover "entering no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let nextBlank = TL.getNextBlank tl cur in
      let target =
        nextBlank
        |> Option.map ~f:(fun id -> Enter (Filling (tlid, id)))
        |> Option.withDefault ~default:(fluidEnteringMod tlid)
      in
      maybeEnterFluid ~nonFluidCursorMod:target tl nextBlank


let selectPrevBlank (m : model) (tlid : tlid) (cur : id) : modification =
  match TL.get m tlid with
  | None ->
      recover "selecting no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let prevBlank = TL.getPrevBlank tl cur in
      let target =
        prevBlank
        |> Option.map ~f:(fun id -> Select (tlid, STID id))
        |> Option.withDefault ~default:(fluidEnteringMod tlid)
      in
      maybeEnterFluid ~nonFluidCursorMod:target tl prevBlank


let enterPrevBlank (m : model) (tlid : tlid) (cur : id) : modification =
  match TL.get m tlid with
  | None ->
      recover "entering no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let prevBlank = TL.getPrevBlank tl cur in
      let target =
        prevBlank
        |> Option.map ~f:(fun id -> Enter (Filling (tlid, id)))
        |> Option.withDefault ~default:(fluidEnteringMod tlid)
      in
      maybeEnterFluid ~nonFluidCursorMod:target tl prevBlank
