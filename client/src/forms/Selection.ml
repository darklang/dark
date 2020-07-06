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
let moveToOlderTrace (m : model) (tlid : TLID.t) : modification =
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


let moveToNewerTrace (m : model) (tlid : TLID.t) : modification =
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
(* Entering *)
(* ------------------------------- *)

let enterDB (m : model) (db : db) (tl : toplevel) (id : ID.t) : modification =
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


let enterWithOffset
    (m : model) (tlid : TLID.t) (id : ID.t) (offset : int option) : modification
    =
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


let enter (m : model) (tlid : TLID.t) (id : ID.t) : modification =
  enterWithOffset m tlid id None


let dblclick (m : model) (tlid : TLID.t) (id : ID.t) (offset : int option) :
    modification =
  enterWithOffset m tlid id offset


(* ------------------------------- *)
(* Blanks *)
(* ------------------------------- *)
(* the name here is _awful_, but going to rip all of the glue
 * out soon so i pinky promise that it'll go away *)
let fluidEnteringMod tlid =
  ReplaceAllModificationsWithThisOne
    (fun m ->
      {m with fluidState = {m.fluidState with newPos = 0}}
      |> CursorState.setCursorState (FluidEntering tlid))


let maybeEnterFluid
    ~(nonFluidCursorMod : modification) (tl : toplevel) (newPD : ID.t option) :
    modification =
  let tlid = TL.id tl in
  match newPD with
  | None ->
      fluidEnteringMod tlid
  | Some id ->
      if TL.isValidBlankOrID tl id
      then nonFluidCursorMod
      else fluidEnteringMod tlid


let enterNextEditable (m : model) (tlid : TLID.t) (cur : ID.t) : modification =
  match TL.get m tlid with
  | None ->
      recover "entering no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let nextEditable = TL.getNextEditable tl cur in
      let target =
        nextEditable
        |> Option.map ~f:(fun id -> Enter (Filling (tlid, id)))
        |> Option.withDefault ~default:(fluidEnteringMod tlid)
      in
      maybeEnterFluid ~nonFluidCursorMod:target tl nextEditable


let enterPrevEditable (m : model) (tlid : TLID.t) (cur : ID.t) : modification =
  match TL.get m tlid with
  | None ->
      recover "entering no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let prevEditable = TL.getPrevEditable tl cur in
      let target =
        prevEditable
        |> Option.map ~f:(fun id -> Enter (Filling (tlid, id)))
        |> Option.withDefault ~default:(fluidEnteringMod tlid)
      in
      maybeEnterFluid ~nonFluidCursorMod:target tl prevEditable
