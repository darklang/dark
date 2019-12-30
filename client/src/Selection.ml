open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
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


(* ------------------------------- *)
(* Move direction-wise *)
(* ------------------------------- *)

(* This is not an easy problem. *)
(* We want to move to the _thing_ above us, to the right of us, etc. *)

(* Left and right are pretty straightforward, until you hit interesting *)
(* edge cases: *)
(*   (1 |> + 2) + (4 |> + 5) *)
(* In this example, pressing right on 2 should go to 5. That pretty much *)
(* rules out a straightforward AST traversal, though we could try up and *)
(* down, it seems tricky to get right. *)
(* *)
(* Up and down are pretty complicated even in simple cases: *)
(*   L1: 4 + 5 + 6 + 7 *)
(*   L2: 485960020 + 8 *)
(* From 8, if you press up you should go to 7. How can we know? *)
(* *)
(* The most obvious implementation is to model a grid (just like a text *)
(* editor) and then use simple integer to go up/down using the column *)
(* numbers. However, this is subject to a lot of weird errors: what if *)
(* the gear or flag icons are showing for example? Or what if we change *)
(* the fonts, etc. We tried to simulate sizes before for a similar *)
(* problem, and kept getting it wrong. *)
(* *)
(* So the easiest thing to get correct -- and, importantly, to keep *)
(* correct over time -- is to use the browser to figure this out. Take *)
(* the TL, draw it, get the sizes and positions of the elements and use *)
(* them to figure out what's "above" and "below". *)

type jSSide = Native.rect =
  { id : string
  ; top : int
  ; left : int
  ; right : int
  ; bottom : int }

and htmlSizing =
  { centerX : float
  ; centerY : float
  ; top : int
  ; left : int
  ; right : int
  ; bottom : int
  ; id : id }

let jsToHtmlSizing (obj : jSSide) : htmlSizing =
  { centerX = float_of_int (obj.left + obj.right) /. 2.
  ; centerY = float_of_int (obj.top + obj.bottom) /. 2.
  ; top = obj.top
  ; left = obj.left
  ; right = obj.right
  ; bottom = obj.bottom
  ; id = ID obj.id }


let tlToSizes (tlid : tlid) : htmlSizing list * htmlSizing list =
  let poses = Native.Size.positions (deTLID tlid) in
  ( List.map ~f:jsToHtmlSizing poses.nested
  , List.map ~f:jsToHtmlSizing poses.atoms )


let findTargetId
    (tlid : tlid)
    (mId : id option)
    (fn : htmlSizing list -> id -> id option)
    (default : id option) : id option =
  let nested, atoms = tlToSizes tlid in
  mId
  |> Option.andThen ~f:(fn atoms)
  |> Option.orElse (Option.andThen ~f:(fn nested) mId)
  (* TODO: if neither, check nested+atoms. this would allow us to *)
  (* press Left on the expr of a let and go to the varbind. I think we *)
  (* would need to switch to use .left and .right instead of .centerX. *)
  |> Option.orElse mId
  |> Option.orElse default


let move
    (_ : model)
    (tlid : tlid)
    (mId : id option)
    (fn : htmlSizing list -> id -> id option)
    (default : id option) : modification =
  let newMId = findTargetId tlid mId fn default in
  match newMId with
  | Some id ->
      Select (tlid, STID id)
  | None ->
      Select (tlid, STTopLevelRoot)


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
             ( pd
             |> Option.andThen ~f:P.toContent
             |> Option.withDefault ~default:"" )) ]
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
      let pd = TL.find tl id |> AST.recoverPD "enterWithOffset" in
      if TL.getChildrenOf tl pd <> []
      then NoChange
      else
        let enterMod =
          match offset with
          | None ->
              Enter (Filling (tlid, id))
          | Some offset ->
              EnterWithOffset (Filling (tlid, id), offset)
        in
        Many
          [ enterMod
          ; AutocompleteMod
              (ACSetQuery (P.toContent pd |> Option.withDefault ~default:"")) ]
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
let maybeEnterFluid
    ~(nonFluidCursorMod : modification)
    (tlid : tlid)
    (oldPD : blankOrData option)
    (newPD : blankOrData option) : modification =
  let fluidEnteringMod =
    Many
      [ SetCursorState (FluidEntering tlid)
      ; TweakModel
          (fun m -> {m with fluidState = {m.fluidState with newPos = 0}}) ]
  in
  let isSpecHeader pd =
    match pd with
    | Some (PEventName _) | Some (PEventSpace _) | Some (PEventModifier _) ->
        true
    | _ ->
        false
  in
  match (isSpecHeader oldPD, isSpecHeader newPD) with
  (* from spec header to fluid *)
  | true, false ->
      fluidEnteringMod
  (* from fluid to specheader *)
  | false, true ->
      nonFluidCursorMod
  | _ ->
      nonFluidCursorMod


let selectNextBlank (m : model) (tlid : tlid) (cur : id option) : modification =
  match TL.get m tlid with
  | None ->
      recover "selecting no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let pd = Option.andThen ~f:(TL.find tl) cur in
      let nextBlankPd = TL.getNextBlank tl pd in
      (* TODO(JULIAN): This should probably do something better if there is no next blank -- wrap around? *)
      let nextIdTarget =
        Option.map nextBlankPd ~f:P.toID
        |> Option.map ~f:(fun id -> STID id)
        |> Option.withDefault ~default:STTopLevelRoot
      in
      maybeEnterFluid
        ~nonFluidCursorMod:(Select (tlid, nextIdTarget))
        tlid
        pd
        nextBlankPd


let enterNextBlank (m : model) (tlid : tlid) (cur : id option) : modification =
  match TL.get m tlid with
  | None ->
      recover "selecting no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let pd = Option.andThen ~f:(TL.find tl) cur in
      let nextBlankPd = TL.getNextBlank tl pd in
      maybeEnterFluid
        ~nonFluidCursorMod:
          ( nextBlankPd
          |> Option.map ~f:(fun pd_ -> Enter (Filling (tlid, P.toID pd_)))
          |> Option.withDefault ~default:NoChange )
        tlid
        pd
        nextBlankPd


let selectPrevBlank (m : model) (tlid : tlid) (cur : id option) : modification =
  match TL.get m tlid with
  | None ->
      recover "selecting no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let pd = Option.andThen ~f:(TL.find tl) cur in
      let nextBlankPd = pd |> TL.getPrevBlank tl in
      (* TODO(JULIAN): This should probably do something better if there is no next blank -- wrap around? *)
      let nextIdTarget =
        Option.map nextBlankPd ~f:P.toID
        |> Option.map ~f:(fun id -> STID id)
        |> Option.withDefault ~default:STTopLevelRoot
      in
      maybeEnterFluid
        ~nonFluidCursorMod:(Select (tlid, nextIdTarget))
        tlid
        pd
        nextBlankPd


let enterPrevBlank (m : model) (tlid : tlid) (cur : id option) : modification =
  match TL.get m tlid with
  | None ->
      recover "selecting no TL" ~debug:(tlid, cur) NoChange
  | Some tl ->
      let pd = Option.andThen ~f:(TL.find tl) cur in
      let nextBlankPd = TL.getPrevBlank tl pd in
      maybeEnterFluid
        ~nonFluidCursorMod:
          ( nextBlankPd
          |> Option.map ~f:(fun pd_ -> Enter (Filling (tlid, P.toID pd_)))
          |> Option.withDefault ~default:NoChange )
        tlid
        pd
        nextBlankPd
