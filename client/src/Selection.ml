open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TL = Toplevel

(* ------------------------------- *)
(* Cursors *)
(* ------------------------------- *)
let moveCursorBackInTime (m : model) (tlid : tlid) : modification =
  let traceIDs = Analysis.getTraces m tlid |> List.map ~f:Tuple2.first in
  let traceID =
    match Analysis.cursor m tlid with
    | None ->
        List.head traceIDs
    | Some current ->
        Util.listNext ~value:current traceIDs
  in
  traceID
  |> Option.map ~f:(fun t -> SetCursor (tlid, t))
  |> Option.withDefault ~default:NoChange


let moveCursorForwardInTime (m : model) (tlid : tlid) : modification =
  let traceIDs = Analysis.getTraces m tlid |> List.map ~f:Tuple2.first in
  let traceID =
    match Analysis.cursor m tlid with
    | None ->
        List.head traceIDs
    | Some current ->
        Util.listPrevious ~value:current traceIDs
  in
  traceID
  |> Option.map ~f:(fun t -> SetCursor (tlid, t))
  |> Option.withDefault ~default:NoChange


(* ------------------------------- *)
(* Toplevels *)
(* ------------------------------- *)
let selectNextToplevel (m : model) (cur : tlid option) : modification =
  let tls = TLIDDict.tlids m.toplevels in
  let next =
    cur |> Option.andThen ~f:(fun value -> Util.listNextWrap ~value tls)
  in
  match next with Some nextId -> Select (nextId, None) | None -> Deselect


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


type udDirection =
  | Up
  | Down

let moveUpDown (direction : udDirection) (sizes : htmlSizing list) (id : id) :
    id option =
  let dir = if direction = Up then -1.0 else 1.0 in
  match List.filter ~f:(fun (o : htmlSizing) -> o.id = id) sizes with
  | [this] ->
      sizes
      |> List.filter ~f:(fun o ->
             o.centerY <> this.centerY
             && dir *. this.centerY < dir *. o.centerY )
      |> List.minimumBy ~f:(fun o ->
             let majorDist = dir *. (o.centerY -. this.centerY) in
             let minorDist = abs_float (o.centerX -. this.centerX) in
             (majorDist *. 100000.0) +. minorDist )
      |> Option.withDefault ~default:this
      |> (fun x -> x.id)
      |> fun x -> Some x
  | _ ->
      None


type lrDirection =
  | Left
  | Right

let moveLeftRight (direction : lrDirection) (sizes : htmlSizing list) (id : id)
    : id option =
  (* I seem to recall some of these values seemed weird, and now I see *)
  (* that moveLeft passes Right and moveRight passes Left. Whoops. *)
  let dir = if direction = Left then -1.0 else 1.0 in
  match List.filter ~f:(fun (o : htmlSizing) -> o.id = id) sizes with
  | [this] ->
      sizes
      |> List.filter ~f:(fun o ->
             o.centerY = this.centerY && dir *. this.centerX > dir *. o.centerX
         )
      |> List.minimumBy ~f:(fun o -> dir *. (this.centerX -. o.centerX))
      |> Option.withDefault ~default:this
      |> (fun x -> x.id)
      |> fun x -> Some x
  | _ ->
      None


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
  Select (tlid, newMId)


let selectDownLevel (m : model) (tlid : tlid) (cur : id option) : modification
    =
  let tl = TL.getExn m tlid in
  let pd = Option.map ~f:(TL.findExn tl) cur in
  pd
  |> Option.orElse (TL.rootOf tl)
  |> Option.andThen ~f:(TL.firstChild tl)
  |> Option.orElse pd
  |> Option.map ~f:P.toID
  |> fun id -> Select (tlid, id)


let enterDB (m : model) (db : dB) (tl : toplevel) (id : id) : modification =
  let isLocked = DB.isLocked m tl.id in
  let isMigrationCol = DB.isMigrationCol db id in
  let pd = TL.findExn tl id in
  let enterField =
    Many
      [ Enter (Filling (tl.id, id))
      ; AutocompleteMod
          (ACSetQuery (P.toContent pd |> Option.withDefault ~default:"")) ]
  in
  match pd with
  | PDBName _ ->
      if isLocked then NoChange else enterField
  | PDBColName _ ->
      if isLocked && not isMigrationCol then NoChange else enterField
  | PDBColType _ ->
      if isLocked && not isMigrationCol then NoChange else enterField
  | PExpr _ ->
      enterField
  (* TODO validate ex.id is in either rollback or rollforward function if there's a migration in progress *)
  | _ ->
      NoChange


let enterWithOffset (m : model) (tlid : tlid) (id : id) (offset : int option) :
    modification =
  let tl = TL.getExn m tlid in
  match tl.data with
  | TLDB db ->
      enterDB m db tl id
  | _ ->
      let pd = TL.findExn tl id in
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
              (ACSetQuery (P.toContent pd |> Option.withDefault ~default:""))
          ]


let enter (m : model) (tlid : tlid) (id : id) : modification =
  enterWithOffset m tlid id None


let dblclick (m : model) (tlid : tlid) (id : id) (offset : int option) :
    modification =
  enterWithOffset m tlid id offset


let moveAndEnter
    (m : model)
    (tlid : tlid)
    (mId : id option)
    (fn : htmlSizing list -> id -> id option)
    (default : id option) : modification =
  let newMId = findTargetId tlid mId fn default in
  enter m tlid (deOption "mId" newMId)


let body (m : model) (tlid : tlid) : id option =
  let tl = TL.getExn m tlid in
  match tl.data with TLHandler h -> Some (B.toID h.ast) | _ -> None


let moveUp ?(andEnter = false) (m : model) (tlid : tlid) (mId : id option) :
    modification =
  let default = body m tlid in
  let moveFn = match andEnter with false -> move | true -> moveAndEnter in
  moveFn m tlid mId (moveUpDown Up) default


let moveDown ?(andEnter = false) (m : model) (tlid : tlid) (mId : id option) :
    modification =
  let default =
    TL.getExn m tlid |> TL.allData |> List.head |> Option.map ~f:P.toID
  in
  let moveFn = match andEnter with false -> move | true -> moveAndEnter in
  moveFn m tlid mId (moveUpDown Down) default


let moveRight ?(andEnter = false) (m : model) (tlid : tlid) (mId : id option) :
    modification =
  let default = body m tlid in
  let moveFn = match andEnter with false -> move | true -> moveAndEnter in
  moveFn m tlid mId (moveLeftRight Left) default


let moveLeft ?(andEnter = false) (m : model) (tlid : tlid) (mId : id option) :
    modification =
  let default = body m tlid in
  let moveFn = match andEnter with false -> move | true -> moveAndEnter in
  moveFn m tlid mId (moveLeftRight Right) default


(* ------------------------------- *)
(* Move AST-wide *)
(* ------------------------------- *)
let selectUpLevel (m : model) (tlid : tlid) (cur : id option) : modification =
  let tl = TL.getExn m tlid in
  let pd = Option.map ~f:(TL.findExn tl) cur in
  pd
  |> Option.andThen ~f:(TL.getParentOf tl)
  |> Option.map ~f:P.toID
  |> fun id -> Select (tlid, id)


(* ------------------------------- *)
(* Blanks *)
(* ------------------------------- *)
(* the name here is _awful_, but going to rip all of the glue
 * out soon so i pinky promise that it'll go away *)
let maybeEnterFluid
    ~(nonFluidCursorMod : modification)
    (m : model)
    (tlid : tlid)
    (oldPD : pointerData option)
    (newPD : pointerData option) : modification =
  let fluidEnteringMod =
    Many
      [ SetCursorState (FluidEntering tlid)
      ; TweakModel
          (fun m -> {m with fluidState = {m.fluidState with newPos = 0}}) ]
  in
  if VariantTesting.isFluid m.tests
  then
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
  else nonFluidCursorMod


let selectNextBlank (m : model) (tlid : tlid) (cur : id option) : modification
    =
  let tl = TL.getExn m tlid in
  let pd = Option.map ~f:(TL.findExn tl) cur in
  let nextBlankPd = pd |> TL.getNextBlank tl in
  let nextId = nextBlankPd |> Option.map ~f:P.toID in
  maybeEnterFluid
    ~nonFluidCursorMod:(Select (tlid, nextId))
    m
    tlid
    pd
    nextBlankPd


let enterNextBlank (m : model) (tlid : tlid) (cur : id option) : modification =
  let tl = TL.getExn m tlid in
  let pd = Option.map ~f:(TL.findExn tl) cur in
  let nextBlankPd = TL.getNextBlank tl pd in
  maybeEnterFluid
    ~nonFluidCursorMod:
      ( nextBlankPd
      |> Option.map ~f:(fun pd_ -> Enter (Filling (tlid, P.toID pd_)))
      |> Option.withDefault ~default:NoChange )
    m
    tlid
    pd
    nextBlankPd


let selectPrevBlank (m : model) (tlid : tlid) (cur : id option) : modification
    =
  let tl = TL.getExn m tlid in
  let pd = Option.map ~f:(TL.findExn tl) cur in
  let nextBlankPd = pd |> TL.getPrevBlank tl in
  let nextId = nextBlankPd |> Option.map ~f:P.toID in
  maybeEnterFluid
    ~nonFluidCursorMod:(Select (tlid, nextId))
    m
    tlid
    pd
    nextBlankPd


let enterPrevBlank (m : model) (tlid : tlid) (cur : id option) : modification =
  let tl = TL.getExn m tlid in
  let pd = Option.map ~f:(TL.findExn tl) cur in
  let nextBlankPd = TL.getPrevBlank tl pd in
  maybeEnterFluid
    ~nonFluidCursorMod:
      ( nextBlankPd
      |> Option.map ~f:(fun pd_ -> Enter (Filling (tlid, P.toID pd_)))
      |> Option.withDefault ~default:NoChange )
    m
    tlid
    pd
    nextBlankPd


(* ------------------------------- *)
(* misc *)
(* ------------------------------- *)
let delete (m : model) (tlid : tlid) (mId : id option) : modification =
  match mId with
  | None ->
      NoChange
  | Some id ->
      let newID = gid () in
      let focus = FocusExact (tlid, newID) in
      let tl = TL.getExn m tlid in
      let pd = TL.findExn tl id in
      ( match P.typeOf pd with
      | DBColType ->
          NoChange
      | DBColName ->
          NoChange
      | VarBind ->
          let newTL = TL.replace pd (PVarBind (F (newID, ""))) tl in
          ( match newTL.data with
          | TLHandler h ->
              RPC ([SetHandler (tlid, tl.pos, h)], focus)
          | TLFunc f ->
              RPC ([SetFunction f], focus)
          | TLTipe _ | TLDB _ ->
              impossible ("pointer type mismatch", newTL.data, pd) )
      | DBName | FnName | TypeName ->
          Many [Enter (Filling (tlid, id)); AutocompleteMod (ACSetQuery "")]
      | _ ->
          let newTL = TL.delete tl pd newID in
          ( match newTL.data with
          | TLHandler h ->
              RPC ([SetHandler (tlid, tl.pos, h)], focus)
          | TLFunc f ->
              RPC ([SetFunction f], focus)
          | TLTipe t ->
              RPC ([SetType t], focus)
          | TLDB _ ->
              impossible ("pointer type mismatch", newTL.data, pd) ) )
