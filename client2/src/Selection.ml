module B = Blank
module P = Pointer
open Prelude
module TL = Toplevel
open Types

let moveCursorBackInTime m tlid =
  let maxCursor = List.length (Analysis.getTraces m tlid) - 1 in
  let current = Analysis.cursor m tlid in
  let newCursor = max 0 (min (current + 1) maxCursor) in
  SetCursor (tlid, newCursor)

let moveCursorForwardInTime m tlid =
  let maxCursor = List.length (Analysis.getTraces m tlid) - 1 in
  let current = Analysis.cursor m tlid in
  let newCursor = max 0 (min (current - 1) maxCursor) in
  SetCursor (tlid, newCursor)

let selectNextToplevel m cur =
  let tls = List.map (fun x -> x.id) m.toplevels in
  let next = cur |> Maybe.andThen (fun cur_ -> Util.listNextWrap cur_ tls) in
  match next with
  | Just nextId -> Select (nextId, Nothing)
  | Nothing -> Deselect

let selectPrevToplevel m cur =
  let tls = List.map (fun x -> x.id) m.toplevels in
  let next =
    cur |> Maybe.andThen (fun cur_ -> Util.listPreviousWrap cur_ tls)
  in
  match next with
  | Just nextId -> Select (nextId, Nothing)
  | Nothing -> Deselect

type jSSide =
  { x: float
  ; y: float
  ; width: float
  ; height: float
  ; top: float
  ; right: float
  ; bottom: float
  ; left: float
  ; id: int }

type htmlSizing = {centerX: float; centerY: float; id: iD}

let jsToHtmlSizing obj =
  { centerX= (obj.left + obj.right) / 2
  ; centerY= (obj.top + obj.bottom) / 2
  ; id= ID obj.id }

let tlToSizes m tlid =
  let poses = Native.Size.positions (deTLID tlid) in
  (List.map jsToHtmlSizing poses.nested, List.map jsToHtmlSizing poses.atoms)

type uDDirection = Up | Down

let moveUpDown direction sizes id =
  let dir = if direction == Up then -1 else 1 in
  match List.filter (fun o -> o.id == id) sizes with
  | [this] ->
      sizes
      |> List.filter (fun o ->
             (((o.centerY /= this.centerY) && dir) * this.centerY < dir)
             * o.centerY )
      |> LE.minimumBy (fun o ->
             let majorDist = dir * (o.centerY - this.centerY) in
             let minorDist = abs (o.centerX - this.centerX) in
             (majorDist * 100000) + minorDist )
      |> Maybe.withDefault this
      |> (fun x -> x.id)
      |> Just
  | _ -> Nothing

type lRDirection = Left | Right

let moveLeftRight direction sizes id =
  let dir = if direction == Left then -1 else 1 in
  match List.filter (fun o -> o.id == id) sizes with
  | [this] ->
      sizes
      |> List.filter (fun o ->
             ((o.centerY == this.centerY && dir) * this.centerX > dir)
             * o.centerX )
      |> LE.minimumBy (fun o -> dir * (this.centerX - o.centerX))
      |> Maybe.withDefault this
      |> (fun x -> x.id)
      |> Just
  | _ -> Nothing

let move m tlid mId fn default =
  let nested, atoms = tlToSizes m tlid in
  Maybe.andThen (fn atoms) mId
  |> Port.optionOrElse (Maybe.andThen (fn nested) mId)
  |> Port.optionOrElse mId |> Port.optionOrElse default |> Select tlid

let body m tlid =
  let tl = TL.getTL m tlid in
  match tl.data with TLHandler h -> Just (B.toID h.ast) | _ -> Nothing

let moveUp m tlid mId =
  let default = body m tlid in
  move m tlid mId (moveUpDown Up) default

let moveDown m tlid mId =
  let default =
    TL.getTL m tlid |> TL.allData |> List.head |> Maybe.map P.toID
  in
  move m tlid mId (moveUpDown Down) default

let moveRight m tlid mId =
  let default = body m tlid in
  move m tlid mId (moveLeftRight Left) default

let moveLeft m tlid mId =
  let default = body m tlid in
  move m tlid mId (moveLeftRight Right) default

let selectUpLevel m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd |> Maybe.andThen (TL.getParentOf tl) |> Maybe.map P.toID |> Select tlid

let selectDownLevel m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd
  |> Port.optionOrElse (TL.rootOf tl)
  |> Maybe.andThen (TL.firstChild tl)
  |> Port.optionOrElse pd |> Maybe.map P.toID |> Select tlid

let selectNextSibling m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd
  |> Maybe.map (TL.getNextSibling tl)
  |> Port.optionOrElse pd |> Maybe.map P.toID |> Select tlid

let selectPreviousSibling m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd
  |> Maybe.map (TL.getPrevSibling tl)
  |> Port.optionOrElse pd |> Maybe.map P.toID |> Select tlid

let selectNextBlank m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd |> TL.getNextBlank tl |> Maybe.map P.toID |> Select tlid

let enterNextBlank m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd |> TL.getNextBlank tl
  |> Maybe.map (fun pd_ -> Enter (Filling (tlid, P.toID pd_)))
  |> Maybe.withDefault NoChange

let selectPrevBlank m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd |> TL.getPrevBlank tl |> Maybe.map P.toID |> Select tlid

let enterPrevBlank m tlid cur =
  let tl = TL.getTL m tlid in
  let pd = Maybe.map (TL.findExn tl) cur in
  pd |> TL.getPrevBlank tl
  |> Maybe.map (fun pd_ -> Enter (Filling (tlid, P.toID pd_)))
  |> Maybe.withDefault NoChange

let delete m tlid mId =
  match mId with
  | Nothing -> (
      let tl = TL.getTL m tlid in
      match tl.data with
      | TLHandler h ->
          if isLocked tlid m then NoChange
          else
            Many
              [RemoveToplevel tl; RPC ([DeleteTL tlid], FocusNothing); Deselect]
      | TLDB _ ->
          Many
            [RemoveToplevel tl; RPC ([DeleteTL tlid], FocusNothing); Deselect]
      | TLFunc _ -> DisplayError "Cannot delete functions!" )
  | Just id -> (
      let newID = gid () in
      let focus = FocusExact (tlid, newID) in
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      match P.typeOf pd with
      | DBColType -> NoChange
      | DBColName -> NoChange
      | VarBind -> (
          let newTL = TL.replace pd (PVarBind (F (newID, ""))) tl in
          match newTL.data with
          | TLHandler h -> RPC ([SetHandler (tlid, tl.pos, h)], focus)
          | TLFunc f -> RPC ([SetFunction f], focus)
          | TLDB _ -> impossible ("pointer type mismatch", newTL.data, pd) )
      | FnName ->
          Many [Enter (Filling (tlid, id)); AutocompleteMod (ACSetQuery "")]
      | _ -> (
          let newTL = TL.delete tl pd newID in
          match newTL.data with
          | TLHandler h -> RPC ([SetHandler (tlid, tl.pos, h)], focus)
          | TLFunc f -> RPC ([SetFunction f], focus)
          | TLDB _ -> impossible ("pointer type mismatch", newTL.data, pd) ) )

let enterDB m db tl id =
  let isLocked = DB.isLocked m tl.id in
  let isMigrationCol = DB.isMigrationCol db id in
  let pd = TL.findExn tl id in
  let enterField autocomplete =
    if autocomplete then
      Many
        [ Enter (Filling (tl.id, id))
        ; AutocompleteMod (ACSetQuery (P.toContent pd |> Maybe.withDefault ""))
        ]
    else Enter (Filling (tl.id, id))
  in
  let _ = pd in
  match pd with
  | PDBColName d ->
      if isLocked && not isMigrationCol then NoChange else enterField false
  | PDBColType d ->
      if isLocked && not isMigrationCol then NoChange else enterField true
  | PExpr ex -> enterField true
  | _ -> NoChange

let enter m tlid id =
  let tl = TL.getTL m tlid in
  match tl.data with
  | TLDB db -> enterDB m db tl id
  | _ ->
      let pd = TL.findExn tl id in
      if TL.getChildrenOf tl pd /= [] then selectDownLevel m tlid (Just id)
      else
        Many
          [ Enter (Filling (tlid, id))
          ; AutocompleteMod
              (ACSetQuery (P.toContent pd |> Maybe.withDefault "")) ]
