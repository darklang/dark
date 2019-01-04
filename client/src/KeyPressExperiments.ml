open Types
open Prelude
open Porting

(* Dark *)
module Key = Keyboard
module TL = Toplevel
module P = Pointer

(* only needed for moving up/down, not left/right *)
(* NB: assumes no id collisions in a given canvas *)
let offsetFromCurrent (newId : id) : int =
  let oldCaretX = Native.Ext.findCaretXPos () in
  Native.Ext.findLogicalOffset (showID newId) oldCaretX


let moveDown (xPos : int) (sizes : Selection.htmlSizing list) (id : id) :
    id option =
  match List.filter (fun (o : Selection.htmlSizing) -> o.id = id) sizes with
  | [this] ->
      sizes
      |> List.filter (fun (o : Selection.htmlSizing) ->
             xPos >= o.left - 8
             && xPos <= o.right + 8
             (* within 1 char of *)
             && this.centerY < o.centerY
             (* below only, inverted y axis *) )
      |> List.minimumBy (fun (o : Selection.htmlSizing) ->
             (* nearest line *)
             (int_of_float o.centerY * 10000)
             (* if there are multiple options, pick the one nearest *)
             + max (o.left - xPos) (xPos - o.right) )
      |> Option.withDefault this
      |> (fun x -> x.id)
      |> fun x -> Some x
  | _ ->
      None


let moveUp (xPos : int) (sizes : Selection.htmlSizing list) (id : id) :
    id option =
  match List.filter (fun (o : Selection.htmlSizing) -> o.id = id) sizes with
  | [this] ->
      sizes
      |> List.filter (fun (o : Selection.htmlSizing) ->
             xPos >= o.left - 8
             && xPos <= o.right + 8
             (* within 1 char of *)
             && this.centerY > o.centerY
             (* above only, inverted y axis *) )
      |> List.maximumBy (fun (o : Selection.htmlSizing) ->
             (* nearest line *)
             (int_of_float o.centerY * 10000)
             (* if there are multiple options, pick the one nearest *)
             - max (o.left - xPos) (xPos - o.right) )
      |> Option.withDefault this
      |> (fun x -> x.id)
      |> fun x -> Some x
  | _ ->
      None


(* TODO this whole arrowMove* section could be DRY'd up, post-experiment *)
let arrowMoveUp (m : model) (tlid : tlid) (mId : id option) : modification =
  let default = Selection.body m tlid in
  let xPos = Native.Ext.findCaretXPos () in
  let newMId =
    Selection.findTargetId tlid mId (moveUp xPos) default |> deOption "mId"
  in
  Selection.enterWithOffset m tlid newMId (Some (offsetFromCurrent newMId))


let arrowMoveDown (m : model) (tlid : tlid) (mId : id option) : modification =
  let default =
    TL.getTL m tlid |> TL.allData |> List.head |> Option.map P.toID
  in
  let xPos = Native.Ext.findCaretXPos () in
  let newMId =
    Selection.findTargetId tlid mId (moveDown xPos) default |> deOption "mId"
  in
  Selection.enterWithOffset m tlid newMId (Some (offsetFromCurrent newMId))


let moveLeft (sizes : Selection.htmlSizing list) (id : id) : id option =
  match List.filter (fun (s : Selection.htmlSizing) -> s.id = id) sizes with
  | [this] ->
      sizes
      |> List.filter (fun (s : Selection.htmlSizing) ->
             s.centerY = this.centerY && s.right <= this.left )
      (* Somewhat counterintuitively, we don't want the expression that ends
      closest to this one, as some expressions wrap other ones. For example,
      with x.y we want to get y, but the "." expression wraps it. So we actually
      want the blank that begins nearest to us. *)
      |> List.minimumBy (fun (s : Selection.htmlSizing) -> this.left - s.left)
      |> Option.withDefault this
      |> (fun x -> x.id)
      |> fun x -> Some x
  | _ ->
      None


let moveRight (sizes : Selection.htmlSizing list) (id : id) : id option =
  match List.filter (fun (s : Selection.htmlSizing) -> s.id = id) sizes with
  | [this] ->
      sizes
      |> List.filter (fun (s : Selection.htmlSizing) ->
             s.centerY = this.centerY && this.right - 8 <= s.left )
      (* Somewhat counterintuitively, we don't want the expression that starts
      closest to this one, as some expressions wrap other ones. For example,
      with x.y we want to get x, but the "." expression wraps it. So we actually
      want the blank that ends nearest to us. *)
      |> List.minimumBy (fun (s : Selection.htmlSizing) -> s.left - this.left)
      |> Option.withDefault this
      |> (fun x -> x.id)
      |> fun x -> Some x
  | _ ->
      None


let arrowMoveLeft (m : model) (tlid : tlid) (mId : id option) : modification =
  (* true means we handled it in js; false means we're moving between nodes *)
  match Native.Ext.moveCaretLeft () with
  | true ->
      NoChange
  | false ->
      let default = Selection.body m tlid in
      let newMId = Selection.findTargetId tlid mId moveLeft default in
      if newMId = mId
      then NoChange
      else Selection.enterWithOffset m tlid (deOption "mId" newMId) (Some (-1))


let arrowMoveRight (m : model) (tlid : tlid) (mId : id option) : modification =
  (* true means we handled it in js; false means we're moving between nodes *)
  match Native.Ext.moveCaretRight () with
  | true ->
      NoChange
  | false ->
      let default = Selection.body m tlid in
      let newMId = Selection.findTargetId tlid mId moveRight default in
      if newMId = mId
      then NoChange
      else Selection.enterWithOffset m tlid (deOption "mId" newMId) (Some 0)


let arrowMoveHandler (event : Keyboard.keyEvent) (m : model) :
    modification option =
  match VariantTesting.variantIsActive m FluidInputModel with
  | false ->
      None
  | true ->
    ( match m.cursorState with
    | Entering (Filling (tlid, id)) ->
      ( match event.keyCode with
      | Key.Up ->
          if m.complete.visible
          then Some (AutocompleteMod ACSelectUp)
          else Some (arrowMoveUp m tlid (Some id))
      | Key.Down ->
          if m.complete.visible
          then Some (AutocompleteMod ACSelectDown)
          else Some (arrowMoveDown m tlid (Some id))
      | Key.Right ->
          if event.metaKey && event.shiftKey
          then Some (Select (tlid, Some id))
          else Some (arrowMoveRight m tlid (Some id))
      | Key.Left ->
          if event.metaKey && event.shiftKey
          then Some (Select (tlid, Some id))
          else Some (arrowMoveLeft m tlid (Some id))
      | Key.Escape ->
          Some NoChange
      | _ ->
          None )
    | Selecting (tlid, Some id) ->
      ( match event.keyCode with
      | Key.Escape ->
          Some (Enter (Filling (tlid, id)))
      | _ ->
          None )
    | _ ->
        None )
