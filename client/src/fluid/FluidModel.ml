(* FluidModel is for helper functions that operate on the model. It is meant
 * to have fewer dependencies than Fluid.ml, so don't include anything like msg
 * or modification in any of these. If your function doesn't return just a
 * model it probably doesn't belong here. *)
module Option = Tc.Option
open Types

let loadTL (tlid : TLID.t) (m : model) : model =
  let m = {m with cursorState = FluidEntering tlid} in
  Toplevel.get m tlid
  |> Option.andThen ~f:Toplevel.getAST
  |> Option.map ~f:(fun ast ->
         let panels = FluidPanel.Group.init tlid ast in
         let ac = FluidAutocomplete.reset m in
         {m with fluidState = {m.fluidState with panels; ac}})
  |> Option.withDefault ~default:m


let toggleFlagPanel
    (tlid : TLID.t) (expressionId : ID.t) (isOpen : bool) (m : model) : model =
  let m = loadTL tlid m in
  let panels =
    FluidPanel.Group.setOpen isOpen expressionId m.fluidState.panels
  in
  {m with fluidState = {m.fluidState with panels}}


let focusPanel (panelId : ID.t) (m : model) : model =
  {m with fluidState = {m.fluidState with activePanelId = Some panelId}}


let focusMainEditor (m : model) : model =
  {m with fluidState = {m.fluidState with activePanelId = None}}
