open Tc
open Types

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard

let filterInputID : string = "cmd-filter"

let reset : fluidCommandState =
  { index = 0
  ; show = false
  ; commands = Commands.commands
  ; cmdOnTL = None
  ; cmdOnID = None
  ; filter = None }


let commandsFor (tl : toplevel) (id : id) : command list =
  let filterForRail rail =
    Commands.commands
    |> List.filter ~f:(fun c ->
           if rail = Rail
           then c.commandName <> Commands.putFunctionOnRail
           else if rail = NoRail
           then c.commandName <> Commands.takeFunctionOffRail
           else true )
  in
  Toplevel.getAST tl
  |> Option.andThen ~f:(fun x -> AST.find id x)
  |> Option.andThen ~f:(fun pd ->
         match pd with
         | PExpr (F (_, FnCall (_, _, rail))) ->
             Some (filterForRail rail)
         | _ ->
             None )
  |> Option.withDefault ~default:Commands.commands


let updateCommandState (tl : toplevel) (id : id) : fluidCommandState =
  { index = 0
  ; show = true
  ; commands = commandsFor tl id
  ; cmdOnTL = Some tl
  ; cmdOnID = Some id
  ; filter = None }


let executeCommand (m : model) (tl : toplevel) (id : id) (cmd : command) :
    modification =
  let pd = Toplevel.findExn tl id in
  cmd.action m tl pd


let runCommand (m : model) (cmd : command) : modification =
  let cp = m.fluidState.cp in
  match (cp.cmdOnTL, cp.cmdOnID) with
  | Some tl, Some id ->
      executeCommand m tl id cmd
  | _ ->
      NoChange


let highlighted (s : fluidCommandState) : command option =
  List.getAt ~index:s.index s.commands


let asName (cmd : command) : string = cmd.commandName

let moveUp (s : fluidCommandState) : fluidCommandState =
  let i = s.index - 1 in
  {s with index = (if i < 0 then 0 else i)}


let moveDown (s : fluidCommandState) : fluidCommandState =
  let i = s.index + 1 in
  let max = List.length s.commands in
  {s with index = (if i >= max then max - 1 else i)}


let focusItem (i : int) : msg Tea.Cmd.t =
  Tea_task.attempt
    (fun _ -> IgnoreMsg)
    (Tea_task.nativeBinding (fun _ ->
         let open Webapi.Dom in
         let open Native.Ext in
         let container = Document.getElementById "fluid-dropdown" document in
         let nthChild =
           querySelector
             ("#fluid-dropdown ul li:nth-child(" ^ string_of_int (i + 1) ^ ")")
         in
         match (container, nthChild) with
         | Some el, Some li ->
             let cRect = getBoundingClientRect el in
             let cBottom = rectBottom cRect in
             let cTop = rectTop cRect in
             let liRect = getBoundingClientRect li in
             let liBottom = rectBottom liRect in
             let liTop = rectTop liRect in
             let liHeight = rectHeight liRect in
             if liBottom +. liHeight > cBottom
             then
               let offset = float_of_int (offsetTop li) in
               let padding = rectHeight cRect -. (liHeight *. 2.0) in
               Element.setScrollTop el (offset -. padding)
             else if liTop -. liHeight < cTop
             then
               let offset = float_of_int (offsetTop li) in
               Element.setScrollTop el (offset -. liHeight)
             else ()
         | _, _ ->
             () ))


let filter (query : string) (s : fluidCommandState) : fluidCommandState =
  let allCmds =
    match (s.cmdOnTL, s.cmdOnID) with
    | Some tl, Some id ->
        commandsFor tl id
    | _ ->
        Commands.commands
  in
  let filter, commands =
    if String.length query > 0
    then
      let isMatched c = String.contains ~substring:query c.commandName in
      (Some query, List.filter ~f:isMatched allCmds)
    else (None, Commands.commands)
  in
  {s with filter; commands; index = 0}


let isOpenOnTL (s : fluidCommandState) (tlid : tlid) : bool =
  if s.show
  then match s.cmdOnTL with Some tl -> tl.id = tlid | None -> false
  else false


let viewCommandPalette (cp : Types.fluidCommandState) : Types.msg Html.html =
  let viewCommands i item =
    let highlighted = cp.index = i in
    let name = asName item in
    Html.li
      [ Attrs.classList
          [ ("autocomplete-item", true)
          ; ("fluid-selected", highlighted)
          ; ("valid", true) ]
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; ViewEntry.defaultPasteHandler
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.eventNoPropagation ~key:("cp-" ^ name) "click" (fun _ ->
            FluidRunCommand item ) ]
      [Html.text name]
  in
  let filterInput =
    Html.input'
      [ Attrs.id filterInputID
      ; Attrs.spellcheck false
      ; Attrs.autocomplete false
      ; Events.onInput (fun query -> FluidCommandsFilter query) ]
      []
  in
  let cmdsView =
    Html.div
      [Attrs.id "fluid-dropdown"]
      [Html.ul [] (List.indexedMap ~f:viewCommands cp.commands)]
  in
  Html.div [Html.class' "command-palette"] [filterInput; cmdsView]


let updateCmds (m : Types.model) (keyEvt : K.keyEvent) : Types.modification =
  let s = m.fluidState in
  let key = keyEvt.key in
  match key with
  | K.Enter ->
      let cp = s.cp in
      ( match (cp.cmdOnTL, cp.cmdOnID) with
      | Some tl, Some id ->
        ( match highlighted cp with
        | Some cmd ->
            Many [executeCommand m tl id cmd; FluidCommandsClose]
        | None ->
            NoChange )
      | _ ->
          NoChange )
  | K.Up ->
      let cp = moveUp s.cp in
      let cmd = Types.MakeCmd (focusItem cp.index) in
      let m = Types.TweakModel (fun m -> {m with fluidState = {s with cp}}) in
      Types.Many [m; cmd]
  | K.Down ->
      let cp = moveDown s.cp in
      let cmd = Types.MakeCmd (focusItem cp.index) in
      let m = Types.TweakModel (fun m -> {m with fluidState = {s with cp}}) in
      Types.Many [m; cmd]
  | K.Escape ->
      FluidCommandsClose
  | _ ->
      NoChange
