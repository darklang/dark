open Tc
open Types
open Prelude
module TL = Toplevel

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard

let filterInputID : string = "cmd-filter"

let fluidCommands =
  Commands.commands
  |> List.filter ~f:(fun {commandName; _} ->
         not (commandName == "add-feature-flag") )


let reset : fluidCommandState =
  {index = 0; commands = fluidCommands; location = None; filter = None}


let commandsFor (tl : toplevel) (id : id) : command list =
  (* NB: do not structurally compare entire Command.command records here, as
   * they contain functions, which BS cannot compare.*)
  let filterForRail rail =
    fluidCommands
    |> List.filter ~f:(fun c ->
           match rail with
           | Rail ->
               c.commandName <> Commands.putFunctionOnRail.commandName
           | NoRail ->
               c.commandName <> Commands.takeFunctionOffRail.commandName )
  in
  Toplevel.getAST tl
  |> Option.andThen ~f:(fun x -> AST.find id x)
  |> Option.andThen ~f:(fun pd ->
         match pd with
         | PExpr (F (_, FnCall (_, _, rail))) ->
             Some (filterForRail rail)
         | _ ->
             let cmds =
               fluidCommands
               |> List.filter ~f:(fun c ->
                      c.commandName <> Commands.putFunctionOnRail.commandName
                      && c.commandName
                         <> Commands.takeFunctionOffRail.commandName )
             in
             Some cmds )
  |> Option.withDefault ~default:fluidCommands


let show (tl : toplevel) (token : fluidToken) : fluidCommandState =
  { index = 0
  ; commands = commandsFor tl (FluidToken.tid token)
  ; location = Some (TL.id tl, token)
  ; filter = None }


let executeCommand
    (m : model) (tlid : tlid) (token : fluidToken) (cmd : command) :
    modification =
  match TL.getTLAndPD m tlid (FluidToken.tid token) with
  | Some (tl, Some pd) ->
      cmd.action m tl pd
  | _ ->
      recover "No pd for the command" ~debug:(tlid, token, cmd) NoChange


let runCommand (m : model) (cmd : command) : modification =
  let cp = m.fluidState.cp in
  match cp.location with
  | Some (tlid, token) ->
      executeCommand m tlid token cmd
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


let filter (m : model) (query : string) (cp : fluidCommandState) :
    fluidCommandState =
  let allCmds =
    match cp.location with
    | Some (tlid, token) ->
        Option.map (TL.get m tlid) ~f:(fun tl ->
            commandsFor tl (FluidToken.tid token) )
        |> recoverOpt "no tl for location" ~default:[]
    | _ ->
        fluidCommands
  in
  let filter, commands =
    if String.length query > 0
    then
      let isMatched c = String.contains ~substring:query c.commandName in
      (Some query, List.filter ~f:isMatched allCmds)
    else (None, fluidCommands)
  in
  {cp with filter; commands; index = 0}


let isOpenOnTL (s : fluidCommandState) (tlid : tlid) : bool =
  match s.location with
  | Some (ltlid, _) when tlid = ltlid ->
      true
  | _ ->
      false


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
            FluidMsg (FluidCommandsClick item) )
      ; ViewUtils.eventBoth ~key:("-mousemove" ^ name) "mousemove" (fun _ ->
            FluidMsg (FluidUpdateDropdownIndex i) ) ]
      [Html.text name]
  in
  let filterInput =
    Html.input'
      [ Attrs.id filterInputID
      ; Vdom.attribute "" "spellcheck" "false"
      ; Attrs.autocomplete false
      ; Events.onInput (fun query -> FluidMsg (FluidCommandsFilter query)) ]
      []
  in
  let cmdsView =
    Html.div
      [Attrs.id "fluid-dropdown"]
      [Html.ul [] (List.indexedMap ~f:viewCommands cp.commands)]
  in
  Html.div [Html.class' "command-palette"] [filterInput; cmdsView]


let cpSetIndex (_m : Types.model) (i : int) (s : Types.fluidState) :
    Types.modification =
  let newState = {s with cp = {s.cp with index = i}; upDownCol = None} in
  let cmd = Types.MakeCmd (focusItem i) in
  let m = Types.TweakModel (fun m -> {m with fluidState = newState}) in
  Types.Many [m; cmd]


let updateCmds (m : Types.model) (keyEvt : K.keyEvent) : Types.modification =
  let s = m.fluidState in
  let key = keyEvt.key in
  match key with
  | K.Enter ->
    ( match s.cp.location with
    | Some (tlid, token) ->
      ( match highlighted s.cp with
      | Some cmd ->
          Many [executeCommand m tlid token cmd; FluidCommandsClose]
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


let isOpened (cp : fluidCommandState) : bool = cp.location <> None

let updateCommandPaletteVisability (m : model) : model =
  let oldTlid =
    match m.fluidState.cp.location with
    | Some (tlid, _) ->
        Some tlid
    | None ->
        tlidOf m.cursorState
  in
  let newTlid = tlidOf m.cursorState in
  if isOpened m.fluidState.cp && oldTlid <> newTlid
  then
    let newCp = reset in
    {m with fluidState = {m.fluidState with cp = newCp}}
  else m
