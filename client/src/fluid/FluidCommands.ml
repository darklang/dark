open Prelude
module TL = Toplevel
module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard

let filterInputID : string = "cmd-filter"

let fluidCommands (_m : model) = Commands.commands

let reset (m : model) : fluidCommandState =
  {index = 0; commands = fluidCommands m; location = None; filter = None}


let commandsFor (m : model) (tl : toplevel) (expr : fluidExpr) : command list =
  fluidCommands m |> List.filter ~f:(fun cmd -> cmd.shouldShow m tl expr)


let show (m : model) (tlid : TLID.t) (id : ID.t) : model =
  let tl = TL.get m tlid in
  tl
  |> Option.andThen ~f:TL.getAST
  |> Option.andThen ~f:(FluidAST.find id)
  |> Option.map2 tl ~f:(fun tl expr ->
         let cp =
           { index = 0
           ; commands = commandsFor m tl expr
           ; location = Some (tlid, id)
           ; filter = None }
         in
         {m with fluidState = {m.fluidState with cp}})
  |> Option.unwrap ~default:m


let executeCommand (m : model) (tlid : TLID.t) (id : ID.t) (cmd : command) :
    modification =
  match TL.get m tlid with
  | Some tl ->
      cmd.action m tl id
  | _ ->
      recover "No pd for the command" ~debug:(tlid, id, cmd) NoChange


let runCommand (m : model) (cmd : command) : modification =
  let cp = m.fluidState.cp in
  match cp.location with
  | Some (tlid, id) ->
      executeCommand m tlid id cmd
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
    (fun _ -> IgnoreMsg "fluid-commands-focus")
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
             ()))


let filter (m : model) (query : string) (cp : fluidCommandState) :
    fluidCommandState =
  let allCmds =
    match cp.location with
    | Some (tlid, id) ->
        let tl = TL.get m tlid in
        tl
        |> Option.andThen ~f:TL.getAST
        |> Option.map2 tl ~f:(fun tl ast ->
               ast
               |> FluidAST.find id
               |> function Some expr -> commandsFor m tl expr | None -> [])
        |> recoverOpt "no tl for location" ~default:[]
    | _ ->
        fluidCommands m
  in
  let filter, commands =
    if String.length query > 0
    then
      let isMatched c = String.includes ~substring:query c.commandName in
      (Some query, List.filter ~f:isMatched allCmds)
    else (None, fluidCommands m)
  in
  {cp with filter; commands; index = 0}


let isOpenOnTL (s : fluidCommandState) (tlid : TLID.t) : bool =
  match s.location with Some (ltlid, _) when tlid = ltlid -> true | _ -> false


(** onKeydown is a special keydown handler for the command palette,
  * which handles a few specific keypresses and ignores everything else.
  *
  * We can't use the generic FluidKeyboard keydown handler for this, as it's
  * too agreessive in capturing keys that we want delegated to the palette's
  * input element for default handling (like backspace and left/right arrows). *)
let onKeydown (evt : Web.Node.event) : Types.msg option =
  K.eventToKeyEvent evt
  |> Option.andThen ~f:(fun e ->
         match e with
         | {K.key = K.Enter; _}
         | {key = K.Up; _}
         | {key = K.Down; _}
         | {key = K.Escape; _} ->
             Some (FluidMsg (FluidInputEvent (Keypress e)))
         | _ ->
             None)


let onLoseFocus (_evt : Web.Node.event) : Types.msg option =
  Some (FluidMsg FluidCloseCmdPalette)


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
      ; ViewUtils.eventNoPropagation ~key:("cp-" ^ name) "mousedown" (fun _ ->
            FluidMsg (FluidCommandsClick item))
      ; ViewUtils.eventBoth ~key:("-mousemove" ^ name) "mousemove" (fun _ ->
            FluidMsg (FluidUpdateDropdownIndex i)) ]
      [Html.text name]
  in
  let filterInput =
    Html.input'
      [ Attrs.id filterInputID
      ; Vdom.attribute "" "spellcheck" "false"
      ; Attrs.autocomplete false
      ; Events.onInput (fun query -> FluidMsg (FluidCommandsFilter query))
      ; Html.onCB "keydown" "command-keydown" onKeydown
      ; Html.onCB "blur" "lose focus" onLoseFocus ]
      []
  in
  let cmdsView =
    Html.div
      [Attrs.id "fluid-dropdown"]
      [Html.ul [] (List.mapWithIndex ~f:viewCommands cp.commands)]
  in
  Html.div [Html.class' "command-palette"] [filterInput; cmdsView]


let cpSetIndex (_m : Types.model) (i : int) : Types.modification =
  ReplaceAllModificationsWithThisOne
    (fun m ->
      let cp = {m.fluidState.cp with index = i} in
      let fluidState = {m.fluidState with cp; upDownCol = None} in
      ({m with fluidState}, focusItem i))


let updateCmds (m : Types.model) (keyEvt : K.keyEvent) : Types.modification =
  let key = keyEvt.key in
  match key with
  | K.Enter ->
    ( match m.fluidState.cp.location with
    | Some (tlid, id) ->
      ( match highlighted m.fluidState.cp with
      | Some cmd ->
          Many [executeCommand m tlid id cmd; FluidCommandsClose]
      | None ->
          NoChange )
    | _ ->
        NoChange )
  | K.Up ->
      ReplaceAllModificationsWithThisOne
        (fun m ->
          let cp = moveUp m.fluidState.cp in
          let fluidState = {m.fluidState with cp} in
          ({m with fluidState}, focusItem cp.index))
  | K.Down ->
      ReplaceAllModificationsWithThisOne
        (fun m ->
          let cp = moveDown m.fluidState.cp in
          let fluidState = {m.fluidState with cp} in
          ({m with fluidState}, focusItem cp.index))
  | K.Escape ->
      FluidCommandsClose
  | _ ->
      NoChange


let isOpened (cp : fluidCommandState) : bool = cp.location <> None

let updateCommandPaletteVisibility (m : model) : model =
  let oldTlid =
    match m.fluidState.cp.location with
    | Some (tlid, _) ->
        Some tlid
    | None ->
        CursorState.tlidOf m.cursorState
  in
  let newTlid = CursorState.tlidOf m.cursorState in
  if isOpened m.fluidState.cp && oldTlid <> newTlid
  then
    let newCp = reset m in
    {m with fluidState = {m.fluidState with cp = newCp}}
  else m
