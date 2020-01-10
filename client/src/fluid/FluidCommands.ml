open Prelude
module TL = Toplevel
module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module K = FluidKeyboard
module Regex = Util.Regex

let filterInputID : string = "cmd-filter"

let fluidCommands =
  Commands.commands
  |> List.filter ~f:(fun {commandName; _} ->
         not (commandName == "add-feature-flag"))


let reset : fluidCommandState =
  {index = 0; commands = fluidCommands; location = None; filter = None}


let commandsFor (expr : fluidExpr) : command list =
  (* NB: do not structurally compare entire Command.command records here, as
   * they contain functions, which BS cannot compare.*)
  Debug.loG "vox" "commandsFor";
  let noPutOn c = c.commandName <> Commands.putFunctionOnRail.commandName in
  let noTakeOff c = c.commandName <> Commands.takeFunctionOffRail.commandName in
  let railFilters =
    match expr with
    | EFnCall (_, _, _, Rail) ->
        noPutOn
    | EFnCall (_, _, _, NoRail) ->
        noTakeOff
    | _ ->
        fun c -> noTakeOff c && noPutOn c
  in
  let httpClientRegex =
    Regex.regex "HttpClient::(delete|get|head|options|patch|post|put)"
  in
  let httpClientRequestFilter =
    match expr with
    | EFnCall (_, fluidName, _, _)
      when Regex.contains ~re:httpClientRegex fluidName ->
        fun _ -> true
    | _ ->
        fun c -> c.commandName <> "copy-request-as-curl"
  in
  let cmds = 
    fluidCommands
    |> List.filter ~f:railFilters
    |> List.filter ~f:httpClientRequestFilter
  in
  Debug.loG "vox filtered commands" cmds;
  cmds


let show (m : model) (id : id) : model =
  TL.selectedAST m
  |> Option.andThen ~f:(FluidExpression.find id)
  |> Option.map ~f:(fun expr ->
    let cp =
      { index = 0
      ; commands = commandsFor expr
      ; location = Some id
      ; filter = None }
    in
    {m with fluidState = {m.fluidState with cp}}
  )
  |> Option.withDefault ~default:m


let executeCommand (m : model) (id : id) (cmd : command) : modification =
  match TL.selected m with
  | Some tl ->
      cmd.action m tl id
  | _ ->
      recover "Can't find selected toplevel" ~debug:(id, cmd) NoChange


let runCommand (m : model) (cmd : command) : modification =
  let cp = m.fluidState.cp in
  match cp.location with
  | Some id ->
      executeCommand m id cmd
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
             ()))


let filter (m : model) (query : string) (cp : fluidCommandState) :
    fluidCommandState =
  let allCmds =
    match cp.location with
    | Some id ->
      TL.selected m
      |> Option.andThen ~f:TL.getAST
      |> Option.andThen ~f:(FluidExpression.find id)
      |> Option.map ~f:commandsFor
      |> Option.withDefault ~default:fluidCommands
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


let viewCommandPalette (cp : Types.fluidCommandState) : Types.msg Html.html =
  Debug.loG "vox viewCommandPalette" cp;
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
    | Some id ->
      ( match highlighted s.cp with
      | Some cmd ->
          Many [executeCommand m id cmd; FluidCommandsClose]
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

let updateCommandPaletteVisibility (m : model) : model =
  (* TODO(alice) calculate visibility *)
  m
