open Tc
open Types

let reset : fluidCommandState =
  { index = 0
  ; show = false
  ; commands = Commands.commands
  ; cmdOnTL = None
  ; cmdOnID = None
  ; filter = None }


let commandsFor (tl : toplevel) (id : id) : fluidCommandState =
  { index = 0
  ; show = true
  ; commands = Commands.commands
  ; cmdOnTL = Some tl
  ; cmdOnID = Some id
  ; filter = None }


let executeCommand (m : model) (tl : toplevel) (id : id) (cmd : command) :
    modification =
  let pd = Toplevel.findExn tl id in
  cmd.action m tl pd


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


let filterCommands (query : string option) : command list =
  match query with
  | Some v ->
      let isSubstring c = String.contains ~substring:v c.commandName in
      List.filter ~f:isSubstring Commands.commands
  | None ->
      Commands.commands


let addFilterChar (s : fluidCommandState) (c : char) : fluidCommandState =
  let v = Option.withDefault ~default:"" s.filter ^ String.fromChar c in
  let filter = Some v in
  let commands = filterCommands filter in
  {s with filter; commands; index = 0}


let removeFilterChar (s : fluidCommandState) : fluidCommandState =
  let filter =
    Option.andThen
      ~f:(fun s ->
        let v = String.dropRight ~count:1 s in
        if String.length v > 0 then Some v else None )
      s.filter
  in
  let commands = filterCommands filter in
  {s with filter; commands; index = 0}
