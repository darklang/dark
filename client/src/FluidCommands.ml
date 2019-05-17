open Tc
open Types

let reset (s : fluidCommandState) : fluidCommandState =
  { index = 0
  ; show = false
  ; commands =
      (if List.isEmpty s.commands then Commands.commands else s.commands)
  ; cmdOnTL = None
  ; cmdOnID = None }


let commandsFor (s : fluidCommandState) (tl : toplevel) (id : id) :
    fluidCommandState =
  { index = 0
  ; show = true
  ; commands =
      (if List.isEmpty s.commands then Commands.commands else s.commands)
  ; cmdOnTL = Some tl
  ; cmdOnID = Some id }


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
