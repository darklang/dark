open Tea
open! Porting
module TL = Toplevel
open Types

let executeCommand (m : model) (tlid : tlid) (id : id)
    (highlighted : autocompleteItem option) : modification =
  match highlighted with
  | Some (ACCommand command) ->
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      command.action m tl pd
  | _ -> NoChange

let endCommandExecution (m : model) (tlid : tlid) (id : id) : modification =
  Many [AutocompleteMod ACReset; Select (tlid, Some id)]

let commands : command list =
  [ { name= "extract-function"
    ; action= Refactor.extractFunction
    ; doc= "Extract expression into a function"
    ; shortcut= "Ctrl-F" }
  ; { name= "extract-variable"
    ; action= Refactor.extractVariable
    ; doc= "Extract expression into a variable"
    ; shortcut= "Ctrl-Shift-L" }
  ; { name= "wrap-if-condition"
    ; action= Refactor.wrap Refactor.WIfCond
    ; doc=
        "Wrap the expression in an if, using the expression as the condition"
    ; shortcut= "Ctrl-Alt-C" }
  ; { name= "wrap-if-then"
    ; action= Refactor.wrap Refactor.WIfThen
    ; doc=
        "Wrap the expression in an if, putting this expression in the `then` \
         body"
    ; shortcut= "Ctrl-I" }
  ; { name= "wrap-if-else"
    ; action= Refactor.wrap Refactor.WIfElse
    ; doc=
        "Wrap the expression in an if, putting this expression in the `else` \
         body"
    ; shortcut= "Ctrl-Alt-I" }
  ; { name= "insert-let-above"
    ; action= Refactor.wrap Refactor.WLetBody
    ; doc= "Add a let on the line above"
    ; shortcut= "Ctrl-B" }
  ; { name= "insert-let-here"
    ; action= Refactor.wrap Refactor.WLetRHS
    ; doc= "Wrap expression in a let"
    ; shortcut= "Ctrl-L" }
  ; { name= "toggle-expression-on-rail"
    ; action= Refactor.toggleOnRail
    ; doc=
        "Switch between using the error rail, or handling this expression \
         yourself"
    ; shortcut= "Alt-E" } ]
