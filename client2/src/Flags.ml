open Tea
open! Porting

type parameter =
  { name: string
  ; tipe: string
  ; block_args: string list
  ; optional: bool
  ; description: string }

and function_ =
  { name: string
  ; parameters: parameter list
  ; description: string
  ; return_type: string
  ; preview_execution_safe: bool
  ; deprecated: bool
  ; infix: bool }

and flags =
  { editorState: string option
  ; complete: function_ list
  ; userContentHost: string
  ; environment: string }
