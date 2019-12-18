open Types
open Fluid
module B = Blank
module K = FluidKeyboard

let debugState s =
  show_fluidState
    { s with
      (* remove the things that take a lot of space and provide little value. *)
      ac =
        { s.ac with
          functions = []
        ; allCompletions = []
        ; completions = (if s.ac.index = None then [] else s.ac.completions) }
    }


let h ast : handler =
  { ast = Expression.toNexpr ast
  ; hTLID = TLID "7"
  ; pos = {x = 0; y = 0}
  ; spec =
      { space = Blank.newF "HTTP"
      ; name = Blank.newF "/test"
      ; modifier = Blank.newF "GET" } }
