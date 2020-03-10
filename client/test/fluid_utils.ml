open Prelude

let debugState s =
  show_fluidState
    { s with
      (* remove the things that take a lot of space and provide little value. *)
      ac =
        { s.ac with
          functions = []
        ; completions = (if s.ac.index = None then [] else s.ac.completions) }
    }


let h (expr : FluidExpression.t) : handler =
  { ast = FluidAST.ofExpr expr
  ; hTLID = TLID.fromString "7"
  ; pos = {x = 0; y = 0}
  ; spec =
      { space = BlankOr.newF "HTTP"
      ; name = BlankOr.newF "/test"
      ; modifier = BlankOr.newF "GET" } }
