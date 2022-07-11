open Prelude

let debugState = s =>
  show_fluidState({
    ...s,
    // remove the things that take a lot of space and provide little value.
    ac: {
      ...s.ac,
      completions: if s.ac.index == None {
        list{}
      } else {
        s.ac.completions
      },
    },
  })

let h = (expr: FluidExpression.t): handler => {
  ast: FluidAST.ofExpr(expr),
  hTLID: TLID.fromInt(7),
  pos: {x: 0, y: 0},
  spec: {
    space: BlankOr.newF("HTTP"),
    name: BlankOr.newF("/test"),
    modifier: BlankOr.newF("GET"),
  },
}
