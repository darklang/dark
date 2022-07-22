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

let h = (expr: FluidExpression.t): PT.Handler.t => {
  ast: FluidAST.ofExpr(expr),
  tlid: TLID.fromInt(7),
  pos: {x: 0, y: 0},
  spec: {
    space: BlankOr.newF("HTTP"), // HttpBytesTODO figure out what this is about
    name: BlankOr.newF("/test"),
    modifier: BlankOr.newF("GET"),
  },
}
