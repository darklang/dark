module Parser.RuntimeTypes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
open Utils

let parseExprWithTypes (code : string) : LibExecution.RuntimeTypes.Expr =
  ProgramTypes.parseExprWithTypes code
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parseExpr = parseExprWithTypes
