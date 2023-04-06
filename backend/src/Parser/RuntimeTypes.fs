module Parser.RuntimeTypes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
open Utils

let parseExprWithTypes
  (availableTypes : AvailableTypes)
  (code : string)
  : LibExecution.RuntimeTypes.Expr =
  ProgramTypes.parseExprWithTypes availableTypes code
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parseExpr = parseExprWithTypes []
