module Parser.RuntimeTypes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

let parseExprWithTypes
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (code : string)
  : LibExecution.RuntimeTypes.Expr =
  ProgramTypes.parseExprWithTypes availableTypes code
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parseExpr = parseExprWithTypes []
