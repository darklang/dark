module Parser.RuntimeTypes

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module PTP = Parser.ProgramTypes

let parseExprWithTypes
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (code : string)
  : LibExecution.RuntimeTypes.Expr =
  PTP.parseExprWithTypes availableTypes code
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parseExpr = parseExprWithTypes []
