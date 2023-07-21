/// Entrypoint to parsing functions
module Parser.Parser

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes

// Parse the program, returning a WrittenTypes expression which doesn't have any names resolved. Call `WT2PT.toPT resolver`
let initialParse (filename : string) (code : string) : WT.Expr =
  code
  |> Utils.parseAsFSharpSourceFile filename
  |> Utils.singleExprFromImplFile
  |> FS2WT.Expr.fromSynExpr

/// Returns an incomplete parse of a PT expression
let parsePTExpr
  (resolver : NameResolver.NameResolver)
  (filename : string)
  (code : string)
  : PT.Expr =
  code |> initialParse filename |> WT2PT.Expr.toPT resolver

let parseSimple (filename : string) (code : string) : PT.Expr =
  parsePTExpr NameResolver.empty filename code


let parseRTExpr
  (resolver : NameResolver.NameResolver)
  (filename : string)
  (code : string)
  : LibExecution.RuntimeTypes.Expr =
  code
  |> parsePTExpr resolver filename
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parsePackage
  (resolver: NameResolver.NameResolver)
  (path : string)
  (contents : string)
  : List<PT.PackageFn.T> * List<PT.PackageType.T> =
  let pModule = Package.parse resolver path contents
  (pModule.fns, pModule.types)
