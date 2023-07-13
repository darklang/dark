/// Entrypoint to parsing functions
module Parser.Parser

open Prelude
open Tablecloth

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes

/// Returns an incomplete parse of a PT expression. Requires calling
/// Expr.resolveNames before using
let initialParse (filename : string) (code : string) : PT.Expr =
  let resolver = WrittenTypesToProgramTypes.NameResolver.empty
  code
  |> Utils.parseAsFSharpSourceFile filename
  |> Utils.singleExprFromImplFile
  |> FS2WT.Expr.fromSynExpr
  |> WT2PT.Expr.toPT resolver


// Shortcut function for tests that ignore user functions and types
let parseIgnoringUser (filename : string) (code : string) : PT.Expr =
  code
  |> initialParse filename
  |> NameResolution.Expr.resolveNames Set.empty Set.empty

let parseRTExpr
  (fns : Set<PT.FnName.UserProgram>)
  (types : Set<PT.TypeName.UserProgram>)
  (filename : string)
  (code : string)
  : LibExecution.RuntimeTypes.Expr =
  code
  |> initialParse filename
  |> NameResolution.Expr.resolveNames fns types
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT

let parsePTExpr (filename : string) (code : string) : PT.Expr =
  code
  |> initialParse filename
  |> NameResolution.Expr.resolveNames Set.empty Set.empty


let parsePackage
  (path : string)
  (contents : string)
  : List<PT.PackageFn.T> * List<PT.PackageType.T> =
  let pModule = Package.parse path contents
  (pModule.fns, pModule.types)
