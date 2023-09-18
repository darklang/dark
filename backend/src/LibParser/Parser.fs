/// Entrypoint to parsing functions
module LibParser.Parser

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module RT = LibExecution.RuntimeTypes
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
  : Ply<PT.Expr> =
  code |> initialParse filename |> WT2PT.Expr.toPT resolver []

let parseSimple
  (filename : string)
  (code : string)
  : Ply<PT.Expr> =
  parsePTExpr NameResolver.empty filename code


let parseRTExpr
  (resolver : NameResolver.NameResolver)
  (filename : string)
  (code : string)
  : Ply<RT.Expr> =
  code
  |> parsePTExpr resolver filename
  |> Ply.map (LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT)

type Packages =
  List<PT.PackageFn.T> * List<PT.PackageType.T> * List<PT.PackageConstant.T>

let parsePackageFile
  (resolver : NameResolver.NameResolver)
  (path : string)
  (contents : string)
  : Ply<Packages> =
  uply {
    let! pModule = Package.parse resolver path contents
    return (pModule.fns, pModule.types, pModule.constants)
  }
