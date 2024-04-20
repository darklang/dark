/// Entrypoint to parsing functions
module LibParser.Parser

open Prelude

module FS2WT = FSharpToWrittenTypes
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = NameResolver

/// Parse the program, returning a WrittenTypes expression which doesn't have any names
/// resolved. Call `WT2PT.toPT resolver`
let initialParse (filename : string) (code : string) : WT.Expr =
  code
  |> Utils.parseAsFSharpSourceFile filename
  |> Utils.singleExprFromImplFile
  |> FS2WT.Expr.fromSynExpr


/// Returns an incomplete parse of a PT expression
let parsePTExpr
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (filename : string)
  (code : string)
  : Ply<PT.Expr> =
  code
  |> initialParse filename
  |> WT2PT.Expr.toPT builtins pm userStuff onMissing []


let parseSimple
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (filename : string)
  (code : string)
  : Ply<PT.Expr> =
  parsePTExpr builtins pm userStuff onMissing filename code


let parseRTExpr
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (filename : string)
  (code : string)
  : Ply<LibExecution.RuntimeTypes.Expr> =
  code
  |> parsePTExpr builtins pm userStuff onMissing filename
  |> Ply.map LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT


let parsePackageFile
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (userStuff : NR.UserStuff)
  (onMissing : NR.OnMissing)
  (path : string)
  (contents : string)
  : Ply<PT.Packages> =
  uply {
    let! pModule = Package.parse builtins pm userStuff onMissing path contents
    return
      { types = pModule.types; constants = pModule.constants; fns = pModule.fns }
  }
