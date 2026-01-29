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
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (filename : string)
  (code : string)
  : Ply<PT.Expr> =
  let context =
    { WT2PT.Context.currentFnName = None
      WT2PT.Context.isInFunction = false
      WT2PT.Context.argMap = Map.empty }
  code
  |> initialParse filename
  |> WT2PT.Expr.toPT builtins pm onMissing [] context


let parseSimple
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (filename : string)
  (code : string)
  : Ply<PT.Expr> =
  parsePTExpr builtins pm onMissing filename code



let parsePackageFile
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (path : string)
  (contents : string)
  : Ply<List<PT.PackageOp>> =
  Package.parse builtins pm onMissing path contents
