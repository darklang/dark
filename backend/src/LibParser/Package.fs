/// Parse + lower package files with the hand-written parser. `parse` is the public
/// entrypoint used by the package loader (LocalExec).
module LibParser.Package

open Prelude
open LibExecution.ProgramTypes

module P = LibParser.Parser
module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module WTSourceFile = SourceFile
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = NameResolver
module PackageLocation = LibDB.PackageLocation
open LibSerialization.Hashing


type private WTPackageModule =
  { fns : List<WT.PackageFn.PackageFn>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue> }
/// Lower a WT package module to PackageOps (WT2PT lowering + AddX/SetName op
/// generation).
let private wtModuleToOps
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (modul : WTPackageModule)
  : Ply<List<PT.PackageOp>> =
  uply {
    let! fns =
      modul.fns
      |> Ply.List.mapSequentially (fun fn ->
        WT2PT.PackageFn.toPT
          builtins
          pm
          onMissing
          (WT2PT.PackageFn.Name.toModules fn.name)
          fn)

    let! types =
      modul.types
      |> Ply.List.mapSequentially (fun typ ->
        WT2PT.PackageType.toPT
          pm
          onMissing
          (WT2PT.PackageType.Name.toModules typ.name)
          typ)

    let! values =
      modul.values
      |> Ply.List.mapSequentially (fun value ->
        WT2PT.PackageValue.toPT
          builtins
          pm
          onMissing
          (WT2PT.PackageValue.Name.toModules value.name)
          value)

    // Set*Name ops carry a placeholder; the real hash replaces it in
    // LoadPackagesFromDisk.computeRealHashes.
    let nameBasedHash = PackageLocation.placeholderHash

    let ops : List<PT.PackageOp> =
      [ for (wtType, ptType) in List.zip modul.types types do
          yield PT.PackageOp.AddType ptType
          let loc = WT2PT.PackageType.Name.toLocation wtType.name
          yield PT.PackageOp.SetName(loc, PT.PackageType(nameBasedHash loc), None)

        for (wtValue, ptValue) in List.zip modul.values values do
          yield PT.PackageOp.AddValue ptValue
          let loc = WT2PT.PackageValue.Name.toLocation wtValue.name
          yield PT.PackageOp.SetName(loc, PT.PackageValue(nameBasedHash loc), None)

        for (wtFn, ptFn) in List.zip modul.fns fns do
          yield PT.PackageOp.AddFn ptFn
          let loc = WT2PT.PackageFn.Name.toLocation wtFn.name
          yield PT.PackageOp.SetName(loc, PT.PackageFn(nameBasedHash loc), None) ]

    return ops
  }

// --- classify package declarations from a parsed package file ---
//
// Package declarations must live under `module Owner...`; the path's first
// segment is the owner and the rest are modules. Items that cannot be package
// declarations become errors.

type private PkgItem =
  | PFn of WT.PackageFn.PackageFn
  | PType of WT.PackageType.PackageType
  | PValue of WT.PackageValue.PackageValue
  | PErr of WT.Range * string

let private noOwner (kind : string) (name : string) : string =
  $"{kind} '{name}' is outside any 'module Owner.…' — package declarations must live inside an owner module"

let private packageItem (item : WTSourceFile.Item) : PkgItem =
  match item with
  | WTSourceFile.Fn(path, fn) ->
    match path with
    | owner :: modules -> PFn(WT.packageFn owner modules fn)
    | [] -> PErr(fn.range, noOwner "function" fn.name.name)
  | WTSourceFile.Type(path, t) ->
    match path with
    | owner :: modules -> PType(WT.packageType owner modules t)
    | [] -> PErr(t.range, noOwner "type" t.name.name)
  | WTSourceFile.Value(path, v) ->
    match path with
    | owner :: modules -> PValue(WT.packageValue owner modules v)
    | [] -> PErr(v.range, noOwner "value" v.name.name)
  | WTSourceFile.Expr(_, e) ->
    PErr(WT.exprRange e, "expressions are not allowed in package files")
  | WTSourceFile.TypeDB(_, t) ->
    PErr(t.range, "[<DB>] declarations are not allowed in package files")
  | WTSourceFile.Test(_, t) ->
    PErr(t.range, "test assertions are not allowed in package files")

/// Lower a parsed package file to module-qualified package declarations, plus
/// errors for declarations a package file can't hold.
let private packageDecls
  (validated : Validation.ValidatedSourceFile)
  : WTPackageModule * List<WT.Range * string> =
  let sf = Validation.ValidatedSourceFile.toWrittenTypes validated
  let items = WTSourceFile.items sf |> List.map packageItem
  let fns =
    items
    |> List.choose (function
      | PFn f -> Some f
      | _ -> None)
  let types =
    items
    |> List.choose (function
      | PType t -> Some t
      | _ -> None)
  let values =
    items
    |> List.choose (function
      | PValue v -> Some v
      | _ -> None)
  let errors =
    items
    |> List.choose (function
      | PErr(r, msg) -> Some(r, msg)
      | _ -> None)
  ({ fns = fns; types = types; values = values }, errors)

/// Parse + lower a package file: the nested module tree gives module-qualified
/// names. Returns `Error diagnostics` on parse failure.
let parse
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (contents : string)
  : Ply<Result<List<PT.PackageOp>, List<string>>> =
  uply {
    match P.parseFor Validation.Package contents with
    | Error diagnostics ->
      return Error(diagnostics |> List.map (P.renderDiagnostic contents))
    | Ok validated ->
      let (modul, packageErrors) = packageDecls validated
      match packageErrors with
      | [] ->
        let! ops = wtModuleToOps builtins pm onMissing modul
        return Ok ops
      | errors ->
        return
          Error(
            errors
            |> List.map (fun (r, msg) ->
              $"error at {r.start.row + 1}:{r.start.column + 1}: {msg}")
          )
  }
