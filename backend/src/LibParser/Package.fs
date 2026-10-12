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
module HashStabilization = LibDB.HashStabilization
open LibSerialization.Hashing


type private WTPackageModule =
  { fns : List<WT.PackageFn.PackageFn>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue>
    traits : List<WT.PackageTrait.PackageTrait>
    impls : List<WT.PackageTraitImpl.PackageTraitImpl>
    tests : List<WT.PackageTest.PackageTest>
    dbs : List<List<string> * WT.TypeDecl> }
/// Lower a WT package module to PackageOps (WT2PT lowering + AddX/SetName op
/// generation).
let private wtModuleToOps
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (modul : WTPackageModule)
  : Ply<List<PT.PackageOp>> =
  uply {
    let! types =
      modul.types
      |> Ply.List.mapSequentially (fun typ ->
        WT2PT.PackageType.toPT
          pm
          onMissing
          (WT2PT.PackageType.Name.toModules typ.name)
          typ)

    // Types in this file are not in `pm` yet. Give them real hashes in a
    // temporary overlay so functions, values, and tests can resolve same-file
    // types, including inline [<DB>] row types. The overlay does not save ops.
    let nameBasedHash = PackageLocation.placeholderHash
    let typeOps : List<PT.PackageOp> =
      [ for (wtType, ptType) in List.zip modul.types types do
          yield PT.PackageOp.AddType ptType
          let loc = WT2PT.PackageType.Name.toLocation wtType.name
          yield PT.PackageOp.SetName(loc, PT.PackageType(nameBasedHash loc), None) ]
    let pmWithTypes =
      if List.isEmpty typeOps then
        pm
      else
        typeOps
        |> HashStabilization.computeRealHashes
        |> LibDB.PackageManager.withExtraOps pm

    let! fns =
      modul.fns
      |> Ply.List.mapSequentially (fun fn ->
        WT2PT.PackageFn.toPT
          builtins
          pmWithTypes
          onMissing
          (WT2PT.PackageFn.Name.toModules fn.name)
          fn)

    let! values =
      modul.values
      |> Ply.List.mapSequentially (fun value ->
        WT2PT.PackageValue.toPT
          builtins
          pmWithTypes
          onMissing
          (WT2PT.PackageValue.Name.toModules value.name)
          value)

    let! traits =
      modul.traits
      |> Ply.List.mapSequentially (fun t ->
        WT2PT.Trait.toPT pm onMissing (t.name.owner :: t.name.modules) t)

    let! impls =
      modul.impls
      |> Ply.List.mapSequentially (fun i ->
        // Method targets resolve from the impl's own module: its member path.
        WT2PT.TraitImpl.toPT
          builtins
          pm
          onMissing
          (i.name.owner :: i.name.modules @ [ i.name.name ])
          i)
    let! tests =
      modul.tests
      |> Ply.List.mapSequentially (fun test ->
        uply {
          let currentModule = WT2PT.PackageTest.Name.toModules test.name
          let! testDBs =
            modul.dbs
            |> List.filter (fun (path, _) ->
              List.length path <= List.length currentModule
              && List.take (List.length path) currentModule = path)
            |> Ply.List.mapSequentially (fun (path, db) ->
              uply {
                let rowType = WT.dbRowTypeReference path db
                let! typ =
                  WT2PT.TypeReference.toPT pmWithTypes onMissing path rowType
                return db.name.name, typ
              })
          let! ptTest =
            WT2PT.PackageTest.toPT builtins pmWithTypes onMissing currentModule test
          return { ptTest with testDBs = testDBs }
        })

    // Emit the original type ops once. The temporary overlay used stabilized
    // copies; the final load pass replaces these SetName placeholders.
    let ops : List<PT.PackageOp> =
      [ yield! typeOps

        for (wtValue, ptValue) in List.zip modul.values values do
          yield PT.PackageOp.AddValue ptValue
          let loc = WT2PT.PackageValue.Name.toLocation wtValue.name
          yield PT.PackageOp.SetName(loc, PT.PackageValue(nameBasedHash loc), None)

        for (wtFn, ptFn) in List.zip modul.fns fns do
          yield PT.PackageOp.AddFn ptFn
          let loc = WT2PT.PackageFn.Name.toLocation wtFn.name
          yield PT.PackageOp.SetName(loc, PT.PackageFn(nameBasedHash loc), None)

        for (wtTrait, ptTrait) in List.zip modul.traits traits do
          yield PT.PackageOp.AddTrait ptTrait
          let loc = WT2PT.Trait.Name.toLocation wtTrait.name
          yield PT.PackageOp.SetName(loc, PT.PackageTrait(nameBasedHash loc), None)

        for (wtImpl, ptImpl) in List.zip modul.impls impls do
          yield PT.PackageOp.AddTraitImpl ptImpl
          let loc = WT2PT.TraitImpl.Name.toLocation wtImpl.name
          yield
            PT.PackageOp.SetName(loc, PT.PackageTraitImpl(nameBasedHash loc), None)

        for (wtTest, ptTest) in List.zip modul.tests tests do
          yield PT.PackageOp.AddTest ptTest
          let loc = WT2PT.PackageTest.Name.toLocation wtTest.name
          yield PT.PackageOp.SetName(loc, PT.PackageTest(nameBasedHash loc), None) ]

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
  | PTrait of WT.PackageTrait.PackageTrait
  | PImpl of WT.PackageTraitImpl.PackageTraitImpl
  | PTest of WT.PackageTest.PackageTest
  | PDB of List<string> * WT.TypeDecl
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
  | WTSourceFile.Trait(path, t) ->
    match path with
    | owner :: modules -> PTrait(WT.packageTrait owner modules t)
    | [] -> PErr(t.range, noOwner "trait" t.name.name)
  | WTSourceFile.Impl(memberPath, impl) ->
    match memberPath with
    | owner :: rest -> PImpl(WT.packageImpl owner rest impl)
    | [] -> PErr(impl.range, noOwner "impl" impl.trait_.typ.name)
  | WTSourceFile.Test(path, test) ->
    match path with
    | owner :: modules -> PTest(WT.packageTest owner modules test)
    | [] -> PErr(test.range, noOwner "test" test.name.name)
  | WTSourceFile.Expr(_, e) ->
    PErr(WT.exprRange e, "expressions are not allowed in package files")
  | WTSourceFile.TypeDB(path, t) ->
    match path with
    | _ :: _ -> PDB(path, t)
    | [] -> PErr(t.range, noOwner "DB" t.name.name)
  | WTSourceFile.Assertion(_, t) ->
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
      | PDB(owner :: modules, ({ definition = WT.TDRecord _ } as t)) ->
        Some(WT.packageType owner modules t)
      | _ -> None)
  let values =
    items
    |> List.choose (function
      | PValue v -> Some v
      | _ -> None)
  let traits =
    items
    |> List.choose (function
      | PTrait t -> Some t
      | _ -> None)
  let impls =
    items
    |> List.choose (function
      | PImpl i -> Some i
      | _ -> None)
  let tests =
    items
    |> List.choose (function
      | PTest test -> Some test
      | _ -> None)
  let dbs =
    items
    |> List.choose (function
      | PDB(path, db) -> Some(path, db)
      | _ -> None)
  let errors =
    items
    |> List.choose (function
      | PErr(r, msg) -> Some(r, msg)
      | _ -> None)
  ({ fns = fns
     types = types
     values = values
     traits = traits
     impls = impls
     tests = tests
     dbs = dbs },
   errors)

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
