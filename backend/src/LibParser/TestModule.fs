module LibParser.TestModule

open Prelude

module WT = WrittenTypes
module WT2PT = WrittenTypesToProgramTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = NameResolver
module P = LibParser.Parser
open LibSerialization.Hashing
module HS = LibDB.HashStabilization


type WTExpected =
  | WTExpectedExpr of WT.Expr
  | WTExpectedError of string
  | WTExpectedSqlError of string

type WTTest =
  { name : string; lineNumber : int; actual : WT.Expr; expected : WTExpected }

type WTModule =
  { name : List<string>
    types : List<WT.PackageType.PackageType>
    values : List<WT.PackageValue.PackageValue>
    dbs : List<WT.DB.T>
    fns : List<WT.PackageFn.PackageFn>
    tests : List<WTTest> }

let emptyWTModule =
  { name = []; types = []; values = []; fns = []; dbs = []; tests = [] }

type PTExpected =
  | PTExpectedExpr of PT.Expr
  | PTExpectedError of string
  | PTExpectedSqlError of string

type PTTest =
  { name : string; lineNumber : int; actual : PT.Expr; expected : PTExpected }

type PTModule =
  { name : List<string>
    ops : List<PT.PackageOp>
    dbs : List<PT.DB.T>
    tests : List<PTTest> }


/// Parse a testfile with the shared parser, then apply Test classification.
/// Produces `WTModule`s for the shared `toPT` pipeline.
let parseFile (owner : string) (source : string) : List<WTModule> =
  match P.parseFor Validation.Test source with
  | Error diagnostics ->
    Exception.raiseInternal
      "test source produced diagnostics"
      [ "diagnostics",
        box (
          diagnostics
          |> List.map (fun d -> $"L{int d.range.start.row + 1}: {d.message}")
        ) ]
  | Ok validated ->
    let sf = Validation.ValidatedSourceFile.toWrittenTypes validated

    let dbFromTypeDecl (t : WT.TypeDecl) : WT.DB.T =
      let typ =
        match t.definition with
        | WT.TDAlias tr -> tr
        | _ ->
          Exception.raiseInternal
            "[<DB>] type must be a type alias"
            [ "name", t.name.name ]
      { name = t.name.name; version = 0; typ = typ }

    let wtTest (test : WT.Test) : WTTest =
      let expected =
        match test.expected with
        | WT.TEExpr e -> WTExpectedExpr e
        | WT.TEError msg -> WTExpectedError(String.normalize msg)
        | WT.TESqlError msg -> WTExpectedSqlError(String.normalize msg)
      { name = "test"
        lineNumber = int test.range.start.row + 1
        actual = test.actual
        expected = expected }

    let rec walk
      (currentModule : List<string>)
      (parentDBs : List<WT.DB.T>)
      (decls : List<WT.Declaration>)
      : List<WTModule> =
      let fns = ResizeArray()
      let values = ResizeArray()
      let types = ResizeArray()
      let dbs = ResizeArray parentDBs // seeded with the parent module's DBs
      let tests = ResizeArray()
      let nested = ResizeArray()
      for d in decls do
        match d with
        | WT.DFunction fn -> fns.Add(WT.packageFn owner currentModule fn)
        | WT.DValue v -> values.Add(WT.packageValue owner currentModule v)
        | WT.DType t -> types.Add(WT.packageType owner currentModule t)
        | WT.DTypeDB t -> dbs.Add(dbFromTypeDecl t)
        | WT.DTest test -> tests.Add(wtTest test)
        // Testfiles evaluate `actual = expected` assertions. A bare expression
        // is a broken assertion, so report it instead of skipping it.
        | WT.DExpr e ->
          Exception.raiseInternal
            "Test case not in format `x = y`"
            [ "line", box ((WT.exprRange e).start.row + 1) ]
        // Recurse where the module appears, passing the DBs accumulated so far
        // so a nested module inherits only the parent's earlier-declared DBs.
        | WT.DModule sub ->
          nested.AddRange(
            walk
              (currentModule @ WT.moduleNameParts sub)
              (List.ofSeq dbs)
              sub.declarations
          )
      { emptyWTModule with
          name = currentModule
          fns = List.ofSeq fns
          values = List.ofSeq values
          types = List.ofSeq types
          dbs = List.ofSeq dbs
          tests = List.ofSeq tests }
      :: List.ofSeq nested

    // Top-level bare expressions land in exprsToEval, not declarations.
    match sf.exprsToEval with
    | e :: _ ->
      Exception.raiseInternal
        "Test case not in format `x = y`"
        [ "line", box ((WT.exprRange e).start.row + 1) ]
    | [] -> walk [] [] sf.declarations


let toPT
  (owner : string)
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (m : WTModule)
  : Ply<PTModule> =
  uply {
    let currentModule = owner :: m.name

    let! typeOps =
      m.types
      |> Ply.List.mapSequentially (fun wtType ->
        uply {
          let! ptType = WT2PT.PackageType.toPT pm onMissing currentModule wtType
          let hash = Hashing.computeTypeHash Hashing.Normal ptType
          return
            [ PT.PackageOp.AddType ptType
              // A parser has no store to ask what this name used to point at, so `previous` is None
              // here and filled in downstream where it IS known.
              PT.PackageOp.SetName(
                WT2PT.PackageType.Name.toLocation wtType.name,
                PT.PackageType hash,
                None
              ) ]
        })
      |> Ply.map List.flatten

    let! valueOps =
      m.values
      |> Ply.List.mapSequentially (fun wtValue ->
        uply {
          let! ptValue =
            WT2PT.PackageValue.toPT builtins pm onMissing currentModule wtValue
          return
            [ PT.PackageOp.AddValue ptValue
              PT.PackageOp.SetName(
                WT2PT.PackageValue.Name.toLocation wtValue.name,
                PT.PackageValue(Hashing.computeValueHash Hashing.Normal ptValue),
                None
              ) ]
        })
      |> Ply.map List.flatten

    let! fnOps =
      m.fns
      |> Ply.List.mapSequentially (fun wtFn ->
        uply {
          let! ptFn = WT2PT.PackageFn.toPT builtins pm onMissing currentModule wtFn
          let hash = Hashing.computeFnHash Hashing.Normal ptFn
          return
            [ PT.PackageOp.AddFn ptFn
              PT.PackageOp.SetName(
                WT2PT.PackageFn.Name.toLocation wtFn.name,
                PT.PackageFn hash,
                None
              ) ]
        })
      |> Ply.map List.flatten

    let! dbs =
      m.dbs |> Ply.List.mapSequentially (WT2PT.DB.toPT pm onMissing currentModule)

    let! (tests : List<PTTest>) =
      m.tests
      |> Ply.List.mapSequentially (fun test ->
        uply {
          let context =
            { WT2PT.Context.currentFnName = None
              WT2PT.Context.argMap = Map.empty
              WT2PT.Context.localBindings = Set.empty }
          let exprToPT = WT2PT.Expr.toPT builtins pm onMissing currentModule context
          let! actual = exprToPT test.actual
          let! expected =
            uply {
              match test.expected with
              | WTExpectedExpr expected ->
                let! expected = exprToPT expected
                return PTExpected.PTExpectedExpr expected
              | WTExpectedError msg -> return PTExpected.PTExpectedError msg
              | WTExpectedSqlError msg -> return PTExpected.PTExpectedSqlError msg
            }
          return
            { PTTest.actual = actual
              expected = expected
              lineNumber = test.lineNumber
              name = test.name }
        })

    let allOps = typeOps @ valueOps @ fnOps

    return { name = m.name; ops = allOps; dbs = dbs; tests = tests }
  }



let parseTestFile
  (owner : string)
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (filename : string)
  : Ply<List<PTModule>> =
  uply {
    let onMissing = NR.OnMissing.Allow

    let modulesWT = filename |> System.IO.File.ReadAllText |> parseFile owner

    // First pass: lower with an empty PM, then compute real SCC-aware hashes.
    let! firstPassModules =
      modulesWT
      |> Ply.List.mapSequentially (
        toPT owner builtins PT.PackageManager.empty onMissing
      )

    let firstPassOps = firstPassModules |> List.collect _.ops

    // Iteratively re-lower until all hashes converge.
    // Same approach as LoadPackagesFromDisk: remapSetNames + computeRealHashes.
    let mutable currentOps = HS.computeRealHashes firstPassOps
    let mutable currentModules = firstPassModules
    // Seed with the first-pass hashes so an already-stable file converges on iter 1.
    let mutable prevHashes = HS.extractAllHashes currentOps
    let mutable converged = false
    let mutable iteration = 0
    let maxIterations = 50
    while not converged && iteration < maxIterations do
      iteration <- iteration + 1
      let enhancedPM = LibDB.PackageManager.withExtraOps pm currentOps
      let! newModules =
        modulesWT
        |> Ply.List.mapSequentially (toPT owner builtins enhancedPM onMissing)
      let newRawOps = newModules |> List.collect _.ops
      let remapped = HS.remapSetNames newRawOps currentOps
      let newOps = HS.computeRealHashes remapped
      let newHashes = HS.extractAllHashes newOps
      converged <- newHashes = prevHashes
      prevHashes <- newHashes
      currentOps <- newOps
      currentModules <- newModules

    // Non-convergence means non-deterministic hashing: a bug, not user error.
    // Fail instead of returning whichever ops the last iteration produced.
    if not converged then
      Exception.raiseInternal
        "test module hashes did not converge"
        [ "filename", filename; "iterations", box maxIterations ]

    // currentModules has correct test expressions (lowered with the converged PM),
    // but its ops are from raw lowering (not through computeRealHashes).
    // Replace each module's ops with the corresponding converged ops.
    // Op count per module is preserved by remapSetNames + computeRealHashes.
    let result, _remaining =
      currentModules
      |> List.mapFold
        (fun opsRemaining m ->
          let count = List.length m.ops
          let (these, rest) = List.splitAt count opsRemaining
          ({ m with ops = these }, rest))
        currentOps

    return result
  }
