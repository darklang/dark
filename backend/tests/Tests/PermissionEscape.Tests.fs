/// Package values are pure.
///
/// A `val` is evaluated once, when it is saved, by the interpreter, and every reader gets that
/// result. So its evaluation may perform no effect, whoever saves it and whatever the instance
/// policy allows: `val x = <clock read>` would freeze one machine's clock into the store, and
/// `val x = <command>` must not run a command the same expression is denied at `eval`.
module Tests.PermissionEscape

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Inserts = LibDB.Inserts
module Seed = LibDB.Seed
module BS = LibSerialization.Binary.Serialization


// ── Helpers ──────────────────────────────────────────────────────────────

let private loc (name : string) : PT.PackageLocation =
  { owner = "Test"; modules = [ "PermissionEscape" ]; name = name }

let private makeValue (body : PT.Expr) : PT.PackageValue.PackageValue =
  let uniqueHash =
    System.Guid.NewGuid().ToString("N") + System.Guid.NewGuid().ToString("N")
    |> PT.Hash
  { hash = uniqueHash; body = body; description = "" }

/// `Builtin.syscallGettimeofday ()`: the cheapest host effect that still needs a
/// permission, so the test asserts the check and not the cost of the effect.
let private clockCall : PT.Expr =
  eApply (eBuiltinFn "syscallGettimeofday" 0) [] [ eUnit () ]

/// Store a value as WIP so its row exists with `rt_dval` NULL -- the state an
/// imported or folded-in op leaves behind for evaluation to find.
let private storeUnevaluated (name : string) (body : PT.Expr) : Task<PT.Hash> =
  task {
    let value = makeValue body
    let! (_ : int64) =
      Inserts.insertAndApplyOps
        [ PT.PackageOp.AddValue value
          PT.PackageOp.SetName(loc name, PT.PackageValue value.hash, None) ]
    return value.hash
  }

let private isUnevaluated (PT.Hash hash) : Task<bool> =
  Sql.query "SELECT rt_dval IS NULL AS n FROM package_values WHERE hash = @hash"
  |> Sql.parameters [ "hash", Sql.string hash ]
  |> Sql.executeRowAsync (fun read -> read.int64 "n")
  |> Task.map (fun n -> n = 1L)

let private evaluate () : Task<Result<unit, List<Seed.ValueEvaluationError>>> =
  Seed.evaluateAllValues (localBuiltIns pmPT) pmRT

/// Did evaluation report a failure for THIS value? Evaluation sweeps every
/// pending value in the store, so an unrelated one must not decide the test.
let private failureFor
  (hash : PT.Hash)
  (result : Result<unit, List<Seed.ValueEvaluationError>>)
  : Option<string> =
  match result with
  | Ok() -> None
  | Error errors ->
    errors
    |> List.tryFind (fun e -> e.hash = Some hash)
    |> Option.map (fun e -> e.message)

let private refusedAsImpure (what : string) (message : Option<string>) : unit =
  match message with
  | None -> failtest $"{what}: expected the value to be refused"
  | Some message ->
    Expect.stringContains message "permission denied by value" what
    Expect.stringContains message "evaluated once, when it is saved" what


// ── Tests ────────────────────────────────────────────────────────────────

let testEffectIsRefused =
  testTask "value evaluation: a body with an effect is refused, naming the value" {
    let! hash = storeUnevaluated "clock" clockCall
    let! result = evaluate ()
    refusedAsImpure "a clock read" (failureFor hash result)
    Expect.stringContains
      (failureFor hash result |> Option.defaultValue "")
      "Test.PermissionEscape.clock"
      "the message names the value"

    let! stillNull = isUnevaluated hash
    Expect.isTrue
      stillNull
      "a refused value stays unevaluated; it must not be stored as though it ran"
  }

let testPureBodyIsEvaluated =
  testTask "value evaluation: a pure body is evaluated by the interpreter" {
    // The control for the test above. An interpolation evaluates to its text, not to empty
    // text.
    let! hash =
      storeUnevaluated
        "interpolated"
        (eStr [ strText "a"; strInterp (eStr [ strText "b" ]) ])
    let! result = evaluate ()
    Expect.isNone (failureFor hash result) "a pure body evaluates"
    let! (stored : Option<RT.PackageValue.PackageValue>) =
      let (PT.Hash h) = hash
      LibDB.RuntimeTypes.Value.get (RT.Hash h) |> Ply.toTask
    match stored with
    | Some value -> Expect.equal value.body (RT.DString "ab") "evaluated, not folded"
    | None -> failtest "an evaluated value is stored"
  }

let testEffectInCalledFnIsRefused =
  testTask "value evaluation: an effect inside a function the body calls is refused" {
    let clockHash =
      System.Guid.NewGuid().ToString("N") + System.Guid.NewGuid().ToString("N")
    let clockFn : PT.PackageFn.PackageFn =
      { hash = PT.Hash clockHash
        typeParams = []
        parameters =
          NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
        returnType = PT.TBigInt
        body = clockCall
        description = ""
        bounds = [] }
    let! (_ : int64) =
      Inserts.insertAndApplyOps
        [ PT.PackageOp.AddFn clockFn
          PT.PackageOp.SetName(loc "readsClock", PT.PackageFn clockFn.hash, None) ]
    let! hash =
      storeUnevaluated "callsClock" (eApply (ePackageFn clockHash) [] [ eUnit () ])
    let! result = evaluate ()
    refusedAsImpure "a clock read one call down" (failureFor hash result)
    let! stillNull = isUnevaluated hash
    Expect.isTrue stillNull "the value stays unevaluated"
  }

// testSequenced because these share one store, and evaluation sweeps EVERY pending value in
// it -- including ones another test just inserted and is about to assert on.
let tests =
  testSequenced
  <| testList
    "PermissionEscape"
    [ testEffectIsRefused; testPureBodyIsEvaluated; testEffectInCalledFnIsRefused ]
