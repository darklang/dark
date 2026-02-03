/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module BS = LibBinarySerialization.BinarySerialization

module PM = LibPackageManager.PackageManager

open Utils


/// Evaluate all package values that have NULL rt_dval.
/// Called after initial package load when all definitions are in the DB.
let evaluateAllValues
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  : Ply<Result<unit, string list>> =
  uply {
    // Get all value IDs with NULL rt_dval
    let! unevaluatedValues =
      Sql.query
        """
        SELECT id, pt_def
        FROM package_values
        WHERE rt_dval IS NULL
        """
      |> Sql.executeAsync (fun read ->
        let id = read.uuid "id"
        let ptDef = read.bytes "pt_def"
        (id, ptDef))

    if List.isEmpty unevaluatedValues then
      return Ok()
    else
      // Create execution state for evaluation
      let program : RT.Program =
        { canvasID = System.Guid.NewGuid()
          internalFnsAllowed = false
          dbs = Map.empty
          secrets = [] }

      let notify _ _ _ _ = uply { return () }
      let sendException _ _ _ _ = uply { return () }

      let exeState =
        Execution.createState
          builtins
          pm
          Execution.noTracing
          sendException
          notify
          program

      // Evaluate each value
      let errors = ResizeArray<string>()

      for (valueId, ptDefBytes) in unevaluatedValues do
        try
          // Deserialize PT definition
          let ptValue = BS.PT.PackageValue.deserialize valueId ptDefBytes

          // Convert PT expression to RT instructions
          let instrs = PT2RT.Expr.toRT Map.empty 0 None ptValue.body

          // Execute the expression
          let! result = Execution.executeExpr exeState instrs

          match result with
          | Error(rte, _callStack) ->
            errors.Add($"Value {valueId}: evaluation failed - {rte}")
          | Ok dval ->
            // Create the RT value and serialize
            let rtValue : RT.PackageValue.PackageValue =
              { id = valueId; body = dval }
            let rtDvalBytes = BS.RT.PackageValue.serialize valueId rtValue
            let valueType = RT.Dval.toValueType dval
            let valueTypeBytes = BS.RT.ValueType.serialize valueType

            // Store the evaluated result
            do!
              Sql.query
                """
                UPDATE package_values
                SET rt_dval = @rt_dval, value_type = @value_type
                WHERE id = @id
                """
              |> Sql.parameters
                [ "id", Sql.uuid valueId
                  "rt_dval", Sql.bytes rtDvalBytes
                  "value_type", Sql.bytes valueTypeBytes ]
              |> Sql.executeStatementAsync
        with ex ->
          errors.Add($"Value {valueId}: exception - {ex.Message}")

      if errors.Count = 0 then return Ok() else return Error(errors |> List.ofSeq)
  }


module HandleCommand =

  let reloadCanvas (name : string) : Ply<Result<unit, string>> =
    uply {
      print $"Reloading {name} canvas..."

      let! (canvasId, toplevels) =
        Canvas.loadFromDisk
          (LibPackageManager.PackageManager.pt
            LibPackageManager.Branches.mainBranchId)
          name

      print $"Loaded canvas {canvasId} with {List.length toplevels} toplevels"

      return Ok()
    }

  let reloadCanvases () : Ply<Result<unit, string>> =
    uply {
      // CLEANUP fetch the list of canvases by 'ls canvases' equiv.
      // CLEANUP stop tossing the result
      let! _ = reloadCanvas "dark-packages"
      let! _ = reloadCanvas "dark-editor"
      return Ok()
    }

  let reloadPackages () : Ply<Result<unit, string>> =
    uply {
      // Load packages from disk, ensuring all parse well
      let! ops = LoadPackagesFromDisk.load Builtins.all

      // CLEANUP consider checking for duplicates (helps prevent a class of issues)

      print "Purging ..."
      do! LibPackageManager.Purge.purge ()

      print "Filling ..."
      // Create an "init" commit with all packages from disk
      // Note: values are stored with NULL rt_dval at this point
      let! commitId =
        LibPackageManager.Inserts.insertAndApplyOpsWithCommit
          LibPackageManager.Branches.mainBranchId
          "Init: packages loaded from disk"
          ops

      // Evaluate all values now that all definitions are in the DB
      let! evalResult = evaluateAllValues Builtins.all PM.rt
      match evalResult with
      | Error errors ->
        for e in errors do
          print $"  Value evaluation error: {e}"
        return Error "Some values failed to evaluate"
      | Ok() ->
        // Get stats after ops are inserted/applied
        let! stats = LibPackageManager.Stats.get ()
        print "Loaded packages from disk "
        print $"{stats.types} types, {stats.values} values, and {stats.fns} fns"
        print $"Created init commit {commitId}"

        // Reload dark-packages and dark-editor canvases after package reload
        print "Reloading dark-packages canvas..."
        let! _ = reloadCanvas "dark-packages"
        let! _ = reloadCanvas "dark-editor"

        return Ok()
    }

  let runMigrations () : Ply<Result<unit, string>> =
    uply {
      try
        print "Running migrations"
        Migrations.run ()
        print "Migrations completed successfully."
        return Ok()
      with ex ->
        return Error $"Migration failed: {ex.Message}"
    }

  let listMigrations () : Ply<Result<unit, string>> =
    uply {
      try
        print "Migrations needed:\n"
        Migrations.migrationsToRun () |> List.iter (fun name -> print $" - {name}")
        return Ok()
      with ex ->
        return Error $"Failed to list migrations: {ex.Message}"
    }

let initSerializers () =
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageFn.PackageFn>>
    "Parse packageFn list"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageType.PackageType>>
    "Parse packageType list"



[<EntryPoint>]
let main (args : string[]) : int =
  let name = "LocalExec"
  try
    initSerializers ()

    LibService.Init.init name

    //let _ = (LibCloud.Init.init name).Result

    let handleCommand
      (description : string)
      (command : Ply<Result<unit, string>>)
      : int =
      print $"Starting: {description}"
      match command.Result with
      | Ok() ->
        print $"Finished {description}"
        NonBlockingConsole.wait ()
        0
      | Error e ->
        print $"Error {description}:\n{e}"
        NonBlockingConsole.wait ()
        1

    match Array.toList args with
    | [ "reload-packages" ] ->
      handleCommand
        "Reload packages from `packages` directory"
        (HandleCommand.reloadPackages ())

    | [ "reload-canvases" ] ->
      handleCommand
        "Reload canvases from 'canvases' directory"
        (HandleCommand.reloadCanvases ())


    | [ "migrations"; "run" ] ->
      handleCommand
        "deleting database and running all migrations"
        (HandleCommand.runMigrations ())

    | [ "migrations"; "list" ] ->
      handleCommand "listing available migrations" (HandleCommand.listMigrations ())

    | _ ->
      print "Invalid arguments"
      print "Available commands:"
      print "  reload-packages"
      print "  reload-canvases"
      print "  migrations run"
      print "  migrations list"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
