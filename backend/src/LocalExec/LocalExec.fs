/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.LocalExec

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open Utils

module HandleCommand =
  let loadPackagesToInternalSqlTables () : Ply<Result<unit, string>> =
    uply {
      // first, load the packages from disk, ensuring all parse well
      let! packagesFromDisk = LoadPackagesFromDisk.load Builtins.all

      let typeLen = packagesFromDisk.types |> List.length
      let constantLen = packagesFromDisk.constants |> List.length
      let fnLen = packagesFromDisk.fns |> List.length

      print "Loaded packages from disk "
      print $"{typeLen} types, {constantLen} constants, and {fnLen} fns"

      print "Purging ..."
      do! LibCloud.PackageManager.purge ()

      print "Filling ..."
      do! LibCloud.PackageManager.savePackageTypes packagesFromDisk.types
      do! LibCloud.PackageManager.savePackageConstants packagesFromDisk.constants
      do! LibCloud.PackageManager.savePackageFunctions packagesFromDisk.fns

      return Ok()
    }


  let reloadDarkPackagesCanvas () : Ply<Result<unit, string>> =
    uply {
      let! (canvasId, toplevels) =
        Canvas.loadFromDisk LibCloud.PackageManager.pt "dark-packages"

      print $"Loaded canvas {canvasId} with {List.length toplevels} toplevels"

      return Ok()
    }



let initSerializers () =
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageFn.PackageFn>>
    "Parse packageFn list"
  Json.Vanilla.allow<List<LibExecution.ProgramTypes.PackageType.PackageType>>
    "Parse packageType list"



let concatPrompt () : string =
  let allCodeFromFile (filePath : string) : string =
    let fileContents = System.IO.File.ReadAllText filePath
    $"// {filePath}\n\n{fileContents}"

  [ "I'd like help in refactoring some code in a project I'm working on. In this document, I'll:
    - provide a preamble about the project, Darklang
    - describe the specific problems I'm facing
    - provide a rough tree of the project structure, so you know how these things fit together
    - provide a few files along the way, of relevant code
    - ask you for advice on the problems"

    "Darklang is a holistic tool for writing software, including: language, runtime, package manager, editor"
    "Darklang's backend is written in F#, which (as a reminder) is built in-order, file by file. as such there are particular dependencies between files"

    "Core to the project are a few key modules, presented in the order they compile (per the .fsproj):
    - RuntimeTypes.fs, the AST-based representation of the things in the language, _for the interpreter_
    - Interpreter.fs, the interpreter itself, that runs on RuntimeTypes
    - Execution.fs, which largely wraps the interpreter
    - ProgramTypes.fs, an AST representation of the things in Darklang, directly mapping to how a user should think about them
    - ProgramTypesToRuntimeTypes.fs, a mapping from ProgramTypes to RuntimeTypes

    For shorthand, you can refer to some of these as RT, PT, and PT2RT."

    "We've started to face a problem with our AST-based interpreter: stack overflows.
    So, it's time to rewrite the interpreter to be bytecode-based rather than AST-based.
    This will involve a few steps:
    - updating RuntimeTypes quite a bit
    - updating the Interpreter even more
    - updating ProgramTypeToRuntimeTypes appropriately"

    "We have some specific ideas in mind for the intereter:
    - instruction/bytecode-based (let's call them Instructions in RT)
    - use a virtual machine with _registers_
    - don't use globals in the interpreter"

    "I'm going to now include the source of the mentioned files"

    allCodeFromFile "backend/src/LibExecution/RuntimeTypes.fs"
    allCodeFromFile "backend/src/LibExecution/Interpreter.fs"
    allCodeFromFile "backend/src/LibExecution/Execution.fs"
    allCodeFromFile "backend/src/LibExecution/ProgramTypes.fs"
    allCodeFromFile "backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs"

    "Could you provide a sketch of updated RuntimeTypes, Interpreter, and Execution?" ]
  |> String.concat "\n-----\n"



[<EntryPoint>]
let main (args : string[]) : int =
  let name = "LocalExec"
  try
    initSerializers ()

    LibService.Init.init name
    LibService.Telemetry.Console.loadTelemetry
      name
      LibService.Telemetry.DontTraceDBQueries

    let _ = (LibCloud.Init.waitForDB LibCloud.Init.WaitForDB).Result

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
    | [ "load-packages-to-internal-sql-tables" ] ->
      handleCommand
        "reading, parsing packages from `packages` directory, and saving to internal SQL tables"
        (HandleCommand.loadPackagesToInternalSqlTables ())

    | [ "reload-dark-packages" ] ->
      handleCommand
        $"purging, re-creating, and seeding `dark-packages` canvas"
        (HandleCommand.reloadDarkPackagesCanvas ())

    | [ "yolo" ] ->
      System.IO.File.WriteAllText("prompt.txt", concatPrompt ())
      NonBlockingConsole.wait ()
      1
    | _ ->
      print "Invalid arguments"
      NonBlockingConsole.wait ()
      1
  with e ->
    // Don't reraise or report as LocalExec is only run interactively
    printException "Exception" [] e
    // LibService.Init.shutdown name
    NonBlockingConsole.wait ()
    1
