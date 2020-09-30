#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.DotNet
open Fake.IO

Target.initEnvironment ()

let libExecutionPath = Path.getFullName "./src/LibExecution"
let serverPath = Path.getFullName "./src/Server"
let deployDir = Path.getFullName "./deploy"
let libExecutionTestsPath = Path.getFullName "./tests/LibExecution"
let serverTestsPath = Path.getFullName "./tests/Server"

let npm args workingDir =
  let npmPath =
    match ProcessUtils.tryFindFileOnPath "npm" with
    | Some path -> path
    | None ->
        "npm was not found in path. Please install it and make sure it's available from your path. "
        + "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        |> failwith

  let arguments =
    args |> String.split ' ' |> Arguments.OfArgs

  Command.RawCommand(npmPath, arguments)
  |> CreateProcess.fromCommand
  |> CreateProcess.withWorkingDirectory workingDir
  |> CreateProcess.ensureExitCode
  |> Proc.run
  |> ignore

let dotnet cmd workingDir =
  let result =
    DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""

  if result.ExitCode <> 0
  then failwithf "'dotnet %s' failed in %s: %s" cmd workingDir (result.ToString())

Target.create "Clean" (fun _ -> Shell.cleanDir deployDir)

Target.create "InstallClient" (fun _ -> npm "install" ".")

Target.create "Bundle" (fun _ ->
  dotnet (sprintf "publish -c Release -o \"%s\"" deployDir) serverPath
  npm "run build" ".")

Target.create "Run" (fun _ ->
  dotnet "build" libExecutionPath
  [ async { dotnet "watch run" serverPath }
    async { npm "run start" "." } ]
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore)

Target.create "RunTests" (fun _ ->
  dotnet "build" libExecutionTestsPath
  [ async { dotnet "watch run" serverTestsPath }
    async { npm "run test:live" "." } ]
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore)

open Fake.Core.TargetOperators

"Clean" ==> "InstallClient" ==> "Bundle"

"Clean" ==> "InstallClient" ==> "Run"

"Clean" ==> "InstallClient" ==> "RunTests"

Target.runOrDefaultWithArguments "Bundle"
