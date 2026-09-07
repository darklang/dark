/// Standard libraries for running processes
module Builtins.Cli.Libs.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
open Builtin.Shortcuts
open System.Runtime.InteropServices


let executionOutcomeTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.executionOutcome ())

/// Creates an ExecutionOutcome record
let createExecutionOutcome
  (exitCode : int)
  (stdout : string)
  (stderr : string)
  : Dval =
  let typeName = executionOutcomeTypeName ()
  let fields =
    [ "exitCode", Dval.int (bigint exitCode)
      "stdout", DString stdout
      "stderr", DString stderr ]
  DRecord(typeName, typeName, [], Map fields)

let private outcomeOf (response : Host.Response) : Dval =
  let (exitCode, stdout, stderr) = Host.expectProcessOutcome response
  createExecutionOutcome exitCode stdout stderr


module OS =
  type OS =
    | Linux
    | OSX
    | Windows

  let osTypeName () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.OS.os ())

  let toDT (os : OS) : Dval =
    let (caseName, fields) =
      match os with
      | Linux -> "Linux", []
      | OSX -> "MacOS", []
      | Windows -> "Windows", []

    DEnum(osTypeName (), osTypeName (), [], caseName, fields)

let fns () : List<BuiltInFn> =
  [ { name = fn "cliExecute" 0
      description = "Runs a process; return exitCode, stdout, and stderr"
      typeParams = []
      parameters = [ Param.make "command" TString "The command to execute" ]
      returnType = TCustomType(NR.ok (executionOutcomeTypeName ()), [])
      fn =
        function
        | state, vm, _, [| DString command |] ->
          uply {
            let cmdName, cmdArgs = Host.prepareShellCommand command
            let op = Host.Operation.ProcessRun(cmdName, cmdArgs, None)
            match! PermissionCheck.performHost state vm op with
            | Ok response -> return outcomeOf response
            | Error failure ->
              return
                Exception.raiseInternal
                  "process execution failed"
                  [ "message", failure.message ]
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Process ]
      deprecated = NotDeprecated }


    { name = fn "cliGetOS" 0
      description = "Returns the operating system name (e.g. Windows, OSX, Linux)"
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TypeReference.result (TCustomType(NR.ok (OS.osTypeName ()), [])) TString
      fn =
        function
        | _, _, _, [| DUnit |] ->
          let osTypeRef = KTCustomType(OS.osTypeName (), [])
          let resultOk = Dval.resultOk osTypeRef KTString
          let resultError = Dval.resultError osTypeRef KTString

          if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            OS.Windows |> OS.toDT |> resultOk |> Ply
          else if RuntimeInformation.IsOSPlatform OSPlatform.Linux then
            OS.Linux |> OS.toDT |> resultOk |> Ply
          else if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
            OS.OSX |> OS.toDT |> resultOk |> Ply
          else
            "Unsupported OS" |> DString |> resultError |> Ply

        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Sub-process-spawning and management. The process table is host-owned
    // (LibExecution.HostProcess); guest code sees only opaque handle ids.
    { name = fn "cliSpawnProcess" 0
      description = "Spawns an interactive process and returns a handle ID"
      typeParams = []
      parameters = [ Param.make "command" TString "The command to execute" ]
      returnType = TInt
      fn =
        function
        | state, vm, _, [| DString command |] ->
          uply {
            let cmdName, cmdArgs = Host.prepareShellCommand command
            let op = Host.Operation.ProcessSpawn(cmdName, cmdArgs)
            match! PermissionCheck.performHost state vm op with
            | Ok response ->
              return Dval.int (bigint (Host.expectProcessHandle response))
            | Error failure ->
              return
                Exception.raiseInternal
                  "process spawn failed"
                  [ "message", failure.message ]
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Process ]
      deprecated = NotDeprecated }


    { name = fn "cliProcessIO" 0
      description = "Send input to process and read available output (non-blocking)"
      typeParams = []
      parameters =
        [ Param.make "processId" TInt "The process handle ID"
          Param.make "input" TString "The input to send (empty string to just read)" ]
      returnType =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.executionOutcome ())
        TCustomType(NR.ok typeName, [])
      fn =
        function
        | state, vm, _, [| DInt processIdArg; DString input |] ->
          uply {
            let processId = intToInt64 vm processIdArg
            let op = Host.Operation.ProcessIO(processId, input)
            match! PermissionCheck.performHost state vm op with
            | Ok response -> return outcomeOf response
            | Error failure -> return createExecutionOutcome -1 "" failure.message
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      // Raw integer handles are not transferable authority. Keep this
      // trusted-only until processes are represented by opaque owned handles.
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }


    { name = fn "cliTerminateProcess" 0
      description = "Terminates a spawned process and returns final output"
      typeParams = []
      parameters = [ Param.make "processId" TInt "The process handle ID" ]
      returnType =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.executionOutcome ())
        TCustomType(NR.ok typeName, [])
      fn =
        function
        | state, vm, _, [| DInt processIdArg |] ->
          uply {
            let processId = intToInt64 vm processIdArg
            let op = Host.Operation.ProcessTerminate processId
            match! PermissionCheck.performHost state vm op with
            | Ok response -> return outcomeOf response
            | Error failure -> return createExecutionOutcome -1 "" failure.message
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }


    ]

let builtins () : Builtins = Builtin.make [] (fns ())
