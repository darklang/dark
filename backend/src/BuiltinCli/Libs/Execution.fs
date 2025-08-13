/// Standard libraries for running processes
module BuiltinCli.Libs.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent
open System.IO
open System.Threading

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module PackageIDs = LibExecution.PackageIDs
open Builtin.Shortcuts
open System.Runtime.InteropServices


let executionOutcomeTypeName =
  FQTypeName.fqPackage PackageIDs.Type.Stdlib.Cli.executionOutcome

// Process management for interactive processes
type ProcessInfo =
  { Process : System.Diagnostics.Process
    StandardInput : StreamWriter
    StandardOutput : StreamReader
    StandardError : StreamReader
    mutable OutputBuffer : string
    mutable ErrorBuffer : string }

let processHandles = ConcurrentDictionary<int64, ProcessInfo>()
let mutable nextProcessId = 1L

let getNextProcessId () = System.Threading.Interlocked.Increment(&nextProcessId)

// Helper functions to reduce code duplication

/// Expands environment variables in command strings (currently just $HOME)
let expandEnvironmentVariables (command : string) : string =
  if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
    command
      .Replace("$HOME", System.Environment.GetEnvironmentVariable "USERPROFILE")
      .Replace("${HOME}", System.Environment.GetEnvironmentVariable "USERPROFILE")
  else
    command
      .Replace("$HOME", System.Environment.GetEnvironmentVariable "HOME")
      .Replace("${HOME}", System.Environment.GetEnvironmentVariable "HOME")

/// Prepares command and arguments for cross-platform process execution
let prepareProcessCommand (command : string) : string * string =
  let expandedCommand = expandEnvironmentVariables command
  if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
    "cmd.exe", $"/c {expandedCommand}"
  else if RuntimeInformation.IsOSPlatform OSPlatform.Linux then
    let shell =
      System.Environment.GetEnvironmentVariable "SHELL"
      |> Option.ofObj
      |> Option.defaultValue "/bin/bash"
    shell, $"-c \"{expandedCommand}\""
  else if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
    let shell =
      System.Environment.GetEnvironmentVariable "SHELL"
      |> Option.ofObj
      |> Option.defaultValue "/bin/bash"
    shell, $"-c \"{expandedCommand}\""
  else
    Exception.raiseInternal
      "Executing CLI commands is not supported for your operating system"
      []

/// Creates an ExecutionOutcome record
let createExecutionOutcome
  (exitCode : int64)
  (stdout : string)
  (stderr : string)
  : Dval =
  let typeName = executionOutcomeTypeName
  let fields =
    [ "exitCode", DInt64 exitCode
      "stdout", DString stdout
      "stderr", DString stderr ]
  DRecord(typeName, typeName, [], Map fields)

/// Gracefully terminates a process with cross-platform support
let terminateProcess (proc : System.Diagnostics.Process) : unit =
  if not proc.HasExited then
    try
      if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
        // On Windows, try CloseMainWindow first
        proc.CloseMainWindow() |> ignore<bool>
        if not (proc.WaitForExit 2000) then
          proc.Kill()
          proc.WaitForExit()
      else
        // On Unix systems, send SIGTERM first, then SIGKILL
        proc.Kill() // .NET Kill() sends SIGTERM on Unix
        if not (proc.WaitForExit 3000) then
          // Force kill with SIGKILL if SIGTERM didn't work
          proc.Kill true // true = entireProcessTree
          proc.WaitForExit()
    with _ ->
      // Force kill if graceful termination fails
      try
        proc.Kill()
      with _ ->
        ()

let killAllSpawnedProcesses () =
  let processIds = processHandles.Keys |> Seq.toList
  for processId in processIds do
    match processHandles.TryGetValue processId with
    | true, processInfo ->
      try
        terminateProcess processInfo.Process
        processHandles.TryRemove(processId) |> ignore<bool * ProcessInfo>
      with _ ->
        // Even if process is dead, remove from our tracking
        processHandles.TryRemove processId |> ignore<bool * ProcessInfo>
    | _ -> ()

// Register cleanup handler to kill all processes when application exits
// This prevents orphaned processes if the exe crashes or exits unexpectedly
let mutable cleanupRegistered = false

let registerCleanupHandler () =
  if not cleanupRegistered then
    cleanupRegistered <- true
    System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
      killAllSpawnedProcesses ())
    System.Console.CancelKeyPress.Add(fun _ -> killAllSpawnedProcesses ())


module OS =
  type OS =
    | Linux
    | OSX
    | Windows

  let osTypeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.Cli.OS.os

  let toDT (os : OS) : Dval =
    let (caseName, fields) =
      match os with
      | Linux -> "Linux", []
      | OSX -> "MacOS", []
      | Windows -> "Windows", []

    DEnum(osTypeName, osTypeName, [], caseName, fields)

let fns : List<BuiltInFn> =
  [ { name = fn "cliExecute" 0
      description = "Runs a process; return exitCode, stdout, and stderr"
      typeParams = []
      parameters = [ Param.make "command" TString "The command to execute" ]
      returnType = TCustomType(Ok executionOutcomeTypeName, [])
      fn =
        function
        | _, _, _, [ DString command ] ->
          let cmdName, cmdArgs = prepareProcessCommand command

          let psi =
            System.Diagnostics.ProcessStartInfo(
              command,
              FileName = cmdName,
              Arguments = cmdArgs,
              UseShellExecute = false,
              RedirectStandardOutput = true,
              RedirectStandardError = true,
              CreateNoWindow = true
            )

          let p = System.Diagnostics.Process.Start(psi)

          // TODO: read+return bytes, not strings, and update the corresponding `ExecutionOutcome` type
          // (need an alternative to `p.StandardOutput.ReadToEnd()` here)
          let stdout = p.StandardOutput.ReadToEnd()
          let stderr = p.StandardError.ReadToEnd()

          p.WaitForExit()

          createExecutionOutcome p.ExitCode stdout stderr |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliGetOS" 0
      description = "Returns the operating system name (e.g. Windows, OSX, Linux)"
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.result (TCustomType(Ok OS.osTypeName, [])) TString
      fn =
        function
        | _, _, _, [ DUnit ] ->
          let osTypeRef = KTCustomType(OS.osTypeName, [])
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
      deprecated = NotDeprecated }


    // Sub-process-spawning and management
    { name = fn "cliSpawnProcess" 0
      description = "Spawns an interactive process and returns a handle ID"
      typeParams = []
      parameters = [ Param.make "command" TString "The command to execute" ]
      returnType = TInt64
      fn =
        function
        | _, _, _, [ DString command ] ->
          let cmdName, cmdArgs = prepareProcessCommand command

          let psi =
            System.Diagnostics.ProcessStartInfo(
              FileName = cmdName,
              Arguments = cmdArgs,
              UseShellExecute = false,
              RedirectStandardInput = true,
              RedirectStandardOutput = true,
              RedirectStandardError = true,
              CreateNoWindow = true
            )


          let p = System.Diagnostics.Process.Start(psi)
          let processId = getNextProcessId ()

          // Register cleanup handler for the first process spawned
          registerCleanupHandler ()

          let processInfo =
            { Process = p
              StandardInput = p.StandardInput
              StandardOutput = p.StandardOutput
              StandardError = p.StandardError
              OutputBuffer = ""
              ErrorBuffer = "" }

          processHandles.TryAdd(processId, processInfo) |> ignore<bool>
          DInt64 processId |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliProcessIO" 0
      description = "Send input to process and read available output (non-blocking)"
      typeParams = []
      parameters =
        [ Param.make "processId" TInt64 "The process handle ID"
          Param.make "input" TString "The input to send (empty string to just read)" ]
      returnType =
        let typeName =
          FQTypeName.fqPackage PackageIDs.Type.Stdlib.Cli.executionOutcome
        TCustomType(Ok typeName, [])
      fn =
        function
        | _, _, _, [ DInt64 processId; DString input ] ->
          match processHandles.TryGetValue processId with
          | true, processInfo when not processInfo.Process.HasExited ->
            try
              // Send input if provided
              if input <> "" then
                processInfo.StandardInput.WriteLine(input)
                processInfo.StandardInput.Flush()

              // Wait for output (blocking read until we get some response)
              let stdout = System.Text.StringBuilder()
              let stderr = System.Text.StringBuilder()

              if input <> "" then
                // When we send input, we expect output - wait for the process to respond
                // Use a more robust approach: wait for complete lines of output
                try
                  let mutable attempts = 0
                  let mutable gotCompleteResponse = false

                  while not gotCompleteResponse && attempts < 100 do // Max 10 seconds wait
                    System.Threading.Thread.Sleep(100)
                    attempts <- attempts + 1

                    // Read all available stdout
                    let mutable continueReading = true
                    while continueReading
                          && not processInfo.StandardOutput.EndOfStream do
                      let peek = processInfo.StandardOutput.Peek()
                      if peek >= 0 then
                        let char = processInfo.StandardOutput.Read() |> char
                        stdout.Append(char) |> ignore<System.Text.StringBuilder>
                        // If we got a newline, we might have a complete response
                        if char = '\n' then gotCompleteResponse <- true
                      else
                        continueReading <- false

                    // Read all available stderr
                    while not processInfo.StandardError.EndOfStream do
                      let peek = processInfo.StandardError.Peek()
                      if peek >= 0 then
                        let char = processInfo.StandardError.Read() |> char
                        stderr.Append(char) |> ignore<System.Text.StringBuilder>
                        if char = '\n' then gotCompleteResponse <- true
                      else
                        continueReading <- false

                    // If we have substantial output, consider it complete
                    if stdout.Length > 10 || stderr.Length > 0 then
                      gotCompleteResponse <- true
                with _ ->
                  () // If reading fails, just continue
              else
                // Just reading without sending input - do a quick non-blocking read
                let mutable continueReading = true
                while continueReading && not processInfo.StandardOutput.EndOfStream do
                  let peek = processInfo.StandardOutput.Peek()
                  if peek >= 0 then
                    let char = processInfo.StandardOutput.Read() |> char
                    stdout.Append(char) |> ignore<System.Text.StringBuilder>
                  else
                    continueReading <- false

                continueReading <- true
                while continueReading && not processInfo.StandardError.EndOfStream do
                  let peek = processInfo.StandardError.Peek()
                  if peek >= 0 then
                    let char = processInfo.StandardError.Read() |> char
                    stderr.Append(char) |> ignore<System.Text.StringBuilder>
                  else
                    continueReading <- false

              // Update buffers
              processInfo.OutputBuffer <-
                processInfo.OutputBuffer + stdout.ToString()
              processInfo.ErrorBuffer <- processInfo.ErrorBuffer + stderr.ToString()

              let exitCode =
                if processInfo.Process.HasExited then
                  processInfo.Process.ExitCode
                else
                  0
              createExecutionOutcome exitCode (stdout.ToString()) (stderr.ToString())
              |> Ply
            with ex ->
              createExecutionOutcome -1L "" $"Process IO error: {ex.Message}" |> Ply
          | _ ->
            createExecutionOutcome -1L "" "Process not found or has exited" |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliTerminateProcess" 0
      description = "Terminates a spawned process and returns final output"
      typeParams = []
      parameters = [ Param.make "processId" TInt64 "The process handle ID" ]
      returnType =
        let typeName =
          FQTypeName.fqPackage PackageIDs.Type.Stdlib.Cli.executionOutcome
        TCustomType(Ok typeName, [])
      fn =
        function
        | _, _, _, [ DInt64 processId ] ->
          match processHandles.TryGetValue processId with
          | true, processInfo ->
            try
              let exitCode =
                if not processInfo.Process.HasExited then
                  terminateProcess processInfo.Process
                  processInfo.Process.ExitCode
                else
                  processInfo.Process.ExitCode

              // Read any remaining output
              let remainingStdout =
                try
                  processInfo.StandardOutput.ReadToEnd()
                with _ ->
                  ""
              let remainingStderr =
                try
                  processInfo.StandardError.ReadToEnd()
                with _ ->
                  ""

              let finalStdout = processInfo.OutputBuffer + remainingStdout
              let finalStderr = processInfo.ErrorBuffer + remainingStderr

              processHandles.TryRemove processId |> ignore<bool * ProcessInfo>

              createExecutionOutcome exitCode finalStdout finalStderr |> Ply
            with ex ->
              processHandles.TryRemove processId |> ignore<bool * ProcessInfo>
              createExecutionOutcome
                -1L
                ""
                $"Process termination error: {ex.Message}"
              |> Ply
          | false, _ -> createExecutionOutcome -1L "" "Process not found" |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    ]

let builtins : Builtins = Builtin.make [] fns
