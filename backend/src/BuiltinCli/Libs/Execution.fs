/// Standard libraries for running processes
module BuiltinCli.Libs.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts
open System.Runtime.InteropServices


let executionOutcomeTypeName =
  FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Cli" ] "ExecutionOutcome" 0

let fns : List<BuiltInFn> =
  [ { name = fn "cliExecute" 0
      description = "Runs a process; return exitCode, stdout, and stderr"
      typeParams = []
      parameters = [ Param.make "command" TString "The command to execute" ]
      returnType = TCustomType(Ok executionOutcomeTypeName, [])
      fn =
        (function
        | _, _, [ DString command ] ->
          let command =
            command.Replace(
              "$HOME",
              System.Environment.GetEnvironmentVariable "HOME"
            )

          let cmdName, cmdArgs =
            if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
              "cmd.exe", $"/c {command}"
            // TODO: run in whatever the default shell is -- not just bash.
            else if
              RuntimeInformation.IsOSPlatform OSPlatform.Linux
              || RuntimeInformation.IsOSPlatform OSPlatform.OSX
            then
              "/bin/bash", $"-c \"{command}\""
            else
              "Executing CLI commands is not supported for your operating system (Linux, Windows, or Mac not detected)"
              |> raiseString

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

          let typeName = executionOutcomeTypeName
          let fields =
            [ "exitCode", DInt64 p.ExitCode
              "stdout", DString stdout
              "stderr", DString stderr ]
          DRecord(typeName, typeName, [], Map fields) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins : Builtins = Builtin.make [] fns
