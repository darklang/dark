/// Standard libraries for running processes
module BuiltinCli.Libs.Process

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts
open System.Runtime.InteropServices

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "Process" ] "run" 0
      description = "Runs a process, return exitCode, stdout and stderr"
      typeParams = []
      parameters = [ Param.make "command" TString "The command to run" ]
      returnType =
        TCustomType(
          Ok(TypeName.fqPackage "Darklang" [ "Stdlib"; "Process" ] "Result" 0),
          []
        )
      fn =
        (function
        | _, _, [ DString command ] ->
          let cmdName, cmdArgs =
            if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
              "cmd.exe", $"/c {command}"
            else if
              RuntimeInformation.IsOSPlatform OSPlatform.Linux
              || RuntimeInformation.IsOSPlatform OSPlatform.OSX
            then
              "/bin/bash", $"-c \"{command}\""
            else
              raiseString
                "Process.run not supported for your operating system (Linux, Windows, or Mac not detected)"

          let psi =
            System.Diagnostics.ProcessStartInfo(command)
            |> fun psi ->
              psi.FileName <- cmdName
              psi.Arguments <- cmdArgs
              psi.UseShellExecute <- false
              psi.RedirectStandardOutput <- true
              psi.RedirectStandardError <- true
              psi.CreateNoWindow <- true
              psi

          let p = System.Diagnostics.Process.Start(psi)

          // TODO: read+return bytes, not strings, and update the corresponding `Process.Result` type
          // (need an alternative to `p.StandardOutput.ReadToEnd()` here)
          let stdout = p.StandardOutput.ReadToEnd()
          let stderr = p.StandardError.ReadToEnd()

          p.WaitForExit()

          let typeName =
            TypeName.fqPackage "Darklang" [ "Stdlib"; "Process" ] "Result" 0
          let fields =
            [ "exitCode", DInt64 p.ExitCode
              "stdout", DString stdout
              "stderr", DString stderr ]
          DRecord(typeName, typeName, [], Map fields) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents : Builtin.Contents = (fns, types, constants)
