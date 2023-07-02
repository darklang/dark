/// Standard libraries for running processes
module StdLibCli.Libs.Process

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module StdLib = LibExecution.StdLib
open StdLib.Shortcuts

let types : List<BuiltInType> =
  [ { name = typ [ "Process" ] "Result" 0
      description = "An error that occurred while running a process."
      typeParams = []
      definition =
        TypeDeclaration.Record(
          { name = "exitCode"; typ = TInt; description = "The exit code" },
          [ { name = "stdout"; typ = TBytes; description = "The stdout" }
            { name = "stderr"; typ = TBytes; description = "The stderr" } ]
        )
      deprecated = NotDeprecated } ]

let fns : List<BuiltInFn> =
  [ { name = fn [ "Process" ] "run" 0
      description = "Runs a process, return exitCode, stdout and stderr"
      typeParams = []
      parameters =
        [ Param.make "command" TString "The command to run"
          Param.make "input" TString "The input to the command" ]
      returnType = stdlibTypeRef [ "Process" ] "Result" 0
      fn =
        (function
        | _, _, [ DString command ] ->
          let psi =
            System.Diagnostics.ProcessStartInfo(command)
            |> fun psi ->
              psi.UseShellExecute <- false
              psi.RedirectStandardOutput <- true
              psi.RedirectStandardError <- true
              psi.CreateNoWindow <- true
              psi

          let p = System.Diagnostics.Process.Start(psi)

          let stdout = p.StandardOutput.ReadToEnd()
          let stderr = p.StandardError.ReadToEnd()

          p.WaitForExit()

          DRecord(
            TypeName.fqBuiltIn [ "Process" ] "Error" 0,
            Map
              [ ("exitCode", DInt(p.ExitCode))
                ("stdout", DString(stdout))
                ("stderr", DString(stderr)) ]
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : StdLib.Contents = (fns, types)
