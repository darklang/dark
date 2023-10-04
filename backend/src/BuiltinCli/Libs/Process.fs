/// Standard libraries for running processes
module BuiltinCli.Libs.Process

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "Process" ] "run" 0
      description = "Runs a process, return exitCode, stdout and stderr"
      typeParams = []
      parameters =
        [ Param.make "command" TString "The command to run"
          Param.make "input" TString "The input to the command" ]
      returnType =
        TCustomType(
          Ok(
            FQName.Package
              { owner = "Darklang"
                modules = [ "Process" ]
                name = TypeName.TypeName "Result"
                version = 0 }
          ),
          []
        )
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

          let typeName = TypeName.fqPackage "Darklang" [ "Process" ] "Result" 0
          let fields =
            [ "exitCode", DInt p.ExitCode
              "stdout", DString stdout
              "stderr", DString stderr ]
          DRecord(typeName, typeName, [], Map fields) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents : Builtin.Contents = (fns, types, constants)
