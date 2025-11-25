/// Standard libraries for process management
module BuiltinCli.Libs.Process

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let fns : List<BuiltInFn> =
  [ { name = fn "processSpawnBackground" 0
      typeParams = []
      parameters =
        [ Param.make "args" (TList TString) "Arguments to pass to the CLI" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Spawns the current CLI executable in the background with the given arguments. Returns the process ID (PID) on success."
      fn =
        (function
        | _state, _, _, [ DList(_vtTODO, args) ] ->
          uply {
            try
              let argStrings =
                args
                |> List.map (fun arg ->
                  match arg with
                  | DString s -> s
                  | _ -> Exception.raiseInternal "Expected string arguments" [])

              // Get the current executable path
              let currentExe =
                System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName

              let psi = System.Diagnostics.ProcessStartInfo()
              psi.FileName <- currentExe
              psi.UseShellExecute <- false
              psi.CreateNoWindow <- true
              // Redirect to prevent inheriting parent's streams
              psi.RedirectStandardOutput <- true
              psi.RedirectStandardError <- true
              psi.RedirectStandardInput <- true

              // Add arguments
              for arg in argStrings do
                psi.ArgumentList.Add(arg)

              let proc = System.Diagnostics.Process.Start(psi)

              if isNull proc then
                return
                  Dval.resultError
                    KTInt64
                    KTString
                    (DString "Failed to start background process")
              else
                return Dval.resultOk KTInt64 KTString (DInt64(int64 proc.Id))
            with ex ->
              return
                Dval.resultError
                  KTInt64
                  KTString
                  (DString $"Error spawning process: {ex.Message}")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "processGetPid" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the current process ID (PID)."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let pid = System.Diagnostics.Process.GetCurrentProcess().Id
            return DInt64(int64 pid)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "processIsRunning" 0
      typeParams = []
      parameters = [ Param.make "pid" TInt64 "Process ID to check" ]
      returnType = TBool
      description = "Checks if a process with the given PID is currently running."
      fn =
        (function
        | _, _, _, [ DInt64 pid ] ->
          uply {
            try
              let proc = System.Diagnostics.Process.GetProcessById(int pid)
              let isRunning = not proc.HasExited
              return DBool isRunning
            with
            | :? System.ArgumentException

            | :? System.InvalidOperationException ->
              // Process doesn't exist or has exited
              return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "processKill" 0
      typeParams = []
      parameters = [ Param.make "pid" TInt64 "Process ID to kill" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Kills the process with the given PID. Returns unit on success, or an error message on failure."
      fn =
        (function
        | _state, _, _, [ DInt64 pid ] ->
          uply {
            try
              let proc = System.Diagnostics.Process.GetProcessById(int pid)
              proc.Kill()
              proc.WaitForExit(5000) |> ignore<bool>
              return Dval.resultOk KTUnit KTString DUnit
            with
            | :? System.ArgumentException ->
              return Dval.resultError KTUnit KTString (DString "Process not found")
            | ex ->
              return
                Dval.resultError
                  KTUnit
                  KTString
                  (DString $"Error killing process: {ex.Message}")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = Builtin.make [] fns
