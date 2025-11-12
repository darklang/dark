module BuiltinCli.Libs.Terminal

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "cliGetTerminalHeight" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Get the current terminal viewport height in number of lines"
      fn =
        (function
        | _, _, [], [ DUnit ] ->
          uply {
            try
              // First try environment variable (most reliable across different terminals)
              match System.Environment.GetEnvironmentVariable("LINES") with
              | null ->
                // Try Console.WindowHeight
                let height = System.Console.WindowHeight

                // If we get exactly 24, it might be a default value
                // Let's try alternative methods
                if height = 24 then
                  // On Unix systems, try tput command
                  if
                    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.Linux
                    )
                    || System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.OSX
                    )
                  then
                    try
                      let p = new System.Diagnostics.Process()
                      p.StartInfo.FileName <- "/bin/sh"
                      p.StartInfo.Arguments <-
                        "-c \"tput lines 2>/dev/null || echo 24\""
                      p.StartInfo.UseShellExecute <- false
                      p.StartInfo.RedirectStandardOutput <- true
                      p.StartInfo.CreateNoWindow <- true
                      if p.Start() then
                        let output = p.StandardOutput.ReadToEnd().Trim()
                        p.WaitForExit()
                        match System.Int32.TryParse(output) with
                        | true, h when h > 0 -> return DInt64(int64 h)
                        | _ -> return DInt64(int64 height)
                      else
                        return DInt64(int64 height)
                    with _ ->
                      return DInt64(int64 height)
                  else
                    return DInt64(int64 height)
                else
                  return DInt64(int64 height)
              | lines ->
                match System.Int32.TryParse(lines) with
                | true, h when h > 0 -> return DInt64(int64 h)
                | _ -> return DInt64 24L
            with _ ->
              // Fallback if unable to detect terminal size
              return DInt64 24L
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliGetTerminalWidth" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Get the current terminal viewport width in number of columns"
      fn =
        (function
        | _, _, [], [ DUnit ] ->
          uply {
            try
              // First try environment variable (most reliable across different terminals)
              match System.Environment.GetEnvironmentVariable("COLUMNS") with
              | null ->
                // Try Console.WindowWidth
                let width = System.Console.WindowWidth

                // If we get exactly 80, it might be a default value
                // Let's try alternative methods
                if width = 80 then
                  // On Unix systems, try tput command
                  if
                    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.Linux
                    )
                    || System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                      System.Runtime.InteropServices.OSPlatform.OSX
                    )
                  then
                    try
                      let p = new System.Diagnostics.Process()
                      p.StartInfo.FileName <- "/bin/sh"
                      p.StartInfo.Arguments <-
                        "-c \"tput cols 2>/dev/null || echo 80\""
                      p.StartInfo.UseShellExecute <- false
                      p.StartInfo.RedirectStandardOutput <- true
                      p.StartInfo.CreateNoWindow <- true
                      if p.Start() then
                        let output = p.StandardOutput.ReadToEnd().Trim()
                        p.WaitForExit()
                        match System.Int32.TryParse(output) with
                        | true, w when w > 0 -> return DInt64(int64 w)
                        | _ -> return DInt64(int64 width)
                      else
                        return DInt64(int64 width)
                    with _ ->
                      return DInt64(int64 width)
                  else
                    return DInt64(int64 width)
                else
                  return DInt64(int64 width)
              | columns ->
                match System.Int32.TryParse(columns) with
                | true, w when w > 0 -> return DInt64(int64 w)
                | _ -> return DInt64 80L
            with _ ->
              // Fallback if unable to detect terminal size
              return DInt64 80L
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
