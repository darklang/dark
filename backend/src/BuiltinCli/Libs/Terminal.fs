module BuiltinCli.Libs.Terminal

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


// Cache terminal dimensions — refreshed at most every 500ms
let mutable private cachedWidth : int64 = 0L
let mutable private cachedHeight : int64 = 0L
let mutable private lastDimensionCheck : int64 = 0L
let private dimensionCacheMs = 500L

let private shouldRefreshDimensions () =
  let now =
    System.Diagnostics.Stopwatch.GetTimestamp() * 1000L
    / System.Diagnostics.Stopwatch.Frequency
  if now - lastDimensionCheck > dimensionCacheMs then
    lastDimensionCheck <- now
    true
  else
    false

let private isUnix () =
  System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
    System.Runtime.InteropServices.OSPlatform.Linux
  )
  || System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
    System.Runtime.InteropServices.OSPlatform.OSX
  )

/// Try to get a terminal dimension via tput, falling back to the given default.
let private tputDimension (tputArg : string) (fallback : int) : int =
  try
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- "/bin/sh"
    p.StartInfo.Arguments <- $"-c \"tput {tputArg} 2>/dev/null || echo {fallback}\""
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.CreateNoWindow <- true
    if p.Start() then
      let output = p.StandardOutput.ReadToEnd().Trim()
      p.WaitForExit()
      match System.Int32.TryParse(output) with
      | true, v when v > 0 -> v
      | _ -> fallback
    else
      fallback
  with _ ->
    fallback

/// Get a terminal dimension with caching: check env var, then Console, then tput.
let private getDimension
  (envVar : string)
  (consoleFn : unit -> int)
  (tputArg : string)
  (defaultVal : int)
  (cached : byref<int64>)
  : int64 =
  if not (shouldRefreshDimensions ()) && cached > 0L then
    cached
  else
    let result =
      try
        match System.Environment.GetEnvironmentVariable(envVar) with
        | null ->
          let consoleVal = consoleFn ()
          if consoleVal = defaultVal && isUnix () then
            tputDimension tputArg defaultVal |> int64
          else
            int64 consoleVal
        | envVal ->
          match System.Int32.TryParse(envVal) with
          | true, v when v > 0 -> int64 v
          | _ -> int64 defaultVal
      with _ ->
        int64 defaultVal
    cached <- result
    result


let fns () : List<BuiltInFn> =
  [ { name = fn "cliGetTerminalHeight" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Get the current terminal viewport height in number of lines"
      fn =
        (function
        | _, _, [], [ DUnit ] ->
          DInt64(
            getDimension
              "LINES"
              (fun () -> System.Console.WindowHeight)
              "lines"
              24
              &cachedHeight
          )
          |> Ply
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
          DInt64(
            getDimension
              "COLUMNS"
              (fun () -> System.Console.WindowWidth)
              "cols"
              80
              &cachedWidth
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliGetLogDir" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the absolute path to the CLI log directory"
      fn =
        (function
        | _, _, [], [ DUnit ] -> DString(LibConfig.Config.logDir) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
