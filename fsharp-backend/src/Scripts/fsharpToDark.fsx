#!/usr/bin/dotnet fsi

#load "../../.paket/load/main.group.fsx"
#r "../../Build/out/Tests.dll"
#r "../../Build/out/LibBackend.dll"
#r "../../Build/out/LibExecution.dll"

let input = stdin.ReadToEnd().Split("\n")

let output =
  input
  |> Seq.map (fun x -> $"  {x}")
  |> String.concat "\n"
  |> (fun input -> $"do ({input})")
  |> FSharpToExpr.parseDarkExpr
  |> fun x -> x.toRuntimeType ()
  |> LibExecution.RuntimeTypes.Shortcuts.toStringRepr

printfn "%s" output
