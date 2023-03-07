#!/usr/bin/dotnet fsi

// This script converts an F# program, passed in as stdin, into a Dark
// _Runtime_ Expr. this will be fully-lowered the runtime types, not program
// types. This can be pasted into LibMiddleware or else where as Dark code.

// Some caveats:
// - it has to fully parse
// - you have to use `let ... in` or else get the indentation exactly right
// - the parses uses Dark ProgramTypes Exprs, so you have to write code like
//   that (eg use `x |> y` instead of `y x`)

// If there's a problem, you need to run:
// $ ./scripts/build/dotnet-regen-fsi
#load "../../.paket/load/net70/main.group.fsx"
#r "../../Build/out/Tests.dll"
#r "../../Build/out/TestUtils.dll"
#r "../../Build/out/Prelude.dll"
#r "../../Build/out/Parser.dll"
#r "../../Build/out/LibBackend.dll"
#r "../../Build/out/LibExecution.dll"
#r "../../Build/out/LibExecutionStdLib.dll"

let input = stdin.ReadToEnd().Split("\n")

let output =
  input
  |> Seq.map (fun x -> $"  {x}")
  |> String.concat "\n"
  |> (fun input -> $"do ({input})")
  |> Parser.Parser.parseRTExpr
  |> LibExecution.Shortcuts.toStringRepr

printfn "%s" output