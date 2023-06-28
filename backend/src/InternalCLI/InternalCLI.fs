/// Run scripts locally using some builtin F#/dotnet libraries
module InternalCLI.InternalCLI

open Prelude
open Tablecloth

let initSerializers () = ()

[<EntryPoint>]
let main (args : string[]) : int =
  let name = "InternalCLI"
  initSerializers ()

  // Init LibService and LibBackend (needed for package-management)
  LibService.Init.init name
  LibService.Telemetry.Console.loadTelemetry
    name
    LibService.Telemetry.DontTraceDBQueries
  (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result

  // run the CLI
  let config : LibCLI.Main.DarkCLIConfig =
    { name = "Darklang Internal CLI"
      extraStdlibForUserPrograms =
        LibExecution.StdLib.combine
          [ InternalCLI.StdLib.contents; StdLibDarkInternal.StdLib.contents ]
          []
          []
      allowInternalDarkFunctions = true }

  LibCLI.Main.main config args
