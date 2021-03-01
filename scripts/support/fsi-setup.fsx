// This file is used to have a nice FSI setup

// Load program code

printfn "Loading program; if something wrong run ./scripts/support/dotnet-regen-fsi"

#I "../../fsharp-backend" ;;
#I "../../fsharp-backend/Build/out" ;;

#load ".paket/load/net50/main.group.fsx" ;;
#r "Prelude.dll" ;;
#r "LibService.dll" ;;
#r "LibExecution.dll" ;;
#r "LibBackend.dll" ;;
#r "ApiServer.dll" ;;
#r "BwdServer.dll" ;;
#r "Tests.dll" ;;
#r "FuzzTests.dll" ;;

// Convenience shortcuts
module RT = LibExecution.RuntimeTypes ;;
module PT = LibBackend.ProgramTypes ;;
module DvalRepr = LibExecution.DvalRepr ;;
module OCamlInterop = LibBackend.OCamlInterop ;;

printfn "Loaded and ready to go"