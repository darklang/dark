// This file is used to have a nice FSI setup

// Load program code

printfn "Loading program; if something wrong run ./scripts/build/dotnet-regen-fsi"

#I "../../backend"
#I "../../backend/.paket/"
#I "../../backend/Build/out"

#load ".paket/load/net7.0/main.group.fsx"
#r "Tablecloth/Debug/net7.0/Tablecloth.dll"
#r "Prelude/Debug/net7.0/Prelude.dll"
#r "LibService/Debug/net7.0/LibService.dll"
#r "LibExecution/Debug/net7.0/linux-x64/LibExecution.dll"
#r "LibBackend/Debug/net7.0/LibBackend.dll"
#r "BwdServer/Debug/net7.0/linux-x64/BwdServer.dll"
#r "TestUtils/Debug/net7.0/TestUtils.dll"
#r "Tests/Debug/net7.0/linux-x64/Tests.dll"
#r "BackendOnlyStdLib/Debug/net7.0/BackendOnlyStdLib.dll"
// #r "ProdExec/Debug/net7.0/ProdExec.dll"
#r "HttpMiddleware/Debug/net7.0/HttpMiddleware.dll"
#r "LibAnalysis/Debug/net7.0/linux-x64/LibAnalysis.dll"
#r "LibBinarySerialization/Debug/net7.0/LibBinarySerialization.dll"
#r "LibExecutionStdLib/Debug/net7.0/LibExecutionStdLib.dll"
#r "LibRealExecution/Debug/net7.0/LibRealExecution.dll"
// #r "QueueWorker/Debug/net7.0/QueueWorker.dll"

// Convenience shortcuts
module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open Prelude
//open FSI

printfn "Loaded and ready to go"