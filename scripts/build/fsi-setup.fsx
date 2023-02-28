// This file is used to have a nice FSI setup

// Load program code

printfn "Loading program; if something wrong run ./scripts/build/dotnet-regen-fsi"

#I "../../backend"
#I "../../backend/.paket/"
#I "../../backend/Build/out"

#load ".paket/load/net6.0/main.group.fsx"
#r "Tablecloth/Debug/net6.0/Tablecloth.dll"
#r "Prelude/Debug/net6.0/Prelude.dll"
#r "LibService/Debug/net6.0/LibService.dll"
#r "LibExecution/Debug/net6.0/linux-x64/LibExecution.dll"
#r "LibBackend/Debug/net6.0/LibBackend.dll"
#r "BwdServer/Debug/net6.0/linux-x64/BwdServer.dll"
#r "TestUtils/Debug/net6.0/TestUtils.dll"
#r "Tests/Debug/net6.0/linux-x64/Tests.dll"
#r "FuzzTests/Debug/net6.0/FuzzTests.dll"
#r "BackendOnlyStdLib/Debug/net6.0/BackendOnlyStdLib.dll"
// #r "ExecHost/Debug/net6.0/ExecHost.dll"
#r "HttpMiddleware/Debug/net6.0/HttpMiddleware.dll"
#r "LibAnalysis/Debug/net6.0/linux-x64/LibAnalysis.dll"
#r "LibBinarySerialization/Debug/net6.0/LibBinarySerialization.dll"
#r "LibExecutionStdLib/Debug/net6.0/LibExecutionStdLib.dll"
#r "LibRealExecution/Debug/net6.0/LibRealExecution.dll"
// #r "QueueWorker/Debug/net6.0/QueueWorker.dll"

// Convenience shortcuts
module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open Prelude
//open FSI

printfn "Loaded and ready to go"
