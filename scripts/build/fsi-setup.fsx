// This file is used to have a nice FSI setup

// Load program code

printfn "Loading program; if something wrong run ./scripts/build/dotnet-regen-fsi"

#I "../../backend"
#I "../../backend/.paket/"
#I "../../backend/Build/out"

#load ".paket/load/net8.0/main.group.fsx"
#r "Prelude/Debug/net8.0/Prelude.dll"
#r "LibService/Debug/net8.0/LibService.dll"
#r "LibExecution/Debug/net8.0/linux-x64/LibExecution.dll"
#r "LibCloud/Debug/net8.0/LibCloud.dll"
#r "BwdServer/Debug/net8.0/linux-x64/BwdServer.dll"
#r "TestUtils/Debug/net8.0/TestUtils.dll"
#r "Tests/Debug/net8.0/linux-x64/Tests.dll"
#r "BuiltinCloudExecution/Debug/net8.0/BuiltinCloudExecution.dll"
#r "BuiltinDarkInternal/Debug/net8.0/BuiltinDarkInternal.dll"
// #r "ProdExec/Debug/net8.0/ProdExec.dll"
#r "LibHttpMiddleware/Debug/net8.0/LibHttpMiddleware.dll"
#r "LibBinarySerialization/Debug/net8.0/LibBinarySerialization.dll"
#r "BuiltinExecution/Debug/net8.0/BuiltinExecution.dll"
#r "LibCloudExecution/Debug/net8.0/LibCloudExecution.dll"
// #r "QueueWorker/Debug/net8.0/QueueWorker.dll"

// Convenience shortcuts
module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open Prelude
//open FSI

printfn "Loaded and ready to go"