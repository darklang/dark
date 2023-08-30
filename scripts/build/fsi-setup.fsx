// This file is used to have a nice FSI setup

// Load program code

printfn "Loading program; if something wrong run ./scripts/build/dotnet-regen-fsi"

#I "../../backend"
#I "../../backend/.paket/"
#I "../../backend/Build/out"

#load ".paket/load/net7.0/main.group.fsx"
#r "Prelude/Debug/net7.0/Prelude.dll"
#r "LibService/Debug/net7.0/LibService.dll"
#r "LibExecution/Debug/net7.0/linux-x64/LibExecution.dll"
#r "LibCloud/Debug/net7.0/LibCloud.dll"
#r "BwdServer/Debug/net7.0/linux-x64/BwdServer.dll"
#r "TestUtils/Debug/net7.0/TestUtils.dll"
#r "Tests/Debug/net7.0/linux-x64/Tests.dll"
#r "StdLibCloudExecution/Debug/net7.0/StdLibCloudExecution.dll"
#r "StdLibDarkInternal/Debug/net7.0/StdLibDarkInternal.dll"
// #r "ProdExec/Debug/net7.0/ProdExec.dll"
#r "LibHttpMiddleware/Debug/net7.0/LibHttpMiddleware.dll"
#r "LibBinarySerialization/Debug/net7.0/LibBinarySerialization.dll"
#r "StdLibExecution/Debug/net7.0/StdLibExecution.dll"
#r "LibCloudExecution/Debug/net7.0/LibCloudExecution.dll"
// #r "QueueWorker/Debug/net7.0/QueueWorker.dll"

// Convenience shortcuts
module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open Prelude
//open FSI

printfn "Loaded and ready to go"