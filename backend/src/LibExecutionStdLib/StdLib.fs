module LibExecutionStdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes


let fn = FQFnName.stdlibFnName

let renames : List<FQFnName.StdlibFnName * FQFnName.StdlibFnName> =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let types : List<PT.BuiltInType> = [ LibMaybe.types ] |> List.concat

let fns : List<BuiltInFn> =
  [ LibBool.fns
    LibBytes.fns
    LibChar.fns
    LibDateTime.fns
    LibDict.fns
    LibFloat.fns
    LibHttp.fns
    LibHttpClient.fns
    LibHttpClientAuth.fns
    LibJson.fns
    LibMath.fns
    LibUuid.fns
    LibInt.fns
    LibList.fns
    // LibMiddleware.fns
    LibNoModule.fns
    LibMaybe.fns
    LibOption.fns
    LibResult.fns
    LibCrypto.fns
    LibString.fns
    LibTuple2.fns
    LibTuple3.fns ]
  |> List.concat
  |> renameFunctions renames
