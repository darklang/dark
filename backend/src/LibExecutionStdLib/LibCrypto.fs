/// <summary>
/// StdLib functions for cryptography
///
/// Computes hashes such as sha256, md5, etc.
/// </summary>
///
/// <remarks>
/// Note: Some of LibCrypto is within LibExecutionStdLib, and some is within
/// BackendOnlyStdLib. This is because only a small set of
/// System.Security.Cryptography functions can be executed on the client:
/// LIGHTTODO
/// https://docs.microsoft.com/en-us/dotnet/core/compatibility/cryptography/5.0/cryptography-apis-not-supported-on-blazor-webassembly
/// LightTODO review this
/// </remarks>
module LibExecutionStdLib.LibCrypto

open System
open System.Security.Cryptography

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "Crypto" "sha256" 0
      parameters = [ Param.make "data" TBytes "" ]
      returnType = TBytes
      description = "Computes the SHA-256 digest of the given <param data>"
      fn =
        (function
        | _, [ DBytes data ] -> SHA256.HashData(ReadOnlySpan data) |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Crypto" "sha384" 0
      parameters = [ Param.make "data" TBytes "" ]
      returnType = TBytes
      description = "Computes the SHA-384 digest of the given <param data>"
      fn =
        (function
        | _, [ DBytes data ] -> SHA384.HashData(ReadOnlySpan data) |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
