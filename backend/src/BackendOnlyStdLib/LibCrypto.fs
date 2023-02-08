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
module BackendOnlyStdLib.LibCrypto

open System
open System.Security.Cryptography

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ { name = fn "Crypto" "md5" 0
      parameters = [ Param.make "data" TBytes "" ]
      returnType = TBytes
      description =
        "Computes the md5 digest of the given <param data>. NOTE: There are multiple security problems with md5, see https://en.wikipedia.org/wiki/MD5#Security"
      fn =
        (function
        | _, [ DBytes data ] -> MD5.HashData(ReadOnlySpan data) |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "Crypto" "sha256hmac" 0
      parameters = [ Param.make "key" TBytes ""; Param.make "data" TBytes "" ]
      returnType = TBytes
      description =
        "Computes the SHA-256 HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, [ DBytes key; DBytes data ] ->
          let hmac = new HMACSHA256(key)
          data |> hmac.ComputeHash |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "Crypto" "sha1hmac" 0
      parameters = [ Param.make "key" TBytes ""; Param.make "data" TBytes "" ]
      returnType = TBytes
      description =
        "Computes the SHA1-HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, [ DBytes key; DBytes data ] ->
          let hmac = new HMACSHA1(key)
          data |> hmac.ComputeHash |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated } ]
