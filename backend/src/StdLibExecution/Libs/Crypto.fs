/// <summary>
/// StdLib functions for cryptography
///
/// Computes hashes such as sha256, md5, etc.
/// </summary>

module StdLibExecution.Libs.Crypto

open System
open System.Security.Cryptography

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "Crypto" "sha256" 0
      typeParams = []
      parameters = [ Param.make "data" TBytes "" ]
      returnType = TBytes
      description = "Computes the SHA-256 digest of the given <param data>"
      fn =
        (function
        | _, _, [ DBytes data ] ->
          SHA256.HashData(ReadOnlySpan data) |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Crypto" "sha384" 0
      typeParams = []
      parameters = [ Param.make "data" TBytes "" ]
      returnType = TBytes
      description = "Computes the SHA-384 digest of the given <param data>"
      fn =
        (function
        | _, _, [ DBytes data ] ->
          SHA384.HashData(ReadOnlySpan data) |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Crypto" "md5" 0
      typeParams = []
      parameters = [ Param.make "data" TBytes "" ]
      returnType = TBytes
      description =
        "Computes the md5 digest of the given <param data>. NOTE: There are multiple security problems with md5, see https://en.wikipedia.org/wiki/MD5#Security"
      fn =
        (function
        | _, _, [ DBytes data ] -> MD5.HashData(ReadOnlySpan data) |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "Crypto" "sha256hmac" 0
      typeParams = []
      parameters = [ Param.make "key" TBytes ""; Param.make "data" TBytes "" ]
      returnType = TBytes
      description =
        "Computes the SHA-256 HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, _, [ DBytes key; DBytes data ] ->
          let hmac = new HMACSHA256(key)
          data |> hmac.ComputeHash |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "Crypto" "sha1hmac" 0
      typeParams = []
      parameters = [ Param.make "key" TBytes ""; Param.make "data" TBytes "" ]
      returnType = TBytes
      description =
        "Computes the SHA1-HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, _, [ DBytes key; DBytes data ] ->
          let hmac = new HMACSHA1(key)
          data |> hmac.ComputeHash |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
