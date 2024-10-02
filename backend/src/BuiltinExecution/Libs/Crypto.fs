/// <summary>
/// Builtin functions for cryptography
///
/// Computes hashes such as sha256, md5, etc.
/// </summary>
module BuiltinExecution.Libs.Crypto

open System.Security.Cryptography

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


let fns : List<BuiltInFn> =
  [ { name = fn "cryptoSha256" 0
      typeParams = []
      parameters = [ Param.make "data" (TList TUInt8) "" ]
      returnType = TList TUInt8
      description = "Computes the SHA-256 digest of the given <param data>"
      fn =
        (function
        | _, _, _, [ DList(_vt, data) ] ->
          let data = Dval.dlistToByteArray data
          let hash = SHA256.HashData(System.ReadOnlySpan(data))
          Dval.byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "cryptoSha384" 0
      typeParams = []
      parameters = [ Param.make "data" (TList TUInt8) "" ]
      returnType = (TList TUInt8)
      description = "Computes the SHA-384 digest of the given <param data>"
      fn =
        (function
        | _, _, _, [ DList(_vt, data) ] ->
          let data = Dval.dlistToByteArray data
          let hash = SHA384.HashData(System.ReadOnlySpan data)
          Dval.byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "cryptoMd5" 0
      typeParams = []
      parameters = [ Param.make "data" (TList TUInt8) "" ]
      returnType = (TList TUInt8)
      description =
        "Computes the md5 digest of the given <param data>. NOTE: There are multiple security problems with md5, see https://en.wikipedia.org/wiki/MD5#Security"
      fn =
        (function
        | _, _, _, [ DList(_vt, data) ] ->
          let data = Dval.dlistToByteArray data
          let hash = MD5.HashData(System.ReadOnlySpan data)
          Dval.byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "cryptoSha256hmac" 0
      typeParams = []
      parameters =
        [ Param.make "key" (TList TUInt8) ""; Param.make "data" (TList TUInt8) "" ]
      returnType = (TList TUInt8)
      description =
        "Computes the SHA-256 HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, _, _, [ DList(_, key); DList(_, data) ] ->
          let key = Dval.dlistToByteArray key
          let hmac = new HMACSHA256(key)
          let data = Dval.dlistToByteArray data
          let hash = hmac.ComputeHash(data)
          Dval.byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "cryptoSha1hmac" 0
      typeParams = []
      parameters =
        [ Param.make "key" (TList TUInt8) ""; Param.make "data" (TList TUInt8) "" ]
      returnType = (TList TUInt8)
      description =
        "Computes the SHA1-HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, _, _, [ DList(_, key); DList(_, data) ] ->
          let key = Dval.dlistToByteArray key
          let hmac = new HMACSHA1(key)
          let data = Dval.dlistToByteArray data
          let hash = hmac.ComputeHash(data)
          Dval.byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
