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

module VT = ValueType

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Crypto" ]


let byteArrayToDvalList (bytes : byte[]) : Dval =
  bytes
  |> Array.toList
  |> List.map (fun b -> DUInt8(uint8 b))
  |> fun dvalList -> DList(VT.uint8, dvalList)

let DlistToByteArray (dvalList : List<Dval>) : byte[] =
  dvalList
  |> List.map (fun dval ->
    match dval with
    | DUInt8 b -> b
    | _ -> (Exception.raiseInternal "Invalid type in byte list") [])
  |> Array.ofList


let fns : List<BuiltInFn> =
  [ { name = fn "sha256" 0
      typeParams = []
      parameters = [ Param.make "data" (TList(TUInt8)) "" ]
      returnType = (TList(TUInt8))
      description = "Computes the SHA-256 digest of the given <param data>"
      fn =
        (function
        | _, _, [ DList(_vt, data) ] ->
          let data = DlistToByteArray data
          let hash = SHA256.HashData(System.ReadOnlySpan<byte>(data))
          byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sha384" 0
      typeParams = []
      parameters = [ Param.make "data" (TList(TUInt8)) "" ]
      returnType = (TList(TUInt8))
      description = "Computes the SHA-384 digest of the given <param data>"
      fn =
        (function
        | _, _, [ DList(_vt, data) ] ->
          let data = DlistToByteArray data
          let hash = SHA384.HashData(System.ReadOnlySpan data)
          byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "md5" 0
      typeParams = []
      parameters = [ Param.make "data" (TList(TUInt8)) "" ]
      returnType = (TList(TUInt8))
      description =
        "Computes the md5 digest of the given <param data>. NOTE: There are multiple security problems with md5, see https://en.wikipedia.org/wiki/MD5#Security"
      fn =
        (function
        | _, _, [ DList(_vt, data) ] ->
          let data = DlistToByteArray data
          let hash = MD5.HashData(System.ReadOnlySpan data)
          byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "sha256hmac" 0
      typeParams = []
      parameters =
        [ Param.make "key" (TList(TUInt8)) ""; Param.make "data" (TList(TUInt8)) "" ]
      returnType = (TList(TUInt8))
      description =
        "Computes the SHA-256 HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, _, [ DList(_, key); DList(_, data) ] ->
          let key = DlistToByteArray key
          let hmac = new HMACSHA256(key)
          let data = DlistToByteArray data
          let hash = hmac.ComputeHash(data)
          byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "sha1hmac" 0
      typeParams = []
      parameters =
        [ Param.make "key" (TList(TUInt8)) ""; Param.make "data" (TList(TUInt8)) "" ]
      returnType = (TList(TUInt8))
      description =
        "Computes the SHA1-HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | _, _, [ DList(_, key); DList(_, data) ] ->
          let key = DlistToByteArray key
          let hmac = new HMACSHA1(key)
          let data = DlistToByteArray data
          let hash = hmac.ComputeHash(data)
          byteArrayToDvalList hash |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
