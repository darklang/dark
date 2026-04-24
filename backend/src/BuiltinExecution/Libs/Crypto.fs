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


/// Dereference a DBlob to its bytes via the ExecutionState. Ephemerals
/// hit `blobStore`, persistents route through `state.blobs.get`.
let private readBlob (state : ExecutionState) (ref : BlobRef) : Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral id ->
      let mutable bs : byte[] = null
      if state.blobStore.TryGetValue(id, &bs) then
        return bs
      else
        return Exception.raiseInternal "ephemeral blob not found" [ "id", id ]
    | Persistent(hash, _) ->
      let! got = state.blobs.get hash
      match got with
      | Some bs -> return bs
      | None ->
        return
          Exception.raiseInternal
            "persistent blob missing in package_blobs"
            [ "hash", hash ]
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "cryptoSha256" 0
      typeParams = []
      parameters = [ Param.make "data" TBlob "" ]
      returnType = TBlob
      description = "Computes the SHA-256 digest of the given <param data>"
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! data = readBlob state ref
            let hash = SHA256.HashData(System.ReadOnlySpan(data))
            return Dval.newEphemeralBlob state hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "cryptoSha384" 0
      typeParams = []
      parameters = [ Param.make "data" TBlob "" ]
      returnType = TBlob
      description = "Computes the SHA-384 digest of the given <param data>"
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! data = readBlob state ref
            let hash = SHA384.HashData(System.ReadOnlySpan data)
            return Dval.newEphemeralBlob state hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "cryptoMd5" 0
      typeParams = []
      parameters = [ Param.make "data" TBlob "" ]
      returnType = TBlob
      description =
        "Computes the md5 digest of the given <param data>. NOTE: There are multiple security problems with md5, see https://en.wikipedia.org/wiki/MD5#Security"
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! data = readBlob state ref
            let hash = MD5.HashData(System.ReadOnlySpan data)
            return Dval.newEphemeralBlob state hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "cryptoSha256hmac" 0
      typeParams = []
      parameters = [ Param.make "key" TBlob ""; Param.make "data" TBlob "" ]
      returnType = TBlob
      description =
        "Computes the SHA-256 HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | state, _, _, [ DBlob keyRef; DBlob dataRef ] ->
          uply {
            let! key = readBlob state keyRef
            let! data = readBlob state dataRef
            use hmac = new HMACSHA256(key)
            let hash = hmac.ComputeHash(data)
            return Dval.newEphemeralBlob state hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }


    { name = fn "cryptoSha1hmac" 0
      typeParams = []
      parameters = [ Param.make "key" TBlob ""; Param.make "data" TBlob "" ]
      returnType = TBlob
      description =
        "Computes the SHA1-HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | state, _, _, [ DBlob keyRef; DBlob dataRef ] ->
          uply {
            let! key = readBlob state keyRef
            let! data = readBlob state dataRef
            use hmac = new HMACSHA1(key)
            let hash = hmac.ComputeHash(data)
            return Dval.newEphemeralBlob state hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
