/// <summary>
/// Builtin functions for cryptography
///
/// Computes hashes such as sha256, md5, etc.
/// </summary>
module Builtins.Pure.Libs.Crypto

open System.Security.Cryptography

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Blob = LibExecution.Blob


let fns () : List<BuiltInFn> =
  [ { name = fn "cryptoSha256" 0
      typeParams = []
      parameters = [ Param.make "data" TBlob "" ]
      returnType = TBlob
      description = "Computes the SHA-256 digest of the given <param data>"
      fn =
        (function
        | state, _, _, [| DBlob ref |] ->
          uply {
            let! data = Blob.readBytes state ref
            let hash = SHA256.HashData(System.ReadOnlySpan(data))
            return Blob.newEphemeral hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // A secret compared with `==` leaks how many leading bytes matched, through how long the
    // comparison took. `CryptographicOperations.FixedTimeEquals` takes the same time whatever the
    // inputs. The lengths are compared first and in the open: the length of a secret is not the secret.
    { name = fn "cryptoConstantTimeEquals" 0
      typeParams = []
      parameters =
        [ Param.make "presented" TString "what the caller sent"
          Param.make "expected" TString "what it should be" ]
      returnType = TBool
      description =
        "Whether <param presented> equals <param expected>, in time that does not depend on where they first differ. For comparing a secret or a token against a request."
      fn =
        (function
        | _, _, _, [| DString presented; DString expected |] ->
          let a = System.Text.Encoding.UTF8.GetBytes presented
          let b = System.Text.Encoding.UTF8.GetBytes expected
          if a.Length <> b.Length then
            Ply(DBool false)
          else
            Ply(
              DBool(
                CryptographicOperations.FixedTimeEquals(
                  System.ReadOnlySpan(a),
                  System.ReadOnlySpan(b)
                )
              )
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cryptoSha384" 0
      typeParams = []
      parameters = [ Param.make "data" TBlob "" ]
      returnType = TBlob
      description = "Computes the SHA-384 digest of the given <param data>"
      fn =
        (function
        | state, _, _, [| DBlob ref |] ->
          uply {
            let! data = Blob.readBytes state ref
            let hash = SHA384.HashData(System.ReadOnlySpan data)
            return Blob.newEphemeral hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "cryptoMd5" 0
      typeParams = []
      parameters = [ Param.make "data" TBlob "" ]
      returnType = TBlob
      description =
        "Computes the md5 digest of the given <param data>. NOTE: There are multiple security problems with md5, see https://en.wikipedia.org/wiki/MD5#Security"
      fn =
        (function
        | state, _, _, [| DBlob ref |] ->
          uply {
            let! data = Blob.readBytes state ref
            let hash = MD5.HashData(System.ReadOnlySpan data)
            return Blob.newEphemeral hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "cryptoSha256hmac" 0
      typeParams = []
      parameters = [ Param.make "key" TBlob ""; Param.make "data" TBlob "" ]
      returnType = TBlob
      description =
        "Computes the SHA-256 HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | state, _, _, [| DBlob keyRef; DBlob dataRef |] ->
          uply {
            let! key = Blob.readBytes state keyRef
            let! data = Blob.readBytes state dataRef
            use hmac = new HMACSHA256(key)
            let hash = hmac.ComputeHash(data)
            return Blob.newEphemeral hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "cryptoSha1hmac" 0
      typeParams = []
      parameters = [ Param.make "key" TBlob ""; Param.make "data" TBlob "" ]
      returnType = TBlob
      description =
        "Computes the SHA1-HMAC (hash-based message authentication code) digest of the given <param key> and <param data>."
      fn =
        (function
        | state, _, _, [| DBlob keyRef; DBlob dataRef |] ->
          uply {
            let! key = Blob.readBytes state keyRef
            let! data = Blob.readBytes state dataRef
            use hmac = new HMACSHA1(key)
            let hash = hmac.ComputeHash(data)
            return Blob.newEphemeral hash
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = ImpurePreviewable
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
