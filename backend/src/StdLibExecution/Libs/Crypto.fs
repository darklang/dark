/// <summary>
/// StdLib functions for cryptography
///
/// Computes hashes such as sha256, md5, etc.
/// </summary>

module StdLibExecution.Libs.Crypto

open System
open System.Security.Cryptography
open System.Security.Cryptography.X509Certificates

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

let types : List<BuiltInType> = []

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
      deprecated = NotDeprecated }

    { name = fn "X509" "pemCertificatePublicKey" 0
      typeParams = []
      parameters = [ Param.make "pemCert" TString "" ]
      returnType = TResult(TString, TString)
      description =
        "Extract the public key from a PEM encoded certificate and return the key in PEM format."
      fn =
        (function
        | _, _, [ DString certString ] ->
          try
            let cert = new X509Certificates.X509Certificate2(UTF8.toBytes certString)
            // Workaround to support ECC certs
            // https://www.pkisolutions.com/accessing-and-using-certificate-private-keys-in-net-framework-net-core/
            let publicKey : AsymmetricAlgorithm =
              match cert.PublicKey.Oid.Value with
              | "1.2.840.10045.2.1" -> // ECC
                cert.GetECDsaPublicKey()
              | "1.2.840.113549.1.1.1" -> // RSA
                cert.GetRSAPublicKey()
              // DSA
              | "1.2.840.10040.4.1"
              | _ -> cert.GetDSAPublicKey()
            let publicKeyBytes = publicKey.ExportSubjectPublicKeyInfo()
            let data = System.ReadOnlySpan<byte> publicKeyBytes
            let label = System.ReadOnlySpan<char>("PUBLIC KEY".ToCharArray())
            let chars = PemEncoding.Write(label, data)
            let str = new System.String(chars) + "\n"
            str |> DString |> Ok |> DResult |> Ply
          with
          | e ->
            // The OCaml version seems to support anything starting in BEGIN
            // CERTIFICATE. If it doesn't find that, it errors with No certificates. If
            // it does find that, it tries to parse it, returning X509: failed to parse
            // certificate if it fails (either data is bullshit or it's not an RSA cert).
            Ply(DResult(Error(DString "No certificates")))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
