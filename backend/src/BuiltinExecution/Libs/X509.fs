/// Builtin functions to work with X.509 public keys
module BuiltinExecution.Libs.X509

open System.Security.Cryptography
open System.Security.Cryptography.X509Certificates

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "x509PemCertificatePublicKey" 0
      typeParams = []
      parameters = [ Param.make "pemCert" TString "" ]
      returnType = TypeReference.result TString TString
      description =
        "Extract the public key from a PEM encoded certificate and return the key in PEM format."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString
        (function
        | _, _, _, [ DString certString ] ->
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
            str |> DString |> resultOk |> Ply
          with e ->
            // If it doesn't find BEGIN CERTIFICATE that, it errors with No
            // certificates. If it does find that, it tries to parse it, returning
            // X509: failed to parse certificate if it fails (either data is bullshit
            // or it's not an RSA cert).
            resultError (DString "No certificates") |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
