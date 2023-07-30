/// StdLib functions to work with X.509 public keys
module StdLibExecution.Libs.X509

open System.Security.Cryptography
open System.Security.Cryptography.X509Certificates

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

let varA = TVariable "a"
let varB = TVariable "b"

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "X509" ] "pemCertificatePublicKey" 0
      typeParams = []
      parameters = [ Param.make "pemCert" TString "" ]
      returnType = TypeReference.result TString TString
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
            str |> DString |> Dval.resultOk |> Ply
          with e ->
            // The OCaml version seems to support anything starting in BEGIN
            // CERTIFICATE. If it doesn't find that, it errors with No certificates. If
            // it does find that, it tries to parse it, returning X509: failed to parse
            // certificate if it fails (either data is bullshit or it's not an RSA cert).
            Ply(Dval.resultError (DString "No certificates"))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
