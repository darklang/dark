module BackendOnlyStdLib.LibX509

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus
open System.Security.Cryptography
open System.Security.Cryptography.X509Certificates

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "X509" "pemCertificatePublicKey" 0
      parameters = [ Param.make "pemCert" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Extract the public key from a PEM encoded certificate and return the key in PEM format."
      fn =
        (function
        | _, [ DStr certString ] ->
          try
            let cert = new X509Certificates.X509Certificate2(toBytes certString)
            // The OCaml version looks like it supports ECC certs, which require
            // this workaround on .NET. However, the OCaml version gets the wrong
            // answer for it.

            // https://www.pkisolutions.com/accessing-and-using-certificate-private-keys-in-net-framework-net-core/
            let publicKey =
              match cert.PublicKey.Oid.Value with
              | "1.2.840.10045.2.1" -> // ECC
                cert.GetECDsaPublicKey() :> AsymmetricAlgorithm
              | "1.2.840.113549.1.1.1" -> // RSA
                cert.GetRSAPublicKey() :> AsymmetricAlgorithm
              // DSA
              | "1.2.840.10040.4.1"
              | _ -> cert.GetDSAPublicKey() :> AsymmetricAlgorithm
            let publicKeyBytes = publicKey.ExportSubjectPublicKeyInfo()
            let data = System.ReadOnlySpan<byte> publicKeyBytes
            let label = System.ReadOnlySpan<char>("PUBLIC KEY".ToCharArray())
            let chars = PemEncoding.Write(label, data)
            let str = new System.String(chars) ++ "\n"
            str |> DStr |> Ok |> DResult |> Value
          with
          | e ->
            // The OCaml version seems to support anything starting in BEGIN
            // CERTIFICATE. If it doesn't find that, it errors with No certificates. If
            // it does find that, it tries to parse it, returning X509: failed to parse
            // certificate if it fails (either data is bullshit or it's not an RSA cert).
            Value(DResult(Error(DStr "No certificates")))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
