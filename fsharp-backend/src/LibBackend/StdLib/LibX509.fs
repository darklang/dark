module LibBackend.StdLib.LibX509

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus
open System.Security.Cryptography

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

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
        | _, [ DStr cert ] ->
          (try
            let x509Cert = new X509Certificates.X509Certificate2(toBytes cert)
            let publicKeyBytes = x509Cert.GetPublicKey()
            let label = System.ReadOnlySpan<char>("PUBLIC KEY".ToCharArray())
            let data = System.ReadOnlySpan<byte> publicKeyBytes
            let chars = PemEncoding.Write(label, data)

            (new System.String(chars)) |> DStr |> Ok |> DResult |> Value
           with
           | e -> Value(DResult(Error(DStr e.Message))))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
