open Core_kernel
open Libexecution
open Libexecution.Lib
module U = Libexecution.Unicode_string

let fns : Types.RuntimeT.fn list =
  [ { name = fn "X509" "pemCertificatePublicKey" 0

    ; parameters = [Param.make "pemCert" TStr]
    ; returnType = TResult
    ; description =
        "Extract the public key from a PEM encoded certificate and return the key in PEM format."
    ; fn =

          (function
          | _, [DStr cert] ->
            ( try
                cert
                |> U.to_string
                |> Cstruct.of_string
                |> X509.Encoding.Pem.Certificate.of_pem_cstruct1
                |> X509.public_key
                |> X509.Encoding.Pem.Public_key.to_pem_cstruct1
                |> Cstruct.to_string
                |> Dval.dstr_of_string_exn
                |> ResOk
                |> DResult
              with Invalid_argument msg ->
                DResult (ResError (Dval.dstr_of_string_exn msg)) )
          | _ ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
    ; previewable = Impure
    ; deprecated = NotDeprecated } ]
