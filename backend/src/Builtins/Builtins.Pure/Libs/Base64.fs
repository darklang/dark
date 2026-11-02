module Builtins.Pure.Libs.Base64

open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes


let fns () : List<BuiltInFn> =
  [ { name = fn "base64Decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard "
        + "Base64 alphabets defined in [RFC "
        + "4648](https://www.rfc-editor.org/rfc/rfc4648.html) sections "
        + "[4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and "
        + "[5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        let resultOk r = Dval.resultOk KTBytes KTString r |> Ply
        let resultError r = Dval.resultError KTBytes KTString r |> Ply
        (function
        | _, _, _, [| DString s |] ->
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            resultOk (Bytes.newEphemeral [||])
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            resultError (DString "Not a valid base64 string")
          else
            try
              let bytes =
                s |> base64FromUrlEncoded |> System.Convert.FromBase64String
              resultOk (Bytes.newEphemeral bytes)
            with _ ->
              resultError (DString "Not a valid base64 string")
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "base64Encode" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Base64 encodes <param bytes> with {{=}} padding. Uses the standard "
        + "alphabet defined in [RFC "
        + "4648](https://www.rfc-editor.org/rfc/rfc4648.html) section "
        + "[4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4)."
      fn =
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bytes ->
            Ply(DString(System.Convert.ToBase64String(bytes))))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
