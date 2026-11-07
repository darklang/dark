/// Base16 ("hex") encoding of bytes, RFC 4648 section 8.
module Builtins.Pure.Libs.Base16

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes


let fns () : List<BuiltInFn> =
  [ { name = fn "base16Encode" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Base16 (hex) encodes <param bytes> using an uppercase alphabet. "
        + "Complies with [RFC 4648 section "
        + "8](https://www.rfc-editor.org/rfc/rfc4648.html#section-8)."
      fn =
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bs ->
            Ply(DString(System.Convert.ToHexString(bs))))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "base16Decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Parses <param s> as an uppercase or lowercase Base16 (hex) string into Bytes."
      fn =
        let ok b = Dval.resultOk KTBytes KTString b
        let err s = Dval.resultError KTBytes KTString (DString s)
        (function
        | _, _, _, [| DString s |] ->
          try
            let bs = System.Convert.FromHexString(s)
            ok (Bytes.newEphemeral bs) |> Ply
          with e ->
            err $"Invalid hex string: {e.Message}" |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
