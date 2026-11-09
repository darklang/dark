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
        // Checked here rather than by catching `FromHexString`'s exception: the CLI is built
        // with `UseSystemResourceKeys`, so its message is a resource key such as
        // `Format_BadHexChar`.
        let isHexDigit (r : System.Text.Rune) =
          r.IsAscii && System.Uri.IsHexDigit(char r.Value)
        (function
        | _, _, _, [| DString s |] ->
          let notHex =
            s.EnumerateRunes() |> Seq.tryFind (fun r -> not (isHexDigit r))
          match notHex with
          | Some r ->
            err $"Invalid hex string: '{r}' is not a hex digit (0-9, a-f, A-F)"
            |> Ply
          | None when s.Length % 2 <> 0 ->
            err
              $"Invalid hex string: it has {s.Length} digits, and each byte takes two"
            |> Ply
          | None -> ok (Bytes.newEphemeral (System.Convert.FromHexString s)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
