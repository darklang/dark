module Builtins.Pure.Libs.Utf8

open System.Text

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes


let fns () : List<BuiltInFn> =
  [ { name = fn "utf8Encode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TBytes
      description = "Encodes <param s> as UTF-8."
      fn =
        (function
        | _, _, _, [| DString s |] ->
          Bytes.newEphemeral (Encoding.UTF8.GetBytes s) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "utf8Decode" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TypeReference.result TString TString
      description =
        "Decodes <param bytes> as UTF-8. Returns Error with a message if the "
        + "bytes aren't valid UTF-8."
      fn =
        let ok r = Dval.resultOk KTString KTString r
        let err r = Dval.resultError KTString KTString r
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bs ->
            try
              let s = (new UTF8Encoding(false, true)).GetString(bs)
              Ply(ok (DString s))
            with e ->
              Ply(err (DString($"Invalid UTF-8: {e.Message}"))))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "utf8DecodeWithReplacement" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Decodes <param bytes> as UTF-8, replacing invalid sequences with "
        + "{{U+FFFD}}."
      fn =
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bs ->
            Ply(DString(Encoding.UTF8.GetString bs)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
