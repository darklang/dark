module Builtins.Pure.Libs.Char

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


/// Apply a Unicode predicate, returning false for invalid scalars.
let private checkCodepoint (pred : System.Text.Rune -> bool) (cp : uint32) : bool =
  let mutable rune = System.Text.Rune(0)
  System.Text.Rune.TryCreate(cp, &rune) && pred rune


let fns () : List<BuiltInFn> =
  [ { name = fn "charIsLessThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description =
        "Return whether <param c1> sorts before <param c2>, by code point"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] ->
          String.compareByCodepoint c1 c2 < 0 |> DBool |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsLessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description =
        "Return whether <param c1> sorts before or equal to <param c2>, by code point"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] ->
          String.compareByCodepoint c1 c2 <= 0 |> DBool |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsGreaterThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> sorts after <param c2>, by code point"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] ->
          String.compareByCodepoint c1 c2 > 0 |> DBool |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsGreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description =
        "Return whether <param c1> sorts after or equal to <param c2>, by code point"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] ->
          String.compareByCodepoint c1 c2 >= 0 |> DBool |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charToString" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TString
      description = "Stringify <param c>"
      fn =
        (function
        | _, _, _, [| DChar c |] -> Ply(DString c)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charToUppercase" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Returns <param c> uppercased with Unicode's simple case mapping, one Char "
        + "to one"
      fn =
        (function
        | _, _, _, [| DChar c |] ->
          Ply(DChar(Builtins.Pure.CaseMapping.charToUppercase c))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charToLowercase" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Returns <param c> lowercased with Unicode's simple case mapping, one Char "
        + "to one"
      fn =
        (function
        | _, _, _, [| DChar c |] ->
          Ply(DChar(Builtins.Pure.CaseMapping.charToLowercase c))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charFromCodepoint" 0
      typeParams = []
      parameters = [ Param.make "codepoint" TUInt32 "" ]
      returnType = TypeReference.option TChar
      description =
        "Return {{Some <var c>}} for the Unicode codepoint <param codepoint>, "
        + "or {{None}} if the value is not a valid scalar codepoint (i.e. "
        + "greater than 0x10FFFF, or a surrogate)."
      fn =
        function
        | _, _, _, [| DUInt32 cp |] ->
          // TryCreate needs a mutable output variable; zero is just its initial value.
          let mutable rune = System.Text.Rune(0)
          if System.Text.Rune.TryCreate(cp, &rune) then
            Dval.optionSome KTChar (DChar(rune.ToString())) |> Ply
          else
            Dval.optionNone KTChar |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "codepointIsLetter" 0
      typeParams = []
      parameters = [ Param.make "c" TUInt32 "" ]
      returnType = TBool
      description =
        "Return whether <param c> is a Unicode letter, in any script rather than just "
        + "ASCII. Digits, punctuation, symbols and emoji are not letters."
      fn =
        function
        | _, _, _, [| DUInt32 c |] ->
          Ply(DBool(checkCodepoint System.Text.Rune.IsLetter c))
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "codepointIsWhitespace" 0
      typeParams = []
      parameters = [ Param.make "c" TUInt32 "" ]
      returnType = TBool
      description =
        "Return whether <param c> is whitespace -- a space, a tab, a newline, or any of "
        + "Unicode's other space characters."
      fn =
        function
        | _, _, _, [| DUInt32 c |] ->
          Ply(DBool(checkCodepoint System.Text.Rune.IsWhiteSpace c))
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "codepointIsAlphanumeric" 0
      typeParams = []
      parameters = [ Param.make "c" TUInt32 "" ]
      returnType = TBool
      description =
        "Return whether <param c> is a letter or a digit, in any script rather than just "
        + "ASCII."
      fn =
        function
        | _, _, _, [| DUInt32 c |] ->
          Ply(DBool(checkCodepoint System.Text.Rune.IsLetterOrDigit c))
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
