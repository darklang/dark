module Builtins.Pure.Libs.Char

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


/// Return the first Unicode scalar in a grapheme cluster.
let private firstRune (s : string) : System.Text.Rune option =
  let mutable rune = Unchecked.defaultof<System.Text.Rune>
  if System.Text.Rune.TryGetRuneAt(s, 0, &rune) then Some rune else None


/// Apply a character predicate to the cluster's first scalar.
let private firstRuneIs (pred : System.Text.Rune -> bool) (s : string) : bool =
  match firstRune s with
  | Some r -> pred r
  | None -> false


let fns () : List<BuiltInFn> =
  [ { name = fn "charIsLessThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is less than <param c2>"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] -> (c1 < c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsLessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is less than <param c2>"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] -> (c1 <= c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsGreaterThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is greater than <param c2>"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] -> (c1 > c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsGreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is greater than <param c2>"
      fn =
        function
        | _, _, _, [| DChar c1; DChar c2 |] -> (c1 >= c2) |> DBool |> Ply
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


    { name = fn "charFromCodepoint" 0
      typeParams = []
      parameters = [ Param.make "codepoint" TInt "" ]
      returnType = TypeReference.option TChar
      description =
        "Return {{Some <var c>}} for the Unicode codepoint <param codepoint>, "
        + "or {{None}} if the value is not a valid scalar codepoint (i.e. "
        + "negative, greater than 0x10FFFF, or a surrogate)."
      fn =
        function
        | _, _, _, [| DInt cp |] ->
          let cp = DarkInt.toBigInt cp
          if
            cp < bigint 0
            || cp > bigint 0x10FFFF
            || (cp >= bigint 0xD800 && cp <= bigint 0xDFFF)
          then
            Dval.optionNone KTChar |> Ply
          else
            let s = System.Char.ConvertFromUtf32(int cp)
            Dval.optionSome KTChar (DChar s) |> Ply
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsLetter" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description =
        "Return whether <param c> is a Unicode letter, in any script rather than just "
        + "ASCII. Digits, punctuation, symbols and emoji are not letters."
      fn =
        function
        | _, _, _, [| DChar c |] ->
          Ply(DBool(firstRuneIs System.Text.Rune.IsLetter c))
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsWhitespace" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description =
        "Return whether <param c> is whitespace -- a space, a tab, a newline, or any of "
        + "Unicode's other space characters."
      fn =
        function
        | _, _, _, [| DChar c |] ->
          Ply(DBool(firstRuneIs System.Text.Rune.IsWhiteSpace c))
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "charIsAlphanumeric" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description =
        "Return whether <param c> is a letter or a digit, in any script rather than just "
        + "ASCII."
      fn =
        function
        | _, _, _, [| DChar c |] ->
          Ply(DBool(firstRuneIs System.Text.Rune.IsLetterOrDigit c))
        | _ -> incorrectArgs ()
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
