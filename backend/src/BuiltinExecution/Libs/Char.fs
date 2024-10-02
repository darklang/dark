module BuiltinExecution.Libs.Char

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


let fns : List<BuiltInFn> =
  [ { name = fn "charToUppercase" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Return the uppercase value of <param c>.
        If <param c> does not have an uppercase value, returns <param c>"
      fn =
        function
        | _, _, _, [ DChar c ] -> Ply(DChar(c.ToUpper()))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "charToLowercase" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Return the lowercase value of <param c>.
        If <param c> does not have a lowercase value, returns <param c>"
      fn =
        function
        | _, _, _, [ DChar c ] -> Ply(DChar(c.ToLower()))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "charToAsciiCode" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TypeReference.option TInt64
      description =
        "Return {{Some <var code>}} if <param c> is a valid ASCII character, otherwise {{None}}"
      fn =
        function
        | _, _, _, [ DChar c ] ->
          let charValue = int c[0]
          if charValue >= 0 && charValue < 256 then
            Dval.optionSome KTInt64 (DInt64 charValue) |> Ply
          else
            Dval.optionNone KTInt64 |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "charIsLessThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is less than <param c2>"
      fn =
        function
        | _, _, _, [ DChar c1; DChar c2 ] -> (c1 < c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "charIsLessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is less than <param c2>"
      fn =
        function
        | _, _, _, [ DChar c1; DChar c2 ] -> (c1 <= c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "charIsGreaterThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is greater than <param c2>"
      fn =
        function
        | _, _, _, [ DChar c1; DChar c2 ] -> (c1 > c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "charIsGreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is greater than <param c2>"
      fn =
        function
        | _, _, _, [ DChar c1; DChar c2 ] -> (c1 >= c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "charToString" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TString
      description = "Stringify <param c>"
      fn =
        (function
        | _, _, _, [ DChar c ] -> Ply(DString c)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
