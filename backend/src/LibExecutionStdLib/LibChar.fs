module LibExecutionStdLib.LibChar

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "Char" "toUppercase" 1
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Return the uppercase value of <param c>. If <param c> does not have an uppercase value, returns <param c>"
      fn =
        function
        | _, _, [ DChar c ] -> Ply(DChar(c.ToUpper()))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "toLowercase" 1
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Return the lowercase value of <param c>. If <param c> does not have a lowercase value, returns <param c>"
      fn =
        function
        | _, _, [ DChar c ] -> Ply(DChar(c.ToLower()))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isLowercase" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is a lowercase character."
      fn =
        function
        | _, _, [ DChar c ] ->
          // If we just check that the uppercase value of the char is the same, then
          // chars that are not letters would be incorrectly reported as uppercase
          Ply(DBool(c.ToLower() = c && c.ToUpper() <> c))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isUppercase" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is an uppercase character."
      fn =
        function
        | _, _, [ DChar c ] ->
          // If we just check that the uppercase value of the char is the same, then
          // chars that are not letters would be incorrectly reported as uppercase
          Ply(DBool(c.ToUpper() = c && c.ToLower() <> c))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isDigit" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is a digit (that is, the digits 0-9)"
      fn =
        function
        | _, _, [ DChar c ] ->
          (if c.Length = 1 then System.Char.IsDigit(c[0]) else false) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isASCIILetter" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is an ASCII letter"
      fn =
        function
        | _, _, [ DChar c ] ->
          (if c.Length = 1 then
             System.Char.IsAscii c[0] && System.Char.IsLetter c[0]
           else
             false)
          |> DBool
          |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isASCII" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is a valid ASCII character"
      fn =
        function
        | _, _, [ DChar c ] ->
          (if c.Length = 1 then System.Char.IsAscii c[0] else false) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Char" "isLessThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is less than <param c2>"
      fn =
        function
        | _, _, [ DChar c1; DChar c2 ] -> (c1 < c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Char" "isGreaterThan" 0
      typeParams = []
      parameters = [ Param.make "c1" TChar ""; Param.make "c2" TChar "" ]
      returnType = TBool
      description = "Return whether <param c1> is greater than <param c2>"
      fn =
        function
        | _, _, [ DChar c1; DChar c2 ] -> (c1 > c2) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "toString" 0
      typeParams = []
      parameters = [ Param.make "c" TInt "" ]
      returnType = TStr
      description = "Stringify <param c>"
      fn =
        (function
        | _, _, [ DChar c ] -> Ply(DStr(c))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    ]
