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
  [ { name = fn "Char" "toASCIICode" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TInt
      description = "Return <param c>'s ASCII code"
      fn =
        function
        | state, [ c ] -> Errors.removedFunction state "Char::toASCIICode"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") }


    { name = fn "Char" "toASCIIChar" 0
      parameters = [ Param.make "i" TInt "" ]
      returnType = TChar
      description = "Convert <param i> to an ASCII character"
      fn =
        function
        | state, [ i ] -> Errors.removedFunction state "Char::toASCIIChar"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") }


    { name = fn "Char" "toLowercase" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description = "Return the lowercase value of <param c>"
      fn =
        function
        | state, [ c ] -> Errors.removedFunction state "Char::toLowercase"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") }


    { name = fn "Char" "toUppercase" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description = "Return the uppercase value of <param c>"
      fn =
        function
        | state, [ c ] -> Errors.removedFunction state "Char::toUppercase"
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") }


    { name = fn "Char" "toUppercase" 1
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Return the uppercase value of <param c>. If <param c> does not have an uppercase value, returns <param c>"
      fn =
        function
        | _, [ DChar c ] -> Ply(DChar(c.ToUpper()))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "toLowercase" 1
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description =
        "Return the lowercase value of <param c>. If <param c> does not have a lowercase value, returns <param c>"
      fn =
        function
        | _, [ DChar c ] -> Ply(DChar(c.ToLower()))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isLowercase" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is a lowercase character."
      fn =
        function
        | _, [ DChar c ] ->
          // If we just check that the uppercase value of the char is the same, then
          // chars that are not letters would be incorrectly reported as uppercase
          Ply(DBool(c.ToLower() = c && c.ToUpper() <> c))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isUppercase" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is an uppercase character."
      fn =
        function
        | _, [ DChar c ] ->
          // If we just check that the uppercase value of the char is the same, then
          // chars that are not letters would be incorrectly reported as uppercase
          Ply(DBool(c.ToUpper() = c && c.ToLower() <> c))
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isDigit" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is a digit (that is, the digits 0-9)"
      fn =
        function
        | _, [ DChar c ] ->
          (if c.Length = 1 then System.Char.IsDigit(c[0]) else false) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Char" "isASCIILetter" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is an ASCII letter"
      fn =
        function
        | _, [ DChar c ] ->
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
      parameters = [ Param.make "c" TChar "" ]
      returnType = TBool
      description = "Return whether <param c> is a valid ASCII character"
      fn =
        function
        | _, [ DChar c ] ->
          (if c.Length = 1 then System.Char.IsAscii c[0] else false) |> DBool |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
