module BuiltinExecution.Libs.Char

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Char" ]

let fns : List<BuiltInFn> =
  [ { name = fn "toUppercase" 0
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


    { name = fn "toLowercase" 0
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


    { name = fn "isDigit" 0
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


    { name = fn "isASCIILetter" 0
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


    { name = fn "isASCII" 0
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

    { name = fn "isLessThan" 0
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

    { name = fn "isGreaterThan" 0
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


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TString
      description = "Stringify <param c>"
      fn =
        (function
        | _, _, [ DChar c ] -> Ply(DString(c))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    ]

let contents = (fns, types, constants)
