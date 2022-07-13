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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") } ]
