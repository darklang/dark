module LibExecutionStdLib.LibChar

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "Char" "toASCIICode" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TInt
      description = "Return `c`'s ASCII code"
      fn =
        function
        | state, [ c ] -> Errors.removedFunction ()
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") }


    { name = fn "Char" "toASCIIChar" 0
      parameters = [ Param.make "i" TInt "" ]
      returnType = TChar
      description = "convert an int to an ASCII character"
      fn =
        function
        | state, [ i ] -> Errors.removedFunction ()
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") }


    { name = fn "Char" "toLowercase" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description = "Return the lowercase value of `c`"
      fn =
        function
        | state, [ c ] -> Errors.removedFunction ()
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") }


    { name = fn "Char" "toUppercase" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description = "Return the uppercase value of `c`"
      fn =
        function
        | state, [ c ] -> Errors.removedFunction ()
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated =
        DeprecatedBecause("used an old Character type that no longer exists") } ]
