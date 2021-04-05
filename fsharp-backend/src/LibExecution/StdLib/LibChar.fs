module LibExecution.StdLib.LibChar

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> = 
  [ { name = fn "Char" "toASCIICode" 0
      parameters = [Param.make "c" TChar ""]
      returnType = TInt
      description = "Return `c`'s ASCII code"
      fn = Errors.removedFunction
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = DeprecatedBecause("Dark does not support Characters yet") }
    { name = fn "Char" "toASCIIChar" 0
      parameters = [Param.make "i" TInt ""]
      returnType = TChar
      description = "convert an int to an ASCII character"
      fn = Errors.removedFunction
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = DeprecatedBecause("Dark does not support Characters yet") }
    { name = fn "Char" "toLowercase" 0
      parameters = [Param.make "c" TChar ""]
      returnType = TChar
      description = "Return the lowercase value of `c`"
      fn = Errors.removedFunction
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = DeprecatedBecause("Dark does not support Characters yet") }
    { name = fn "Char" "toUppercase" 0
      parameters = [Param.make "c" TChar ""]
      returnType = TChar
      description = "Return the uppercase value of `c`"
      fn = Errors.removedFunction
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = DeprecatedBecause("Dark does not support Characters yet") }
  ]
