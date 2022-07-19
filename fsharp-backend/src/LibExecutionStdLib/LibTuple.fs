module LibExecutionStdLib.LibTuple

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> = []
