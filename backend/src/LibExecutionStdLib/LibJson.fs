module LibExecutionStdLib.LibJson

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> = []
