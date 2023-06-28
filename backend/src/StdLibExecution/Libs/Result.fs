module StdLibExecution.Libs.Result

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module Interpreter = LibExecution.Interpreter
module Errors = LibExecution.Errors

let varOk = TVariable "ok"
let varErr = TVariable "err"
let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"

let types : List<BuiltInType> = []
let fns : List<BuiltInFn> = []
let contents = (fns, types)
