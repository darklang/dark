module BuiltinExecution.IntRuntimeError

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs

type Error =
  | DivideByZeroError
  | OutOfRange
  | NegativeExponent
  | NegativeModulus
  | ZeroModulus

module RTE =
  let toRuntimeError (e : Error) : RuntimeError =
    let (caseName, fields) =
      match e with
      | DivideByZeroError -> "DivideByZeroError", []
      | OutOfRange -> "OutOfRange", []
      | NegativeExponent -> "NegativeExponent", []
      | NegativeModulus -> "NegativeModulus", []
      | ZeroModulus -> "ZeroModulus", []

    let typeName =
      FQTypeName.fqPackage PackageIDs.Type.LanguageTools.RuntimeError.Int.error

    DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError.intError
