/// Playgrond environment for testing
module FuzzTests.Playground

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module G = Generators

type Widget =
  /// Counter - starts at 0, and clicks upward by one
  | Counter of int

  /// Very useful flag
  | Flag of bool

module Generators =
  let widgetBad =
    Gen.oneof [ Arb.generate<int> |> Gen.map Counter
                Arb.generate<bool> |> Gen.map Flag ]

  let widgetGood =
    Gen.oneof [ G.nonNegativeInt |> Gen.map Counter
                Arb.generate<bool> |> Gen.map Flag ]

  type WidgetGood =
    static member Arb() = Arb.fromGen widgetGood

  type WidgetBad =
    static member Arb() = Arb.fromGen widgetBad

module Properties =
  /// We should be able to successfully 'check' a
  /// password against a hash of the same password
  let doNotHaveNegativeInts (widget : Widget) : bool =
    match widget with
    | Counter i -> i >= 0
    | Flag _ -> true


let tests =
  testList
    "widgets"
    [ testProperty
        typeof<Generators.WidgetGood>
        "don't have negative ints"
        Properties.doNotHaveNegativeInts ]
