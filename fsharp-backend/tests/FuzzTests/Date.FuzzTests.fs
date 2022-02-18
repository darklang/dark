module FuzzTests.Date

open Expecto
open FsCheck

open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

let roundtrip (date : System.DateTime) : bool =
  let truncate (d : System.DateTime) =
    d.AddTicks(-(d.Ticks % System.TimeSpan.TicksPerSecond))

  let date = truncate date

  let roundTripped =
    date.toIsoString ()
    |> System.DateTime.ofIsoString
    |> fun d -> d.toIsoString ()
    |> System.DateTime.ofIsoString

  roundTripped = date

let tests = testList "date" [ testProperty "roundtrip" roundtrip ]
