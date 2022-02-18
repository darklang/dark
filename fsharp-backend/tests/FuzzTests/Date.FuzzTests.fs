module FuzzTests.Date

open Expecto
open FsCheck

open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

let roundtrip (date : NodaTime.Instant) : bool =
  let date = date.truncate ()
  let roundTripped =
    date.toIsoString ()
    |> NodaTime.Instant.ofIsoString
    |> fun d -> d.toIsoString ()
    |> NodaTime.Instant.ofIsoString

  roundTripped = date

let tests = testList "date" [ testProperty "roundtrip" roundtrip ]
