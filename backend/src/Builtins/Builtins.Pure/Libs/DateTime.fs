module Builtins.Pure.Libs.DateTime

type Instant = NodaTime.Instant

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module Dval = LibExecution.Dval
module DarkDateTime = LibExecution.DarkDateTime

// NodaTime's `uuuu` writes years before 1, which .NET's `DateTime` cannot hold.
let private isoSeconds =
  NodaTime.Text.LocalDateTimePattern.CreateWithInvariantCulture
    "uuuu'-'MM'-'dd'T'HH':'mm':'ss'Z'"

let private isoMilliseconds =
  NodaTime.Text.LocalDateTimePattern.CreateWithInvariantCulture
    "uuuu'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'"


let fns () : List<BuiltInFn> =
  [ { name = fn "dateTimeFromUnixTicks" 0
      typeParams = []
      parameters =
        [ Param.make "ticks" TInt64 "100ns ticks since Unix epoch"
          Param.make "count" TInt64 "How many units to move by"
          Param.make "unit" TInt64 "100ns ticks per unit" ]
      returnType = TDateTime
      description =
        "The DateTime <param count> units of <param unit> ticks each after <param ticks>"
      fn =
        (function
        | _, vm, _, [| DInt64 ticks; DInt64 count; DInt64 unit |] ->
          // NodaTime throws on instants past its range, even ones that fit int64.
          try
            bigint ticks + bigint count * bigint unit
            |> int64
            |> Instant.FromUnixTimeTicks
            |> DarkDateTime.fromInstant
            |> DDateTime
            |> Ply
          with
          | :? System.ArgumentOutOfRangeException
          | :? System.OverflowException ->
            let kind =
              match unit with
              | 10000000L -> RuntimeError.BigInts.DateTimeSeconds
              | 10000L -> RuntimeError.BigInts.DateTimeMilliseconds
              | _ -> RuntimeError.BigInts.DateTimeTicks
            outOfRange vm kind (DInt64 count)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "dateTimeToUnixTicks" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt64
      description =
        "100ns ticks since the Unix epoch; the inverse of <fn dateTimeFromUnixTicks>"
      fn =
        (function
        | _, _, _, [| DDateTime d |] ->
          (DarkDateTime.toInstant d).ToUnixTimeTicks() |> int64 |> Dval.dint64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dateTimeToString" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TString
      description =
        "Stringify <param date> to the ISO 8601 format {{YYYY-MM-DD'T'hh:mm:ss'Z'}} "
        + "or {{YYYY-MM-DD'T'hh:mm:ss.fff'Z'}} when milliseconds are non-zero"
      fn =
        (function
        | _, _, _, [| DDateTime d |] ->
          let pattern = if d.Millisecond <> 0 then isoMilliseconds else isoSeconds
          pattern.Format d |> DString |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
