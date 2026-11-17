module Builtins.Pure.Libs.DateTime

type Instant = NodaTime.Instant

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module Dval = LibExecution.Dval
module DarkDateTime = LibExecution.DarkDateTime
module PackageRefs = LibExecution.PackageRefs

module ParseError =
  type ParseError =
    | InvalidFormat
    | InvalidDate

  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.Stdlib.dateTimeParseError ())

  let toDT (error : ParseError) : Dval =
    let name = typeName ()
    let caseName =
      match error with
      | InvalidFormat -> "InvalidFormat"
      | InvalidDate -> "InvalidDate"
    DEnum(name, name, [], caseName, [])

// NodaTime's `uuuu` writes years before 1, which .NET's `DateTime` cannot hold.
let private isoSeconds =
  NodaTime.Text.LocalDateTimePattern.CreateWithInvariantCulture
    "uuuu'-'MM'-'dd'T'HH':'mm':'ss'Z'"

let private isoMilliseconds =
  NodaTime.Text.LocalDateTimePattern.CreateWithInvariantCulture
    "uuuu'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'"

/// The number in the `count` ASCII digits of `s` from `at`, or -1.
let private digitsAt (s : string) (at : int) (count : int) : int =
  let rec go (i : int) (acc : int) =
    if i = at + count then acc
    elif s[i] >= '0' && s[i] <= '9' then go (i + 1) (acc * 10 + int s[i] - int '0')
    else -1
  if at + count > s.Length then -1 else go at 0

/// The instant `s` names: `YYYY-MM-DD` as midnight UTC, or `YYYY-MM-DDThh:mm:ss`, optionally
/// with `.fff`, then `Z` or an offset `+hh:mm` / `-hh:mm`.
let private parseIso (s : string) : Result<Instant, ParseError.ParseError> =
  let at (i : int) (c : char) = i < s.Length && s[i] = c
  let n = digitsAt s
  let year, month, day = n 0 4, n 5 2, n 8 2
  let dateSyntax = at 4 '-' && at 7 '-' && year >= 0 && month >= 0 && day >= 0
  let validDate =
    at 4 '-'
    && at 7 '-'
    && year >= 1
    && month >= 1
    && month <= 12
    && day >= 1
    && day <= System.DateTime.DaysInMonth(year, month)
  let instant
    (hour : int)
    (minute : int)
    (second : int)
    (ms : int)
    (offsetMinutes : int)
    =
    if not validDate then
      Error ParseError.InvalidDate
    else
      NodaTime
        .LocalDateTime(year, month, day, hour, minute, second, ms)
        .InUtc()
        .ToInstant()
        .Minus(NodaTime.Duration.FromMinutes(int64 offsetMinutes))
      |> Ok
  if not dateSyntax then
    Error ParseError.InvalidFormat
  elif s.Length = 10 then
    instant 0 0 0 0 0
  else
    let hour, minute, second = n 11 2, n 14 2, n 17 2
    let validTime =
      at 10 'T'
      && at 13 ':'
      && at 16 ':'
      && hour >= 0
      && hour <= 23
      && minute >= 0
      && minute <= 59
      && second >= 0
      && second <= 59
    let ms, zone = if at 19 '.' then n 20 3, 23 else 0, 19
    let offsetMinutes =
      if at zone 'Z' && s.Length = zone + 1 then
        Some 0
      elif
        (at zone '+' || at zone '-') && at (zone + 3) ':' && s.Length = zone + 6
      then
        let hours, minutes = n (zone + 1) 2, n (zone + 4) 2
        if hours >= 0 && hours <= 23 && minutes >= 0 && minutes <= 59 then
          Some((if s[zone] = '-' then -1 else 1) * (hours * 60 + minutes))
        else
          None
      else
        None
    match offsetMinutes with
    | Some offset when validTime && ms >= 0 -> instant hour minute second ms offset
    | _ -> Error ParseError.InvalidFormat


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


    { name = fn "dateTimeParse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TDateTime
          (TCustomType(
            { originalName = []; resolved = Ok(ParseError.typeName ()) },
            []
          ))
      description =
        "The date and time in <param s>: {{YYYY-MM-DD}}, or {{YYYY-MM-DDThh:mm:ss}} with an "
        + "optional {{.fff}} and then {{Z}} or an offset such as {{+02:00}}"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          let errorType = KTCustomType(ParseError.typeName (), [])
          match parseIso s with
          | Ok instant ->
            DDateTime(DarkDateTime.fromInstant instant)
            |> Dval.resultOk KTDateTime errorType
          | Error error ->
            ParseError.toDT error |> Dval.resultError KTDateTime errorType
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
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
          (DarkDateTime.toInstant d).ToUnixTimeTicks() |> int64 |> Dval.dint64
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
          pattern.Format d |> DString
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
