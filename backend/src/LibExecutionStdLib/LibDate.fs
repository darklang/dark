module LibExecutionStdLib.LibDate

open System.Threading.Tasks
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

type Instant = NodaTime.Instant

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let product lists =
  let folder list state = state |> Seq.allPairs list |> Seq.map List.Cons
  Seq.singleton List.empty |> List.foldBack folder lists

let ocamlDateTimeFormats : array<string> =
  // List of Instant custom formats intented to exactly match the behaviour of OCaml Core.Time.of_string.
  // https://github.com/janestreet/core/blob/b0be1daa71b662bd38ef2bb406f7b3e70d63d05f/core/src/time.ml#L398
  // I looked at every permutation of separators and optional fields to match it
  // exactly. We generate about 600 different patterns.

  // All the different variations
  let dateFieldSeparators = [ " "; "-"; "" ]
  let dateTimeSeparator = [ "T"; " "; "" ]
  let timeSeparator = [ ":"; "" ]
  let millisecondFormat =
    // Support up to 7-digits of sub-second precision
    [ ""; ".f"; ".ff"; ".fff"; ".ffff"; ".fffff"; ".ffffff"; ".fffffff" ]
  let seconds = [ "ss"; "" ]
  let meridians = [ "tt"; "" ] // am or pm
  let tzSuffixes = [ "zzz"; "Z"; "" ] // +01:30 or just 'Z'

  // Cartesian product
  [ dateFieldSeparators
    dateTimeSeparator
    timeSeparator
    seconds
    meridians
    millisecondFormat
    tzSuffixes ]
  |> product
  |> Seq.map (fun parts ->
    match parts with
    | [ dfs; dts; ts; ss; tt; msf; tzs ] ->
      // Dont use a time separator for seconds if there are no seconds
      let ss = if ss = "" then "" else ts + ss

      // AM/PM requires the 12-hour hour format, but otherwise we support 24 hour
      let hh = if tt = "" then "HH" else "hh"

      // Don't allow milliseconds if no seconds
      let msf = if ss = "" then "" else msf

      $"yyyy{dfs}MM{dfs}dd{dts}{hh}{ts}mm{ss}{msf}{tt}{tzs}"
    | _ -> Exception.raiseInternal "ocamlDateTimeFormats is wrong shape" [])
  |> Set.ofSeq // remove dups
  |> Set.toArray
  |> (fun allFormats ->
    let mostCommon =
      [|
         // Do the most common ones first
         "yyyy-MM-ddTHH:mm:ssZ"
         "yyyy-MM-ddTHH:mm:ss"
         "yyyy-MM-dd HH:mm:ssZ"
         "yyyy-MM-dd HH:mm:ss"
         "yyyy MM dd HH:mm:ssZ"
         "yyyy MM dd HH:mm:ss" |]
    Array.append mostCommon allFormats)
// |> debugBy "formats" (String.concat "\n  ")

// CLEANUP The Date parser was OCaml's Core.Time.of_string, which supported an awful
// lot of use cases. Our docs say that we only want to support the format
// "yyyy-MM-ddTHH:mm:ssZ", so add a new version that only supports that format.
let ocamlCompatibleDateParser (s : string) : Result<DDateTime.T, unit> =
  if s.EndsWith('z') || s.Contains("GMT") then
    Error()
  else
    let culture = System.Globalization.CultureInfo.InvariantCulture
    let styles = System.Globalization.DateTimeStyles.AssumeUniversal
    let mutable (result : System.DateTime) = Unchecked.defaultof<System.DateTime>
    if
      System.DateTime.TryParseExact
        (
          s,
          ocamlDateTimeFormats,
          culture,
          styles,
          &result
        )
    then
      // print $"SUCCESS parsed {s}"
      Ok(DDateTime.fromDateTime (result.ToUniversalTime()))
    else
      // print $"              failed to parse {s}"
      Error()

let fns : List<BuiltInFn> =
  [ { name = fn "Date" "parse" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TDate
      description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns a Date"
      fn =
        (function
        | _, [ DStr s ] ->
          match ocamlCompatibleDateParser s with
          | Error () -> Ply(DError(SourceNone, "Invalid date format"))
          | Ok d -> Ply(DDate d)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "parse" 1) }


    { name = fn "Date" "parse" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TDate, TStr)
      description =
        "Parses a <type string> representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the <type Date> wrapped in a <type Result>."
      fn =
        (function
        | _, [ DStr s ] ->
          ocamlCompatibleDateParser s
          |> Result.map DDate
          |> Result.mapError (fun () -> DStr "Invalid date format")
          |> DResult
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "parse" 2) }


    { name = fn "Date" "parse" 2
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TDate, TStr)
      description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the {{Date}} wrapped in a {{Result}}."
      fn =
        (function
        | _, [ DStr s ] ->
          ocamlCompatibleDateParser s
          |> Result.map DDate
          |> Result.mapError (fun () -> DStr "Invalid date format")
          |> DResult
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toString" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TStr
      description =
        "Stringify <param date> to the ISO 8601 format {{YYYY-MM-DD'T'hh:mm:ss'Z'}}"
      fn =
        (function
        | _, [ DDate d ] ->
          let dt = DDateTime.toDateTimeUtc d
          dt.ToString("s", System.Globalization.CultureInfo.InvariantCulture) + "Z"
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toStringISO8601BasicDateTime" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TStr
      description =
        "Stringify <param date> to the ISO 8601 basic format {{YYYYMMDD'T'hhmmss'Z'}}"
      fn =
        (function
        | _, [ DDate d ] ->
          (DDateTime.toDateTimeUtc d).ToString("yyyyMMddTHHmmssZ") |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toStringISO8601BasicDate" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TStr
      description = "Stringify <param date> to the ISO 8601 basic format YYYYMMDD"
      fn =
        (function
        | _, [ DDate d ] ->
          (DDateTime.toDateTimeUtc d).ToString("yyyyMMdd") |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "now" 0
      parameters = []
      returnType = TDate
      description = "Returns the current time"
      fn =
        (function
        | _, [] -> Instant.now () |> DDateTime.fromInstant |> DDate |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Date" "today" 0
      parameters = []
      returnType = TDate
      description = "Returns the <type Date> with the time set to midnight"
      fn =
        (function
        | _, [] ->
          let now = DDateTime.fromInstant (Instant.now ())
          Ply(DDate(DDateTime.T(now.Year, now.Month, now.Day, 0, 0, 0)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Date" "add" 0
      parameters = [ Param.make "d" TDate ""; Param.make "seconds" TInt "" ]
      returnType = TDate
      description = "Returns a <type Date> <param seconds> seconds after <param d>"
      fn =
        (function
        | _, [ DDate d; DInt s ] ->
          d + (NodaTime.Period.FromSeconds s) |> DDate |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "sub" 0
      parameters = [ Param.make "d" TDate ""; Param.make "seconds" TInt "" ]
      returnType = TDate
      description = "Returns a <type Date> <param seconds> seconds before <param d>"
      fn =
        (function
        | _, [ DDate d; DInt s ] ->
          d - (NodaTime.Period.FromSeconds s) |> DDate |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable // As the OCaml one wasn't
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "subtract" 0) }


    // Note: this was was previously named `Date::subtract_v0`.
    // A new Date::subtract_v1 was created to replace this, and subtract_v0 got
    // replaced with this ::subtractSeconds_v0. "Date::subtract" implies that
    // you are subtracting one date from another, so subtracting anything else
    // should include the name of the relevant unit in the fn name.
    { name = fn "Date" "subtractSeconds" 0
      parameters = [ Param.make "d" TDate ""; Param.make "seconds" TInt "" ]
      returnType = TDate
      description = "Returns a <type Date> <param seconds> seconds before <param d>"
      fn =
        (function
        | _, [ DDate d; DInt s ] ->
          d - (NodaTime.Period.FromSeconds s) |> DDate |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "greaterThan" 0
      parameters = [ Param.make "d1" TDate ""; Param.make "d2" TDate "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> > <param d2>}}"
      fn =
        (function
        | _, [ DDate d1; DDate d2 ] -> Ply(DBool(d1 > d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "lessThan" 0
      parameters = [ Param.make "d1" TDate ""; Param.make "d2" TDate "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> < <param d2>}}"
      fn =
        (function
        | _, [ DDate d1; DDate d2 ] -> Ply(DBool(d1 < d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp("<")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "greaterThanOrEqualTo" 0
      parameters = [ Param.make "d1" TDate ""; Param.make "d2" TDate "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> >= <param d2>}}"
      fn =
        (function
        | _, [ DDate d1; DDate d2 ] -> Ply(DBool(d1 >= d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp(">=")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "lessThanOrEqualTo" 0
      parameters = [ Param.make "d1" TDate ""; Param.make "d2" TDate "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> <= <param d2>}}"
      fn =
        (function
        | _, [ DDate d1; DDate d2 ] -> Ply(DBool(d1 <= d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp("<=")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toSeconds" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description =
        "Converts <param date> to an <type integer> representing seconds since the Unix epoch"
      fn =
        (function
        | _, [ DDate d ] ->
          (DDateTime.toInstant d).ToUnixTimeSeconds() |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "fromSeconds" 0
      parameters = [ Param.make "seconds" TInt "" ]
      returnType = TDate
      description =
        "Converts an <type integer> representing seconds since the Unix epoch into a <type Date>"
      fn =
        (function
        | _, [ DInt s ] ->
          s |> Instant.FromUnixTimeSeconds |> DDateTime.fromInstant |> DDate |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "year" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the year portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] -> d.Year |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'year'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "month" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description =
        "Returns the month portion of <param date> as an <type int> between {{1}} and {{12}}"
      fn =
        (function
        | _, [ DDate d ] -> d.Month |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'month'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "day" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the day portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] -> d.Day |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'day'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "weekday" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description =
        "Returns the weekday of <param date> as an <type int>. Monday = {{1}}, Tuesday = {{2}}, ... Sunday = {{7}} (in accordance with ISO 8601)"
      fn =
        (function
        | _, [ DDate d ] -> d.DayOfWeek |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "hour" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the hour portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] ->
          // This is wrong, hence being replaced
          let duration = DDateTime.toInstant d - Instant.UnixEpoch
          (duration.TotalHours % 60.0) |> int |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "hour" 1) }


    { name = fn "Date" "hour" 1
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the hour portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] ->
          // this was made bug-for-bug compatible with old OCaml backend
          let s = if d.Year < 1970 then d.Hour - 23 else d.Hour
          Ply(Dval.int s)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'hour'" ])
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "hour" 2) }


    { name = fn "Date" "hour" 2
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the hour portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] -> Ply(Dval.int d.Hour)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'hour'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "minute" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the minute portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] ->
          // this was made bug-for-bug compatible with the old OCaml backend
          let s =
            if d.Year < 1970 then
              if d.Second = 0 then (d.Minute - 60) % 60 else d.Minute - 59
            else
              d.Minute

          Ply(Dval.int s)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'minute'" ])
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "minute" 1) }


    { name = fn "Date" "minute" 1
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the minute portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] -> Ply(Dval.int d.Minute)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'minute'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "second" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the second portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] ->
          // this was made bug-for-bug compatible with the old OCaml backend
          let s = if d.Year < 1970 then (d.Second - 60) % 60 else d.Second
          Ply(Dval.int s)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'second'" ])
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "second" 1) }


    { name = fn "Date" "second" 1
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the second portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDate d ] -> Ply(Dval.int d.Second)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'second'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "atStartOfDay" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TDate
      description = "Returns <type date> with the time set to midnight"
      fn =
        (function
        | _, [ DDate d ] ->
          DDateTime.T(d.Year, d.Month, d.Day, 0, 0, 0) |> DDate |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_trunc", [ "'day'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    // TODO: when we have timespans in Dark, deprecate this
    // in favor of a fn which returns a timespan rather than seconds
    //
    // Note: the SQL here would be `EPOCH FROM (end - start)`, but we don't
    // currently support such a complex sqlSpec in Dark fns.
    { name = fn "Date" "subtract" 1
      parameters = [ Param.make "end" TDate ""; Param.make "start" TDate "" ]
      returnType = TInt
      description = "Returns the difference of the two dates, in seconds"
      fn =
        (function
        | _, [ DDate endDate; DDate startDate ] ->
          let diff = (DDateTime.toInstant endDate) - (DDateTime.toInstant startDate)
          diff.TotalSeconds |> System.Math.Round |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
