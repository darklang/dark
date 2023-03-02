module LibExecutionStdLib.LibDateTime

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
let ocamlCompatibleDateParser (s : string) : Result<DarkDateTime.T, unit> =
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
      Ok(DarkDateTime.fromDateTime (result.ToUniversalTime()))
    else
      // print $"              failed to parse {s}"
      Error()

let fns : List<BuiltInFn> =
  [ { name = fn "DateTime" "parse" 2
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TDateTime, TStr)
      description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the {{Date}} wrapped in a {{Result}}."
      fn =
        (function
        | _, [ DStr s ] ->
          ocamlCompatibleDateParser s
          |> Result.map DDateTime
          |> Result.mapError (fun () -> DStr "Invalid date format")
          |> DResult
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "toString" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TStr
      description =
        "Stringify <param date> to the ISO 8601 format {{YYYY-MM-DD'T'hh:mm:ss'Z'}}"
      fn =
        (function
        | _, [ DDateTime d ] ->
          let dt = DarkDateTime.toDateTimeUtc d
          dt.ToString("s", System.Globalization.CultureInfo.InvariantCulture) + "Z"
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "toStringISO8601BasicDateTime" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TStr
      description =
        "Stringify <param date> to the ISO 8601 basic format {{YYYYMMDD'T'hhmmss'Z'}}"
      fn =
        (function
        | _, [ DDateTime d ] ->
          (DarkDateTime.toDateTimeUtc d).ToString("yyyyMMddTHHmmssZ") |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "toStringISO8601BasicDate" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TStr
      description = "Stringify <param date> to the ISO 8601 basic format YYYYMMDD"
      fn =
        (function
        | _, [ DDateTime d ] ->
          (DarkDateTime.toDateTimeUtc d).ToString("yyyyMMdd") |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "now" 0
      parameters = []
      returnType = TDateTime
      description = "Returns the current time"
      fn =
        (function
        | _, [] -> Instant.now () |> DarkDateTime.fromInstant |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "today" 0
      parameters = []
      returnType = TDateTime
      description = "Returns the <type Date> with the time set to midnight"
      fn =
        (function
        | _, [] ->
          let now = DarkDateTime.fromInstant (Instant.now ())
          Ply(DDateTime(DarkDateTime.T(now.Year, now.Month, now.Day, 0, 0, 0)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "add" 0
      parameters = [ Param.make "d" TDateTime ""; Param.make "seconds" TInt "" ]
      returnType = TDateTime
      description = "Returns a <type Date> <param seconds> seconds after <param d>"
      fn =
        (function
        | _, [ DDateTime d; DInt s ] ->
          d + (NodaTime.Period.FromSeconds s) |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    // Note: this was was previously named `Date::subtract_v0`.
    // A new Date::subtract_v1 was created to replace this, and subtract_v0 got
    // replaced with this ::subtractSeconds_v0. "Date::subtract" implies that
    // you are subtracting one date from another, so subtracting anything else
    // should include the name of the relevant unit in the fn name.
    { name = fn "DateTime" "subtractSeconds" 0
      parameters = [ Param.make "d" TDateTime ""; Param.make "seconds" TInt "" ]
      returnType = TDateTime
      description = "Returns a <type Date> <param seconds> seconds before <param d>"
      fn =
        (function
        | _, [ DDateTime d; DInt s ] ->
          d - (NodaTime.Period.FromSeconds s) |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "greaterThan" 0
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> > <param d2>}}"
      fn =
        (function
        | _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 > d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "lessThan" 0
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> < <param d2>}}"
      fn =
        (function
        | _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 < d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp("<")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "greaterThanOrEqualTo" 0
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> >= <param d2>}}"
      fn =
        (function
        | _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 >= d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp(">=")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "lessThanOrEqualTo" 0
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> <= <param d2>}}"
      fn =
        (function
        | _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 <= d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp("<=")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "toSeconds" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description =
        "Converts <param date> to an <type integer> representing seconds since the Unix epoch"
      fn =
        (function
        | _, [ DDateTime d ] ->
          (DarkDateTime.toInstant d).ToUnixTimeSeconds() |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "fromSeconds" 0
      parameters = [ Param.make "seconds" TInt "" ]
      returnType = TDateTime
      description =
        "Converts an <type integer> representing seconds since the Unix epoch into a <type Date>"
      fn =
        (function
        | _, [ DInt s ] ->
          s |> Instant.FromUnixTimeSeconds |> DarkDateTime.fromInstant |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "year" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the year portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDateTime d ] -> d.Year |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'year'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "month" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description =
        "Returns the month portion of <param date> as an <type int> between {{1}} and {{12}}"
      fn =
        (function
        | _, [ DDateTime d ] -> d.Month |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'month'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "day" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the day portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDateTime d ] -> d.Day |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'day'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "weekday" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description =
        "Returns the weekday of <param date> as an <type int>. Monday = {{1}}, Tuesday = {{2}}, ... Sunday = {{7}} (in accordance with ISO 8601)"
      fn =
        (function
        | _, [ DDateTime d ] -> d.DayOfWeek |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "hour" 2
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the hour portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDateTime d ] -> Ply(Dval.int d.Hour)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'hour'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "minute" 1
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the minute portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDateTime d ] -> Ply(Dval.int d.Minute)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'minute'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "second" 1
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the second portion of <param date> as an <type int>"
      fn =
        (function
        | _, [ DDateTime d ] -> Ply(Dval.int d.Second)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'second'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "DateTime" "atStartOfDay" 0
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TDateTime
      description = "Returns <type date> with the time set to midnight"
      fn =
        (function
        | _, [ DDateTime d ] ->
          DarkDateTime.T(d.Year, d.Month, d.Day, 0, 0, 0) |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_trunc", [ "'day'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    // TODO: when we have timespans in Dark, deprecate this
    // in favor of a fn which returns a timespan rather than seconds
    //
    // Note: the SQL here would be `EPOCH FROM (end - start)`, but we don't
    // currently support such a complex sqlSpec in Dark fns.
    { name = fn "DateTime" "subtract" 1
      parameters = [ Param.make "end" TDateTime ""; Param.make "start" TDateTime "" ]
      returnType = TInt
      description = "Returns the difference of the two dates, in seconds"
      fn =
        (function
        | _, [ DDateTime endDate; DDateTime startDate ] ->
          let diff = (DarkDateTime.toInstant endDate) - (DarkDateTime.toInstant startDate)
          diff.TotalSeconds |> System.Math.Round |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
