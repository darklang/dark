module StdLibExecution.Libs.DateTime

type Instant = NodaTime.Instant

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

let ISO8601Format = "yyyy-MM-ddTHH:mm:ssZ"

let ISO8601DateParser (s : string) : Result<DarkDateTime.T, unit> =

  let culture = System.Globalization.CultureInfo.InvariantCulture
  let styles = System.Globalization.DateTimeStyles.AssumeUniversal
  let mutable (result : System.DateTime) = Unchecked.defaultof<System.DateTime>
  match s with
  | date when date.Contains("GMT") -> Error()
  | date when date.EndsWith('z') -> Error()
  | date when
    System.DateTime.TryParseExact(date, ISO8601Format, culture, styles, &result)
    ->
    Ok(DarkDateTime.fromDateTime (result.ToUniversalTime()))
  | _ -> Error()

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "DateTime" ]

let fns : List<BuiltInFn> =
  [ { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TResult(TDateTime, TString)
      description =
        "Parses a string representing a date and time in the ISO 8601 format exactly {{"
        + ISO8601Format
        + "}} (for example: 2019-09-07T22:44:25Z) and returns the {{Date}} wrapped in a {{Result}}."
      fn =
        (function
        | _, _, [ DString s ] ->
          ISO8601DateParser s
          |> Result.map DDateTime
          |> Result.mapError (fun () -> DString "Invalid date format")
          |> DResult
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TString
      description =
        "Stringify <param date> to the ISO 8601 format {{YYYY-MM-DD'T'hh:mm:ss'Z'}}"
      fn =
        (function
        | _, _, [ DDateTime d ] ->
          let dt = DarkDateTime.toDateTimeUtc d
          dt.ToString("s", System.Globalization.CultureInfo.InvariantCulture) + "Z"
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toStringISO8601BasicDateTime" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TString
      description =
        "Stringify <param date> to the ISO 8601 basic format {{YYYYMMDD'T'hhmmss'Z'}}"
      fn =
        (function
        | _, _, [ DDateTime d ] ->
          (DarkDateTime.toDateTimeUtc d).ToString("yyyyMMddTHHmmssZ")
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toStringISO8601BasicDate" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TString
      description = "Stringify <param date> to the ISO 8601 basic format YYYYMMDD"
      fn =
        (function
        | _, _, [ DDateTime d ] ->
          (DarkDateTime.toDateTimeUtc d).ToString("yyyyMMdd") |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "now" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDateTime
      description = "Returns the current time"
      fn =
        (function
        | _, _, [ DUnit ] ->
          Instant.now () |> DarkDateTime.fromInstant |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "today" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDateTime
      description = "Returns the <type Date> with the time set to midnight"
      fn =
        (function
        | _, _, [ DUnit ] ->
          let now = DarkDateTime.fromInstant (Instant.now ())
          Ply(DDateTime(DarkDateTime.T(now.Year, now.Month, now.Day, 0, 0, 0)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "addSeconds" 0
      typeParams = []
      parameters = [ Param.make "d" TDateTime ""; Param.make "seconds" TInt "" ]
      returnType = TDateTime
      description = "Returns a <type Date> <param seconds> seconds after <param d>"
      fn =
        (function
        | _, _, [ DDateTime d; DInt s ] ->
          d + (NodaTime.Period.FromSeconds s) |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    // Note: this was was previously named `Date.subtract_v0`.
    // A new DateTime.subtract_v1 was created to replace this, and subtract_v0 got
    // replaced with this .subtractSeconds_v0. "DateTime.subtract" implies that
    // you are subtracting one date from another, so subtracting anything else
    // should include the name of the relevant unit in the fn name.
    { name = fn "subtractSeconds" 0
      typeParams = []
      parameters = [ Param.make "d" TDateTime ""; Param.make "seconds" TInt "" ]
      returnType = TDateTime
      description = "Returns a <type Date> <param seconds> seconds before <param d>"
      fn =
        (function
        | _, _, [ DDateTime d; DInt s ] ->
          d - (NodaTime.Period.FromSeconds s) |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> > <param d2>}}"
      fn =
        (function
        | _, _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 > d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> < <param d2>}}"
      fn =
        (function
        | _, _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 < d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp("<")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> >= <param d2>}}"
      fn =
        (function
        | _, _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 >= d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp(">=")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "d1" TDateTime ""; Param.make "d2" TDateTime "" ]
      returnType = TBool
      description = "Returns whether {{<param d1> <= <param d2>}}"
      fn =
        (function
        | _, _, [ DDateTime d1; DDateTime d2 ] -> Ply(DBool(d1 <= d2))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp("<=")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toSeconds" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description =
        "Converts <param date> to an <type Int> representing seconds since the Unix epoch"
      fn =
        (function
        | _, _, [ DDateTime d ] ->
          (DarkDateTime.toInstant d).ToUnixTimeSeconds() |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromSeconds" 0
      typeParams = []
      parameters = [ Param.make "seconds" TInt "" ]
      returnType = TDateTime
      description =
        "Converts an <type Int> representing seconds since the Unix epoch into a <type Date>"
      fn =
        (function
        | _, _, [ DInt s ] ->
          s
          |> Instant.FromUnixTimeSeconds
          |> DarkDateTime.fromInstant
          |> DDateTime
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "year" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the year portion of <param date> as an <type Int>"
      fn =
        (function
        | _, _, [ DDateTime d ] -> d.Year |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'year'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "month" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description =
        "Returns the month portion of <param date> as an <type Int> between {{1}} and {{12}}"
      fn =
        (function
        | _, _, [ DDateTime d ] -> d.Month |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'month'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "day" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the day portion of <param date> as an <type Int>"
      fn =
        (function
        | _, _, [ DDateTime d ] -> d.Day |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'day'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "weekday" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description =
        "Returns the weekday of <param date> as an <type Int>. Monday = {{1}}, Tuesday = {{2}}, ... Sunday = {{7}} (in accordance with ISO 8601)"
      fn =
        (function
        | _, _, [ DDateTime d ] -> d.DayOfWeek |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "hour" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the hour portion of <param date> as an <type Int>"
      fn =
        (function
        | _, _, [ DDateTime d ] -> Ply(Dval.int d.Hour)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'hour'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "minute" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the minute portion of <param date> as an <type Int>"
      fn =
        (function
        | _, _, [ DDateTime d ] -> Ply(Dval.int d.Minute)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'minute'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "second" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description = "Returns the second portion of <param date> as an <type Int>"
      fn =
        (function
        | _, _, [ DDateTime d ] -> Ply(Dval.int d.Second)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'second'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "atStartOfDay" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TDateTime
      description = "Returns <type date> with the time set to midnight"
      fn =
        (function
        | _, _, [ DDateTime d ] ->
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
    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "end" TDateTime ""; Param.make "start" TDateTime "" ]
      returnType = TInt
      description = "Returns the difference of the two dates, in seconds"
      fn =
        (function
        | _, _, [ DDateTime endDate; DDateTime startDate ] ->
          let diff =
            (DarkDateTime.toInstant endDate) - (DarkDateTime.toInstant startDate)
          diff.TotalSeconds |> System.Math.Round |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
