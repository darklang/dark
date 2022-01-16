module LibExecutionStdLib.LibDate

open System.Threading.Tasks
open FSharp.Control.Tasks
open LibExecution.RuntimeTypes
open Prelude

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "Date" "parse" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TDate
      description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns a Date"
      fn =

        (function
        | _, [ DStr s ] ->
          (try
            Ply(DDate(System.DateTime.ofIsoString s))
           with
           | e -> Ply(DError(SourceNone, "Invalid date format")))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "parse" 1) }


    { name = fn "Date" "parse" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TDate, TStr)
      description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the Date wrapped in a Result."
      fn =
        (function
        | _, [ DStr s ] ->
          (try
            Ply(DResult(Ok(DDate(System.DateTime.ofIsoString s))))
           with
           | e -> Ply(DResult(Error(DStr "Invalid date format"))))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "parse" 2) }


    { name = fn "Date" "parse" 2
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TDate, TStr)
      description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the Date wrapped in a Result."
      fn =
        (function
        | _, [ DStr s ] ->
          (try
            Ply(Dval.resultOk (DDate(System.DateTime.ofIsoString s)))
           with
           | e -> Ply(Dval.resultError (DStr "Invalid date format")))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toString" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TStr
      description =
        "Stringify `date` to the ISO 8601 format YYYY-MM-DD'T'hh:mm:ss'Z'"
      fn =
        (function
        | _, [ DDate d ] -> d.toIsoString () |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toStringISO8601BasicDateTime" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TStr
      description =
        "Stringify `date` to the ISO 8601 basic format YYYYMMDD'T'hhmmss'Z'"
      fn =
        (function
        | _, [ DDate d ] -> d.ToString("yyyyMMddTHHmmssZ") |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toStringISO8601BasicDate" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TStr
      description = "Stringify `date` to the ISO 8601 basic format YYYYMMDD"
      fn =
        (function
        | _, [ DDate d ] -> d.ToString("yyyyMMdd") |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "now" 0
      parameters = []
      returnType = TDate
      description = "Returns the current time."
      fn =
        (function
        | _, [] -> Ply(DDate(System.DateTime.Now))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Date" "today" 0
      parameters = []
      returnType = TDate
      description = "Returns the Date with the time set to midnight"
      fn =
        (function
        | _, [] ->
          let now = System.DateTime.Now
          Ply(DDate(System.DateTime(now.Year, now.Month, now.Day, 0, 0, 0)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "add" 0
      parameters = [ Param.make "d" TDate ""; Param.make "seconds" TInt "" ]
      returnType = TDate
      description = "Returns a new Date `seconds` seconds after `d`"
      fn =
        (function
        | _, [ DDate d; DInt s ] -> (Ply(DDate(d.AddSeconds(float s))))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "sub" 0
      parameters = [ Param.make "d" TDate ""; Param.make "seconds" TInt "" ]
      returnType = TDate
      description = "Returns a new Date `seconds` seconds before `d`"
      fn =
        (function
        | _, [ DDate d; DInt s ] -> (Ply(DDate(d.AddSeconds(float -s))))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable // As the OCaml one wasn't
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "subtract" 0) }


    { name = fn "Date" "subtract" 0
      parameters = [ Param.make "d" TDate ""; Param.make "seconds" TInt "" ]
      returnType = TDate
      description = "Returns a new Date `seconds` seconds before `d`"
      fn =
        (function
        | _, [ DDate d; DInt s ] -> (Ply(DDate(d.AddSeconds(float -s))))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "greaterThan" 0
      parameters = [ Param.make "d1" TDate ""; Param.make "d2" TDate "" ]
      returnType = TBool
      description = "Returns whether `d1` > ` d2`"
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
      description = "Returns whether `d1` < ` d2`"
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
      description = "Returns whether `d1` >= ` d2`"
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
      description = "Returns whether `d1` <= ` d2`"
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
        "Converts a Date `date` to an integer representing seconds since the Unix epoch"
      fn =
        (function
        | _, [ DDate d ] ->
          d
          |> System.DateTimeOffset
          |> (fun dto -> dto.ToUnixTimeSeconds())
          |> DInt
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "fromSeconds" 0

      parameters = [ Param.make "seconds" TInt "" ]
      returnType = TDate
      description =
        "Converts an integer representing seconds since the Unix epoch into a Date"
      fn =

        (function
        | _, [ DInt s ] ->
          s
          |> int64
          |> System.DateTimeOffset.FromUnixTimeSeconds
          |> (fun dto -> dto.DateTime)
          |> DDate
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "toHumanReadable" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TStr
      description = "Turn a Date into a human readable format"
      fn =
        (function
        | _, [ DDate date ] ->
          let time =
            date
            |> System.DateTimeOffset
            |> (fun dto -> dto.ToUnixTimeSeconds())
            |> float

          let msPerMinute = 60.0 * 1000.0
          let msPerHour = msPerMinute * 60.0
          let msPerDay = msPerHour * 24.0
          let msPerMonth = msPerDay * 30.0
          let msPerYear = msPerDay * 365.0

          let rec f time =
            if time / msPerYear > 1.0 then
              let suffix = if time / msPerYear > 2.0 then "years" else "year"

              ((time / msPerYear |> int |> string) + " " + suffix + ", ")
              + f (time % msPerYear)
            else if time / msPerMonth > 1.0 then
              let suffix = if time / msPerMonth > 2.0 then "months" else "month"

              ((time / msPerMonth |> int |> string) + " " + suffix + ", ")
              + f (time % msPerMonth)
            else if time / msPerDay > 1.0 then
              let suffix = if time / msPerDay > 2.0 then "days" else "day"

              ((time / msPerDay |> int |> string) + " " + suffix + ", ")
              + f (time % msPerDay)
            else if time / msPerHour > 1.0 then
              let suffix = if time / msPerHour > 2.0 then "hours" else "hour"

              ((time / msPerHour |> int |> string) + " " + suffix + ", ")
              + f (time % msPerHour)
            else if time / msPerMinute > 1.0 then
              let suffix = if time / msPerMinute > 2.0 then "minutes" else "minute"

              ((time / msPerMinute |> int |> string) + " " + suffix)
              + f (time % msPerMinute)
            else
              ""

          let diff = f time
          let result = if diff = "" then "less than a minute" else diff
          Ply(DStr result)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = DeprecatedBecause "This function doesn't work" }


    { name = fn "Date" "year" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the year portion of the Date as an int"
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
        "Returns the month portion of the Date as an int between 1 and 12"
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
      description = "Returns the day portion of the Date as an int"
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
        "Returns the weekday of `date` as an int. Monday = 1, Tuesday = 2, ... Sunday = 7 (in accordance with ISO 8601)."
      fn =
        (function
        | _, [ DDate d ] ->
          let day = d.DayOfWeek
          let day = if day = System.DayOfWeek.Sunday then 7 else int day
          day |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "hour" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the hour portion of the Date as an int"
      fn =
        (function
        | _, [ DDate d ] ->
          // This is wrong, hence being replaced
          ((d - System.DateTime.UnixEpoch).TotalHours % 60.0)
          |> int
          |> Dval.int
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "Date" "hour" 1) }


    { name = fn "Date" "hour" 1
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the hour portion of the Date as an int"
      fn =
        (function
        | _, [ DDate d ] ->
          // CLEANUP - this was made bug-for-bug compatible
          let s = if d.Year < 1970 then d.Hour - 23 else d.Hour
          Ply(Dval.int s)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'hour'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "minute" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the minute portion of the Date as an int"
      fn =
        (function
        | _, [ DDate d ] ->
          // CLEANUP - this was made bug-for-bug compatible
          let s =
            if d.Year < 1970 then
              if d.Second = 0 then (d.Minute - 60) % 60 else d.Minute - 59
            else
              d.Minute

          Ply(Dval.int s)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'minute'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "second" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TInt
      description = "Returns the second portion of the Date as an int"
      fn =
        (function
        | _, [ DDate d ] ->
          // CLEANUP - this was made bug-for-bug compatible
          let s = if d.Year < 1970 then (d.Second - 60) % 60 else d.Second
          Ply(Dval.int s)
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_part", [ "'second'" ])
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Date" "atStartOfDay" 0
      parameters = [ Param.make "date" TDate "" ]
      returnType = TDate
      description = "Returns the Date with the time set to midnight"
      fn =
        (function
        | _, [ DDate d ] ->
          System.DateTime(d.Year, d.Month, d.Day, 0, 0, 0) |> DDate |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunctionWithPrefixArgs("date_trunc", [ "'day'" ])
      previewable = Pure
      deprecated = NotDeprecated } ]
