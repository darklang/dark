module LibExecution.StdLib.LibDate

open System.Threading.Tasks
open FSharp.Control.Tasks
open LibExecution.RuntimeTypes
open FSharpPlus
open Prelude

let fn = FQFnName.stdlibName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [
    // [ { name = fn "Date" "parse" 0
//     parameters = [ Param.make "s" TStr ]
//     returnType = TDate
//     description =
//       "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns a Date"
//     fn =
//
//       (function
//       | _, [ DStr s ] ->
//           (try
//             DDate(Stdlib_util.date_of_isostring (Unicode_string.to_string s))
//            with e -> RT.error "Invalid date format")
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = ReplacedBy(fn "" "" 0) }
//   { name = fn "Date" "parse" 1
//
//     parameters = [ Param.make "s" TStr ]
//     returnType = TResult
//     description =
//       "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the Date wrapped in a Result."
//     fn =
//
//       (function
//       | _, [ DStr s ] ->
//           (try
//             DResult(
//               ResOk(
//                 DDate(Stdlib_util.date_of_isostring (Unicode_string.to_string s))
//               )
//             )
//            with e ->
//              DResult(ResError(Dval.dstr_of_string_exn "Invalid date format")))
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "Date" "parse" 2
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TDate, TStr)
      description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the Date wrapped in a Result."
      fn =
        InProcess
          (function
          | _, [ DStr s ] ->
              (try
                Value(Dval.resultOk (DDate(System.DateTime.ofIsoString s)))
               with e -> Value(Dval.resultError (DStr "Invalid date format")))
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
//   { name = fn "Date" "toString" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TStr
//     description =
//       "Stringify `date` to the ISO 8601 format YYYY-MM-DD'T'hh:mm:ss'Z'"
//     fn =
//
//       (function
//       | _, [ DDate d ] -> Dval.dstr_of_string_exn (Stdlib_util.isostring_of_date d)
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "toStringISO8601BasicDateTime" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TStr
//     description =
//       "Stringify `date` to the ISO 8601 basic format YYYYMMDD'T'hhmmss'Z'"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           Dval.dstr_of_string_exn (Stdlib_util.isostring_of_date_basic_datetime d)
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "toStringISO8601BasicDate" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TStr
//     description = "Stringify `date` to the ISO 8601 basic format YYYYMMDD"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           Dval.dstr_of_string_exn (Stdlib_util.isostring_of_date_basic_date d)
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "now" 0
//
//     parameters = []
//     returnType = TDate
//     description = "Returns the current time."
//     fn =
//       (function
//       | _, [] -> DDate(Time.now ())
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Impure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "today" 0
//
//     parameters = []
//     returnType = TDate
//     description = "Returns the Date with the time set to midnight"
//     fn =
//
//       (function
//       | _, [] ->
//           Time.now ()
//           |> Time.to_date Time.Zone.utc
//           |> (fun x -> Time.of_date_ofday Time.Zone.utc x Time.Ofday.start_of_day)
//           |> DDate
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "add" 0
//
//     parameters = [ Param.make "d" TDate; Param.make "seconds" TInt ]
//     returnType = TDate
//     description = "Returns a new Date `seconds` seconds after `d`"
//     fn =
//
//       (function
//       | _, [ DDate d; DInt s ] ->
//           DDate(Time.add d (Time.Span.of_int_sec (Dint.to_int_exn s)))
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "sub" 0
//
//     parameters = [ Param.make "d" TDate; Param.make "seconds" TInt ]
//     returnType = TDate
//     description = "Returns a new Date `seconds` seconds before `d`"
//     fn =
//
//       (function
//       | _, [ DDate d; DInt s ] ->
//           DDate(Time.sub d (Time.Span.of_int_sec (Dint.to_int_exn s)))
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = ReplacedBy(fn "" "" 0) }
//   { name = fn "Date" "subtract" 0
//
//     parameters = [ Param.make "d" TDate; Param.make "seconds" TInt ]
//     returnType = TDate
//     description = "Returns a new Date `seconds` seconds before `d`"
//     fn =
//
//       (function
//       | _, [ DDate d; DInt s ] ->
//           DDate(Time.sub d (Time.Span.of_int_sec (Dint.to_int_exn s)))
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "greaterThan" 0
//     infix_names = [ "Date::>" ]
//     parameters = [ Param.make "d1" TDate; Param.make "d2" TDate ]
//     returnType = TBool
//     description = "Returns whether `d1` > ` d2`"
//     fn =
//
//       (function
//       | _, [ DDate d1; DDate d2 ] -> DBool(Time.op_GreaterThan d1 d2)
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "lessThan" 0
//     infix_names = [ "Date::<" ]
//     parameters = [ Param.make "d1" TDate; Param.make "d2" TDate ]
//     returnType = TBool
//     description = "Returns whether `d1` < ` d2`"
//     fn =
//
//       (function
//       | _, [ DDate d1; DDate d2 ] -> DBool(Time.op_LessThan d1 d2)
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "greaterThanOrEqualTo" 0
//     infix_names = [ "Date::>=" ]
//     parameters = [ Param.make "d1" TDate; Param.make "d2" TDate ]
//     returnType = TBool
//     description = "Returns whether `d1` >= ` d2`"
//     fn =
//
//       (function
//       | _, [ DDate d1; DDate d2 ] -> DBool(Time.op_GreaterThanOrEqual d1 d2)
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "lessThanOrEqualTo" 0
//     infix_names = [ "Date::<=" ]
//     parameters = [ Param.make "d1" TDate; Param.make "d2" TDate ]
//     returnType = TBool
//     description = "Returns whether `d1` <= ` d2`"
//     fn =
//
//       (function
//       | _, [ DDate d1; DDate d2 ] -> DBool(Time.op_LessThanOrEqual d1 d2)
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "toSeconds" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description =
//       "Converts a Date `date` to an integer representing seconds since the Unix epoch"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_span_since_epoch
//           |> Time.Span.to_sec
//           |> Float.iround_exn
//           |> Dval.dint
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "fromSeconds" 0
//
//     parameters = [ Param.make "seconds" TInt ]
//     returnType = TDate
//     description =
//       "Converts an integer representing seconds since the Unix epoch into a Date"
//     fn =
//
//       (function
//       | _, [ DInt s ] ->
//           s
//           |> Dint.to_int63
//           |> Time.Span.of_int63_seconds
//           |> Time.of_span_since_epoch
//           |> DDate
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "toHumanReadable" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TStr
//     description = "Turn a Date into a human readable format"
//     fn =
//
//       (function
//       | _, [ DDate date ] ->
//           let time = date |> Time.to_span_since_epoch |> Time.Span.to_sec in
//           let msPerMinute = 60.0 *. 1000.0 in
//           let msPerHour = msPerMinute *. 60.0 in
//           let msPerDay = msPerHour *. 24.0 in
//           let msPerMonth = msPerDay *. 30.0 in
//           let msPerYear = msPerDay *. 365.0 in
//
//           let rec f time =
//             if time /. msPerYear > 1.0 then
//               let suffix = if time /. msPerYear > 2.0 then "years" else "year"
//
//               ((time /. msPerYear |> int_of_float |> string_of_int)
//                ^ " " ^ suffix ^ ", ")
//               ^ f (Float.mod_float time msPerYear)
//             else if time /. msPerMonth > 1.0 then
//               let suffix = if time /. msPerMonth > 2.0 then "months" else "month"
//
//               ((time /. msPerMonth |> int_of_float |> string_of_int)
//                ^ " " ^ suffix ^ ", ")
//               ^ f (Float.mod_float time msPerMonth)
//             else if time /. msPerDay > 1.0 then
//               let suffix = if time /. msPerDay > 2.0 then "days" else "day"
//
//               ((time /. msPerDay |> int_of_float |> string_of_int)
//                ^ " " ^ suffix ^ ", ")
//               ^ f (Float.mod_float time msPerDay)
//             else if time /. msPerHour > 1.0 then
//               let suffix = if time /. msPerHour > 2.0 then "hours" else "hour"
//
//               ((time /. msPerHour |> int_of_float |> string_of_int)
//                ^ " " ^ suffix ^ ", ")
//               ^ f (Float.mod_float time msPerHour)
//             else if time /. msPerMinute > 1.0 then
//               let suffix =
//                 if time /. msPerMinute > 2.0 then "minutes" else "minute"
//
//               ((time /. msPerMinute |> int_of_float |> string_of_int)
//                ^ " " ^ suffix)
//               ^ f (Float.mod_float time msPerMinute)
//             else
//               ""
//
//           let diff = f time in
//           let diff = if diff = "" then "less than a minute" else diff in
//           Dval.dstr_of_string_exn diff
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = ReplacedBy(fn "" "" 0) (* This doesn't mean anything *)  }
//   { name = fn "Date" "year" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description = "Returns the year portion of the Date as an int"
//     fn =
//
//       (function
//       | _, [ DDate d ] -> d |> Time.to_date Time.Zone.utc |> Date.year |> Dval.dint
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "month" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description =
//       "Returns the month portion of the Date as an int between 1 and 12"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_date Time.Zone.utc
//           |> Date.month
//           |> Month.to_int
//           |> Dval.dint
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "day" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description = "Returns the day portion of the Date as an int"
//     fn =
//
//       (function
//       | _, [ DDate d ] -> d |> Time.to_date Time.Zone.utc |> Date.day |> Dval.dint
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "weekday" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description =
//       "Returns the weekday of `date` as an int. Monday = 1, Tuesday = 2, ... Sunday = 7 (in accordance with ISO 8601)."
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_date Time.Zone.utc
//           |> Date.day_of_week
//           |> Day_of_week.iso_8601_weekday_number
//           |> Dval.dint
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "hour" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description = "Returns the hour portion of the Date as an int"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_span_since_epoch
//           |> Time.Span.to_hr
//           |> (fun x -> Float.mod_float x 60.0)
//           |> Dint.of_float
//           |> DInt
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = ReplacedBy(fn "" "" 0) }
//   { name = fn "Date" "hour" 1
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description = "Returns the hour portion of the Date as an int"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_span_since_epoch
//           |> Time.Span.to_hr
//           |> (fun x -> Float.mod_float x 24.0)
//           |> Dint.of_float
//           |> DInt
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "minute" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description = "Returns the minute portion of the Date as an int"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_span_since_epoch
//           |> Time.Span.to_min
//           |> (fun x -> Float.mod_float x 60.0)
//           |> Dint.of_float
//           |> DInt
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "second" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TInt
//     description = "Returns the second portion of the Date as an int"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_span_since_epoch
//           |> Time.Span.to_sec
//           |> (fun x -> Float.mod_float x 60.0)
//           |> Dint.of_float
//           |> DInt
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated }
//   { name = fn "Date" "atStartOfDay" 0
//
//     parameters = [ Param.make "date" TDate ]
//     returnType = TDate
//     description = "Returns the Date with the time set to midnight"
//     fn =
//
//       (function
//       | _, [ DDate d ] ->
//           d
//           |> Time.to_date Time.Zone.utc
//           |> (fun x -> Time.of_date_ofday Time.Zone.utc x Time.Ofday.start_of_day)
//           |> DDate
//       | args -> incorrectArgs ())
//     sqlSpec = NotYetImplementedTODO
//     previewable = Pure
//     deprecated = NotDeprecated } ]
//
