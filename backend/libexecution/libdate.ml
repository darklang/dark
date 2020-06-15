open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Types.fluid_expr fn list =
  [ { prefix_names = ["Date::parse"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TDate
    ; description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns a Date"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
            ( try
                DDate
                  (Stdlib_util.date_of_isostring (Unicode_string.to_string s))
              with e -> RT.error "Invalid date format" )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Date::parse_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TResult
    ; description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the Date wrapped in a Result."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
            ( try
                DResult
                  (ResOk
                     (DDate
                        (Stdlib_util.date_of_isostring
                           (Unicode_string.to_string s))))
              with e ->
                DResult
                  (ResError (Dval.dstr_of_string_exn "Invalid date format")) )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Date::parse_v2"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TResult
    ; description =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns the Date wrapped in a Result."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
            ( try
                Dval.to_res_ok
                  (DDate
                     (Stdlib_util.date_of_isostring
                        (Unicode_string.to_string s)))
              with e ->
                Dval.to_res_err (Dval.dstr_of_string_exn "Invalid date format")
            )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::toString"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TStr
    ; description =
        "Stringify `date` to the ISO 8601 format YYYY-MM-DD'T'hh:mm:ss'Z'"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              Dval.dstr_of_string_exn (Stdlib_util.isostring_of_date d)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::toStringISO8601BasicDateTime"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TStr
    ; description =
        "Stringify `date` to the ISO 8601 basic format YYYYMMDD'T'hhmmss'Z'"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              Dval.dstr_of_string_exn
                (Stdlib_util.isostring_of_date_basic_datetime d)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::toStringISO8601BasicDate"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TStr
    ; description = "Stringify `date` to the ISO 8601 basic format YYYYMMDD"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              Dval.dstr_of_string_exn
                (Stdlib_util.isostring_of_date_basic_date d)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::now"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TDate
    ; description = "Returns the current time."
    ; func =
        InProcess (function _, [] -> DDate (Time.now ()) | args -> fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["Date::today"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TDate
    ; description = "Returns the Date with the time set to midnight"
    ; func =
        InProcess
          (function
          | _, [] ->
              Time.now ()
              |> Time.to_date ~zone:Time.Zone.utc
              |> (fun x ->
                   Time.of_date_ofday Time.Zone.utc x Time.Ofday.start_of_day)
              |> DDate
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::add"]
    ; infix_names = []
    ; parameters = [par "d" TDate; par "seconds" TInt]
    ; return_type = TDate
    ; description = "Returns a new Date `seconds` seconds after `d`"
    ; func =
        InProcess
          (function
          | _, [DDate d; DInt s] ->
              DDate (Time.add d (Time.Span.of_int_sec (Dint.to_int_exn s)))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::sub"]
    ; infix_names = []
    ; parameters = [par "d" TDate; par "seconds" TInt]
    ; return_type = TDate
    ; description = "Returns a new Date `seconds` seconds before `d`"
    ; func =
        InProcess
          (function
          | _, [DDate d; DInt s] ->
              DDate (Time.sub d (Time.Span.of_int_sec (Dint.to_int_exn s)))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Date::subtract"]
    ; infix_names = []
    ; parameters = [par "d" TDate; par "seconds" TInt]
    ; return_type = TDate
    ; description = "Returns a new Date `seconds` seconds before `d`"
    ; func =
        InProcess
          (function
          | _, [DDate d; DInt s] ->
              DDate (Time.sub d (Time.Span.of_int_sec (Dint.to_int_exn s)))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::greaterThan"]
    ; infix_names = ["Date::>"]
    ; parameters = [par "d1" TDate; par "d2" TDate]
    ; return_type = TBool
    ; description = "Returns whether `d1` > ` d2`"
    ; func =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( > ) d1 d2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::lessThan"]
    ; infix_names = ["Date::<"]
    ; parameters = [par "d1" TDate; par "d2" TDate]
    ; return_type = TBool
    ; description = "Returns whether `d1` < ` d2`"
    ; func =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( < ) d1 d2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::greaterThanOrEqualTo"]
    ; infix_names = ["Date::>="]
    ; parameters = [par "d1" TDate; par "d2" TDate]
    ; return_type = TBool
    ; description = "Returns whether `d1` >= ` d2`"
    ; func =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( >= ) d1 d2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::lessThanOrEqualTo"]
    ; infix_names = ["Date::<="]
    ; parameters = [par "d1" TDate; par "d2" TDate]
    ; return_type = TBool
    ; description = "Returns whether `d1` <= ` d2`"
    ; func =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( <= ) d1 d2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::toSeconds"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description =
        "Converts a Date `date` to an integer representing seconds since the Unix epoch"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_span_since_epoch
              |> Time.Span.to_sec
              |> Float.iround_exn
              |> Dval.dint
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::fromSeconds"]
    ; infix_names = []
    ; parameters = [par "seconds" TInt]
    ; return_type = TDate
    ; description =
        "Converts an integer representing seconds since the Unix epoch into a Date"
    ; func =
        InProcess
          (function
          | _, [DInt s] ->
              s
              |> Dint.to_int63
              |> Time.Span.of_int63_seconds
              |> Time.of_span_since_epoch
              |> DDate
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::toHumanReadable"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TStr
    ; description = "Turn a Date into a human readable format"
    ; func =
        InProcess
          (function
          | _, [DDate date] ->
              let time = date |> Time.to_span_since_epoch |> Time.Span.to_sec in
              let msPerMinute = 60.0 *. 1000.0 in
              let msPerHour = msPerMinute *. 60.0 in
              let msPerDay = msPerHour *. 24.0 in
              let msPerMonth = msPerDay *. 30.0 in
              let msPerYear = msPerDay *. 365.0 in
              let rec f time =
                if time /. msPerYear > 1.0
                then
                  let suffix =
                    if time /. msPerYear > 2.0 then "years" else "year"
                  in
                  ( (time /. msPerYear |> int_of_float |> string_of_int)
                  ^ " "
                  ^ suffix
                  ^ ", " )
                  ^ f (Float.mod_float time msPerYear)
                else if time /. msPerMonth > 1.0
                then
                  let suffix =
                    if time /. msPerMonth > 2.0 then "months" else "month"
                  in
                  ( (time /. msPerMonth |> int_of_float |> string_of_int)
                  ^ " "
                  ^ suffix
                  ^ ", " )
                  ^ f (Float.mod_float time msPerMonth)
                else if time /. msPerDay > 1.0
                then
                  let suffix =
                    if time /. msPerDay > 2.0 then "days" else "day"
                  in
                  ( (time /. msPerDay |> int_of_float |> string_of_int)
                  ^ " "
                  ^ suffix
                  ^ ", " )
                  ^ f (Float.mod_float time msPerDay)
                else if time /. msPerHour > 1.0
                then
                  let suffix =
                    if time /. msPerHour > 2.0 then "hours" else "hour"
                  in
                  ( (time /. msPerHour |> int_of_float |> string_of_int)
                  ^ " "
                  ^ suffix
                  ^ ", " )
                  ^ f (Float.mod_float time msPerHour)
                else if time /. msPerMinute > 1.0
                then
                  let suffix =
                    if time /. msPerMinute > 2.0 then "minutes" else "minute"
                  in
                  ( (time /. msPerMinute |> int_of_float |> string_of_int)
                  ^ " "
                  ^ suffix )
                  ^ f (Float.mod_float time msPerMinute)
                else ""
              in
              let diff = f time in
              let diff = if diff = "" then "less than a minute" else diff in
              Dval.dstr_of_string_exn diff
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true (* This doesn't mean anything *) }
  ; { prefix_names = ["Date::year"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description = "Returns the year portion of the Date as an int"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d |> Time.to_date ~zone:Time.Zone.utc |> Date.year |> Dval.dint
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::month"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description =
        "Returns the month portion of the Date as an int between 1 and 12"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_date ~zone:Time.Zone.utc
              |> Date.month
              |> Month.to_int
              |> Dval.dint
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::day"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description = "Returns the day portion of the Date as an int"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d |> Time.to_date ~zone:Time.Zone.utc |> Date.day |> Dval.dint
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::weekday"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description =
        "Returns the weekday of `date` as an int. Monday = 1, Tuesday = 2, ... Sunday = 7 (in accordance with ISO 8601)."
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_date ~zone:Time.Zone.utc
              |> Date.day_of_week
              |> Day_of_week.iso_8601_weekday_number
              |> Dval.dint
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::hour"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description = "Returns the hour portion of the Date as an int"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_span_since_epoch
              |> Time.Span.to_hr
              |> (fun x -> Float.mod_float x 60.0)
              |> Dint.of_float
              |> DInt
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Date::hour_v1"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description = "Returns the hour portion of the Date as an int"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_span_since_epoch
              |> Time.Span.to_hr
              |> (fun x -> Float.mod_float x 24.0)
              |> Dint.of_float
              |> DInt
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::minute"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description = "Returns the minute portion of the Date as an int"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_span_since_epoch
              |> Time.Span.to_min
              |> (fun x -> Float.mod_float x 60.0)
              |> Dint.of_float
              |> DInt
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::second"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TInt
    ; description = "Returns the second portion of the Date as an int"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_span_since_epoch
              |> Time.Span.to_sec
              |> (fun x -> Float.mod_float x 60.0)
              |> Dint.of_float
              |> DInt
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Date::atStartOfDay"]
    ; infix_names = []
    ; parameters = [par "date" TDate]
    ; return_type = TDate
    ; description = "Returns the Date with the time set to midnight"
    ; func =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_date ~zone:Time.Zone.utc
              |> (fun x ->
                   Time.of_date_ofday Time.Zone.utc x Time.Ofday.start_of_day)
              |> DDate
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
