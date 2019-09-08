open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["Date::parse"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TDate
    ; d =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns a Date"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
            ( try DDate (Util.date_of_isostring (Unicode_string.to_string s))
              with e -> RT.error "Invalid date format" )
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Date::parse_v1"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TResult
    ; d =
        "Parses a string representing a date and time in the ISO 8601 format (for example: 2019-09-07T22:44:25Z) and returns a Date"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
            ( try
                DResult
                  (ResOk
                     (DDate
                        (Util.date_of_isostring (Unicode_string.to_string s))))
              with e ->
                DResult
                  (ResError (Dval.dstr_of_string_exn "Invalid date format")) )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::toString"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TResult
    ; d = "Print a string in the ISO format"
    ; f =
        InProcess
          (function
          | _, [DDate d] ->
              Dval.dstr_of_string_exn (Util.isostring_of_date d)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::now"]
    ; ins = []
    ; p = []
    ; r = TDate
    ; d =
        "Returns the number of seconds since the epoch (midnight, Jan 1, 1970)"
    ; f =
        InProcess (function _, [] -> DDate (Time.now ()) | args -> fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["Date::add"]
    ; ins = []
    ; p = [par "d" TDate; par "seconds" TInt]
    ; r = TDate
    ; d = "Returns a new Date `seconds` seconds after `d`"
    ; f =
        InProcess
          (function
          | _, [DDate d; DInt s] ->
              DDate (Time.add d (Time.Span.of_int_sec (Dint.to_int_exn s)))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::sub"]
    ; ins = []
    ; p = [par "d" TDate; par "seconds" TInt]
    ; r = TDate
    ; d = "Returns a new Date `seconds` seconds before `d`"
    ; f =
        InProcess
          (function
          | _, [DDate d; DInt s] ->
              DDate (Time.sub d (Time.Span.of_int_sec (Dint.to_int_exn s)))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::greaterThan"]
    ; ins = ["Date::>"]
    ; p = [par "d1" TDate; par "d2" TDate]
    ; r = TBool
    ; d = "Returns whether `d1` > ` d2`"
    ; f =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( > ) d1 d2)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::lessThan"]
    ; ins = ["Date::<"]
    ; p = [par "d1" TDate; par "d2" TDate]
    ; r = TBool
    ; d = "Returns whether `d1` < ` d2`"
    ; f =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( < ) d1 d2)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::toSeconds"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d =
        "Converts a Date `date` to an integer representing seconds since the Unix epoch"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::toHumanReadable"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TStr
    ; d = "Turn a Date into a human readable format"
    ; f =
        InProcess
          (function
          | _, [DDate date] ->
              let time =
                date |> Time.to_span_since_epoch |> Time.Span.to_sec
              in
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::year"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d = "Returns the year portion of the Date as an int"
    ; f =
        InProcess
          (function
          | _, [DDate d] ->
              d |> Time.to_date ~zone:Time.Zone.utc |> Date.year |> Dval.dint
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::month"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d = "Returns the month portion of the Date as an int between 1 and 12"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::day"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d = "Returns the day portion of the Date as an int"
    ; f =
        InProcess
          (function
          | _, [DDate d] ->
              d |> Time.to_date ~zone:Time.Zone.utc |> Date.day |> Dval.dint
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::hour"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d = "Returns the hour portion of the Date as an int"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::minute"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d = "Returns the minute portion of the Date as an int"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::second"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d = "Returns the second portion of the Date as an int"
    ; f =
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
    ; ps = true
    ; dep = false } ]
