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
        "Parses a string representing a date in the ISO format and returns a Date"
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
        "Parses a string representing a date in the ISO format and returns a Date"
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
    ; dep = false } ]
