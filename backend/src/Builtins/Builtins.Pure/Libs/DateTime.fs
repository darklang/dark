module Builtins.Pure.Libs.DateTime

type Instant = NodaTime.Instant

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module Dval = LibExecution.Dval
module DarkDateTime = LibExecution.DarkDateTime


/// NodaTime rejects instants/periods outside its supported range by throwing
/// host exceptions, even for values that fit int64. Run `f` and turn those into
/// a Dark `OutOfRange` error rather than letting them escape.
let private inDateRange (vm : VMState) (f : unit -> Dval) : Ply<Dval> =
  try
    f () |> Ply
  with
  | :? System.ArgumentOutOfRangeException
  | :? System.OverflowException ->
    RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID


let fns () : List<BuiltInFn> =
  [ { name = fn "dateTimeFromUnixTicks" 0
      typeParams = []
      parameters = [ Param.make "ticks" TInt "100ns ticks since Unix epoch" ]
      returnType = TDateTime
      description = "Pure DateTime representation conversion; no clock access"
      fn =
        (function
        | _, vm, _, [| DInt ticks |] ->
          inDateRange vm (fun () ->
            intToInt64 vm ticks
            |> Instant.FromUnixTimeTicks
            |> DarkDateTime.fromInstant
            |> DDateTime)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dateTimeToUnixTicks" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TInt
      description =
        "100ns ticks since the Unix epoch; the inverse of <fn dateTimeFromUnixTicks>"
      fn =
        (function
        | _, _, _, [| DDateTime d |] ->
          (DarkDateTime.toInstant d).ToUnixTimeTicks() |> bigint |> Dval.int |> Ply
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
          let dt = DarkDateTime.toDateTimeUtc d
          let hasMs = dt.Millisecond <> 0

          let str =
            if hasMs then
              dt.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
            else
              dt.ToString("s", System.Globalization.CultureInfo.InvariantCulture)
              + "Z"

          str |> DString |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dateTimeToStringISO8601BasicDateTime" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TString
      description =
        "Stringify <param date> to the ISO 8601 basic format {{YYYYMMDD'T'hhmmss'Z'}}"
      fn =
        (function
        | _, _, _, [| DDateTime d |] ->
          (DarkDateTime.toDateTimeUtc d).ToString("yyyyMMddTHHmmssZ")
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dateTimeToStringISO8601BasicDate" 0
      typeParams = []
      parameters = [ Param.make "date" TDateTime "" ]
      returnType = TString
      description = "Stringify <param date> to the ISO 8601 basic format YYYYMMDD"
      fn =
        (function
        | _, _, _, [| DDateTime d |] ->
          (DarkDateTime.toDateTimeUtc d).ToString("yyyyMMdd") |> DString |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
