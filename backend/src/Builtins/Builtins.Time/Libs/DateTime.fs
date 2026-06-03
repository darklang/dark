/// Clock-reading DateTime builtins. The pure DateTime ops (parse,
/// format, arithmetic, accessors) live in `Builtins.Pure/Libs/DateTime`.
module Builtins.Time.Libs.DateTime

type Instant = NodaTime.Instant

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module DarkDateTime = LibExecution.DarkDateTime


let fns () : List<BuiltInFn> =
  [ { name = fn "dateTimeNow" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDateTime
      description = "Returns the current datetime"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          Instant.now () |> DarkDateTime.fromInstant |> DDateTime |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.clock
      deprecated = NotDeprecated }


    { name = fn "dateTimeToday" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDateTime
      description = "Returns the <type DateTime> with the time set to midnight"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let now = DarkDateTime.fromInstant (Instant.now ())
          Ply(DDateTime(DarkDateTime.T(now.Year, now.Month, now.Day, 0, 0, 0)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.clock
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
