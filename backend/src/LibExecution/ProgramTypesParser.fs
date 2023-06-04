/// Provides toString and fromString functions for program types
///
/// We are moving to not needing parsers, but for now they are still necessary. They
/// mostly convert to a format used in the database, or in the OCaml compatible
/// representation (spoken by the client and used in the API for example)
module LibExecution.ProgramTypesParser

// CLEANUP remove this entire file ASAP

// CLEANUP: almost all users of functions in this file could be replaced by having
// better types and/or propagating the type definitions end-to-end (such as to the
// client). It would likely also increase the reliability and reduce the
// maintainance costs

open Prelude
open VendoredTablecloth

module PT = ProgramTypes

module Handler =
  module CronInterval =
    let toString (interval : PT.Handler.CronInterval) : string =
      match interval with
      | PT.Handler.EveryDay -> "Daily"
      | PT.Handler.EveryWeek -> "Weekly"
      | PT.Handler.EveryFortnight -> "Fortnightly"
      | PT.Handler.EveryHour -> "Every 1hr"
      | PT.Handler.Every12Hours -> "Every 12hrs"
      | PT.Handler.EveryMinute -> "Every 1min"

    let parse (modifier : string) : Option<PT.Handler.CronInterval> =
      match String.toLowercase modifier with
      | "daily" -> Some PT.Handler.EveryDay
      | "weekly" -> Some PT.Handler.EveryWeek
      | "fortnightly" -> Some PT.Handler.EveryFortnight
      | "every 1hr" -> Some PT.Handler.EveryHour
      | "every 12hrs" -> Some PT.Handler.Every12Hours
      | "every 1min" -> Some PT.Handler.EveryMinute
      | _ -> None

  module Spec =

    let toName (s : PT.Handler.Spec) =
      match s with
      | PT.Handler.HTTP(route, _method) -> route
      | PT.Handler.Worker name -> name
      | PT.Handler.Cron(name, _interval) -> name
      | PT.Handler.REPL name -> name

    let toModifier (s : PT.Handler.Spec) =
      match s with
      | PT.Handler.HTTP(_route, method) -> method
      | PT.Handler.Worker _name -> "_"
      | PT.Handler.Cron(_name, interval) -> CronInterval.toString interval
      | PT.Handler.REPL _name -> "_"

    let toModule (s : PT.Handler.Spec) =
      match s with
      | PT.Handler.HTTP _ -> "HTTP_BASIC"
      | PT.Handler.Worker _ -> "WORKER" // CLEANUP the DB relies on the casing
      | PT.Handler.Cron _ -> "CRON" // CLEANUP the DB relies on the casing
      | PT.Handler.REPL _ -> "REPL"

    let isComplete (s : PT.Handler.Spec) : bool =
      match s with
      | PT.Handler.HTTP("", _) -> false
      | PT.Handler.HTTP(_, "") -> false
      | PT.Handler.Worker "" -> false
      | PT.Handler.Cron("", _) -> false
      | PT.Handler.REPL "" -> false
      | _ -> true

    // Same as a TraceInput.EventDesc
    let toEventDesc (s : PT.Handler.Spec) : Option<HandlerDesc> =
      if isComplete s then Some(toModule s, toName s, toModifier s) else None

module Toplevel =
  let toDBTypeString (tl : PT.Toplevel.T) =
    match tl with
    | PT.Toplevel.TLDB _ -> "db"
    | PT.Toplevel.TLHandler _ -> "handler"
    | PT.Toplevel.TLFunction _ -> "user_function"
    | PT.Toplevel.TLType _ -> "user_tipe"
