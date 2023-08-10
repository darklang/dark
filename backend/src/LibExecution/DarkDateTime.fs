
module LibExecution.DarkDateTime

open NodaTime

open Prelude

// A datetime in Dark is always in UTC, so we don't include the utc info
type T = LocalDateTime
let utc = DateTimeZone.Utc

let toZonedDateTime (dt : T) = ZonedDateTime(dt, utc, Offset.Zero)

let toInstant (dt : T) = (toZonedDateTime dt).ToInstant()

let toDateTimeUtc (dt : T) = (toInstant dt).ToDateTimeUtc()

let fromInstant (i : Instant) : T = i.toUtcLocalTimeZone ()

let fromDateTime (dt : System.DateTime) : T =
  Instant.FromDateTimeUtc dt |> fromInstant

let toIsoString (d : T) : string = (toInstant d).toIsoString ()

