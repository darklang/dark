open Core

val conn : Postgresql.connection

val status : unit -> [`Healthy | `Disconnected]
