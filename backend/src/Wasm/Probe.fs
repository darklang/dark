/// Boot-time probes callable from JS, to check the native pieces linked.
module Darklang.Wasm.Probe

open Microsoft.JSInterop

/// The statically linked SQLite's version string. Throws if e_sqlite3 didn't link.
[<JSInvokable>]
let SqliteVersion () : string =
  SQLitePCL.Batteries_V2.Init()
  SQLitePCL.raw.sqlite3_libversion().utf8_to_string ()
