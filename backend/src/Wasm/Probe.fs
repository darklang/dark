/// Boot-time probes callable from JS, to check the native pieces linked.
module Darklang.Wasm.Probe

open Prelude

open Microsoft.JSInterop

/// The statically linked SQLite's version string. Throws if e_sqlite3 didn't link.
[<JSInvokable>]
let SqliteVersion () : string =
  SQLitePCL.Batteries_V2.Init()
  SQLitePCL.raw.sqlite3_libversion().utf8_to_string ()

module RT = LibExecution.RuntimeTypes

/// Does the harmful-deprecation set load? (`Async.RunSynchronously` over Fumble.)
[<JSInvokable>]
let ProbeHarmful () : bool =
  LibDB.PackageManager.rt.isHarmful (RT.Hash "0000")

/// Does a package fn load from the store?
[<JSInvokable>]
let ProbeGetFn (hash : string) : System.Threading.Tasks.Task<bool> =
  task {
    let! fn = LibDB.PackageManager.rt.getFn (RT.Hash hash) |> Ply.toTask
    return fn.IsSome
  }
