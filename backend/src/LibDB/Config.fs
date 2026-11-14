/// Mutable, per-install local config (key/value): the current branch, the scheduler knobs, the
/// tracing knobs, the HTTP settings, and who this install is (`instance.*`).
///
/// F# owns it: which keys exist, what each takes, and which only F# writes. Dark reads and
/// writes through `configGet` / `configSet`, and `configSet` refuses anything `check` (or, for an
/// unset, `checkUnset`) does.
///
/// Deliberately NOT content-addressed. This is local mutable state -- what one install chooses
/// for itself -- kept separate from the immutable op log by design.
module LibDB.Config

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Sqlite

open Prelude

/// The value for `key`, or None if unset.
let get (key : string) : Task<string option> =
  Sql.query "SELECT value FROM config_v0 WHERE key = @key"
  |> Sql.parameters [ "key", Sql.string key ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "value")

/// The values of several keys in one query, keyed by name; unset keys are absent.
/// For a startup that reads a handful of settings: one round trip, not one per key.
let getMany (keys : List<string>) : Task<Map<string, string>> =
  task {
    let names = keys |> List.mapi (fun i _ -> $"@k{i}") |> String.concat ", "
    let! rows =
      Sql.query $"SELECT key, value FROM config_v0 WHERE key IN ({names})"
      |> Sql.parameters (keys |> List.mapi (fun i k -> $"k{i}", Sql.string k))
      |> Sql.executeAsync (fun read -> read.string "key", read.string "value")
    return Map.ofList rows
  }

/// Set `key` to `value` (upsert).
let set (key : string) (value : string) : Task<unit> =
  task {
    let! (_ : int) =
      Sql.query
        """
        INSERT INTO config_v0 (key, value) VALUES (@key, @value)
        ON CONFLICT(key) DO UPDATE SET value = @value
        """
      |> Sql.parameters [ "key", Sql.string key; "value", Sql.string value ]
      |> Sql.executeNonQueryAsync

    return ()
  }


/// Forget `key`, so whatever reads it falls back to its default. A no-op if it is not set.
let unset (key : string) : Task<unit> =
  task {
    let! (_ : int) =
      Sql.query "DELETE FROM config_v0 WHERE key = @key"
      |> Sql.parameters [ "key", Sql.string key ]
      |> Sql.executeNonQueryAsync

    return ()
  }


/// The keys a person sets with `dark config set`. Each is read by something: the boot
/// (`startupSettings` in `Cli.fs`), `serve`, the HTTP guard, or the workbench header.
let settings : List<string> =
  [ "exec.workers"
    "exec.policy"
    "exec.maxInstructions"
    "exec.maxBytes"
    "exec.storePollMs"
    "exec.spreadCrossover"
    "exec.spreadMinChunk"
    "exec.spreadPredict"
    "http.requestTimeoutMs"
    "http.loopback"
    "http.private"
    "trace.record"
    "trace.keep"
    "trace.maxMb"
    "instance.name" ]

/// The settings that open HTTP's network guard or decide what traces record. They are the
/// person's to change, like the permission policy: `dark config set` or the `DARK_CONFIG_*`
/// variables, never code Dark runs, whatever that code was granted.
let personOnly : List<string> = [ "http.loopback"; "http.private"; "trace.record" ]

/// What code that tried to change a `personOnly` key is told.
let refusal (key : string) : string =
  $"permission denied: code cannot change `{key}`, which decides what code may reach or "
  + $"what traces record. To change it yourself: `dark config set {key} on|off`"

/// Every key `dark config` answers for: the settings, and `instance.id`, which is read but
/// never set.
let readable : List<string> = settings @ [ "instance.id" ]

/// Keys Dark code keeps for itself, with any value: which live view is showing, which function
/// `traces watch` follows, what GitHub last said about releases. Nobody types these.
let private internalKeys = [ "live.show"; "traces.watch.fn"; "update.check" ]

/// Scratch keys for tests. Nothing in the product reads a key under this prefix.
let private testPrefix = "test."

/// Why a value will not do, or Ok. The boot falls back silently on a value it cannot read, so the
/// refusal has to happen here. <param policyResolves> says whether `exec.policy` names a package
/// function the boot can find; it is passed in because the package manager is built after this.
let check
  (policyResolves : string -> bool)
  (key : string)
  (value : string)
  : Result<unit, string> =
  let int32Keys = [ "exec.workers"; "exec.storePollMs"; "http.requestTimeoutMs" ]
  let int64Keys =
    [ "exec.maxInstructions"
      "exec.maxBytes"
      "exec.spreadMinChunk"
      "trace.keep"
      "trace.maxMb" ]
  let onOff =
    [ "trace.record"; "http.loopback"; "http.private"; "exec.spreadPredict" ]
  let wholeNumber () = Error $"{key} takes a whole number (0 = none), not {value}"
  if value = "" && List.contains key settings then
    Error $"{key} needs a value; `dark config unset {key}` goes back to the default"
  elif List.contains key int32Keys then
    match System.Int32.TryParse value with
    // The boot ignores a poll under 10 ms (`Cli.fs`), so refuse it here rather than store it.
    | true, n when key = "exec.storePollMs" && n < 10 ->
      Error $"{key} takes a whole number of ms, 10 or more, not {value}"
    | true, n when n >= 0 -> Ok()
    | _ -> wholeNumber ()
  elif List.contains key int64Keys then
    match System.Int64.TryParse value with
    | true, n when n >= 0L -> Ok()
    | _ -> wholeNumber ()
  elif key = "exec.spreadCrossover" then
    match System.Int64.TryParse value with
    | true, _ -> Ok()
    | _ -> Error $"{key} takes a whole number (below 0 = never spread), not {value}"
  elif List.contains key onOff then
    if value = "on" || value = "off" then Ok() else Error $"{key} is on or off"
  elif key = "exec.policy" then
    if policyResolves value then
      Ok()
    else
      Error $"{key} names a package function, and {value} is not one on main"
  elif key = "instance.name" then
    if value.Trim() = "" then Error $"{key} needs a name" else Ok()
  elif key = "instance.id" then
    Error "instance.id is made once for this install, and cannot be set"
  elif key = "current_branch" || key = "current_branch_name" then
    Error $"{key} follows `dark switch`, and cannot be set directly"
  elif List.contains key internalKeys || key.StartsWith testPrefix then
    Ok()
  else
    Error $"{key} is not a config key"


/// Why `key` may not be unset, or Ok: the keys `check` takes, less the ones only the runtime
/// writes, which have no default to go back to.
let checkUnset (key : string) : Result<unit, string> =
  if key = "instance.id" then
    Error "instance.id is made once for this install, and cannot be unset"
  elif key = "current_branch" || key = "current_branch_name" then
    Error $"{key} follows `dark switch`, and cannot be unset directly"
  elif
    List.contains key settings
    || List.contains key internalKeys
    || key.StartsWith testPrefix
  then
    Ok()
  else
    Error $"{key} is not a config key"


/// Who this install is: `instance.id` is made here, once, and never changes; `instance.name` is
/// what a person set. Both used to live in `cli-config.json` beside the store, written by Dark,
/// so the first time the store has no id, the file's id and name move in, and the file is left
/// alone. Every later call is one read.
let ensureInstance () : Task<unit> =
  task {
    match! get "instance.id" with
    | Some _ -> return ()
    | None ->
      let fromFile =
        try
          let dir = System.IO.Path.GetDirectoryName LibDB.Sqlite.currentDbPath
          let path = System.IO.Path.Combine(dir, "cli-config.json")
          if System.IO.File.Exists path then
            use doc =
              System.Text.Json.JsonDocument.Parse(System.IO.File.ReadAllText path)
            let field (name : string) =
              match doc.RootElement.TryGetProperty name with
              | true, v when v.ValueKind = System.Text.Json.JsonValueKind.String ->
                match v.GetString() with
                | null
                | "" -> None
                | s -> Some s
              | _ -> None
            [ "instance.id"; "instance.name" ]
            |> List.choose (fun k -> field k |> Option.map (fun v -> k, v))
            |> Map.ofList
          else
            Map.empty
        with _ ->
          Map.empty
      match Map.tryFind "instance.name" fromFile with
      | Some name ->
        match! get "instance.name" with
        | Some _ -> ()
        | None -> do! set "instance.name" name
      | None -> ()
      let id =
        Map.tryFind "instance.id" fromFile
        |> Option.defaultWith (fun () -> string (System.Guid.NewGuid()))
      do! set "instance.id" id
  }
