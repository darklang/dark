/// Store-metadata builtins: this instance's own package-store path + the Release coordinate it speaks.
/// These are facts about the local package STORE, used across the CLI (`dark version`, `dark ops`, config)
/// and by sync — not sync's own machinery, so they live with the package manager, not under Sync/. (The
/// wrappers live under `Darklang.SCM.Wire` on the Dark side; the F# home is here.)
module Builtins.Matter.Libs.PM.Store

open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module NR = LibExecution.RuntimeTypes.NameResolution


/// Refuse a store-level operation unless every non-root frame is bundled Darklang
/// code. The same check the private-network transport makes, for the same reason: a
/// guest that can call a bundled wrapper would otherwise turn it into a confused
/// deputy.
let private requireBundledCaller
  (state : ExecutionState)
  (vm : VMState)
  (builtinName : string)
  : unit =
  if not (LibExecution.PermissionCheck.callerIsBundled state vm) then
    RuntimeError.UncaughtException(
      $"permission denied: `{builtinName}` is restricted to trusted first-party (Darklang) code",
      []
    )
    |> raiseUntargetedRTE


/// The watch behind `Stdlib.Host.await` (see `hostWatchGet` below). One slot per OS process,
/// because one host loop runs per process. A second loop in the same process would share it
/// and see the first's "already reported" state; make it per-process state if that day comes.
module HostWatch =
  let mutable watch : Option<Dval> = None


let private backupTypeName () =
  FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.localStoreBackup ())

let private backupActionTypeName () =
  FQTypeName.fqPackage (
    LibExecution.PackageRefs.Type.Stdlib.localStoreBackupAction ()
  )

let private backupToDT (b : LibDB.StoreBackups.Backup) : Dval =
  let tn = backupTypeName ()
  DRecord(
    tn,
    tn,
    [],
    Map
      [ "name", DString b.name
        "path", DString b.path
        "bytes", Dval.int (bigint b.bytes) ]
  )


let fns () : List<BuiltInFn> =
  [
    // This instance's OWN package store path (data.db). The op-log builtins write ops here; the sync config
    // tables (sync_peers/sync_cursors) live here too — the daemon/CLI don't have to know the path.
    { name = fn "localDbPath" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "The file path of this instance's own package store (data.db)."
      fn =
        (function
        // `LibDB.Sqlite.currentDbPath`, not `Config.dbPath`: the two differ under a test that
        // repointed the store, and Dark's SQL has to reach the same file as the F# it is
        // interleaved with.
        | _, _, _, [| DUnit |] -> uply { return DString LibDB.Sqlite.currentDbPath }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    // Every whole copy of this instance's store: the backups kept beside it, and a snapshot
    // written elsewhere. F# owns the folder, the names and the copying (`LibDB.StoreBackups`);
    // Dark says which action and prints what comes back.
    //
    // The store path is guarded, so the file builtins refuse it and `dark backups` could not
    // do this for itself. First-party only, checked the way the sync transport checks it:
    // replacing the store wholesale from a file is not something a guest holding
    // package-write should be able to reach through a wrapper.
    { name = fn "storeBackup" 0
      typeParams = []
      parameters =
        [ Param.make
            "action"
            (TCustomType(NR.ok (backupActionTypeName ()), []))
            "what to do" ]
      returnType =
        TypeReference.result
          (TList(TCustomType(NR.ok (backupTypeName ()), [])))
          TString
      description =
        "Lists, takes, restores or deletes this instance's store backups, or snapshots the store "
        + "to a path. Returns the backups the action is about, or the failure as an Error."
      fn =
        (function
        | state, vm, _, [| DEnum(_, _, _, caseName, fields) |] ->
          uply {
            requireBundledCaller state vm "storeBackup"
            let db = LibDB.Sqlite.currentDbPath
            let result =
              match caseName, fields with
              | "List", [] -> Ok(LibDB.StoreBackups.list db)
              | "Create", [] ->
                LibDB.StoreBackups.create db |> Result.map List.singleton
              | "Restore", [ DString name ] ->
                LibDB.StoreBackups.restore db name |> Result.map List.singleton
              | "Delete", [ DString name ] ->
                LibDB.StoreBackups.delete db name |> Result.map List.singleton
              | "SnapshotTo", [ DString path ] ->
                LibDB.StoreBackups.snapshotTo db path |> Result.map List.singleton
              | _ -> incorrectArgs ()
            let backupKT = KTCustomType(backupTypeName (), [])
            let listKT = KTList(ValueType.Known backupKT)
            match result with
            | Ok backups ->
              return
                backups
                |> List.map backupToDT
                |> Dval.list backupKT
                |> Dval.resultOk listKT KTString
            | Error e -> return Dval.resultError listKT KTString (DString e)
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "configGet" 0
      typeParams = []
      parameters = [ Param.make "key" TString "" ]
      returnType = TString
      description = "Get a local config value (config_v0), or \"\" if unset."
      fn =
        (function
        | _, _, _, [| DString key |] ->
          uply {
            let! v = LibDB.Config.get key
            return DString(Option.defaultValue "" v)
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "configSet" 0
      typeParams = []
      parameters = [ Param.make "key" TString ""; Param.make "value" TString "" ]
      returnType = TUnit
      description = "Set a local config value (config_v0). Local + unsynced."
      fn =
        (function
        | _, _, _, [| DString key; DString value |] ->
          uply {
            do! LibDB.Config.set key value
            return DUnit
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageWrite ]
      deprecated = NotDeprecated }

    // The store-change signal for a process that stays up (a TUI, `serve`, a daemon). See
    // `LibDB.Sqlite.DataVersion`: a counter that moves when a commit lands through any other
    // connection, so a host loop can ask "anything new?" for the cost of one pragma instead of
    // re-reading the log. Comparable only with an earlier answer from the same process.
    { name = fn "storeVersion" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description =
        "The store's change counter as this process sees it: moves whenever a commit lands "
        + "through another connection. Only \"same as before\" or \"moved\" means anything."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply { return Dval.int (bigint (LibDB.Sqlite.DataVersion.current ())) }
        | _ -> incorrectArgs ())
      previewable = Impure
      // `PackageRead`, like every other question about the package store.
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    // The one slot `Stdlib.Host.await` keeps its `Stdlib.Live.Watch` in between calls: the runtime
    // says the store MOVED (a counter, from a timer), and Dark says which ops landed by polling
    // from where its last look left off. `await` takes no state, and a Dark value cannot outlive
    // the call that made it, so the watch lives here. Nothing else may use this.
    { name = fn "hostWatchGet" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option (TVariable "a")
      description = "`Host.await`'s watch, if one has been stored."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            match HostWatch.watch with
            | Some w ->
              return
                DEnum(
                  Dval.optionType (),
                  Dval.optionType (),
                  [ ValueType.Unknown ],
                  "Some",
                  [ w ]
                )
            | None ->
              return
                DEnum(
                  Dval.optionType (),
                  Dval.optionType (),
                  [ ValueType.Unknown ],
                  "None",
                  []
                )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "hostWatchSet" 0
      typeParams = []
      parameters = [ Param.make "watch" (TVariable "a") "" ]
      returnType = TUnit
      description = "Store the watch for the next `Host.await`."
      fn =
        (function
        | _, _, _, [| w |] ->
          uply {
            HostWatch.watch <- Some w
            return DUnit
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      // Not a read: `Effects.readsOnly` would let a resume serve it instead of arming the watch.
      callEffects = set [ Effect.Runtime ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
