/// Store-metadata builtins: this instance's own package-store path + the Release coordinate it speaks.
/// These are facts about the local package STORE, used across the CLI (`dark version`, `dark ops`, config)
/// and by sync — not sync's own machinery, so they live with the package manager, not under Sync/. (The
/// wrappers live under `Darklang.SCM.Wire` on the Dark side; the F# home is here.)
module Builtins.Admin.Libs.Store

open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval


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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    // Take a copy of this instance's store, and put one back.
    //
    // The store path is guarded, so the file builtins refuse it and `dark backups`
    // cannot do this for itself -- see `LibDB.Sqlite.Backup`. First-party only,
    // checked the way the sync transport checks it: replacing the store wholesale
    // from an arbitrary file is not something a guest holding package-write should
    // be able to reach through a wrapper.
    { name = fn "localDbBackupTo" 0
      typeParams = []
      parameters = [ Param.make "path" TString "where to write the copy" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Snapshots this instance's package store to <param path>. Returns the failure as an Error."
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          uply {
            requireBundledCaller state vm "localDbBackupTo"
            match LibDB.Sqlite.Backup.toFile path with
            | Ok() -> return Dval.resultOk KTUnit KTString DUnit
            | Error e -> return Dval.resultError KTUnit KTString (DString e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "localDbRestoreFrom" 0
      typeParams = []
      parameters = [ Param.make "path" TString "the copy to restore" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Replaces this instance's package store contents with <param path>'s. Returns the failure as an Error."
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          uply {
            requireBundledCaller state vm "localDbRestoreFrom"
            match LibDB.Sqlite.Backup.fromFile path with
            | Ok() -> return Dval.resultOk KTUnit KTString DUnit
            | Error e -> return Dval.resultError KTUnit KTString (DString e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageWrite ]
      deprecated = NotDeprecated }

    // Whether a write secret is stored for a relay, WITHOUT handing it over.
    //
    // `dark sync setup` needs to know if one is already there, so that pressing Enter keeps it rather
    // than wiping it. That is the only thing Dark ever needed the value for outside the transport.
    { name = fn "relaySecretIsSet" 0
      typeParams = []
      parameters = [ Param.make "url" TString "relay url" ]
      returnType = TBool
      description =
        "Whether a write secret is stored for <param url>. Never returns the secret."
      fn =
        (function
        | _, _, _, [| DString url |] ->
          uply {
            let! v = LibDB.Config.get (LibDB.Config.secretPrefix + url)
            return DBool(v |> Option.exists (fun s -> s <> ""))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // The ops in a bundle whose bytes contain this relay's write secret, as short ids.
    //
    // THE LAST GATE before a secret becomes public: an op carrying it becomes a page anyone can read,
    // and nothing else would notice, because the op is valid and folds and syncs like any other. The
    // scan is native for the same reason the secret is: Dark cannot be handed the needle.
    { name = fn "relaySecretLeakingOps" 0
      typeParams = []
      parameters =
        [ Param.make "url" TString "relay url"
          Param.make "opsHex" (TList TString) "hex blobs, in order"
          Param.make "opIds" (TList TString) "the ids of those blobs, same order" ]
      returnType = TList TString
      description =
        "The ids (first 8 chars) of ops whose bytes contain the write secret stored for "
        + "<param url>. Empty when none do, which is the normal case."
      fn =
        (function
        | _, _, _, [| DString url; DList(_, opsHex); DList(_, opIds) |] ->
          uply {
            let! stored = LibDB.Config.get (LibDB.Config.secretPrefix + url)

            match stored with
            | None
            | Some "" -> return Dval.list KTString []
            | Some secret ->
              let needle =
                System.Convert
                  .ToHexString(System.Text.Encoding.UTF8.GetBytes secret)
                  .ToLowerInvariant()

              let ids =
                // The wrapper builds both lists from one bundle, but `zip` on ragged input is an
                // uncaught host exception; truncating scans what can be paired and no more.
                let pairable = min (List.length opsHex) (List.length opIds)
                List.zip
                  (List.truncate pairable opsHex)
                  (List.truncate pairable opIds)
                |> List.choose (fun (hex, id) ->
                  match hex, id with
                  | DString h, DString i when h.ToLowerInvariant().Contains needle ->
                    Some(DString(if i.Length > 8 then i.Substring(0, 8) else i))
                  | _ -> None)

              return Dval.list KTString ids
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "configGet" 0
      typeParams = []
      parameters = [ Param.make "key" TString "" ]
      returnType = TString
      description =
        "Get a local config value (config_v0), or \"\" if unset. Credential keys are refused; "
        + "see `LibDB.Config.secretPrefix`."
      fn =
        (function
        | _, _, _, [| DString key |] ->
          uply {
            // Refused rather than returned empty: a silent "" would look like "no secret configured" and
            // send an unauthenticated push, which fails in a way nobody would trace back to here.
            if LibDB.Config.isSecretKey key then
              return
                RuntimeError.UncaughtException(
                  $"`{key}` holds a credential and cannot be read from Dark. The transport attaches it.",
                  []
                )
                |> raiseUntargetedRTE
            else
              let! v = LibDB.Config.get key
              return DString(Option.defaultValue "" v)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
