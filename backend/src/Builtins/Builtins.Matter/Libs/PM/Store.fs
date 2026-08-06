/// Store-metadata builtins: this instance's own package-store path + the Release coordinate it speaks.
/// These are facts about the local package STORE, used across the CLI (`dark version`, `dark ops`, config)
/// and by sync — not sync's own machinery, so they live with the package manager, not under Sync/. (The
/// wrappers stay in the `Darklang.Sync.*` namespace on the Dark side; only the F# home moves.)
module Builtins.Matter.Libs.PM.Store

open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval


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
        | _, _, _, [ DUnit ] -> uply { return DString LibConfig.Config.dbPath }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    // Local, per-install mutable config (config_v0): the entry-point pointer + per-user settings. NOT
    // content-addressed, NOT synced — deliberately separate from the op log. "" means unset.
    { name = fn "configGet" 0
      typeParams = []
      parameters = [ Param.make "key" TString "" ]
      returnType = TString
      description = "Get a local config value (config_v0), or \"\" if unset."
      fn =
        (function
        | _, _, _, [ DString key ] ->
          uply {
            let! v = LibDB.Config.get key
            return DString(Option.defaultValue "" v)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "configSet" 0
      typeParams = []
      parameters = [ Param.make "key" TString ""; Param.make "value" TString "" ]
      returnType = TUnit
      description = "Set a local config value (config_v0). Local + unsynced."
      fn =
        (function
        | _, _, _, [ DString key; DString value ] ->
          uply {
            do! LibDB.Config.set key value
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
