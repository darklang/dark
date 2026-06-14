/// Builtins behind `dark remote` — the registry of sync peers the tailnet daemon polls. Thin
/// wrappers over `LibDB.Remotes`; the pull/apply sync surface lives in `Libs.PM.Sync`.
module Builtins.Matter.Libs.PM.Remotes

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin

open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "pmRemoteAdd" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "url" TString "" ]
      returnType = TUnit
      description =
        "Register (or update) a sync remote by name. `url` is the pollable target (an http(s) URL or a
         local data.db path). Idempotent by name."
      fn =
        (function
        | _, _, _, [ DString name; DString url ] ->
          uply {
            do! LibDB.Remotes.add name url
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmRemoteList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList TString
      description =
        "Registered sync remotes, one `name  →  url` line each (for `dark remote list`). Empty if none."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! remotes = LibDB.Remotes.list ()
            let lines =
              remotes |> List.map (fun (name, url) -> DString $"{name}  →  {url}")
            return Dval.list KTString lines
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmRemoteRemove" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TBool
      description =
        "Unregister a sync remote by name. Returns true if it existed (its sync cursor, if any, stays)."
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! existed = LibDB.Remotes.remove name
            return DBool existed
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
