/// Builtin functions for account lookup. Companion to
/// `LibCloud.Account` (which holds the F#-side getters that aren't
/// builtin-shaped).
module Builtins.Matter.Libs.Account

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Sqlite

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType


let fns () : List<BuiltInFn> =
  [ { name = fn "accountGetByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "Account display name" ]
      returnType = TypeReference.option TUuid
      description =
        "Look up an account ID by name. Returns None if no account with that name exists."
      fn =
        (function
        | _, vm, _, [ DString name ] ->
          uply {
            let! result =
              Sql.query "SELECT id FROM accounts_v0 WHERE name = @name"
              |> Sql.parameters [ "name", Sql.string name ]
              |> Sql.executeRowOptionAsync (fun read -> read.uuid "id")
            return
              result
              |> Option.map DUuid
              |> LibExecution.TypeChecker.DvalCreator.option vm.threadID VT.uuid
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "accountGetNameByID" 0
      typeParams = []
      parameters = [ Param.make "accountId" TUuid "Account ID" ]
      returnType = TypeReference.option TString
      description =
        "Look up an account name by ID. Returns None if no account with that ID exists."
      fn =
        (function
        | _, vm, _, [ DUuid id ] ->
          uply {
            let! result =
              Sql.query "SELECT name FROM accounts_v0 WHERE id = @id"
              |> Sql.parameters [ "id", Sql.uuid id ]
              |> Sql.executeRowOptionAsync (fun read -> read.string "name")
            return
              result
              |> Option.map DString
              |> LibExecution.TypeChecker.DvalCreator.option vm.threadID VT.string
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "accountList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Returns every (id, name) pair in the accounts table, ordered by name."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! rows =
              Sql.query "SELECT id, name FROM accounts_v0 ORDER BY name"
              |> Sql.executeAsync (fun read -> (read.uuid "id", read.string "name"))
            return
              rows
              |> List.map (fun (id, name) -> DTuple(DUuid id, DString name, []))
              |> Dval.list (KTTuple(VT.uuid, VT.string, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
