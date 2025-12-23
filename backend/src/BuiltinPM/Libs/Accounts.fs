module BuiltinPM.Libs.Accounts

open Prelude
open LibExecution.RuntimeTypes

open Fumble
open LibDB.Db

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin

open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "pmListAccounts" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList TString
      description = "List all accounts"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! accounts =
              """
              SELECT name FROM accounts_v0 ORDER BY name
              """
              |> Sql.query
              |> Sql.executeAsync (fun read -> read.string "name")
            return DList(VT.string, accounts |> List.map DString)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetAccountByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TUuid
      description = "Get account UUID by name"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! result = LibPackageManager.Accounts.getByName name
            return result |> Option.map DUuid |> Dval.option KTUuid
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetAccountNameById" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType = TypeReference.option TString
      description = "Get account name by UUID"
      fn =
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = LibPackageManager.Accounts.getName id
            return result |> Option.map DString |> Dval.option KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins : Builtins = LibExecution.Builtin.make [] fns
