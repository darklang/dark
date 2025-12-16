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
              SELECT id FROM accounts ORDER BY id
              """
              |> Sql.query
              |> Sql.executeAsync (fun read -> read.string "id")
            return DList(VT.string, accounts |> List.map DString)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins : Builtins = LibExecution.Builtin.make [] fns
