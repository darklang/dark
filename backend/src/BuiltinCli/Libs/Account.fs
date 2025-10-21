module BuiltinCli.Libs.Account

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module AccountContext = LibPackageManager.AccountContext

open Builtin.Shortcuts


// CLEANUP: For now, we use a hardcoded account ID ('anon')
// In the future, this should come from a config file or environment variable
let defaultAccountId = System.Guid.Parse("00000000-0000-0000-0000-000000000001")


let fns : List<BuiltInFn> =
  [ { name = fn "cliAccountGetCurrentId" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TUuid
      description = "Returns the current account ID for the CLI session"
      fn =
        (function
        | _, _, _, [ DUnit ] -> Ply(DUuid defaultAccountId)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliAccountGetCurrentBranch" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TUuid
      description =
        "Get the current branch for the account, ensuring 'main' exists if needed"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! branchId = AccountContext.getOrCreateCurrentBranch defaultAccountId
            return DUuid branchId
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliAccountSetCurrentBranch" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "" ]
      returnType = TUnit
      description = "Set the current branch for the account"
      fn =
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            do! AccountContext.setCurrentBranch defaultAccountId branchId
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliAccountClearCurrentBranch" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TUnit
      description = "Clear the current branch for the account (sets it to NULL)"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            do! AccountContext.clearCurrentBranch defaultAccountId
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
