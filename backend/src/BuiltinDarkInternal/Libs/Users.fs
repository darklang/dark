/// Builtin functions for user management. Note that user management is intended to be
/// built in Darklang itself, so this functionality is deliberately sparse.
module BuiltinDarkInternal.Libs.Users

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalUserCreate" 0
      typeParams = []
      parameters = [ Param.make "name" TString "The name for the new user" ]
      returnType = TUuid
      description = "Creates a user with the given name, and returns their userID."
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! userID = LibCloud.Account.createUser name
            return DUuid userID
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
