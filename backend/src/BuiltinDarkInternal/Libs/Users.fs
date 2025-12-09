/// Builtin functions for user management. Note that user management is intended to be
/// built in Darklang itself, so this functionality is deliberately sparse.
module BuiltinDarkInternal.Libs.Users

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module Dval = LibExecution.Dval


let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalUserCreate" 0
      typeParams = []
      parameters = [ Param.make "name" TString "The name for the new user" ]
      returnType = TypeReference.result TUuid TString
      description =
        "Creates a user with the given name and returns their userID, or an error if the name is already taken."
      fn =
        let resultOk = Dval.resultOk KTUuid KTString
        let resultError = Dval.resultError KTUuid KTString
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! result = LibCloud.Account.createUser name
            match result with
            | Ok userID -> return resultOk (DUuid userID)
            | Error msg -> return resultError (DString msg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
