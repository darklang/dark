/// Builtin functions for user management. Note that user management is intended to be
/// built in Darklang itself, so this functionality is deliberately sparse.
module BuiltinDarkInternal.Libs.Users

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalUserCreate" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description = "Creates a user, and returns their userID."
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            let! canvasID = LibCloud.Account.createUser ()
            return DUuid canvasID
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents : Builtins = LibExecution.Builtin.fromContents constants fns
