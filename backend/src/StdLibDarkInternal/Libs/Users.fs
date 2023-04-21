/// StdLib functions for user management. Note that user management is intended to be
/// built in Darklang itself, so this functionality is deliberately sparse.
module StdLibDarkInternal.Libs.Users

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

open LibBackend

let fn = FQFnName.stdlibFnName
let typ = FQTypeName.stdlibTypeName

let incorrectArgs = LibExecution.Errors.incorrectArgs

// only accessible to the LibBackend.Config.allowedDarkInternalCanvasID canvas
let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "createUser" 0
      typeParams = []
      parameters = []
      returnType = TUuid
      description = "Creates a user, and returns their userID."
      fn =
        (function
        | _, _, [] ->
          uply {
            let! canvasID = Account.createUser ()
            return DUuid canvasID
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
