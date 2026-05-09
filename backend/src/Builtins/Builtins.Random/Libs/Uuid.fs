/// Impure UUID builtins. Pure UUID ops (parse, toString) live in
/// `Builtins.Pure/Libs/Uuid`.
module Builtins.Random.Libs.Uuid

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "uuidGenerate" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description = "Generate a new <type Uuid> v4 according to RFC 4122"
      fn =
        (function
        | _, _, _, [ DUnit ] -> Ply(DUuid(System.Guid.NewGuid()))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable =
        // Similarly to DateTime.now, it's not particularly fun for this to change
        // when live programming, so let's keep this as Impure rather than ImpurePreviewable
        Impure
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
