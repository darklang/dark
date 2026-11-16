/// Impure UUID builtins. Pure UUID ops (parse, toString) live in
/// `Builtins.Pure/Libs/Uuid`.
module Builtins.Random.Libs.Uuid

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects


let fns () : List<BuiltInFn> =
  [ { name = fn "uuidGenerate" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description = "Generate a new <type Uuid> v4 according to RFC 4122"
      fn =
        (function
        | state, _, _, [| DUnit |] ->
          DUuid(LibExecution.HostRandom.uuidV4 state.random)
        | _ -> incorrectArgs ())
      previewable =
        // Similarly to DateTime.now, it's not particularly fun for this to change
        // when live programming, so `Impure` is the honest label
        Impure
      callEffects = set [ Effect.Random ]
      deprecated = NotDeprecated }


    { name = fn "uuidGenerateV7" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description =
        "Generate a new <type Uuid> v7 according to RFC 9562: the current Unix time in milliseconds, then random bits"
      fn =
        (function
        | state, _, _, [| DUnit |] ->
          let now =
            match state.clock with
            | LibExecution.HostClock.Virtual v -> v.Now / 10_000L
            | LibExecution.HostClock.Real ->
              System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
          DUuid(LibExecution.HostRandom.uuidV7 state.random now)
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Random; Effect.Clock ]
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
