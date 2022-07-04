module LibExecution.Init

open Prelude

let init () =
  do Json.Vanilla.allow<OCamlTypes.op> ()
  do Json.Vanilla.allow<OCamlTypes.RuntimeT.dval> ()
  do Json.OCamlCompatible.allow<OCamlTypes.op> ()
  do Json.OCamlCompatible.allow<OCamlTypes.RuntimeT.dval> ()
  do Json.OCamlCompatible.allow<OCamlTypes.PackageManager.parameter> ()
  do Json.OCamlCompatible.allow<OCamlTypes.PackageManager.fn> ()
  do
    Json.Vanilla.allow<DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
      ()

  // CLEANUP - we're not supposed to directly serialize these types. We should
  // convert them to a specific format for serialization, like we do to SerializationTypes
  do Prelude.Json.Vanilla.allow<ProgramTypes.Handler.T> ()
  do Prelude.Json.Vanilla.allow<ProgramTypes.Position> ()
  do Prelude.Json.OCamlCompatible.allow<ProgramTypes.Handler.T> ()
  do Json.Vanilla.allow<RuntimeTypes.Dval> ()
  do Json.OCamlCompatible.allow<RuntimeTypes.Dval> ()
  do Json.OCamlCompatible.allow<AnalysisTypes.TraceData> ()
