module ClientTypes.Init

open Prelude

let init (serviceName : string) =
  print $"Initing ClientTypes in {serviceName}"

  Json.Vanilla.registerConverter (Converters.STJ.WorkerStateConverter())

  print $"Inited ClientTypes in {serviceName}"
