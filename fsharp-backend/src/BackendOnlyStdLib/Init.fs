module BackendOnlyStdLib.Init

// Initialize BackendOnlyStdLib

open Prelude

let init (serviceName : string) : unit =
  print $"Initing BackendOnlyStdLib in {serviceName}"
  print $" Inited BackendOnlyStdLib in {serviceName}"
