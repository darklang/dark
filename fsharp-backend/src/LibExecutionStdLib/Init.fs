module LibExecutionStdLib.Init

// Initialize LibExecutionStdLib

open Prelude

let init (serviceName : string) : unit =
  print $"Initing LibExecutionStdLib in {serviceName}"
  print $" Inited LibExecutionStdLib in {serviceName}"
