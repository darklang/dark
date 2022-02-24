module BackendOnlyStdLib.Init

open Prelude

// WHATISTHIS why do we have these when they're empty?

/// Initialize BackendOnlyStdLib
///
/// Currently a no-ops
let init (serviceName : string) : unit =
  print $"Initing BackendOnlyStdLib in {serviceName}"
  print $" Inited BackendOnlyStdLib in {serviceName}"
