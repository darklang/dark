module TestUtils.Init

// Initialize TestUtils

open Prelude

let init (serviceName : string) : unit =
  print $"Initing TestUtils in {serviceName}"
  print $" Inited TestUtils in {serviceName}"
