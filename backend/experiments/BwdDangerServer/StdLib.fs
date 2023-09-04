module BwdDangerServer.StdLib

open Prelude
module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let typeRenames : Builtin.TypeRenames =
  // old names, new names
  // eg: typ "Http" "Response" 0, typ "Http" "Response" 1
  []

let contents : Builtin.Contents =
  Builtin.combine [ Libs.Experiments.contents ] fnRenames typeRenames
