module BuiltinCli.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let contents =
  Builtin.combine
    [ Libs.Directory.contents
      Libs.Environment.contents
      Libs.File.contents
      Libs.Process.contents
      Libs.Output.contents
      Libs.Stdin.contents
      Libs.Time.contents
      Libs.LanguageTools.contents ]
    fnRenames
