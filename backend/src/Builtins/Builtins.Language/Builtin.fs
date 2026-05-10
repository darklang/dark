module Builtins.Language.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () =
  Builtin.combine
    [ Libs.LanguageTools.builtins ()
      Libs.Parser.builtins ()
      Libs.Reflection.builtins () ]
    fnRenames
