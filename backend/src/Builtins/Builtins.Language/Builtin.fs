module Builtins.Language.Builtin

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () =
  Builtin.combine
    [ Libs.LanguageTools.builtins ()
      Libs.Parser.builtins ()
      Libs.Reflection.builtins ()
      Libs.Instrumentation.builtins () ]
    fnRenames


/// Darklang looking at itself: the parser, reflection over runtime values, and instrumentation.
///
/// It does NOT require the `Store` platform. Its builtins take no package manager and its assembly
/// does not reference `LibDB` at all; the package store its parser resolves against arrives through
/// the execution state, not through another platform's builtins. The Dark code written on top of it
/// does wrap `Store` builtins, which is a fact about `darklang/languageTools` rather than about this
/// platform.
let platform : LibExecution.Platform.Platform =
  { name = "Lang"
    version = 0
    description = "Parser, reflection, instrumentation."
    builtins = builtins ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = true }
