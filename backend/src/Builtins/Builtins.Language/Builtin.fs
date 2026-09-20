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
/// It does NOT require the `Store` platform, and it needs no store handle. Its parser builtins
/// answer WrittenTypes, which are unresolved: turning a written name into a package reference is
/// Dark code in `darklang/languageTools` over `Store`'s builtins, which declare `package-read`. So
/// the store read a person might expect here happens one platform over and is gated there. This
/// was checked when the question "does `Lang` under-declare what it reaches" came up: no builtin in
/// this assembly takes a package manager or reads the execution state's.
///
/// The one host-state read is `platformsInstalled` answering an installed platform's artifact hash,
/// which describes the runtime the way `getAllBuiltinFns` does and is what `dark platforms` shows
/// anyone.
let platform : LibExecution.Platform.Platform =
  { name = "Lang"
    version = 0
    description = "Parser, reflection, instrumentation."
    builtins = builtins ()
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    // False, and it was wrongly true: nothing here opens or reads a package database.
    requiresStore = false }
