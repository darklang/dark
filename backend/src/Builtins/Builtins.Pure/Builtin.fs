module Builtins.Pure.Builtin

open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin

let fnRenames : Builtin.FnRenames = []

let builtins () : Builtins =
  Builtin.combine
    [ Libs.NoModule.builtins ()

      Libs.Bool.builtins ()

      Libs.Int8.builtins ()
      Libs.UInt8.builtins ()
      Libs.Int16.builtins ()
      Libs.UInt16.builtins ()
      Libs.Int32.builtins ()
      Libs.UInt32.builtins ()
      Libs.Int64.builtins ()
      Libs.UInt64.builtins ()
      Libs.Int128.builtins ()
      Libs.UInt128.builtins ()
      Libs.Int.builtins ()

      Libs.Float.builtins ()

      Libs.Math.builtins ()

      Libs.Blob.builtins ()
      Libs.Stream.builtins ()

      Libs.Char.builtins ()
      Libs.String.builtins ()
      Libs.Regex.builtins ()

      Libs.List.builtins ()
      Libs.Dict.builtins ()

      Libs.DateTime.builtins ()
      Libs.Uuid.builtins ()

      Libs.Base64.builtins ()

      Libs.Json.builtins ()
      Libs.AltJson.builtins ()

      Libs.Crypto.builtins ()
      Libs.X509.builtins () ]
    fnRenames


/// The floor: arithmetic, text, collections, encodings, crypto. No effects at all, which is what
/// makes a `Core`-only executable a sealed computation engine rather than a small dangerous one.
///
/// Every other platform `requires` this, because every builtin that can fail returns an
/// `Option`/`Result` whose Dark definitions live in Core's package modules.
let platform : LibExecution.Platform.Platform =
  { name = "Core"
    version = 0
    description = "Arithmetic, text, collections, JSON, crypto. No effects."
    builtins = builtins ()
    requires = []
    dynamicEffects = Set.empty
    requiresStore = false }
