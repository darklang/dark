module BuiltinExecution.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin

let fnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins : Builtins =
  Builtin.combine
    [ Libs.NoModule.builtins

      // Libs.Bool.builtins

      // Libs.Int8.builtins
      // Libs.UInt8.builtins
      // Libs.Int16.builtins
      // Libs.UInt16.builtins
      // Libs.Int32.builtins
      // Libs.UInt32.builtins
      Libs.Int64.builtins
      // Libs.UInt64.builtins
      // Libs.Int128.builtins
      // Libs.UInt128.builtins

      // Libs.Float.builtins

      // Libs.Math.builtins

      // Libs.Bytes.builtins

      // Libs.Char.builtins
      // Libs.String.builtins

      // Libs.List.builtins
      // Libs.Dict.builtins

      // Libs.DateTime.builtins
      // Libs.Uuid.builtins

      // Libs.Base64.builtins

      // Libs.Json.builtins
      // Libs.AltJson.builtins

      // Libs.HttpClient.builtins httpConfig

      // Libs.LanguageTools.builtins
      // Libs.Parser.builtins

      // Libs.Crypto.builtins
      // Libs.X509.builtins

      // Libs.Packages.builtins pm
      ]
    fnRenames
