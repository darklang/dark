module BuiltinExecution.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin

let fnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let builtins (httpConfig : Libs.HttpClient.Configuration) : Builtins =
  Builtin.combine
    [ Libs.Bool.builtins
      Libs.Base64.builtins
      Libs.Bytes.builtins
      Libs.Char.builtins
      Libs.DateTime.builtins
      Libs.Dict.builtins
      Libs.LanguageTools.builtins
      Libs.Float.builtins
      Libs.HttpClient.builtins httpConfig
      Libs.Json.builtins
      Libs.AltJson.builtins
      Libs.Math.builtins
      Libs.Uuid.builtins
      Libs.Int64.builtins
      Libs.UInt64.builtins
      Libs.Int8.builtins
      Libs.UInt8.builtins
      Libs.Int16.builtins
      Libs.UInt16.builtins
      Libs.Int32.builtins
      Libs.UInt32.builtins
      Libs.Int128.builtins
      Libs.UInt128.builtins
      Libs.List.builtins
      Libs.NoModule.builtins
      Libs.Crypto.builtins
      Libs.String.builtins
      Libs.Parser.builtins
      Libs.X509.builtins ]
    fnRenames
