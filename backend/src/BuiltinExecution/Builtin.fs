module BuiltinExecution.Builtin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin

let fnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let contents (httpConfig : Libs.HttpClient.Configuration) : Builtin.Contents =
  Builtin.combine
    [ Libs.Bool.contents
      Libs.Base64.contents
      Libs.Bytes.contents
      Libs.Char.contents
      Libs.DateTime.contents
      Libs.Dict.contents
      Libs.Float.contents
      Libs.HttpClient.contents httpConfig
      Libs.Json.contents
      Libs.Math.contents
      Libs.Uuid.contents
      Libs.Int.contents
      Libs.List.contents
      Libs.NoModule.contents
      Libs.Crypto.contents
      Libs.String.contents
      Libs.X509.contents ]
    fnRenames
