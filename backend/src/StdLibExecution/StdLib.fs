module StdLibExecution.StdLib

open Prelude
open LibExecution.RuntimeTypes

module StdLib = LibExecution.StdLib

let fnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let typeRenames =
  // old names, new names
  // eg: typ "Http" "Response" 0, typ "Http" "Response" 1
  []

let contents : StdLib.Contents =
  StdLib.combine
    [ Libs.Bool.contents
      Libs.Bytes.contents
      Libs.Char.contents
      Libs.DateTime.contents
      Libs.Dict.contents
      Libs.Float.contents
      Libs.Http.contents
      Libs.HttpClient.contents
      Libs.HttpClientAuth.contents
      Libs.Json.contents
      Libs.Math.contents
      Libs.Uuid.contents
      Libs.Int.contents
      Libs.List.contents
      // Libs.Middleware.contents
      Libs.NoModule.contents
      Libs.Option.contents
      Libs.Result.contents
      Libs.Crypto.contents
      Libs.String.contents
      Libs.Tuple2.contents
      Libs.Tuple3.contents
      Libs.X509.contents
      Libs.Password.contents ]
    fnRenames
    typeRenames
