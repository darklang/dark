/// StdLib functions to work with Twilio, namely supporting SMS communicationd
module BackendOnlyStdLib.LibTwilio

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

module Canvas = LibBackend.Canvas

module LibHttpClientAuth = LibExecutionStdLib.LibHttpClientAuth

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs

let varA = TVariable "a"

let fns : List<BuiltInFn> =
  [ { name = fn "Twilio" "sendText" 0
      parameters =
        [ Param.make "accountSID" TStr ""
          Param.make "authToken" TStr ""
          Param.make "fromNumber" TStr ""
          Param.make "toNumber" TStr ""
          Param.make "body" TStr "" ]
      returnType = TDict varA
      description =
        "Send text with <param body> to phone number <param toNumber> from number <param fromNumber>, authenticated via <param accountSID> and <param authToken>"
      fn =
        (function
        | (s,
           [ DStr accountSID
             DStr authToken
             DStr fromNumber
             DStr toNumber
             DStr body ]) ->
          let basicAuthString =
            LibHttpClientAuth.encodeBasicAuthBroken accountSID authToken
          let encoding = "application/x-www-form-urlencoded" in
          let headers =
            [ ("Authorization", DStr basicAuthString)
              ("Content-Type", DStr encoding) ]
            |> Map
            |> DObj
          let hostUrl = Canvas.urlFor s.program.canvasName in
          let body =
            [ ("From", DStr fromNumber)
              ("To", DStr toNumber)
              ("Body", DStr body)
              ("ValidityPeriod", DStr "900")
              ("StatusCallback", DStr(hostUrl + "/twilioCallback")) ]
            |> Map
            |> DObj
          let twilioUri =
            $"https://api.twilio.com/2010-04-01/Accounts/{accountSID}/Messages.json"
          LegacyHttpClient2.sendRequest
            twilioUri
            System.Net.Http.HttpMethod.Post
            (Some body)
            (DObj Map.empty)
            headers
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      // Deprecated due to using encodeBasicAuthBroken
      deprecated = DeprecatedBecause "bad unicode support" }


    { name = fn "Twilio" "sendText" 1
      parameters =
        [ Param.make "accountSID" TStr ""
          Param.make "authToken" TStr ""
          Param.make "fromNumber" TStr ""
          Param.make "toNumber" TStr ""
          Param.make "body" TStr "" ]
      returnType = TDict varA
      description =
        "Send text with <param body> to phone number <param toNumber> from number <param fromNumber>, authenticated via <param accountSID> and <param authToken>"
      fn =
        (function
        | (s,
           [ DStr accountSID
             DStr authToken
             DStr fromNumber
             DStr toNumber
             DStr body ]) ->
          let basicAuthString =
            LibHttpClientAuth.encodeBasicAuth accountSID authToken
          let encoding = "application/x-www-form-urlencoded" in
          let headers =
            [ ("Authorization", DStr basicAuthString)
              ("Content-Type", DStr encoding) ]
            |> Map
            |> DObj
          let hostUrl = Canvas.urlFor s.program.canvasName in
          let body =
            [ ("From", DStr fromNumber)
              ("To", DStr toNumber)
              ("Body", DStr body)
              ("ValidityPeriod", DStr "900")
              ("StatusCallback", DStr(hostUrl + "/twilioCallback")) ]
            |> Map
            |> DObj
          let twilioUri =
            $"https://api.twilio.com/2010-04-01/Accounts/{accountSID}/Messages.json"
          LegacyHttpClient2.sendRequest
            twilioUri
            System.Net.Http.HttpMethod.Post
            (Some body)
            (DObj Map.empty)
            headers
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
