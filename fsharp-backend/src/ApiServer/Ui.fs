/// Supports loading of HTML pages
///
/// Handles replacements of known variables (e.g. `{{LIVERELOADJS}}`)
/// used within .html files
module ApiServer.Ui

open Microsoft.AspNetCore.Http
open System.Text

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth
open Http

module File = LibBackend.File
module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account

/// Loads the Admin UI HTML template
///
/// Handles the replacement of known used variables
/// such as `{{ENVIRONMENT_NAME}}`
let adminUiTemplate : string =
  let liveReloadStr =
    "<script type=\"text/javascript\" src=\"//localhost:35729/livereload.js\"> </script>"

  let liveReloadJs = if Config.browserReloadEnabled then liveReloadStr else ""
  let uiHtml = File.readfile Config.Templates "ui.html"
  let appSupport = File.readfile Config.Webroot "appsupport.js"

  // Load as much of this as we can in advance
  // CLEANUP make APIs to load the dynamic data
  uiHtml
    .Replace("{{ENVIRONMENT_NAME}}", LibService.Config.envDisplayName)
    .Replace("{{LIVERELOADJS}}", liveReloadJs)
    .Replace("{{HEAPIO_ID}}", LibService.Config.heapioId)
    .Replace("{{ROLLBARCONFIG}}", Config.rollbarJs)
    .Replace("{{PUSHERCONFIG}}", LibBackend.Pusher.jsConfigString)
    .Replace("{{USER_CONTENT_HOST}}", Config.bwdServerContentHost)
    .Replace("{{APPSUPPORT}}", appSupport)
    .Replace("{{BUILD_HASH}}", LibService.Config.buildHash)

let hashedFilename (filename : string) (hash : string) : string =
  let parts = filename.Split '.'

  if parts.Length < 2 then
    Exception.raiseInternal "incorrect hash name" [ "filename", filename ]
  else
    let extension = parts[parts.Length - 1]
    let name = parts[.. (parts.Length - 2)] |> String.concat "."
    $"/{name}-{hash}.{extension}"


let prodHashReplacements : Map<string, string> =
  "etags.json"
  |> File.readfile Config.Webroot
  |> Json.Vanilla.deserialize<Map<string, string>>
  |> Map.remove "__date"
  |> Map.remove ".gitkeep"
  // Only hash our assets, not vendored assets
  |> Map.filterWithIndex (fun k _ -> not (String.includes "vendor/" k))
  |> Map.toList
  |> List.map (fun (filename, hash) -> ($"/{filename}", hashedFilename filename hash))
  |> Map.ofList

let prodHashReplacementsString : string =
  prodHashReplacements |> Json.Vanilla.serialize


// TODO: clickjacking/ CSP/ frame-ancestors
let uiHtml
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (csrfToken : string)
  (localhostAssets : string option)
  (accountCreated : NodaTime.Instant)
  (user : Account.UserInfo)
  : string =

  let shouldHash =
    if localhostAssets = None then Config.hashStaticFilenames else false

  let hashReplacements = if shouldHash then prodHashReplacementsString else "{}"

  let accountCreatedMsTs =
    accountCreated.ToUnixTimeMilliseconds()
    // CLEANUP strip milliseconds to make it identical to ocaml
    |> fun x -> (x / 1000L) * 1000L
    |> string

  let staticHost =
    match localhostAssets with
    // TODO: can add other people to this for easier debugging
    | Some username -> $"darklang-{username}.ngrok.io"
    | _ -> Config.apiServerStaticHost


  let t = StringBuilder(adminUiTemplate)

  // Replace any filenames in the response html with the hashed version
  if shouldHash then
    prodHashReplacements
    |> Map.iter (fun filename hashed ->
      t.Replace(filename, hashed) |> ignore<StringBuilder>)

  // CLEANUP move functions into an API call, or even to the CDN
  // CLEANUP move the user info into an API call
  t
    .Replace("{{ALLFUNCTIONS}}", Functions.functions user.admin)
    .Replace("{{USER_CONTENT_HOST}}", Config.bwdServerContentHost)
    .Replace("{{USER_USERNAME}}", string user.username)
    .Replace("{{USER_EMAIL}}", user.email)
    .Replace("{{USER_FULLNAME}}", user.name)
    .Replace("{{USER_CREATED_AT_UNIX_MSTS}}", accountCreatedMsTs)
    .Replace("{{USER_IS_ADMIN}}", (if user.admin then "true" else "false"))
    .Replace("{{USER_ID}}", string user.id)
    .Replace("{{CANVAS_ID}}", string canvasID)
    .Replace("{{CANVAS_NAME}}", string canvasName)
    .Replace("{{STATIC}}", staticHost)
    .Replace("{{HASH_REPLACEMENTS}}", hashReplacements)
    .Replace("{{CSRF_TOKEN}}", csrfToken)
  |> ignore<StringBuilder>

  string t

/// API endpoint that returns HTML for given Canvas
///
/// Contains special logic for special case of integration tests
let uiHandler (ctx : HttpContext) : Task<string> =
  task {
    use t = startTimer "read-request" ctx
    let user = loadUserInfo ctx
    let sessionData = loadSessionData ctx
    let canvasInfo = loadCanvasInfo ctx
    let localhostAssets = ctx.GetQueryStringValue "localhost-assets"

    t.next "create-at"
    let! createdAt = Account.getUserCreatedAt user.username

    // Create the data for integration tests
    t.next "integration-tests"
    let integrationTests =
      ctx.GetQueryStringValue "integration-test" |> Option.isSome

    if integrationTests && Config.allowTestRoutes then
      do! LibBackend.Canvas.loadAndResaveFromTestFile canvasInfo

    t.next "html-response"
    let result =
      uiHtml
        canvasInfo.id
        canvasInfo.name
        sessionData.csrfToken
        localhostAssets
        createdAt
        user

    return result
  }
