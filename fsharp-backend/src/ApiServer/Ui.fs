module ApiServer.Ui

open Microsoft.AspNetCore.Http
open Giraffe

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Prelude
open Tablecloth

module Config = LibBackend.Config
module Session = LibBackend.Session
module Account = LibBackend.Account

let adminUiTemplate : Lazy<string> =
  lazy (LibBackend.File.readfile Config.Templates "ui.html")

let appSupportFile : Lazy<string> =
  lazy (LibBackend.File.readfile LibBackend.Config.Webroot "appsupport.js")

let prodHashReplacements : Lazy<string> =
  lazy
    ("etags.json"
     |> LibBackend.File.readfile Config.Webroot
     |> Json.Vanilla.deserialize<Map<string, string>>
     |> Map.remove "__date"
     |> Map.remove ".gitkeep"
     // Only hash our assets, not vendored assets
     |> Map.filterWithIndex (fun k v -> not (String.includes "vendor/" k))
     |> Map.toList
     |> List.map
          (fun (filename, hash) ->
            let hashed =
              match filename.Split '.' with
              | [| name; extension |] -> $"/{name}-{hash}{extension}"
              | _ -> failwith "incorrect hash name"

            ($"/{filename}", hashed))
     |> Map.ofList
     |> Json.Vanilla.serialize)



// FSTODO: clickjacking/ CSP/ frame-ancestors
let uiHtml
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (csrfToken : string)
  (localhostAssets : string option)
  (accountCreated : System.DateTime)
  (user : Account.UserInfo)
  : string =

  let hashReplacements =
    let shouldHash =
      if localhostAssets = None then Config.hashStaticFilenames else false

    if shouldHash then prodHashReplacements.Force() else "{}"

  let accountCreatedMsTs =
    System.DateTimeOffset(accountCreated).ToUnixTimeMilliseconds()
    // CLEANUP strip milliseconds to make it identical to ocaml
    |> fun x -> (x / 1000L) * 1000L
    |> toString

  let staticHost =
    match localhostAssets with
    // TODO: can add other people to this for easier debugging
    | Some username -> $"darklang-{username}.ngrok.io"
    | _ -> Config.apiServerStaticHost


  let liveReloadJs =
    if Config.browserReloadEnabled then
      "<script type=\"text/javascript\" src=\"//localhost:35729/livereload.js\"> </script>"
    else
      ""

  (* TODO: allow APPSUPPORT in here *)
  let t = System.Text.StringBuilder(adminUiTemplate.Force())

  t
    .Replace("{{ENVIRONMENT_NAME}}", LibService.Config.envDisplayName)
    .Replace("{{ALLFUNCTIONS}}", (Functions.functions user.admin).Force())
    .Replace("{{LIVERELOADJS}}", liveReloadJs)
    .Replace("{{STATIC}}", staticHost)
    .Replace("{{HEAPIO_ID}}", Config.heapioId)
    .Replace("{{ROLLBARCONFIG}}", Config.rollbarJs)
    .Replace("{{PUSHERCONFIG}}", LibBackend.Pusher.jsConfigString)
    .Replace("{{USER_CONTENT_HOST}}", Config.bwdServerContentHost)
    .Replace("{{USER_USERNAME}}", user.username.ToString())
    .Replace("{{USER_EMAIL}}", user.email)
    .Replace("{{USER_FULLNAME}}", user.name)
    .Replace("{{USER_CREATED_AT_UNIX_MSTS}}", accountCreatedMsTs)
    .Replace("{{USER_IS_ADMIN}}", (if user.admin then "true" else "false"))
    .Replace("{{USER_ID}}", user.id.ToString())
    .Replace("{{CANVAS_ID}}", (canvasID.ToString()))
    .Replace("{{CANVAS_NAME}}", canvasName.ToString())
    .Replace("{{APPSUPPORT}}", appSupportFile.Force())
    .Replace("{{HASH_REPLACEMENTS}}", hashReplacements)
    .Replace("{{CSRF_TOKEN}}", csrfToken)
    .Replace("{{BUILD_HASH}}", LibService.Config.buildHash)
    .ToString()

let uiHandler (ctx : HttpContext) : Task<string> =
  task {
    let user = Middleware.loadUserInfo ctx
    let sessionData = Middleware.loadSessionData ctx
    let canvasInfo = Middleware.loadCanvasInfo ctx
    let! createdAt = Account.getUserCreatedAt user.username
    let localhostAssets = ctx.TryGetQueryStringValue "localhost-assets"

    return
      uiHtml
        canvasInfo.id
        canvasInfo.name
        sessionData.csrfToken
        localhostAssets
        createdAt
        user
  }
