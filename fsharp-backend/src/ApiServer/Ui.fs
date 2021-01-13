module ApiServer.Ui

open Prelude
module Config = LibBackend.Config

let adminUiTemplate = LibBackend.File.readfile Config.Templates "ui.html"


let prodHashReplacements : string =
  "etags.json"
  |> LibBackend.File.readfile Config.Webroot
  |> Prelude.Json.AutoSerialize.deserialize<Map<string, string>>
  |> Map.remove "__date"
  |> Map.remove ".gitkeep"
  // Only hash our assets, not vendored assets
  |> Map.filter (fun k v -> not (k.Contains "vendor/"))
  |> Map.toList
  |> List.map
       (fun (filename, hash) ->
         let hashed =
           match filename.Split '.' with
           | [| name; extension |] -> $"/{name}-{hash}{extension}"
           | _ -> failwith "incorrect hash name"

         ($"/{filename}", hashed))
  |> Map.ofList
  |> Prelude.Json.AutoSerialize.serialize



// FSTODO: clickjacking/ CSP/ frame-ancestors
let uiHtml
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (csrfToken : string)
  (localhostAssets : string option)
  (accountCreated : System.DateTime)
  (user : LibBackend.Account.UserInfo)
  : string =

  let hashReplacements =
    let shouldHash =
      if localhostAssets = None then Config.hashStaticFilenames else false

    if shouldHash then prodHashReplacements else "{}"

  let accountCreatedMsTs =
    System.DateTimeOffset(accountCreated).ToUnixTimeMilliseconds().ToString()

  let staticHost =
    match localhostAssets with
    (* TODO: can add other people to this for easier debugging *)
    | Some username -> $"darklang-{username}.ngrok.io"
    | _ -> Config.staticHost


  let liveReloadJs =
    if Config.browserReloadEnabled then
      "<script type=\"text/javascript\" src=\"//localhost:35729/livereload.js\"> </script>"
    else
      ""

  (* TODO: allow APPSUPPORT in here *)
  let t = System.Text.StringBuilder(adminUiTemplate)

  t
    .Replace("{{ENVIRONMENT_NAME}}", Config.envDisplayName)
    .Replace("{{ALLFUNCTIONS}}", Api.functions user.admin)
    .Replace("{{LIVERELOADJS}}", liveReloadJs)
    .Replace("{{STATIC}}", staticHost)
    .Replace("{{HEAPIO_ID}}", Config.heapioId)
    .Replace("{{ROLLBARCONFIG}}", Config.rollbarJs)
    .Replace("{{PUSHERCONFIG}}", Config.pusherJs)
    .Replace("{{USER_CONTENT_HOST}}", Config.userContentHost)
    .Replace("{{USER_USERNAME}}", user.username.ToString())
    .Replace("{{USER_EMAIL}}", user.email)
    .Replace("{{USER_FULLNAME}}", user.name)
    .Replace("{{USER_CREATED_AT_UNIX_MSTS}}", accountCreatedMsTs)
    .Replace("{{USER_IS_ADMIN}}", (if user.admin then "true" else "false"))
    .Replace("{{USER_ID}}", user.id.ToString())
    .Replace("{{CANVAS_ID}}", (canvasID.ToString()))
    .Replace("{{CANVAS_NAME}}", canvasName.ToString())
    .Replace("{{APPSUPPORT}}",
             (LibBackend.File.readfile LibBackend.Config.Webroot "appsupport.js"))
    .Replace("{{HASH_REPLACEMENTS}}", hashReplacements)
    .Replace("{{CSRF_TOKEN}}", csrfToken)
    .Replace("{{BUILD_HASH}}", Config.buildHash)
    // There isn't separate routing for static in ASP.NET
    .Replace("http://static.darklang.localhost:8000", "darklang.localhost:9000")
    // FSTODO: Config is set up for OCaml right now
    .Replace("http://darklang.localhost:8000", "darklang.localhost:9000")
    .Replace("http://builtwithdark.localhost:8000", "builtwithdark.localhost:9001")
    .ToString()
