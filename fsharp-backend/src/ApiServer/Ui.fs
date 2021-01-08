module ApiServer.Ui

open Prelude
module Config = LibBackend.Config

let adminUiTemplate = LibBackend.File.readfile Config.Templates "ui.html"

// FSTODO: clickjacking/ CSP/ frame-ancestors
let uiHtml
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (csrfToken : string)
  (localhostAssets : string option)
  (accountCreated : System.DateTime)
  (user : LibBackend.Account.UserInfo)
  : string =

  let accountCreatedMsTs =
    System.DateTimeOffset(accountCreated).ToUnixTimeMilliseconds().ToString()

  let staticHost =
    match localhostAssets with
    (* TODO: can add other people to this for easier debugging *)
    | Some username -> $"darklang-{username}.ngrok.io"
    | _ -> Config.staticHost

  // let hashStaticFilenames =
  //   if local = None then Config.hashStaticFilenames else false

  let liveReloadJs =
    if Config.browserReloadEnabled then
      "<script type=\"text/javascript\" src=\"//localhost:35729/livereload.js\"> </script>"
    else
      ""

  let hashedStaticFilenames source =
    // FSTODO
    ""
  // if not hash_static_filenames
  // then "{{HASH_REPLACEMENTS}}" "{}" x
  // else
  //   let etags_str = File.readfile ~root:Webroot "etags.json" in
  //   let etags_json = Yojson.Safe.from_string etags_str in
  //   let etag_assoc_list =
  //     to_assoc_list etags_json
  //     |> List.filter ~f:(fun (file, _) -> not (String.equal "__date" file))
  //     |> List.filter (* Only hash our assets, not vendored assets *)
  //          ~f:(fun (file, _) ->
  //            not (String.is_substring ~substring:"vendor/" file))
  //   in
  //   x
  //   |> fun instr ->
  //   etag_assoc_list
  //   |> List.fold ~init:instr ~f:(fun acc (file, hash) ->
  //          (Util.string_replace file (hashed_filename file hash)) acc)
  //   |> fun instr ->
  //   Util.string_replace
  //     "{{HASH_REPLACEMENTS}}"
  //     ( etag_assoc_list
  //     |> List.map ~f:(fun (k, v) ->
  //            ("/" ^ k, `String ("/" ^ hashed_filename k v)))
  //     |> (fun x -> `Assoc x)
  //     |> Yojson.Safe.to_string )
  //     instr)
  //

  (* TODO: allow APPSUPPORT in here *)
  let t = System.Text.StringBuilder(adminUiTemplate)

  t
    .Replace("{{ENVIRONMENT_NAME}}", Config.envDisplayName)
    .Replace("{{ALLFUNCTIONS}}", Api.functions "") //FSTODO user.username)
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
    // .Replace("{{HASH_REPLACEMENTS}}", hash_replacements)
    .Replace("{{HASH_REPLACEMENTS}}", "[]")
    .Replace("{{CSRF_TOKEN}}", csrfToken)
    .Replace("{{BUILD_HASH}}", Config.buildHash)
    // There isn't separate routing for static in ASP.NET
    .Replace("http://static.darklang.localhost:8000", "darklang.localhost:9000")
    // FSTODO: Config is set up for OCaml right now
    .Replace("http://darklang.localhost:8000", "darklang.localhost:9000")
    .Replace("http://builtwithdark.localhost:8000", "builtwithdark.localhost:9001")
    .ToString()
