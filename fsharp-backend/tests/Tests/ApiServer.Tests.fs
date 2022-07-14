module Tests.ApiServer

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open System.Net.Http
open Microsoft.AspNetCore.Http

open Tablecloth
open Prelude
open Prelude.Tablecloth
open TestUtils.TestUtils
open LibService.Exception

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibExecution.OCamlTypes
module ORT = OT.RuntimeT
module Convert = LibExecution.OCamlTypes.Convert
module TI = LibBackend.TraceInputs
module Canvas = LibBackend.Canvas

type AuthData = LibBackend.Session.AuthData

open ApiServer

type Server =
  | FSharp
  | OCaml

let ident = Fun.identity

type Client = { http : HttpClient; csrf : string }

type C = Lazy<Task<Client>>

let portFor (server : Server) : int =
  match server with
  | OCaml -> 8000 // nginx for the ocaml server is on port 8000
  | FSharp -> TestConfig.apiServerNginxPort

let forceLogin (username : UserName.T) : Task<Client> =
  task {
    let! user = LibBackend.Account.getUser username // validate user exists
    let _user = Exception.unwrapOptionInternal "" [] user
    let! authData = LibBackend.Session.insert username
    let cookie =
      System.Net.Cookie(
        "__session",
        authData.sessionKey,
        "",
        LibBackend.Config.cookieDomain
      )
    let messageHandler = new SocketsHttpHandler()
    messageHandler.CookieContainer.Add(cookie)
    let client = new HttpClient(messageHandler, true)
    client.Timeout <- System.TimeSpan.FromSeconds 60.0
    return { http = client; csrf = authData.csrfToken }
  }

/// login as test user and return the csrfToken (the cookies are stored in httpclient)
let login (username : string) (password : string) : Task<Client> =
  task {
    let client = new HttpClient()
    client.Timeout <- System.TimeSpan.FromSeconds 1
    let port = portFor FSharp

    use loginReq =
      new HttpRequestMessage(
        HttpMethod.Post,
        $"http://darklang.localhost:{port}/login"
      )

    let body =
      [ ("username", username); ("password", password) ]
      |> List.map Tuple2.toKeyValuePair

    loginReq.Content <- new FormUrlEncodedContent(body)

    let! loginResp = client.SendAsync(loginReq)
    let! loginContent = loginResp.Content.ReadAsStringAsync()

    if loginResp.StatusCode <> System.Net.HttpStatusCode.OK then
      debuG "loginContent" loginContent
      Expect.equal loginResp.StatusCode System.Net.HttpStatusCode.OK ""

    let csrfToken =
      match loginContent with
      | Regex "const csrfToken = \"(.*?)\";" [ token ] -> token
      | _ ->
        print loginContent
        Exception.raiseInternal
          $"could not find csrfToken"
          [ "loginContent", loginContent ]

    return { http = client; csrf = csrfToken }
  }

let getAsync
  (server : Server)
  (client : C)
  (path : string)
  : Task<HttpResponseMessage> =
  task {
    let! client = Lazy.force client
    let port = portFor server
    let url = $"http://darklang.localhost:{port}{path}"
    use message = new HttpRequestMessage(HttpMethod.Get, url)
    message.Headers.Add("X-CSRF-Token", client.csrf)

    return! client.http.SendAsync(message)
  }

let postAsync
  (server : Server)
  (client : C)
  (path : string)
  (body : string)
  : Task<HttpResponseMessage> =
  task {
    let! client = Lazy.force client
    let port = portFor server
    let url = $"http://darklang.localhost:{port}{path}"
    use message = new HttpRequestMessage(HttpMethod.Post, url)
    message.Headers.Add("X-CSRF-Token", client.csrf)

    message.Content <-
      new StringContent(body, System.Text.Encoding.UTF8, "application/json")

    return! client.http.SendAsync(message)
  }

let deserialize<'a> (str : string) : 'a = Json.OCamlCompatible.deserialize<'a> str

let serialize = Json.OCamlCompatible.serialize

let noBody () = ""



// let getInitialLoad (client : C) (canvasName : CanvasName.T) : Task<InitialLoad.T> =
//   task {
//     let! (r : HttpResponseMessage) =
//       postAsync FSharp client $"/api/{canvasName}/initial_load" ""

//     Expect.equal r.StatusCode System.Net.HttpStatusCode.OK ""
//     let! body = r.Content.ReadAsStringAsync()
//     return deserialize<InitialLoad.T> body
//   }



// let testUiReturnsTheSame (client : C) (canvas : CanvasName.T) : Task<unit> =
//   task {
//     let! (ocamlResponse : HttpResponseMessage) =
//       try
//         getAsync OCaml client $"/a/{canvas}/"
//       with
//       | e ->
//         Exception.raiseInternal "exception getting ocaml data" [ "canvas", canvas ] e

//     let! (fsharpResponse : HttpResponseMessage) =
//       try
//         getAsync FSharp client $"/a/{canvas}/"
//       with
//       | e ->
//         Exception.raiseInternal
//           "exception getting fsharp data"
//           [ "canvas", canvas ]
//           e

//     Expect.equal
//       ocamlResponse.StatusCode
//       fsharpResponse.StatusCode
//       $"status code in {canvas}"

//     let! ocamlContent = ocamlResponse.Content.ReadAsStringAsync()
//     let! fsharpContent = fsharpResponse.Content.ReadAsStringAsync()

//     let parseFns (s : string) : string * List<Functions.FunctionMetadata> =
//       match s with
//       | RegexAny "(.*const complete = )(\[.*\])(;\n.*)" [ before; fns; after ] ->
//         let text = $"{before}{after}"

//         let fns =
//           fns
//           |> FsRegEx.replace "\\s+" " " // ignore differences in string spacing in docstrings
//           |> Json.Vanilla.deserialize<List<Functions.FunctionMetadata>>

//         (text, fns)
//       | _ -> Exception.raiseInternal "doesn't match" [ "string", s ]

//     let ocamlContent, ocamlFns = parseFns ocamlContent
//     let actualFsharpContent, fsharpFns = parseFns fsharpContent

//     let port = portFor OCaml

//     // There have been some tiny changes, let's work around them.
//     // The values here are the NEW values
//     let ocamlFns =
//       ocamlFns
//       |> List.map (fun (fn : Functions.FunctionMetadata) ->
//         match fn.name with
//         | "assoc" ->
//           { fn with
//               description = "Returns a copy of `dict` with the `key` set to `val`."
//               parameters =
//                 [ { name = "dict"
//                     tipe = "Dict"
//                     block_args = []
//                     optional = false
//                     description = "" }
//                   { name = "key"
//                     tipe = "Str"
//                     block_args = []
//                     optional = false
//                     description = "" }
//                   { name = "val"
//                     tipe = "Any"
//                     block_args = []
//                     optional = false
//                     description = "" } ] }
//         | "dissoc" ->
//           { fn with
//               parameters =
//                 [ { name = "dict"
//                     tipe = "Dict"
//                     block_args = []
//                     optional = false
//                     description = "" }
//                   { name = "key"
//                     tipe = "Str"
//                     block_args = []
//                     optional = false
//                     description = "" } ]
//               description =
//                 "If the `dict` contains `key`, returns a copy of `dict` with `key` and its associated value removed. Otherwise, returns `dict` unchanged." }
//         | "Object::empty" -> { fn with description = "Returns an empty dictionary." }
//         | "Object::merge" ->
//           { fn with
//               description =
//                 "Returns a combined dictionary with both dictionaries' entries. If the same key exists in both `left` and `right`, it will have the value from `right`." }
//         | "Object::toJSON_v1" ->
//           { fn with
//               description = "Returns `dict` as a JSON string."
//               parameters =
//                 [ { name = "dict"
//                     tipe = "Dict"
//                     block_args = []
//                     optional = false
//                     description = "" } ] }
//         | "DB::queryOneWithKey_v2" ->
//           { fn with
//               description =
//                 fn.description + ". Previously called DB::queryOnewithKey_v2" }
//         | "DB::queryWithKey_v2" ->
//           { fn with
//               description = fn.description + ". Previous called DB::queryWithKey_v2" }
//         | "DB::query_v3" ->
//           { fn with description = fn.description + ". Previously called DB::query_v3" }
//         | "DB::query_v2" ->
//           { fn with description = fn.description + ". Previously called DB::query_v3" }
//         | "DB::queryOne_v2" ->
//           { fn with
//               description = fn.description + ". Previously called DB::queryOne_v2" }
//         | _ -> fn)

//     let expectedFsharpContent =
//       ocamlContent
//         // a couple of specific ones
//         .Replace(
//           $"static.darklang.localhost:{port}",
//           $"static.darklang.localhost:{LibService.Config.apiServerNginxPort}"
//         )
//         .Replace(
//           $"builtwithdark.localhost:{port}",
//           $"builtwithdark.localhost:{LibService.Config.bwdServerPort}"
//         )
//         // get the rest
//         .Replace(
//           $"localhost:{port}",
//           $"localhost:{LibService.Config.apiServerNginxPort}"
//         )

//     Expect.equal actualFsharpContent expectedFsharpContent ""

//     List.iter2
//       (fun (ffn : Functions.FunctionMetadata) ofn -> Expect.equal ffn ofn ffn.name)
//       fsharpFns
//       ocamlFns
//   }

type ApiResponse<'a> = Task<'a * System.Net.HttpStatusCode * Map<string, string>>

let postApiTestCase
  (client : C)
  (canvasName : CanvasName.T)
  (server : Server)
  (api : string)
  (requestBody : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  : ApiResponse<'a> =
  task {
    let! (response : HttpResponseMessage) =
      postAsync server client $"/api/{canvasName}/{api}" requestBody

    let! responseBody = response.Content.ReadAsStringAsync()

    let (result : 'a) =
      try
        responseBody |> deserialize |> canonicalizeBody
      with
      | e ->
        print
          $"Error deserializing or canonicalizing {server} in {canvasName}/{api} with body\n\n{requestBody}\n\n and response\n\n{responseBody}"
        e.Reraise()

    let headerMap (h : Headers.HttpResponseHeaders) : Map<string, string> =
      let clear str =
        if h.Contains str then
          h.Remove str |> ignore<bool>
          h.TryAddWithoutValidation(str, "XXX") |> ignore<bool>

      clear "Date"
      clear "Server"
      clear "Server-Timing"
      clear "x-darklang-execution-id"

      h.Remove "Connection" |> ignore<bool> // not useful, not in new API
      h.Remove "Strict-Transport-Security" |> ignore<bool> // only in new API

      h
      |> Seq.toList
      |> List.map (fun (KeyValue (x, y)) -> x, String.concat "," y)
      |> Map

    let headers = headerMap response.Headers
    return (result, response.StatusCode, headers)
  }

let postApiTest
  (api : string)
  (body : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  (client : C)
  (canvasName : CanvasName.T)
  : Task<unit> =
  task {
    let! (oContent, oStatus, oHeaders) as o =
      postApiTestCase client canvasName OCaml api body deserialize canonicalizeBody

    let! (fContent, fStatus, fHeaders) as f =
      postApiTestCase client canvasName FSharp api body deserialize canonicalizeBody

    if oStatus <> fStatus then
      print (
        $"Non-matching status codes: {api}\n\nbody:\n{body}\n\n"
        + $"ocaml:\n{o}\n\nfsharp:\n{f}"
      )

    Expect.equal fStatus oStatus "status"
    Expect.equal fContent oContent "content"
    Expect.equal fHeaders oHeaders "headers"
  }


// let testGetTraceData (client : C) (canvasName : CanvasName.T) : Task<unit> =
//   task {
//     let! (o : HttpResponseMessage) =
//       postAsync OCaml client $"/api/{canvasName}/all_traces" ""

//     Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
//     let! body = o.Content.ReadAsStringAsync()

//     let canonicalize (t : Traces.TraceData.T) : Traces.TraceData.T =
//       { t with
//           trace =
//             t.trace
//             |> Tuple2.mapSecond (fun td ->
//               { td with
//                   timestamp = td.timestamp.truncate ()
//                   input = td.input |> List.sortBy (fun (k, v) -> k) }) }

//     do!
//       body
//       |> deserialize<Traces.AllTraces.T>
//       |> fun ts -> ts.traces
//       |> Task.iterInParallel (fun (tlid, traceID) ->
//         task {
//           let (ps : Traces.TraceData.Params) = { tlid = tlid; trace_id = traceID }

//           do!
//             postApiTest
//               "get_trace_data"
//               (serialize ps)
//               (deserialize<Traces.TraceData.T>)
//               canonicalize
//               client
//               canvasName
//         })
//   }

// let testDBStats (client : C) (canvasName : CanvasName.T) : Task<unit> =
//   task {
//     let! canvas = Canvas.getMetaExn canvasName

//     let! canvasWithJustDBs = Canvas.loadAllDBs canvas
//     let parameters =
//       canvasWithJustDBs.dbs
//       |> Map.values
//       |> List.map (fun db -> db.tlid)
//       |> fun tlids -> ({ tlids = tlids } : DBs.DBStats.Params)

//     do!
//       postApiTest
//         "get_db_stats"
//         (serialize parameters)
//         deserialize<DBs.DBStats.T>
//         ident
//         client
//         canvasName
//   }


// let testExecuteFunction (client : C) (canvasName : CanvasName.T) : Task<unit> =
//   let tlid = gid ()

//   let (body : Execution.Function.Params) =
//     { tlid = tlid
//       trace_id = System.Guid.NewGuid()
//       caller_id = gid ()
//       args = [ ORT.DInt 5L; ORT.DInt 6L ]
//       fnname = "Int::add" }

//   postApiTest
//     "execute_function"
//     (serialize body)
//     (deserialize<Execution.Function.T>)
//     // New version includes the tlid of the caller
//     (fun (r : Execution.Function.T) ->
//       { r with touched_tlids = List.filter ((<>) tlid) r.touched_tlids })
//     client
//     canvasName


// let testTriggerHandler (client : C) (canvasName : CanvasName.T) =
//   task {
//     let! (initialLoad : InitialLoad.T) = getInitialLoad client canvasName

//     let handlerTLID =
//       initialLoad.toplevels
//       |> Convert.ocamlToplevel2PT
//       |> Tuple2.first
//       |> List.filterMap (fun h ->
//         // This test won't succeed unless you manually go to
//         // http://darklang.localhost:9000/a/{canvasName} and add a POST http handler with the
//         // path below
//         match h.spec with
//         | PT.Handler.HTTP ("/a-test-handler/:user", "POST", _) -> Some h.tlid
//         | _ -> None)
//       |> List.head
//       |> Exception.unwrapOptionInternal "" []

//     let (body : Execution.Handler.Params) =
//       { tlid = handlerTLID
//         input = [ "user", ORT.DStr "test" ]
//         trace_id = System.Guid.NewGuid() }

//     do!
//       postApiTest
//         "trigger_handler"
//         (serialize body)
//         (deserialize<Execution.Handler.T>)
//         ident
//         client
//         canvasName
//   }



// let testWorkerStats (client : C) (canvasName : CanvasName.T) : Task<unit> =
//   task {
//     let! canvas = Canvas.getMetaExn canvasName
//     let! canvasWithJustWorkers = Canvas.loadAllWorkers canvas

//     do!
//       canvasWithJustWorkers.handlers
//       |> Map.values
//       |> List.filterMap (fun h ->
//         match h.spec with
//         | PT.Handler.Worker _ -> Some h.tlid
//         | PT.Handler.OldWorker _ -> Some h.tlid
//         | _ -> None)
//       |> Task.iterInParallel (fun tlid ->
//         postApiTest
//           "get_worker_stats"
//           (serialize ({ tlid = tlid } : Workers.WorkerStats.Params))
//           (deserialize<Workers.WorkerStats.T>)
//           ident
//           client
//           canvasName)
//   }

// let testInsertDeleteSecrets (client : C) (canvasName : CanvasName.T) =
//   task {
//     let secretName = $"MY_SPECIAL_SECRET_{randomString 5}"
//     let secretVal = randomString 32

//     let insertParam : Secrets.Insert.Params =
//       { secret_name = secretName; secret_value = secretVal }

//     let deleteParam : Secrets.Delete.Params = { secret_name = secretName }

//     let getSecret () : Task<Option<string>> =
//       task {
//         let! (initialLoad : InitialLoad.T) = getInitialLoad client canvasName

//         return
//           initialLoad.secrets
//           |> List.filter (fun (s : InitialLoad.ApiSecret) ->
//             s.secret_name = secretName)
//           |> List.map (fun (s : InitialLoad.ApiSecret) -> s.secret_value)
//           |> List.head
//       }

//     let deleteSecret () : Task<unit> =
//       task {
//         let! (_, status, _) =
//           postApiTestCase
//             client
//             canvasName
//             FSharp
//             "delete_secret"
//             (serialize deleteParam)
//             (deserialize<Secrets.Delete.T>)
//             ident

//         Expect.equal status System.Net.HttpStatusCode.OK "delete secret"

//         let! secret = getSecret ()
//         Expect.equal secret None "initial"
//       }

//     let insertSecret server : ApiResponse<Secrets.Insert.T> =
//       task {
//         let! result =
//           postApiTestCase
//             client
//             canvasName
//             server
//             "insert_secret"
//             (serialize insertParam)
//             (deserialize<Secrets.Insert.T>)
//             ident

//         // assert secret added
//         let! secret = getSecret ()
//         Expect.equal secret (Some secretVal) $"added by {server}"
//         return result
//       }

//     // assert secret initially missing
//     let! secret = getSecret ()
//     Expect.equal secret None "initial"

//     let! oResponse = insertSecret OCaml
//     do! deleteSecret ()

//     let! fResponse = insertSecret FSharp
//     do! deleteSecret ()

//     Expect.equal fResponse oResponse "compare responses"
//   }

// let canonicalize404s (fofs : List<TI.F404>) : List<TI.F404> =
//   fofs
//   |> List.map (fun ((space, name, modifier, datetime, traceID) : TI.F404) ->
//     (space, name, modifier, datetime.truncate (), traceID))

// let testDelete404s (client : C) (canvasName : CanvasName.T) =
//   task {

//     let get404s () : Task<List<TI.F404>> =
//       task {
//         let! (o : HttpResponseMessage) =
//           postAsync OCaml client $"/api/{canvasName}/get_404s" ""

//         Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
//         let! body = o.Content.ReadAsStringAsync()
//         return (deserialize<F404s.List.T> body).f404s |> canonicalize404s
//       }

//     let path = $"/some-missing-handler-{randomString 5}"

//     let deleteParam : F404s.Delete.Params =
//       { space = "HTTP"; path = path; modifier = "GET" }

//     let get404 () : Task<Option<TI.F404>> =
//       task {
//         let! (f404s : List<TI.F404>) = get404s ()

//         return
//           f404s
//           |> List.filter (fun ((space, name, modifier, _, _) : TI.F404) ->
//             space = "HTTP" && name = path && modifier = "GET")
//           |> List.head
//       }

//     let delete404 (server : Server) : ApiResponse<F404s.Delete.T> =
//       postApiTestCase
//         client
//         canvasName
//         server
//         "delete_404"
//         (serialize deleteParam)
//         (deserialize<F404s.Delete.T>)
//         ident

//     let insert404 server : Task<unit> =
//       task {
//         let! client = Lazy.force client
//         let port =
//           match server with
//           | OCaml -> 8000
//           | FSharp -> LibService.Config.bwdServerPort
//         let url = $"http://{canvasName}.builtwithdark.localhost:{port}{path}"
//         use message = new HttpRequestMessage(HttpMethod.Get, url)
//         let! result = client.http.SendAsync(message)
//         Expect.equal result.StatusCode System.Net.HttpStatusCode.NotFound "404s"

//         // assert 404 added
//         match! get404 () with
//         | Some (space, thisPath, modifier, date, traceID) ->
//           Expect.equal space "HTTP" "inserted space correctly"
//           Expect.equal thisPath path "inserted path correctly"
//           Expect.equal modifier "GET" "inserted modifier correctly"
//         | v -> Exception.raiseInternal "Unexpected value" [ "value", v ]
//       }

//     // assert secret initially missing
//     let! f404 = get404 ()
//     Expect.equal f404 None "initial"

//     do! insert404 OCaml
//     let! oResponse = delete404 OCaml

//     do! insert404 FSharp
//     let! fResponse = delete404 FSharp

//     Expect.equal fResponse oResponse "compare responses"
//   }


// let canonicalizeAst (e : OT.RuntimeT.fluidExpr) =
//   LibExecution.OCamlTypesAst.preTraversal
//     (fun expr ->
//       match expr with
//       // This is a random number so make it zero so they can be compared
//       | LibExecution.OCamlTypes.RuntimeT.EPipeTarget _ as p ->
//         LibExecution.OCamlTypes.RuntimeT.EPipeTarget 0UL
//       // This is inconsistent in stored code
//       | LibExecution.OCamlTypes.RuntimeT.EFnCall (id, name, args, ster) ->
//         LibExecution.OCamlTypes.RuntimeT.EFnCall(
//           id,
//           name |> String.replace "_v0" "",
//           args,
//           ster
//         )
//       | LibExecution.OCamlTypes.RuntimeT.EInteger (id, i) ->
//         // CLEANUP some values have '+' in them
//         let i = if i.Length > 0 && i[0] = '+' then i.Substring(1) else i
//         LibExecution.OCamlTypes.RuntimeT.EInteger(id, i)
//       | other -> other)
//     e


// let testHsts (client : C) (canvasName : CanvasName.T) =
//   let testHstsHeader (response : HttpResponseMessage) =
//     let hstsHeader =
//       response.Headers
//       |> Seq.toList
//       |> List.choose (fun (KeyValue (x, y)) ->
//         if x.ToLower() = "strict-transport-security" then
//           Some(String.concat "," y)
//         else
//           None)

//     Expect.equal
//       hstsHeader
//       [ "max-age=31536000; includeSubDomains; preload" ]
//       "Strict-Transport-Security header either missing or incorrect"

//   task {
//     let server = FSharp

//     let! responses =
//       [ getAsync server client $"/api/{canvasName}/login"
//         getAsync server client $"/api/{canvasName}/logout"
//         getAsync server client $"/api/check-apiserver"

//         getAsync server client $"/api/{canvasName}"
//         postAsync server client $"/api/{canvasName}/initial_load" ""
//         postAsync server client $"/api/{canvasName}/packages" ""
//         postAsync server client $"/api/{canvasName}/get_worker_stats" ""
//         postAsync server client $"/api/{canvasName}/get_404s" ""
//         postAsync server client $"/api/{canvasName}/all_traces" ""
//         postAsync server client $"/api/{canvasName}/get_db_stats" ""
//         postAsync server client $"/api/{canvasName}/get_trace_data" ""
//         postAsync server client $"/api/{canvasName}/get_unlocked_dbs" ""

//         getAsync server client $"/completely-fake-url"
//         postAsync server client $"/completely-fake-url" "" ]
//       |> Task.WhenAll

//     responses |> Array.iter testHstsHeader
//   }



// let testInitialLoadReturnsTheSame (client : C) (canvasName : CanvasName.T) =
//   let deserialize v = Json.OCamlCompatible.deserialize<InitialLoad.T> v

//   let canonicalize (v : InitialLoad.T) : InitialLoad.T =
//     let canonicalizeToplevel (tl : ORT.toplevel) =
//       match tl.data with
//       // We dont have migrations anymore
//       | ORT.DB db ->
//         { tl with
//             data = ORT.DB { db with old_migrations = []; active_migration = None } }
//       | ORT.Handler h ->
//         { tl with
//             data =
//               ORT.Handler
//                 { h with
//                     ast = canonicalizeAst h.ast
//                     spec =
//                       { h.spec with
//                           modifier =
//                             // Found some workers, repls, etc, with blank modifiers,
//                             // or non-blank modifiers left over from a long time ago
//                             match h.spec.``module``, h.spec.modifier with
//                             | OT.Filled (_, "NOTIFY"), OT.Filled (id, _) ->
//                               OT.Filled(id, "_")
//                             | OT.Filled (_, _), OT.Blank id -> OT.Filled(id, "_")
//                             | _, other -> other
//                           name =
//                             // Both forms exist, probably not a big deal
//                             match h.spec.name with
//                             | OT.Filled (id, "") -> OT.Blank id
//                             | other -> other
//                           // We don't have these anymore
//                           types = { input = OT.Blank 0UL; output = OT.Blank 0UL } } } }
//     let canonicalizeUserFn (uf : ORT.user_fn) =
//       { uf with
//           ast = canonicalizeAst uf.ast
//           metadata =
//             { uf.metadata with
//                 parameters =
//                   List.map
//                     (fun p ->
//                       { p with
//                           // Unclear how anyone got this set to optional
//                           // CLEANUP remove optional
//                           optional = false
//                           // Not supposed to be possible
//                           tipe =
//                             match p.tipe with
//                             // CLEANUP can't believe this is still here
//                             | OT.Filled (id, OT.TDeprecated4 _) ->
//                               OT.Filled(id, OT.TAny)
//                             | other -> other
//                           name =
//                             match p.name with
//                             | OT.Filled (id, "") -> OT.Blank id
//                             | other -> other })
//                     uf.metadata.parameters } }

//     { v with
//         toplevels =
//           v.toplevels
//           |> List.sortBy (fun tl -> tl.tlid)
//           |> List.map canonicalizeToplevel
//         deleted_toplevels = []
//         // sometimes deleted_toplevels are missed from cached, and so don't get loaded by initial_load.
//         // Not the end of the world tbh
//         // v.deleted_toplevels
//         // |> List.sortBy (fun tl -> tl.tlid)
//         // |> List.map canonicalizeToplevel
//         user_functions =
//           v.user_functions
//           |> List.sortBy (fun uf -> uf.tlid)
//           |> List.map canonicalizeUserFn
//         deleted_user_functions =
//           v.deleted_user_functions
//           |> List.sortBy (fun uf -> uf.tlid)
//           |> List.map canonicalizeUserFn
//         user_tipes = v.user_tipes |> List.sortBy (fun ut -> ut.tlid)
//         deleted_user_tipes = v.deleted_user_tipes |> List.sortBy (fun ut -> ut.tlid)
//         canvas_list = v.canvas_list |> List.sort
//         assets =
//           v.assets
//           |> List.map (fun a -> { a with last_update = a.last_update.truncate () })
//         creation_date = v.creation_date.truncate () }

//   postApiTest "initial_load" "" deserialize canonicalize client canvasName

// let testAllTraces =
//   // In a number of cases, these tests fail because the old backend didn't like a
//   // handler and substituted a default trace, but the new backend found a trace. The
//   // new behaviour seems better.
//   postApiTest "all_traces" "" (deserialize<Traces.AllTraces.T>) ident

// let testGetUnlockedDBs =
//   postApiTest "get_unlocked_dbs" "" (deserialize<DBs.Unlocked.T>) ident

// let testPackages =
//   let canonicalizePackages (ps : Packages.List.T) : Packages.List.T =
//     ps |> List.map (fun p -> { p with body = canonicalizeAst p.body })
//   postApiTest "packages" "" (deserialize<Packages.List.T>) canonicalizePackages

// let test404s =
//   postApiTest "get_404s" "" deserialize<F404s.List.T> (fun x ->
//     { x with f404s = canonicalize404s x.f404s })

// let localOnlyTests =

//   let tests =
//     if System.Environment.GetEnvironmentVariable "CI" = null then
//       let c = lazy (login "test" "fVm2CUePzGKCwoEQQdNJktUQ")
//       let cn = CanvasName.createExn "test"
//       // This test is hard to run in CI without moving a lot of things around.
//       // It calls the ocaml webserver which is not running in that job, and not
//       // compiled/available to be run either.
//       [ "ui", testUiReturnsTheSame
//         "initial load", testInitialLoadReturnsTheSame
//         // TODO add_ops
//         "all traces", testAllTraces
//         "execute_function", testExecuteFunction
//         "get 404s", test404s
//         "db stats", testDBStats
//         "get trace data", testGetTraceData
//         "get unlocked_dbs", testGetUnlockedDBs
//         "worker_stats", testWorkerStats
//         "secrets", testInsertDeleteSecrets
//         "packages", testPackages
//         "trigger handler", testTriggerHandler
//         "delete 404s", testDelete404s
//         "hsts", testHsts
//         // TODO upload_package
//         // worker_schedule tested by hand
//         ]
//       |> List.map (fun (name, fn) -> testTask name { do! fn c cn })
//     else
//       []

//   testSequencedGroup "local" (testList "local" tests)

let testClient = lazy (login "test" "fVm2CUePzGKCwoEQQdNJktUQ")
let testAdminClient = lazy (login "test_admin" "fVm2CUePzGKCwoEQQdNJktUQ")

let loggedOutClient () =
  lazy
    (let handler = new HttpClientHandler(AllowAutoRedirect = false)
     let client = new HttpClient(handler)
     client.Timeout <- System.TimeSpan.FromSeconds 1
     let user = { http = client; csrf = "" }
     Task.FromResult user)




let permissions =
  testMany2Task
    "check apiserver permissions"
    (fun (testClient : C) (username : string) ->
      task {
        let! (uiResp : HttpResponseMessage) =
          getAsync FSharp testClient $"/a/{username}"

        let! (apiResp : HttpResponseMessage) =
          postAsync FSharp testClient $"/api/{username}/initial_load" ""

        return (int uiResp.StatusCode, int apiResp.StatusCode)
      })
    // test user can access their canvases (and sample)
    [ (testClient, "test", (200, 200))
      (testClient, "test-something", (200, 200))
      (testClient, "sample", (200, 200))
      (testClient, "sample-something", (200, 200))
      (testClient, "test_admin", (401, 401))
      (testClient, "test_admin-something", (401, 401))
      // admin user can access everything
      (testAdminClient, "test", (200, 200))
      (testAdminClient, "test-something", (200, 200))
      (testAdminClient, "test_admin", (200, 200))
      (testAdminClient, "test_admin-something", (200, 200))
      (testAdminClient, "sample", (200, 200))
      (testAdminClient, "sample-something", (200, 200))
      // logged out user can access nothing
      (loggedOutClient (), "test", (302, 401))
      (loggedOutClient (), "test-something", (302, 401))
      (loggedOutClient (), "test_admin", (302, 401))
      (loggedOutClient (), "test_admin-something", (302, 401))
      (loggedOutClient (), "sample", (302, 401))
      (loggedOutClient (), "sample-something", (302, 401)) ]


let cookies =
  let pw = "fVm2CUePzGKCwoEQQdNJktUQ"
  let local = "darklang.localhost"
  let prod = "darklang.com"

  testMany2Task
    "Check login gives the right cookies"
    (fun (host : string) (creds : Option<string * string>) ->
      task {
        let handler = new HttpClientHandler(AllowAutoRedirect = false)
        let client = new HttpClient(handler)
        client.Timeout <- System.TimeSpan.FromSeconds 1

        use req =
          new HttpRequestMessage(
            HttpMethod.Post,
            $"http://darklang.localhost:{TestConfig.apiServerNginxPort}/login"
          )

        req.Headers.Host <- host

        match creds with
        | Some (username, password) ->
          let body =
            [ ("username", username); ("password", password) ]
            |> List.map Tuple2.toKeyValuePair

          req.Content <- new FormUrlEncodedContent(body)
        | None -> ()

        let! resp = client.SendAsync(req)

        let getHeader name =
          let mutable c = seq []

          match resp.Headers.TryGetValues(name, &c) with
          | true -> c |> String.concat "," |> Some
          | false -> None

        let cookie =
          getHeader "set-cookie"
          |> Option.andThen (fun c ->
            match String.split ";" c with
            | h :: rest when String.startsWith "__session" h ->
              rest |> String.concat ";" |> String.trim |> Some
            | split -> None)

        let location = getHeader "location"

        return (int resp.StatusCode, cookie, location)
      })
    [ (local,
       Some("test", pw),
       (302,
        Some "max-age=604800; domain=darklang.localhost; path=/; httponly",
        Some "/a/test"))
      (local,
       Some("test", ""),
       (302, None, Some "/login?error=Invalid+username+or+password"))
      (local,
       Some("", pw),
       (302, None, Some "/login?error=Invalid+username+or+password"))
      (local, None, (302, None, Some "/login?error=Invalid+username+or+password"))
      (prod,
       Some("test", pw),
       (302,
        // Prod would also have the secure header, but that's a config var so we don't have that here
        Some "max-age=604800; domain=darklang.com; path=/; httponly",
        Some "/a/test"))
      (prod,
       Some("test", ""),
       (302, None, Some "/login?error=Invalid+username+or+password"))
      (prod,
       Some("", pw),
       (302, None, Some "/login?error=Invalid+username+or+password"))
      (local, None, (302, None, Some "/login?error=Invalid+username+or+password")) ]

let tests = testList "ApiServer" [ permissions; cookies ]

open Microsoft.Extensions.Hosting

let init (token : System.Threading.CancellationToken) : Task =
  // run our own webserver instead of relying on the dev webserver
  let port = TestConfig.apiServerBackendPort
  let k8sPort = TestConfig.apiServerKubernetesPort
  let packages = LibBackend.PackageManager.allFunctions().Result
  let logger = configureLogging "test-apiserver"
  (ApiServer.webserver packages logger port k8sPort).RunAsync(token)
