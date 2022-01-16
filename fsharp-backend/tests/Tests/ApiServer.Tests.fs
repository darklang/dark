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

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibExecution.OCamlTypes
module ORT = OT.RuntimeT
module Convert = LibExecution.OCamlTypes.Convert
module TI = LibBackend.TraceInputs

type AuthData = LibBackend.Session.AuthData

open ApiServer

type Server =
  | FSharp
  | OCaml


let canonicalizeDate (d : System.DateTime) : System.DateTime =
  d.AddTicks(-d.Ticks % System.TimeSpan.TicksPerSecond)

let ident = Fun.identity

type User = { client : HttpClient; csrf : string }

type U = Lazy<Task<User>>

let portFor (server : Server) : int =
  match server with
  | OCaml -> 8000 // nginx for the ocaml server is on port 8000
  | FSharp -> LibService.Config.apiServerNginxPort

// login as test user and return the csrfToken (the cookies are stored in httpclient)
let login (username : string) (password : string) : Task<User> =
  task {
    let client = new HttpClient()
    client.Timeout <- System.TimeSpan.FromSeconds 1
    let port = portFor OCaml

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

    let csrfToken =
      match loginContent with
      | Regex "const csrfToken = \"(.*?)\";" [ token ] -> token
      | _ ->
        Exception.raiseInternal
          $"could not find csrfToken"
          [ "loginContent", loginContent ]

    return { client = client; csrf = csrfToken }
  }

let testUser = lazy (login "test" "fVm2CUePzGKCwoEQQdNJktUQ")
let testAdminUser = lazy (login "test_admin" "fVm2CUePzGKCwoEQQdNJktUQ")

let loggedOutUser () =
  lazy
    (let handler = new HttpClientHandler(AllowAutoRedirect = false)
     let client = new HttpClient(handler)
     client.Timeout <- System.TimeSpan.FromSeconds 1
     let user = { client = client; csrf = "" }
     Task.FromResult user)



let getAsync
  (server : Server)
  (user : U)
  (path : string)
  : Task<HttpResponseMessage> =
  task {
    let! user = Lazy.force user
    let port = portFor server
    let url = $"http://darklang.localhost:{port}{path}"
    use message = new HttpRequestMessage(HttpMethod.Get, url)
    message.Headers.Add("X-CSRF-Token", user.csrf)

    return! user.client.SendAsync(message)
  }

let postAsync
  (server : Server)
  (user : U)
  (path : string)
  (body : string)
  : Task<HttpResponseMessage> =
  task {
    let! user = Lazy.force user
    let port = portFor server
    let url = $"http://darklang.localhost:{port}{path}"
    use message = new HttpRequestMessage(HttpMethod.Post, url)
    message.Headers.Add("X-CSRF-Token", user.csrf)

    message.Content <-
      new StringContent(body, System.Text.Encoding.UTF8, "application/json")

    return! user.client.SendAsync(message)
  }

let deserialize<'a> (str : string) : 'a = Json.OCamlCompatible.deserialize<'a> str

let serialize = Json.OCamlCompatible.serialize

let noBody () = ""



let getInitialLoad (user : U) : Task<InitialLoad.T> =
  task {
    let! (o : HttpResponseMessage) =
      postAsync OCaml user $"/api/test/initial_load" ""

    Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
    let! body = o.Content.ReadAsStringAsync()
    return deserialize<InitialLoad.T> body
  }



let testUiReturnsTheSame =
  testTask "ui returns the same" {

    let! (o : HttpResponseMessage) = getAsync OCaml testUser "/a/test"
    let! (f : HttpResponseMessage) = getAsync FSharp testUser "/a/test"

    Expect.equal o.StatusCode f.StatusCode ""

    let! oc = o.Content.ReadAsStringAsync()
    let! fc = f.Content.ReadAsStringAsync()

    let parse (s : string) : string * List<Functions.FunctionMetadata> =
      match s with
      | RegexAny "(.*const complete = )(\[.*\])(;\n.*)" [ before; fns; after ] ->
        let text = $"{before}{after}"

        let fns =
          fns
          |> FsRegEx.replace "\\s+" " " // ignore differences in string spacing in docstrings
          |> Json.Vanilla.deserialize<List<Functions.FunctionMetadata>>

        (text, fns)
      | _ -> Exception.raiseInternal "doesn't match" [ "string", s ]

    let oc, ocfns = parse oc
    let fc, fcfns = parse fc

    let port = portFor OCaml

    // There have been some tiny changes, let's work around them
    let ocfns =
      List.map
        (fun (fn : Functions.FunctionMetadata) ->
          match string fn.name with
          | "DB::queryOneWithKey_v2" ->
            { fn with
                description =
                  fn.description + ". Previously called DB::queryOnewithKey_v2" }
          | "DB::queryWithKey_v2" ->
            { fn with
                description =
                  fn.description + ". Previous called DB::queryWithKey_v2" }
          | "DB::query_v3" ->
            { fn with
                description = fn.description + ". Previously called DB::query_v3" }
          | "DB::query_v2" ->
            { fn with
                description = fn.description + ". Previously called DB::query_v3" }
          | "DB::queryOne_v2" ->
            { fn with
                description = fn.description + ". Previously called DB::queryOne_v2" }
          | _ -> fn)
        ocfns

    let oc =
      oc
        // a couple of specific ones
        .Replace(
          $"static.darklang.localhost:{port}",
          $"static.darklang.localhost:{LibService.Config.apiServerNginxPort}"
        )
        .Replace(
          $"builtwithdark.localhost:{port}",
          $"builtwithdark.localhost:{LibService.Config.bwdServerNginxPort}"
        )
        // get the rest
        .Replace(
          $"localhost:{port}",
          $"localhost:{LibService.Config.apiServerNginxPort}"
        )

    Expect.equal fc oc ""

    let builtins =
      Functions.allFunctions
      |> List.filter (fun fn ->
        Functions.fsharpOnlyFns |> Lazy.force |> Set.contains (string fn.name) |> not)
      |> List.map (fun fn -> RT.FQFnName.Stdlib fn.name)
      |> Set

    let mutable notImplementedCount = 0

    let filtered
      (myFns : List<Functions.FunctionMetadata>)
      : List<Functions.FunctionMetadata> =
      List.filter
        (fun fn ->
          if Set.contains (PT.FQFnName.parse fn.name) builtins then
            true
          else
            print $"Not yet implemented: {fn.name}"
            notImplementedCount <- notImplementedCount + 1
            false)
        myFns

    // FSTODO: Here we test that the metadata for all the APIs is the same.
    // Since we don't yet support all the tests, we just filter to the ones we
    // do support for now. Before shipping, we obviously need to support them
    // all.
    let filteredOCamlFns = filtered ocfns

    print $"Implemented fns  : {List.length Functions.allFunctions}"
    print $"Excluding F#-only: {Set.length builtins}"
    print $"Fns in OCaml api : {List.length ocfns}"
    print $"Fns in F# api    : {List.length fcfns}"
    print $"Missing fns      : {notImplementedCount}"

    List.iter2
      (fun (ffn : Functions.FunctionMetadata) ofn -> Expect.equal ffn ofn ffn.name)
      fcfns
      filteredOCamlFns
  }

type ApiResponse<'a> = Task<'a * System.Net.HttpStatusCode * Map<string, string>>

let postApiTestCase
  (server : Server)
  (api : string)
  (body : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  : ApiResponse<'a> =
  task {
    let! (response : HttpResponseMessage) =
      postAsync server testUser $"/api/test/{api}" body

    let! content = response.Content.ReadAsStringAsync()

    let (body : 'a) =
      try
        content |> deserialize |> canonicalizeBody
      with
      | e ->
        print $"Error deserializing {server}: \n{content}"
        reraise ()

    let headerMap (h : Headers.HttpResponseHeaders) : Map<string, string> =
      let clear str =
        if h.Contains str then
          (h.Remove str |> ignore<bool>
           h.TryAddWithoutValidation(str, "XXX") |> ignore<bool>)

      clear "Date"
      clear "Server"
      clear "Server-Timing"
      clear "x-darklang-execution-id"
      let (_ : bool) = h.Remove "Connection" // not useful, not in new API
      let (_ : bool) = h.Remove "Strict-Transport-Security" // only in new API

      h
      |> Seq.toList
      |> List.map (fun (KeyValue (x, y)) -> x, String.concat "," y)
      |> Map

    let headers = headerMap response.Headers
    return (body, response.StatusCode, headers)

  }

let postApiTestCases
  (api : string)
  (body : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  : Task<unit> =
  task {
    let! (oContent, oStatus, oHeaders) as o =
      postApiTestCase OCaml api body deserialize canonicalizeBody

    let! (fContent, fStatus, fHeaders) as f =
      postApiTestCase FSharp api body deserialize canonicalizeBody

    let () =
      if oStatus <> fStatus then
        print (
          $"Non-matching status codes: {api}\n\nbody:\n{body}\n\n"
          + $"ocaml:\n{o}\n\nfsharp:\n{f}"
        )

    Expect.equal fStatus oStatus "status"
    Expect.equal fContent oContent "content"
    Expect.equal fHeaders oHeaders "headers"
  }

let testPostApi
  (api : string)
  (body : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  : Test =
  testTask $"{api} API returns same" {
    return! postApiTestCases api body deserialize canonicalizeBody
  }


let testGetTraceData =
  testTask "get_trace is the same" {
    let! (o : HttpResponseMessage) =
      postAsync OCaml testUser $"/api/test/all_traces" ""

    Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
    let! body = o.Content.ReadAsStringAsync()

    let canonicalize (t : Traces.TraceData.T) : Traces.TraceData.T =
      t
      |> Option.map (fun t ->
        { t with
            trace =
              t.trace
              |> Tuple2.mapSecond (fun td ->
                { td with timestamp = canonicalizeDate td.timestamp }) })

    do!
      body
      |> deserialize<Traces.AllTraces.T>
      |> fun ts -> ts.traces
      |> List.take 5 // lets not get carried away
      |> List.map (fun (tlid, traceID) ->
        task {
          let (ps : Traces.TraceData.Params) = { tlid = tlid; trace_id = traceID }
          do!
            postApiTestCases
              "get_trace_data"
              (serialize ps)
              (deserialize<Traces.TraceData.T>)
              canonicalize
        })

      |> Task.flatten
  }

let testDBStats =
  testTask "db_stats is the same" {
    let! (initialLoad : InitialLoad.T) = getInitialLoad testUser

    let dbs =
      initialLoad.toplevels
      |> Convert.ocamlToplevel2PT
      |> Tuple2.second
      |> List.map (fun db -> db.tlid)
      |> fun tlids -> ({ tlids = tlids } : DBs.DBStats.Params)

    return!
      postApiTestCases
        "get_db_stats"
        (serialize dbs)
        (deserialize<DBs.DBStats.T>)
        ident
  }

let testExecuteFunction =
  testTask "execute_function behaves the same" {
    let tlid = gid ()

    let (body : Execution.Function.Params) =
      { tlid = tlid
        trace_id = System.Guid.NewGuid()
        caller_id = gid ()
        args = [ ORT.DInt 5L; ORT.DInt 6L ]
        fnname = "Int::add" }

    return!
      postApiTestCases
        "execute_function"
        (serialize body)
        (deserialize<Execution.Function.T>)
        // New version includes the tlid of the caller
        (fun (r : Execution.Function.T) ->
          { r with touched_tlids = List.filter ((<>) tlid) r.touched_tlids })
  }

let testTriggerHandler =
  testTask "trigger_handler behaves the same" {
    let! (initialLoad : InitialLoad.T) = getInitialLoad testUser

    let handlerTLID =
      initialLoad.toplevels
      |> Convert.ocamlToplevel2PT
      |> Tuple2.first
      |> List.filterMap (fun h ->
        // This test won't succeed unless you manually go to
        // http://darklang.localhost:9000/a/test and add a POST http handler with the
        // path below
        match h.spec with
        | PT.Handler.HTTP ("/a-test-handler/:user", "POST", _) -> Some h.tlid
        | _ -> None)
      |> List.head
      |> Exception.unwrapOptionInternal "" []

    let (body : Execution.Handler.Params) =
      { tlid = handlerTLID
        input = [ "user", ORT.DStr "test" ]
        trace_id = System.Guid.NewGuid() }

    return!
      postApiTestCases
        "trigger_handler"
        (serialize body)
        (deserialize<Execution.Handler.T>)
        ident
  }



let testWorkerStats =
  testTask "worker_stats is the same" {
    let! (initialLoad : InitialLoad.T) = getInitialLoad testUser

    do!
      initialLoad.toplevels
      |> Convert.ocamlToplevel2PT
      |> Tuple2.first
      |> List.filterMap (fun h ->
        match h.spec with
        | PT.Handler.Worker _ -> Some h.tlid
        | _ -> None)
      |> List.map (fun tlid ->
        postApiTestCases
          "get_worker_stats"
          (serialize ({ tlid = tlid } : Workers.WorkerStats.Params))
          (deserialize<Workers.WorkerStats.T>)
          ident)
      |> Task.flatten
  }

let testInsertDeleteSecrets =
  testTask "insert_secrets is the same" {
    let secretName = $"MY_SPECIAL_SECRET_{randomString 5}"
    let secretVal = randomString 32

    let insertParam : Secrets.Insert.Params =
      { secret_name = secretName; secret_value = secretVal }

    let deleteParam : Secrets.Delete.Params = { secret_name = secretName }

    let getSecret () : Task<Option<string>> =
      task {
        let! (initialLoad : InitialLoad.T) = getInitialLoad testUser

        return
          initialLoad.secrets
          |> List.filter (fun (s : InitialLoad.ApiSecret) ->
            s.secret_name = secretName)
          |> List.map (fun (s : InitialLoad.ApiSecret) -> s.secret_value)
          |> List.head
      }

    let deleteSecret () : Task<unit> =
      task {
        let! (_, status, _) =
          postApiTestCase
            FSharp
            "delete_secret"
            (serialize deleteParam)
            (deserialize<Secrets.Delete.T>)
            ident

        Expect.equal status System.Net.HttpStatusCode.OK "delete secret"

        let! secret = getSecret ()
        Expect.equal secret None "initial"
      }

    let insertSecret server : ApiResponse<Secrets.Insert.T> =
      task {
        let! result =
          postApiTestCase
            server
            "insert_secret"
            (serialize insertParam)
            (deserialize<Secrets.Insert.T>)
            ident

        // assert secret added
        let! secret = getSecret ()
        Expect.equal secret (Some secretVal) $"added by {server}"
        return result
      }

    // assert secret initially missing
    let! secret = getSecret ()
    Expect.equal secret None "initial"

    let! oResponse = insertSecret OCaml
    do! deleteSecret ()

    let! fResponse = insertSecret FSharp
    do! deleteSecret ()

    Expect.equal fResponse oResponse "compare responses"
  }

let testDelete404s =
  testTask "delete_404s is the same" {

    let get404s () : Task<List<TI.F404>> =
      task {
        let! (o : HttpResponseMessage) =
          postAsync OCaml testUser $"/api/test/get_404s" ""

        Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
        let! body = o.Content.ReadAsStringAsync()
        return (deserialize<F404s.List.T> body).f404s
      }

    let path = $"/some-missing-handler-{randomString 5}"

    let deleteParam : F404s.Delete.Params =
      { space = "HTTP"; path = path; modifier = "GET" }

    let get404 () : Task<Option<TI.F404>> =
      task {
        let! (f404s : List<TI.F404>) = get404s ()

        return
          f404s
          |> List.filter (fun ((space, name, modifier, _, _) : TI.F404) ->
            space = "HTTP" && name = path && modifier = "GET")
          |> List.head
      }

    let delete404 (server : Server) : ApiResponse<F404s.Delete.T> =
      postApiTestCase
        server
        "delete_404"
        (serialize deleteParam)
        (deserialize<F404s.Delete.T>)
        ident

    let insert404 server : Task<unit> =
      task {
        let! user = Lazy.force testUser
        let port =
          match server with
          | OCaml -> 8000
          | FSharp -> LibService.Config.bwdServerNginxPort
        let url = $"http://test.builtwithdark.localhost:{port}{path}"
        use message = new HttpRequestMessage(HttpMethod.Get, url)
        let! result = user.client.SendAsync(message)
        Expect.equal result.StatusCode System.Net.HttpStatusCode.NotFound "404s"

        // assert 404 added
        match! get404 () with
        | Some (space, thisPath, modifier, date, traceID) ->
          Expect.equal space "HTTP" "inserted space correctly"
          Expect.equal thisPath path "inserted path correctly"
          Expect.equal modifier "GET" "inserted modifier correctly"
        | v -> Exception.raiseInternal "Unexpected value" [ "value", v ]
      }

    // assert secret initially missing
    let! f404 = get404 ()
    Expect.equal f404 None "initial"

    do! insert404 OCaml
    let! oResponse = delete404 OCaml

    do! insert404 FSharp
    let! fResponse = delete404 FSharp

    Expect.equal fResponse oResponse "compare responses"
  }




let testInitialLoadReturnsTheSame =
  let deserialize v = Json.OCamlCompatible.deserialize<InitialLoad.T> v


  let canonicalize (v : InitialLoad.T) : InitialLoad.T =
    let clearTypes (tl : ORT.toplevel) =
      match tl.data with
      | ORT.DB _ -> tl
      | ORT.Handler h ->
        { tl with
            data =
              ORT.Handler
                { h with
                    spec =
                      { h.spec with
                          types = { input = OT.Blank 0UL; output = OT.Blank 0UL } } } }

    { v with
        toplevels =
          v.toplevels |> List.sortBy (fun tl -> tl.tlid) |> List.map clearTypes
        deleted_toplevels =
          v.deleted_toplevels
          |> List.sortBy (fun tl -> tl.tlid)
          |> List.map clearTypes
        canvas_list = v.canvas_list |> List.sort
        creation_date = v.creation_date |> canonicalizeDate }

  testPostApi "initial_load" "" deserialize canonicalize

let localOnlyTests =
  let tests =

    if System.Environment.GetEnvironmentVariable "CI" = null then
      // This test is hard to run in CI without moving a lot of things around.
      // It calls the ocaml webserver which is not running in that job, and not
      // compiled/available to be run either.
      [ testUiReturnsTheSame
        // FSTODO add_ops
        testPostApi "all_traces" "" (deserialize<Traces.AllTraces.T>) ident
        testDelete404s
        testExecuteFunction
        testPostApi "get_404s" "" (deserialize<F404s.List.T>) ident
        testDBStats
        testGetTraceData
        testPostApi "get_unlocked_dbs" "" (deserialize<DBs.Unlocked.T>) ident
        testWorkerStats
        testInitialLoadReturnsTheSame
        testInsertDeleteSecrets
        testPostApi "packages" "" (deserialize<Packages.List.T>) ident
        // TODO upload_package
        testTriggerHandler
        // FSTODO worker_schedule
        ]
    else
      []

  testSequencedGroup "local" (testList "local" tests)

// FSTODO: this should be on the *TEST* api server, not the dev one
let permissions =
  testMany2Task
    "check apiserver permissions"
    (fun (user : U) (username : string) ->
      task {
        let! (uiResp : HttpResponseMessage) = getAsync FSharp user $"/a/{username}"

        let! (apiResp : HttpResponseMessage) =
          postAsync FSharp user $"/api/{username}/initial_load" ""

        return (int uiResp.StatusCode, int apiResp.StatusCode)
      })
    // test user can access their canvases (and sample)
    [ (testUser, "test", (200, 200))
      (testUser, "test-something", (200, 200))
      (testUser, "sample", (200, 200))
      (testUser, "sample-something", (200, 200))
      (testUser, "test_admin", (401, 401))
      (testUser, "test_admin-something", (401, 401))
      // admin user can access everything
      (testAdminUser, "test", (200, 200))
      (testAdminUser, "test-something", (200, 200))
      (testAdminUser, "test_admin", (200, 200))
      (testAdminUser, "test_admin-something", (200, 200))
      (testAdminUser, "sample", (200, 200))
      (testAdminUser, "sample-something", (200, 200))
      // logged out user can access nothing
      (loggedOutUser (), "test", (302, 401))
      (loggedOutUser (), "test-something", (302, 401))
      (loggedOutUser (), "test_admin", (302, 401))
      (loggedOutUser (), "test_admin-something", (302, 401))
      (loggedOutUser (), "sample", (302, 401))
      (loggedOutUser (), "sample-something", (302, 401)) ]


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
            $"http://darklang.localhost:9000/login"
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


let tests = testList "ApiServer" [ localOnlyTests; permissions; cookies ]
