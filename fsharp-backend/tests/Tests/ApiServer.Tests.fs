module Tests.ApiServer

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open System.Net.Http

type KeyValuePair<'k, 'v> = System.Collections.Generic.KeyValuePair<'k, 'v>
type AuthData = LibBackend.Session.AuthData

open Tablecloth
open Prelude
open Prelude.Tablecloth
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibBackend.OCamlInterop.OCamlTypes
module ORT = OT.RuntimeT
module Convert = LibBackend.OCamlInterop.Convert
module TI = LibBackend.TraceInputs

open ApiServer

let ident = Fun.identity
let client = new HttpClient()

// login as test user and return the csrfToken (the cookies are stored in httpclient)
let login : Lazy<Task<string>> =
  lazy
    (task {
      use loginReq =
        new HttpRequestMessage(
          HttpMethod.Post,
          $"http://darklang.localhost:8000/login"
        )

      let body =
        [ KeyValuePair<string, string>("username", "test")
          KeyValuePair<string, string>("password", "fVm2CUePzGKCwoEQQdNJktUQ") ]

      loginReq.Content <- new FormUrlEncodedContent(body)

      let! loginResp = client.SendAsync(loginReq)
      let! loginContent = loginResp.Content.ReadAsStringAsync()

      let csrfToken =
        match loginContent with
        | Regex "const csrfToken = \"(.*?)\";" [ token ] -> token
        | _ -> failwith $"could not find csrfToken in {loginContent}"

      return csrfToken
     })


let getAsync (url : string) : Task<HttpResponseMessage> =
  task {
    let! csrfToken = login.Force()
    use message = new HttpRequestMessage(HttpMethod.Get, url)
    message.Headers.Add("X-CSRF-Token", csrfToken)

    return! client.SendAsync(message)
  }

let postAsync (url : string) (body : string) : Task<HttpResponseMessage> =
  task {
    let! csrfToken = login.Force()
    use message = new HttpRequestMessage(HttpMethod.Post, url)
    message.Headers.Add("X-CSRF-Token", csrfToken)

    message.Content <-
      new StringContent(body, System.Text.Encoding.UTF8, "application/json")

    return! client.SendAsync(message)
  }

let deserialize<'a> (str : string) : 'a = Json.OCamlCompatible.deserialize<'a> str

let serialize = Json.OCamlCompatible.serialize

let noBody () = ""



let getInitialLoad () : Task<InitialLoad.T> =
  task {
    let! (o : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:8000/api/test/initial_load" ""

    Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
    let! body = o.Content.ReadAsStringAsync()
    return deserialize<InitialLoad.T> body
  }



let testFunctionsReturnsTheSame =
  testTask "functions returns the same" {

    let! (o : HttpResponseMessage) = getAsync "http://darklang.localhost:8000/a/test"
    let! (f : HttpResponseMessage) = getAsync "http://darklang.localhost:9000/a/test"

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
      | _ -> failwith "doesn't match"

    let oc, ocfns = parse oc
    let fc, fcfns = parse fc
    Expect.equal fc oc ""

    let allBuiltins = (LibExecution.StdLib.StdLib.fns @ LibBackend.StdLib.StdLib.fns)

    let builtins =
      allBuiltins
      |> List.filter
           (fun fn ->
             not (Set.contains (toString fn.name) (Functions.fsharpOnlyFns.Force())))
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
            printfn $"Not yet implemented: {fn.name}"
            notImplementedCount <- notImplementedCount + 1
            false)
        myFns

    // FSTODO: Here we test that the metadata for all the APIs is the same.
    // Since we don't yet support all the tests, we just filter to the ones we
    // do support for now. Before shipping, we obviously need to support them
    // all.
    let filteredOCamlFns = filtered ocfns

    printfn $"Implemented fns  : {List.length allBuiltins}"
    printfn $"Excluding F#-only: {Set.length builtins}"
    printfn $"Fns in OCaml api : {List.length ocfns}"
    printfn $"Fns in F# api    : {List.length fcfns}"
    printfn $"Missing fns      : {notImplementedCount}"

    List.iter2
      (fun (ffn : Functions.FunctionMetadata) ofn -> Expect.equal ffn ofn ffn.name)
      fcfns
      filteredOCamlFns
  }

type Server =
  | FSharp
  | OCaml

type ApiResponse<'a> = Task<'a * System.Net.HttpStatusCode * Map<string, string>>

let postApiTestCase
  (server : Server)
  (api : string)
  (body : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  : ApiResponse<'a> =
  task {
    let port =
      match server with
      | OCaml -> 8000
      | FSharp -> 9000

    let! (response : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:{port}/api/test/{api}" body

    let! content = response.Content.ReadAsStringAsync()

    let (body : 'a) =
      try
        content |> deserialize |> canonicalizeBody
      with e ->
        printfn $"Error deserializing {server}: \n{content}"
        reraise ()

    let headerMap (h : Headers.HttpResponseHeaders) : Map<string, string> =
      let clear str =
        if h.Contains str then
          (h.Remove str |> ignore
           h.TryAddWithoutValidation(str, "XXX") |> ignore)

      clear "Date"
      clear "Server"
      clear "ServerTiming"
      let (_ : bool) = h.Remove "x-darklang-execution-id" // not in new API
      let (_ : bool) = h.Remove "Connection" // not useful, not in new API
      h |> Seq.toList |> List.map (fun (KeyValue (x, y)) -> x, y.ToString()) |> Map

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
        printfn
          "%s"
          ($"Non-matching status codes: {api}\n\nbody:\n{body}\n\n"
           + $"ocaml:\n{o}\n\nfsharp:\n{f}")

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
    return! postApiTestCases api body deserialize canonicalizeBody }


let testGetTraceData =
  testTask "get_trace is the same" {
    let! (o : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:8000/api/test/all_traces" ""

    Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
    let! body = o.Content.ReadAsStringAsync()

    do!
      body
      |> deserialize<Traces.AllTraces.T>
      |> fun ts -> ts.traces
      |> List.take 5 // lets not get carried away
      |> List.map
           (fun (tlid, traceID) ->
             task {
               do!
                 let (ps : Traces.TraceData.Params) =
                   { tlid = tlid; trace_id = traceID }

                 postApiTestCases
                   "get_trace_data"
                   (serialize ps)
                   (deserialize<Traces.TraceData.T>)
                   ident
             })

      |> Task.flatten
  }

let testDBStats =
  testTask "db_stats is the same" {
    let! (initialLoad : InitialLoad.T) = getInitialLoad ()

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
    let (body : Execution.Function.Params) =
      { tlid = gid ()
        trace_id = System.Guid.NewGuid()
        caller_id = gid ()
        args = [ ORT.DInt 5L; ORT.DInt 6L ]
        fnname = "Int::add" }

    return!
      postApiTestCases
        "execute_function"
        (serialize body)
        (deserialize<Execution.Function.T>)
        ident
  }

let testTriggerHandler =
  testTask "trigger_handler behaves the same" {
    let! (initialLoad : InitialLoad.T) = getInitialLoad ()

    let handlerTLID =
      initialLoad.toplevels
      |> Convert.ocamlToplevel2PT
      |> Tuple2.first
      |> List.filterMap
           (fun h ->
             match h.spec with
             | PT.Handler.HTTP ("/a-test-handler/:user", "POST", _) -> Some h.tlid
             | _ -> None)
      |> List.head
      |> Option.unwrapUnsafe

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
    let! (initialLoad : InitialLoad.T) = getInitialLoad ()

    do!
      initialLoad.toplevels
      |> Convert.ocamlToplevel2PT
      |> Tuple2.first
      |> List.filterMap
           (fun h ->
             match h.spec with
             | PT.Handler.Worker _ -> Some h.tlid
             | _ -> None)
      |> List.map
           (fun tlid ->
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
        let! (initialLoad : InitialLoad.T) = getInitialLoad ()

        return
          initialLoad.secrets
          |> List.filter
               (fun (s : InitialLoad.ApiSecret) -> s.secret_name = secretName)
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
          postAsync $"http://darklang.localhost:8000/api/test/get_404s" ""

        Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
        let! body = o.Content.ReadAsStringAsync()
        debuG "404s body" body
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
          |> debug "f404s"
          |> List.filter
               (fun ((space, name, modifier, _, _) : TI.F404) ->
                 space = "HTTP" && name = path && modifier = "GET")
          |> debug "filtered"
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
        let url = $"http://test.builtwithdark.localhost:9001{path}"
        let! result = getAsync url
        Expect.equal result.StatusCode System.Net.HttpStatusCode.NotFound "404s"

        // assert 404 added
        match! get404 () with
        | Some (space, thisPath, modifier, date, traceID) ->
            Expect.equal space "HTTP" "inserted space correctly"
            Expect.equal thisPath path "inserted path correctly"
            Expect.equal modifier "GET" "inserted modifier correctly"
        | v -> failwith $"Unexpected value: {v}"
      }

    // assert secret initially missing
    let! f404 = get404 ()
    Expect.equal f404 None "initial"
    printfn "initial_check"

    do! insert404 ()
    printfn "insert"
    let! oResponse = delete404 OCaml
    printfn "ocaml delete"

    do! insert404 ()
    printfn "insert"
    let! fResponse = delete404 FSharp
    printfn "fsharp_delete"

    Expect.equal fResponse oResponse "compare responses"
  }




let testInitialLoadReturnsTheSame =
  let deserialize v = Json.OCamlCompatible.deserialize<InitialLoad.T> v

  let canonicalizeDate (d : System.DateTime) : System.DateTime =
    d.AddTicks(-d.Ticks % System.TimeSpan.TicksPerSecond)

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
      [ testFunctionsReturnsTheSame
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
        // FSTODO upload_package
        testTriggerHandler
        // FSTODO worker_schedule
        ]
    else
      []

  testList "local" tests


let tests = testList "ApiServer" [ localOnlyTests ]
