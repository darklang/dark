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


let testFunctionsReturnsTheSame =
  testTask "functions returns the same" {

    let! (o : HttpResponseMessage) = getAsync "http://darklang.localhost:8000/a/test"
    let! (f : HttpResponseMessage) = getAsync "http://darklang.localhost:9000/a/test"

    Expect.equal o.StatusCode f.StatusCode ""

    let! oc = o.Content.ReadAsStringAsync()
    let! fc = f.Content.ReadAsStringAsync()

    let parse (s : string) : string * List<Api.FunctionMetadata> =
      match s with
      | RegexAny "(.*const complete = )(\[.*\])(;\n.*)" [ before; fns; after ] ->
          let text = $"{before}{after}"

          let fns =
            fns
            |> FsRegEx.replace "\\s+" " " // ignore differences in string spacing in docstrings
            |> Json.Vanilla.deserialize<List<Api.FunctionMetadata>>

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
             not (
               Set.contains
                 (fn.name.ToString())
                 (ApiServer.Api.fsharpOnlyFns.Force())
             ))
      |> List.map (fun fn -> RT.FQFnName.Stdlib fn.name)
      |> Set

    let mutable notImplementedCount = 0

    let filtered (myFns : List<Api.FunctionMetadata>) : List<Api.FunctionMetadata> =
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
      (fun (ffn : Api.FunctionMetadata) ofn -> Expect.equal ffn ofn ffn.name)
      fcfns
      filteredOCamlFns
  }

let deserialize<'a> (str : string) : 'a = Json.OCamlCompatible.deserialize<'a> str

let serialize = Json.OCamlCompatible.serialize

let noBody () = ""

let postApiTestCases
  (api : string)
  (body : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  : Task<unit> =
  task {
    let! (o : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:8000/api/test/{api}" body

    let! (f : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:9000/api/test/{api}" body

    let! oc = o.Content.ReadAsStringAsync()
    let! fc = f.Content.ReadAsStringAsync()

    let () =
      if (o.StatusCode <> f.StatusCode) then
        printfn
          "%s"
          ($"Non-matching status codes: {api}\n\nbody:\n{body}\n\n"
           + $"ocaml:\n{oc}\n\nfsharp:\n{fc}")

    Expect.equal f.StatusCode o.StatusCode ""

    let oVal =
      try
        oc |> deserialize |> canonicalizeBody
      with e ->
        printfn $"Error deserializing OCaml: \n{oc}"
        reraise ()

    let fVal =
      try
        fc |> deserialize |> canonicalizeBody
      with e ->
        printfn $"Error deserializing F#: \n{fc}"
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

    Expect.equal fVal oVal "content"
    Expect.equal (headerMap f.Headers) (headerMap o.Headers) "headers"
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
      |> deserialize<Api.Traces.AllTraces>
      |> fun ts -> ts.traces
      |> List.take 5 // lets not get carried away
      |> List.map
           (fun (tlid, traceID) ->
             task {
               do!
                 let (ps : Api.Traces.Params) = { tlid = tlid; trace_id = traceID }

                 postApiTestCases
                   "get_trace_data"
                   (serialize ps)
                   (deserialize<Api.Traces.T>)
                   ident
             })

      |> Task.flatten
  }

let testDBStats =
  testTask "db_stats is the same" {
    let! (o : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:8000/api/test/initial_load" ""

    Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
    let! body = o.Content.ReadAsStringAsync()

    let dbs =
      body
      |> deserialize<Api.InitialLoad.T>
      |> fun ts -> ts.toplevels |> Convert.ocamlToplevel2PT
      |> Tuple2.second
      |> List.map (fun db -> db.tlid)
      |> fun tlids -> ({ tlids = tlids } : Api.DB.Stats.Params)

    return!
      postApiTestCases
        "get_db_stats"
        (serialize dbs)
        (deserialize<Api.DB.Stats.T>)
        ident
  }

let testWorkerStats =
  testTask "worker_stats is the same" {
    let! (o : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:8000/api/test/initial_load" ""

    Expect.equal o.StatusCode System.Net.HttpStatusCode.OK ""
    let! body = o.Content.ReadAsStringAsync()

    do!
      body
      |> deserialize<Api.InitialLoad.T>
      |> fun ts -> ts.toplevels |> Convert.ocamlToplevel2PT
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
               (serialize ({ tlid = tlid } : Api.Worker.Params))
               (deserialize<Api.Worker.T>)
               ident)
      |> Task.flatten
  }




let testInitialLoadReturnsTheSame =
  let deserialize v = Json.OCamlCompatible.deserialize<Api.InitialLoad.T> v

  let canonicalizeDate (d : System.DateTime) : System.DateTime =
    d.AddTicks(-d.Ticks % System.TimeSpan.TicksPerSecond)

  let canonicalize (v : Api.InitialLoad.T) : Api.InitialLoad.T =
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
        testPostApi "packages" "" (deserialize<Api.Packages.T>) ident
        testPostApi "get_404s" "" (deserialize<Api.F404.T>) ident
        testPostApi "get_unlocked_dbs" "" (deserialize<Api.DB.Unlocked.T>) ident
        testDBStats
        testWorkerStats
        // testPostApi "worker_schedule" "" (deserialize<Api.DB.T>) ident
        testPostApi "all_traces" "" (deserialize<Api.Traces.AllTraces>) ident
        testGetTraceData
        testInitialLoadReturnsTheSame ]
    else
      []

  testList "local" tests


let tests = testList "ApiServer" [ localOnlyTests ]
