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

open ApiServer

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

let postAsync (url : string) : Task<HttpResponseMessage> =
  task {
    let! csrfToken = login.Force()
    use message = new HttpRequestMessage(HttpMethod.Post, url)
    message.Headers.Add("X-CSRF-Token", csrfToken)

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

let deserialize<'a> = Json.OCamlCompatible.deserialize<'a>

let testPostApi
  (api : string)
  (deserialize : string -> 'a)
  (canonicalizeBody : 'a -> 'a)
  =
  testTask $"{api} API returns same" {
    let! (o : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:8000/api/test/{api}"

    let! (f : HttpResponseMessage) =
      postAsync $"http://darklang.localhost:9000/api/test/{api}"

    Expect.equal o.StatusCode f.StatusCode ""

    let! oc = o.Content.ReadAsStringAsync()
    let! fc = f.Content.ReadAsStringAsync()
    let oVal = oc |> deserialize |> canonicalizeBody
    let fVal = fc |> deserialize |> canonicalizeBody

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

  testPostApi "intial_load" deserialize canonicalize

let localOnlyTests =
  let tests =
    if System.Environment.GetEnvironmentVariable "CI" = null then
      // This test is hard to run in CI without moving a lot of things around.
      // It calls the ocaml webserver which is not running in that job, and not
      // compiled/available to be run either.
      [ testFunctionsReturnsTheSame

        testPostApi "packages" (deserialize<Api.Packages.T>) Fun.identity
        testPostApi "get_404s" (deserialize<Api.F404.T>) Fun.identity
        testPostApi "get_trace_data" (deserialize<Api.F404.T>) Fun.identity
        // testGet404sReturnsSame
        // testAllTracesReturnsSame
        testInitialLoadReturnsTheSame
        // testPackagesReturnsSame
        ]
    else
      []

  testList "local" tests


let tests = testList "ApiServer" [ localOnlyTests ]
