module Tests.ApiServer

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Tablecloth
open Prelude
open Prelude.Tablecloth

open LibExecution
open ApiServer

open System.Net.Http

type KeyValuePair<'k, 'v> = System.Collections.Generic.KeyValuePair<'k, 'v>
type AuthData = LibBackend.Session.AuthData

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
          ($"{before}{after}",
           Json.Vanilla.deserialize<List<Api.FunctionMetadata>> fns)
      | _ -> failwith "doesn't match"

    let oc, ocfns = parse oc
    let fc, fcfns = parse fc

    Expect.equal oc fc ""
    Expect.equal ocfns fcfns ""
  }

let tests = testList "ApiServer" [ testFunctionsReturnsTheSame ]
