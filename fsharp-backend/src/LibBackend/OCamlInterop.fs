/// <summary> Provides interop with our OCaml backend. </summary>
///
/// <remarks>
/// Programs are stored using an OCaml-only serialization format, so we have to
/// call OCaml code to fetch it and save it. We send binary code which we get
/// from the DB, convert it to OCaml types, then json convert it to get it back
/// into F#. At that point we convert it to these types, and potentially convert
/// it to the runtime types to run it.
///
/// We also use these types to convert to the types the API uses, which are
/// typically direct deserializations of these types.
/// </remarks>
module LibBackend.OCamlInterop


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlTypes = LibExecution.OCamlTypes
module Convert = LibExecution.OCamlTypes.Convert

let digest () = "0e91e490041f06fae012f850231eb6ab"


// ----------------
// Getting values from OCaml
// ----------------

// Make it lazy so that it initialized AFTER Telemetry initializes
let client =
  lazy
    (let handler = new System.Net.Http.SocketsHttpHandler()

     // Unsure whether we need this, but the code is going away so best be sure
     handler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
     handler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0
     // In .NET 6, this is fine without having to do anything about socket exhaustion or
     // DNS.
     let client = new System.Net.Http.HttpClient(handler, disposeHandler = false)
     // We prefer that this raise than hang
     client.Timeout <- System.TimeSpan.FromSeconds 5
     client)

/// Make a request to the legacy OCaml server
let legacyReq
  (endpoint : string)
  (data : byte array)
  : Task<System.Net.Http.HttpContent> =
  task {
    // CLEANUP I can't figure out HttpClient auto-instrumentation, so let's do it manually
    use _span = LibService.Telemetry.child "legacyReq" [ "endpoint", endpoint ]

    // prep request
    let host, port =
      if endpoint.StartsWith("bs") then
        (LibService.Config.legacySerializationServerHost,
         LibService.Config.legacySerializationServerPort)
      elif
        endpoint = "execute" || endpoint = "benchmark"
        || endpoint.StartsWith("fuzzing")
      then
        ("localhost", LibService.Config.legacyFuzzingServerPort)
      else
        Exception.raiseInternal "unexpected endpoint" [ "endpoint", endpoint ]
    let url = $"http://{host}:{port}/{endpoint}"

    use message =
      new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, url)

    message.Content <- new System.Net.Http.ByteArrayContent(data)

    // get response
    let! response = client.Force().SendAsync(message)
    LibService.Telemetry.addTag "statuscode" response.StatusCode

    // handle and return response
    if response.StatusCode = System.Net.HttpStatusCode.OK then
      ()
    else if response.StatusCode = System.Net.HttpStatusCode.BadRequest then
      // This is how errors are reported
      let! content = response.Content.ReadAsStringAsync()
      LibService.Telemetry.addTag "error" content
      Exception.raiseInternal content []
    else
      let! content = response.Content.ReadAsStringAsync()
      Exception.raiseInternal
        "not a 200 response"
        [ "statusCode", response.StatusCode
          "content", content
          "endpoint", endpoint ]

    return response.Content
  }

let legacyStringReq (endpoint : string) (data : byte array) : Task<string> =
  task {
    let! content = legacyReq endpoint data
    return! content.ReadAsStringAsync()
  }

let legacyBytesReq (endpoint : string) (data : byte array) : Task<byte array> =
  task {
    let! content = legacyReq endpoint data
    return! content.ReadAsByteArrayAsync()
  }

let stringToBytesReq (endpoint : string) (str : string) : Task<byte array> =
  str |> UTF8.toBytes |> legacyBytesReq endpoint

let bytesToStringReq (endpoint : string) (data : byte array) : Task<string> =
  data |> legacyStringReq endpoint

let stringToStringReq (endpoint : string) (str : string) : Task<string> =
  str |> UTF8.toBytes |> legacyStringReq endpoint

let stringToDvalReq (endpoint : string) (str : string) : Task<RT.Dval> =
  str
  |> UTF8.toBytes
  |> legacyStringReq endpoint
  |> Task.map (Json.OCamlCompatible.legacyDeserialize<OCamlTypes.RuntimeT.dval>)
  |> Task.map Convert.ocamlDval2rt

let dvalToStringReq (endpoint : string) (dv : RT.Dval) : Task<string> =
  dv
  |> Convert.rt2ocamlDval
  |> Json.OCamlCompatible.legacySerialize
  |> UTF8.toBytes
  |> legacyStringReq endpoint

let dvalListToStringReq (endpoint : string) (l : List<RT.Dval>) : Task<string> =
  l
  |> List.map Convert.rt2ocamlDval
  |> Json.OCamlCompatible.legacySerialize
  |> UTF8.toBytes
  |> legacyStringReq endpoint


// Binary deserialization functions

let oplistOfBinary (data : byte array) : Task<PT.Oplist> =
  data
  |> bytesToStringReq "bs/oplist_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.oplist<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map Convert.ocamlOplist2PT

let oplistToBinary (oplist : PT.Oplist) : Task<byte array> =
  oplist
  |> Convert.pt2ocamlOplist
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/oplist_json2bin"

let exprTLIDPairOfCachedBinary (data : byte array) : Task<PT.Expr * tlid> =
  data
  |> bytesToStringReq "bs/expr_tlid_pair_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.fluidExpr * OCamlTypes.tlid>
  |> Task.map Convert.ocamlexprTLIDPair2PT

let exprTLIDPairToCachedBinary ((expr, tlid) : (PT.Expr * tlid)) : Task<byte array> =
  (expr, tlid)
  |> Convert.pt2ocamlexprTLIDPair
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/expr_tlid_pair_json2bin"

let handlerBin2Json (data : byte array) (pos : pos) : Task<PT.Handler.T> =
  data
  |> bytesToStringReq "bs/handler_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.HandlerT.handler<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map (Convert.ocamlHandler2PT pos)

let dbBin2Json (data : byte array) (pos : pos) : Task<PT.DB.T> =
  data
  |> bytesToStringReq "bs/db_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.DbT.db<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map (Convert.ocamlDB2PT pos)

let userFnBin2Json (data : byte array) : Task<PT.UserFunction.T> =
  data
  |> bytesToStringReq "bs/user_fn_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.user_fn<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map Convert.ocamlUserFunction2PT

let userTypeBin2Json (data : byte array) : Task<PT.UserType.T> =
  data
  |> bytesToStringReq "bs/user_tipe_bin2json"
  |> Task.map Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.user_tipe>
  |> Task.map Convert.ocamlUserType2PT


let handlerJson2Bin (h : PT.Handler.T) : Task<byte array> =
  h
  |> Convert.pt2ocamlHandler
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/handler_json2bin"

let dbJson2Bin (db : PT.DB.T) : Task<byte array> =
  db
  |> Convert.pt2ocamlDB
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/db_json2bin"


let userFnJson2Bin (userFn : PT.UserFunction.T) : Task<byte array> =
  userFn
  |> Convert.pt2ocamlUserFunction
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/user_fn_json2bin"


let userTypeJson2Bin (userType : PT.UserType.T) : Task<byte array> =
  userType
  |> Convert.pt2ocamlUserType
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/user_tipe_json2bin"

let toplevelToCachedBinary (toplevel : PT.Toplevel.T) : Task<byte array> =
  match toplevel with
  | PT.Toplevel.TLHandler h -> handlerJson2Bin h
  | PT.Toplevel.TLDB db -> dbJson2Bin db
  | PT.Toplevel.TLFunction f -> userFnJson2Bin f
  | PT.Toplevel.TLType t -> userTypeJson2Bin t


// ---------------------------
// These are only here for fuzzing. We should not be fetching dvals via the
// OCaml runtime, but always via HTTP or via the DB.
// ---------------------------

let ofInternalQueryableV1 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_internal_queryable_v1" str

let ofInternalRoundtrippableV0 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_internal_roundtrippable_v0" str

let ofUnknownJsonV0 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_unknown_json_v0" str

let ofUnknownJsonV1 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_unknown_json_v1" str

let toDeveloperRepr (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_developer_repr_v0" dv

let toEnduserReadableTextV0 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_enduser_readable_text_v0" dv

let toHashableRepr (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_hashable_repr" dv

let toInternalQueryableV1 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_internal_queryable_v1" dv

let toInternalRoundtrippableV0 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_internal_roundtrippable_v0" dv

let toPrettyMachineJsonV1 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_pretty_machine_json_v1" dv

let toSafePrettyMachineYojsonV1 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_safe_pretty_machine_yojson_v1" dv

let toPrettyRequestJson (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_pretty_request_json" dv

let toPrettyResponseJson (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_pretty_response_json" dv

let toUrlString (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_url_string" dv

let dvalToQuery (dv : RT.Dval) : Task<List<string * List<string>>> =
  dvalToStringReq "fuzzing/dval_to_query" dv
  |> Task.map (Json.OCamlCompatible.deserialize<List<string * List<string>>>)

let queryToDval (p : List<string * List<string>>) : Task<RT.Dval> =
  p |> Json.OCamlCompatible.serialize |> stringToDvalReq "fuzzing/query_to_dval"

let dvalToFormEncoding (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/dval_to_form_encoding" dv

let queryStringToParams (s : string) : Task<List<string * List<string>>> =
  s
  |> UTF8.toBytes
  |> legacyStringReq "fuzzing/query_string_to_params"
  |> Task.map (Json.OCamlCompatible.deserialize<List<string * List<string>>>)

let paramsToQueryString (p : List<string * List<string>>) : Task<string> =
  p
  |> Json.OCamlCompatible.serialize
  |> UTF8.toBytes
  |> legacyStringReq "fuzzing/params_to_query_string"


let hashV0 (l : List<RT.Dval>) : Task<string> =
  dvalListToStringReq "fuzzing/hash_v0" l

let hashV1 (l : List<RT.Dval>) : Task<string> =
  dvalListToStringReq "fuzzing/hash_v1" l

let execute
  (ownerID : UserID)
  (canvasID : CanvasID)
  (program : PT.Expr)
  (symtable : Map<string, RT.Dval>)
  (dbs : List<PT.DB.T>)
  (fns : List<PT.UserFunction.T>)
  (packageFns : List<PT.Package.Fn>)
  : Task<RT.Dval> =
  let program = Convert.pt2ocamlExpr program

  let args =
    symtable |> Map.toList |> List.map (fun (k, dv) -> (k, Convert.rt2ocamlDval dv))

  let dbs = List.map Convert.pt2ocamlDB dbs
  let fns = List.map Convert.pt2ocamlUserFunction fns
  let packageFns = List.map Convert.pt2ocamlPackageManagerFn packageFns

  let tuple = (ownerID, canvasID, program, args, dbs, fns, packageFns)

  let str = Json.OCamlCompatible.serialize tuple

  task {
    try
      let! result = stringToDvalReq "execute" str
      return result
    with
    | e -> return (RT.DError(RT.SourceNone, e.Message))
  }

let executeExpr
  (ownerID : UserID)
  (canvasID : CanvasID)
  (program : RT.Expr)
  (symtable : Map<string, RT.Dval>)
  : Task<RT.Dval> =
  let program = Convert.rt2ocamlExpr program

  let args =
    symtable |> Map.toList |> List.map (fun (k, dv) -> (k, Convert.rt2ocamlDval dv))

  let dbs, fns, packageFns = [], [], []

  let tuple = (ownerID, canvasID, program, args, dbs, fns, packageFns)

  let str = Json.OCamlCompatible.serialize tuple

  task {
    try
      let! result = stringToDvalReq "execute" str
      return result
    with
    | e -> return (RT.DError(RT.SourceNone, e.Message))
  }

type BenchmarkResult = (float * OCamlTypes.RuntimeT.dval)

let benchmark
  (ownerID : UserID)
  (canvasID : CanvasID)
  (program : PT.Expr)
  (symtable : Map<string, RT.Dval>)
  (dbs : List<PT.DB.T>)
  (fns : List<PT.UserFunction.T>)
  : Task<float * RT.Dval> =
  let program = Convert.pt2ocamlExpr program

  let args =
    symtable |> Map.toList |> List.map (fun (k, dv) -> (k, Convert.rt2ocamlDval dv))

  let dbs = List.map Convert.pt2ocamlDB dbs
  let fns = List.map Convert.pt2ocamlUserFunction fns

  let str =
    Json.OCamlCompatible.serialize ((ownerID, canvasID, program, args, dbs, fns))

  task {
    try
      let! resultStr = str |> UTF8.toBytes |> legacyStringReq "benchmark"
      let (timing, dval) =
        Json.OCamlCompatible.deserialize<BenchmarkResult> resultStr

      return (timing, Convert.ocamlDval2rt dval)
    with
    | e ->
      return (System.Double.PositiveInfinity, RT.DError(RT.SourceNone, e.Message))
  }
