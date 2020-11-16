module LibBackend.Serialization

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp
open Ply
open Npgsql
open Db
open Prelude

open LibExecution.Runtime
open LibExecution.Framework

let gid = Shortcuts.gid


module YojsonParse =
  open FSharp.Data

  let parseExpr (j : JsonValue) =
    printfn "%s" (j.ToString())
    let constructor = j.Item(0).AsString()
    let id = j.Item(1).AsInteger64()
    match constructor, j.AsArray().[2..] with
    | "EString", [| str |] -> EString(id, str.AsString())
    | "EBlank", _ -> EBlank(id)
    | _ -> failwith "Unimplemented"

  let parseHandlerSpec (j : JsonValue) : Handler.Spec =
    let blankOr (j : JsonValue) : (id * string) =
      // element 0 is "filled" or "blank"
      let value =
        j.AsArray()
        |> Array.tryItem 2
        |> Option.map (fun j -> j.AsString())
        |> Option.defaultValue ""

      (j.Item(1).AsInteger64(), value)

    let moduleID, module_ = j.Item("module") |> blankOr
    let nameID, name = j.Item("name") |> blankOr
    let modifierID, modifier = j.Item("modifier") |> blankOr

    let (ids : Handler.ids) =
      { moduleID = moduleID; nameID = nameID; modifierID = modifierID }

    if module_ = "" then
      Handler.REPL {| name = name; ids = ids |}
    else if module_ = "REPL" then
      Handler.REPL {| name = name; ids = ids |}
    else if module_ = "HTTP" then
      Handler.HTTP {| path = name; method = modifier; ids = ids |}
    else if module_ = "CRON" then
      Handler.Cron {| name = name; interval = modifier; ids = ids |}
    else if module_ = "WORKER" then
      Handler.Worker {| name = name; ids = ids |}
    else
      Handler.OldWorker {| modulename = module_; name = name; ids = ids |}

  let parseHandler (j : JsonValue) : Handler.T =
    { tlid = j.Item("tlid").AsInteger64()
      ast = j.Item("ast") |> parseExpr
      spec = j.Item("spec") |> parseHandlerSpec }

module ReadFromOCaml =
  // FSTODO: if we need speed, switch to transferring using protobufs instead of JSON
  // where do we get the protobuf files?
  // - 1) generate using ocaml types (maybe deprecated? ocaml-protoc)
  // - 2) generate using FSharp types (possible?)
  // - 3) hand-write them (converters on both ends)

  // initialize OCaml runtime
  [<DllImport("./libserialization.so",
              CallingConvention = CallingConvention.Cdecl,
              EntryPoint = "dark_init_ocaml")>]
  extern string darkInitOcaml()

  // take a binary rep of a handler from the DB and convert it into JSON
  [<DllImport("./libserialization.so",
              CallingConvention = CallingConvention.Cdecl,
              EntryPoint = "handler_of_binary_string_to_json")>]
  extern string handlerOfBinaryStringToJson(byte[] bytes, int length)

  let init () =
    printfn "serialization_init"
    let str = darkInitOcaml ()
    printfn "serialization_inited: %s" str
    ()

  let parseCachedOCaml ((data, pos) : (byte array * byte array option)) : Toplevel =
    try
      handlerOfBinaryStringToJson (data, data.Length)
      |> FSharp.Data.JsonValue.Parse
      |> YojsonParse.parseHandler
      |> TLHandler
    with _ -> failwith "Exception found in cachedOcamlToJson"

let fetchReleventTLIDsForHTTP (host : string)
                              (canvasID : CanvasID)
                              (path : string)
                              (method : string)
                              : Task<List<tlid>> =

  // The pattern `$2 like name` is deliberate, to leverage the DB's
  // pattern matching to solve our routing.
  Sql.query "SELECT tlid
             FROM toplevel_oplists
             WHERE canvas_id = @canvasID
               AND ((module = 'HTTP'
                     AND @path like name
                     AND modifier = @method)
               OR tipe <> 'handler'::toplevel_type)"
  |> Sql.parameters [ "path", Sql.string path
                      "method", Sql.string method
                      "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.int64 "tlid")

let canvasIDForCanvas (owner : UserID) (canvasName : string) : Task<CanvasID> =
  if canvasName.Length > 64 then
    failwith $"Canvas name was length {canvasName.Length}, must be <= 64"
  else
    // TODO: we create the canvas if it doesn't exist here, seems like a poor choice
    Sql.query "SELECT canvas_id(@newUUID, @owner, @canvasName)"
    |> Sql.parameters [ "newUUID", Sql.uuid (System.Guid.NewGuid())
                        "owner", Sql.uuid owner
                        "canvasName", Sql.string canvasName ]
    |> Sql.executeRowAsync (fun read -> read.uuid "canvas_id")

// split into owner and canvasName
let ownerNameFromHost (host : string) : string =
  match host.Split [| '-' |] |> Seq.toList with
  | owner :: _rest -> owner
  | _ -> host

let userIDForUsername (user : string) : Task<UserID> =
  let owner = String.toLower user
  if List.contains owner Account.bannedUsernames then
    failwith "Banned username"
  else
    Sql.query "SELECT id
                  FROM accounts
                  WHERE accounts.username = @username"
    |> Sql.parameters [ "username", Sql.string user ]
    |> Sql.executeRowAsync (fun read -> read.uuid "id")


let canvasNameFromCustomDomain host : Task<string> =
  Sql.query "SELECT canvas
                  FROM customer_domains
                  WHERE host = @host"
  |> Sql.parameters [ "host", Sql.string host ]
  |> Sql.executeRowAsync (fun read -> read.string "canvas")



let loadUncachedToplevels (host : string)
                          (canvasID : CanvasID)
                          (tlids : List<tlid>)
                          : Task<List<byte array>> =

  Sql.query "SELECT data
             FROM toplevel_oplists
             WHERE canvas_id = @canvasID
             AND tlid = ANY(@tlids)"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.tlidArray tlids ]
  |> Sql.executeAsync (fun read -> read.bytea "data")

let loadCachedToplevels (host : string)
                        (canvasID : CanvasID)
                        (tlids : List<tlid>)
                        : Task<List<byte array * byte array option>> =
  Sql.query "SELECT rendered_oplist_cache, pos FROM toplevel_oplists
             WHERE canvas_id = @canvasID
             AND tlid = ANY (@tlids)
             AND deleted IS FALSE
             AND (((tipe = 'handler'::toplevel_type) AND pos IS NOT NULL))"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.tlidArray tlids ]
  |> Sql.executeAsync (fun read ->
       (read.bytea "rendered_oplist_cache", read.byteaOrNone "pos"))


let loadHttpHandlersFromCache (host : string)
                              (canvasID : CanvasID)
                              (owner : UserID)
                              (path : string)
                              (method : string)
                              : Task<List<Toplevel>> =
  task {
    let! tlids = fetchReleventTLIDsForHTTP host canvasID path method
    let! binaryTLs = loadCachedToplevels host canvasID tlids
    let tls = List.map ReadFromOCaml.parseCachedOCaml binaryTLs

    return tls
  }
