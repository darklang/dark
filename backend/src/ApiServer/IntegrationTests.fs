/// For loading/saving integration tests
module ApiServer.IntegrationTests

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module File = LibBackend.File
module Canvas = LibBackend.Canvas


type Meta = LibBackend.Canvas.Meta


let jsonFilename (name : string) = $"{name}.json"

/// Load a json-serialized canvas from disk
///
/// If file not found, returns empty oplist
let tryLoadJsonFromDisk
  (root : LibBackend.Config.Root)
  (c : Meta)
  : List<tlid * PT.Oplist> option =
  string c.name
  |> jsonFilename
  |> File.tryReadFile root
  |> Option.map (fun json ->
    json
    |> Json.Vanilla.deserialize<ClientTypes.Program.Oplist>
    |> List.map ClientTypes2ExecutionTypes.ProgramTypes.Op.fromCT
    |> LibBackend.Op.oplist2TLIDOplists)



// let save_json_to_disk ~root (filename : string) (ops : Types.tlid_oplists) :
//     unit =
//   Log.infO
//     "serialization"
//     ~params:[("save_to", "disk"); ("format", "json"); ("filename", filename)] ;
//   let module SF = Serialization_format in
//   ops
//   |> Op.tlid_oplists2oplist
//   |> Serialization_converters.oplist_of_fluid
//   |> SF.oplist_to_yojson SF.RuntimeT.expr_to_yojson
//   |> Yojson.Safe.pretty_to_string
//   |> (fun s -> s ^ "\n")
//   |> File.writefile ~root filename


// let minimize (c T) T =
//   (* TODO *)
//   (* let ops = *)
//   (*   c.ops *)
//   (*   |> Undo.preprocess *)
//   (*   |> List.filter ~f:Op.has_effect *)
//   (* in { c with ops = ops } *)
//   c
//
//
// let save_test (c T) : string =
//   let c = minimize c in
//   let host = "test-" ^ c.host in
//   let file = json_filename host in
//   let host =
//     if File.file_exists ~root:Testdata file
//     then host ^ "_" ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
//     else host
//   in
//   let file = json_filename host in
//   save_json_to_disk ~root:Testdata file c.ops ;
//   file

let loadAndResaveFromTestFile (meta : Meta) : Task<unit> =
  task {
    let oplists =
      let tls = meta |> tryLoadJsonFromDisk LibBackend.Config.Testdata

      match tls with
      | Some tls ->
        tls
        |> List.map (fun (tlid, oplist) ->
          let tl =
            let oplist = Canvas.fromOplist meta [] oplist
            let tls = Canvas.toplevels oplist
            let dtls = Canvas.deletedToplevels oplist
            (Map.mergeFavoringLeft tls dtls)
            |> Map.get tlid
            |> Exception.unwrapOptionInternal "could not find tlid" [ "tlid", tlid ]
          (tlid, oplist, tl, Canvas.NotDeleted))
      | None -> []

    do! Canvas.saveTLIDs meta oplists
    return ()
  }
