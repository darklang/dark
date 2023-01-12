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

type OnDiskFormat = ClientTypes.Program.Oplist


let jsonFilename (name : string) = $"{name}.json"

/// Load a json-serialized canvas from disk
///
/// If file not found, returns empty oplist
let tryLoadJsonFromDisk
  (root : LibBackend.Config.Root)
  (c : Meta)
  : Option<PT.Oplist> =
  string c.name
  |> jsonFilename
  |> File.tryReadFile root
  |> Option.map (fun json ->
    json
    |> Json.Vanilla.deserialize<OnDiskFormat>
    |> List.map ClientTypes2ExecutionTypes.ProgramTypes.Op.fromCT)

// Save oplists to disk
let saveJsonToDisk
  (root : LibBackend.Config.Root)
  (c : Meta)
  (oplists : PT.Oplist)
  : unit =
  let json =
    oplists
    |> List.map ClientTypes2ExecutionTypes.ProgramTypes.Op.toCT
    |> Json.Vanilla.prettySerialize
  let filename = string c.name |> jsonFilename
  File.writefile root filename json







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
      // Save back in the latest format
      do tls |> Option.tap (saveJsonToDisk LibBackend.Config.Testdata meta)

      match tls with
      | Some tls ->
        tls
        |> LibBackend.Op.oplist2TLIDOplists

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
