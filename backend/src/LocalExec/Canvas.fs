/// Run scripts locally using some builtin F#/dotnet libraries
module LocalExec.Canvas

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module NR = LibParser.NameResolver

module PM = LibPackageManager.PackageManager

open Utils

let parseYamlExn<'a> (filename : string) : 'a =
  let contents = System.IO.File.ReadAllText filename
  let deserialized = Legivel.Serialization.Deserialize<'a> contents

  match List.head deserialized with
  | Some(Legivel.Serialization.Success s) -> s.Data
  | ex -> raise (System.Exception $"couldn't parse {filename} with error {ex}")

type Config =
  { [<Legivel.Attributes.YamlField("id")>]
    CanvasId : Option<string>

    [<Legivel.Attributes.YamlField("main")>]
    Main : string }


let purgeDataFromInternalSqlTables (id : CanvasID) : Task<unit> =
  task {
    let runStmt stmt : Task<unit> =
      Sql.query stmt
      |> Sql.parameters [ "id", Sql.uuid id ]
      |> Sql.executeStatementAsync

    do! runStmt "DELETE FROM scheduling_rules_v0 WHERE canvas_id = @id"
    do! runStmt "DELETE FROM traces_v0 WHERE canvas_id = @id"
    do! runStmt "DELETE FROM user_data_v0 WHERE canvas_id = @id"
    do! runStmt "DELETE FROM cron_records_v0 WHERE canvas_id = @id"
    do! runStmt "DELETE FROM toplevels_v0 WHERE canvas_id = @id"
    do! runStmt "DELETE FROM canvases_v0 WHERE id = @id"
    do! runStmt "DELETE FROM domains_v0 WHERE canvas_id = @id"
    do! runStmt "DELETE FROM secrets_v0 WHERE canvas_id = @id"
  }


let loadFromDisk
  (pm : PT.PackageManager)
  (canvasName : string)
  : Ply<System.Guid * List<LibExecution.ProgramTypes.Toplevel.T>> =
  uply {
    print $"Loading canvas {canvasName} from disk"

    let canvasDir = $"canvases/{canvasName}"
    let config = parseYamlExn<Config> $"{canvasDir}/config.yml"

    // Create the canvas - expect this to be empty
    let domain = $"{canvasName}.dlio.localhost"
    let canvasID =
      match config.CanvasId with
      | None -> System.Guid.NewGuid()
      | Some id -> System.Guid.Parse id

    // i.e. dark-packages -> ("Darklang", "Packages")
    let (ownerName, canvasName) =
      if String.startsWith "dark-" canvasName then
        let canvasName = canvasName |> String.dropLeft 5
        // capitialize first letter
        let canvasName = canvasName[0].ToString().ToUpper() + canvasName.Substring 1
        ("Darklang", canvasName)
      else
        raise (
          System.Exception
            $"Currently only prepared to parse Darklang (dark-) canvases"
        )

    let! ownerID =
      task {
        match! LibCloud.Canvas.getOwner canvasID with
        | Some id -> return id
        | None ->
          let! ownerID = LibCloud.Account.createUser ownerName
          do! LibCloud.Canvas.createWithExactID canvasID ownerID domain
          return ownerID
      }

    // TODO this doesn't purge any added types/vals/fns from PM...
    // maybe we should?
    do! purgeDataFromInternalSqlTables canvasID
    do! LibCloud.Canvas.createWithExactID canvasID ownerID domain

    let! tls =
      uply {
        let fileName = $"{canvasDir}/{config.Main}.dark"
        let source = System.IO.File.ReadAllText fileName

        let! canvas =
          LibParser.Canvas.parse
            ownerName
            canvasName
            Builtins.accessibleByCanvas
            pm
            NR.OnMissing.ThrowError
            fileName
            source

        let handlers =
          canvas.handlers
          |> List.map (fun (spec, ast) ->
            PT.Toplevel.TLHandler { tlid = gid (); ast = ast; spec = spec })

        let dbs = canvas.dbs |> List.map PT.Toplevel.TLDB

        // Insert+apply canvas types, values, and fns as PackageOps
        let! _ = LibPackageManager.Inserts.insertAndApplyOps None None canvas.ops

        return
          (dbs @ handlers) |> List.map (fun tl -> tl, LibCloud.Serialize.NotDeleted)
      }

    print $"Saving {List.length tls} toplevels to canvas"

    do! LibCloud.Canvas.saveTLIDs canvasID tls

    let host = $"http://{domain}:{LibService.Config.bwdServerPort}"
    print $"Success, saved canvas - endpoints available at {host}"

    return (canvasID, List.map Tuple2.first tls)
  }
