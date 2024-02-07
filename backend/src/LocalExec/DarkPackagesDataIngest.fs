/// Used to inject package items into the UserDB of the `dark-packages` canvas,
/// via SQL. CLEANUP.
module LocalExec.DarkPackagesDataIngest

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes

open Utils

open Npgsql.FSharp
open Npgsql
open LibCloud.Db


let typ name = RT.FQTypeName.fqPackage "Darklang" [ "DarkPackages" ] name 0

let genericNameTypeName = typ "GenericName"
let entryTypeName = typ "Entry"

let makeName owner modules name version =
  let fields =
    [ "owner", RT.DString owner
      "modules", RT.DList(RT.ValueType.string, modules |> List.map RT.DString)
      "name", RT.DString name
      "version", RT.DInt64 version ]
  RT.DRecord(genericNameTypeName, genericNameTypeName, [], Map fields)

let makeCategory (caseName : string) =
  let typeName = typ "Category"
  RT.DEnum(typeName, typeName, [], caseName, [])

let makeItem (caseName : string) (dv : RT.Dval) =
  let typeName = typ "Item"
  RT.DEnum(typeName, typeName, [], caseName, [ dv ])

let makeEntry name category item =
  let fields = [ "name", name; "category", category; "item", item ]
  RT.DRecord(entryTypeName, entryTypeName, [], Map fields)

/// Manually seed the DB with the package items
///
/// We do this here, in F#, because it's seemingly impossible to do anywhere else,
/// at least until the package manager is _stable_ in production.
///
/// Let's say there was a POST endpoint in `dark-packages` that took this data
/// and filled in the database. Well, the code in the `dark-packages` HTTP
/// endpoint's handler would need to be able to _access_ the package manager,
/// which is currently impossible, because the package manager is _only_ available
/// after it's filled in, like this. Quite the conundrum, huh?
let fillDarkPackagesCanvasWithData
  types
  canvasId
  dbTlid
  (packagesToManuallyInsert : PT.Packages)
  : Ply<unit> =
  uply {
    let packages = packagesToManuallyInsert

    let typeEntries =
      packages.types
      |> List.map (fun t ->
        let name = makeName t.name.owner t.name.modules t.name.name t.name.version
        let category = makeCategory "Type"
        let item = makeItem "Type" (PT2DT.PackageType.toDT t)
        makeEntry name category item)

    let constEntries =
      packages.constants
      |> List.map (fun c ->
        let name = makeName c.name.owner c.name.modules c.name.name c.name.version
        let category = makeCategory "Constant"
        let item = makeItem "Constant" (PT2DT.PackageConstant.toDT c)
        makeEntry name category item)

    let fnEntries =
      packages.fns
      |> List.map (fun f ->
        let name = makeName f.name.owner f.name.modules f.name.name f.name.version
        let category = makeCategory "Fn"
        let item = makeItem "Fn" (PT2DT.PackageFn.toDT f)
        makeEntry name category item)

    let allEntries = List.concat [ typeEntries; constEntries; fnEntries ]


    print $"Entries to insert: {List.length allEntries}"
    let dbRef : RT.DB.T =
      { tlid = dbTlid
        name = "PackageEntriesDB"
        typ = RT.TCustomType(Ok entryTypeName, [])
        version = 0 }


    let! dbEntries =
      allEntries
      |> Ply.List.mapSequentially (fun entry ->
        LibCloud.UserDB.dvalToDB None types dbRef entry)

    let dbJsonEntries =
      dbEntries
      |> List.map (fun entryDbJson ->
        [ "id", Sql.uuid (System.Guid.NewGuid())
          "canvasID", Sql.uuid canvasId
          "tlid", Sql.id dbRef.tlid
          "userVersion", Sql.int dbRef.version
          "darkVersion", Sql.int LibCloud.UserDB.currentDarkVersion
          "key", Sql.string (System.Guid.NewGuid() |> string)
          "data", Sql.jsonb entryDbJson ])

    let batchSize = 100

    // Function to split the list into batches of a specified size
    let batchList size list =
      list |> Seq.chunkBySize size |> Seq.map List.ofSeq |> Seq.toList

    // Splitting dbJsonEntries into batches
    let dbJsonEntryBatches = batchList batchSize dbJsonEntries

    dbJsonEntryBatches
    |> List.iteri (fun i batch ->
      print $"Inserting batch {i + 1}/{List.length dbJsonEntryBatches}"

      let query =
        $"INSERT INTO user_data_v0
            (id, canvas_id, table_tlid, user_version, dark_version, key, data, updated_at)
          VALUES
            (@id, @canvasID, @tlid, @userVersion, @darkVersion, @key, @data, NOW())"

      try
        LibService.DBConnection.dataSource
        |> Sql.fromDataSource
        |> Sql.executeTransaction [ query, batch ]
        |> ignore<int list>
      with e ->
        print $"Error inserting batch: {e.Message}"

        // print all records in the batch that failed to insert
        batch
        |> List.iter (fun entry ->
          print $"----"
          entry |> List.iter (fun (k, v) -> print $"- {k}: {v}"))

        raise e)

    print "Done inserting entries"
  }
