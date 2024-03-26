/// Used to inject package items into the UserDB of the `dark-packages` canvas,
/// via SQL. CLEANUP.
module LocalExec.DarkPackagesDataIngest

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes

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

    let copyCommand =
      """
      COPY user_data_v0 (id, canvas_id, table_tlid, user_version, dark_version, key, data)
      FROM STDIN WITH (FORMAT csv)
      """

    let csvLines =
      dbEntries
      |> List.map (fun entryDbJson ->
        [ System.Guid.NewGuid() |> string // id
          canvasId |> string // canvas_id
          dbRef.tlid |> string // table_tlid
          dbRef.version |> string // user_version
          LibCloud.UserDB.currentDarkVersion |> string // dark_version
          System.Guid.NewGuid() |> string // key
          "\"" + entryDbJson.Replace("\"", "\"\"") + "\"" ] // escape double quotes
        |> String.concat ",")

    try
      use conn = LibService.DBConnection.dataSource.OpenConnection()
      use writer = conn.BeginTextImport copyCommand
      csvLines |> List.iter writer.WriteLine
      writer.Close()
    with e ->
      print $"Error inserting batch: {e.Message}"
      raise e

    print "Done inserting entries"
  }
