module Tests.BinarySerialization

open Expecto

open Prelude
open Tablecloth
open TestUtils.TestUtils

module File = LibBackend.File
module Config = LibBackend.Config
module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBackend.BinarySerialization



let testExprString =
  " do
      let x1 = 5
      let x2 = 6
      let bool = true
      let bool = false
      let str = \"a string\"
      let char = 'a'
      let float = -6.5
      let n = null
      let b = blank
      let i =
        if Bool.isError_ster 6
        then
          if 5 <> 6
          then 5 + 2
          else (fun y -> 2 + y)
        else
          x.y + Int.add_v0 6 2 + [5;6;7]
      let r =
        { field = 5 |> (+) 2
          constructor = Ok (Error (Just Nothing))
        }
      let m =
        match Mod.function_v2 with
        | Ok x -> v
        | 5 -> 6
        | true -> 7
        | 'c' -> 'c'
        | \"string\" -> \"string\"
        | null -> null
        | var -> 6 + var
        | 5.6 -> 5.6
        | blank -> 6
      let f = flag \"test\" true 5 6
      4"

let nonStringTestExprs =
  let e = PT.EInteger(gid (), 5)
  PT.EList(
    gid (),
    [ PT.EPartial(gid (), "some 🤬 string", e)
      PT.ERightPartial(gid (), "some 😭 string", e)
      PT.ELeftPartial(gid (), "some 👨‍👩‍👧‍👦 string", e) ]
  )

let testExpr =
  PT.ELet(gid (), "v", FSharpToExpr.parsePTExpr testExprString, nonStringTestExprs)

let testPos : PT.Position = { x = 6; y = 6 }

let testHandlerIDs : PT.Handler.ids =
  { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }

let testHttpHandler : PT.Handler.T =
  let spec = PT.Handler.HTTP("/path", "GET", testHandlerIDs)
  { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }

let testWorker : PT.Handler.T =
  let spec = PT.Handler.Worker("name", testHandlerIDs)
  { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }

let testOldWorker : PT.Handler.T =
  let spec = PT.Handler.OldWorker("MODULE", "name", testHandlerIDs)
  { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }

let testRepl : PT.Handler.T =
  let spec = PT.Handler.REPL("name", testHandlerIDs)
  { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }

let testCron1 : PT.Handler.T =
  let spec = PT.Handler.Cron("name", None, testHandlerIDs)
  { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }

let testCron2 : PT.Handler.T =
  let spec = PT.Handler.Cron("name", Some PT.Handler.Every12Hours, testHandlerIDs)
  { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }

let testUnknownHandler : PT.Handler.T =
  let spec = PT.Handler.UnknownHandler("name", "", testHandlerIDs)
  { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }


let testHandlers : List<PT.Handler.T> =
  [ testHttpHandler
    testWorker
    testCron1
    testCron2
    testRepl
    testUnknownHandler
    testOldWorker ]

let testType =
  PT.TRecord [ ("nested",
                PT.TList(
                  PT.TDict(
                    PT.TDB(
                      PT.THttpResponse(
                        PT.TOption(
                          PT.TDbList(
                            PT.TResult(PT.TInt, PT.TFn([ PT.TFloat ], PT.TNull))
                          )
                        )
                      )
                    )
                  )
                ))
               ("int", PT.TInt)
               ("int", PT.TFloat)
               ("float", PT.TFloat)
               ("bool", PT.TBool)
               ("null", PT.TNull)
               ("str", PT.TStr)
               ("incomplete", PT.TIncomplete)
               ("error", PT.TError)
               ("date", PT.TDate)
               ("char", PT.TChar)
               ("password", PT.TPassword)
               ("uuid", PT.TUuid)
               ("errorRail", PT.TErrorRail)
               ("bytes", PT.TBytes)
               ("variable ", PT.TVariable "v") ]


let testDB : List<PT.DB.T> =
  [ { tlid = 0UL
      pos = testPos
      nameID = gid ()
      name = "User"
      version = 0
      cols =
        [ { name = None; typ = None; nameID = gid (); typeID = gid () }
          { name = None; typ = Some PT.TInt; nameID = gid (); typeID = gid () }
          { name = Some "name"; typ = None; nameID = gid (); typeID = gid () }
          { name = Some "value"
            typ = Some testType
            nameID = gid ()
            typeID = gid () } ] } ]

let testFunctions : List<PT.UserFunction.T> =
  [ { tlid = 0UL
      name = "myFunc"
      nameID = gid ()
      parameters =
        [ { name = "myparam1"
            nameID = gid ()
            typ = None
            typeID = gid ()
            description = "param1" }
          { name = "myparam2"
            nameID = gid ()
            typ = Some testType
            typeID = gid ()
            description = "param1" } ]
      returnType = testType
      returnTypeID = gid ()
      description = "function description"
      infix = false
      body = testExpr } ]

let testUserTypes : List<PT.UserType.T> =
  [ { tlid = 0UL
      name = "User"
      nameID = gid ()
      version = 0
      definition =
        PT.UserType.Record [ { name = "prop1"
                               typ = None
                               nameID = gid ()
                               typeID = gid () }
                             { name = "prop1"
                               typ = Some testType
                               nameID = gid ()
                               typeID = gid () } ] } ]


let testToplevels : List<PT.Toplevel.T> =
  [ List.map PT.Toplevel.TLHandler testHandlers
    List.map PT.Toplevel.TLDB testDB
    List.map PT.Toplevel.TLFunction testFunctions
    List.map PT.Toplevel.TLType testUserTypes ]
  |> List.concat

let toplevelRoundtripTest =
  testMany
    "serializeToplevels"
    (fun tl ->
      tl
      |> BinarySerialization.serializeToplevel
      |> BinarySerialization.deserializeToplevel
      |> (=) tl)
    (List.map (fun x -> x, true) testToplevels)

let testOplist : PT.Oplist =
  let id = gid ()
  let tlid = gid ()
  [ PT.SetHandler(testHttpHandler.tlid, testPos, testHttpHandler)
    PT.CreateDB(tlid, testPos, "name")
    PT.AddDBCol(tlid, id, id)
    PT.SetDBColName(tlid, id, "name")
    PT.SetDBColType(tlid, id, "int")
    PT.DeleteTL tlid
    PT.MoveTL(tlid, testPos)
    PT.SetFunction(testFunctions[0])
    PT.ChangeDBColName(tlid, id, "name")
    PT.ChangeDBColType(tlid, id, "int")
    PT.UndoTL tlid
    PT.RedoTL tlid
    PT.SetExpr(tlid, id, testExpr)
    PT.TLSavepoint tlid
    PT.DeleteFunction tlid
    PT.CreateDBMigration(tlid, id, id, [ "fn", id, "str", id ])
    PT.AddDBColToDBMigration(tlid, id, id)
    PT.SetDBColNameInDBMigration(tlid, id, "newname")
    PT.SetDBColTypeInDBMigration(tlid, id, "string")
    PT.AbandonDBMigration tlid
    PT.DeleteColInDBMigration(tlid, id)
    PT.DeleteDBCol(tlid, id)
    PT.DeprecatedInitDBm(tlid, id, id, id, PT.DeprecatedMigrationKind)
    PT.RenameDBname(tlid, "newname")
    PT.CreateDBWithBlankOr(tlid, testPos, id, "User")
    PT.DeleteTLForever tlid
    PT.DeleteFunctionForever tlid
    PT.SetType(testUserTypes[0])
    PT.DeleteType tlid
    PT.DeleteTypeForever tlid ]

let oplistRoundtripTest =
  test "roundtrip oplists" {
    let actual =
      testOplist
      |> BinarySerialization.serializeOplist
      |> BinarySerialization.deserializeOplist
    Expect.equal actual testOplist ""
  }

/// Generates test files for binary serialization. These files are used to prove that
/// the binary serialization format did not change. We commit the output of these
/// files and if it differs then something changed. If we make changes to the binary
/// serialization format (or to the test cases), we regenerate the files instead and
/// commit them. Regenerate using ./scripts/build/regenerate-test-files

let testTestFiles =
  test "check test files are correct" {
    let expected = File.readfileBytes Config.Serialization "oplist-format.bin"
    let actual = testOplist |> BinarySerialization.serializeOplist
    Expect.equal actual expected "binary oplist"

    let expected = File.readfile Config.Serialization "oplist-format.json"
    let actual = testOplist |> BinarySerialization.serializeOplistToJson
    Expect.equal actual expected "json oplist"
  }


let generateBinarySerializationTestFiles () : unit =
  testOplist
  |> BinarySerialization.serializeOplist
  |> File.writefileBytes Config.Serialization "oplist-format.bin"

  testOplist
  |> BinarySerialization.serializeOplistToJson
  |> File.writefile Config.Serialization "oplist-format.json"

let tests =
  testList
    "BinarySerialization"
    [ toplevelRoundtripTest; oplistRoundtripTest; testTestFiles ]
