module Tests.BinarySerialization

open Expecto

open Prelude
open Tablecloth
open TestUtils.TestUtils

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

let testHandlers : List<PT.Handler.T> =
  let ids : PT.Handler.ids = { moduleID = 0UL; nameID = 0UL; modifierID = 0UL }
  let http : PT.Handler.T =
    let spec = PT.Handler.HTTP("/path", "GET", ids)
    { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }
  let worker : PT.Handler.T =
    let spec = PT.Handler.Worker("name", ids)
    { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }
  let oldWorker : PT.Handler.T =
    let spec = PT.Handler.OldWorker("MODULE", "name", ids)
    { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }
  let repl : PT.Handler.T =
    let spec = PT.Handler.REPL("name", ids)
    { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }
  let cron1 : PT.Handler.T =
    let spec = PT.Handler.Cron("name", None, ids)
    { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }
  let cron2 : PT.Handler.T =
    let spec = PT.Handler.Cron("name", Some PT.Handler.Every12Hours, ids)
    { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }
  let unknown : PT.Handler.T =
    let spec = PT.Handler.UnknownHandler("name", "", ids)
    { spec = spec; tlid = 0UL; ast = testExpr; pos = testPos }
  [ http; worker; cron1; cron2; repl; unknown; oldWorker ]

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



let roundtripTest =
  testMany
    "serializeProgramTypes"
    (fun tl ->
      tl
      |> BinarySerialization.serializeToplevel
      |> BinarySerialization.deserializeToplevel
      |> (=) tl)
    (List.map (fun x -> x, true) testToplevels)


let tests = testList "BinarySerialization" [ roundtripTest ]
