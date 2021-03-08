module TestUtils

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp.Tasks
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Exe = LibExecution.Execution

let testOwner : Lazy<Task<Account.UserInfo>> =
  lazy
    (UserName.create "test"
     |> LibBackend.Account.getUser
     |> Task.map Option.unwrapUnsafe)

let testCanvasID : Lazy<Task<CanvasID>> =
  lazy
    (task {
      let! owner = testOwner.Force()

      return!
        CanvasName.create "test" |> LibBackend.Canvas.canvasIDForCanvasName owner.id
     })

// delete test data for one canvas
let clearCanvasData (name : CanvasName.T) : Task<unit> =
  task {
    let! owner = testOwner.Force()
    let! canvasID = Canvas.canvasIDForCanvasName owner.id name

    let events =
      Sql.query "DELETE FROM events where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let storedEvents =
      Sql.query "DELETE FROM stored_events_v2 where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let functionResults =
      Sql.query "DELETE FROM function_results_v2 where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let functionArguments =
      Sql.query "DELETE FROM function_arguments where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let userData =
      Sql.query "DELETE FROM user_data where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let cronRecords =
      Sql.query "DELETE FROM cron_records where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    let toplevelOplists =
      Sql.query "DELETE FROM toplevel_oplists where canvas_id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync
      :> Task

    do!
      Task.WhenAll [| cronRecords
                      toplevelOplists
                      userData
                      functionArguments
                      functionResults
                      storedEvents
                      events |]

    do!
      Sql.query "DELETE FROM canvases where id = @id::uuid"
      |> Sql.parameters [ "id", Sql.uuid canvasID ]
      |> Sql.executeStatementAsync

    return ()
  }

let executionStateFor
  (name : string)
  (dbs : Map<string, RT.DB.T>)
  (userFunctions : Map<string, RT.UserFunction.T>)
  (fns : Map<RT.FQFnName.T, RT.BuiltInFn>)
  : Task<RT.ExecutionState> =
  task {
    let! owner = testOwner.Force()
    let ownerID : UserID = (owner : LibBackend.Account.UserInfo).id

    // Performance optimization: don't touch the DB if you don't use the DB
    let! canvasID =
      if Map.count dbs > 0 then
        task {
          let hash = sha1digest name |> System.Convert.ToBase64String
          let canvasName = CanvasName.create $"test-{hash}"
          do! clearCanvasData canvasName

          let! canvasID = LibBackend.Canvas.canvasIDForCanvasName ownerID canvasName

          return canvasID
        }
      else
        task { return! testCanvasID.Force() }

    let tlid = id 7

    return
      Exe.createState
        ownerID
        canvasID
        tlid
        fns
        Map.empty
        dbs
        userFunctions
        Map.empty
        []
  }




let testMany (name : string) (fn : 'a -> 'b) (values : List<'a * 'b>) =
  testList
    name
    (List.mapi
      (fun i (input, expected) ->
        test $"{name}[{i}]: ({input}) -> {expected}" {
          Expect.equal (fn input) expected "" })
      values)

let testMany2 (name : string) (fn : 'a -> 'b -> 'c) (values : List<'a * 'b * 'c>) =
  testList
    name
    (List.mapi
      (fun i (input1, input2, expected) ->
        test $"{name}[{i}]: ({input1}, {input2}) -> {expected}" {
          Expect.equal (fn input1 input2) expected "" })
      values)

let testManyTask (name : string) (fn : 'a -> Task<'b>) (values : List<'a * 'b>) =
  testList
    name
    (List.mapi
      (fun i (input, expected) ->
        testTask $"{name} - {i}" {
          let! result = fn input
          Expect.equal result expected ""
        })
      values)

// Allow reusing property-test definitions with test cases found by fuzzing
let testListUsingProperty
  (name : string)
  (prop : 'a -> bool)
  (list : (string * 'a) list)
  =
  testList
    name
    (List.map
      (fun (doc, testCase) ->
        let doc = if doc = "" then testCase.ToString() else doc
        testTask $"{name} {doc}" { return (Expect.isTrue (prop testCase) "") })
      list)

open LibExecution.RuntimeTypes

module Expect =
  let rec equalDval (actual : Dval) (expected : Dval) (msg : string) : unit =
    let de a e = equalDval a e msg

    match actual, expected with
    | DFloat l, DFloat r ->
        if System.Double.IsNaN l && System.Double.IsNaN r then
          ()
        else if System.Double.IsPositiveInfinity l
                && System.Double.IsPositiveInfinity r then
          ()
        else if System.Double.IsNegativeInfinity l
                && System.Double.IsNegativeInfinity r then
          ()
        else
          Expect.floatClose Accuracy.veryHigh l r msg
    | DResult (Ok l), DResult (Ok r) -> de l r
    | DResult (Error l), DResult (Error r) -> de l r
    | DOption (Some l), DOption (Some r) -> de l r
    | DDate l, DDate r ->
        // Set the milliseconds to zero as we don't preserve them in serializations
        Expect.equal
          (l.AddMilliseconds(-(double l.Millisecond)))
          (r.AddMilliseconds(-(double r.Millisecond)))
          $"{msg}: {l} <> {r}"
    | DList ls, DList rs ->
        let lLength = List.length ls
        let rLength = List.length rs
        Expect.equal lLength rLength $"{ls} <> {rs} in ({msg})"
        List.iter2 de ls rs
    | DObj ls, DObj rs ->
        let lLength = Map.count ls
        let rLength = Map.count rs
        Expect.equal lLength rLength $"{ls} <> {rs} in ({msg})"

        List.iter2
          (fun (k1, v1) (k2, v2) ->
            Expect.equal k1 k2 msg
            de v1 v2)
          (Map.toList ls)
          (Map.toList rs)
    | DHttpResponse (Response (sc1, h1), b1), DHttpResponse (Response (sc2, h2), b2) ->
        Expect.equal sc1 sc2 msg
        Expect.equal h1 h2 msg
        de b1 b2
    | DHttpResponse (Redirect u1, b1), DHttpResponse (Redirect u2, b2) ->
        Expect.equal u1 u2 msg
        de b1 b2
    | DIncomplete _, DIncomplete _ -> Expect.equal true true "two incompletes"
    | DErrorRail l, DErrorRail r -> de l r
    // Keep for exhaustiveness checking
    | DHttpResponse _, _
    | DObj _, _
    | DList _, _
    | DResult _, _
    | DErrorRail _, _
    | DOption _, _
    // All others can be directly compared
    | DInt _, _
    | DDate _, _
    | DBool _, _
    | DFloat _, _
    | DNull, _
    | DStr _, _
    | DChar _, _
    | DPassword _, _
    | DFnVal _, _
    | DIncomplete _, _
    | DError _, _
    | DDB _, _
    | DUuid _, _
    | DBytes _, _ -> Expect.equal actual expected msg

// We reimplement the same conditions as it's confusing to have this print out the same errors
let rec dvalEquality (left : Dval) (right : Dval) : bool =
  let de = dvalEquality

  match left, right with
  | DFloat l, DFloat r ->
      if System.Double.IsNaN l && System.Double.IsNaN r then
        true
      else if System.Double.IsPositiveInfinity l
              && System.Double.IsPositiveInfinity r then
        true
      else if System.Double.IsNegativeInfinity l
              && System.Double.IsNegativeInfinity r then
        true
      else
        Accuracy.areClose Accuracy.veryHigh l r
  | DResult (Ok l), DResult (Ok r) -> de l r
  | DResult (Error l), DResult (Error r) -> de l r
  | DOption (Some l), DOption (Some r) -> de l r
  | DDate l, DDate r ->
      // Set the milliseconds to zero as we don't preserve them in serializations
      let newL = l.AddMilliseconds(-(double l.Millisecond))
      let newR = r.AddMilliseconds(-(double r.Millisecond))
      newL = newR
  | DList ls, DList rs -> List.map2 de ls rs |> List.all (fun x -> x)
  | DObj ls, DObj rs ->
      List.map2
        (fun (k1, v1) (k2, v2) -> k1 = k2 && de v1 v2)
        (Map.toList ls)
        (Map.toList rs)
      |> List.all (fun x -> x)
  | DHttpResponse (Response (sc1, h1), b1), DHttpResponse (Response (sc2, h2), b2) ->
      sc1 = sc2 && h1 = h2 && de b1 b2
  | DHttpResponse (Redirect u1, b1), DHttpResponse (Redirect u2, b2) ->
      u1 = u2 && de b1 b2
  | DIncomplete _, DIncomplete _ -> true
  | DError (_, msg1), DError (_, msg2) ->
      (msg1.Replace("_v0", "")) = (msg2.Replace("_v0", ""))
  | DErrorRail l, DErrorRail r -> de l r
  // Keep for exhaustiveness checking
  | DHttpResponse _, _
  | DObj _, _
  | DErrorRail _, _
  | DList _, _
  | DResult _, _
  | DOption _, _
  // All others can be directly compared
  | DInt _, _
  | DDate _, _
  | DPassword _, _
  | DBool _, _
  | DFloat _, _
  | DNull, _
  | DStr _, _
  | DChar _, _
  | DFnVal _, _
  | DIncomplete _, _
  | DError _, _
  | DDB _, _
  | DUuid _, _
  | DBytes _, _ -> left = right

let dvalMapEquality (m1 : DvalMap) (m2 : DvalMap) = dvalEquality (DObj m1) (DObj m2)

let interestingFloats : List<string * float> =
  let initial =
    // interesting cause OCaml uses 31 bit ints
    [ "min 31 bit", System.Math.Pow(2.0, 30.0) - 1.0
      "max 31 bit", -System.Math.Pow(2.0, 30.0)
      // interesting cause boundary of 32 bit ints
      "min 32 bit", System.Math.Pow(2.0, 31.0) - 1.0
      "max 32 bit", -System.Math.Pow(2.0, 31.0)
      // interesting cause doubles support up to 53-bit ints
      "min 53 bit", System.Math.Pow(2.0, 52.0) - 1.0
      "max 53 bit", -System.Math.Pow(2.0, 52.0)
      // interesting cause OCaml uses 63 bit ints
      "min 63 bit", System.Math.Pow(2.0, 62.0) - 1.0
      "max 63 bit", -System.Math.Pow(2.0, 62.0)
      // interesting cause boundary of 64 bit ints
      "min 64 bit", System.Math.Pow(2.0, 63.0) - 1.0
      "max 64 bit", -System.Math.Pow(2.0, 63.0)
      // Interesting anyway
      "zero", 0.0
      "negative zero", -0.0
      "NaN", nan
      "infinity", infinity
      "-infinity", -infinity
      // Mathy values
      "e", System.Math.E
      "pi", System.Math.PI
      "tau", System.Math.Tau ]

  initial
  |> List.flatMap
       (fun (doc, v) ->
         [ ($"{doc} - 1", v - 1.0); ($"{doc} + 0", v); ($"{doc} + 1", v + 1.0) ])

let sampleDvals : List<string * Dval> =
  [ ("int0", Dval.int 0)
    ("int-1", Dval.int (-1))
    ("int5", Dval.int 5)
    ("int_max_31_bits", DInt 1073741824I)
    ("int_above_31_bits", DInt 1073741825I)
    ("int_max_32_bits", DInt 2147483647I)
    ("int_above_32_bits", DInt 2147483648I)
    ("int_max_53_bits", DInt 4503599627370496I)
    ("int_above_53_bits", DInt 4503599627370497I)
    ("int_max_63_bits", DInt 4611686018427387903I)
    ("float", DFloat 7.2)
    ("float2", DFloat -7.2)
    ("float3", DFloat 15.0)
    ("float4", DFloat -15.0) ]
  @ (List.map (fun (doc, float) -> ($"float {doc}", DFloat float)) interestingFloats)
    @ [ ("true", DBool true)
        ("false", DBool false)
        ("null", DNull)
        ("datastore", DDB "Visitors")
        ("string", DStr "incredibly this was broken")
        // Json.NET has a habit of converting things automatically based on the type in the string
        ("date string", DStr "2018-09-14T00:31:41Z")
        ("int string", DStr "1039485")
        ("int string2", DStr "-1039485")
        ("int string3", DStr "0")
        ("uuid string", DStr "7d9e5495-b068-4364-a2cc-3633ab4d13e6")
        ("list", DList [ Dval.int 4 ])
        ("obj", DObj(Map.ofList [ "foo", Dval.int 5 ]))
        ("obj2", DObj(Map.ofList [ ("type", DStr "weird"); ("value", DNull) ]))
        ("obj3", DObj(Map.ofList [ ("type", DStr "weird"); ("value", DStr "x") ]))
        // More Json.NET tests
        ("obj4", DObj(Map.ofList [ "foo\\\\bar", Dval.int 5 ]))
        ("obj5", DObj(Map.ofList [ "$type", Dval.int 5 ]))
        ("incomplete", DIncomplete SourceNone)
        ("error", DError(SourceNone, "some error string"))
        ("block",
         DFnVal(
           Lambda
             { body = RT.EBlank(id 1234)
               symtable = Map.empty
               parameters = [ (id 5678, "a") ] }
         ))
        ("errorrail", DErrorRail(Dval.int 5))
        ("errorrail with float",
         DErrorRail(DObj(Map.ofList ([ ("", DFloat nan); ("", DNull) ]))))
        ("redirect", DHttpResponse(Redirect "/home", DNull))
        ("httpresponse", DHttpResponse(Response(200, []), DStr "success"))
        ("db", DDB "Visitors")
        ("date", DDate(System.DateTime.ofIsoString "2018-09-14T00:31:41Z"))
        ("password", DPassword(Password(toBytes "somebytes")))
        ("uuid", DUuid(System.Guid.Parse "7d9e5495-b068-4364-a2cc-3633ab4d13e6"))
        ("uuid0", DUuid(System.Guid.Parse "00000000-0000-0000-0000-000000000000"))
        ("option", DOption None)
        ("option2", DOption(Some(Dval.int 15)))
        ("option3", DOption(Some(DStr "a string")))
        ("character", DChar "s")
        ("result", DResult(Ok(Dval.int 15)))
        ("result2", DResult(Error(DList [ DStr "dunno if really supported" ])))
        ("result3", DResult(Ok(DStr "a string")))
        ("bytes", "JyIoXCg=" |> System.Convert.FromBase64String |> DBytes)
        // use image bytes here to test for any weird bytes forms
        ("bytes2",
         DBytes(
           LibBackend.File.readfileBytes
             LibBackend.Config.Testdata
             "sample_image_bytes.png"
         )) ]

// FSTODO: deeply nested data
