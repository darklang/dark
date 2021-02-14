module Tests.TestUtils

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


open LibExecution.RuntimeTypes

let rec dvalEquals (left : Dval) (right : Dval) (msg : string) : unit =
  let de l r = dvalEquals l r msg

  match left, right with
  | DFloat l, DFloat r -> Expect.floatClose Accuracy.veryHigh l r msg
  | DResult (Ok l), DResult (Ok r) -> de l r
  | DResult (Error l), DResult (Error r) -> de l r
  | DOption (Some l), DOption (Some r) -> de l r
  | DList ls, DList rs -> List.iter2 de ls rs
  | DObj ls, DObj rs ->
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
  | DFakeVal (DIncomplete _), DFakeVal (DIncomplete _) ->
      Expect.equal true true "two incompletes"
  // Keep for exhaustiveness checking
  | DHttpResponse _, _
  | DObj _, _
  | DList _, _
  | DResult _, _
  | DOption _, _
  // All others can be directly compared
  | DInt _, _
  | DDate _, _
  | DBool _, _
  | DFloat _, _
  | DNull, _
  | DStr _, _
  | DChar _, _
  | DFnVal _, _
  | DFakeVal _, _
  | DDB _, _
  | DUuid _, _
  | DBytes _, _ -> Expect.equal left right msg
