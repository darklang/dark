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
