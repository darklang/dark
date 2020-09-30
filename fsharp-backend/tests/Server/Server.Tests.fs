module Server.Tests

open Expecto

open LibExecution
open Server

let server =
  testList
    "Server"
    [ testCase "Adding valid Todo"
      <| fun _ ->
        let storage = Storage()
        let validTodo = Todo.create "TODO"
        let expectedResult = Ok()

        let result = storage.AddTodo validTodo

        Expect.equal result expectedResult "Result should be ok"
        Expect.contains (storage.GetTodos()) validTodo "Storage should contain new todo" ]

let all =
  testList
    "All"
    [ LibExecution.Tests.libExecution
      server ]

[<EntryPoint>]
let main _ = runTests defaultConfig all
