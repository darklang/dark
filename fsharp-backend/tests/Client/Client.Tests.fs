module Client.Tests

open Fable.Mocha

let client =
  testList
    "Client"
    [ testCase "Added todo"
      <| fun _ ->
        let newTodo = Todo.create "new todo"
        let model, _ = init ()

        let model, _ = update (AddedTodo newTodo) model

        Expect.equal 1 model.Todos.Length "There should be 1 todo"
        Expect.equal newTodo model.Todos.[0] "Todo should equal new todo" ]

let all =
  testList
    "All"
    [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
    LibExecution.Tests.libExecution
#endif
    client ]

[<EntryPoint>]
let main _ = Mocha.runTests all
