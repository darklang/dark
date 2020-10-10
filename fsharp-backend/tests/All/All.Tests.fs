module All.Tests

open Expecto

let all =
  testList
    "All"
    // FSTODO Client tests
    [ LibExecution.Tests.all
      BwdServer.Tests.all
      ApiServer.Tests.all ]

[<EntryPoint>]
let main _ = runTests defaultConfig all
