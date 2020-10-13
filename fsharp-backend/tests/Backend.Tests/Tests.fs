module All.Tests

open Expecto

let tests =
  testList
    "Backend tests"
    // FSTODO Client tests
    [ LibExecution.Tests.tests
      BwdServer.Tests.tests
      ApiServer.Tests.tests ]

[<EntryPoint>]
let main _ = runTests defaultConfig tests
