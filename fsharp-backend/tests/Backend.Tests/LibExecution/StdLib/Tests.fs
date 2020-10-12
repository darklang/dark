module LibExecution.StdLib.Tests

open Expecto

let tests =
  testList "StdLib" [ LibExecution.LibString.Tests.tests ]
