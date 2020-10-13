module LibExecution.Tests

open Expecto

let tests =
  testList "LibExecution" [ LibExecution.StdLib.Tests.tests ]
