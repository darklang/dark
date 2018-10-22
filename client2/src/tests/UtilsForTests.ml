open Tea
open! Porting
module B = Blank
open Types

let pass : testResult = Ok ()

let fail (v : 'a) : testResult = Error (toString v)

let expectOk (r : testResult) : expectation =
  match r with Ok () -> Expect.pass | Error msg -> Expect.fail msg

let expectTrue (b : bool) : expectation = Expect.true_ "" b

let expectFalse (b : bool) : expectation = Expect.false_ "" b

let test (msg : string) (e : expectation) : Test.test =
  Test.test msg (fun _ -> e)
