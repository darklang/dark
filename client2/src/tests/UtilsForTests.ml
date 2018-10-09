open Tea
open! Porting
module B = Blank
open Types

let pass = Ok ()

let fail v = Error (toString v)

let expectOk r =
  match r with Ok () -> Expect.pass | Error msg -> Expect.fail msg

let expectTrue = Expect.true_ ""

let expectFalse = Expect.false_ ""

let test msg e = Test.test msg (fun _ -> e)
