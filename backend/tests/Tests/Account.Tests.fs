module Tests.Account

// Tests for LibBackend.Account

open Expecto
open Prelude
open TestUtils.TestUtils

module Account = LibBackend.Account


let testUsernameValidationWorks =
  testMany
    "validateUsername"
    UserName.validate
    [ "Upper",
      (Error
        "Invalid username 'Upper', can only contain lowercase roman letters and digits")
      "uPPer",
      (Error
        "Invalid username 'uPPer', can only contain lowercase roman letters and digits")
      "a",
      (Error
        "Invalid username 'a', can only contain lowercase roman letters and digits")
      "aaa❤️",
      (Error
        "Invalid username 'aaa❤️', can only contain lowercase roman letters and digits")
      "aaa-aaa",
      (Error
        "Invalid username 'aaa-aaa', can only contain lowercase roman letters and digits")
      "aaa aaa",
      (Error
        "Invalid username 'aaa aaa', can only contain lowercase roman letters and digits")
      "aaa_aaa", Ok "aaa_aaa"
      "myusername09", Ok "myusername09"
      "paul", Ok "paul" ]

let testCannotCreateReservedUser =
  let reservedAccount () = UserName.create "admin"
  let okAccount (suffix : string) = UserName.create $"notreserved_{suffix}"
  testList
    "reservedUser"
    [ testTask "upsert reserved" {
        let a = reservedAccount ()
        let! upserted = Account.createUser a
        Expect.equal upserted (Error "Username is not allowed") "reserved"
      }
      testTask "upsert not reserved" {
        let a = okAccount "a"
        let! upserted = Account.createUser a
        Expect.isOk upserted "not reserved"
      }
      testTask "insert reserved" {
        let a = reservedAccount ()
        let! inserted = Account.createUser a
        Expect.equal inserted (Error "Username is not allowed") "reserved"
      }
      testTask "insert not reserved" {
        let a = okAccount "b"
        let! inserted = Account.createUser a
        Expect.isOk inserted "not reserved"
      } ]


let tests =
  testList "Account" [ testUsernameValidationWorks; testCannotCreateReservedUser ]
