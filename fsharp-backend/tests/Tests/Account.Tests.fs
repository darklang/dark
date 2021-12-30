module Tests.Account

open Expecto
open Prelude
open TestUtils.TestUtils

module Account = LibBackend.Account

let testAuthentication =
  testTask "authenticated users" {
    let! username = Account.authenticate "test" "fVm2CUePzGKCwoEQQdNJktUQ"
    Expect.equal username (Some "test") "valid authentication"

    let! username = Account.authenticate "test_unhashed" "fVm2CUePzGKCwoEQQdNJktUQ"
    Expect.equal username None "invalid authentication"

    let! username = Account.authenticate "test" "no"
    Expect.equal username None "incorrect hash"

    let! username = Account.authenticate "test_unhashed" "no"
    Expect.equal username None "invalid authentication for unhashed"
  }


let testEmailValidationWorks =
  testMany
    "validateEmail"
    Account.validateEmail
    [ "novalidemail", (Error "Invalid email 'novalidemail'") ]


let testUsernameValidationWorks =
  testMany
    "validateUsername"
    UserName.validate
    [ "Upper",
      (Error "Invalid username 'Upper', must match /^[a-z][a-z0-9_]{2,20}$/")
      "uPPer",
      (Error "Invalid username 'uPPer', must match /^[a-z][a-z0-9_]{2,20}$/")
      "a", (Error "Invalid username 'a', must match /^[a-z][a-z0-9_]{2,20}$/")
      "aaa❤️",
      (Error "Invalid username 'aaa❤️', must match /^[a-z][a-z0-9_]{2,20}$/")
      "aaa-aaa",
      (Error "Invalid username 'aaa-aaa', must match /^[a-z][a-z0-9_]{2,20}$/")
      "aaa aaa",
      (Error "Invalid username 'aaa aaa', must match /^[a-z][a-z0-9_]{2,20}$/")
      "aaa_aaa", Ok "aaa_aaa"
      "myusername09", Ok "muusername09"
      "paul", Ok "paul" ]


let tests =
  testList
    "Account"
    [ testEmailValidationWorks; testUsernameValidationWorks; testAuthentication ]
