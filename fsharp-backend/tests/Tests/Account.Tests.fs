module Tests.Account

open Expecto
open Prelude
open TestUtils

module Account = LibBackend.Account

// let t_special_case_accounts_work () =
//   AT.check
//     AT.bool
//     "lee is allowed"
//     true
//     (Authorization.can_edit_canvas ~canvas:"rootvc" ~username:"lee") ;
//   AT.check
//     AT.bool
//     "donkey isn't allowed"
//     false
//     (Authorization.can_edit_canvas ~canvas:"rootvc" ~username:"donkey") ;
//   AT.check
//     AT.bool
//     "only goes one way"
//     false
//     (Authorization.can_edit_canvas ~canvas:"lee" ~username:"rootvc") ;
//   ()
//
//
let testEmailValidationWorks =
  testMany
    "validateEmail"
    Account.validateEmail
    [ "novalidemail", (Error "Invalid email 'novalidemail'") ]


let testUsernameValidationWorks =
  testMany
    "validateUsername"
    Account.validateUserName
    [ "Upper",
      (Error "Invalid username 'Upper', must match /^[a-z][a-z0-9_]{2,19}$/")
      "uPPer",
      (Error "Invalid username 'uPPer', must match /^[a-z][a-z0-9_]{2,19}$/")
      "a", (Error "Invalid username 'a', must match /^[a-z][a-z0-9_]{2,19}$/")
      "aaa❤️",
      (Error "Invalid username 'aaa❤️', must match /^[a-z][a-z0-9_]{2,19}$/")
      "aaa-aaa",
      (Error "Invalid username 'aaa-aaa', must match /^[a-z][a-z0-9_]{2,19}$/")
      "aaa aaa",
      (Error "Invalid username 'aaa aaa', must match /^[a-z][a-z0-9_]{2,19}$/")
      "aaa_aaa", Ok()
      "myusername09", Ok()
      "paul", Ok() ]


let tests =
  testList "Account" [ testEmailValidationWorks; testUsernameValidationWorks ]
