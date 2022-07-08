module Tests.Authorization

open Expecto
open Prelude
open TestUtils.TestUtils

module A = LibBackend.Authorization

// CLEANUP remove special case accounts
let testSpecialCaseAccounts () =
  testTask "special case permissions" {
    let! p = A.permission (OwnerName.create "rootvc") (UserName.create "lee")
    Expect.equal p (Some A.ReadWrite) "lee"

    let! p = A.permission (OwnerName.create "rootvc") (UserName.create "donkey")
    Expect.equal p None "donkey"

    let! p = A.permission (OwnerName.create "lee") (UserName.create "rootvc")
    Expect.equal p None "only goes one way"
  }



let testSetUserAccess =
  testTask "Changes with A.set_user_access affect permissions" {
    do!
      LibBackend.Account.upsertNonAdmin
        { username = UserName.create "test2"
          password = LibBackend.Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test2@darklang.com"
          name = "Dark Backend Tests" }
      |> Task.map (Exception.unwrapResultInternal [])

    do!
      LibBackend.Account.upsertAdmin
        { username = UserName.create "test_admin2"
          password = LibBackend.Password.fromPlaintext "fVm2CUePzGKCwoEQQdNJktUQ"
          email = "test+admin2@darklang.com"
          name = "Dark Backend Test Admin" }
      |> Task.map (Exception.unwrapResultInternal [])

    let username = UserName.create "test2"
    let owner = OwnerName.create "test_admin2"

    do! A.setUserAccess username owner None
    let! permission = A.grantedPermission username owner
    Expect.equal permission None "no initial access"

    do! A.setUserAccess username owner (Some A.Read)
    let! permission = A.grantedPermission username owner
    Expect.equal permission (Some A.Read) "read access"

    do! A.setUserAccess username owner (Some A.ReadWrite)
    let! permission = A.grantedPermission username owner
    Expect.equal permission (Some A.ReadWrite) "write access"

    do! A.setUserAccess username owner None
    let! permission = A.grantedPermission username owner
    Expect.equal permission None "revoked access"
  }


let testPermissionMax =
  test "permissionMax" {
    Expect.equal (A.Read.max (A.Read)) A.Read ""

    Expect.equal (A.Read.max (A.ReadWrite)) A.ReadWrite ""

    Expect.equal (A.ReadWrite.max (A.Read)) A.ReadWrite ""

    Expect.equal (A.ReadWrite.max (A.ReadWrite)) A.ReadWrite ""
  }

let tests = testList "A" [ testPermissionMax; testSetUserAccess ]
