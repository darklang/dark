module Tests.Authorization

open Expecto
open Prelude
open TestUtils

module Authorization = LibBackend.Authorization

// let t_authenticate_user () =
//   AT.check
//     AT.bool
//     "Account.authenticate_user works for the test user"
//     true
//     ( Account.authenticate "test" "fVm2CUePzGKCwoEQQdNJktUQ"
//       |> Option.value ~default:"failure"
//       = "test"
//     && (not
//           ( Account.authenticate "test_unhashed" "fVm2CUePzGKCwoEQQdNJktUQ"
//           |> Option.is_some ))
//     && (not (Account.authenticate "test" "no" |> Option.is_some))
//     && not (Account.authenticate "test_unhashed" "no" |> Option.is_some) )

let testSetUserAccess =
  testTask "Changes with Authorization.set_user_access affect permissions" {
    let username = UserName.create "test"
    let owner = OwnerName.create "test_admin"

    do! Authorization.setUserAccess username owner None
    let! permission = Authorization.grantedPermission username owner
    Expect.equal permission None "no initial access"

    do! Authorization.setUserAccess username owner (Some Authorization.Read)
    let! permission = Authorization.grantedPermission username owner
    Expect.equal permission (Some Authorization.Read) "read access"

    do! Authorization.setUserAccess username owner (Some Authorization.ReadWrite)
    let! permission = Authorization.grantedPermission username owner
    Expect.equal permission (Some Authorization.ReadWrite) "write access"

    do! Authorization.setUserAccess username owner None
    let! permission = Authorization.grantedPermission username owner
    Expect.equal permission None "revoekd access"
  }


let testPermissionMax =
  test "permissionMax" {
    Expect.equal (Authorization.Read.max (Authorization.Read)) Authorization.Read ""

    Expect.equal
      (Authorization.Read.max (Authorization.ReadWrite))
      Authorization.ReadWrite
      ""

    Expect.equal
      (Authorization.ReadWrite.max (Authorization.Read))
      Authorization.ReadWrite
      ""

    Expect.equal
      (Authorization.ReadWrite.max (Authorization.ReadWrite))
      Authorization.ReadWrite
      ""
  }

let tests = testList "Authorization" [ testPermissionMax; testSetUserAccess ]
