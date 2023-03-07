module Tests.Authorization

open Expecto
open Prelude
open TestUtils.TestUtils

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

  }
