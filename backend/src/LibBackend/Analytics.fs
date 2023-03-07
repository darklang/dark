/// Analytics functions for users
module LibBackend.Analytics

module Account = LibBackend.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module FireAndForget = LibService.FireAndForget

type HeapIOMetadata = Map<string, string>

// We call this in two contexts: DarkInternal:: fns
let identifyUser (username : UserName.T) : unit =
  FireAndForget.fireAndForgetTask "identify user" (fun () ->
    task {
      let! data = Account.getUserAndCreatedAt username
      let userInfoAndCreatedAt =
        Exception.unwrapOptionInternal
          "unwrapping metadata"
          [ "metadata", data ]
          data

      let payload =
        [ ("username", string userInfoAndCreatedAt.username)
          ("email", userInfoAndCreatedAt.email)
          ("name", userInfoAndCreatedAt.name)
          ("admin", string userInfoAndCreatedAt.admin)
          ("handle", string userInfoAndCreatedAt.username) ]

      let payload = Map payload
      LibService.HeapAnalytics.emitIdentifyUserEvent userInfoAndCreatedAt.id payload
      return ()
    })
