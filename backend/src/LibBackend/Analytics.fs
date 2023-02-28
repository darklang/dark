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
      let! orgs = Authorization.orgsFor username
      let organization =
        // A user's orgs for this purpose do not include orgs it has
        // read-only access to
        orgs
        |> List.filter (fun (_orgName, perm) -> perm = Authorization.ReadWrite)
        // If you have one org, that's your org! If you have no orgs, or
        // more than one, then we just use your username. This is because
        // Heap's properties/traits don't support lists.
        |> (fun org ->
          match org with
          | [ (orgName, _perm) ] -> orgName
          | _ -> username |> string |> OrgName.create)

      let payload =
        [ ("username", string userInfoAndCreatedAt.username)
          ("email", userInfoAndCreatedAt.email)
          ("name", userInfoAndCreatedAt.name)
          ("admin", string userInfoAndCreatedAt.admin)
          ("handle", string userInfoAndCreatedAt.username)
          ("organization", string organization) ]

      let payload = Map payload
      LibService.HeapAnalytics.emitIdentifyUserEvent userInfoAndCreatedAt.id payload
      return ()
    })
