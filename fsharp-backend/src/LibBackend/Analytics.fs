/// Analytics functions for users
module LibBackend.Analytics

module Account = LibBackend.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module FireAndForget = LibService.FireAndForget


// We call this in two contexts: DarkInternal:: fns
let identifyUser (username : UserName.T) : unit =
  FireAndForget.fireAndForgetTask "identify user" (fun () ->
    task {
      let! data = Account.getUserAndCreatedAtAndAnalyticsMetadata username
      let (userInfoAndCreatedAt, heapioMetadataJson) =
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

      let heapioMetadata =
        heapioMetadataJson
        |> Option.map Json.Vanilla.deserialize<Map<string, string>>
        |> Option.defaultValue Map.empty

      let payload =
        [ ("username", string userInfoAndCreatedAt.username)
          ("email", userInfoAndCreatedAt.email)
          ("name", userInfoAndCreatedAt.name)
          ("admin", string userInfoAndCreatedAt.admin)
          ("handle", string userInfoAndCreatedAt.username)
          ("organization", string organization) ]

      // We do zero checking of fields in heapio_metadata, but this is ok
      // because it's a field we control, going to a service only we see.
      // If we wanted to harden this later, we could List.filter the
      // heapio_metadata yojson
      let payload = Map(payload @ Map.toList heapioMetadata)
      LibService.HeapAnalytics.emitIdentifyUserEvent userInfoAndCreatedAt.id payload
      return ()
    })

let init () = Json.Vanilla.allow<Map<string, string>> "heapioMetadata"
