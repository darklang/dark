module LibBackend.Authorization

// Permission levels, scoped to a single owner.

// These are stored in the datastore for granted access,
// or returned by the functions in this namespace to indicate a
// particular user's permissions for a particular auth_domain.
//
// We often use `permission option' to include the case where
// a user has no special access to a particular auth_domain.

module Account = LibBackend.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth

type Permission =
  | Read
  | ReadWrite

  member this.max(other : Permission) : Permission =
    match this, other with
    | ReadWrite, _
    | _, ReadWrite -> ReadWrite
    | Read, _
    | _, Read -> Read

  static member parse(str : string) : Permission =
    match str with
    | "r" -> Read
    | "rw" -> ReadWrite
    | _ -> Exception.raiseInternal "couldn't decode permission" [ "permission", str ]

  override this.ToString() : string =
    match this with
    | Read -> "r"
    | ReadWrite -> "rw"

let setUserAccess
  (username : UserName.T)
  (orgName : OwnerName.T)
  (p : Option<Permission>)
  : Task<unit> =
  match p with
  | None ->
    Sql.query
      "DELETE FROM access AS a
         USING accounts AS u, accounts AS o
         WHERE o.username = @orgName
           AND u.username = @username
           AND a.access_account = u.id
           AND a.organization_account = o.id"
    |> Sql.parameters [ "username", username |> string |> Sql.string
                        "orgName", orgName |> string |> Sql.string ]
    |> Sql.executeStatementAsync
  | Some p ->
    Sql.query
      "INSERT into access
         (access_account, organization_account, permission)
         SELECT u.id, o.id, @permission
         FROM accounts u, accounts o
         WHERE o.username = @orgName
           AND u.username = @username
         ON CONFLICT (access_account, organization_account) DO UPDATE SET permission = EXCLUDED.permission"
    |> Sql.parameters [ "username", username |> string |> Sql.string
                        "orgName", orgName |> string |> Sql.string
                        "permission", p |> string |> Sql.string ]
    |> Sql.executeStatementAsync


/// Returns a list of (username, permission) pairs for a given auth_domain,
/// denoting who has been granted access to a given domain
let grantsFor (ownerName : OwnerName.T) : Task<List<UserName.T * Permission>> =
  Sql.query
    "SELECT user_.username, permission FROM access
     INNER JOIN accounts user_ on access.access_account = user_.id
     INNER JOIN accounts org on access.organization_account = org.id
     WHERE org.username = @username"
  |> Sql.parameters [ "username", Sql.string (string ownerName) ]
  |> Sql.executeAsync (fun read ->
    read.string "username" |> UserName.create,
    read.string "permission" |> Permission.parse)


/// Returns a list of (organization name, permission) pairs for a given username,
/// denoting which organizations the user has been granted permissions towards
let orgsFor (username : UserName.T) : Task<List<OrgName.T * Permission>> =
  Sql.query
    "SELECT org.username, permission
     FROM access
     INNER JOIN accounts user_ on access.access_account = user_.id
     INNER JOIN accounts org on access.organization_account = org.id
     WHERE user_.username = @username"
  |> Sql.parameters [ "username", Sql.string (string username) ]
  |> Sql.executeAsync (fun read ->
    read.string "username" |> OrgName.create,
    read.string "permission" |> Permission.parse)

/// If a user has a DB row indicating granted access to this auth_domain,
/// find it.
let grantedPermission
  (username : UserName.T)
  (ownerName : OwnerName.T)
  : Task<Option<Permission>> =
  Sql.query
    "SELECT permission FROM access
     INNER JOIN accounts user_ ON access.access_account = user_.id
     INNER JOIN accounts org ON access.organization_account = org.id
     WHERE org.username = @ownerName
       AND user_.username = @username"
  |> Sql.parameters [ "username", Sql.string (username |> string)
                      "ownerName", Sql.string (ownerName |> string) ]
  |> Sql.executeRowOptionAsync (fun read ->
    read.string "permission" |> Permission.parse)


/// If a user is an admin they get write on everything.
let adminPermission (username : UserName.T) : Task<Option<Permission>> =
  task {
    let! isAdmin = Account.isAdmin username
    if isAdmin then return Some ReadWrite else return None
  }

// We special-case some users, so they have access to particular shared canvases
let specialCases : List<OwnerName.T * UserName.T> =
  [ OwnerName.create "pixelkeet", UserName.create "laxels"
    OwnerName.create "rootvc", UserName.create "adam"
    OwnerName.create "rootvc", UserName.create "lee"
    OwnerName.create "talkhiring", UserName.create "harris"
    OwnerName.create "talkhiring", UserName.create "anson" ]


let specialCasePermission
  (username : UserName.T)
  (ownerName : OwnerName.T)
  : Option<Permission> =
  if List.any ((=) (ownerName, username)) specialCases then
    Some ReadWrite
  else
    None

// People should have access to the canvases under their name
let matchPermission
  (username : UserName.T)
  (ownerName : OwnerName.T)
  : Option<Permission> =
  if string username = string ownerName then Some ReadWrite else None

// Everyone should have read-access to 'sample'.
let samplePermission (owner : OwnerName.T) : Option<Permission> =
  if "sample" = (string owner) then Some Read else None

// What's the highest level of access a particular user has to a
// particular user's canvas
let permission
  (owner : OwnerName.T)
  (username : UserName.T)
  : Task<Option<Permission>> =
  let permFs : List<unit -> Task<Option<Permission>>> =
    [ (fun () -> task { return matchPermission username owner })
      // TODO CLEANUP: remove specialCasePermission
      (fun () -> task { return specialCasePermission username owner })
      (fun () -> task { return samplePermission owner })
      (fun () -> task { return! grantedPermission username owner })
      (fun () -> task { return! adminPermission username }) ]
      
  // Return the greatest `permission option` of a set of functions producing
  // `permission option`, lazily, so we don't hit the db unnecessarily
  List.fold
    (task { return None })
    (fun (p : Task<Option<Permission>>) (f : unit -> Task<Option<Permission>>) ->
      task {
        match! p with
        | Some ReadWrite -> return Some ReadWrite
        | Some older ->
          match! f () with
          | Some newer -> return Some(older.max (newer))
          | None -> return! p
        | None -> return! f ()
      })
    permFs

let permitted
  (neededPermission : Permission)
  (actualPermission : Option<Permission>)
  : bool =
  match neededPermission, actualPermission with
  | ReadWrite, Some ReadWrite -> true
  | ReadWrite, Some Read -> false
  | ReadWrite, None -> false
  | Read, Some ReadWrite -> true
  | Read, Some Read -> true
  | Read, None -> false
