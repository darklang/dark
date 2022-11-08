module LibBackend.Session

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude
open Tablecloth
open Db

type JsonData = { username : string; csrf_token : string }

type T =
  { username : UserName.T
    csrfToken : string
    expiry : NodaTime.Instant
    key : string }

type AuthData = { csrfToken : string; sessionKey : string }

let cookieKey = "__session"
let csrfHeader = "X-CSRF-Token"

/// <summary> Get the sessionData with no CSRF. </summary>
///
/// <remarks>
/// By default, you should be using CSRF - the only exception is for GETs
/// </remarks>
let getNoCSRF (key : string) : Task<Option<T>> =
  Sql.query
    "SELECT expire_date, session_data
     FROM session
     WHERE session_key = @key"
  |> Sql.parameters [ "key", Sql.string key ]
  |> Sql.executeRowOptionAsync (fun read ->
    let serializedData = read.string "session_data"
    let date = read.instant "expire_date"
    let data = Json.Vanilla.deserialize<JsonData> serializedData

    { username = UserName.create data.username
      expiry = date
      csrfToken = data.csrf_token
      key = key })

/// Get the sessionData
let get (key : string) (csrfToken : string) : Task<Option<T>> =
  getNoCSRF key
  |> Task.map (
    Option.bind (fun sd -> if sd.csrfToken <> csrfToken then None else Some sd)
  )


/// Creates a session in the DB, returning a new session key
/// and new CSRF token to be returned to the user
let insert (username : UserName.T) : Task<AuthData> =
  task {
    let key = randomString 40
    let csrfToken = randomString 40

    let sessionData =
      Json.Vanilla.serialize { username = string username; csrf_token = csrfToken }

    do!
      Sql.query
        "INSERT INTO session
         (session_key, expire_date, session_data)
         VALUES (@key, now() + interval '2 weeks', @sessionData)"
      |> Sql.parameters [ "key", Sql.string key
                          "sessionData", Sql.string sessionData ]
      |> Sql.executeStatementAsync

    return { csrfToken = csrfToken; sessionKey = key }
  }

let clear (key : string) : Task<unit> =
  Sql.query
    "DELETE
     FROM session
     WHERE session_key = @key"
  |> Sql.parameters [ "key", Sql.string key ]
  |> Sql.executeStatementAsync
