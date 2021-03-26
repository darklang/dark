module LibBackend.Session

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp.Tasks
open Npgsql

open Prelude
open Tablecloth
open Db

type JsonData = { username : string; csrf_token : string }

type T =
  { username : string
    csrfToken : string
    expiry : System.DateTime
    key : string }

type AuthData = { csrfToken : string; sessionKey : string }

let cookieKey = "__session"
let csrfHeader = "X-CSRF-Token"

// Get the sessionData with no CSRF. By default you should be using CSRF, the only exception is for GETs
let getNoCSRF (key : string) : Task<Option<T>> =
  Sql.query
    "SELECT expire_date, session_data
     FROM session
     WHERE session_key = @key"
  |> Sql.parameters [ "key", Sql.string key ]
  |> Sql.executeRowOptionAsync
       (fun read ->
         let serializedData = read.string "session_data"
         let date = read.dateTime "expire_date"
         let data = Json.Vanilla.deserialize<JsonData> serializedData

         { username = data.username
           expiry = date
           csrfToken = data.csrf_token
           key = key })

// Get the sessionData
let get (key : string) (csrfToken : string) : Task<Option<T>> =
  getNoCSRF key
  |> Task.map (
    Option.bind (fun sd -> if sd.csrfToken <> csrfToken then None else Some sd)
  )


// Creates a session in the DB, returning a new session key and new CSRF token
// to be returned to the user
let insert (username : string) : Task<AuthData> =
  task {
    let key = randomString 40
    let csrfToken = randomString 40
    let expiryDate = System.DateTime.Now

    let sessionData =
      Json.Vanilla.serialize { username = username; csrf_token = csrfToken }

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


//   let username_of_key (key : string) : string option =
//     Db.fetch_one_option
//       ~name:"username_of_key"
//       "SELECT session_data
//        FROM session
//        WHERE expire_date > NOW() AND session_key = $1"
//       ~params:[Db.String key]
//     |> Option.bind ~f:(fun row -> row |> List.hd)
//     |> Option.map ~f:(fun session_data ->
//            session_data
//            |> Yojson.Basic.from_string
//            |> Yojson.Basic.Util.member "username"
//            |> Yojson.Basic.Util.to_string)
//
// let of_request req = ""
// of_header backend cookie_key (req |> Cohttp_lwt_unix.Request.headers)
//
//
//   let new_for_username username = generate backend (session_data username)

// let getUserName (x : int) : string = "todo"

//   let username_for session =
//     session.value
//     |> Yojson.Basic.from_string
//     |> Yojson.Basic.Util.member "username"
//     |> Yojson.Basic.Util.to_string
//
//
//   let csrf_token_for session =
//     session.value
//     |> Yojson.Basic.from_string
//     |> Yojson.Basic.Util.member "csrf_token"
//     |> Yojson.Basic.Util.to_string
//
