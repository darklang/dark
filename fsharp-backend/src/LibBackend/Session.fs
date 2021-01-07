module LibBackend.Session

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Npgsql.FSharp.Tasks
open Npgsql

open Prelude
open Prelude.Tablecloth
open Db

type JsonData = { username : string; csrfToken : string }
type T = { username : string; csrfToken : string; expiry : System.DateTime }

let get (key : byte []) : Task<Option<T>> =
  let keyAsString = System.Text.Encoding.UTF8.GetString key

  Sql.query
    "SELECT expire_date, session_data
     FROM session
     WHERE session_key = @key"
  |> Sql.parameters [ "key", Sql.string keyAsString ]
  |> Sql.executeRowOptionAsync
       (fun read ->
         let serializedData = read.string "session_data"
         let date = read.dateTime "expire_date"
         let data = Json.AutoSerialize.deserialize<JsonData> serializedData
         { username = data.username; expiry = date; csrfToken = data.csrfToken })

//   let random_string (len : int) : string =
//     Cstruct.to_string
//       (let open Nocrypto in
//       Base64.encode (Rng.generate len))
//
//
//   (* We store two values alongside each other in the session.value: one, the
//    * username; and two, the current CSRF token. These are stored as a JSON map
//    * with values "username" and "csrf_token". *)
//   let session_data (username : string) : string =
//     Yojson.to_string
//       (`Assoc
//         [ ("username", `String username)
//           (* Generate a random CSRF token the same way Session
//                does internally *)
//         ; ("csrf_token", `String (random_string 30)) ])
//
//
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
