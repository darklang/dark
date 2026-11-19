/// The tokens a sync server accepts, one person each (`dark sync token`). Only a token's SHA-256
/// is kept, so the store holds nothing that lets anyone in.
module LibDB.SyncTokens

open Microsoft.Data.Sqlite

open Prelude

/// Who a token speaks for.
type Account = { person : string; readOnly : bool; tokenId : string }

/// One token, as `dark sync token list` shows it. Never the token itself.
type Token =
  { id : string
    person : string
    readOnly : bool
    label : string
    createdAt : string
    lastUsedAt : Option<string>
    revokedAt : Option<string> }

/// Why a token was not taken.
type Refusal =
  | Unknown
  | Revoked of person : string

let private openStore (path : string) : SqliteConnection =
  let conn = new SqliteConnection($"Data Source={path};Pooling=false")
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- "PRAGMA busy_timeout=5000"
  cmd.ExecuteNonQuery() |> ignore<int>
  conn

let private command
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<string * obj>)
  : SqliteCommand =
  let cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  for (name, value) in ps do
    cmd.Parameters.AddWithValue(name, value) |> ignore<SqliteParameter>
  cmd

let private hashOf (token : string) : string =
  System.Security.Cryptography.SHA256.HashData(System.Text.Encoding.UTF8.GetBytes token)
  |> System.Convert.ToHexStringLower

/// A person's name as a token may carry it: letters, digits, `.`, `_` and `-`.
let validPerson (person : string) : bool =
  person <> ""
  && person.Length <= 64
  && person |> Seq.forall (fun c -> System.Char.IsAsciiLetterOrDigit c || c = '.' || c = '_' || c = '-')

/// A new token: 32 random bytes, so guessing one is not a plan.
let generate () : string =
  let bytes = System.Security.Cryptography.RandomNumberGenerator.GetBytes 32
  "dsync_"
  + (System.Convert.ToBase64String bytes).TrimEnd('=').Replace('+', '-').Replace('/', '_')

/// Keep <param token> as one of <param person>'s, and answer its id. Adding a token already kept
/// changes nothing, so a server's bootstrap token can be added on every start.
let add
  (storePath : string)
  (person : string)
  (token : string)
  (readOnly : bool)
  (label : string)
  : string =
  use conn = openStore storePath
  let hash = hashOf token
  let id = hash.Substring(0, 8)
  use cmd =
    command
      conn
      "INSERT INTO sync_tokens (hash, id, person, read_only, label, created_at)
       VALUES ($hash, $id, $person, $ro, $label, strftime('%Y-%m-%dT%H:%M:%SZ','now'))
       ON CONFLICT(hash) DO NOTHING"
      [ "$hash", box hash
        "$id", box id
        "$person", box person
        "$ro", box (if readOnly then 1 else 0)
        "$label", box label ]
  cmd.ExecuteNonQuery() |> ignore<int>
  id

/// Every token the store at <param storePath> knows, newest first.
let list (storePath : string) : List<Token> =
  use conn = openStore storePath
  use cmd =
    command
      conn
      "SELECT id, person, read_only, label, created_at, last_used_at, revoked_at
       FROM sync_tokens ORDER BY created_at DESC, id"
      []
  use reader = cmd.ExecuteReader()
  let text (i : int) = if reader.IsDBNull i then None else Some(reader.GetString i)
  [ while reader.Read() do
      yield
        { id = reader.GetString 0
          person = reader.GetString 1
          readOnly = reader.GetInt64 2 <> 0L
          label = reader.GetString 3
          createdAt = reader.GetString 4
          lastUsedAt = text 5
          revokedAt = text 6 } ]

/// Revoke the token with id <param id>, or every token of the person <param id> names. Answers the
/// tokens it revoked; a revoked token stays listed.
let revoke (storePath : string) (idOrPerson : string) : List<Token> =
  let matching =
    list storePath
    |> List.filter (fun t ->
      Option.isNone t.revokedAt && (t.id = idOrPerson || t.person = idOrPerson))
  use conn = openStore storePath
  for t in matching do
    use cmd =
      command
        conn
        "UPDATE sync_tokens SET revoked_at = strftime('%Y-%m-%dT%H:%M:%SZ','now')
         WHERE id = $id AND revoked_at IS NULL"
        [ "$id", box t.id ]
    cmd.ExecuteNonQuery() |> ignore<int>
  matching

/// Who <param token> speaks for, if the store at <param storePath> takes it. Notes when it was used.
let check (storePath : string) (token : string) : Result<Account, Refusal> =
  use conn = openStore storePath
  let hash = hashOf token
  use cmd =
    command
      conn
      "SELECT id, person, read_only, revoked_at FROM sync_tokens WHERE hash = $hash"
      [ "$hash", box hash ]
  let found =
    use reader = cmd.ExecuteReader()
    if reader.Read() then
      Some(
        reader.GetString 0,
        reader.GetString 1,
        reader.GetInt64 2 <> 0L,
        reader.IsDBNull 3 |> not
      )
    else
      None
  match found with
  | None -> Error Unknown
  | Some(_, person, _, true) -> Error(Revoked person)
  | Some(id, person, readOnly, false) ->
    use used =
      command
        conn
        "UPDATE sync_tokens SET last_used_at = strftime('%Y-%m-%dT%H:%M:%SZ','now')
         WHERE hash = $hash"
        [ "$hash", box hash ]
    used.ExecuteNonQuery() |> ignore<int>
    Ok { person = person; readOnly = readOnly; tokenId = id }

/// Whether the token with id <param id> is still taken.
let isLive (storePath : string) (id : string) : bool =
  try
    use conn = openStore storePath
    use cmd =
      command
        conn
        "SELECT COUNT(*) FROM sync_tokens WHERE id = $id AND revoked_at IS NULL"
        [ "$id", box id ]
    unbox<int64> (cmd.ExecuteScalar()) > 0L
  with _ ->
    true
