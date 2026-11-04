/// Reads over `locations`, the name -> hash bindings the fold projects for MAIN,
/// for the Dark SCM code. One function, one query family: `query` takes what is
/// being asked and answers with bindings.
///
/// Each case is the SQL Dark used to run itself, moved across as it was: the same
/// statement, the same ordering, the same scope. Nothing here decides anything.
/// `locations` only ever holds main's bindings (a branch's SetNames never fold into
/// it), so every case answers about MAIN, and the Dark side lays the branch chain
/// over the answer exactly as before.
///
/// Rows the Dark decoder used to drop (a NULL where it wanted text) are dropped
/// here, and the columns it read a NULL as "" still read as "". So the Dark side
/// sees the same rows it always saw.
module LibDB.ScmBindings

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite


/// A name as `locations` stores it: modules joined by ".".
type Name = { owner : string; modules : string; name : string }

/// What is being asked. One case per question the Dark side asks.
type Query =
  /// One name main binds this hash at, if any.
  | MainLocationOf of hash : string
  /// Every live binding on main whose bare name is one of these, whatever its
  /// owner and modules.
  | MainByBareNames of names : List<string>
  /// What main binds at each of these names, draft included.
  | MainAtEach of List<Name>
  /// The hash main binds at this name for this kind, if any.
  | MainHashAt of kind : string * Name
  /// Every live binding on main that propagation wrote.
  | Propagated
  /// Every hash this name was bound to by an op or by propagation, oldest first,
  /// live or not.
  | AuthoringOrder of Name
  /// Every hash ever bound at each of these (name, kind) pairs, unbinds left out,
  /// the live one first.
  | EverBound of List<Name * string>
  /// Every name main binds this content at.
  | MainByHash of hash : string * kind : string
  /// What main binds, committed work only, at each name this branch recorded a
  /// base for.
  | CommittedAtBases of branchId : string


/// One answer row. Which fields are filled depends on the query; the rest are ""
/// (or None).
type Binding =
  {
    owner : string
    modules : string
    name : string
    itemType : string
    hash : string
    /// The `origin_ts` of the op that bound it; "" when none was recorded.
    originTs : string
    /// The hash this binding replaced, when the op said.
    previous : Option<string>
  }

let private empty : Binding =
  { owner = ""
    modules = ""
    name = ""
    itemType = ""
    hash = ""
    originTs = ""
    previous = None }


let private run
  (sql : string)
  (parameters : List<string * Microsoft.Data.Sqlite.SqliteParameter>)
  (read : RowReader -> Option<Binding>)
  : Task<List<Binding>> =
  task {
    let! rows = Sql.query sql |> Sql.parameters parameters |> Sql.executeAsync read
    return List.choose identity rows
  }

let private nameParams
  (n : Name)
  : List<string * Microsoft.Data.Sqlite.SqliteParameter> =
  [ "owner", Sql.string n.owner
    "modules", Sql.string n.modules
    "name", Sql.string n.name ]

// JSON for `json_each` to bind. Written with the writer, never the reflecting serializer, which
// the published (AOT) binary does not have.
let private writeJson (f : System.Text.Json.Utf8JsonWriter -> unit) : string =
  use buffer = new System.IO.MemoryStream()
  (use w = new System.Text.Json.Utf8JsonWriter(buffer)
   f w)
  System.Text.Encoding.UTF8.GetString(buffer.ToArray())

let private writeStrings
  (w : System.Text.Json.Utf8JsonWriter)
  (items : List<string>)
  =
  w.WriteStartArray()
  items |> List.iter (fun (s : string) -> w.WriteStringValue s)
  w.WriteEndArray()

let private jsonList (items : List<string>) : string =
  writeJson (fun w -> writeStrings w items)

let private json (rows : List<List<string>>) : string =
  writeJson (fun w ->
    w.WriteStartArray()
    rows |> List.iter (writeStrings w)
    w.WriteEndArray())

let private namesJson (names : List<Name>) : string =
  names |> List.map (fun n -> [ n.owner; n.modules; n.name ]) |> json

let private orEmpty (read : RowReader) (col : string) : string =
  read.stringOrNone col |> Option.defaultValue ""

/// The full binding shape the Dark decoder read: owner, modules, name and hash
/// required, a missing kind or stamp read as "", `previous` as it is.
let private fullBinding (read : RowReader) : Option<Binding> =
  match
    read.stringOrNone "owner",
    read.stringOrNone "modules",
    read.stringOrNone "name",
    read.stringOrNone "item_hash"
  with
  | Some o, Some m, Some n, Some h ->
    Some
      { owner = o
        modules = m
        name = n
        itemType = orEmpty read "item_type"
        hash = h
        originTs = orEmpty read "ts"
        previous = read.stringOrNone "previous" }
  | _ -> None

/// Owner, modules and name, all required.
let private nameOnly (read : RowReader) : Option<Binding> =
  match
    read.stringOrNone "owner", read.stringOrNone "modules", read.stringOrNone "name"
  with
  | Some o, Some m, Some n -> Some { empty with owner = o; modules = m; name = n }
  | _ -> None


let query (q : Query) : Task<List<Binding>> =
  match q with
  | MainLocationOf hash ->
    run
      "SELECT owner, modules, name FROM locations
       WHERE item_hash = @hash AND unlisted_at IS NULL LIMIT 1"
      [ "hash", Sql.string hash ]
      (fun read -> nameOnly read |> Option.map (fun b -> { b with hash = hash }))

  | MainByBareNames names ->
    if List.isEmpty names then
      Task.FromResult []
    else
      // One statement over a JSON list, where Dark built an IN-list and split it
      // into chunks of 900 to stay under SQLite's bound-parameter cap.
      run
        "SELECT owner, modules, name, item_type, item_hash, COALESCE(origin_ts,'') AS ts,
                previous
         FROM locations
         WHERE unlisted_at IS NULL AND name IN (SELECT value FROM json_each(@names))"
        [ "names", Sql.string (jsonList names) ]
        fullBinding

  | MainAtEach names ->
    if List.isEmpty names then
      Task.FromResult []
    else
      run
        "SELECT l.owner, l.modules, l.name, l.item_type, l.item_hash,
                COALESCE(l.origin_ts,'') AS ts, l.previous
         FROM json_each(@names) j
         JOIN locations l
           ON l.owner = json_extract(j.value, '$[0]')
          AND l.modules = json_extract(j.value, '$[1]')
          AND l.name = json_extract(j.value, '$[2]')
         WHERE l.unlisted_at IS NULL"
        [ "names", Sql.string (namesJson names) ]
        fullBinding

  | MainHashAt(kind, n) ->
    run "SELECT item_hash FROM locations
       WHERE owner = @owner AND modules = @modules AND name = @name
         AND item_type = @kind AND unlisted_at IS NULL
       LIMIT 1" (("kind", Sql.string kind) :: nameParams n) (fun read ->
      read.stringOrNone "item_hash"
      |> Option.map (fun h ->
        { empty with
            owner = n.owner
            modules = n.modules
            name = n.name
            itemType = kind
            hash = h }))

  | Propagated ->
    // The name is required; a missing owner or modules read as "", as the Dark
    // decoder did.
    run "SELECT owner, modules, name FROM locations
        WHERE source = 'propagation' AND unlisted_at IS NULL" [] (fun read ->
      read.stringOrNone "name"
      |> Option.map (fun nm ->
        { empty with
            owner = orEmpty read "owner"
            modules = orEmpty read "modules"
            name = nm }))

  | AuthoringOrder n ->
    run "SELECT item_hash FROM locations
       WHERE owner = @owner AND modules = @modules AND name = @name
         AND source IN ('op', 'propagation')
       ORDER BY rowid" (nameParams n) (fun read ->
      read.stringOrNone "item_hash"
      |> Option.map (fun h ->
        { empty with
            owner = n.owner
            modules = n.modules
            name = n.name
            hash = h }))

  | EverBound keys ->
    if List.isEmpty keys then
      Task.FromResult []
    else
      let keysJson =
        keys
        |> List.map (fun (n, kind) -> [ n.owner; n.modules; n.name; kind ])
        |> json
      run
        "SELECT l.owner, l.modules, l.name, l.item_type, l.item_hash
         FROM json_each(@keys) j
         JOIN locations l
           ON l.owner = json_extract(j.value, '$[0]')
          AND l.modules = json_extract(j.value, '$[1]')
          AND l.name = json_extract(j.value, '$[2]')
          AND l.item_type = json_extract(j.value, '$[3]')
         WHERE l.source <> 'unbind'
         GROUP BY l.owner, l.modules, l.name, l.item_type, l.item_hash
         ORDER BY l.owner, l.modules, l.name, l.item_type,
           MAX(CASE WHEN l.unlisted_at IS NULL THEN '9999-12-31' ELSE l.unlisted_at END) DESC"
        [ "keys", Sql.string keysJson ]
        (fun read ->
          match
            nameOnly read,
            read.stringOrNone "item_type",
            read.stringOrNone "item_hash"
          with
          | Some b, Some k, Some h -> Some { b with itemType = k; hash = h }
          | _ -> None)

  | MainByHash(hash, kind) ->
    run
      "SELECT owner, modules, name FROM locations
       WHERE item_hash = @hash AND item_type = @kind AND unlisted_at IS NULL"
      [ "hash", Sql.string hash; "kind", Sql.string kind ]
      (fun read ->
        nameOnly read
        |> Option.map (fun b -> { b with itemType = kind; hash = hash }))

  | CommittedAtBases branchId ->
    run
      "SELECT l.owner, l.modules, l.name, l.item_hash
       FROM branch_name_bases b
       JOIN locations l
         ON l.owner = b.owner AND l.modules = b.modules AND l.name = b.name
       JOIN package_ops p ON p.id = l.op_id
       WHERE b.branch_id = @branch AND l.unlisted_at IS NULL
         AND p.commit_hash IS NOT NULL"
      [ "branch", Sql.string branchId ]
      (fun read ->
        match nameOnly read, read.stringOrNone "item_hash" with
        | Some b, Some h -> Some { b with hash = h }
        | _ -> None)
