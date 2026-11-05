/// Reads over `locations`, the name -> hash bindings, for the Dark SCM code. One function, one query
/// family: `query` takes what is being asked and answers with bindings.
///
/// Every case names the branch it asks about and answers as that branch SEES its names
/// (`LibDB.Layers`): its own work over what its ancestors committed. Main is a branch like any
/// other.
module LibDB.ScmBindings

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes


/// A name as `locations` stores it: modules joined by ".".
type Name = { owner : string; modules : string; name : string }

/// What is being asked. One case per question the Dark side asks.
type Query =
  /// One name the branch binds this hash at, if any.
  | LocationOf of branch : PT.BranchId * hash : string
  /// Every binding the branch sees whose bare name is one of these, whatever its owner and modules.
  | ByBareNames of branch : PT.BranchId * names : List<string>
  /// What the branch binds at each of these names; a name bound to nothing is absent.
  | At of branch : PT.BranchId * names : List<Name>
  /// `At`, in the branch's committed view: its draft left out.
  | CommittedAt of branch : PT.BranchId * names : List<Name>
  /// Every binding the branch's own work holds that propagation wrote: on a branch, its own followers,
  /// not main's.
  | Propagated of branch : PT.BranchId
  /// Every hash this name was bound to by an op or by propagation in the branch's view, oldest
  /// first, live or not.
  | AuthoringOrder of branch : PT.BranchId * Name
  /// Every hash ever bound at each of these (name, kind) pairs in the branch's view, removals left
  /// out, the live one first.
  | EverBound of branch : PT.BranchId * List<Name * string>
  /// Every name the branch binds this content at.
  | ByHash of branch : PT.BranchId * hash : string * kind : string
  /// Every name the branch binds each of these hashes at.
  | ByHashes of branch : PT.BranchId * hashes : List<string>
  /// What the branch's own work last said about each name it touched, removals included (hash
  /// ""): the side it brings to a merge. Nothing for main, which is the trunk.
  | Own of branch : PT.BranchId
  /// Every hash the branch and its ancestors below main bound by an op or by propagation, per name,
  /// oldest first: a branch's own authoring history, which main's rows do not hold.
  | BranchHistory of branch : PT.BranchId
  /// The names the branch's view shows as removed by a branch below main -- the branch itself or an
  /// ancestor -- with the removal's stamp. Main's own removals are not among them: a merge compares
  /// against those as main's bindings, not as removals.
  | RemovedOffMain of branch : PT.BranchId
  /// What the branch sees BENEATH itself -- its ancestors' committed work -- at each name it
  /// recorded a base for; a name bound to nothing there reads hash "".
  | AboveBases of branch : PT.BranchId


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

let private namesJson (names : List<Name>) : string =
  names |> List.map (fun n -> [ n.owner; n.modules; n.name ]) |> jsonRows

let private orEmpty (read : RowReader) (col : string) : string =
  read.stringOrNone col |> Option.defaultValue ""

/// A whole binding: owner, modules, name and hash
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


let private visibleSql (select : string) : string =
  Layers.view "@" Layers.locations + " " + select

let private fullCols =
  "owner, modules, name, item_type, item_hash, COALESCE(origin_ts,'') AS ts, previous"

let private view (b : PT.BranchId) = Layers.sqlParams b true

let query (q : Query) : Task<List<Binding>> =
  match q with
  | LocationOf(b, hash) ->
    run
      // Main's name first: callers render it as a label, and identical content is one item.
      (visibleSql
        "SELECT owner, modules, name FROM visible WHERE item_hash = @hash
         ORDER BY (branch_id = @vm) DESC, seq DESC LIMIT 1")
      (("hash", Sql.string hash) :: view b)
      (fun read -> nameOnly read |> Option.map (fun r -> { r with hash = hash }))

  | ByBareNames(b, names) ->
    if List.isEmpty names then
      Task.FromResult []
    else
      run
        (visibleSql (
          $"SELECT {fullCols} FROM visible
            WHERE item_hash <> '' AND name IN (SELECT value FROM json_each(@names))"
        ))
        (("names", Sql.string (jsonStrings names)) :: view b)
        fullBinding

  | At(b, names)
  | CommittedAt(b, names) ->
    let viewOf =
      match q with
      | CommittedAt _ -> Layers.committedSqlParams b
      | _ -> view b
    if List.isEmpty names then
      Task.FromResult []
    else
      run
        (visibleSql (
          // From the names, then their rows: joined the other way round, SQLite builds the whole view
          // and filters it.
          $"SELECT {fullCols} FROM json_each(@names) j
            CROSS JOIN visible l
              ON l.owner = json_extract(j.value, '$[0]')
             AND l.modules = json_extract(j.value, '$[1]')
             AND l.name = json_extract(j.value, '$[2]')
            WHERE l.item_hash <> ''"
        ))
        (("names", Sql.string (namesJson names)) :: viewOf)
        fullBinding

  | Propagated b ->
    run
      (visibleSql
        "SELECT owner, modules, name FROM visible
         WHERE source = 'propagation' AND item_hash <> '' AND branch_id = @vb")
      (view b)
      nameOnly

  | AuthoringOrder(b, n) ->
    run
      (Layers.cte "@"
       + " SELECT l.item_hash FROM locations l
           JOIN layer y ON y.branch_id = l.branch_id AND y.draft = l.draft
           WHERE l.owner = @owner AND l.modules = @modules AND l.name = @name
             AND l.source IN ('op', 'propagation') AND l.item_hash <> ''
           ORDER BY l.rowid")
      (nameParams n @ view b)
      (fun read ->
        read.stringOrNone "item_hash"
        |> Option.map (fun h ->
          { empty with
              owner = n.owner
              modules = n.modules
              name = n.name
              hash = h }))

  | EverBound(b, keys) ->
    if List.isEmpty keys then
      Task.FromResult []
    else
      let keysJson =
        keys
        |> List.map (fun (n, kind) -> [ n.owner; n.modules; n.name; kind ])
        |> jsonRows
      run
        (Layers.cte "@"
         + " SELECT l.owner, l.modules, l.name, l.item_type, l.item_hash
             FROM json_each(@keys) j
             JOIN locations l
               ON l.owner = json_extract(j.value, '$[0]')
              AND l.modules = json_extract(j.value, '$[1]')
              AND l.name = json_extract(j.value, '$[2]')
              AND l.item_type = json_extract(j.value, '$[3]')
             JOIN layer y ON y.branch_id = l.branch_id AND y.draft = l.draft
             WHERE l.item_hash <> ''
             GROUP BY l.owner, l.modules, l.name, l.item_type, l.item_hash
             ORDER BY l.owner, l.modules, l.name, l.item_type, MAX(l.live) DESC, MAX(l.rowid) DESC")
        (("keys", Sql.string keysJson) :: view b)
        (fun read ->
          match
            nameOnly read,
            read.stringOrNone "item_type",
            read.stringOrNone "item_hash"
          with
          | Some r, Some k, Some h -> Some { r with itemType = k; hash = h }
          | _ -> None)

  | ByHash(b, hash, kind) ->
    run
      (visibleSql
        "SELECT owner, modules, name FROM visible WHERE item_hash = @hash AND item_type = @kind")
      ([ "hash", Sql.string hash; "kind", Sql.string kind ] @ view b)
      (fun read ->
        nameOnly read
        |> Option.map (fun r -> { r with itemType = kind; hash = hash }))

  | ByHashes(b, hashes) ->
    if List.isEmpty hashes then
      Task.FromResult []
    else
      run
        (visibleSql
          "SELECT item_hash, owner, modules, name FROM visible
           WHERE item_hash IN (SELECT value FROM json_each(@hashes))")
        (("hashes", Sql.string (jsonStrings hashes)) :: view b)
        (fun read ->
          match nameOnly read, read.stringOrNone "item_hash" with
          | Some r, Some h -> Some { r with hash = h }
          | _ -> None)

  | Own b ->
    // The branch's own two layers only: its draft over its committed work.
    run
      $"SELECT {fullCols} FROM locations l
        WHERE l.branch_id = @b AND l.branch_id <> @main AND l.live = 1
          AND (l.draft = 1 OR NOT EXISTS (
            SELECT 1 FROM locations d
            WHERE d.branch_id = l.branch_id AND d.draft = 1 AND d.live = 1
              AND d.owner = l.owner AND d.modules = l.modules AND d.name = l.name))
        ORDER BY l.owner, l.modules, l.name"
      [ "b", Sql.string (string b); "main", Sql.string (string PT.BranchId.Main) ]
      (fun read ->
        match nameOnly read with
        | Some r ->
          Some
            { r with
                itemType = orEmpty read "item_type"
                hash = orEmpty read "item_hash"
                originTs = orEmpty read "ts"
                previous = read.stringOrNone "previous" }
        | None -> None)

  | BranchHistory b ->
    run
      (Layers.cte "@"
       + " SELECT l.owner, l.modules, l.name, l.item_hash FROM locations l
           JOIN layer y ON y.branch_id = l.branch_id AND y.draft = l.draft
           WHERE l.branch_id <> @vm AND l.source IN ('op', 'propagation') AND l.item_hash <> ''
           ORDER BY l.rowid")
      (view b)
      (fun read ->
        match nameOnly read with
        | Some r -> Some { r with hash = orEmpty read "item_hash" }
        | None -> None)

  | RemovedOffMain b ->
    run
      (visibleSql (
        $"SELECT {fullCols} FROM visible WHERE item_hash = '' AND branch_id <> @vm"
      ))
      (view b)
      (fun read ->
        nameOnly read
        |> Option.map (fun r ->
          { r with
              originTs = orEmpty read "ts"
              previous = read.stringOrNone "previous" }))

  | AboveBases b ->
    run
      (visibleSql
        "SELECT nb.owner, nb.modules, nb.name,
                COALESCE((SELECT v.item_hash FROM visible v
                          WHERE v.owner = nb.owner AND v.modules = nb.modules
                            AND v.name = nb.name), '') AS item_hash
         FROM branch_name_bases nb WHERE nb.branch_id = @vb")
      (Layers.sqlParams b false)
      (fun read ->
        match nameOnly read with
        | Some r -> Some { r with hash = orEmpty read "item_hash" }
        | None -> None)
