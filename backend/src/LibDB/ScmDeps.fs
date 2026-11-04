/// Reads over `package_dependencies`, for the Dark SCM code (`SCM.Deps` and the modules that ask it
/// who references whom). One function, one query family: `query` takes what is being asked and
/// answers with edges.
///
/// Each case is the SQL Dark used to run itself, moved across as it was: the same statement, the
/// same ordering, the same scope. Nothing here decides anything. Main versus branch is still Dark's
/// call: the cases that join `locations` answer about MAIN (a branch has no rows there), and the
/// Dark side lays the branch overlay over them exactly as before.
///
/// Rows the Dark decoder used to drop (a NULL where it wanted text) are dropped here, and the
/// queries that read a NULL as "" still do. So the Dark side sees the same rows it always saw.
module LibDB.ScmDeps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite


/// A name as `package_dependencies` and `locations` store it: modules joined by ".".
type Name = { kind : string; owner : string; modules : string; name : string }

/// What is being asked. One case per question the Dark side asks.
type Query =
  /// What one item references, as distinct (hash, kind) pairs.
  | Of of hash : string
  /// What one item references, with the name each edge recorded. Edges with no name are left out.
  | EdgesOf of hash : string
  /// Every edge out of these items, in the order they were recorded. A missing name reads as "".
  | EdgesOfAll of hashes : List<string>
  /// The names one item references, skipping edges with no name.
  | NamesOf of hash : string
  /// The names referenced by whatever MAIN binds at this fully-qualified name.
  | NamesOfFqn of fqn : string
  /// The distinct hashes these items reference.
  | Closure of hashes : List<string>
  /// Who references this name, with the name MAIN binds each of them at.
  | Dependents of Name
  /// Who references this name, by hash only: branch-authored dependents included.
  | DependentHashes of Name
  /// The other names dependents wrote for this content.
  | RenamedAway of hash : string * Name
  /// Whether anything references this hash: at most one edge.
  | Referenced of hash : string
  /// Every live item on main whose edge points at a version its name no longer holds.
  | Outdated
  /// Everything that transitively references an old version of each seed (owner, modules, name,
  /// current hash), tagged with the seed's index.
  | StaleBySeed of seeds : List<string * string * string * string>


/// One answer row. Which fields are filled depends on the query; the rest are "" (or 0).
type Edge =
  {
    /// The item holding the reference.
    itemHash : string
    /// Where main binds that item, when the query joined `locations`.
    itemType : string
    owner : string
    modules : string
    name : string
    /// What it references, as the edge recorded it.
    dependsOnHash : string
    dependsOnType : string
    dependsOnOwner : string
    dependsOnModules : string
    dependsOnName : string
    /// `Outdated` only: what the referenced name holds now.
    currentHash : string
    currentType : string
    /// `StaleBySeed` only: which seed this item was reached from.
    seed : int64
  }

let private empty : Edge =
  { itemHash = ""
    itemType = ""
    owner = ""
    modules = ""
    name = ""
    dependsOnHash = ""
    dependsOnType = ""
    dependsOnOwner = ""
    dependsOnModules = ""
    dependsOnName = ""
    currentHash = ""
    currentType = ""
    seed = 0L }


let private run
  (sql : string)
  (parameters : List<string * Microsoft.Data.Sqlite.SqliteParameter>)
  (read : RowReader -> Option<Edge>)
  : Task<List<Edge>> =
  task {
    let! rows = Sql.query sql |> Sql.parameters parameters |> Sql.executeAsync read
    return List.choose identity rows
  }

let private nameParams
  (n : Name)
  : List<string * Microsoft.Data.Sqlite.SqliteParameter> =
  [ "kind", Sql.string n.kind
    "owner", Sql.string n.owner
    "modules", Sql.string n.modules
    "name", Sql.string n.name ]

// A JSON array of strings, or of string arrays, for `json_each` to bind. Written with the writer,
// never the reflecting serializer, which the published (AOT) binary does not have.
let private writeJson (f : System.Text.Json.Utf8JsonWriter -> unit) : string =
  use buffer = new System.IO.MemoryStream()
  (use w = new System.Text.Json.Utf8JsonWriter(buffer)
   f w)
  System.Text.Encoding.UTF8.GetString(buffer.ToArray())

let private jsonList (items : List<string>) : string =
  writeJson (fun w ->
    w.WriteStartArray()
    items |> List.iter (fun (s : string) -> w.WriteStringValue s)
    w.WriteEndArray())

let private jsonRows (rows : List<List<string>>) : string =
  writeJson (fun w ->
    w.WriteStartArray()
    rows
    |> List.iter (fun row ->
      w.WriteStartArray()
      row |> List.iter (fun (s : string) -> w.WriteStringValue s)
      w.WriteEndArray())
    w.WriteEndArray())

let private orEmpty (read : RowReader) (col : string) : string =
  read.stringOrNone col |> Option.defaultValue ""


let query (q : Query) : Task<List<Edge>> =
  match q with
  | Of hash ->
    run
      "SELECT DISTINCT depends_on_hash, depends_on_item_type
       FROM package_dependencies WHERE item_hash = @hash"
      [ "hash", Sql.string hash ]
      (fun read ->
        Some
          { empty with
              itemHash = hash
              dependsOnHash = read.string "depends_on_hash"
              dependsOnType = read.string "depends_on_item_type" })

  | EdgesOf hash ->
    run
      "SELECT DISTINCT depends_on_hash, depends_on_item_type,
              depends_on_owner, depends_on_modules, depends_on_name
       FROM package_dependencies WHERE item_hash = @hash"
      [ "hash", Sql.string hash ]
      (fun read ->
        match
          read.stringOrNone "depends_on_owner",
          read.stringOrNone "depends_on_modules",
          read.stringOrNone "depends_on_name"
        with
        | Some o, Some m, Some n ->
          Some
            { empty with
                itemHash = hash
                dependsOnHash = read.string "depends_on_hash"
                dependsOnType = read.string "depends_on_item_type"
                dependsOnOwner = o
                dependsOnModules = m
                dependsOnName = n }
        | _ -> None)

  | EdgesOfAll hashes ->
    run "SELECT item_hash, depends_on_owner AS d_owner, depends_on_modules AS d_modules,
              depends_on_name AS d_name, depends_on_item_type AS d_type,
              depends_on_hash AS using_hash
       FROM package_dependencies
       WHERE item_hash IN (SELECT value FROM json_each(@hashes))
       ORDER BY rowid" [ "hashes", Sql.string (jsonList hashes) ] (fun read ->
      Some
        { empty with
            itemHash = orEmpty read "item_hash"
            dependsOnHash = orEmpty read "using_hash"
            dependsOnType = orEmpty read "d_type"
            dependsOnOwner = orEmpty read "d_owner"
            dependsOnModules = orEmpty read "d_modules"
            dependsOnName = orEmpty read "d_name" })

  | NamesOf hash ->
    run
      "SELECT d.depends_on_owner AS o, d.depends_on_modules AS m, d.depends_on_name AS n
       FROM package_dependencies d
       WHERE d.item_hash = @hash AND d.depends_on_name <> ''"
      [ "hash", Sql.string hash ]
      (fun read ->
        match
          read.stringOrNone "o", read.stringOrNone "m", read.stringOrNone "n"
        with
        | Some o, Some m, Some n ->
          Some
            { empty with
                itemHash = hash
                dependsOnOwner = o
                dependsOnModules = m
                dependsOnName = n }
        | _ -> None)

  | NamesOfFqn fqn ->
    // Main-scoped, as it was: `locations` only, and NOT filtered to live rows. The caller asks the
    // draft first and comes here only for names the draft does not hold.
    run "SELECT d.depends_on_owner AS o, d.depends_on_modules AS m, d.depends_on_name AS n
       FROM package_dependencies d
       JOIN locations l ON l.item_hash = d.item_hash
       WHERE l.owner || '.' || (CASE WHEN l.modules = '' THEN '' ELSE l.modules || '.' END)
             || l.name = @fqn
         AND d.depends_on_name <> ''" [ "fqn", Sql.string fqn ] (fun read ->
      match read.stringOrNone "o", read.stringOrNone "m", read.stringOrNone "n" with
      | Some o, Some m, Some n ->
        Some
          { empty with dependsOnOwner = o; dependsOnModules = m; dependsOnName = n }
      | _ -> None)

  | Closure hashes ->
    if List.isEmpty hashes then
      Task.FromResult []
    else
      let ps = hashes |> List.mapi (fun i h -> $"h{i}", Sql.string h)
      let slots = ps |> List.map (fun (k, _) -> "@" + k) |> String.concat ", "
      run $"SELECT DISTINCT depends_on_hash AS h FROM package_dependencies
          WHERE item_hash IN ({slots}) AND depends_on_hash != ''" ps (fun read ->
        Some { empty with dependsOnHash = read.string "h" })

  | Dependents target ->
    // Main-scoped: a branch-authored dependent has no `locations` row, so the join drops it. The
    // Dark caller finds those through `DependentHashes` and its overlay.
    run "SELECT DISTINCT l.item_hash, l.item_type, l.owner, l.modules, l.name
       FROM package_dependencies pd
       JOIN locations l ON l.item_hash = pd.item_hash AND l.unlisted_at IS NULL
       WHERE pd.depends_on_item_type = @kind
         AND pd.depends_on_owner = @owner
         AND pd.depends_on_modules = @modules
         AND pd.depends_on_name = @name
       ORDER BY l.item_hash" (nameParams target) (fun read ->
      match
        read.stringOrNone "item_hash",
        read.stringOrNone "item_type",
        read.stringOrNone "owner",
        read.stringOrNone "modules",
        read.stringOrNone "name"
      with
      | Some h, Some k, Some o, Some m, Some n ->
        Some
          { empty with
              itemHash = h
              itemType = k
              owner = o
              modules = m
              name = n
              dependsOnType = target.kind
              dependsOnOwner = target.owner
              dependsOnModules = target.modules
              dependsOnName = target.name }
      | _ -> None)

  | DependentHashes target ->
    run "SELECT DISTINCT pd.item_hash
       FROM package_dependencies pd
       WHERE pd.depends_on_item_type = @kind
         AND pd.depends_on_owner = @owner
         AND pd.depends_on_modules = @modules
         AND pd.depends_on_name = @name
       ORDER BY pd.item_hash" (nameParams target) (fun read ->
      Some
        { empty with
            itemHash = read.string "item_hash"
            dependsOnType = target.kind
            dependsOnOwner = target.owner
            dependsOnModules = target.modules
            dependsOnName = target.name })

  | RenamedAway(hash, current) ->
    run
      "SELECT DISTINCT depends_on_owner, depends_on_modules, depends_on_name
       FROM package_dependencies
       WHERE depends_on_hash = @hash
         AND depends_on_item_type = @kind
         AND depends_on_owner IS NOT NULL
         AND NOT (depends_on_owner = @owner
                  AND depends_on_modules = @modules
                  AND depends_on_name = @name)"
      (("hash", Sql.string hash) :: nameParams current)
      (fun read ->
        match
          read.stringOrNone "depends_on_owner",
          read.stringOrNone "depends_on_modules",
          read.stringOrNone "depends_on_name"
        with
        | Some o, Some m, Some n ->
          Some
            { empty with
                dependsOnHash = hash
                dependsOnType = current.kind
                dependsOnOwner = o
                dependsOnModules = m
                dependsOnName = n }
        | _ -> None)

  | Referenced hash ->
    // Asked only as "is there any", so one row answers it; it used to be a count.
    run
      "SELECT item_hash FROM package_dependencies WHERE depends_on_hash = @hash LIMIT 1"
      [ "hash", Sql.string hash ]
      (fun read ->
        Some { empty with itemHash = read.string "item_hash"; dependsOnHash = hash })

  | Outdated ->
    // Stale (name, version) pairs first, then the edges and live usages behind them. The obvious
    // join, from each live usage to its edges, looks every edge's name up: one lookup per edge in
    // the store, on the common path where none is stale. MATERIALIZED and CROSS JOIN hold that
    // order against the planner, which otherwise drives from the ORDER BY. The ORDER BY's tail
    // restates the order the obvious join produced, so ties come out as they always did.
    //
    // Main-scoped, on purpose: a property of the store, not of the branch the process is on.
    run
      "WITH stale AS MATERIALIZED (
         SELECT t.depends_on_owner AS o, t.depends_on_modules AS m, t.depends_on_name AS n,
                t.depends_on_hash AS h, cur.item_hash AS ch, cur.item_type AS ct, cur.rowid AS crow
         FROM (SELECT DISTINCT depends_on_owner, depends_on_modules, depends_on_name, depends_on_hash
               FROM package_dependencies WHERE depends_on_owner IS NOT NULL) t
         JOIN locations cur
           ON cur.owner = t.depends_on_owner
          AND cur.modules = t.depends_on_modules
          AND cur.name = t.depends_on_name
          AND cur.unlisted_at IS NULL
         WHERE cur.item_hash <> t.depends_on_hash)
       SELECT pd.item_hash, l.owner, l.modules, l.name, l.item_type,
              pd.depends_on_owner AS d_owner, pd.depends_on_modules AS d_modules,
              pd.depends_on_name AS d_name, pd.depends_on_item_type AS d_type,
              pd.depends_on_hash AS using_hash, s.ch AS current_hash,
              s.ct AS current_type
       FROM stale s
       CROSS JOIN package_dependencies pd
       CROSS JOIN locations l
       WHERE pd.depends_on_owner = s.o AND pd.depends_on_modules = s.m
         AND pd.depends_on_name = s.n AND pd.depends_on_hash = s.h
         AND l.item_hash = pd.item_hash AND l.unlisted_at IS NULL
       ORDER BY l.owner, l.modules, l.name, l.item_type, l.rowid,
                pd.depends_on_hash, pd.depends_on_item_type,
                pd.depends_on_owner, pd.depends_on_modules, pd.depends_on_name, s.ct, s.crow"
      []
      (fun read ->
        Some
          { itemHash = orEmpty read "item_hash"
            itemType = orEmpty read "item_type"
            owner = orEmpty read "owner"
            modules = orEmpty read "modules"
            name = orEmpty read "name"
            dependsOnHash = orEmpty read "using_hash"
            dependsOnType = orEmpty read "d_type"
            dependsOnOwner = orEmpty read "d_owner"
            dependsOnModules = orEmpty read "d_modules"
            dependsOnName = orEmpty read "d_name"
            currentHash = orEmpty read "current_hash"
            currentType = orEmpty read "current_type"
            seed = 0L })

  | StaleBySeed seeds ->
    if List.isEmpty seeds then
      Task.FromResult []
    else
      // `seed` is the seed's index in the list, as `json_each` numbers it.
      let json = seeds |> List.map (fun (o, m, n, h) -> [ o; m; n; h ]) |> jsonRows
      run
        "WITH RECURSIVE seeds(seed, owner, modules, name, hash) AS (
           SELECT j.key, json_extract(j.value, '$[0]'), json_extract(j.value, '$[1]'),
                  json_extract(j.value, '$[2]'), json_extract(j.value, '$[3]')
           FROM json_each(@seeds) j
         ),
         stale(seed, hash) AS (
           SELECT s.seed, d.item_hash FROM seeds s
             JOIN package_dependencies d
               ON d.depends_on_owner = s.owner
              AND d.depends_on_modules = s.modules
              AND d.depends_on_name = s.name
              AND d.depends_on_hash <> s.hash
           UNION
           SELECT st.seed, d2.item_hash FROM package_dependencies d2
             JOIN stale st ON d2.depends_on_hash = st.hash
         )
         SELECT DISTINCT CAST(seed AS TEXT) AS seed, hash FROM stale"
        [ "seeds", Sql.string json ]
        (fun read ->
          match read.stringOrNone "seed", read.stringOrNone "hash" with
          | Some seed, Some h -> Some { empty with itemHash = h; seed = int64 seed }
          | _ -> None)
