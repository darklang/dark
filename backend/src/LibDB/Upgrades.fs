/// Store upgrades as units: one directory per upgrade under `backend/migrations/upgrades/`, ordered by what
/// each says it comes after rather than by a counter.
///
/// A unit is `upgrade.toml` (its spec), and either `shape.sql` (tables and columns, run in one transaction
/// with the unit's record) or `step.fs` (code). The build generates the list of units from the
/// directories (`UpgradeRegistry`, written by `scripts/build/_gen-upgrade-registry`), so two PRs that each
/// add a unit share no file. Generated rather than found by reflection, which the published (AOT, trimmed)
/// binary does not reliably have.
///
/// A unit's name is its directory's name and its identity: a store records it in `system_migrations_v0`
/// once it has run, and never runs it again. Nothing about a unit may depend on where it was built, since
/// it is run by binaries built elsewhere.
module LibDB.Upgrades

open Fumble
open LibDB.Sqlite

open Prelude

module BS = LibSerialization.Binary.Serialization


// ---------------------
// Helpers a unit's code uses
// ---------------------

/// Where an upgrade's progress lines go. LocalExec leaves them on stderr, for its log. The CLI collects
/// them and says one line when the upgrade has finished: someone whose store was upgraded wants to know
/// that it happened and where the old copy is, not which units ran (`dark store` has those).
let mutable say : string -> unit = fun line -> System.Console.Error.WriteLine line

/// Does <param table> already have <param column>?
let hasColumn (table : string) (column : string) : bool =
  // `pragma_table_info` is the queryable form of `PRAGMA table_info`, so this can be a normal SELECT.
  // Table name is interpolated because a pragma-table argument cannot be a bound parameter; it is a
  // literal in a unit, never caller input.
  Sql.query $"SELECT 1 AS n FROM pragma_table_info('{table}') WHERE name = @c"
  |> Sql.parameters [ "c", Sql.string column ]
  |> Sql.executeExistsSync

let tableExists (table : string) : bool =
  Sql.query "SELECT 1 AS n FROM sqlite_master WHERE type = 'table' AND name = @t"
  |> Sql.parameters [ "t", Sql.string table ]
  |> Sql.executeExistsSync

/// Add a column, or do nothing if it is already there (a fresh store, or a table that does not exist).
let addColumnIfMissing
  (table : string)
  (column : string)
  (declaration : string)
  : unit =
  if tableExists table && not (hasColumn table column) then
    say $"  upgrade: adding {table}.{column}"
    Sql.query $"ALTER TABLE {table} ADD COLUMN {column} {declaration}"
    |> Sql.executeStatementSync


// ---------------------
// What a unit is
// ---------------------


/// Every canonical blob older than this binary's format, rewritten in it: the package-op log and the
/// toplevels, the two tables the fold cannot rebuild. Called by one upgrade unit per format bump, so a
/// store's oldest blob is this binary's format afterwards and a reader can one day be retired.
///
/// A rewritten op keeps its row id. Ids are content hashes taken when the op was written, and the fold
/// only looks an op up by a RECOMPUTED id for the kinds whose layout no bump has touched (names,
/// deprecations, docs, impls), so keeping the id changes nothing a lookup sees. An op this binary
/// cannot read is left exactly as it is, as the fold leaves it: a later build may read it.
///
/// One transaction: all of a store's blobs move, or none do. Silent when there is nothing to move.
let rewriteBlobsToCurrentFormat () : unit =
  let current = LibSerialization.Binary.BaseFormat.CurrentVersion
  let versionOf (blob : byte[]) =
    if blob.Length >= 4 then System.BitConverter.ToUInt32(blob, 0) else 0u
  use conn = new Microsoft.Data.Sqlite.SqliteConnection(LibDB.Sqlite.connString)
  conn.Open()
  use tx = conn.BeginTransaction()
  let rows (sql : string) : List<string * byte[]> =
    use cmd = conn.CreateCommand()
    cmd.Transaction <- tx
    cmd.CommandText <- sql
    use r = cmd.ExecuteReader()
    [ while r.Read() do
        yield (r.GetString 0, r.GetFieldValue<byte[]> 1) ]
  let update (sql : string) (key : string) (blob : byte[]) =
    use cmd = conn.CreateCommand()
    cmd.Transaction <- tx
    cmd.CommandText <- sql
    cmd.Parameters.AddWithValue("$key", key)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    cmd.Parameters.AddWithValue("$blob", blob)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    cmd.ExecuteNonQuery() |> ignore<int>
  let mutable oldest = current
  let mutable ops = 0
  let mutable unreadable = 0
  if tableExists "package_ops" then
    for (id, blob) in rows "SELECT id, op_blob FROM package_ops" do
      let v = versionOf blob
      if v < current then
        let guid = System.Guid.Parse id
        match BS.PT.PackageOp.tryDeserialize guid blob with
        | Some op ->
          update
            "UPDATE package_ops SET op_blob = $blob WHERE id = $key"
            id
            (BS.PT.PackageOp.serialize guid op)
          oldest <- min oldest v
          ops <- ops + 1
        | None -> unreadable <- unreadable + 1
  let mutable toplevels = 0
  if tableExists "toplevels_v0" then
    for (tlid, blob) in rows "SELECT CAST(tlid AS TEXT), data FROM toplevels_v0" do
      let v = versionOf blob
      if v < current then
        let id = System.UInt64.Parse tlid
        let tl = BS.PT.Toplevel.deserialize id blob
        update
          "UPDATE toplevels_v0 SET data = $blob WHERE tlid = $key"
          tlid
          (BS.PT.Toplevel.serialize id tl)
        oldest <- min oldest v
        toplevels <- toplevels + 1
  tx.Commit()
  if ops + toplevels > 0 then
    say
      $"  upgrade: rewrote {ops} package op(s) and {toplevels} toplevel(s) from format {oldest} to {current}"
  if unreadable > 0 then
    say
      $"  upgrade: left {unreadable} package op(s) this dark cannot read as they were"

/// A unit as the build hands it over: its directory's name and the text of its files.
type Source =
  { name : string
    spec : string
    shape : Option<string>
    code : Option<unit -> unit> }

type Kind =
  | Shape
  | Data
  | Format
  | Backfill

/// One statement of a `shape.sql`.
type ShapeStatement =
  /// `add-column <table> <column> <declaration...>`: SQLite has no `ADD COLUMN IF NOT EXISTS`, so this is
  /// the one form the runner understands rather than passes through. It looks before it acts.
  | AddColumn of table : string * column : string * declaration : string
  /// Anything else, run as written. Must be safe to run on a store that already has the shape.
  | Statement of string

type Unit =
  {
    name : string
    kind : Kind
    after : List<string>
    touches : List<string>
    /// False for a unit whose code reaches outside the store (another database, a file), which one
    /// transaction cannot cover. Its record is written after it runs, as every step's was before units.
    transactional : bool
    shape : List<ShapeStatement>
    code : Option<unit -> unit>
  }

/// A unit that raised, by name. It is not recorded, so the next start runs it again.
exception StepFailed of step : string * inner : exn


// ---------------------
// Reading a unit
// ---------------------

/// The `upgrade.toml` subset units use: `key = "string"`, `key = true|false` and `key = ["a", "b"]`, one
/// per line, with `#` comments.
let parseSpec
  (unitName : string)
  (text : string)
  : Map<string, Choice<string, bool, List<string>>> =
  text.Split('\n')
  |> Array.choose (fun raw ->
    let line = raw.Trim()
    if line = "" || line.StartsWith "#" then
      None
    else
      match line.IndexOf '=' with
      | -1 ->
        Exception.raiseInternal
          $"upgrade {unitName}: a line of upgrade.toml is not `key = value`"
          [ "line", line ]
      | i ->
        let key = line.Substring(0, i).Trim()
        let value = line.Substring(i + 1).Trim()
        let unquote (s : string) =
          let s = s.Trim()
          if s.Length >= 2 && s.StartsWith "\"" && s.EndsWith "\"" then
            s.Substring(1, s.Length - 2)
          else
            Exception.raiseInternal
              $"upgrade {unitName}: `{key}` wants a quoted string"
              [ "value", s ]
        let parsed =
          if value = "true" then
            Choice2Of3 true
          elif value = "false" then
            Choice2Of3 false
          elif value.StartsWith "[" && value.EndsWith "]" then
            let inner = value.Substring(1, value.Length - 2).Trim()
            if inner = "" then
              Choice3Of3 []
            else
              inner.Split(',') |> Array.map unquote |> List.ofArray |> Choice3Of3
          else
            Choice1Of3(unquote value)
        Some(key, parsed))
  |> Map.ofArray

/// The statements of a `shape.sql`: `--` comments dropped, statements split on `;`, and `add-column`
/// lines recognised.
let parseShape (unitName : string) (text : string) : List<ShapeStatement> =
  let stripped =
    text.Split('\n')
    |> Array.map (fun line ->
      match line.IndexOf "--" with
      | -1 -> line
      | i -> line.Substring(0, i))
  // `add-column` is a line of its own; everything else is SQL, possibly over several lines.
  let addColumns, sqlLines =
    stripped |> Array.partition (fun l -> l.Trim().StartsWith "add-column ")
  let added =
    addColumns
    |> Array.map (fun l ->
      match
        l.Trim().Split(' ', 4, System.StringSplitOptions.RemoveEmptyEntries)
      with
      | [| _; table; column; declaration |] ->
        AddColumn(table, column, declaration.Trim())
      | _ ->
        Exception.raiseInternal
          $"upgrade {unitName}: `add-column` wants a table, a column and a declaration"
          [ "line", l ])
    |> List.ofArray
  let statements =
    (String.concat "\n" sqlLines).Split(';')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.map Statement
    |> List.ofArray
  // Order within a unit: the columns first, then the statements, which may read them.
  added @ statements

let load (source : Source) : Unit =
  let spec = parseSpec source.name source.spec
  let str key =
    match Map.tryFind key spec with
    | Some(Choice1Of3 s) -> Some s
    | None -> None
    | Some _ ->
      Exception.raiseInternal $"upgrade {source.name}: `{key}` wants a string" []
  let list key =
    match Map.tryFind key spec with
    | Some(Choice3Of3 l) -> l
    | None -> []
    | Some _ ->
      Exception.raiseInternal $"upgrade {source.name}: `{key}` wants a list" []
  let flag key fallback =
    match Map.tryFind key spec with
    | Some(Choice2Of3 b) -> b
    | None -> fallback
    | Some _ ->
      Exception.raiseInternal
        $"upgrade {source.name}: `{key}` wants true or false"
        []
  let kind =
    match str "kind" with
    | Some "shape" -> Shape
    | Some "data" -> Data
    | Some "format" -> Format
    | Some "backfill" -> Backfill
    | other ->
      Exception.raiseInternal
        $"upgrade {source.name}: `kind` must be shape, data, format or backfill"
        [ "kind", other ]
  let shape =
    match source.shape with
    | Some text -> parseShape source.name text
    | None -> []
  if List.isEmpty shape && Option.isNone source.code then
    Exception.raiseInternal
      $"upgrade {source.name}: has neither a shape.sql nor a step.fs, so it would do nothing"
      []
  { name = source.name
    kind = kind
    after = list "after"
    touches = list "touches"
    transactional = flag "transactional" true
    shape = shape
    code = source.code }


// ---------------------
// Order
// ---------------------

/// What is wrong with a set of units, as sentences a person can act on. Empty when the set can run.
///
/// Two units that touch the same table must be ordered, one reaching the other through `after`; the
/// fix is a line in the later unit's own spec, never an edit to a shared file.
let problems (units : List<Unit>) : List<string> =
  let byName = units |> List.map (fun u -> u.name, u) |> Map.ofList
  let duplicates =
    units
    |> List.countBy _.name
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map (fun (name, _) -> $"two upgrades are named {name}")
  let unknown =
    units
    |> List.collect (fun u ->
      u.after
      |> List.filter (fun a -> not (Map.containsKey a byName))
      |> List.map (fun a ->
        $"upgrade {u.name} comes after {a}, which does not exist"))
  // Everything a unit comes after, directly or not.
  let rec ancestors (seen : Set<string>) (name : string) : Set<string> =
    match Map.tryFind name byName with
    | None -> seen
    | Some u ->
      u.after
      |> List.fold
        (fun acc a ->
          if Set.contains a acc then acc else ancestors (Set.add a acc) a)
        seen
  let reach =
    units |> List.map (fun u -> u.name, ancestors Set.empty u.name) |> Map.ofList
  let cycles =
    units
    |> List.filter (fun u -> Set.contains u.name reach[u.name])
    |> List.map (fun u -> $"upgrade {u.name} comes after itself, through `after`")
  let unordered =
    [ for a in units do
        for b in units do
          if a.name < b.name then
            let shared = Set.intersect (Set.ofList a.touches) (Set.ofList b.touches)
            if
              not (Set.isEmpty shared)
              && not (Set.contains a.name reach[b.name])
              && not (Set.contains b.name reach[a.name])
            then
              let tables = shared |> String.concat ", "
              yield
                $"upgrades {a.name} and {b.name} both touch {tables} and neither comes after the other. Add `after = [\"{a.name}\"]` to {b.name}'s upgrade.toml (or the other way round), so every store runs them in one order." ]
  duplicates @ unknown @ cycles @ unordered

/// The units in the order every store runs them: `after` first, ties broken by name, so every binary
/// computes the same order from the same set.
let order (units : List<Unit>) : List<Unit> =
  match problems units with
  | [] ->
    let byName = units |> List.map (fun u -> u.name, u) |> Map.ofList
    let rec go
      (placed : List<Unit>)
      (placedNames : Set<string>)
      (remaining : List<Unit>)
      =
      match remaining with
      | [] -> List.rev placed
      | _ ->
        let ready =
          remaining
          |> List.filter (fun u ->
            u.after |> List.forall (fun a -> Set.contains a placedNames))
          |> List.sortBy _.name
        match ready with
        | [] -> Exception.raiseInternal "upgrades could not be ordered" []
        | next :: _ ->
          go
            (next :: placed)
            (Set.add next.name placedNames)
            (remaining |> List.filter (fun u -> u.name <> next.name))
    ignore<Map<string, Unit>> byName
    go [] Set.empty units
  | ps ->
    Exception.raiseInternal
      "the upgrades cannot run"
      [ "problems", String.concat "\n" ps ]


// ---------------------
// Running them
// ---------------------

let alreadyRun () : Set<string> =
  if not (tableExists "system_migrations_v0") then
    Set.empty
  else
    Sql.query "SELECT name FROM system_migrations_v0"
    |> Sql.execute (fun read -> read.string "name")
    |> Result.unwrap
    |> Set.ofList

let private recordSql =
  "INSERT INTO system_migrations_v0 (name, execution_date, sql)
   VALUES ($name, CURRENT_TIMESTAMP, $sql)
   ON CONFLICT(name) DO NOTHING"

/// A shape unit, in one transaction with its own record: it is all in, or none of it is.
let private runShape (u : Unit) : unit =
  use conn = new Microsoft.Data.Sqlite.SqliteConnection(LibDB.Sqlite.connString)
  conn.Open()
  use tx = conn.BeginTransaction()
  let exec (sql : string) =
    use cmd = conn.CreateCommand()
    cmd.Transaction <- tx
    cmd.CommandText <- sql
    cmd.ExecuteNonQuery() |> ignore<int>
  let scalarExists (sql : string) (p : string * string) =
    use cmd = conn.CreateCommand()
    cmd.Transaction <- tx
    cmd.CommandText <- sql
    cmd.Parameters.AddWithValue(fst p, snd p)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    not (isNull (cmd.ExecuteScalar()))
  for st in u.shape do
    match st with
    | AddColumn(table, column, declaration) ->
      let hasTable =
        scalarExists
          "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = $t"
          ("$t", table)
      let hasCol =
        hasTable
        && scalarExists
          $"SELECT 1 FROM pragma_table_info('{table}') WHERE name = $c"
          ("$c", column)
      if hasTable && not hasCol then
        say $"  upgrade: adding {table}.{column}"
        exec $"ALTER TABLE {table} ADD COLUMN {column} {declaration}"
    | Statement sql -> exec sql
  use record = conn.CreateCommand()
  record.Transaction <- tx
  record.CommandText <- recordSql
  record.Parameters.AddWithValue("$name", u.name)
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  record.Parameters.AddWithValue("$sql", $"(upgrade: {u.name})")
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  record.ExecuteNonQuery() |> ignore<int>
  tx.Commit()

let private recordCode (u : Unit) : unit =
  Sql.query
    "INSERT INTO system_migrations_v0 (name, execution_date, sql)
     VALUES (@name, CURRENT_TIMESTAMP, @sql)
     ON CONFLICT(name) DO NOTHING"
  |> Sql.parameters
    [ "name", Sql.string u.name; "sql", Sql.string $"(upgrade: {u.name})" ]
  |> Sql.executeStatementSync

/// Run every unit this store has not run, in order. A unit that raises stops the run as `StepFailed`,
/// unrecorded, so the next start runs it again; the caller decides what a failure does to the store.
let runPending (sources : List<Source>) : unit =
  let units = sources |> List.map load |> order
  let done_ = alreadyRun ()
  for u in units do
    if not (Set.contains u.name done_) then
      // Progress, not output: on a first run this is the first thing a language server would send its
      // editor, where stdout is the protocol channel.
      say $"Running upgrade: {u.name}"
      try
        match u.code with
        | Some run ->
          // A unit that is code runs its shape first, if it has one, then its code.
          if not (List.isEmpty u.shape) then runShape { u with code = None }
          run ()
          recordCode u
        | None -> runShape u
      with
      | StepFailed _ -> reraise ()
      | e -> raise (StepFailed(u.name, e))
