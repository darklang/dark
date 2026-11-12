/// Shape changes to CANONICAL tables, on stores that already exist.
///
/// `migrations/schema/*.sql` declares the from-scratch shape, but `CREATE TABLE IF NOT EXISTS` no-ops
/// against a table that already exists, so a new column never reaches an existing store from those files.
///
/// In principle a PROJECTION needs no step, since dropping and re-folding it rebuilds the new shape. In
/// practice nothing on the shipped path does that: `rebuildProjections` is reachable only from LocalExec,
/// which is not shipped. So a projection whose shape changed needs a step here too, or, when the change
/// is too large to carry, a reset (`resetIfOlderShape`).
///
/// Dated schema files may add columns with ALTER TABLE ... ADD COLUMN. The schema
/// runner guards those statements with addColumnIfMissing before release steps
/// and indexes run. Release steps remain responsible for data migrations.
module LibDB.Releases

open Fumble
open LibDB.Sqlite

open Prelude


/// Does <param table> already have <param column>?
let private hasColumn (table : string) (column : string) : bool =
  // `pragma_table_info` is the queryable form of `PRAGMA table_info`, so this can be a normal SELECT.
  // Table name is interpolated because a pragma-table argument cannot be a bound parameter; it is a
  // literal in this file, never caller input.
  Sql.query $"SELECT 1 AS n FROM pragma_table_info('{table}') WHERE name = @c"
  |> Sql.parameters [ "c", Sql.string column ]
  |> Sql.executeExistsSync


let private tableExists (table : string) : bool =
  Sql.query "SELECT 1 AS n FROM sqlite_master WHERE type = 'table' AND name = @t"
  |> Sql.parameters [ "t", Sql.string table ]
  |> Sql.executeExistsSync


/// Create a table, or do nothing if it is already there (the FRESH store, where the schema just
/// declared it). The statement carries its own `IF NOT EXISTS`, so this is about the print.
let createTableIfMissing (table : string) (ddl : string) : unit =
  if not (tableExists table) then
    System.Console.Error.WriteLine $"  release: adding {table}"
    Sql.query ddl |> Sql.executeStatementSync


/// Add a column, or do nothing if it is already there (the FRESH store, where the schema just declared
/// it).
let addColumnIfMissing
  (table : string)
  (column : string)
  (declaration : string)
  : unit =
  if tableExists table && not (hasColumn table column) then
    System.Console.Error.WriteLine $"  release: adding {table}.{column}"
    Sql.query $"ALTER TABLE {table} ADD COLUMN {column} {declaration}"
    |> Sql.executeStatementSync


/// The package and SCM tables of a store made before names, deprecations and docs were kept per
/// branch (`LibDB.Layers`). Their shape changed too much to carry across: such a store's packages are
/// reset, and the caller's top-up or reload refills them. Every other table -- config, policy,
/// traces -- is kept. `package_ops` is last, as the table the shape is recognised by.
let private beforeLayers : List<string> =
  [ "op_branches"
    "branches"
    "branch_name_bases"
    "commits"
    "conflicts"
    "locations"
    "deprecations"
    "location_docs"
    "package_types"
    "package_values"
    "package_functions"
    "package_tests"
    "package_traits"
    "package_trait_impls"
    "package_dependencies"
    "seed_ops"
    "package_ops_applied_through"
    "package_ops_unapplied"
    "package_ops" ]

let private isOlderShape () : bool =
  (tableExists "package_ops"
   && not (
     hasColumn "package_ops" "branch_id"
     && hasColumn "package_ops" "op_kind"
     && hasColumn "package_ops" "seq"
   ))
  || (tableExists "locations" && not (hasColumn "locations" "draft"))

/// Reset the package tables of the store at <param dbPath> when it is older than the layered shape,
/// so the schema can create them afresh. Returns the backup taken first, when it reset.
///
/// Runs BEFORE the schema: `CREATE TABLE IF NOT EXISTS` would leave the old shapes in place and the
/// first index on a new column would fail. The backup is always a fresh copy, taken here, and the
/// reset refuses without one. The drops are one transaction, so a reset that is interrupted leaves
/// the old store whole, and the next start backs it up and resets it again.
let resetIfOlderShape (dbPath : string) : Option<string> =
  if not (isOlderShape ()) then
    None
  else
    match StoreBackups.beforeReset dbPath with
    | Error reason ->
      Exception.raiseInternal
        $"this store predates per-branch names, and the backup taken before resetting it failed: {reason}"
        []
    | Ok backup ->
      System.Console.Error.WriteLine
        $"  release: this store predates per-branch names. Its packages, branches and commits are copied to {backup.path}, and its packages are refilled from this build."
      beforeLayers
      |> List.filter tableExists
      |> List.map (fun t -> $"DROP TABLE {t}", [ [] ])
      |> Sql.executeTransactionSync
      |> ignore<List<int>>
      Some backup.path


// ---------------------
// The steps
// ---------------------
//
// APPEND ONLY. A step's name is how a store remembers having run it, so renaming one re-runs it and
// reordering changes what "already applied" means.

type Step = { name : string; run : unit -> unit }

/// A step that raised, by name. It is not recorded, so the next start runs it again.
exception StepFailed of step : string * inner : exn


/// Fill in operation kinds for rows we can decode. Leave their saved bytes unchanged.
let populateOpKinds () : unit =
  let rows =
    Sql.query "SELECT id, op_blob FROM package_ops WHERE op_kind = 'unknown'"
    |> Sql.execute (fun r -> r.uuid "id", r.bytes "op_blob")
  let rows =
    match rows with
    | Ok rows -> rows
    | Error error -> raise error
  let updates =
    rows
    |> List.choose (fun (id, blob) ->
      let kind =
        LibSerialization.Binary.Serialization.PT.PackageOp.kindOfBlob id blob
      if kind = "unknown" then
        None
      else
        Some [ "id", Sql.uuid id; "kind", Sql.string kind ])
  if not (List.isEmpty updates) then
    Sql.executeTransactionSync
      [ "UPDATE package_ops SET op_kind = @kind WHERE id = @id", updates ]
    |> ignore<List<int>>

/// Remove the obsolete warning-mode column and its CHECK constraints once.
/// SQLite requires rebuilding the table because its row constraint names the column.
let removeTraceWarningColumn () : unit =
  if hasColumn "trace_cli_invocations" "warn_permissions" then
    Sql.executeTransactionSync
      [ """CREATE TABLE trace_cli_invocations_current (
  trace_id TEXT PRIMARY KEY,
  kind TEXT NOT NULL CHECK (kind IN ('eval', 'script')),
  source TEXT NOT NULL,
  filename TEXT,               -- scripts only
  args BLOB,                   -- scripts only, see ScriptOrEvalInput.encodeArgs
  allow_harmful INTEGER NOT NULL CHECK (allow_harmful IN (0, 1)),
  sandbox INTEGER CHECK (sandbox IN (0, 1)),                  -- scripts only
  CHECK (
    (kind = 'eval'
     AND filename IS NULL AND args IS NULL
     AND sandbox IS NULL)
    OR (kind = 'script'
        AND filename IS NOT NULL AND args IS NOT NULL
        AND sandbox IS NOT NULL))
)""",
        [ [] ]
        """INSERT INTO trace_cli_invocations_current
             (trace_id, kind, source, filename, args, allow_harmful, sandbox)
           SELECT trace_id, kind, source, filename, args, allow_harmful, sandbox
           FROM trace_cli_invocations""",
        [ [] ]
        "DROP TABLE trace_cli_invocations", [ [] ]
        "ALTER TABLE trace_cli_invocations_current RENAME TO trace_cli_invocations",
        [ [] ] ]
    |> ignore<List<int>>

let steps : List<Step> =
  [ { name = "remove-trace-warning-mode"; run = removeTraceWarningColumn } ]


let private alreadyRun () : Set<string> =
  if not (tableExists "system_migrations_v0") then
    Set.empty
  else
    Sql.query "SELECT name FROM system_migrations_v0"
    |> Sql.execute (fun read -> read.string "name")
    |> Result.unwrap
    |> Set.ofList


/// Replay the schema statements that `keep` selects, against an existing store.
///
/// Schema text is passed in because shipped binaries have no migrations directory.
/// CREATE and INSERT statements are idempotent; ADD COLUMN is guarded separately.
///
/// Split in three on purpose, because ORDER matters against an existing store:
///
///   1. tables   -- create tables and apply guarded ADD COLUMN declarations
///   2. steps    -- migrate existing data and support previously recorded releases
///   3. indexes  -- `CREATE INDEX IF NOT EXISTS`, which FAILS if it names a column step 2 just added
///
/// Doing it in one pass fails exactly there: the schema indexes `package_ops(op_kind)`, and on a
/// store predating that column the index cannot be created.
let private schemaStatements (schemaSql : string) =
  // Comments FIRST, then split. The schema's comments contain semicolons ("NULL = the branch's
  // draft."), so splitting first cuts statements in half and SQLite reports "incomplete input".
  // No `--` appears inside a string literal in that file, so truncating at one is safe here.
  let stripped =
    schemaSql.Split('\n')
    |> Array.map (fun line ->
      match line.IndexOf "--" with
      | -1 -> line
      | i -> line.Substring(0, i))
    |> String.concat "\n"

  stripped.Split(';')
  |> Array.map (fun st -> st.Trim())
  |> Array.filter (fun st -> st <> "")

let private runStatements (keep : string -> bool) (schemaSql : string) : unit =
  let statements =
    schemaStatements schemaSql
    |> Array.filter (fun st -> keep (st.ToUpperInvariant()))

  if not (Array.isEmpty statements) then
    use conn = new Microsoft.Data.Sqlite.SqliteConnection(LibDB.Sqlite.connString)
    conn.Open()

    Telemetry.countBy "sql.schema" (int64 statements.Length)
    for st in statements do
      use cmd = conn.CreateCommand()
      cmd.CommandText <- st
      cmd.ExecuteNonQuery() |> ignore<int>

/// Step 1: tables, seeded rows, and guarded columns from the schema files.
let applySchemaTables (schemaSql : string) : unit =
  runStatements
    (fun upper ->
      upper.StartsWith "CREATE TABLE" || upper.StartsWith "INSERT OR IGNORE")
    schemaSql
  // SQLite lacks ADD COLUMN IF NOT EXISTS. Guard the declarations in dated
  // files so both fresh stores and repeated startup use the same schema source.
  for statement in schemaStatements schemaSql do
    if
      statement.StartsWith("ALTER TABLE", System.StringComparison.OrdinalIgnoreCase)
    then
      let matched =
        System.Text.RegularExpressions.Regex.Match(
          statement,
          @"^ALTER\s+TABLE\s+([A-Za-z_][A-Za-z0-9_]*)\s+ADD\s+COLUMN\s+([A-Za-z_][A-Za-z0-9_]*)\s+(.+)$",
          System.Text.RegularExpressions.RegexOptions.IgnoreCase
          ||| System.Text.RegularExpressions.RegexOptions.Singleline
        )
      if not matched.Success then
        Exception.raiseInternal
          "Unsupported schema ALTER TABLE statement"
          [ "sql", statement ]
      addColumnIfMissing
        matched.Groups[1].Value
        matched.Groups[2].Value
        matched.Groups[3].Value

/// Step 3: indexes, once every column they name exists. `UNIQUE` has to be matched separately:
/// `CREATE UNIQUE INDEX` does not start with "CREATE INDEX".
let applySchemaIndexes (schemaSql : string) : unit =
  runStatements
    (fun upper ->
      upper.StartsWith "CREATE INDEX" || upper.StartsWith "CREATE UNIQUE INDEX")
    schemaSql


/// Step 2: run unrecorded data migrations and legacy column additions in order.
/// Schema declarations have already established the shape. Keep each step guarded
/// because a fresh store or an earlier startup may already have applied its changes.
let runPending () : unit =
  // A step's name is its identity in `system_migrations_v0`, so two steps sharing one would run as
  // one and record as one, silently. Refused here, where every store passes on startup, because no
  // test constructs this list.
  let duplicates =
    steps
    |> List.countBy _.name
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map fst

  if not (List.isEmpty duplicates) then
    Exception.raiseInternal
      "duplicate release step name(s)"
      [ "names", String.concat ", " duplicates ]

  let done_ = alreadyRun ()

  for step in steps do
    if not (Set.contains step.name done_) then
      // Progress, not output. On a first run this is the first thing a language server
      // would send its editor, where stdout is the protocol channel.
      System.Console.Error.WriteLine $"Running release step: {step.name}"
      try
        step.run ()
      with e ->
        raise (StepFailed(step.name, e))

      Sql.query
        "INSERT INTO system_migrations_v0 (name, execution_date, sql)
         VALUES (@name, CURRENT_TIMESTAMP, @sql)
         ON CONFLICT(name) DO NOTHING"
      |> Sql.parameters
        [ "name", Sql.string step.name
          // The `sql` column wants the statement that ran; a code step has no single statement, so name
          // the step rather than leaving it empty.
          "sql", Sql.string $"(release step: {step.name})" ]
      |> Sql.executeStatementSync


  // A newer decoder can classify ops an older build stored as 'unknown'; reclassify them on
  // every start, including when every release step has already been recorded. Canonical bytes
  // stay unchanged.
  populateOpKinds ()
