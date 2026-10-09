/// The store-upgrade units (`LibDB.Upgrades`): their order, what makes a set of them unrunnable, and that a
/// shape unit is all in or not at all.
module Tests.Upgrades

open Expecto

open Prelude

open Fumble
open LibDB.Sqlite

module Upgrades = LibDB.Upgrades
module BSOp = LibSerialization.Binary.Serialization.PT.PackageOp


/// The steps as they were listed in `Releases.fs` before they became units. A store that ran them did
/// so in this order, so the ported units must keep it relative to each other, whatever units come after.
let private portedOrder =
  [ "20260731_000001_conflicts_branch_id"
    "20260819_000001_commits_parent"
    "20260828_000001_package_ops_effective"
    "20260828_000002_branches_parent_id"
    "20260828_000003_commits_author"
    "20260828_000004_commits_origin_ts"
    "20260828_000005_locations_op_id"
    "20260828_000006_locations_previous"
    "20260904_000001_op_branches_source"
    "20260904_000002_previous_scm_store"
    "20260908_000001_relay_branch_freshness"
    "20260908_000002_item_docs"
    "20260909_000001_location_docs"
    "20260909_000002_conflicts_part"
    "20260909_000003_secrets_leave_the_store"
    "20260909_000004_candidates_gain_removed"
    "20260921_000001_trace_fn_calls_process_id"
    "20260921_000002_trace_fn_calls_seq"
    "20260921_000003_trace_fn_calls_ord"
    "20260924_000001_traces_status"
    "20260924_000002_traces_parent_id"
    "20260924_000003_traces_parent_seq"
    "20260924_000004_traces_pinned"
    "20260924_000005_traces_updated"
    "20260924_000006_traces_entry_hash"
    "20260926_000001_traces_result_value"
    "20260927_000001_traces_duration_ms"
    "20260928_000001_trace_fns_fn_hash"
    "20260930_000003_trace_loops"
    "20260930_000002_package_functions_debug_symbols"
    "20261007_000001_traces_bytes"
    "20261008_000001_trace_results_in_trace_blobs" ]

let private unit
  (name : string)
  (after : List<string>)
  (touches : List<string>)
  : Upgrades.Unit =
  { name = name
    kind = Upgrades.Shape
    after = after
    touches = touches
    transactional = true
    shape = [ Upgrades.Statement "SELECT 1" ]
    code = None }

let private ordering =
  testList
    "order"
    [ test "every unit in the tree loads, and the set has no problems" {
        let units = LibDB.UpgradeRegistry.sources |> List.map Upgrades.load
        Expect.isGreaterThanOrEqual
          (List.length units)
          32
          "the ported steps are all there"
        Expect.equal (Upgrades.problems units) [] "the units in the tree can run"
      }

      test "the ported units run in the order the steps did" {
        let ordered =
          LibDB.UpgradeRegistry.sources
          |> List.map Upgrades.load
          |> Upgrades.order
          |> List.map _.name
          |> List.filter (fun n -> List.contains n portedOrder)
        Expect.equal ordered portedOrder "relative order is unchanged"
      }

      test "units on different tables need no order, and ties break by name" {
        let ordered =
          [ unit "b" [] [ "t2" ]; unit "a" [] [ "t1" ]; unit "c" [ "b" ] [ "t2" ] ]
          |> Upgrades.order
          |> List.map _.name
        Expect.equal ordered [ "a"; "b"; "c" ] "after first, then by name"
      }

      test
        "two units on one table, neither after the other, are a problem that names the fix" {
        let ps =
          Upgrades.problems [ unit "x1" [] [ "traces" ]; unit "x2" [] [ "traces" ] ]
        Expect.equal (List.length ps) 1 "one problem"
        Expect.stringContains
          ps[0]
          "x1 and x2 both touch traces"
          "names both and the table"
        Expect.stringContains ps[0] "after = [\"x1\"]" "and the line that fixes it"
      }

      test "an unknown after, a cycle and a duplicate are each a problem" {
        Expect.isNonEmpty
          (Upgrades.problems [ unit "a" [ "ghost" ] [] ])
          "unknown after"
        Expect.isNonEmpty
          (Upgrades.problems [ unit "a" [ "b" ] []; unit "b" [ "a" ] [] ])
          "a cycle"
        Expect.isNonEmpty
          (Upgrades.problems [ unit "a" [] []; unit "a" [] [] ])
          "a duplicate"
      } ]

let private reading =
  testList
    "reading"
    [ test "a spec and a shape read as written" {
        let u =
          Upgrades.load
            { name = "r1"
              spec =
                "# why\nkind = \"shape\"\nafter = [\"r0\"]\ntouches = [\"t\", \"u\"]\n"
              shape =
                Some
                  "add-column t c TEXT NOT NULL DEFAULT ''\nCREATE INDEX IF NOT EXISTS i ON t(c);\n"
              code = None }
        Expect.equal u.after [ "r0" ] "after"
        Expect.equal u.touches [ "t"; "u" ] "touches"
        Expect.equal
          u.shape
          [ Upgrades.AddColumn("t", "c", "TEXT NOT NULL DEFAULT ''")
            Upgrades.Statement "CREATE INDEX IF NOT EXISTS i ON t(c)" ]
          "the column first, then the statement"
      } ]

let private table (name : string) : bool =
  Sql.query "SELECT 1 AS n FROM sqlite_master WHERE type = 'table' AND name = @t"
  |> Sql.parameters [ "t", Sql.string name ]
  |> Sql.executeExistsSync

let private recorded (name : string) : bool =
  Sql.query "SELECT 1 AS n FROM system_migrations_v0 WHERE name = @n"
  |> Sql.parameters [ "n", Sql.string name ]
  |> Sql.executeExistsSync

let private forget (name : string) (tbl : string) : unit =
  Sql.query $"DROP TABLE IF EXISTS {tbl}" |> Sql.executeStatementSync
  Sql.query "DELETE FROM system_migrations_v0 WHERE name = @n"
  |> Sql.parameters [ "n", Sql.string name ]
  |> Sql.executeStatementSync

let private running =
  testSequenced
  <| testList
    "running"
    [ test
        "a shape unit that fails part way leaves nothing behind, and is not recorded" {
        let name = "test_upgrade_fails_part_way"
        let tbl = "zz_upgrade_test_txn"
        forget name tbl
        let source : Upgrades.Source =
          { name = name
            spec = $"kind = \"shape\"\ntouches = [\"{tbl}\"]\n"
            shape =
              Some
                $"CREATE TABLE IF NOT EXISTS {tbl} (a INTEGER);\nINSERT INTO zz_no_such_table VALUES (1);\n"
            code = None }
        let raised =
          try
            Upgrades.runPending [ source ]
            false
          with Upgrades.StepFailed(step, _) ->
            step = name
        forget name "zz_unused" // the table is the thing under test, so only the record is cleared here
        Expect.isTrue raised "the failure is reported as this unit's"
        Expect.isFalse
          (table tbl)
          "the first statement was rolled back with the second"
        Expect.isFalse
          (recorded name)
          "and the unit is not recorded, so it runs again"
        forget name tbl
      }

      test "a shape unit that succeeds is recorded, and does not run again" {
        let name = "test_upgrade_succeeds"
        let tbl = "zz_upgrade_test_ok"
        forget name tbl
        let source : Upgrades.Source =
          { name = name
            spec = $"kind = \"shape\"\ntouches = [\"{tbl}\"]\n"
            shape = Some $"CREATE TABLE {tbl} (a INTEGER);\n"
            code = None }
        Upgrades.runPending [ source ]
        Expect.isTrue (table tbl) "it ran"
        Expect.isTrue (recorded name) "and is recorded"
        // A plain CREATE TABLE fails if run twice, so a second run that ran it would raise.
        Upgrades.runPending [ source ]
        forget name tbl
      } ]

let private opBlob (id : System.Guid) : Option<byte[]> =
  use conn = new Microsoft.Data.Sqlite.SqliteConnection(LibDB.Sqlite.connString)
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- "SELECT op_blob FROM package_ops WHERE id = $id"
  cmd.Parameters.AddWithValue("$id", string id)
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  match cmd.ExecuteScalar() with
  | :? (byte[]) as b -> Some b
  | _ -> None

let private putOp (id : System.Guid) (blob : byte[]) : unit =
  Sql.query
    "INSERT OR REPLACE INTO package_ops (id, op_blob, applied) VALUES (@id, @blob, 1)"
  |> Sql.parameters [ "id", Sql.uuid id; "blob", Sql.bytes blob ]
  |> Sql.executeStatementSync

let private dropOp (id : System.Guid) : unit =
  Sql.query "DELETE FROM package_ops WHERE id = @id"
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeStatementSync

let private rewriting =
  testSequenced
  <| testList
    "rewriting blobs"
    [ test
        "an older op moves to the current format under the same id; an unreadable one is left as it was" {
        let current = LibSerialization.Binary.BaseFormat.CurrentVersion
        // A SetName has had one layout in every format so far, so its current bytes under a v4 header
        // are exactly what a v4 binary wrote.
        let op =
          LibExecution.ProgramTypes.PackageOp.SetName(
            { owner = "Tests"; modules = [ "Rewrite" ]; name = "it" },
            LibExecution.ProgramTypes.Reference.PackageFn(
              LibExecution.ProgramTypes.Hash "rewrite-me"
            ),
            None
          )
        let id = System.Guid.Parse "0000000a-0000-0000-0000-00000000beef"
        let older = BSOp.serialize id op
        System.BitConverter.GetBytes(current - 1u).CopyTo(older, 0)
        let garbageId = System.Guid.Parse "0000000b-0000-0000-0000-00000000beef"
        let garbage =
          Array.append
            (System.BitConverter.GetBytes 1u)
            [| 4uy; 0uy; 0uy; 0uy; 99uy; 99uy; 99uy; 99uy |]
        putOp id older
        putOp garbageId garbage
        try
          LibDB.Upgrades.rewriteBlobsToCurrentFormat ()
          let after = opBlob id |> Option.get
          Expect.equal
            (System.BitConverter.ToUInt32(after, 0))
            current
            "rewritten in the current format"
          Expect.equal (BSOp.deserialize id after) op "to the same op"
          Expect.equal
            (opBlob garbageId)
            (Some garbage)
            "the unreadable one is untouched"
          // Nothing left to move, so a second run changes nothing.
          LibDB.Upgrades.rewriteBlobsToCurrentFormat ()
          Expect.equal (opBlob id) (Some after) "and running it again is a no-op"
        finally
          dropOp id
          dropOp garbageId
      } ]

let tests = testList "Upgrades" [ ordering; reading; running; rewriting ]
