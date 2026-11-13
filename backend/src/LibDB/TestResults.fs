/// The `dark test run` result cache (`package_test_results`).
///
/// The store keeps the outcome as text it never reads: what an outcome means, and which ones may
/// be reused, is the test runner's business (`Darklang.Cli.Packages.Test`).
module LibDB.TestResults

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Sqlite

open Prelude

/// Everything a stored result depends on that the test's own hash does not cover.
type Key =
  {
    /// the test's content hash
    test : string
    /// the build and the judging code it ran under
    runtime : string
    branch : string
    /// the branch's binding of `Stdlib.Test.Result`
    resultType : string
  }

let private keyParams (key : Key) =
  [ "test", Sql.string key.test
    "runtime", Sql.string key.runtime
    "branch", Sql.string key.branch
    "resultType", Sql.string key.resultType ]

/// The outcome stored under <param key>, if any.
let get (key : Key) : Task<Option<string>> =
  Sql.query
    "SELECT outcome_json FROM package_test_results
     WHERE test_hash = @test AND runtime_hash = @runtime
       AND branch_id = @branch AND result_type_hash = @resultType"
  |> Sql.parameters (keyParams key)
  |> Sql.executeRowOptionAsync (fun read -> read.string "outcome_json")

/// The outcome stored under each of <param keys>, in order, in one read.
let getMany (keys : List<Key>) : Task<List<Option<string>>> =
  task {
    let! found =
      Sql.query
        "SELECT j.key AS i, r.outcome_json FROM json_each(@keys) j
         JOIN package_test_results r
           ON r.test_hash = json_extract(j.value, '$[0]')
          AND r.runtime_hash = json_extract(j.value, '$[1]')
          AND r.branch_id = json_extract(j.value, '$[2]')
          AND r.result_type_hash = json_extract(j.value, '$[3]')"
      |> Sql.parameters
        [ "keys",
          keys
          |> List.map (fun k -> [ k.test; k.runtime; k.branch; k.resultType ])
          |> jsonRows
          |> Sql.string ]
      |> Sql.executeAsync (fun read -> read.int64 "i", read.string "outcome_json")
    let found = Map.ofList found
    return keys |> List.mapi (fun i _ -> Map.tryFind (int64 i) found)
  }

/// Store <param outcome> under <param key>, replacing what was there.
let put (key : Key) (outcome : string) : Task<unit> =
  Sql.query
    "INSERT INTO package_test_results
       (test_hash, runtime_hash, branch_id, result_type_hash, outcome_json)
     VALUES (@test, @runtime, @branch, @resultType, @outcome)
     ON CONFLICT(test_hash, runtime_hash, branch_id, result_type_hash)
     DO UPDATE SET outcome_json = excluded.outcome_json"
  |> Sql.parameters (("outcome", Sql.string outcome) :: keyParams key)
  |> Sql.executeStatementAsync
