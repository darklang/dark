/// Counts of things that can be wrong with the STORE itself, as opposed to with your code.
///
/// What each count means, and which ones a reload fixes, is `Darklang.SCM.StoreHealth`'s
/// business; this only counts. Read-only.
module LibDB.StoreHealth

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Sqlite

open Prelude

type Counts =
  {
    /// ops stamped more than a day ahead of now: some instance's clock is wrong
    futureStamped : int64
    /// `op_branches` rows naming an op that is not in `package_ops`
    orphanTags : int64
    /// `op_branches` rows naming a branch that is not in `branches`
    taggedToNothing : int64
    /// effective ops the fold never applied
    unfolded : int64
    /// live names bound to a hash with no content
    danglingBindings : int64
  }

/// One count, 0 when it cannot be answered: a health check that fails takes `dark status`
/// down with it, and that is worse than one missing line.
let private count (sql : string) : Task<int64> =
  task {
    try
      let! n = Sql.query sql |> Sql.executeRowAsync (fun read -> read.int64 "n")
      return n
    with _ ->
      return 0L
  }

let counts () : Task<Counts> =
  task {
    let! futureStamped =
      count
        "SELECT count(*) AS n FROM package_ops
         WHERE origin_ts > strftime('%Y-%m-%dT%H:%M:%fZ','now','+1 day')"

    let! orphanTags =
      count
        "SELECT count(*) AS n FROM op_branches ob
         LEFT JOIN package_ops p ON p.id = ob.op_id
         WHERE p.id IS NULL"

    let! taggedToNothing =
      count
        "SELECT count(*) AS n FROM op_branches ob
         LEFT JOIN branches b ON b.id = ob.branch_id
         WHERE b.id IS NULL"

    let! unfolded =
      count
        "SELECT count(*) AS n FROM package_ops WHERE applied = 0 AND effective = 1"

    let! danglingBindings =
      count
        "SELECT count(*) AS n FROM locations l
         WHERE l.unlisted_at IS NULL
           AND NOT EXISTS (SELECT 1 FROM package_functions f WHERE f.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_types t WHERE t.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_values v WHERE v.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_traits tr WHERE tr.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_trait_impls i WHERE i.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_tests test WHERE test.hash = l.item_hash)"

    return
      { futureStamped = futureStamped
        orphanTags = orphanTags
        taggedToNothing = taggedToNothing
        unfolded = unfolded
        danglingBindings = danglingBindings }
  }
