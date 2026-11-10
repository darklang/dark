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
    /// ops the fold never applied
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

    let pending = AppliedOps.pending "p.rowid"
    let! unfolded = count $"SELECT count(*) AS n FROM ({pending})"

    // Each name looked up only in its own kind's table.
    let! danglingBindings =
      count
        "SELECT count(*) AS n FROM locations l
         WHERE l.live = 1 AND l.item_hash <> ''
           AND NOT EXISTS (SELECT 1 FROM package_functions f WHERE l.item_type = 'fn' AND f.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_types t WHERE l.item_type = 'type' AND t.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_values v WHERE l.item_type = 'value' AND v.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_traits tr WHERE l.item_type = 'trait' AND tr.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_trait_impls i WHERE l.item_type = 'impl' AND i.hash = l.item_hash)
           AND NOT EXISTS (SELECT 1 FROM package_tests test WHERE l.item_type = 'test' AND test.hash = l.item_hash)"

    return
      { futureStamped = futureStamped
        unfolded = unfolded
        danglingBindings = danglingBindings }
  }
