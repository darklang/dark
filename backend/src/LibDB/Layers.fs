/// What a branch SEES, for the three layered projections (`locations`, `deprecations`,
/// `location_docs`), and the row bookkeeping they share.
///
/// Every row of those tables belongs to a layer: a branch, and whether the op that wrote it is
/// still in that branch's draft. Within a layer, `live = 1` marks the newest row for its key.
/// A branch's VIEW is its own draft layer, its own committed layer, then each ancestor's
/// committed layer up to main's, nearest first; the first layer with a live row for a key
/// answers for that key. So a branch sees its own work over what its parents committed, and
/// never a parent's draft. Main's view is its draft over its committed work.
///
/// This is the whole branch model: the fold writes each op into its layer, and every read is a
/// query over the view.
module LibDB.Layers

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite
open LibDB.PreparedBatch

module PT = LibExecution.ProgramTypes


/// A branch's rows, split by whether the op that wrote them is committed.
type Layer = { branch : PT.BranchId; draft : bool }

/// A branch and its ancestors, nearest first, ending at main: what a point lookup ranks rows by.
///
/// Point lookups (one name, one hash) are the hot path, run hundreds of times a command, and
/// compiling `cte` + `visible` costs more than running them. So they read the few live rows at a key
/// with a plain indexed query and rank them here; whole-view queries use `visible`. Parents never
/// change once a branch exists, so a chain is read once per process.
let private chains =
  System.Collections.Concurrent.ConcurrentDictionary<PT.BranchId, List<PT.BranchId>>()

// Dropped with every other cache: a branch can be registered after a lookup first asked about it.
Caching.register (fun () -> chains.Clear())

/// Raises when the registry cannot be read: a guessed chain would drop an ancestor's layer from every
/// read, and record fork bases against it.
let chainOf (branch : PT.BranchId) : List<PT.BranchId> =
  chains.GetOrAdd(
    branch,
    fun b ->
      if b.IsMain then
        [ PT.BranchId.Main ]
      else
        let rows =
          Sql.query
            "WITH RECURSIVE chain(bid, depth) AS (
               SELECT @b, 0
               UNION ALL
               SELECT p.parent_id, c.depth + 1 FROM branches p JOIN chain c ON p.id = c.bid
               WHERE c.bid <> @m AND c.depth < 64)
             SELECT bid FROM chain ORDER BY depth"
          |> Sql.parameters
            [ "b", Sql.string (string b); "m", Sql.string (string PT.BranchId.Main) ]
          |> Sql.execute (fun read -> read.string "bid")
        match rows with
        | Ok ids ->
          let chain = ids |> List.choose PT.BranchId.Parse
          if List.contains PT.BranchId.Main chain then
            chain
          else
            chain @ [ PT.BranchId.Main ]
        | Error e ->
          Exception.raiseInternal
            "could not read a branch's parents"
            [ "branch", box (string b); "error", box e.Message ]
  )

/// The CTE that names <param branch>'s view: `chain(bid, depth)`, the branch and its ancestors
/// nearest first, and `layer(branch_id, draft, rank)`, lowest rank first. With <param own> false,
/// only what the branch sees BENEATH itself: its ancestors' committed work, which is what it forked
/// from and what it merges back into. `vo = 2` (`committedSqlParams`) is the branch's committed view.
///
/// The chain is bound as a JSON list (`chainOf`), not walked in SQL, and `layer` is materialised:
/// both keep a whole-view query as cheap as a scan of one table. Its own `WITH`; a caller with
/// more CTEs appends them after a comma. Parameters are `{prefix}vc` (the chain), `{prefix}vb` (the
/// branch), `{prefix}vm` (main's id) and `{prefix}vo` (own), bound by `bind`.
let cte (prefix : string) : string =
  $"WITH chain(bid, depth) AS (SELECT value, key FROM json_each({prefix}vc)),
    layer(branch_id, draft, rank) AS MATERIALIZED (
      SELECT bid, 1, 0 FROM chain WHERE depth = 0 AND {prefix}vo = 1
      UNION ALL
      SELECT bid, 0, depth * 2 + 1 FROM chain WHERE depth > 0 OR {prefix}vo >= 1
    )"

let private chainJson (branch : PT.BranchId) : string =
  chainOf branch |> List.map string |> jsonStrings

/// The parameters `cte` names, as (name, value) pairs.
let parameters
  (prefix : string)
  (branch : PT.BranchId)
  (own : bool)
  : List<string * obj> =
  [ prefix + "vc", box (chainJson branch)
    prefix + "vb", box (string branch)
    prefix + "vm", box (string PT.BranchId.Main)
    prefix + "vo", box (if own then 1L else 0L) ]

let private paramsWith
  (branch : PT.BranchId)
  (vo : int64)
  : List<string * Microsoft.Data.Sqlite.SqliteParameter> =
  [ "vc", Sql.string (chainJson branch)
    "vb", Sql.string (string branch)
    "vm", Sql.string (string PT.BranchId.Main)
    "vo", Sql.int64 vo ]

/// `cte "@"`'s parameters, for a `Fumble` query.
let sqlParams (branch : PT.BranchId) (own : bool) =
  paramsWith branch (if own then 1L else 0L)

/// `cte "@"`'s parameters for <param branch>'s COMMITTED view: its own committed work over its
/// ancestors', its draft left out.
let committedSqlParams (branch : PT.BranchId) = paramsWith branch 2L

/// `cte`'s parameters onto a command.
let bind
  (cmd : Microsoft.Data.Sqlite.SqliteCommand)
  (branch : PT.BranchId)
  (own : bool)
  =
  for (n, v) in parameters "$" branch own do
    p cmd n v

/// A CTE `visible` over <param table>: its live rows the view can see, the first layer's per key,
/// with that layer's `rank` and the row's `rowid` as `seq`. To follow `cte`.
///
/// Shadowing is decided against `{table}_shadow`, the keys the view's NEARER layers hold, rather than
/// by looking every row's key up in the table: the deepest layer (main's committed work) is nearly
/// every row, and the layers above it are a draft and a branch or two. The CROSS JOIN makes the
/// shadow set start from those few layers (through `idx_{table}_layer`) instead of the whole table.
let visible (table : string) (key : List<string>) : string =
  let cols = key |> List.map (fun c -> $"h.{c}") |> String.concat ", "
  let sameKey = key |> List.map (fun c -> $"s.{c} = t.{c}") |> String.concat " AND "
  $"{table}_shadow AS MATERIALIZED (
      SELECT {cols}, MIN(z.rank) AS r
      FROM layer z CROSS JOIN {table} h
        ON h.branch_id = z.branch_id AND h.draft = z.draft AND h.live = 1
      WHERE z.rank < (SELECT MAX(rank) FROM layer)
      GROUP BY {cols}),
    visible AS (
      SELECT t.*, t.rowid AS seq, y.rank AS rank
      FROM {table} t JOIN layer y ON y.branch_id = t.branch_id AND y.draft = t.draft
      WHERE t.live = 1 AND NOT EXISTS (
        SELECT 1 FROM {table}_shadow s WHERE {sameKey} AND s.r < y.rank))"


/// Where a row of (<param rowBranch>, <param draft>) ranks in the view of the chain's first branch,
/// lower first; None when the view does not see that layer (another branch's, or an ancestor's draft).
let rankIn
  (chain : List<PT.BranchId>)
  (rowBranch : string)
  (draft : bool)
  : Option<int> =
  chain
  |> List.tryFindIndex (fun b -> string b = rowBranch)
  |> Option.bind (fun i ->
    if i = 0 then Some(if draft then 0 else 1)
    elif draft then None
    else Some(2 * i + 1))

/// Of rows at one key, each with its (branch, draft), the one the view answers with.
let pick (chain : List<PT.BranchId>) (rows : List<string * bool * 'a>) : Option<'a> =
  rows
  |> List.choose (fun (b, d, x) -> rankIn chain b d |> Option.map (fun r -> (r, x)))
  |> List.sortBy fst
  |> List.tryHead
  |> Option.map snd


/// One layered table: its name, the columns that make a row's key, and the order that puts a key's
/// winning row first (rows aliased `x`): the fold's own rule, so a recompute agrees with the fold.
type Table = { name : string; key : List<string>; newestFirst : string }

/// `Lww`: the newer stamp, then a binding over a removal, then the higher hash.
let locations =
  { name = "locations"
    key = [ "owner"; "modules"; "name" ]
    newestFirst =
      "COALESCE(x.origin_ts, '') DESC, (x.item_hash <> '') DESC, x.item_hash DESC, x.rowid DESC" }

/// The newer statement, and on a tie the later arrival.
let deprecations =
  { name = "deprecations"
    key = [ "item_hash"; "item_kind" ]
    newestFirst = "x.origin_ts DESC, x.rowid DESC" }

let docs =
  { name = "location_docs"
    key = [ "owner"; "modules"; "name"; "kind"; "within" ]
    newestFirst = "x.origin_ts DESC, x.rowid DESC" }

let all = [ locations; deprecations; docs ]

/// `cte` and `visible` over <param table> as one `WITH`: what a query selecting `FROM visible` needs.
let view (prefix : string) (table : Table) : string =
  cte prefix + ", " + visible table.name table.key

/// Whether the view has a row of <param table> that meets <param where>, a condition over `t`:
/// `visible`'s own test, asked of the rows <param where> picks out. To follow `view`, whose `layer`
/// and `{table}_shadow` it reads.
///
/// For a query that knows the few keys it wants to ask about: each probe finds its rows through an
/// index on <param where>'s columns, where a filter over `visible` checks every row the view holds.
let existsVisible (table : Table) (where : string) : string =
  let sameKey =
    table.key |> List.map (fun c -> $"s.{c} = t.{c}") |> String.concat " AND "
  $"EXISTS (
      SELECT 1 FROM {table.name} t CROSS JOIN layer y
      WHERE {where} AND t.live = 1 AND y.branch_id = t.branch_id AND y.draft = t.draft
        AND NOT EXISTS (
          SELECT 1 FROM {table.name}_shadow s WHERE {sameKey} AND s.r < y.rank))"


/// Make the row being written the live one of its layer: everything else of <param layer> at
/// that key stops being live. Called right before inserting it; <param bindKey> binds the key
/// columns as `$col`.
let supersede
  (ctx : Ctx)
  (table : Table)
  (layer : Layer)
  (bindKey : Microsoft.Data.Sqlite.SqliteCommand -> unit)
  : Task<unit> =
  let sameKey =
    table.key |> List.map (fun c -> $"{c} = ${c}") |> String.concat " AND "
  exec
    ctx
    $"UPDATE {table.name} SET live = 0
      WHERE branch_id = $branch AND draft = $draft AND live = 1 AND {sameKey}"
    (fun cmd ->
      p cmd "$branch" (string layer.branch)
      p cmd "$draft" (if layer.draft then 1L else 0L)
      bindKey cmd)


/// Change <param branch>'s rows that the ops <param opsWhere> selects -- delete them, or move them
/// to the committed layer -- and recompute which row is live at every key they touched.
///
/// <param opsWhere> is a condition on `op_id`, over `$branch` and whatever <param bind> adds.
/// Marking first (`live = -1`, every row of the branch at an affected key) is what lets the
/// recompute find keys whose rows have just been deleted.
let private reshape
  (ctx : Ctx)
  (branch : PT.BranchId)
  (opsWhere : string)
  (bind : Microsoft.Data.Sqlite.SqliteCommand -> unit)
  (change : Table -> string)
  : Task<unit> =
  task {
    let bindAll (cmd : Microsoft.Data.Sqlite.SqliteCommand) =
      p cmd "$branch" (string branch)
      bind cmd
    for t in all do
      let cols = String.concat ", " t.key
      do!
        exec
          ctx
          $"UPDATE {t.name} SET live = -1
            WHERE branch_id = $branch AND ({cols}) IN
              (SELECT {cols} FROM {t.name} WHERE branch_id = $branch AND {opsWhere})"
          bindAll
      do! exec ctx (change t) bindAll
      let sameKey =
        t.key |> List.map (fun c -> $"x.{c} = {t.name}.{c}") |> String.concat " AND "
      do!
        exec
          ctx
          $"UPDATE {t.name} SET live = (rowid = (
              SELECT x.rowid FROM {t.name} x
              WHERE x.branch_id = {t.name}.branch_id AND x.draft = {t.name}.draft AND {sameKey}
              ORDER BY {t.newestFirst} LIMIT 1))
            WHERE branch_id = $branch AND live = -1"
          bindAll
  }

/// Delete what the ops <param opsWhere> selects wrote on <param branch>, in every layered table.
let dropRowsOf
  (ctx : Ctx)
  (branch : PT.BranchId)
  (opsWhere : string)
  (bind : Microsoft.Data.Sqlite.SqliteCommand -> unit)
  : Task<unit> =
  reshape ctx branch opsWhere bind (fun t ->
    $"DELETE FROM {t.name} WHERE branch_id = $branch AND {opsWhere}")

/// Move what the ops <param opsWhere> selects wrote on <param branch> from its draft layer to its
/// committed one: what a commit does to the projections.
let commitRowsOf
  (ctx : Ctx)
  (branch : PT.BranchId)
  (opsWhere : string)
  (bind : Microsoft.Data.Sqlite.SqliteCommand -> unit)
  : Task<unit> =
  reshape ctx branch opsWhere bind (fun t ->
    $"UPDATE {t.name} SET draft = 0 WHERE branch_id = $branch AND draft = 1 AND {opsWhere}")
