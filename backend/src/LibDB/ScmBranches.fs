/// Reads over `branches` and `branch_name_bases`, for the Dark SCM code (`SCM.Branches` and the
/// branch lookups in `SCM.PackageOps` and `SCM.Conflicts`). One function, one query family: `query`
/// takes which branches are wanted and answers with branch rows.
///
/// Each case is the SQL Dark used to run itself, moved across as it was: the same filter, the same
/// ordering. Nothing here decides anything. Main has no `branches` row, so none of these find it;
/// the Dark side special-cases main exactly as before.
///
/// Every column read here is NOT NULL in the schema, except `merged_at`, which is only ever tested
/// with `IS NOT NULL`. So there is no NULL for the Dark decoders' defaults to have covered.
module LibDB.ScmBranches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite


/// Which branches are wanted. One case per question the Dark side asks.
type Query =
  /// Every branch not archived, newest first, each with its op count.
  | All
  /// The branch with this id, archived or not. At most one row.
  | ById of id : string
  /// The newest branch not archived that goes by this name. At most one row.
  | ByName of name : string
  /// The active (unmerged, unarchived) children of this branch, by display name (the id when the
  /// name is "").
  | ChildrenOf of id : string
  /// The bases this branch recorded, one per name it touched. These are kept by branch id, with or
  /// without a `branches` row, so this answers one row carrying them whether or not the branch is
  /// registered.
  | NameBasesOf of id : string


/// The parent's hash for one name when the branch first touched it ("" = the name was new).
type NameBase =
  { owner : string; modules : string; name : string; baseHash : string }

/// One answer row. Which fields are filled depends on the query; the rest are "", 0 or empty.
type Info =
  {
    id : string
    /// "" when the id is the only handle.
    name : string
    parentId : string
    merged : bool
    /// `All` only: how many ops it holds.
    ops : int64
    /// `NameBasesOf` only.
    nameBases : List<NameBase>
  }

let private empty : Info =
  { id = ""; name = ""; parentId = ""; merged = false; ops = 0L; nameBases = [] }


let private run
  (sql : string)
  (parameters : List<string * Microsoft.Data.Sqlite.SqliteParameter>)
  : Task<List<Info>> =
  Sql.query sql
  |> Sql.parameters parameters
  |> Sql.executeAsync (fun read ->
    { empty with
        id = read.string "id"
        name = read.string "name"
        parentId = read.string "parent_id"
        merged = read.int64 "merged" <> 0L })


let query (q : Query) : Task<List<Info>> =
  match q with
  | All ->
    Sql.query
      "SELECT b.id, b.name, b.parent_id, b.merged_at IS NOT NULL AS merged,
              (SELECT count(*) FROM package_ops p WHERE p.branch_id = b.id) AS ops
       FROM branches b WHERE b.archived_at IS NULL ORDER BY b.created_at DESC"
    |> Sql.executeAsync (fun read ->
      { empty with
          id = read.string "id"
          name = read.string "name"
          parentId = read.string "parent_id"
          merged = read.int64 "merged" <> 0L
          ops = read.int64 "ops" })

  | ById id ->
    run
      "SELECT id, name, parent_id, merged_at IS NOT NULL AS merged
       FROM branches WHERE id = @id"
      [ "id", Sql.string id ]

  | ByName name ->
    run
      "SELECT id, name, parent_id, merged_at IS NOT NULL AS merged FROM branches
       WHERE name = @name AND archived_at IS NULL
       ORDER BY created_at DESC, rowid DESC LIMIT 1"
      [ "name", Sql.string name ]

  | ChildrenOf id ->
    run
      "SELECT id, name, parent_id, merged_at IS NOT NULL AS merged FROM branches
       WHERE parent_id = @id AND merged_at IS NULL AND archived_at IS NULL
       ORDER BY CASE WHEN name = '' THEN id ELSE name END"
      [ "id", Sql.string id ]

  | NameBasesOf id ->
    task {
      let! bases =
        Sql.query
          "SELECT owner, modules, name, base_hash FROM branch_name_bases WHERE branch_id = @id"
        |> Sql.parameters [ "id", Sql.string id ]
        |> Sql.executeAsync (fun read ->
          { owner = read.string "owner"
            modules = read.string "modules"
            name = read.string "name"
            baseHash = read.string "base_hash" })
      return [ { empty with id = id; nameBases = bases } ]
    }
