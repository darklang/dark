/// Reads over `package_dependencies`, for the Dark SCM code (`SCM.Deps` and the modules that ask it
/// who references whom). One function, one query family: `query` takes what is being asked and
/// answers with edges.
///
/// Every case reads edges by hash only and joins nothing, so none of them is scoped to a branch.
/// Name-aware reads (who calls a name, which names a caller follows) are other builtins, such as
/// `pmDependents`.
module LibDB.ScmDeps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite


/// What is being asked. One case per question the Dark side asks.
type Query =
  /// What one item references, as distinct (hash, kind) pairs.
  | Of of hash : string
  /// The distinct hashes these items reference.
  | Closure of hashes : List<string>
  /// Whether anything references this hash: at most one edge.
  | Referenced of hash : string


/// One answer row. `Closure` leaves `dependsOnType` as "".
type Edge =
  {
    /// The item holding the reference.
    itemHash : string
    /// What it references, as the edge recorded it.
    dependsOnHash : string
    dependsOnType : string
  }

let private empty : Edge = { itemHash = ""; dependsOnHash = ""; dependsOnType = "" }


let private run
  (sql : string)
  (parameters : List<string * Microsoft.Data.Sqlite.SqliteParameter>)
  (read : RowReader -> Option<Edge>)
  : Task<List<Edge>> =
  task {
    let! rows = Sql.query sql |> Sql.parameters parameters |> Sql.executeAsync read
    return List.choose identity rows
  }


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

  | Closure hashes ->
    if List.isEmpty hashes then
      Task.FromResult []
    else
      let ps = hashes |> List.mapi (fun i h -> $"h{i}", Sql.string h)
      let slots = ps |> List.map (fun (k, _) -> "@" + k) |> String.concat ", "
      run $"SELECT DISTINCT depends_on_hash AS h FROM package_dependencies
          WHERE item_hash IN ({slots}) AND depends_on_hash != ''" ps (fun read ->
        Some { empty with dependsOnHash = read.string "h" })

  | Referenced hash ->
    // Asked only as "is there any", so one row answers it; it used to be a count.
    run
      "SELECT item_hash FROM package_dependencies WHERE depends_on_hash = @hash LIMIT 1"
      [ "hash", Sql.string hash ]
      (fun read ->
        Some { empty with itemHash = read.string "item_hash"; dependsOnHash = hash })
