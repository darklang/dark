/// The store's half of trait dispatch: which items might be impls of a trait.
///
/// An impl is an ordinary value (or provider fn) whose body is a record of the
/// trait's type, so `package_dependencies` already says which items reference the
/// trait. That set is the candidate list; PT2RT's `ImplCandidate` keeps the ones
/// that are records of named fns, and the RT conversion drops the ones whose
/// name no longer binds them on the branch being asked.
///
/// Cached per trait and dropped with every other cache when ops fold, so a
/// freshly authored `impl` is dispatchable on its next call.
module LibDB.Impls

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes
open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module PMPT = LibDB.ProgramTypes


/// Content hashes of every stored item that depends on <param traitHash>, with
/// their kind. Reads `package_dependencies` directly: `Queries.getDependencies`
/// answers the other direction (what does X use).
let private dependentsOf (traitHash : Hash) : Task<List<Hash * PT.ItemKind>> =
  task {
    let (Hash h) = traitHash
    // The kind is not on the dependency row; a hash is a type, a value or a fn, and
    // the projection tables say which. One query per table keeps it index-only.
    let! values =
      Sql.query
        """
        SELECT DISTINCT pv.hash
        FROM package_dependencies pd
        INNER JOIN package_values pv ON pv.hash = pd.item_hash
        WHERE pd.depends_on_hash = @h
        """
      |> Sql.parameters [ "h", Sql.string h ]
      |> Sql.executeAsync (fun read -> Hash(read.string "hash"))
    let! fns =
      Sql.query
        """
        SELECT DISTINCT pf.hash
        FROM package_dependencies pd
        INNER JOIN package_functions pf ON pf.hash = pd.item_hash
        WHERE pd.depends_on_hash = @h
        """
      |> Sql.parameters [ "h", Sql.string h ]
      |> Sql.executeAsync (fun read -> Hash(read.string "hash"))
    return
      (values |> List.map (fun v -> (v, PT.ItemKind.Value)))
      @ (fns |> List.map (fun f -> (f, PT.ItemKind.Fn)))
  }


let private itemsFor
  (traitHash : Hash)
  : Ply<List<PT.PackageValue.PackageValue> * List<PT.PackageFn.PackageFn>> =
  uply {
    let! dependents = dependentsOf traitHash
    let! values =
      dependents
      |> List.choose (fun (h, k) -> if k = PT.ItemKind.Value then Some h else None)
      |> Ply.List.mapSequentially PMPT.Value.get
    let! fns =
      dependents
      |> List.choose (fun (h, k) -> if k = PT.ItemKind.Fn then Some h else None)
      |> Ply.List.mapSequentially PMPT.Fn.get
    return (List.choose (fun v -> v) values, List.choose (fun f -> f) fns)
  }

/// `implItems` for the stored package manager. The cache holds the whole answer
/// per trait; `Caching.invalidateAll` empties it when the store changes.
let implItems
  : Hash -> Ply<List<PT.PackageValue.PackageValue> * List<PT.PackageFn.PackageFn>> =
  let cached = Caching.withCache (fun h -> itemsFor h |> Ply.map Some)
  fun traitHash ->
    uply {
      match! cached traitHash with
      | Some items -> return items
      | None -> return ([], [])
    }
