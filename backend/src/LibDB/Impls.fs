/// The store's half of trait dispatch: every impl of a trait, straight off the
/// `package_trait_impls` index (`trait_hash` is a column). The RT conversion drops the
/// ones whose name no longer binds them on the branch being asked.
///
/// Cached per trait and dropped with every other cache when ops fold, so a freshly
/// authored `impl` is dispatchable on its next call.
module LibDB.Impls

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes
open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module PMPT = LibDB.ProgramTypes


let private implsFor (traitHash : Hash) : Ply<List<PT.TraitImpl.TraitImpl>> =
  uply {
    let (Hash h) = traitHash
    let! hashes =
      Sql.query "SELECT hash FROM package_trait_impls WHERE trait_hash = @h"
      |> Sql.parameters [ "h", Sql.string h ]
      |> Sql.executeAsync (fun read -> Hash(read.string "hash"))
    let! impls = hashes |> Ply.List.mapSequentially PMPT.TraitImpl.get
    return impls |> List.choose (fun i -> i)
  }

/// Impls with a method of this name, for receiver calls. The method names are inside
/// the serialized item, so this reads every impl once; small today, and cached.
let private implsWithMethodNamed
  (methodName : string)
  : Ply<List<PT.TraitImpl.TraitImpl>> =
  uply {
    let! hashes =
      Sql.query "SELECT hash FROM package_trait_impls"
      |> Sql.executeAsync (fun read -> Hash(read.string "hash"))
    let! impls = hashes |> Ply.List.mapSequentially PMPT.TraitImpl.get
    return
      impls
      |> List.choose (fun i -> i)
      |> List.filter (fun i ->
        i.methods |> List.exists (fun (m, _) -> m = methodName))
  }

/// `impls` for the stored package manager. The cache holds the whole answer per
/// trait; `Caching.invalidateAll` empties it when the store changes.
let impls : Hash -> Ply<List<PT.TraitImpl.TraitImpl>> =
  let cached = Caching.withCache (fun h -> implsFor h |> Ply.map Some)
  fun traitHash ->
    uply {
      match! cached traitHash with
      | Some items -> return items
      | None -> return []
    }

let implsWithMethod : string -> Ply<List<PT.TraitImpl.TraitImpl>> =
  let cached = Caching.withCache (fun m -> implsWithMethodNamed m |> Ply.map Some)
  fun methodName ->
    uply {
      match! cached methodName with
      | Some items -> return items
      | None -> return []
    }
