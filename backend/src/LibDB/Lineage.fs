/// What a binding REPLACED, recorded at the moment of authoring.
///
/// A `SetName` says what a name points at. It did not used to say what it pointed at BEFORE, and that
/// missing fact is why two machines could not tell an ordinary edit from an independent creation: both
/// arrive as "one local binding, one incoming binding, no shared base". With it, the two are different
/// facts rather than the same shape -- your edit of my function names my hash as its predecessor, and two
/// people who each invented the same name name nothing.
///
/// This is the ONE place that answers the question, and it answers it by asking the store. Parsers cannot:
/// they read a source file and have no store. So they emit `None`, this fills it in on the way to being
/// stored, and `None` survives only where it is true.
///
/// `None` means "no predecessor known", NOT "there was definitely nothing there". A conclusion is only
/// drawn when both sides of a comparison say it, which is why the weaker reading is safe.
module LibDB.Lineage

open System.Threading.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes


/// The hash currently bound at <param location> for <param itemKind>, if anything is.
///
/// Reads `locations` directly rather than going through a package manager, because this runs mid-author:
/// the answer wanted is what the store says right now, not what some overlay resolves.
let private currentlyBound
  (location : PT.PackageLocation)
  (itemKind : PT.ItemKind)
  : Task<Option<PT.Hash>> =
  task {
    let modules = String.concat "." location.modules
    let! found =
      Sql.query
        "SELECT item_hash FROM locations
         WHERE owner = @owner AND modules = @modules AND name = @name
           AND item_type = @item_type AND unlisted_at IS NULL
         LIMIT 1"
      |> Sql.parameters
        [ "owner", Sql.string location.owner
          "modules", Sql.string modules
          "name", Sql.string location.name
          "item_type", Sql.string (itemKind.toString ()) ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "item_hash")
    return found |> Option.map PT.Hash
  }


/// Fill in `previous` on every SetName in <param ops> that does not already carry one.
///
/// Ops are looked at in order and the answer is taken from the store, so a batch that binds the same name
/// twice records the same predecessor for both. That is correct: both replaced what was there when the
/// batch started, and the second's real predecessor is the first, which is in the same batch and travels
/// with it. Chasing that would mean the fold's own ordering rules leaking into authoring.
///
/// An op that already carries a predecessor keeps it. Propagation sets its own, and it knows better than
/// this does: it repointed a caller from a specific version deliberately.
let recordPrevious (ops : List<PT.PackageOp>) : Task<List<PT.PackageOp>> =
  task {
    let mutable result = []

    for op in List.rev ops do
      match op with
      | PT.PackageOp.SetName(location, target, None) ->
        let! previous = currentlyBound location target.kind
        result <- PT.PackageOp.SetName(location, target, previous) :: result
      | other -> result <- other :: result

    return result
  }
