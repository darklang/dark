/// Checks a batch of PackageOps must pass before anything is written.
///
/// These run against the ops themselves plus, where a check needs it, the names
/// already visible on the branch. None of them write. They exist so an authoring
/// surface can refuse a batch and say why, while the paths that must converge
/// without asking anyone (sync's fold, historical replay) go on accepting
/// everything and resolving it by their own rules.
module LibDB.OpValidation

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module PackageLocation = LibDB.PackageLocation
open LibSerialization.Hashing


/// The names that more than one declaration in <param ops> would bind, as
/// "fn Owner.Module.name" strings. Empty when the batch is consistent.
///
/// Authoring has to reject these before `HashStabilization.computeRealHashes`, which
/// keys items by FQN and would otherwise give two different bodies one content hash,
/// storing one of them under the other's identity. Raw WIP legitimately holds
/// successive edits to one name; `WipRefresh` compacts those before asking.
let duplicateDeclarations (ops : List<PT.PackageOp>) : List<string> =
  // Every Add* is immediately followed by the SetName that binds it, so a SetName
  // preceded by its own Add* is exactly one declaration. The Add* half is never read
  // here, only used to tell a declaration apart from a bare rename.
  ops
  |> List.pairwise
  |> List.choose (fun (previous, op) ->
    match previous, op with
    | PT.PackageOp.AddType _, PT.PackageOp.SetName(loc, PT.PackageType _) ->
      Some("type", PackageLocation.toFQN loc)
    | PT.PackageOp.AddFn _, PT.PackageOp.SetName(loc, PT.PackageFn _) ->
      Some("fn", PackageLocation.toFQN loc)
    | PT.PackageOp.AddValue _, PT.PackageOp.SetName(loc, PT.PackageValue _) ->
      Some("value", PackageLocation.toFQN loc)
    | _ -> None)
  |> List.countBy (fun declaration -> declaration)
  |> List.filter (fun (_, count) -> count > 1)
  |> List.map (fun ((kind, fqn), _) -> $"{kind} {fqn}")


/// The hashes in <param ops> that more than one body claims, as ready-to-print
/// messages. Empty when the batch is consistent.
///
/// A hash is a content identity, so two bodies under one hash means whichever lands
/// second silently becomes the first. This is the early check, for callers that can
/// still refuse the batch before it reaches the op log. `PackageOpPlayback` enforces
/// the same invariant against already-stored content, where raising is all that's left.
let hashClashes (ops : List<PT.PackageOp>) : List<string> =
  // Fingerprint by canonical content, not by the declared hash: authoring metadata and
  // alpha-renamed binders must not make two copies of one body look like two bodies.
  let claim (op : PT.PackageOp) : Option<(string * Hash) * Hash> =
    match op with
    | PT.PackageOp.AddType t ->
      Some(("type", t.hash), Hashing.computeTypeHash Hashing.Normal t)
    | PT.PackageOp.AddFn f ->
      Some(("fn", f.hash), Hashing.computeFnHash Hashing.Normal f)
    | PT.PackageOp.AddValue v ->
      Some(("value", v.hash), Hashing.computeValueHash Hashing.Normal v)
    | PT.PackageOp.SetName _
    | PT.PackageOp.Deprecate _
    | PT.PackageOp.Undeprecate _
    | PT.PackageOp.PropagateUpdate _
    | PT.PackageOp.RevertPropagation _ -> None

  ops
  |> List.choose claim
  // An empty hash is a placeholder awaiting stabilization; it claims nothing yet.
  |> List.filter (fun ((_, Hash declared), _) -> declared <> "")
  // Prelude's List.groupBy returns a Map, so this is keyed and sorted, not raw pairs.
  |> List.groupBy fst
  |> Map.toList
  |> List.choose (fun ((kind, Hash declared), claims) ->
    let bodies = claims |> List.map snd |> List.distinct |> List.length

    if bodies > 1 then
      Some $"{kind} hash {declared} is claimed by {bodies} different bodies"
    else
      None)


/// The names in <param ops> already held on the branch by an item of another kind, as
/// ready-to-print messages, empty when there's no clash.
///
/// One name holds one item, so replacing a value with a fn at the same name is a real decision, not a typo to
/// absorb silently. Local authoring can ask the human to be explicit (delete it first); the SYNC fold cannot -
/// it has no one to ask and must converge, so it replaces by last-writer-wins. That asymmetry is deliberate:
/// this guard is UX, not an invariant. Anything that reaches the fold is still handled.
let kindClashes
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<string>> =
  task {
    let setNames =
      ops
      |> List.choose (fun op ->
        match op with
        | PT.PackageOp.SetName(loc, target) -> Some(loc, target.kind)
        | _ -> None)

    let mutable clashes = []

    for (loc, kind) in setNames do
      // A DEPRECATED binding doesn't defend its name: `delete` retires the item, and the name is then free to
      // be re-bound as another kind (which is exactly what the clash message tells you to do). Reusing the
      // name is safe because dependents reference items by HASH, not by name - the retired item still
      // resolves for anything pointing at it. "Currently deprecated" = the newest deprecations row for the
      // item says so, matching how readers decide it (Queries.fs).
      let! live =
        Sql.query
          """
          SELECT l.item_type FROM locations l
          WHERE l.owner = @owner AND l.modules = @modules AND l.name = @name
            AND l.branch_id = @branch_id AND l.unlisted_at IS NULL
            AND NOT EXISTS (
              SELECT 1 FROM deprecations d
              WHERE d.item_hash = l.item_hash
                AND d.item_kind = l.item_type
                AND d.branch_id = l.branch_id
                AND d.unlisted_at IS NULL
                AND d.state = 'deprecated'
                AND d.created_at = (
                  SELECT MAX(d2.created_at) FROM deprecations d2
                  WHERE d2.item_hash = d.item_hash
                    AND d2.item_kind = d.item_kind
                    AND d2.branch_id = d.branch_id
                    AND d2.unlisted_at IS NULL
                )
            )
          LIMIT 1
          """
        |> Sql.parameters
          [ "owner", Sql.string loc.owner
            "modules", Sql.string (String.concat "." loc.modules)
            "name", Sql.string loc.name
            "branch_id", Sql.uuid branchId ]
        |> Sql.executeRowOptionAsync (fun read -> read.string "item_type")

      let incoming = kind.toString ()

      match live with
      | Some existing when existing <> incoming ->
        let dotted = (loc.owner :: loc.modules) @ [ loc.name ] |> String.concat "."
        clashes <-
          $"{dotted} is already a {existing}. A name holds one item, so it can't also be a {incoming}. "
          + $"Retire it first (dark delete {existing} {dotted}), or pick another name."
          :: clashes
      | _ -> ()

    return List.rev clashes
  }
