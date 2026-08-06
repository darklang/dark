/// Keep the edits you made when a newer build ships its own version of the same name.
///
/// A binary carries a package set, and topping a store up with it folds those ops like any others: the
/// binding for each name goes to whichever op has the later `origin_ts`. A build cut after your edit is
/// later than your edit, so the build silently takes the name back, and nothing says it happened.
///
/// So the ones you wrote are recorded before the fold (`EmbeddedResources.locallyAuthored`, which reads
/// `locations.op_id` against the seed's ids) and re-asserted here afterwards, for names the fold actually
/// moved. A name the build did not touch needs nothing.
///
/// The re-assertion is a `Resolve`, not another `SetName`. Re-authoring the SetName that made the binding
/// produces an op that already exists, which dedups and folds nothing -- the same reason a conflict
/// override is a `Resolve`. It also means the choice is in the log rather than a local edit to `locations`
/// that the next re-fold would undo.
///
/// It is a DEFAULT, not a decision anyone stated, so the caller says which names it kept.
///
/// It does NOT put the build's version within one command's reach. `dark undo` walks the names YOU
/// authored (`source = 'op'`), and the kept binding is one of those, so undo steps to your previous
/// version rather than to the build's. The build's op is still in the log; nothing yet binds it back by
/// name in one step.
module LibDB.UpgradeKeep

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes

/// A name whose binding the upgrade took, and the hash it held before.
type Kept =
  {
    location : PT.PackageLocation
    kind : PT.ItemKind
    hash : PT.Hash
    /// "op" when you wrote it, "propagation" when it followed something you wrote.
    source : string
  }

let private kindOf (itemType : string) : PT.ItemKind option =
  match itemType with
  | "fn" -> Some PT.ItemKind.Fn
  | "type" -> Some PT.ItemKind.Type
  | "value" -> Some PT.ItemKind.Value
  | _ -> None

/// What the store binds at each of these names now.
let private currentBindings
  (names : List<string * string * string>)
  : Task<Map<string * string * string, string>> =
  task {
    let! rows =
      Sql.query
        """
        SELECT owner, modules, name, item_hash
        FROM locations
        WHERE unlisted_at IS NULL
        """
      |> Sql.executeAsync (fun read ->
        (read.string "owner", read.string "modules", read.string "name"),
        read.string "item_hash")

    let wanted = Set.ofList names
    return rows |> List.filter (fun (k, _) -> Set.contains k wanted) |> Map.ofList
  }

/// Re-assert <param before> wherever the upgrade's fold moved the name. Returns what it kept.
let restore
  (before : List<string * string * string * string * string * string>)
  : Task<List<Kept>> =
  task {
    match before with
    | [] -> return []
    | _ ->
      let keys = before |> List.map (fun (o, m, n, _, _, _) -> (o, m, n))
      let! now = currentBindings keys

      let moved =
        before
        |> List.choose (fun (owner, modules, name, itemType, hash, source) ->
          match Map.tryFind (owner, modules, name) now, kindOf itemType with
          // Still ours: the build shipped nothing for this name, or shipped the same content.
          | Some current, _ when current = hash -> None
          // The name is gone entirely rather than rebound. Leave it; re-binding a name the build
          // retired would resurrect it, which is a different decision from keeping an edit.
          | None, _ -> None
          | Some _, None -> None
          | Some _, Some kind ->
            Some
              { location =
                  { owner = owner
                    modules =
                      if modules = "" then
                        []
                      else
                        modules.Split('.') |> List.ofArray
                    name = name }
                kind = kind
                hash = PT.Hash hash
                source = source })

      match moved with
      | [] -> return []
      | _ ->
        let stamp = System.DateTime.UtcNow.ToString("o")

        let ops =
          moved
          |> List.map (fun k ->
            let reference =
              match k.kind with
              | PT.ItemKind.Fn -> PT.Reference.PackageFn k.hash
              | PT.ItemKind.Type -> PT.Reference.PackageType k.hash
              | PT.ItemKind.Value -> PT.Reference.PackageValue k.hash

            let mods = String.concat "." k.location.modules
            let (PT.Hash h) = k.hash
            // Distinct from the SetName that first made this binding, and distinct per upgrade, so
            // keeping the same name twice across two upgrades folds both times.
            let decisionId =
              $"upgrade-kept:{k.location.owner}.{mods}.{k.location.name}:{h}:{stamp}"

            PT.PackageOp.Decision(
              decisionId,
              k.location,
              "kept across an upgrade",
              PT.DecisionKind.Override reference
            ))

        let! _ = LibDB.Inserts.insertAndApplyOps ops
        return moved
  }
