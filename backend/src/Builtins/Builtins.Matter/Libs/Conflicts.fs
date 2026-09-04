/// Builtins for the sync Conflict/Resolution UX (`dark conflicts`). Conflicts are the LOCAL review log of
/// divergences; a human decision (`conflictKeep`) mints a SYNCED `Resolution` that overrides the op-fold and
/// converges on peers.
module Builtins.Matter.Libs.Conflicts

open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module PT = LibExecution.ProgramTypes
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution


let private conflictType () =
  FQTypeName.fqPackage (PackageRefs.Type.Sync.Conflicts.conflict ())


/// A `LibDB.Conflicts.Conflict` as its `Darklang.Sync.Conflicts.Conflict` Dval — the shape `dark conflicts`
/// reads. All fields are strings (the row is flat text), so there's no option/int marshalling to do.
let private toDval (c : LibDB.Conflicts.Conflict) : Dval =
  let typeName = conflictType ()
  let fields =
    Map
      [ "id", DString c.id
        "location", DString c.location
        "itemKind", DString c.itemKind
        "localHash", DString c.localHash
        "incomingHash", DString c.incomingHash
        "chosenHash", DString c.chosenHash
        "resolvedBy", DString c.resolvedBy
        "status", DString c.status ]
  DRecord(typeName, typeName, [], fields)


let fns () : List<BuiltInFn> =
  [ { name = fn "conflictsList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(NR.ok (conflictType ()), []))
      description =
        "This instance's recorded sync conflicts, each as a "
        + "`Darklang.Sync.Conflicts.Conflict`. Local-only review log."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            let! conflicts = LibDB.Conflicts.list ()
            let kt = KTCustomType(conflictType (), [])
            return conflicts |> List.map toDval |> Dval.list kt
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "conflictAcknowledge" 0
      typeParams = []
      parameters =
        [ Param.make
            "location"
            TString
            "the conflicted location (owner.modules.name)" ]
      returnType = TBool
      description =
        "Acknowledge the auto-resolved conflict at <param location> — the auto "
        + "(last-writer-wins) choice stands. Returns whether one was found."
      fn =
        (function
        | _, _, _, [| DString location |] ->
          uply {
            let! conflicts = LibDB.Conflicts.list ()

            match
              conflicts
              |> List.tryFind (fun c ->
                c.location = location && c.status = "auto-resolved")
            with
            | Some c ->
              do! LibDB.Conflicts.acknowledge c.id
              return DBool true
            | None -> return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }

    { name = fn "conflictKeep" 0
      typeParams = []
      parameters =
        [ Param.make
            "location"
            TString
            "the conflicted location (owner.modules.name)"
          Param.make
            "chosenHash"
            TString
            "the candidate content hash to keep (local or incoming)" ]
      returnType = TBool
      description =
        "Override the auto-resolution at <param location>: bind it to <param "
        + "chosenHash> by minting a SYNCED Resolution (applied locally now; "
        + "converges on peers). Marks that conflict overridden. Returns whether "
        + "one was found."
      fn =
        (function
        | _, _, _, [| DString location; DString chosenHash |] ->
          uply {
            let! conflicts = LibDB.Conflicts.list ()

            match
              conflicts
              |> List.tryFind (fun c ->
                c.location = location && c.status = "auto-resolved")
            with
            // A resolution must bind one of the TWO conflicting candidates; refuse an arbitrary hash so a
            // caller can't mint a synced Resolution to content that was never in contention at this location.
            | Some c when chosenHash = c.localHash || chosenHash = c.incomingHash ->
              let loc = LibDB.Conflicts.parseLocation location
              let chosenKind = PT.ItemKind.fromString c.itemKind
              let chosenRef =
                PT.Reference.fromHashAndKind (PT.Hash chosenHash, chosenKind)
              // The resolution's `at` competes in the same string-LWW as op origin_ts, so it MUST be minted
              // by the one canonical stamp source (lock-guarded, monotonic, InvariantCulture) — not a raw
              // UtcNow, which can tie/regress and formats locale-dependently.
              let at = LibDB.Inserts.nextOriginTs ()
              let r = LibDB.Resolutions.mk loc chosenRef "human" c.branchId at
              do! LibDB.Resolutions.recordAndApply r
              do! LibDB.Conflicts.markOverridden c.id
              return DBool true
            | Some _
            | None -> return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
