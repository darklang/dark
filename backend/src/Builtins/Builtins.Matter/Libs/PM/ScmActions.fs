/// The SCM actions, one builtin each: `scmCommit`, `scmBranch`, `scmMerge`, `scmDiscard`,
/// `scmResolve`. Each runs a whole operation in F# (`LibDB.ScmActions`) and answers with a typed
/// outcome, or a typed refusal Dark words for a person. Plus the two pure pieces of the conflict rule
/// Dark still asks about: `scmDetect` and `scmLwwBeats`.
module Builtins.Matter.Libs.PM.ScmActions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Actions = LibDB.ScmActions
module Detect = LibDB.ScmDetect


let private tn (f : unit -> string) : FQTypeName.FQTypeName =
  FQTypeName.fqPackage (f ())
let private kt (f : unit -> string) : KnownType = KTCustomType(tn f, [])
let private tref (f : unit -> string) : TypeReference = TCustomType(NR.ok (tn f), [])

let private record (f : unit -> string) (fields : List<string * Dval>) : Dval =
  let name = tn f
  DRecord(name, name, [], RecordFields.ofList fields)

let private enumCase
  (f : unit -> string)
  (case : string)
  (fields : List<Dval>)
  : Dval =
  let name = tn f
  DEnum(name, name, [], case, fields)

let private strings (items : List<Dval>) : List<string> =
  items
  |> List.map (function
    | DString s -> s
    | other -> Exception.raiseInternal "expected a String" [ "got", other ])

let private field (fields : RecordFields) (name : string) : Dval =
  match RecordFields.tryFind name fields with
  | Some v -> v
  | None -> Exception.raiseInternal "missing field" [ "field", name ]

let private str (fields : RecordFields) (name : string) : string =
  match field fields name with
  | DString s -> s
  | other ->
    Exception.raiseInternal "expected a String" [ "field", name; "got", other ]

module Refs = LibExecution.PackageRefs.Type.SCM


// --- outcomes ------------------------------------------------------------------------------

let private refusalToDT (r : Actions.Refusal) : Dval =
  let c name = enumCase Refs.refusal name []
  match r with
  | Actions.NothingToCommit -> c "NothingToCommit"
  | Actions.IsMain -> c "IsMain"
  | Actions.NothingToMerge -> c "NothingToMerge"
  | Actions.HasActiveChildren -> c "HasActiveChildren"
  | Actions.NameTaken -> c "NameTaken"
  | Actions.NotBound -> c "NotBound"
  | Actions.NoSuchConflict -> c "NoSuchConflict"
  | Actions.NoVersion -> c "NoVersion"
  | Actions.UnreadablePart part ->
    enumCase Refs.refusal "UnreadablePart" [ DString part ]
  | Actions.Moved -> c "Moved"

let private failureToDT (f : Actions.Failure) : Dval =
  match f with
  | Actions.Refused r -> enumCase Refs.failure "Refused" [ refusalToDT r ]
  | Actions.Failed message -> enumCase Refs.failure "Failed" [ DString message ]

/// A `Result<ok, Failure>`.
let private outcome
  (okType : KnownType)
  (toDT : 'a -> Dval)
  (r : Result<'a, Actions.Failure>)
  : Dval =
  match r with
  | Ok v -> Dval.resultOk okType (kt Refs.failure) (toDT v)
  | Error f -> Dval.resultError okType (kt Refs.failure) (failureToDT f)

let private resultType (ok : TypeReference) : TypeReference =
  TypeReference.result ok (tref Refs.failure)


// --- conflicts as Dark values ----------------------------------------------------------------

let private candidateToDT (c : Detect.Candidate) : Dval =
  record
    Refs.candidate
    [ "side", DString c.side
      "hash", DString c.hash
      "removed", DBool c.removed
      "text", DString c.text
      "originTs", DString c.originTs
      "author", DString c.author ]

let private conflictToDT (c : Detect.Conflict) : Dval =
  record
    Refs.conflict
    [ "id", DString c.id
      "owner", DString c.owner
      "modules", DString c.modules
      "name", DString c.name
      "itemType", DString c.itemType
      "part", DString c.part
      "kind", DString c.kind
      "candidates",
      c.candidates |> List.map candidateToDT |> Dval.list (kt Refs.candidate)
      "autoResolvedTo", DString c.autoResolvedTo
      "reason", DString c.reason
      "status", DString c.status
      "resolvedBy", DString c.resolvedBy ]

let private conflictList (cs : List<Detect.Conflict>) : Dval =
  cs |> List.map conflictToDT |> Dval.list (kt Refs.conflict)

let private optionString (d : Dval) : Option<string> =
  match d with
  | DEnum(_, _, _, "Some", [ DString s ]) -> Some s
  | _ -> None

let private bindingFromDT (d : Dval) : Detect.Binding =
  match d with
  | DRecord(_, _, _, f) ->
    { Detect.Binding.owner = str f "owner"
      modules = str f "modules"
      name = str f "name"
      itemType = str f "itemType"
      hash = str f "hash"
      originTs = str f "originTs"
      author = str f "author"
      previous = optionString (field f "previous") }
  | other -> Exception.raiseInternal "expected a Binding" [ "got", other ]

let private removalFromDT (d : Dval) : Detect.Removal =
  match d with
  | DRecord(_, _, _, f) ->
    { Detect.Removal.owner = str f "owner"
      modules = str f "modules"
      name = str f "name"
      originTs = str f "originTs"
      author = str f "author"
      previous = optionString (field f "previous") }
  | other -> Exception.raiseInternal "expected a Removal" [ "got", other ]

let private nameBaseFromDT (d : Dval) : Detect.NameBase =
  match d with
  | DRecord(_, _, _, f) ->
    { Detect.NameBase.owner = str f "owner"
      modules = str f "modules"
      name = str f "name"
      baseHash = str f "baseHash" }
  | other -> Exception.raiseInternal "expected a NameBase" [ "got", other ]

let private listOf (f : Dval -> 'a) (d : Dval) : List<'a> =
  match d with
  | DList(_, items) -> List.map f items
  | other -> Exception.raiseInternal "expected a List" [ "got", other ]


// --- requests --------------------------------------------------------------------------------

let private reconciledToDT (r : Actions.Reconciled) : Dval =
  record
    Refs.reconciled
    [ "moved", DInt64 r.moved
      "conflicts", conflictList r.conflicts
      "diverged", r.diverged |> List.map DString |> Dval.list KTString
      "repointed", r.repointed |> List.map DString |> Dval.list KTString
      "commit", DString r.commit
      "warning", DString r.warning ]

let private branchDoneToDT (b : Actions.BranchDone) : Dval =
  record Refs.branchDone [ "id", DUuid b.id.Guid; "created", DBool b.created ]

let private committedToDT (c : Actions.Committed) : Dval =
  record
    Refs.committed
    [ "hash", DString c.hash
      "stamped", DInt64 c.stamped
      "parent", DString c.parent ]

let private discardWhatFromDT (d : Dval) : Actions.DiscardWhat =
  match d with
  | DEnum(_, _, _, "All", []) -> Actions.All
  | DEnum(_, _, _, "Name", [ loc ]) -> Actions.Name(PT2DT.PackageLocation.fromDT loc)
  | DEnum(_, _, _, "Collapse", []) -> Actions.Collapse
  | DEnum(_, _, _, "Ops", [ DList(_, ids) ]) -> Actions.Ops(strings ids)
  | other -> Exception.raiseInternal "unknown DiscardWhat" [ "got", other ]

let private repointFromDT (d : Dval) : Actions.Repoint =
  match d with
  | DRecord(_, _, _, f) ->
    { Actions.Repoint.usage = str f "usage"
      dependency = PT2DT.PackageLocation.fromDT (field f "dependency")
      kind = PT2DT.ItemKind.fromDT (field f "kind")
      fromHash = PT2DT.Hash.fromDT (field f "fromHash")
      toHash = PT2DT.Hash.fromDT (field f "toHash") }
  | other -> Exception.raiseInternal "expected a Repoint" [ "got", other ]

let private branch (g : System.Guid) : PT.BranchId = PT.BranchId.Id g

let private referenceOf (itemType : string) (hash : PT.Hash) : PT.Reference =
  match itemType with
  | "type" -> PT.Reference.PackageType hash
  | "value" -> PT.Reference.PackageValue hash
  | "trait" -> PT.Reference.PackageTrait hash
  | "impl" -> PT.Reference.PackageTraitImpl hash
  | "test" -> PT.Reference.PackageTest hash
  | _ -> PT.Reference.PackageFn hash

let private ok () : Result<unit, Actions.Failure> = Ok()


/// Author one op as settling conflict <param id>, then close the record. Authoring goes through the
/// ordinary path (a whole edit: stored, folded, refreshed, evaluated); the close is a transaction of
/// its own after it. Stopped between the two, the op is in and the record still pending, and settling
/// again authors nothing new (the op is content-addressed) and closes it. A close that fails is the
/// answer, so the caller never reports a conflict settled while its record is still open.
let private settle
  (exeState : ExecutionState)
  (vm : VMState)
  (pm : PT.PackageManager)
  (branchId : PT.BranchId)
  (c : Detect.Conflict)
  (op : PT.PackageOp)
  (resolvedBy : string)
  : Ply<Result<unit, Actions.Failure>> =
  uply {
    match! PackageOps.authorOps exeState vm pm branchId [ op ] with
    | Error e -> return Error(Actions.Failed e)
    | Ok _ -> return! Actions.setConflictStatus c.id "overridden" resolvedBy
  }

let private resolve
  (exeState : ExecutionState)
  (vm : VMState)
  (pm : PT.PackageManager)
  (r : Dval)
  : Ply<Result<unit, Actions.Failure>> =
  uply {
    let withConflict
      (id : string)
      (f : Detect.Conflict -> Ply<Result<unit, Actions.Failure>>)
      : Ply<Result<unit, Actions.Failure>> =
      uply {
        match! Actions.conflictById id with
        | None -> return Error(Actions.Refused Actions.NoSuchConflict)
        | Some c -> return! f c
      }

    match r with
    | DEnum(_, _, _, "KeepMine", [ DUuid b; DString o; DString m; DString n ]) ->
      return! Actions.keepMine (branch b) o m n
    | DEnum(_, _, _, "TakeTheirs", [ DUuid b; DString o; DString m; DString n ]) ->
      return! Actions.takeTheirs (branch b) o m n
    | DEnum(_, _, _, "Ack", [ DString id ]) ->
      return! Actions.setConflictStatus id "acked" ""
    | DEnum(_, _, _, "Supersede", [ DString id; DString liveHash ]) ->
      return! Actions.setConflictStatus id "superseded" liveHash

    | DEnum(_, _, _, "Override", [ DUuid b; DString id; DString hash ]) ->
      // Refused rather than binding a name to nothing: `Hash ""` corrupts a binding in a way no
      // later fold undoes.
      if hash = "" then
        return Error(Actions.Refused Actions.NoVersion)
      else
        return!
          withConflict id (fun c ->
            uply {
              let location = Actions.locationOfConflict c
              let chosen = PT.Hash hash
              // A `Decision` carrying the conflict id: distinct from the losing `SetName` (which the
              // log already holds), and idempotent, so settling twice is one op.
              let op =
                PT.PackageOp.Decision(
                  c.id,
                  location,
                  "",
                  PT.DecisionKind.Override(referenceOf c.itemType chosen)
                )
              match! settle exeState vm pm (branch b) c op hash with
              | Error e -> return Error e
              | Ok() ->
                // Cascade, the way an edit does: an override rebinds the NAME, and the callers that
                // followed the losing version would otherwise go on calling it by hash.
                let losers =
                  c.candidates
                  |> List.choose (fun cand ->
                    if cand.hash = hash || cand.hash = "" then
                      None
                    else
                      Some(PT.Hash cand.hash))
                let kind =
                  match c.itemType with
                  | "type" -> PT.ItemKind.Type
                  | "value" -> PT.ItemKind.Value
                  | "trait" -> PT.ItemKind.Trait
                  | "impl" -> PT.ItemKind.TraitImpl
                  | "test" -> PT.ItemKind.Test
                  | _ -> PT.ItemKind.Fn
                let! _ = Packages.repoint (branch b) location kind losers chosen
                return ok ()
            })

    | DEnum(_, _, _, "TakeRemoval", [ DUuid b; DString id; previous ]) ->
      return!
        withConflict id (fun c ->
          // `previous` is what the name holds now, so the op says what it ended.
          let op =
            PT.PackageOp.Unbind(
              Actions.locationOfConflict c,
              optionString previous |> Option.map PT.Hash
            )
          settle exeState vm pm (branch b) c op "")

    | DEnum(_, _, _, "Reword", [ DUuid b; DString id; DString text ]) ->
      return!
        withConflict id (fun c ->
          uply {
            match Actions.docPartOfKey c.part with
            | None -> return Error(Actions.Refused(Actions.UnreadablePart c.part))
            | Some part ->
              // `previous` names the text being replaced, so the op reads as an ordinary edit
              // made on top of what this store holds.
              let previous =
                c.candidates
                |> List.tryFind (fun cand -> cand.side = "local")
                |> Option.bind (fun cand ->
                  if cand.text = "" then
                    None
                  else
                    Some(Actions.hashOfText cand.text))
              let op =
                PT.PackageOp.UpdateDoc(
                  Actions.locationOfConflict c,
                  part,
                  text,
                  previous,
                  None
                )
              return! settle exeState vm pm (branch b) c op ""
          })

    | other -> return Exception.raiseInternal "unknown Resolution" [ "got", other ]
  }


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "scmCommit" 0
      typeParams = []
      parameters =
        [ Param.make
            "request"
            (tref Refs.commitRequest)
            "what to commit, where, under what" ]
      returnType = resultType (tref Refs.committed)
      description =
        "Commits the draft, or the part of it <param request> names: writes the commit and "
        + "stamps its ops in one transaction, so a commit never holds nothing. Refuses, "
        + "writing nothing, when no op of the request is in the draft."
      fn =
        (function
        | _, _, _, [| DRecord(_, _, _, f) |] ->
          uply {
            let scope =
              match field f "scope" with
              | DEnum(_, _, _, "Whole", []) -> Actions.Whole
              | DEnum(_, _, _, "Only", [ DList(_, ids) ]) ->
                Actions.Only(strings ids)
              | other ->
                Exception.raiseInternal "unknown CommitScope" [ "got", other ]
            let branchId =
              match field f "branchId" with
              | DUuid g -> branch g
              | other -> Exception.raiseInternal "expected a Uuid" [ "got", other ]
            let collapse =
              match field f "collapse" with
              | DBool b -> b
              | _ -> false
            let! r = Actions.commit branchId (str f "message") scope collapse
            return outcome (kt Refs.committed) committedToDT r
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "scmBranch" 0
      typeParams = []
      parameters =
        [ Param.make "action" (tref Refs.branchAction) "what to do to which branch" ]
      returnType = resultType (tref Refs.branchDone)
      description =
        "Starts, renames, switches to or archives a branch. Starting resolves a name to the "
        + "live branch holding it, creating it if none does. Switching moves this process "
        + "and stores the branch for the next command. Archiving deletes the branch's ops, "
        + "the rows they wrote and its bases, and marks it archived, in one transaction "
        + "(a merged branch is only marked)."
      fn =
        (function
        | _, _, _, [| action |] ->
          uply {
            let done' = outcome (kt Refs.branchDone) branchDoneToDT
            match action with
            | DEnum(_, _, _, "Start", [ DString name; DUuid parent ]) ->
              let! d = Actions.start name (branch parent)
              return done' (Ok d)
            | DEnum(_, _, _, "Rename", [ DUuid b; DString name ]) ->
              let! r = Actions.rename (branch b) name
              return done' r
            | DEnum(_, _, _, "Switch", [ DUuid b ]) ->
              let! d = Actions.switchTo (branch b)
              return done' (Ok d)
            | DEnum(_, _, _, "Archive", [ DUuid b ]) ->
              let! r = Actions.archive (branch b)
              return done' r
            | other ->
              return Exception.raiseInternal "unknown BranchAction" [ "got", other ]
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "scmMerge" 0
      typeParams = []
      parameters =
        [ Param.make "action" (tref Refs.mergeAction) "merge, rebase, or only look" ]
      returnType = resultType (tref Refs.reconciled)
      description =
        "Merges a branch into its parent, rebases it onto its parent, or previews the "
        + "conflicts a merge would record. Both first repoint the outdated callers they are "
        + "handed. A merge is then one transaction, which also carries the branch's pending "
        + "conflicts to the parent; values that land in main are evaluated after it. A rebase "
        + "then moves every base, takes the parent's version where the parent's newer edit "
        + "won, and records the conflicts in one transaction. Conflicts are detected here, "
        + "against the rule the fold applies."
      fn =
        (function
        | exeState, vm, _, [| action |] ->
          uply {
            let reconciled = outcome (kt Refs.reconciled) reconciledToDT
            // One repoint, on the branch: a whole propagation, as an edit there would make.
            let repointOn
              (b : System.Guid)
              (rp : Actions.Repoint)
              : System.Threading.Tasks.Task<unit> =
              uply {
                let! _ =
                  Packages.repoint
                    (branch b)
                    rp.dependency
                    rp.kind
                    [ rp.fromHash ]
                    rp.toHash
                return ()
              }
              |> Ply.toTask
            match action with
            | DEnum(_, _, _, "IntoParent", [ DUuid b; repoints ]) ->
              let repoints = listOf repointFromDT repoints
              let! r =
                Actions.merge (branch b) repoints (repointOn b) (fun () ->
                  PackageOps.evaluateLandedValues exeState vm |> Ply.toTask)
              return reconciled r
            | DEnum(_, _, _, "Rebase", [ DUuid b; repoints ]) ->
              let repoints = listOf repointFromDT repoints
              let! r = Actions.rebase (branch b) repoints (repointOn b)
              return reconciled r
            | DEnum(_, _, _, "Preview", [ DUuid b ]) ->
              let! conflicts = Actions.preview (branch b)
              return
                reconciled (
                  Ok
                    { Actions.Reconciled.moved = 0L
                      conflicts = conflicts
                      diverged = []
                      repointed = []
                      commit = ""
                      warning = "" }
                )
            | other ->
              return Exception.raiseInternal "unknown MergeAction" [ "got", other ]
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "scmDiscard" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "whose draft"
          Param.make "what" (tref Refs.discardWhat) "which part of it goes" ]
      returnType = resultType TInt64
      description =
        "Drops ops from a draft and answers how many went. On main it deletes the rows the "
        + "dropped ops wrote and re-lists what they superseded, or rebuilds the draft when "
        + "that cannot be done surgically; an op this build cannot decode is never deleted. "
        + "On a branch it deletes the uncommitted ops no other branch holds and drops this "
        + "branch's tags on them. Either way in one transaction."
      fn =
        (function
        | _, _, _, [| DUuid b; what |] ->
          uply {
            let! r = Actions.discard (branch b) (discardWhatFromDT what)
            return outcome KTInt64 (fun n -> DInt64(int64 n)) r
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "scmResolve" 0
      typeParams = []
      parameters =
        [ Param.make "resolution" (tref Refs.resolution) "how a conflict ends" ]
      returnType = resultType TUnit
      description =
        "Settles a conflict. Keep-mine and take-theirs rewrite the branch's binding and its "
        + "base in one transaction. Overriding, taking a removal or rewording authors the op "
        + "that says so, closes the record, and (for an override) repoints the callers of "
        + "the losing version. Ack and supersede close the record."
      fn =
        (function
        | exeState, vm, _, [| r |] ->
          uply {
            let! result = resolve exeState vm pm r
            return outcome KTUnit (fun () -> DUnit) result
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "scmDetect" 0
      typeParams = []
      parameters =
        [ Param.make
            "request"
            (tref Refs.detectRequest)
            "both sides, their bases, and labels" ]
      returnType = TList(tref Refs.conflict)
      description =
        "The conflicts between two sides: names both moved since their base (or, with no "
        + "base, both created or both edited from one parent), and removals of names the "
        + "local side has since edited. Each names the winner the fold will pick."
      fn =
        (function
        | _, _, _, [| DRecord(_, _, _, f) |] ->
          let localLabel = str f "localLabel"
          let incomingLabel = str f "incomingLabel"
          let local = listOf bindingFromDT (field f "local")
          let divergences =
            Detect.detect
              localLabel
              incomingLabel
              local
              (listOf bindingFromDT (field f "incoming"))
              (listOf nameBaseFromDT (field f "bases"))
          let removals =
            Detect.detectRemovals
              localLabel
              incomingLabel
              local
              (listOf removalFromDT (field f "removals"))
          Ply(conflictList (divergences @ removals))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "scmLwwBeats" 0
      typeParams = []
      parameters =
        [ Param.make "aTs" TString "the first side's stamp"
          Param.make "aHash" TString "the first side's hash"
          Param.make "bTs" TString "the second side's stamp, \"\" for none"
          Param.make "bHash" TString "the second side's hash" ]
      returnType = TBool
      description =
        "Whether the first side beats the second under the fold's last-writer-wins rule: "
        + "the later stamp, an exact tie to the higher hash, and a side with no stamp never "
        + "defends itself."
      fn =
        (function
        | _, _, _, [| DString aTs; DString aHash; DString bTs; DString bHash |] ->
          Ply(DBool(LibExecution.Lww.incomingWins aTs aHash bTs bHash))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins (pm : PT.PackageManager) : Builtins =
  LibExecution.Builtin.make [] (fns pm)
