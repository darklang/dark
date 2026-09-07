/// Which branch a PROCESS runs on: `--branch` (this command), then `DARK_BRANCH` (this shell), then the
/// stored `current_branch` (this machine). Each tier is scoped tighter than the one below, which is what
/// lets several agents work on several branches at once without fighting over one config key.
///
/// Out of `Cli.main` so the order is a function with a test, rather than a match expression only a real
/// process could exercise. What to PRINT about the outcome stays in `Cli.main`; this says what happened.
module LibDB.BranchSelection

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude

module PT = LibExecution.ProgramTypes

type Tier =
  | Flag
  | Env
  | Stored
  | Default

/// A ref that the flag or the env tier could not turn into a branch, with the reason: these are refused,
/// not created, because a branch minted under a peer's uuid would read as success.
type Refusal =
  | AmbiguousPrefix of string
  | UnknownId of PT.BranchId

type Selection =
  {
    /// None is main.
    branchId : Option<PT.BranchId>
    tier : Tier
    /// The flag and env tiers CREATE a name they don't know; this names it when they did.
    created : Option<string>
    /// A stored branch that is archived or merged degrades to main; this names it. The config is reset
    /// to main when this is set, so it is said once.
    goneStored : Option<string>
  }

let private none =
  { branchId = None; tier = Default; created = None; goneStored = None }

/// Point the stored tier back at main. Said once: left in place, every command from here on would
/// repeat that the branch is gone.
let private resetStoredToMain () : Task<unit> =
  task {
    do! Config.set "current_branch" (string PT.BranchId.Main)
    do! Config.set "current_branch_name" PT.BranchId.MainName
  }

/// The flag and env tiers: a name, a full id, or an unambiguous id prefix. `main` is spelled as the
/// absence of a branch. Only a name nobody has is created.
let private resolveName
  (name : string)
  : Task<Result<Option<PT.BranchId> * Option<string>, Refusal>> =
  task {
    if name = PT.BranchId.MainName then
      return Ok(None, None)
    else
      match! Branches.lookupRef name with
      | Branches.Found id -> return Ok(Some id, None)
      | Branches.Ambiguous prefix -> return Error(AmbiguousPrefix prefix)
      | Branches.UnknownId id -> return Error(UnknownId id)
      | Branches.NoSuchName name ->
        let! (id, created) = Branches.resolveOrCreate name PT.BranchId.Main
        return Ok(Some id, (if created then Some name else None))
  }

/// The stored tier: text, so it can be an id, a name, or something a previous build wrote. Resolves
/// without creating, and degrades to main rather than failing, saying which branch is gone.
let private fromStored () : Task<Selection> =
  task {
    match! Config.get "current_branch" with
    | Some stored when stored <> "" ->
      match PT.BranchId.Parse stored with
      | Some id when id = PT.BranchId.Main -> return { none with tier = Stored }
      | Some id ->
        let! live = Branches.isLive id
        if live then
          return { none with branchId = Some id; tier = Stored }
        else
          match! Branches.liveIdForName stored with
          | Some id -> return { none with branchId = Some id; tier = Stored }
          | None ->
            let! label = Config.get "current_branch_name"
            let label =
              match label with
              | Some n when n <> "" -> n
              | _ -> stored
            do! resetStoredToMain ()
            return { none with tier = Stored; goneStored = Some label }
      | None ->
        match! Branches.liveIdForName stored with
        | Some id -> return { none with branchId = Some id; tier = Stored }
        | None ->
          do! resetStoredToMain ()
          return { none with tier = Stored; goneStored = Some stored }
    | _ -> return none
  }

/// The one order. `flag` is `--branch`'s value, `env` is `DARK_BRANCH`; either may be absent.
let select
  (flag : Option<string>)
  (env : Option<string>)
  : Task<Result<Selection, Refusal>> =
  task {
    match flag, env with
    | Some name, _ ->
      match! resolveName name with
      | Ok(id, created) ->
        return Ok { none with branchId = id; tier = Flag; created = created }
      | Error r -> return Error r
    | None, Some name when name <> "" ->
      match! resolveName name with
      | Ok(id, created) ->
        return Ok { none with branchId = id; tier = Env; created = created }
      | Error r -> return Error r
    | _ ->
      let! s = fromStored ()
      return Ok s
  }
