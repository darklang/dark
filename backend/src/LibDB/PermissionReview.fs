/// Check access that can be determined before running, and ask the user
/// for approval through the trusted host. Do not run user code to find
/// out which permissions it needs.
module LibDB.PermissionReview

open System.Threading.Tasks

open Prelude
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module P = LibExecution.Permissions

/// A known operation and the named entry whose approval will apply to it.
type KnownRequest = { request : P.Request; entry : Option<Option<string> * RT.Hash> }

let atRoot request : KnownRequest = { request = request; entry = None }

[<RequireQualifiedAccess>]
type private Known =
  | Unknown
  | Text of string
  | Number of int64
  | Bool of bool
  | Unit

/// Follow only unconditional calls and branches whose conditions are literals.
/// Unknown branches, callbacks and deferred bodies are left to runtime checks.
let preflight
  (load : PackagePermissions.LoadFn)
  (policyFor : Option<string> -> RT.Hash -> Option<P.Policy>)
  (expressions : List<PT.Expr>)
  : List<KnownRequest> =
  let requests = ResizeArray<KnownRequest>()
  let mutable budget = 2000
  let add entry =
    function
    | Ok request -> requests.Add { request = request; entry = entry }
    | Error _ -> ()
  let builtin entry name args =
    match name, args with
    | "runtimeEnvironmentGet", [ Known.Text name ] ->
      add entry (P.Request.env P.AccessKind.Read name)
    | "runtimeEnvironmentGetAll", _ -> add entry (Ok P.Request.envList)
    | ("httpClientRequest" | "httpClientStream"),
      Known.Text method :: Known.Text url :: _ ->
      add entry (P.Request.http method url)
    | "runtimeAuthorizeReadPath", Known.Text path :: _
    | "syscallStatAt", Known.Text path :: _
    | "syscallChdir", Known.Text path :: _
    | "syscallReadlinkAt", Known.Text path :: _ ->
      add entry (P.Request.file P.AccessKind.Read path)
    | ("syscallMkdir" | "syscallUnlinkAt" | "syscallChmod"), Known.Text path :: _ ->
      add entry (P.Request.file P.AccessKind.Write path)
    | "kernel.open", Known.Text path :: Known.Number flags :: _ ->
      if
        flags >= int64 System.Int32.MinValue && flags <= int64 System.Int32.MaxValue
      then
        let reads, writes = LibExecution.HostPosix.openAccess (int flags)
        if reads then add entry (P.Request.file P.AccessKind.Read path)
        if writes then add entry (P.Request.file P.AccessKind.Write path)
    | _ -> ()
    Known.Unknown, true
  let rec walk entry stack env args expr : Known * bool =
    budget <- budget - 1
    if budget < 0 then
      Known.Unknown, false
    else
      match expr with
      | PT.EString(_, segments) when
        segments
        |> List.forall (function
          | PT.StringText _ -> true
          | _ -> false)
        ->
        Known.Text(
          String.concat
            ""
            (segments
             |> List.choose (function
               | PT.StringText s -> Some s
               | _ -> None))
        ),
        true
      | PT.EInt64(_, n) -> Known.Number n, true
      | PT.EBool(_, b) -> Known.Bool b, true
      | PT.EUnit _ -> Known.Unit, true
      | PT.EArg(_, index) ->
        List.tryItem index args |> Option.defaultValue Known.Unknown, true
      | PT.EVariable(_, name) ->
        Map.tryFind name env |> Option.defaultValue Known.Unknown, true
      | PT.ELet(_, pattern, value, body) ->
        let value, continues = walk entry stack env args value
        if not continues then
          Known.Unknown, false
        else
          let env =
            match pattern with
            | PT.LPVariable(_, name) -> Map.add name value env
            | _ -> env
          walk entry stack env args body
      | PT.EStatement(_, first, next) ->
        let _, continues = walk entry stack env args first
        if continues then walk entry stack env args next else Known.Unknown, false
      | PT.EIf(_, condition, yes, no) ->
        let condition, continues = walk entry stack env args condition
        match condition, continues with
        | Known.Bool true, true -> walk entry stack env args yes
        | Known.Bool false, true ->
          match no with
          | Some no -> walk entry stack env args no
          | None -> Known.Unit, true
        | _ -> Known.Unknown, false
      | PT.EApply(_, PT.EFnName(_, nr, _, _), _, arguments) ->
        let values = walkMany entry stack env args (NEList.toList arguments)
        if values |> List.exists (fun (_, continues) -> not continues) then
          Known.Unknown, false
        else
          let values = List.map fst values
          match nr.resolved with
          | Ok { name = PT.FQFnName.Builtin b } -> builtin entry b.name values
          | Ok { name = PT.FQFnName.Package hash; location = location } when
            not (Set.contains hash stack)
            ->
            match load hash with
            | Some fn ->
              let bindings =
                fn.parameters
                |> NEList.toList
                |> List.mapi (fun index parameter ->
                  parameter.name,
                  List.tryItem index values |> Option.defaultValue Known.Unknown)
                |> Map.ofList
              let name =
                location
                |> Option.map (fun loc ->
                  String.concat "." (loc.owner :: loc.modules @ [ loc.name ]))
              let runtimeHash =
                LibExecution.ProgramTypesToRuntimeTypes.Hash.toRT hash
              let entry =
                match entry, policyFor name runtimeHash with
                | None, Some _ -> Some(name, runtimeHash)
                | _ -> entry
              walk entry (Set.add hash stack) bindings values fn.body
            | None -> Known.Unknown, false
          | _ -> Known.Unknown, false
      | PT.EExtern(id, library, symbol, arguments) ->
        walk entry stack env args (PT.Expr.externCall id library symbol arguments)
      | PT.EApply(_, callee, _, arguments) ->
        let _, continues = walk entry stack env args callee
        if continues then
          let _ = walkMany entry stack env args (NEList.toList arguments)
          ()
        Known.Unknown, false
      | PT.EPipe(_, first, _) ->
        let _ = walk entry stack env args first
        Known.Unknown, false
      | PT.EInfix(_, PT.BinOp _, left, _, _) ->
        let _ = walk entry stack env args left
        Known.Unknown, false
      | PT.ELambda _
      | PT.EFnName _ -> Known.Unknown, true
      | PT.EMatch(_, subject, _)
      | PT.EUnwrap(_, subject) ->
        let _ = walk entry stack env args subject
        Known.Unknown, false
      | _ ->
        let values =
          walkMany entry stack env args (LibExecution.ProgramTypesAst.subExprs expr)
        Known.Unknown, List.forall snd values
  and walkMany entry stack env args expressions =
    match expressions with
    | [] -> []
    | first :: rest ->
      let value, continues = walk entry stack env args first
      if not continues then
        [ value, false ]
      else
        let remaining = walkMany entry stack env args rest
        (value, true) :: remaining
  let mutable continues = true
  for expression in expressions do
    if continues then
      let _, next = walk None Set.empty Map.empty [] expression
      continues <- next
  requests |> Seq.distinct |> List.ofSeq

/// Configure one immutable entry. Prompts run in the trusted caller, serially;
/// grants belong to this run's exact invocation contexts and are never serialized.
/// Also returns whether the user has cancelled a prompt, which ends the run.
let configure
  (guest : RT.ExecutionState)
  (name : string)
  (hash : string)
  (script : bool)
  (knownRequests : List<KnownRequest>)
  (ask : Option<List<string> -> Task<int64>>)
  : Task<RT.ExecutionState * (unit -> bool)> =
  let fingerprint = lazy (PolicyStore.builtinFingerprint guest.fns.builtIn)
  let initial = PolicyStore.get ()
  let key = guest.accountID, name
  let saved =
    match Map.tryFind key initial.approvals with
    | Some approval when
      approval.hash = hash && approval.fingerprint = fingerprint.Value
      ->
      approval.policy
    | _ -> P.Policy.denyAll
  let baseAccess =
    if script then
      P.Access.grantRun saved guest.access
    else
      P.Access.enter name saved guest.access
  let grants = System.Collections.Generic.Dictionary<P.Access, P.Policy>()
  let gate = new System.Threading.SemaphoreSlim(1, 1)
  let mutable cancelled = false
  let resolve access =
    lock grants (fun () ->
      let access =
        match grants.TryGetValue access with
        | true, policy -> P.Access.approve policy access
        | _ -> access
      if cancelled then
        P.Access.restrict P.Layer.Run P.Policy.denyAll access
      else
        let _, denies = P.Policy.rules (PolicyStore.instancePolicy ())
        P.Access.restrict
          P.Layer.Instance
          (P.Policy.create [ P.Rule.All ] denies)
          access)
  let describe request =
    match P.Request.effect request with
    | LibExecution.Effects.Effect.Native ->
      "native (unrestricted host access; Dark cannot limit its resources)"
    | _ ->
      P.Request.suggestRule request
      |> P.Suggestion.rule
      |> Option.defaultValue (LibExecution.Effects.name (P.Request.effect request))
  let review access requests : Task<bool> =
    task {
      do! gate.WaitAsync()
      try
        let current = resolve access
        let missing =
          requests |> List.filter (fun r -> not (P.Access.allows r current))
        if List.isEmpty missing then
          return true
        elif
          cancelled
          || not (List.forall (fun r -> P.Access.canApprove r current) missing)
        then
          return false
        else
          match ask with
          | None -> return false
          | Some ask ->
            let before = PolicyStore.get ()
            let! choice = ask (List.map describe missing)
            if choice <> 1L && choice <> 2L then
              lock grants (fun () -> cancelled <- true)
              return false
            elif before <> PolicyStore.get () then
              lock grants (fun () -> cancelled <- true)
              return
                RT.RuntimeError.UncaughtException(
                  "Permissions changed while you were deciding. Run again to review current access.",
                  []
                )
                |> RT.raiseRTE
            else
              let added = missing |> List.map P.Rule.forRequest
              if choice = 2L then
                PolicyStore.rememberReviewed
                  before
                  guest.accountID
                  name
                  hash
                  fingerprint.Value
                  added
              lock grants (fun () ->
                let existing =
                  match grants.TryGetValue access with
                  | true, p -> fst (P.Policy.rules p)
                  | _ -> []
                grants[access] <-
                  P.Policy.create (List.distinct (existing @ added)) [])
              return true
      finally
        gate.Release() |> ignore<int>
    }
  task {
    // Saved script grants work unattended too. With no prompt installed there
    // is no mutable interactive authority to carry into callbacks or workers.
    let state =
      { guest with
          access = baseAccess
          permissionAccess = if ask.IsSome then resolve else (fun access -> access)
          requestPermission =
            ask
            |> Option.map (fun _ ->
              fun access request _ -> review access [ request ]) }
    let context item =
      let access =
        match item.entry with
        | None -> baseAccess
        | Some(name, hash) ->
          match guest.packagePolicy name hash with
          | None -> baseAccess
          | Some policy ->
            P.Access.enter (Option.defaultValue (string hash) name) policy baseAccess
      state.permissionAccess access
    let missing =
      knownRequests
      |> List.filter (fun item -> not (P.Access.allows item.request (context item)))
    let blocked =
      missing
      |> List.tryFind (fun item ->
        not (P.Access.canApprove item.request (context item)))
    if not (List.isEmpty missing) then
      let! approved =
        match blocked with
        | Some _ -> Task.FromResult false
        | None -> review baseAccess (List.map (fun item -> item.request) missing)
      if not approved && not cancelled then
        let item = blocked |> Option.defaultValue (List.head missing |> Option.get)
        let denial = P.Access.decide item.request (context item) |> Option.get
        LibExecution.PermissionCheck.raiseDenial
          guest
          (describe item.request + $" (before running {name})")
          denial.reason
          denial.layer
          (P.Request.suggestRule item.request)
    // Preflight grants are installed before any function captures its access.
    return
      { state with access = state.permissionAccess baseAccess },
      (fun () -> cancelled)
  }
