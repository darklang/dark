/// Exact permission checks at host-effect boundaries.
///
/// Static `Effects` describe behavior for analysis. This module is the single
/// runtime access path: a scoped builtin constructs its exact `Request`
/// immediately before using the host resource, and every active policy must
/// allow it. OS-facing builtins go through `performHost`, which hands an
/// `Operation` to the checked host boundary; store-facing builtins such as
/// datastores use the `require*` helpers below. Operations that cannot be
/// scoped honestly, such as arbitrary SQLite SQL, are `Native` instead.
module LibExecution.PermissionCheck

open Prelude
open LibExecution.RuntimeTypes
module Permission = LibExecution.Permissions
module Effect = LibExecution.Effects

let private layerName (layer : Permission.Layer) : string =
  match layer with
  | Permission.Layer.Instance -> "instance policy"
  | Permission.Layer.Run -> "run policy"
  | Permission.Layer.Package name -> $"the access of `{name}`"
  | Permission.Layer.Value name -> $"value `{name}`"

let private reasonText (reason : Permission.PolicyDenial) : string =
  match reason with
  | Permission.PolicyDenial.ExplicitlyDenied -> "explicitly denied"
  | Permission.PolicyDenial.NotAllowed -> "not allowed"

/// The one denial message. `suggestion` is the exact `permissions allow`
/// argument that would cover the denied request, or why none is offered; it is
/// only actionable for an instance-layer denial, since that is the policy
/// `permissions allow` edits. Other layers need the approval changes the
/// remedy names.
let private denialMessage
  (resource : string)
  (reason : Permission.PolicyDenial)
  (layer : Permission.Layer)
  (suggestion : Permission.Suggestion)
  : string =
  let remedy =
    match layer, suggestion with
    | Permission.Layer.Instance, _ when
      reason = Permission.PolicyDenial.ExplicitlyDenied
      ->
      "Remove or narrow the matching deny rule. Show the rules:\n"
      + "  dark permissions list"
    | Permission.Layer.Instance, Permission.Suggestion.Paste rule ->
      $"To allow:\n  dark permissions allow {rule}"
    | Permission.Layer.Instance, Permission.Suggestion.File(rule, folder, profile) ->
      $"To allow:\n  dark permissions allow {rule}\n\n"
      + $"For everything in its folder:\n  dark permissions allow {folder}\n\n"
      + $"Preview a policy for working in that folder:\n  dark permissions profile {profile}\n"
      + "  This replaces the whole instance policy; add --yes to apply it."
    | Permission.Layer.Instance, Permission.Suggestion.Process(rule, executableRule) ->
      $"To allow:\n  dark permissions allow {rule}\n"
      + "  This allows this exact command only.\n\n"
      + $"To allow this program with any arguments:\n  dark permissions allow {executableRule}\n"
      + "For a shell, this allows any command run through that shell."
    | Permission.Layer.Instance, Permission.Suggestion.NoRule ->
      "Configure the instance policy."
    | Permission.Layer.Run, _ -> "Start the run with a broader permission policy."
    | Permission.Layer.Package name, _ when
      reason = Permission.PolicyDenial.ExplicitlyDenied
      ->
      $"Its remembered approval denies this; `dark permissions show {name}` shows it."
    | Permission.Layer.Package name, _ ->
      $"Remember access with `dark permissions approve {name}` (`permissions update {name}` "
      + "after an edit), or allow it for one run with `dark run --allow`."
    | Permission.Layer.Value _, _ ->
      "A `val` is evaluated once, when it is saved, so it cannot use effects. "
      + "Make it a function to do this each time it is called."
  let runnerHint =
    match layer, reason, Permission.Suggestion.rule suggestion with
    | Permission.Layer.Instance, Permission.PolicyDenial.NotAllowed, Some _ ->
      "\n\nAgents:\n"
      + "  Submit the exact allow command through your runner's approval flow.\n"
      + "  The current approval mode applies.\n"
      + "  After it succeeds, retry only if earlier side effects will not be repeated."
    | _ -> ""
  $"permission denied by {layerName layer}:\n"
  + $"  {resource} is {reasonText reason}.\n\n{remedy}{runnerHint}"

/// Record and report a denied operation, including checks before a run starts.
let raiseDenial
  (state : ExecutionState)
  (resource : string)
  (reason : Permission.PolicyDenial)
  (layer : Permission.Layer)
  (suggestion : Permission.Suggestion)
  : 'a =
  // Shared by every process spawned under this state, on any scheduler thread; the host reads
  // the list after the run, so a child's denial has to land in the parent's list, not a copy.
  lock state.deniedRequests (fun () ->
    state.deniedRequests.Add
      { layer = layer
        resource = resource
        suggestion =
          if reason = Permission.PolicyDenial.ExplicitlyDenied then
            None
          else
            Permission.Suggestion.rule suggestion })
  RuntimeError.UncaughtException(denialMessage resource reason layer suggestion, [])
  |> raiseUntargetedRTE

let private raiseRejected (message : string) : 'a =
  RuntimeError.UncaughtException(message, []) |> raiseUntargetedRTE

/// Check one exact request against the access the running builtin was
/// applied under (`vm.activeAccess`: the frame's, narrowed by the callee's
/// capture). `resource` is only rendered on denial, so allowed calls pay
/// nothing for the description.
let private checkRequest
  (state : ExecutionState)
  (vm : VMState)
  (resource : unit -> string)
  (request : Permission.Request)
  : unit =
  match
    Permission.Access.decide request (state.permissionAccess vm.activeAccess)
  with
  | None -> ()
  | Some denial ->
    raiseDenial
      state
      (resource ())
      denial.reason
      denial.layer
      (Permission.Request.suggestRule request)

let private checkRequestWithAccess
  (state : ExecutionState)
  (_vm : VMState)
  (access : Permission.Access)
  (resource : unit -> string)
  (request : Permission.Request)
  : unit =
  match Permission.Access.decide request (state.permissionAccess access) with
  | None -> ()
  | Some denial ->
    raiseDenial
      state
      (resource ())
      denial.reason
      denial.layer
      (Permission.Request.suggestRule request)

/// `checkRequest` for a request built from guest input. Invalid requests are
/// always errors; audit mode cannot turn malformed input into a wildcard.
let private check
  (state : ExecutionState)
  (vm : VMState)
  (resource : unit -> string)
  (request : Result<Permission.Request, string>)
  : unit =
  match request with
  | Error message -> raiseRejected $"permission request rejected: {message}"
  | Ok request -> checkRequest state vm resource request

/// Perform one host operation through the single checked boundary
/// (`Host.perform`), under the running builtin's access. Malformed requests
/// and policy denials raise runtime errors exactly like the `require*`
/// helpers; OS-level failures come back as structured data for the builtin to
/// surface as a guest-visible Result.
let performHostWithAccess
  (state : ExecutionState)
  (_vm : VMState)
  (access : Permission.Access)
  (op : Host.Operation)
  : Ply<Result<Host.Response, Host.Failure>> =
  let rec perform () =
    uply {
      let current = state.permissionAccess access
      let! outcome = Host.performInSession state.descriptorSession.Value current op
      match outcome with
      | Host.Outcome.Success response -> return Ok response
      | Host.Outcome.Failed failure -> return Error failure
      | Host.Outcome.Rejected message -> return raiseRejected message
      | Host.Outcome.Denied(layer, reason, resource, suggestion, request) ->
        let! approved =
          match state.requestPermission with
          | Some ask when Permission.Access.canApprove request current ->
            ask access request resource
          | _ -> Ply false
        // A denied host operation has done no work. Recheck this operation
        // after the user responds; never rerun the script or earlier effects.
        if approved then
          return! perform ()
        else
          return raiseDenial state resource reason layer suggestion
    }
  perform ()

/// Resolve approval for ambient effects before entering the synchronous builtin
/// path. Scoped effects ask at their host operation, where resources are known.
let approveBuiltinEffects state vm effects builtinName : Ply<unit> =
  uply {
    for effect in Effect.permissionEffects effects do
      if not (Effect.isScoped effect) then
        let request = Permission.Request.ofAmbientEffect effect builtinName
        let access = state.permissionAccess vm.activeAccess
        match Permission.Access.decide request access, state.requestPermission with
        | Some _, Some ask when Permission.Access.canApprove request access ->
          let! _ = ask vm.activeAccess request (Effect.name effect)
          ()
        | _ -> ()
  }

let performHost
  (state : ExecutionState)
  (vm : VMState)
  (op : Host.Operation)
  : Ply<Result<Host.Response, Host.Failure>> =
  performHostWithAccess state vm vm.activeAccess op

/// Check ambient effects against an explicitly supplied access. Builtins that
/// create a child guest state use this before host-side work performed on that
/// child's behalf; the invoking VM may belong to a broader trusted caller.
let requireBuiltinEffectsWithAccess
  (state : ExecutionState)
  (vm : VMState)
  (access : Permission.Access)
  (effects : Set<Effect.Effect>)
  (builtinName : string)
  : unit =
  for effect in Effect.permissionEffects effects do
    if not (Effect.isScoped effect) then
      checkRequestWithAccess
        state
        vm
        access
        (fun () ->
          match Effect.plainWords effect with
          | Some words -> $"`{builtinName}` ({Effect.name effect}: {words})"
          | None -> $"`{builtinName}` ({Effect.name effect})")
        (Permission.Request.ofAmbientEffect effect builtinName)

/// Check a builtin's ambient effects — the ones with no resource parameter —
/// before its body runs. Scoped effects are checked by the body (or by the
/// host boundary from the `Operation` it constructs).
let requireBuiltinEffects
  (state : ExecutionState)
  (vm : VMState)
  (effects : Set<Effect.Effect>)
  (builtinName : string)
  : unit =
  requireBuiltinEffectsWithAccess state vm vm.activeAccess effects builtinName

// ── filesystem ────────────────────────────────────────────────────────────────
// For the seed-export store builtin, which takes a path but does not go through
// a host `Operation`. Same guards as the boundary's file arms.

let private guardFile (write : bool) (path : string) : unit =
  match Host.fileGuard write path with
  | Some message -> raiseRejected message
  | None -> ()

let requireFileWrite (state : ExecutionState) (vm : VMState) (path : string) : unit =
  guardFile true path
  check
    state
    vm
    (fun () -> $"writing `{path}`")
    (Permission.Request.file Permission.AccessKind.Write path)

let requireFileReadWrite
  (state : ExecutionState)
  (vm : VMState)
  (path : string)
  : unit =
  guardFile true path
  check
    state
    vm
    (fun () -> $"reading `{path}`")
    (Permission.Request.file Permission.AccessKind.Read path)
  check
    state
    vm
    (fun () -> $"writing `{path}`")
    (Permission.Request.file Permission.AccessKind.Write path)
