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

open System.Threading.Tasks

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
/// passed to `permissions allow` for instance denials and to
/// `permissions approve --for` for named package denials.
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
      let target = Permission.Request.quoteRuleToken name
      "Its remembered approval denies this. Review it with:\n"
      + $"  dark permissions show {target}"
    | Permission.Layer.Package name, _ ->
      match Permission.Suggestion.rule suggestion with
      | Some rule ->
        let target = Permission.Request.quoteRuleToken name
        // Approve parses the rule text again, so preserve its token quotes in argv.
        let spec = Permission.Request.quoteRuleToken rule
        "To remember access for this function:\n"
        + $"  dark permissions approve --for {target} {spec} --yes"
      | None -> "Review the function's permission requirements."
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
  let explicitlyDenied = reason = Permission.PolicyDenial.ExplicitlyDenied
  lock state.deniedRequests (fun () ->
    state.deniedRequests.Add
      { layer = layer
        resource = resource
        suggestion =
          if explicitlyDenied then None else Permission.Suggestion.rule suggestion
        anyArgsRule =
          if explicitlyDenied then
            None
          else
            Permission.Suggestion.anyArgsRule suggestion })
  RuntimeError.UncaughtException(denialMessage resource reason layer suggestion, [])
  |> raiseRTE

let private raiseRejected (message : string) : 'a =
  RuntimeError.UncaughtException(message, []) |> raiseRTE

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
  : Task<Result<Host.Response, Host.Failure>> =
  let rec perform () =
    task {
      let current = state.permissionAccess access
      let! outcome =
        Host.performOnClock state.clock state.descriptorSession.Value current op
      match outcome with
      | Host.Outcome.Success response -> return Ok response
      | Host.Outcome.Failed failure -> return Error failure
      | Host.Outcome.Rejected message -> return raiseRejected message
      | Host.Outcome.Denied(layer, reason, resource, suggestion, request) ->
        let! approved =
          match state.requestPermission with
          | Some ask when Permission.Access.canApprove request current ->
            ask access request resource
          | _ -> Task.FromResult false
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
let approveBuiltinEffects state vm effects builtinName : Task<unit> =
  task {
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
  : Task<Result<Host.Response, Host.Failure>> =
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
