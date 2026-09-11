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
  | Permission.Layer.Package id -> $"package policy `{id}`"
  | Permission.Layer.Function id -> $"function policy `{id}`"

let private reasonText (reason : Permission.PolicyDenial) : string =
  match reason with
  | Permission.PolicyDenial.ExplicitlyDenied -> "explicitly denied"
  | Permission.PolicyDenial.NotAllowed -> "not allowed"

/// The one denial message. `suggestion` is the exact `permissions allow`
/// argument that would cover the denied request; it is only actionable for an
/// instance-layer denial, since that is the policy `permissions allow` edits.
/// Other layers need the approve/ceiling changes the remedy names.
let private denialMessage
  (resource : string)
  (reason : Permission.PolicyDenial)
  (layer : Permission.Layer)
  (suggestion : Option<string>)
  : string =
  let remedy =
    match layer, suggestion with
    | Permission.Layer.Instance, Some rule ->
      $"To allow: `permissions allow {rule}`."
    | Permission.Layer.Instance, None -> "Configure the instance policy."
    | Permission.Layer.Run, _ -> "Start the run with a broader permission policy."
    | Permission.Layer.Package _, _ ->
      // Approval is by logical name, not by the policy id printed above: the
      // id names the immutable version, and the name is what a person has.
      "To approve: `permissions approve <fn>`, naming the function that needs it."
    | Permission.Layer.Function _, _ ->
      "The function's declared ceiling does not allow this operation."
  $"permission denied by {layerName layer}: {resource} is {reasonText reason}. {remedy}"

let private raiseDenial
  (state : ExecutionState)
  (resource : string)
  (reason : Permission.PolicyDenial)
  (layer : Permission.Layer)
  (suggestion : Option<string>)
  : 'a =
  state.deniedRequests.Add
    { layer = layer; resource = resource; suggestion = suggestion }
  RuntimeError.UncaughtException(denialMessage resource reason layer suggestion, [])
  |> raiseUntargetedRTE

let private raiseRejected (message : string) : 'a =
  RuntimeError.UncaughtException(message, []) |> raiseUntargetedRTE

/// Where execution stood when a violation was recorded. Only needed on a
/// denial in audit mode, so the frame lookup is deferred to that point.
let private executionPoint (vm : VMState) : Option<ExecutionPoint> =
  match vm.callFrames.TryGetValue vm.currentFrameID with
  | true, frame -> Some frame.executionPoint
  | false, _ -> None

/// Audit mode (`--warn-permissions`) records a denial from a caller-owned
/// layer and proceeds; `Permissions.Access.decide` keeps the operator-owned
/// instance policy a hard maximum.
let private relaxFor (state : ExecutionState) (vm : VMState) : Permission.Relax =
  match state.permissionWarnings with
  | Some sink ->
    Permission.Relax(fun resource layer reason ->
      recordPermissionViolation
        sink
        (executionPoint vm)
        $"{resource} ({layerName layer})"
        (reasonText reason))
  | None -> Permission.NoRelax

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
  let relax = relaxFor state vm
  match Permission.Access.decide relax resource request vm.activeAccess with
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
  (vm : VMState)
  (access : Permission.Access)
  (resource : unit -> string)
  (request : Permission.Request)
  : unit =
  let relax = relaxFor state vm
  match Permission.Access.decide relax resource request access with
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
  (vm : VMState)
  (access : Permission.Access)
  (op : Host.Operation)
  : Ply<Result<Host.Response, Host.Failure>> =
  uply {
    let! outcome = Host.perform (relaxFor state vm) access op
    match outcome with
    | Host.Outcome.Success response -> return Ok response
    | Host.Outcome.Failed failure -> return Error failure
    | Host.Outcome.Rejected message -> return raiseRejected message
    | Host.Outcome.Denied(layer, reason, resource, suggestion) ->
      return raiseDenial state resource reason layer suggestion
  }

let performHost
  (state : ExecutionState)
  (vm : VMState)
  (op : Host.Operation)
  : Ply<Result<Host.Response, Host.Failure>> =
  performHostWithAccess state vm vm.activeAccess op

/// Builtins that only BUNDLED first-party Dark may call, whatever the policy says.
///
/// The second gate, and the one that is easy to miss. A builtin's effects are declared, checked
/// before the body runs, printed by `dark permissions`, and carried in a manifest. This one is a
/// call inside a body, so without a list of its own nothing outside that body can see it: someone
/// reading what `Instance` reaches would see `package-read` and have no way to learn that some of
/// those builtins refuse a pulled package outright.
///
/// It exists for the cases no rule can scope honestly. `localDbBackupTo` writes this instance's
/// whole store to a path the caller names; the sync transport reaches loopback and the tailnet,
/// which the safe HTTP client bans. Both would have to declare `native` to be honest in the effect
/// vocabulary, and `native` is all-or-nothing, so a stock install would need `allow native` to run
/// `dark sync`. Caller trust is the narrower answer, and this makes it a visible one.
///
/// Lives here rather than beside the catalog because it describes a RUNTIME trust mechanism, not
/// a fact about which platform ships what, and because everything that has to read it sits below
/// the catalog. Pinned by a test against the source, so the list cannot drift from the calls.
let firstPartyOnly : Set<string> =
  Set.ofList
    [ "httpGetUnsafeBytes"
      "httpGetUnsafeBytesStart"
      "httpGetUnsafeBytesWithHeaders"
      "httpPostUnsafeBytes"
      "localDbBackupTo"
      "localDbRestoreFrom" ]


/// Is every non-root frame bundled first-party (Darklang) code?
///
/// The trust question behind the two builtins nobody can scope honestly: the private-network
/// sync transport, and raw SQLite against this instance's own store. Checking the immediate
/// caller is not enough -- an untrusted package can call a bundled wrapper and turn it into a
/// confused deputy -- so every frame in the chain must be bundled.
let callerIsBundled (state : ExecutionState) (vm : VMState) : bool =
  let rec fnOf (point : ExecutionPoint) : Option<FQFnName.Package> =
    match point with
    | ExecutionPoint.Function(FQFnName.Package p) -> Some p
    | ExecutionPoint.Lambda(parent, _) -> fnOf parent
    | _ -> None
  let rec allBundled (frameID : System.Guid) (sawPackage : bool) : bool =
    match vm.callFrames.TryGetValue frameID with
    | false, _ -> false
    | true, frame ->
      let frameTrusted, sawPackage =
        match fnOf frame.executionPoint with
        | Some p -> state.isBundledPackageFn p, true
        // Source-derived frames -- the host's own expression, and lambdas defined in it --
        // are trusted here; what separates the CLI from a guest is the caller's own posture,
        // checked by whoever calls this. Requiring a parentless frame refused the CLI itself,
        // which reaches its work through a lambda of its entry expression.
        | None -> true, sawPackage
      if not frameTrusted then
        false
      else
        match frame.parent with
        | ValueNone -> sawPackage
        | ValueSome(parentID, _, _) -> allBundled parentID sawPackage
  allBundled vm.currentFrameID false

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
  for effect in effects do
    if not (Effect.isScoped effect) then
      checkRequestWithAccess
        state
        vm
        access
        (fun () -> $"`{builtinName}` ({Effect.name effect})")
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

/// Check every effect an OUT-OF-PROCESS platform declared, before its process is asked to do
/// anything.
///
/// The ambient check above deliberately skips scoped effects, because a linked builtin's body
/// builds an `Operation` naming the resource and the host boundary checks that instead. A platform
/// in another process has no body here. It performs its own I/O, inside itself, and the host never
/// learns which URL or which path. So the boundary check never happens, and before this existed a
/// manifest declaring `http` got the network with the policy never consulted.
///
/// The only question that can honestly be asked is whether the WHOLE effect was granted, and that
/// is what `Request.WholeEffect` asks. A narrow rule deliberately does not satisfy it: nothing
/// would hold the platform to the URL in the rule.
///
/// The cost is real and belongs to running somebody else's process, not to this check: an external
/// platform that wants the network needs `permissions allow http`, where a linked one can be held
/// to one host. That is a reason to prefer a linked platform for a scoped effect, which is true.
let requireDescribedPlatformEffects
  (state : ExecutionState)
  (vm : VMState)
  (effects : Set<Effect.Effect>)
  (builtinName : string)
  : unit =
  for effect in effects do
    let request =
      if Effect.isScoped effect then
        Permission.Request.wholeEffect effect
      else
        Permission.Request.ofAmbientEffect effect builtinName
    checkRequestWithAccess
      state
      vm
      vm.activeAccess
      (fun () -> $"`{builtinName}` ({Effect.name effect})")
      request

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

// ── datastore ─────────────────────────────────────────────────────────────────

let requireDbRead (state : ExecutionState) (vm : VMState) (table : string) : unit =
  check
    state
    vm
    (fun () -> $"reading datastore `{table}`")
    (Permission.Request.db Permission.AccessKind.Read table)

/// Listing all datastores requires the unscoped datastore-read permission,
/// because the request does not name one table.
let requireDbReadAll (state : ExecutionState) (vm : VMState) : unit =
  check state vm (fun () -> "listing all datastores") (Ok Permission.Request.dbList)

let requireDbWrite (state : ExecutionState) (vm : VMState) (table : string) : unit =
  check
    state
    vm
    (fun () -> $"writing datastore `{table}`")
    (Permission.Request.db Permission.AccessKind.Write table)
