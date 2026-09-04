/// IL scan for resource effects: filesystem, process, socket, HTTP, and
/// environment APIs must be reached through the checked host modules.
/// Ambient effects (stdout, clock, randomness, and stdin) are checked at the
/// interpreter gate and are intentionally outside this scan. The scan covers
/// compiled IL, including helpers and closures; allowlist entries are trusted
/// holdouts and must carry a reason.
module Tests.HostBoundary

open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

open Expecto
open Prelude

/// Resource-effect OS types: their authorization is an exact `Request` that must
/// be built at the boundary, so a reference from outside the host modules is a
/// violation. (Ambient effects — Console/DateTime/Random — are gated at the
/// call site instead; see the module doc.)
let private bannedTypes =
  Set.ofList
    [ "System.IO.File"
      "System.IO.Directory"
      "System.IO.FileStream"
      "System.IO.FileInfo"
      "System.IO.DirectoryInfo"
      "System.Diagnostics.Process"
      "System.Diagnostics.ProcessStartInfo"
      "System.Net.Http.HttpClient"
      "System.Net.Http.HttpClientHandler"
      "System.Net.Http.SocketsHttpHandler"
      // Direct socket use must go through HostHttp; nothing else may open one.
      "System.Net.Sockets.Socket"
      "System.Net.Sockets.TcpClient"
      "System.Net.Sockets.NetworkStream" ]

/// Host subsystem implementations are not a second public door. A builtin that
/// calls one directly can otherwise avoid both request derivation and auditing,
/// while the IL only contains a reference to our wrapper rather than the BCL
/// type beneath it.
let private bannedHostSubsystemTypes =
  Set.ofList
    [ "LibExecution.HostHttp"
      "LibExecution.HostLibc"
      "LibExecution.HostProcess"
      "LibExecution.HostSecurity" ]

/// The libc bridge members an effect-free host-fact builtin may call
/// directly: process facts and constants that name no resource (docs/effects.md,
/// "Checked host boundary"). Everything else on `HostLibc` opens a path, a
/// descriptor, or an environment entry and must go through `Host.perform`.
let private ambientHostLibcMembers =
  Set.ofList
    [ "uname"
      "getpid"
      "getuid"
      "cpuCount"
      "fnmatch"
      "tryTerminalWindowSize"
      "get_O_RDONLY"
      "get_O_WRONLY"
      "get_O_RDWR"
      "get_O_CREAT"
      "get_O_TRUNC"
      "get_O_APPEND"
      "get_FNM_PATHNAME"
      "get_LOCK_EX"
      "get_LOCK_UN" ]

/// System.Environment is banned member-by-member: NewLine and ProcessorCount
/// are fine, the environment-variable accessors are host effects.
let private bannedEnvironmentMembers =
  Set.ofList
    [ "GetEnvironmentVariable"; "GetEnvironmentVariables"; "SetEnvironmentVariable" ]

/// HttpListener remains in the server adapter for request/response IO after a
/// checked bind, but creating and starting one is the scoped resource action.
/// Keep those members behind Host without banning the capability's later use.
let private bannedMembersByType =
  Map.ofList
    [ "System.Net.HttpListener", Set.ofList [ ".ctor"; "get_Prefixes"; "Start" ] ]

/// The door itself. These may reference OS APIs; nothing else may.
let private hostModules =
  Set.ofList
    [ "LibExecution", "LibExecution.Host"
      "LibExecution", "LibExecution.HostHttp"
      "LibExecution", "LibExecution.HostLibc"
      "LibExecution", "LibExecution.HostProcess"
      "LibExecution", "LibExecution.HostSecurity" ]

/// Known holdouts, one entry per (assembly, top-level type), shrinking as
/// families migrate through the checked boundary. Every entry is a type whose
/// builtins still call OS APIs beside an inline `PermissionCheck.require*`
/// (or ambient-effect) check. Do not add entries; convert the family instead.
let private allowlist : Set<string * string> =
  Set.ofList
    [ // terminal capability sniffing reads $TERM and friends: host
      // infrastructure for rendering, deliberately not policy-gated (the TUI
      // must detect capabilities even under deny-all to render the
      // permission prompt itself)
      "Builtins.Cli", "Builtins.Cli.Libs.Terminal"
      // trusted host startup: reads package-ref-hashes.txt before any guest
      // code runs; not reachable from a builtin
      "LibExecution", "LibExecution.PackageRefs" ]

let private assembliesToScan =
  [ "LibExecution"
    "Builtins.Pure"
    "Builtins.Cli"
    "Builtins.CliHost"
    "Builtins.Matter"
    "Builtins.Language"
    "Builtins.Http.Client"
    "Builtins.Http.Server"
    "Builtins.Time"
    "Builtins.Random" ]

// ── IL scanning ───────────────────────────────────────────────────────────────

/// Operand layouts from the runtime's own opcode table, so the walker can't
/// drift from the instruction set.
let private singleByteOps, private twoByteOps =
  let single : System.Reflection.Emit.OpCode voption[] = Array.create 256 ValueNone
  let two : System.Reflection.Emit.OpCode voption[] = Array.create 256 ValueNone
  for field in typeof<System.Reflection.Emit.OpCodes>.GetFields() do
    match field.GetValue null with
    | :? System.Reflection.Emit.OpCode as op ->
      // `OpCode.Value` is a signed int16: every two-byte opcode (0xFE xx) is
      // negative there, and reading it as a plain int used to drop all 27 of
      // them, so the walker stopped at the first `ceq`/`ldarg` in a body.
      let value = int (uint16 op.Value)
      if value <= 0xFF then single[value] <- ValueSome op
      elif (value >>> 8) = 0xFE then two[value &&& 0xFF] <- ValueSome op
    | _ -> ()
  single, two

/// Every metadata token an instruction stream references.
let private tokensIn (il : ImmutableArray<byte>) : List<int> =
  let tokens = ResizeArray<int>()
  let mutable i = 0
  let readInt32 (at : int) =
    int il[at]
    ||| (int il[at + 1] <<< 8)
    ||| (int il[at + 2] <<< 16)
    ||| (int il[at + 3] <<< 24)
  while i < il.Length do
    let op =
      if il[i] = 0xFEuy then
        let op = twoByteOps[int il[i + 1]]
        i <- i + 2
        op
      else
        let op = singleByteOps[int il[i]]
        i <- i + 1
        op
    match op with
    | ValueNone -> i <- il.Length // undecodable: stop rather than misparse
    | ValueSome op ->
      match op.OperandType with
      | System.Reflection.Emit.OperandType.InlineNone -> ()
      | System.Reflection.Emit.OperandType.ShortInlineBrTarget
      | System.Reflection.Emit.OperandType.ShortInlineI
      | System.Reflection.Emit.OperandType.ShortInlineVar -> i <- i + 1
      | System.Reflection.Emit.OperandType.InlineVar -> i <- i + 2
      | System.Reflection.Emit.OperandType.InlineField
      | System.Reflection.Emit.OperandType.InlineMethod
      | System.Reflection.Emit.OperandType.InlineTok
      | System.Reflection.Emit.OperandType.InlineType ->
        tokens.Add(readInt32 i)
        i <- i + 4
      | System.Reflection.Emit.OperandType.InlineBrTarget
      | System.Reflection.Emit.OperandType.InlineI
      | System.Reflection.Emit.OperandType.InlineSig
      | System.Reflection.Emit.OperandType.InlineString
      | System.Reflection.Emit.OperandType.ShortInlineR -> i <- i + 4
      | System.Reflection.Emit.OperandType.InlineI8
      | System.Reflection.Emit.OperandType.InlineR -> i <- i + 8
      | System.Reflection.Emit.OperandType.InlineSwitch ->
        let count = readInt32 i
        i <- i + 4 + (4 * count)
      | _ -> i <- il.Length
  List.ofSeq tokens

let private typeRefFullName
  (reader : MetadataReader)
  (handle : TypeReferenceHandle)
  =
  let typeRef = reader.GetTypeReference handle
  let ns = reader.GetString typeRef.Namespace
  let name = reader.GetString typeRef.Name
  if ns = "" then name else $"{ns}.{name}"

/// The banned member a token refers to, if any, as "Type::member".
let rec private bannedTarget (reader : MetadataReader) (handle : EntityHandle) =
  match handle.Kind with
  | HandleKind.MemberReference ->
    let memberRef =
      reader.GetMemberReference(MemberReferenceHandle.op_Explicit handle)
    match memberRef.Parent.Kind with
    | HandleKind.TypeReference ->
      let typeName =
        typeRefFullName reader (TypeReferenceHandle.op_Explicit memberRef.Parent)
      let memberName = reader.GetString memberRef.Name
      if
        typeName = "LibExecution.HostLibc"
        && Set.contains memberName ambientHostLibcMembers
      then
        None
      elif
        Set.contains typeName bannedTypes
        || Set.contains typeName bannedHostSubsystemTypes
      then
        Some $"{typeName}::{memberName}"
      elif
        typeName = "System.Environment"
        && Set.contains memberName bannedEnvironmentMembers
      then
        Some $"{typeName}::{memberName}"
      elif
        bannedMembersByType
        |> Map.tryFind typeName
        |> Option.exists (Set.contains memberName)
      then
        Some $"{typeName}::{memberName}"
      else
        None
    | _ -> None
  | HandleKind.MethodSpecification ->
    let spec =
      reader.GetMethodSpecification(MethodSpecificationHandle.op_Explicit handle)
    bannedTarget reader spec.Method
  | _ -> None

/// The outermost declaring type of a method, so closure classes attribute to
/// their module.
let private topLevelTypeName
  (reader : MetadataReader)
  (typeHandle : TypeDefinitionHandle)
  =
  let mutable current = reader.GetTypeDefinition typeHandle
  while not (current.GetDeclaringType().IsNil) do
    current <- reader.GetTypeDefinition(current.GetDeclaringType())
  let ns = reader.GetString current.Namespace
  let name = reader.GetString current.Name
  if ns = "" then name else $"{ns}.{name}"

/// (top-level type, banned target) pairs for one assembly.
let private violationsIn (dllPath : string) : List<string * string> =
  use file = System.IO.File.OpenRead dllPath
  use pe = new PEReader(file)
  let reader = pe.GetMetadataReader()
  let found = System.Collections.Generic.HashSet<string * string>()
  for methodHandle in reader.MethodDefinitions do
    let methodDef = reader.GetMethodDefinition methodHandle
    if methodDef.RelativeVirtualAddress <> 0 then
      let body = pe.GetMethodBody methodDef.RelativeVirtualAddress
      for token in tokensIn (body.GetILContent()) do
        match bannedTarget reader (MetadataTokens.EntityHandle token) with
        | Some target ->
          let typeName = topLevelTypeName reader (methodDef.GetDeclaringType())
          found.Add((typeName, target)) |> ignore<bool>
        | None -> ()
  List.ofSeq found

let onlyHostModulesTouchTheOS =
  test "only the host modules reference OS APIs" {
    let outputDir =
      System.IO.Path.GetDirectoryName(
        System.Reflection.Assembly.GetExecutingAssembly().Location
      )
    let observed =
      assembliesToScan
      |> List.collect (fun assembly ->
        let dll = System.IO.Path.Combine(outputDir, assembly + ".dll")
        if System.IO.File.Exists dll then
          violationsIn dll
          |> List.map (fun (typeName, target) -> (assembly, typeName, target))
        else
          [])
    let violations =
      observed
      |> List.filter (fun (assembly, typeName, _) ->
        not (Set.contains (assembly, typeName) hostModules)
        && not (Set.contains (assembly, typeName) allowlist))
    if not (List.isEmpty violations) then
      let lines =
        violations
        |> List.sort
        |> List.map (fun (assembly, typeName, target) ->
          $"  {assembly}: {typeName} -> {target}")
        |> String.concat "\n"
      Expect.isTrue
        false
        ("OS API references outside the checked host boundary:\n"
         + lines
         + "\nRoute the operation through Host.Operation + PermissionCheck.performHost.")
    // A stale allowlist entry means a family finished migrating; delete it so
    // the ratchet holds.
    let observedTypes =
      observed
      |> List.map (fun (assembly, typeName, _) -> (assembly, typeName))
      |> Set.ofList
    let stale = Set.difference allowlist observedTypes
    if not (Set.isEmpty stale) then
      let lines =
        stale
        |> Set.toList
        |> List.map (fun (assembly, typeName) -> $"  {assembly}: {typeName}")
        |> String.concat "\n"
      Expect.isTrue
        false
        ("Stale host-boundary allowlist entries (delete them):\n" + lines)
  }

let tests = testList "hostBoundary" [ onlyHostModulesTouchTheOS ]
