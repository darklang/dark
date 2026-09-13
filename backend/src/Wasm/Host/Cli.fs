/// The Dark CLI, hosted in a browser tab.
///
/// This is `Cli/Cli.fs:main` with the process parts removed. The store is the real
/// one: a copy of the shipped `data.db` is fetched at boot and written into
/// emscripten's in-memory filesystem, where SQLite (statically linked) opens it like
/// any other file. Everything above that runs unchanged; only the tty and the
/// process are stood in for (`Browser.fs`, `BrowserBuiltins.fs`).
///
/// Boot order matters and mirrors the CLI's: the rundir env var before anything
/// touches `LibConfig`, the policy directory before anything checks a permission,
/// the store file in place before `LibDB.Sqlite` opens a connection.
module Darklang.Wasm.Host.Cli

open System
open System.Threading.Tasks
open Microsoft.JSInterop

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Dval = LibExecution.Dval
module Exe = LibExecution.Execution
module PackageRefs = LibExecution.PackageRefs

/// Brotli, one shot, straight from the runtime's own native library. `BrotliStream` refuses
/// to construct on the browser (marked unsupported), but the decoder it would have called
/// is linked into dotnet.native.wasm regardless, and brotli is less than half of gzip on the
/// store. The caller passes the inflated size (the page reads it from `store.json`).
// F# lowers an `extern` through `failwith`, which Prelude bans; HostLibc shadows it too.
let private failwith (s : string) : 'a = raise (System.Exception(s))

[<Runtime.InteropServices.DllImport("libSystem.IO.Compression.Native",
                                    EntryPoint = "BrotliDecoderDecompress")>]
extern int private brotliDecompress(
  unativeint encodedSize,
  byte[] encoded,
  unativeint& decodedSize,
  byte[] decoded)

let private inflateBrotli (encoded : byte[]) (rawSize : int) : byte[] =
  let decoded = Array.zeroCreate<byte> rawSize
  let mutable decodedSize = unativeint rawSize
  let ok = brotliDecompress (unativeint encoded.Length, encoded, &decodedSize, decoded)
  if ok <> 1 || int decodedSize <> rawSize then
    Exception.raiseInternal
      "brotli: decode failed"
      [ "ok", ok; "decoded", decodedSize; "expected", rawSize ]
  decoded

/// Where the store lives in the tab's virtual filesystem. `DARK_CONFIG_RUNDIR` must be
/// absolute, and this is set before `LibConfig` computes its paths (Program.Main).
let runDir = "/dark"

/// Set the environment the CLI expects to find. Called first thing in Program.Main, so
/// it precedes every module initializer that reads `LibConfig`.
let configureEnvironment () : unit =
  Environment.SetEnvironmentVariable("DARK_CONFIG_RUNDIR", runDir)
  Environment.SetEnvironmentVariable("HOME", runDir)
  Environment.SetEnvironmentVariable("TERM", "xterm-256color")
  Environment.SetEnvironmentVariable("DARK_AUDIT", "off")
  IO.Directory.CreateDirectory(IO.Path.Combine(runDir, "logs")) |> ignore<IO.DirectoryInfo>

let private builtinsLazy : Lazy<RT.Builtins> =
  lazy
    (LibExecution.Builtin.combine
      [ Builtins.CliHost.Libs.Cli.builtinsToUse ()
        Builtins.CliHost.Builtin.builtins ()
        Builtins.Cli.Builtin.builtins ()
        // Last, so the browser's stdin/terminal answers win over Builtins.Cli's.
        BrowserBuiltins.builtins () ]
      [])

/// `?env=NAME=value` on the page, for switches like `DARK_CLASSIC=1`.
[<JSInvokable>]
let SetEnv (name : string, value : string) : unit =
  Environment.SetEnvironmentVariable(name, value)

let mutable private booted = false

/// Fetch the store and bring the package manager up. Idempotent.
[<JSInvokable>]
let Boot (storeUrl : string, rawSize : int) : Task =
  task {
    if not booted then
      let dbPath = LibConfig.Config.dbPath
      Browser.log $"boot: fetching {storeUrl} -> {dbPath}"
      use http = new Net.Http.HttpClient()
      let! bytes = http.GetByteArrayAsync storeUrl
      // Shipped gzip'd under its own name, and inflated here: an edge proxy in front of the
      // static host was seen handing the store back uncompressed however it was asked.
      let inflate (mk : IO.Stream -> IO.Stream) =
        use src = new IO.MemoryStream(bytes)
        use s = mk src
        use dst = new IO.MemoryStream()
        s.CopyTo dst
        dst.ToArray()
      let bytes =
        if storeUrl.EndsWith ".br" then
          inflateBrotli bytes rawSize
        elif storeUrl.EndsWith ".gz" then
          inflate (fun src ->
            new IO.Compression.GZipStream(src, IO.Compression.CompressionMode.Decompress))
        else
          bytes
      IO.File.WriteAllBytes(dbPath, bytes)
      Browser.log $"boot: wrote {bytes.Length} bytes"

      LibExecution.HostSecurity.setPolicyDirectory (IO.Path.Combine(runDir, "policy"))
      LibDB.PolicyStore.seedInstanceIfMissing LibExecution.Permissions.Policy.defaultInstance
      LibExecution.HostSecurity.setPackageDbPath dbPath
      Browser.log "boot: policy seeded"

      LibDB.Sqlite.Sql.warm ()
      Browser.log "boot: connection warm"
      let pm = LibDB.PackageManager.rt
      Browser.log "boot: growing"
      let! _grew =
        LibDB.Seed.growIfNeeded
          LibDB.Seed.EvaluationAuthority.underInstancePolicy
          (fun () -> builtinsLazy.Force())
          pm
          (fun msg -> Browser.writeToTerminal (msg + "\r\n"))
      Browser.log "boot: grown"
      do! pm.init |> Ply.toTask
      // `isHarmful` is synchronous and its miss path blocks; on one thread that never returns.
      do! LibDB.PackageManager.preloadHarmful ()
      LibDB.PackageManager.selectBranch PT.BranchId.Main
      Browser.log "boot: done"
      booted <- true
  }
  :> Task

/// `?trace=1` on the page: log every frame entry to the console, so a hang can be placed.
let mutable private traceFrames = false

[<JSInvokable>]
let SetTrace (on : bool) : unit = traceFrames <- on

let private tracing () : RT.Tracing.Tracing =
  if not traceFrames then
    Exe.noTracing
  else
    { Exe.noTracing with
        skipTracing = false
        storeFrameEntry =
          fun _ ep _ ->
            match ep with
            | RT.Function(RT.FQFnName.Package(RT.Hash h)) -> Browser.log $"frame: fn {h}"
            | RT.Function(RT.FQFnName.Builtin b) -> Browser.log $"frame: builtin {b.name}"
            | ep -> Browser.log $"frame: {ep}" }

let private state () : RT.ExecutionState =
  let program : RT.Program = { dbs = Map.empty }
  let notify _ _ _ _ = uply { return () }
  let sendException _ _ (metadata : Metadata) (exn : exn) =
    uply {
      match exn with
      | :? Exception.StoreConditionException -> ()
      | _ ->
        // The terminal may be in the alternate screen, so the console gets it too.
        Browser.log $"exception: {exn.GetType().Name}: {exn.Message}\n{exn.StackTrace}"
        printException "Internal error" metadata exn
    }
  Exe.createState
    (builtinsLazy.Force())
    LibDB.PackageManager.rt
    (tracing ())
    sendException
    notify
    program

/// Run the CLI entry point with these arguments, the way `dark <args>` would, and
/// return its exit code. With no arguments this opens the workbench and does not return
/// until the person quits.
[<JSInvokable>]
let RunCli (args : string[]) : Task<int> =
  task {
    Browser.log "run: bundled hashes"
    let! bundled = LibDB.ProgramTypes.Fn.hashesOwnedBy "Darklang" |> Ply.toTask
    Browser.log $"run: {bundled.Count} bundled; forcing builtins"
    builtinsLazy.Force().fns.Count |> ignore<int>
    Browser.log "run: builtins ready; building state"
    let state =
      { Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll (state ()) with
          branchId = LibDB.PackageManager.currentBranchId ()
          canManagePolicies = true
          canUsePrivateNetworkHttp = true
          isBundledPackageFn = fun (RT.Hash h) -> bundled.Contains h }
    Browser.log "run: state built; resolving entry point"
    let fnName = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
    Browser.log $"run: executing {fnName}"
    let args =
      args |> Array.toList |> List.map RT.DString |> Dval.list RT.KTString |> NEList.singleton
    match! Exe.executeFunction state fnName [] args with
    | Ok(RT.DInt64 code) -> return int code
    | Ok(RT.DInt code) -> return int (RT.DarkInt.toBigInt code)
    | Ok other ->
      Browser.writeToTerminal $"\r\nCLI returned {other}\r\n"
      return 0
    | Error(rte, _) ->
      let! msg = Exe.runtimeErrorToString state rte
      let text =
        match msg with
        | Ok(RT.DString s) -> s
        | _ -> $"{rte}"
      Browser.writeToTerminal $"\r\nEncountered a Runtime Error:\r\n{text}\r\n"
      return 1
  }

/// Run one command beside whatever is already running (the workbench, waiting on a key),
/// with its output captured rather than painted over the terminal. Same store, its own VM.
/// Returns the output followed by a line with the exit code.
[<JSInvokable>]
let RunCommand (args : string[]) : Task<string> =
  task {
    let! (code, output) = Browser.captured (fun () -> RunCli args)
    return output + $"\n[exit {code}]"
  }

/// Evaluate one Dark expression under the CLI state. A probe for the async path: the
/// page calls this with `Stdlib.Cli.Stdin.readKey ()`, then pushes a key.
[<JSInvokable>]
let EvalProbe (source : string) : Task<string> =
  task {
    let state = Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll (state ())
    let r = LibParser.Parser.parse source
    match r.parsed, r.diagnostics with
    | Some(LibParser.WrittenTypes.SourceFile sf), [] ->
      match List.rev sf.exprsToEval with
      | e :: _ ->
        let ctx : LibParser.WrittenTypesToProgramTypes.Context =
          { currentFnName = None; argMap = Map.empty; localBindings = Set.empty }
        let! pt =
          LibParser.WrittenTypesToProgramTypes.Expr.toPT
            (builtinsLazy.Force())
            LibDB.PackageManager.pt
            LibParser.NameResolver.OnMissing.Allow
            []
            ctx
            e
          |> Ply.toTask
        let instrs = LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT Map.empty 0 None pt
        match! Exe.executeExpr state instrs with
        | Ok dv ->
          let! repr = Exe.dvalToRepr state dv
          return repr
        | Error(rte, _) -> return $"error: {rte}"
      | [] -> return "nothing to evaluate"
    | _, diags -> return $"parse: {diags}"
  }

