/// The Dark CLI, hosted in a browser tab: the seam to the page (`Browser`), the builtins a
/// tab has to answer differently (`BrowserBuiltins`), and boot + run (`Cli`).
namespace Darklang.Wasm

/// The seam between the CLI running in the tab and the page around it.
///
/// Output: everything the CLI prints lands in a buffer the page drains (`DrainOutput`)
/// into xterm.js. Input: JS pushes keys in through `PushKey`/`PushPaste`; the browser
/// `stdinReadKey` awaits the next one. Size: JS tells us the terminal's columns and rows
/// whenever they change.
///
/// Everything here is single-threaded by construction (Blazor WebAssembly has one
/// thread), so the queue and the waiter need no locking.
module Browser =
  open System
  open System.Collections.Generic
  open System.Threading.Tasks
  open Microsoft.JSInterop

  let mutable private js : IJSInProcessRuntime option = None

  /// Called once from Program.Main with the host's runtime.
  let init (runtime : IJSInProcessRuntime) : unit = js <- Some runtime

  /// Output is buffered here and drained by the page (`DrainOutput`) on a short timer,
  /// never pushed through a synchronous JS call. A synchronous .NET -> JS call made from
  /// inside a resumed task continuation left the next await unable to suspend (it fell
  /// into a blocking wait, which a browser thread cannot do), and the interpreter prints
  /// from exactly there. Draining from JS also batches a whole TUI frame into one write.
  let private pending = Text.StringBuilder()

  /// While set, output goes here instead of the terminal: the command panel runs a one-shot
  /// command beside the workbench and shows its output in its own pane.
  let mutable private capture : Text.StringBuilder option = None

  /// Queue text for the page's terminal (or the capture, while one is open).
  let writeToTerminal (text : string) : unit =
    match capture with
    | Some c -> c.Append text |> ignore<Text.StringBuilder>
    | None -> pending.Append text |> ignore<Text.StringBuilder>

  /// Run <param f> with output captured; returns (result, everything it printed).
  let captured
    (f : unit -> Threading.Tasks.Task<'a>)
    : Threading.Tasks.Task<'a * string> =
    task {
      let c = Text.StringBuilder()
      capture <- Some c
      try
        let! r = f ()
        return (r, c.ToString())
      finally
        capture <- None
    }

  /// JS -> .NET: everything written since the last drain.
  [<JSInvokable>]
  let DrainOutput () : string =
    if pending.Length = 0 then
      ""
    else
      let s = pending.ToString()
      pending.Clear() |> ignore<Text.StringBuilder>
      s

  /// Boot diagnostics to the browser console; reaches DevTools even while the main thread
  /// is busy. Only for use outside the interpreter (boot, exception reports): see above.
  let log (text : string) : unit =
    match js with
    | Some j -> j.InvokeVoid("console.log", "[dark] " + text)
    | None -> ()

  /// A TextWriter that feeds the output buffer. Installed as Console.Out and
  /// Console.Error; `NonBlockingConsole` writes straight to Console in the browser, so
  /// every `print`/`printLine` and every frame the TUI paints lands here.
  type TerminalWriter() =
    inherit IO.TextWriter()
    override _.Encoding = Text.Encoding.UTF8
    override _.Write(c : char) = writeToTerminal (string c)
    override _.Write(s : string) = writeToTerminal s
    override _.Write(buffer : char[], index : int, count : int) =
      writeToTerminal (String(buffer, index, count))
    override _.WriteLine(s : string) = writeToTerminal (s + "\n")
    override _.WriteLine() = writeToTerminal "\n"

  /// One keyboard event, already in the shape `Console.ReadKey` would report it.
  type KeyEvent =
    {
      key : ConsoleKeyInfo
      /// The whole text of a paste, when this event is one.
      paste : string option
    }

  let private queue = Queue<KeyEvent>()
  let mutable private waiter : TaskCompletionSource<KeyEvent> option = None

  let private deliver (ev : KeyEvent) : unit =
    match waiter with
    | Some w ->
      waiter <- None
      // Completed synchronously, so the interpreter resumes INSIDE this JS -> .NET call and
      // runs until it next waits for a key. Resuming it from the thread pool instead (a
      // TaskCompletionSource with RunContinuationsAsynchronously) worked for that one turn,
      // and then the next suspension fell into a blocking wait, which is fatal on the browser's
      // single thread. A suspension from an interop call context suspends cleanly.
      w.SetResult ev
    | None -> queue.Enqueue ev

  /// The next key event: immediately if one is queued, otherwise when JS pushes one.
  let nextKey () : Task<KeyEvent> =
    if queue.Count > 0 then
      Task.FromResult(queue.Dequeue())
    else
      let tcs = TaskCompletionSource<KeyEvent>()
      waiter <- Some tcs
      tcs.Task

  let mutable private columns = 80L
  let mutable private rows = 24L

  let terminalSize () : int64 * int64 = (columns, rows)

  /// Map a DOM `KeyboardEvent.key` (plus modifiers) to what `Console.ReadKey` would
  /// have returned for the same keystroke on a Unix terminal.
  let private toConsoleKeyInfo
    (domKey : string)
    (ctrl : bool)
    (alt : bool)
    (shift : bool)
    : ConsoleKeyInfo =
    let make (ch : char) (key : ConsoleKey) =
      ConsoleKeyInfo(ch, key, shift, alt, ctrl)
    match domKey with
    | "Enter" -> make '\r' ConsoleKey.Enter
    | "Backspace" -> make '\b' ConsoleKey.Backspace
    | "Tab" -> make '\t' ConsoleKey.Tab
    | "Escape" -> make '\u001b' ConsoleKey.Escape
    | " " -> make ' ' ConsoleKey.Spacebar
    | "ArrowUp" -> make '\u0000' ConsoleKey.UpArrow
    | "ArrowDown" -> make '\u0000' ConsoleKey.DownArrow
    | "ArrowLeft" -> make '\u0000' ConsoleKey.LeftArrow
    | "ArrowRight" -> make '\u0000' ConsoleKey.RightArrow
    | "Home" -> make '\u0000' ConsoleKey.Home
    | "End" -> make '\u0000' ConsoleKey.End
    | "PageUp" -> make '\u0000' ConsoleKey.PageUp
    | "PageDown" -> make '\u0000' ConsoleKey.PageDown
    | "Insert" -> make '\u0000' ConsoleKey.Insert
    | "Delete" -> make '\u0000' ConsoleKey.Delete
    | "F1" -> make '\u0000' ConsoleKey.F1
    | "F2" -> make '\u0000' ConsoleKey.F2
    | "F3" -> make '\u0000' ConsoleKey.F3
    | "F4" -> make '\u0000' ConsoleKey.F4
    | "F5" -> make '\u0000' ConsoleKey.F5
    | "F6" -> make '\u0000' ConsoleKey.F6
    | "F7" -> make '\u0000' ConsoleKey.F7
    | "F8" -> make '\u0000' ConsoleKey.F8
    | "F9" -> make '\u0000' ConsoleKey.F9
    | "F10" -> make '\u0000' ConsoleKey.F10
    | "F11" -> make '\u0000' ConsoleKey.F11
    | "F12" -> make '\u0000' ConsoleKey.F12
    | k when k.Length = 1 ->
      let c = k[0]
      let upper = Char.ToUpperInvariant c
      let key =
        if upper >= 'A' && upper <= 'Z' then
          enum<ConsoleKey> (int upper)
        elif c >= '0' && c <= '9' then
          enum<ConsoleKey> (int ConsoleKey.D0 + int c - int '0')
        else
          ConsoleKey.NoName
      // A terminal reports Ctrl+<letter> as the control character; `Ctrl+C` is '\u0003'.
      let ch =
        if ctrl && upper >= 'A' && upper <= 'Z' then char (int upper - 64) else c
      make ch key
    | _ ->
      // Shift/Ctrl/Alt on their own, media keys, IME: nothing a CLI acts on.
      make '\u0000' ConsoleKey.NoName

  /// JS -> .NET: one keydown.
  [<JSInvokable>]
  let PushKey (domKey : string, ctrl : bool, alt : bool, shift : bool) : unit =
    deliver { key = toConsoleKeyInfo domKey ctrl alt shift; paste = None }

  /// JS -> .NET: pasted text. Reported the way the real host reports a paste: the
  /// key is the first printable character, the text is the whole paste.
  [<JSInvokable>]
  let PushPaste (text : string) : unit =
    if not (String.IsNullOrEmpty text) then
      let first =
        text
        |> Seq.tryFind (fun c -> not (Char.IsControl c))
        |> Option.defaultValue ' '
      deliver
        { key = toConsoleKeyInfo (string first) false false false
          paste = Some text }

  /// JS -> .NET: the terminal was (re)sized. Also wakes the read loop with a key nobody
  /// handles, which is how SIGWINCH gets a repaint on the real host.
  [<JSInvokable>]
  let SetTerminalSize (cols : int, rws : int) : unit =
    let changed = int64 cols <> columns || int64 rws <> rows
    columns <- int64 (max 1 cols)
    rows <- int64 (max 1 rws)
    // Only a read that is already waiting needs waking; a loop that is busy samples the
    // size on its next pass, and a queued wake-up would be read as a keystroke later.
    if changed && waiter.IsSome then
      deliver
        { key = ConsoleKeyInfo('\u0000', ConsoleKey.NoName, false, false, false)
          paste = None }


/// The builtins the browser has to answer differently from a process on a tty.
///
/// Layered LAST in the CLI's builtin set (`Builtin.combine` keeps the later definition
/// of a name), so everything not listed here is the real implementation from
/// `Builtins.Cli`. Kept deliberately short: the point of the exercise is running the
/// real CLI, not a browser edition of it.
module BrowserBuiltins =
  open System
  open System.Threading.Tasks

  open Prelude
  open LibExecution.RuntimeTypes
  open LibExecution.Builtin.Shortcuts
  open LibExecution.Effects

  module Builtin = LibExecution.Builtin
  module PackageRefs = LibExecution.PackageRefs
  module NR = LibExecution.RuntimeTypes.NameResolution

  /// The `Stdlib.Cli.Stdin.KeyRead` record, the shape `Builtins.Cli`'s `stdinReadKey` builds. The
  /// Dark `Key` enum's cases are the `ConsoleKey` names, so the enum's own name is the case.
  let private keyRead (ev : Browser.KeyEvent) : Dval =
    let k = ev.key
    let has (m : ConsoleModifiers) = (k.Modifiers &&& m) <> ConsoleModifiers.None
    let typ (hash : unit -> string) = FQTypeName.fqPackage (hash ())
    let modifiers =
      DRecord(
        typ PackageRefs.Type.Stdlib.Cli.Stdin.modifiers,
        typ PackageRefs.Type.Stdlib.Cli.Stdin.modifiers,
        [],
        Map
          [ "alt", DBool(has ConsoleModifiers.Alt)
            "shift", DBool(has ConsoleModifiers.Shift)
            "ctrl", DBool(has ConsoleModifiers.Control) ]
      )
    let key =
      DEnum(
        typ PackageRefs.Type.Stdlib.Cli.Stdin.key,
        typ PackageRefs.Type.Stdlib.Cli.Stdin.key,
        [],
        string k.Key,
        []
      )
    let keyChar =
      match ev.paste with
      | Some text -> DString text
      | None ->
        if Char.IsControl k.KeyChar then DString "" else DString(string k.KeyChar)
    DRecord(
      typ PackageRefs.Type.Stdlib.Cli.Stdin.keyRead,
      typ PackageRefs.Type.Stdlib.Cli.Stdin.keyRead,
      [],
      Map
        [ "key", key
          "modifiers", modifiers
          "keyChar", keyChar
          "repeat", Dval.int 1I ]
    )

  /// Read a whole line off the key queue. Echoes nothing; the prompt that asked is
  /// expected to be a TUI region (which is how the CLI's own line reads work).
  let private readLine () : Task<string> =
    task {
      let sb = Text.StringBuilder()
      let mutable fin = false
      while not fin do
        let! ev = Browser.nextKey ()
        match ev.paste with
        | Some text ->
          match text.IndexOf '\n' with
          | -1 -> sb.Append text |> ignore<Text.StringBuilder>
          | i ->
            sb.Append(text.Substring(0, i)) |> ignore<Text.StringBuilder>
            fin <- true
        | None ->
          match ev.key.Key with
          | ConsoleKey.Enter -> fin <- true
          | ConsoleKey.Backspace -> if sb.Length > 0 then sb.Length <- sb.Length - 1
          | _ ->
            let c = ev.key.KeyChar
            if not (Char.IsControl c) then sb.Append c |> ignore<Text.StringBuilder>
      return sb.ToString()
    }

  let private fns : List<BuiltInFn> =
    [ { name = fn "stdinReadKey" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType =
          let typeName =
            FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.keyRead ())
          TCustomType(NR.ok typeName, [])
        description = "Waits for the next key the page pushes in."
        fn =
          (function
          | _, _, _, [| DUnit |] ->
            uply {
              let! ev = Browser.nextKey ()
              return keyRead ev
            }
          | _ -> incorrectArgs ())
        sqlSpec = NotQueryable
        previewable = Impure
        callEffects = set [ Effect.Stdin ]
        deprecated = NotDeprecated }

      { name = fn "stdinReadLine" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TString
        description = "Reads a line from the keys the page pushes in."
        fn =
          (function
          | _, _, _, [| DUnit |] ->
            uply {
              let! line = readLine ()
              return DString line
            }
          | _ -> incorrectArgs ())
        sqlSpec = NotQueryable
        previewable = Impure
        callEffects = set [ Effect.Stdin ]
        deprecated = NotDeprecated }

      { name = fn "stdinIsInteractive" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TBool
        description = "True: the page's terminal is always a person."
        fn =
          (function
          | _, _, _, [| DUnit |] -> Ply(DBool true)
          | _ -> incorrectArgs ())
        sqlSpec = NotQueryable
        previewable = Impure
        callEffects = set [ Effect.Stdin ]
        deprecated = NotDeprecated }

      { name = fn "stdinReadAll" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TString
        description = "Nothing is ever piped into a tab."
        fn =
          (function
          | _, _, _, [| DUnit |] -> Ply(DString "")
          | _ -> incorrectArgs ())
        sqlSpec = NotQueryable
        previewable = Impure
        callEffects = set [ Effect.Stdin ]
        deprecated = NotDeprecated }

      { name = fn "cliTerminalSize" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TTuple(TInt, TInt, [])
        description =
          "The xterm.js terminal's (columns, rows), as last reported by the page."
        fn =
          (function
          | _, _, _, [| DUnit |] ->
            let (width, height) = Browser.terminalSize ()
            DTuple(Dval.int (bigint width), Dval.int (bigint height), []) |> Ply
          | _ -> incorrectArgs ())
        sqlSpec = NotQueryable
        previewable = Impure
        callEffects = Set.empty
        deprecated = NotDeprecated }

      { name = fn "cliTerminalSessionInfo" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "A unit" ]
        returnType = TTuple(TBool, TBool, [ TString ])
        description =
          "(input is terminal, output is terminal, TERM): always a terminal here."
        fn =
          (function
          | _, _, _, [| DUnit |] ->
            DTuple(DBool true, DBool true, [ DString "xterm-256color" ]) |> Ply
          | _ -> incorrectArgs ())
        sqlSpec = NotQueryable
        previewable = Impure
        callEffects = Set.empty
        deprecated = NotDeprecated }

      { name = fn "stdoutClear" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "A unit" ]
        returnType = TUnit
        description = "Clears the terminal."
        fn =
          (function
          | _, _, _, [| DUnit |] ->
            Browser.writeToTerminal "\u001b[2J\u001b[H"
            Ply DUnit
          | _ -> incorrectArgs ())
        sqlSpec = NotQueryable
        previewable = Impure
        callEffects = set [ Effect.Stdout ]
        deprecated = NotDeprecated } ]

  let builtins () : Builtins = Builtin.make [] fns


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
module Cli =
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
    byte[] decoded
  )

  let private inflateBrotli (encoded : byte[]) (rawSize : int) : byte[] =
    let decoded = Array.zeroCreate<byte> rawSize
    let mutable decodedSize = unativeint rawSize
    let ok =
      brotliDecompress (unativeint encoded.Length, encoded, &decodedSize, decoded)
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
    IO.Directory.CreateDirectory(IO.Path.Combine(runDir, "logs"))
    |> ignore<IO.DirectoryInfo>

  /// The CLI's platform set, with the browser's answers layered over it.
  ///
  /// `combine` rather than `PlatformSet.make` on purpose: the set refuses two platforms claiming
  /// one name, and the browser's whole point is to claim `Terminal`'s names and win. That is a
  /// deliberate shadowing of a linked platform, which nothing else in the tree does, and it is
  /// confined to this one executable.
  let private builtinsLazy : Lazy<RT.Builtins> =
    lazy
      (LibExecution.Builtin.combine
        [ (Platforms.Sets.cli ()).builtins
          // Last, so the browser's stdin/terminal answers win over `Terminal`'s.
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
              new IO.Compression.GZipStream(
                src,
                IO.Compression.CompressionMode.Decompress
              ))
          else
            bytes
        IO.File.WriteAllBytes(dbPath, bytes)

        LibExecution.HostSecurity.setPolicyDirectory (
          IO.Path.Combine(runDir, "policy")
        )
        LibDB.PolicyStore.seedInstanceIfMissing
          LibExecution.Permissions.Policy.defaultInstance
        LibExecution.HostSecurity.setPackageDbPath dbPath

        LibDB.Sqlite.Sql.warm ()
        let pm = LibDB.PackageManager.rt
        let! _grew =
          LibDB.Seed.growIfNeeded
            LibDB.Seed.EvaluationAuthority.underInstancePolicy
            (fun () -> builtinsLazy.Force())
            pm
            (fun msg -> Browser.writeToTerminal (msg + "\r\n"))
        do! pm.init |> Ply.toTask
        // `isHarmful` is synchronous and its miss path blocks; on one thread that never returns.
        do! LibDB.PackageManager.preloadHarmful ()
        LibDB.PackageManager.selectBranch PT.BranchId.Main
        Browser.log "boot: done"
        booted <- true
    }
    :> Task

  let private state () : RT.ExecutionState =
    let program : RT.Program = { dbs = Map.empty }
    let notify _ _ _ _ = uply { return () }
    let sendException _ _ (metadata : Metadata) (exn : exn) =
      uply {
        match exn with
        | :? Exception.StoreConditionException -> ()
        | _ ->
          // The terminal may be in the alternate screen, so the console gets it too.
          Browser.log
            $"exception: {exn.GetType().Name}: {exn.Message}\n{exn.StackTrace}"
          printException "Internal error" metadata exn
      }
    Exe.createState
      (builtinsLazy.Force())
      LibDB.PackageManager.rt
      Exe.noTracing
      sendException
      notify
      program

  /// Run the CLI entry point with these arguments, the way `dark <args>` would, and
  /// return its exit code. With no arguments this opens the workbench and does not return
  /// until the person quits.
  [<JSInvokable>]
  let RunCli (args : string[]) : Task<int> =
    task {
      let! bundled = LibDB.ProgramTypes.Fn.hashesOwnedBy "Darklang" |> Ply.toTask
      let state =
        { Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll (state ()) with
            branchId = LibDB.PackageManager.currentBranchId ()
            canManagePolicies = true
            canUsePrivateNetworkHttp = true
            isBundledPackageFn = fun (RT.Hash h) -> bundled.Contains h }
      let fnName = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
      let args =
        args
        |> Array.toList
        |> List.map RT.DString
        |> Dval.list RT.KTString
        |> NEList.singleton
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
