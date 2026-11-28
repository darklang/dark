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
/// Everything here is single-threaded by construction (the runtime has one thread
/// in its worker), so the queue and the waiter need no locking.
module Browser =
  open System
  open System.Collections.Generic
  open System.Threading.Tasks

  let mutable private logTo : Action<string> option = None

  /// Called once from `Startup.Start` with the page's `console.log`.
  let setLog (log : Action<string>) : unit = logTo <- Some log

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
    match logTo with
    | Some l -> l.Invoke("[dark] " + text)
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
  let PushKey (domKey : string, ctrl : bool, alt : bool, shift : bool) : unit =
    deliver { key = toConsoleKeyInfo domKey ctrl alt shift; paste = None }

  /// JS -> .NET: pasted text. Reported the way the real host reports a paste: the
  /// key is the first printable character, the text is the whole paste.
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
  module Pending = LibExecution.Pending
  module Dval = LibExecution.Dval
  module Stdin = Builtins.Cli.Libs.Stdin

  let private keyRead (ev : Browser.KeyEvent) : Dval =
    Stdin.keyReadToDval ev.key ev.paste 1

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

  let private someLine (line : string) : Dval =
    Dval.option KTString (Some(DString(String.normalizeOutside line)))

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
          | _, _, _, [| DUnit |] -> Browser.nextKey () |> Pending.map keyRead
          | _ -> incorrectArgs ())
        previewable = Impure
        callEffects = set [ Effect.Stdin ]
        deprecated = NotDeprecated }

      { name = fn "stdinReadLine" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TypeReference.option TString
        description = "Reads a line from the keys the page pushes in."
        fn =
          (function
          | _, _, _, [| DUnit |] -> readLine () |> Pending.map someLine
          | _ -> incorrectArgs ())
        previewable = Impure
        callEffects = set [ Effect.Stdin; Effect.Raise ]
        deprecated = NotDeprecated }

      { name = fn "stdinCheckedReadLine" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TypeReference.result (TypeReference.option TString) TUnit
        description =
          "Reads a line from the keys the page pushes in. Never Error: keys are text."
        fn =
          (function
          | _, _, _, [| DUnit |] ->
            let okType =
              KTCustomType(Dval.optionType (), [ LibExecution.ValueType.string ])
            readLine ()
            |> Pending.map (fun line -> someLine line |> Dval.resultOk okType KTUnit)
          | _ -> incorrectArgs ())
        previewable = Impure
        callEffects = set [ Effect.Stdin ]
        deprecated = NotDeprecated }

      { name = fn "stdinReadAll" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TypeReference.result TString TBytes
        description = "Nothing is ever piped into a tab."
        fn =
          (function
          | _, _, _, [| DUnit |] -> Dval.resultOk KTString KTBytes (DString "")
          | _ -> incorrectArgs ())
        previewable = Impure
        callEffects = set [ Effect.Stdin ]
        deprecated = NotDeprecated }

      { name = fn "cliTerminalSize" 0
        typeParams = []
        parameters = [ Param.make "unit" TUnit "" ]
        returnType = TTuple(TInt64, TInt64, [])
        description =
          "The xterm.js terminal's (columns, rows), as last reported by the page."
        fn =
          (function
          | _, _, _, [| DUnit |] ->
            let (width, height) = Browser.terminalSize ()
            DTuple(Dval.dint64 width, Dval.dint64 height, [])
          | _ -> incorrectArgs ())
        previewable = Impure
        callEffects = Set.empty
        deprecated = NotDeprecated }

      { name = fn "cliTerminalIsatty" 0
        typeParams = []
        parameters =
          [ Param.make "fd" TInt64 "0 for stdin, 1 for stdout, 2 for stderr" ]
        returnType = TBool
        description = "The page's terminal is always a terminal."
        fn =
          (function
          | _, _, _, [| DInt64 fd |] -> DBool(fd >= 0L && fd <= 2L)
          | _ -> incorrectArgs ())
        previewable = Impure
        callEffects = Set.empty
        deprecated = NotDeprecated }

      // The prompt and every TUI park here. Without a tab-side answer the native builtin falls
      // through to `awaitBlocking`, which polls `Console.KeyAvailable` and sleeps between polls:
      // a tab may not sleep its only thread, so the first key wait would kill it. This answers
      // the specs a tab CAN answer and refuses the rest by name.
      { name = fn "hostAwait" 0
        typeParams = []
        parameters =
          [ Param.make
              "specs"
              (TList(
                TCustomType(
                  NR.ok (
                    FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Host.eventSpec ())
                  ),
                  []
                )
              ))
              "" ]
        returnType =
          TCustomType(
            NR.ok (FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Host.rawEvent ())),
            []
          )
        description =
          "Parks until the first of the given events happens, as a tab can."
        fn =
          (function
          | _, _, _, [| DList(_, Items specs) |] ->
            let rawEvent =
              FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Host.rawEvent ())
            let case name fields = DEnum(rawEvent, rawEvent, [], name, fields)

            let wantsKey =
              specs
              |> List.exists (fun d ->
                match d with
                | DEnum(_, _, _, "Key", []) -> true
                | _ -> false)

            let timerMs =
              specs
              |> List.tryPick (fun d ->
                match d with
                | DEnum(_, _, _, "Timer", [ DInt64 ms ]) -> Some ms
                | _ -> None)

            // Key first when both are asked for, which is what the prompt asks for
            // (`Host.await [Key, StoreChanged]`). A tab's only writer is itself, so a store
            // change cannot arrive while this process is the one parked.
            if wantsKey then
              Browser.nextKey () |> Pending.map (fun ev -> case "Key" [ keyRead ev ])
            else
              match timerMs with
              | Some ms ->
                task {
                  do! Task.Delay(int ms)
                  return case "Timer" []
                }
                |> Pending.ofTask
              | None ->
                RuntimeError.UncaughtException(
                  "This waits on something a browser tab cannot produce. "
                  + "Install the CLI to run it (darklang.com has the one-line installer).",
                  [ "waiting on",
                    DString(specs |> List.map string |> String.concat ", ") ]
                )
                |> raiseRTE
          | _ -> incorrectArgs ())
        previewable = Impure
        callEffects = set [ Effect.Stdin; Effect.PackageRead; Effect.Raise ]
        deprecated = NotDeprecated } ]

  let builtins () : Builtins = Builtin.make [] fns



/// The Dark CLI, hosted in a browser tab.
///
/// This is `Cli/Cli.fs:main` with the process parts removed. The store is the real
/// one: a copy of the shipped `data.db` is fetched at boot and written into
/// emscripten's in-memory filesystem, where SQLite (statically linked) opens it like
/// any other file. Everything above that runs unchanged; only the tty and the
/// process are stood in for (`Browser`, `BrowserBuiltins`).
///
/// Boot order matters and mirrors the CLI's: the rundir env var before anything
/// touches `LibConfig`, the policy directory before anything checks a permission,
/// the store file in place before `LibDB.Sqlite` opens a connection.
module Cli =
  open System
  open System.Threading.Tasks

  open Prelude

  module RT = LibExecution.RuntimeTypes
  module PT = LibExecution.ProgramTypes
  module Dval = LibExecution.Dval
  module Exe = LibExecution.Execution
  module PackageRefs = LibExecution.PackageRefs

  /// Where the store lives in the tab's virtual filesystem: `$HOME/.darklang`, as for the CLI,
  /// with HOME set to `/` in `configureEnvironment`.
  let runDir = "/.darklang"

  /// Set the environment the CLI expects to find. Called first thing in `Startup.Start`, so
  /// it precedes every module initializer that reads `LibConfig`.
  let configureEnvironment () : unit =
    Environment.SetEnvironmentVariable("HOME", "/")
    Environment.SetEnvironmentVariable("TERM", "xterm-256color")
    Environment.SetEnvironmentVariable("DARK_AUDIT", "off")
    IO.Directory.CreateDirectory(IO.Path.Combine(runDir, "logs"))
    |> ignore<IO.DirectoryInfo>

  let private builtinsLazy : Lazy<RT.Builtins> =
    lazy
      (LibExecution.Builtin.combine
        // `builtinsToUse` is the CLI's whole table already.
        [ Builtins.CliHost.Libs.Cli.builtinsToUse ()
          // Last, so the browser's stdin/terminal answers win over Builtins.Cli's.
          BrowserBuiltins.builtins () ]
        [])

  /// `?env=NAME=value` on the page, for switches like `DARK_CLASSIC=1`.
  let SetEnv (name : string, value : string) : unit =
    Environment.SetEnvironmentVariable(name, value)

  let mutable private booted = false

  /// What Boot did with a store the page put back from the browser's own storage.
  type private Restored =
    /// This build had already reconciled it, so it is used as it is.
    | Kept
    /// An older release saved it, and it was brought up to this one. Carries the names it held that
    /// the shipped store's newer ops may have taken back, for `UpgradeKeep` after the fold.
    | Upgraded of CatchUpStore.LocallyAuthored
    /// A newer release saved it, so it is left alone for that release and the tab starts fresh.
    | SavedByNewer of why : string
    /// It could not be brought up to this build, so the tab starts fresh.
    | Refused of why : string

  let mutable private restored : Option<Restored> = None

  /// What Boot did, for the page (`site.js`, `keepSaving`): "", "kept", "upgraded",
  /// "newer: <why>" or "refused: <why>".
  let RestoreOutcome () : string =
    match restored with
    | None -> ""
    | Some Kept -> "kept"
    | Some(Upgraded _) -> "upgraded"
    | Some(SavedByNewer why) -> "newer: " + why
    | Some(Refused why) -> "refused: " + why

  /// Fetch the shipped store. The host sends it brotli- or gzip-encoded and the browser's fetch
  /// inflates it: .NET 11's browser runtime has no brotli decoder of its own.
  let private fetchStore (storeUrl : string) : Task<byte[]> =
    task {
      use http = new Net.Http.HttpClient()
      return! http.GetByteArrayAsync storeUrl
    }

  /// The store files Boot may have to throw away, all of them, so a refused store leaves nothing behind
  /// for SQLite to pair with the shipped one.
  let private discardStore (dbPath : string) : unit =
    // Pooled connections to it would outlive the files and hand the next caller the deleted store.
    Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools()
    for suffix in [ ""; "-wal"; "-shm"; "-journal" ] do
      let f = dbPath + suffix
      if IO.File.Exists f then IO.File.Delete f

  /// When the release a store came from was built: its newest commit's `origin_ts`. Every op and commit
  /// in a shipped store is stamped at build time, so for a store that is nothing but a shipped one,
  /// this orders releases. A build hash does not: it says "different", never "newer".
  let private builtAt (dbPath : string) : string =
    use conn = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
    conn.Open()
    use cmd = conn.CreateCommand()
    cmd.CommandText <- "SELECT COALESCE(MAX(origin_ts), '') FROM commits"
    string (cmd.ExecuteScalar())

  let private releaseTable =
    "CREATE TABLE IF NOT EXISTS tab_release_v0 (id INTEGER PRIMARY KEY CHECK (id = 0), built_at TEXT NOT NULL)"

  /// The release the tab last brought this store up to, kept in the store so it travels with the
  /// snapshot. "" for a store with no record, which reads as older than any release.
  let private releaseOf (dbPath : string) : string =
    use conn = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
    conn.Open()
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      releaseTable
      + "; SELECT COALESCE((SELECT built_at FROM tab_release_v0 WHERE id = 0), '')"
    string (cmd.ExecuteScalar())

  let private recordRelease (dbPath : string) (builtAt : string) : unit =
    use conn = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
    conn.Open()
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      releaseTable
      + "; INSERT OR REPLACE INTO tab_release_v0 (id, built_at) VALUES (0, $at)"
    cmd.Parameters.AddWithValue("$at", builtAt)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    cmd.ExecuteNonQuery() |> ignore<int>

  let private stampWithThisBuild (dbPath : string) : unit =
    let build = LibConfig.Config.buildHash
    if build <> "dev" then CatchUpStore.recordStoreStamp dbPath build

  /// Bring a store the page restored up to this build, as the desktop CLI does on upgrade: the
  /// schema and the release steps (`LibDB.Releases`), then the shipped store's package ops
  /// (`CatchUpStore`), then the stamp. Raises on any failure, deliberately: the caller drops the
  /// store rather than run a half-migrated one, which in a tab is worse than starting over.
  let private upgradeRestored (dbPath : string) (shipped : byte[]) : Restored =
    let releasePath = IO.Path.Combine(runDir, "incoming-store.db")
    IO.File.WriteAllBytes(releasePath, shipped)
    try
      // Checked before anything writes the store: an older build must not fold a newer release's
      // store back to its own, and the newer build will want it as it was.
      let shippedAt = builtAt releasePath
      let savedAt = releaseOf dbPath
      if savedAt > shippedAt then
        SavedByNewer
          $"saved by a release built {savedAt}; this one was built {shippedAt}"
      else
        match
          CatchUpStore.schemaFrom (Reflection.Assembly.GetExecutingAssembly())
        with
        | Some sql ->
          LibDB.Releases.applySchemaTables sql
          LibDB.Releases.runPending ()
          LibDB.Releases.applySchemaIndexes sql
        | None -> Exception.raiseInternal "this build embeds no schema" []
        // No backup here: the page keeps the restored snapshot aside until this succeeds.
        let held = CatchUpStore.fromRelease dbPath releasePath
        recordRelease dbPath shippedAt
        stampWithThisBuild dbPath
        Upgraded held
    finally
      if IO.File.Exists releasePath then IO.File.Delete releasePath

  /// What Boot does with a store the page put back. Never raises: any failure is `Refused`.
  let private reconcileRestored
    (dbPath : string)
    (storeUrl : string)
    : Task<Restored> =
    task {
      let build = LibConfig.Config.buildHash
      if build <> "dev" && CatchUpStore.storeStamp dbPath = Some build then
        return Kept
      else
        try
          let! shipped = fetchStore storeUrl
          return upgradeRestored dbPath shipped
        with e ->
          // The first line: a database error carries its whole stack trace in its message.
          let firstLine (ex : exn) = (ex.Message.Split '\n' |> Array.head).Trim()
          return
            Refused(
              match e with
              | LibDB.Releases.StepFailed(step, inner) ->
                $"release step {step}: {firstLine inner}"
              | e -> firstLine e
            )
    }

  /// Fetch the store and bring the package manager up. Idempotent.
  ///
  /// When the page has put back a store from the browser's own storage (`site.js`, `restore`), that
  /// store is used instead of the shipped one: as it is, when this build reconciled it, or brought
  /// up to this build first when an older one saved it.
  let Boot (storeUrl : string) : Task =
    task {
      if not booted then
        let dbPath = LibConfig.Config.dbPath
        // Nothing else writes the store before Boot, so a store already here is one the page put back.
        if IO.File.Exists dbPath then
          let! r = reconcileRestored dbPath storeUrl
          restored <- Some r
        // The shipped store is this build's own, so a copy the page keeps of it needs nothing on the
        // next visit to this build.
        let startFromShipped () =
          task {
            let! bytes = fetchStore storeUrl
            IO.File.WriteAllBytes(dbPath, bytes)
            stampWithThisBuild dbPath
            CatchUpStore.markAllAsShipped dbPath
            recordRelease dbPath (builtAt dbPath)
          }
        match restored with
        | Some Kept
        | Some(Upgraded _) -> ()
        | Some(SavedByNewer _) ->
          discardStore dbPath
          Browser.writeToTerminal (
            "Your saved work in this browser was saved by a newer version of Darklang than "
            + "this page runs. It is left as it is, for that version; this tab starts fresh "
            + "and will not save.\r\n"
          )
          do! startFromShipped ()
        | Some(Refused why) ->
          discardStore dbPath
          Browser.writeToTerminal (
            "Your saved work in this browser could not be brought up to this version of "
            + $"Darklang ({why}). It is kept, unchanged; this tab starts fresh.\r\n"
          )
          do! startFromShipped ()
        | None -> do! startFromShipped ()

        LibExecution.HostSecurity.setPolicyDirectory (
          IO.Path.Combine(runDir, "policy")
        )
        // A tab gets the permissive policy, not `defaultInstance`, and the reason is that the
        // browser is already a tighter sandbox than this policy can describe. There is no
        // filesystem to read, no process to spawn and no environment to leak; the things
        // `defaultInstance` withholds are things a tab cannot do at all. Network is the one that
        // matters and granting it gives away nothing, because `fetch` is bound by CORS and the
        // page's origin either way: the page could already make the request.
        //
        // Without this, the first interesting thing anyone types dies with "permission denied by
        // instance policy" and advice (`permissions allow http GET ...`) that cannot be followed
        // in a tab, which reads as broken rather than as sandboxed.
        LibDB.PolicyStore.seedInstanceIfMissing
          LibExecution.Permissions.Policy.allowAll
        LibExecution.HostSecurity.setPackageDbPath dbPath

        LibDB.Sqlite.Sql.warm ()
        let pm = LibDB.PackageManager.rt
        LibDB.Seed.growIfNeeded (fun () -> builtinsLazy.Force()) pm (fun msg ->
          Browser.writeToTerminal (msg + "\r\n"))
        |> ignore<bool>
        // After the fold, never before: an edit the shipped store also carries has just lost its name
        // to the newer stamp, and this puts it back, as the desktop CLI does after an upgrade.
        match restored with
        | Some(Upgraded held) when not (List.isEmpty held) ->
          let kept = LibDB.UpgradeKeep.restore held
          if not (List.isEmpty kept) then
            Browser.writeToTerminal (
              $"Kept {List.length kept} of your edits that this version also ships.\r\n"
            )
        | _ -> ()
        LibDB.PackageManager.selectBranch PT.BranchId.Main
        Browser.log "boot: done"
        booted <- true
    }
    :> Task

  let private state () : RT.ExecutionState =
    let notify _ _ (_ : string) (_ : Metadata) = ()
    let sendException _ _ (metadata : Metadata) (exn : exn) =
      match exn with
      | :? Exception.StoreConditionException -> ()
      | _ ->
        // The terminal may be in the alternate screen, so the console gets it too.
        Browser.log
          $"exception: {exn.GetType().Name}: {exn.Message}\n{exn.StackTrace}"
        printException "Internal error" metadata exn
    Exe.createState
      (builtinsLazy.Force())
      LibDB.PackageManager.rt
      Exe.noTracing
      sendException
      notify

  /// Run the CLI entry point with these arguments, the way `dark <args>` would, and
  /// return its exit code. With no arguments this opens the workbench and does not return
  /// until the person quits.
  let RunCli (args : string[]) : Task<int> =
    task {
      // Everything in a tab's store is bundled, which is why this is `true` rather than the
      // `hashesOwnedBy "Darklang"` a native binary uses. Natively that set separates the stdlib
      // that shipped with the binary from code a person authored or pulled, and only the former
      // is trusted without an explicit `permissions approve`. A tab has no such distinction: the
      // store is downloaded with the build and is the same artifact, there is nothing in it that
      // did not ship, and there is no way to approve anything anyway because an approval is
      // written to `~/.darklang`, which a browser does not have.
      //
      // Without this, a demo function in the store dies with "permission denied by package
      // policy ... To approve: `permissions approve <fn>`", naming a command a tab cannot run.
      let state =
        { Exe.setInstancePolicy LibExecution.Permissions.Policy.allowAll (state ()) with
            branchId = LibDB.PackageManager.currentBranchId ()
            canManagePolicies = true
            isBundledPackageFn = fun _ -> true }
      let fnName = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.executeCliCommand ())
      let args =
        args
        |> Array.toList
        |> List.map RT.DString
        |> Dval.list RT.KTString
        |> NEList.singleton
      match! Exe.executeFunction state fnName [] args with
      | Ok(RT.DInt64 code) -> return int code
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
  let RunCommand (args : string[]) : Task<string> =
    task {
      let! (code, output) = Browser.captured (fun () -> RunCli args)
      return output + $"\n[exit {code}]"
    }
