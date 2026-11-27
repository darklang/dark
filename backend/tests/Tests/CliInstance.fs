/// A throwaway Darklang install, for tests that only read what a command PRINTED.
///
/// The CLI suites drive `executeCliCommand` in this process, against the one store
/// `LibDB.Sqlite` points at. That is why they are `testSequenced`: one process has one
/// store, so every store-writing test waits for the one before it, and 217 of them are
/// most of the suite's wall clock.
///
/// A command run as a CHILD has none of that. `HOME` decides where an install lives
/// (`~/.darklang`), so an instance is a home directory and a store of its own in it.
/// A command costs a process start rather than a dispatch, call it a third more, and in
/// exchange the tests run as wide as everything else does.
///
/// The Dark side of this is `Darklang.Cli.Tests.Instance`, same idea for tests written in
/// `.dark`. This one exists because Expecto is what runs tests in parallel.
module Tests.CliInstance

open System.Threading.Tasks

open Prelude

/// Where an instance lives (`dir` is its `<home>/.darklang`), and what drives it.
type T = { dir : string; cli : string }

let private homeOf (dir : string) : string = System.IO.Path.GetDirectoryName dir

/// The binary under test.
///
/// `run-fsharp-tests` sets this, because only it knows whether this is a `--published`
/// run. The fallback is the debug build, which is what a bare `dotnet run` of the suite
/// would have.
let private cliPath () : string =
  let path =
    match System.Environment.GetEnvironmentVariable "DARK_TEST_CLI" with
    | null
    | "" -> "backend/Build/out/Cli/Debug/net11.0/Cli"
    | path -> path

  // Named rather than left to fail as "cannot start process": the two ways this goes wrong
  // are a `--published` run before the publish step and a debug run before any build, and
  // neither says so on its own.
  if not (System.IO.File.Exists path) then
    Expecto.Tests.failtestf
      "no CLI to drive at %s (DARK_TEST_CLI); build it, or run through scripts/run-fsharp-tests"
      path

  path

/// In the temp directory, not rundir: the dev container keeps /tmp in memory. One per process,
/// removed when it exits.
let private instanceRoot : Lazy<string> =
  lazy
    (let dir =
      System.IO.Directory.CreateTempSubdirectory("dark-cli-instances-").FullName
     System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
       try
         System.IO.Directory.Delete(dir, true)
       with _ ->
         ())
     dir)

let private backup (source : string) (dest : string) : unit =
  for suffix in [ ""; "-wal"; "-shm" ] do
    if System.IO.File.Exists(dest + suffix) then
      System.IO.File.Delete(dest + suffix)

  use src =
    new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={source};Mode=ReadOnly")
  src.Open()
  use dst =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={dest};Mode=ReadWriteCreate;Pooling=False"
    )
  dst.Open()
  src.BackupDatabase dst

/// A fresh `<path>/.darklang`, whatever was there before.
let private freshDir (path : string) : string =
  if System.IO.Directory.Exists path then System.IO.Directory.Delete(path, true)
  let dir = System.IO.Path.Combine(path, ".darklang")
  System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
  dir

/// The template's install: its store is the file every instance's store links to.
let private templateDir () : string =
  System.IO.Path.Combine(instanceRoot.Value, "template", ".darklang")

/// Copy the store before any test runs, and build the instance template from this copy. Built
/// from the live store instead, it would include whatever earlier tests left behind, so every
/// instance would start differently depending on which tests ran first.
let snapshotStore () : unit =
  let dir = freshDir (System.IO.Path.GetDirectoryName(templateDir ()))
  backup LibDB.Sqlite.currentDbPath (System.IO.Path.Combine(dir, "data.db"))

let private childEnv
  (dir : string)
  (psi : System.Diagnostics.ProcessStartInfo)
  : unit =
  psi.Environment["DARK_CONFIG_DB_NAME"] <- "data.db"
  psi.Environment["HOME"] <- homeOf dir
  psi.Environment["USERPROFILE"] <- homeOf dir
  psi.Environment["DARK_CONFIG_TELEMETRY_EXPORTER"] <- "none"
  // tests read what a person sees; a pipe would otherwise get plain output
  psi.Environment["DARK_OUTPUT"] <- "pretty"

/// Run one command in `dir` and wait, discarding what it said. For preparing the template;
/// tests use `runRaw`.
let private prepare (dir : string) (args : string list) : unit =
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- cliPath ()
  for arg in args do
    psi.ArgumentList.Add arg
  psi.UseShellExecute <- false
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  childEnv dir psi
  use p = System.Diagnostics.Process.Start psi
  p.StandardOutput.ReadToEnd() |> ignore<string>
  p.StandardError.ReadToEnd() |> ignore<string>
  p.WaitForExit()

/// What every instance starts from, prepared ONCE per process: a store and a policy.
///
/// Three things happen here and each has to. The live store is WAL, so it is read through
/// SQLite rather than copied. It is an OP LOG, so the first command against a cold store
/// materialises every package from it -- seconds, paid once here instead of in each
/// instance's first command. And a fresh install's policy denies guests `package-write`,
/// so anything a test does through `dark eval` would be refused; the in-process harness grants
/// it in `buildState`, for the same reason.
///
/// `branches` rather than `version` for the warming command: `version` checks GitHub for a
/// newer release, deliberately, so it is a network round trip rather than a cheap way to
/// touch the store.
let private template : Lazy<string * string> =
  lazy
    (let dir = templateDir ()
     let db = System.IO.Path.Combine(dir, "data.db")
     if not (System.IO.File.Exists db) then
       Expecto.Tests.failtest
         "no store snapshot to build instances from (snapshotStore)"

     prepare dir [ "branches" ]
     prepare dir [ "permissions"; "allow"; "package-write" ]

     // The warming run left its work in the store's WAL; into the file with it, before anything
     // links to it. Nothing writes it again: every instance is a layer over it.
     do
       use conn =
         new Microsoft.Data.Sqlite.SqliteConnection(
           $"Data Source={db};Pooling=False"
         )
       conn.Open()
       use cmd = conn.CreateCommand()
       cmd.CommandText <- "PRAGMA wal_checkpoint(TRUNCATE)"
       cmd.ExecuteNonQuery() |> ignore<int>
     System.IO.File.WriteAllText(db + "-layer", "")
     db, System.IO.Path.Combine(dir, "policy"))

let private templateFile () : int64 * System.DateTime =
  let info = System.IO.FileInfo(fst (template.Force()))
  info.Length, info.LastWriteTimeUtc

let private templateAsMade = lazy (templateFile ())

/// Fails the test if the template's file has changed: something checkpointed a layer over it, so
/// its writes reached every instance made after it ("Test stores are layers", docs/internals.md).
let checkTemplate () : unit =
  if templateFile () <> templateAsMade.Force() then
    Expecto.Tests.failtest
      "a connection checkpointed an instance's store into the template every instance shares; open instance stores with CliInstance.Store"

let private script (name : string) : string =
  System.IO.Path.Combine(LibConfig.Dirs.sourceRootDir, "scripts/testing", name)

let private runScript
  (failure : string)
  (name : string)
  (args : string list)
  : unit =
  let psi = System.Diagnostics.ProcessStartInfo(script name)
  for arg in args do
    psi.ArgumentList.Add arg
  psi.UseShellExecute <- false
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  use p = System.Diagnostics.Process.Start psi
  let out = p.StandardOutput.ReadToEndAsync()
  let err = p.StandardError.ReadToEnd()
  p.WaitForExit()
  if p.ExitCode <> 0 then
    Expecto.Tests.failtestf "%s: %s%s" failure out.Result err

let rec private copyDir (source : string) (dest : string) : unit =
  System.IO.Directory.CreateDirectory dest |> ignore<System.IO.DirectoryInfo>
  for file in System.IO.Directory.GetFiles source do
    System.IO.File.Copy(
      file,
      System.IO.Path.Combine(dest, System.IO.Path.GetFileName file)
    )
  for dir in System.IO.Directory.GetDirectories source do
    copyDir dir (System.IO.Path.Combine(dest, System.IO.Path.GetFileName dir))

let private seeded (ownFile : bool) : T =
  let dir =
    System.IO.Path.Combine(
      instanceRoot.Value,
      System.Guid.NewGuid().ToString("N"),
      ".darklang"
    )
  System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
  let (db, policy) = template.Force()
  templateAsMade.Force() |> ignore<int64 * System.DateTime>
  let store = System.IO.Path.Combine(dir, "data.db")
  if ownFile then
    System.IO.File.Copy(db, store)
  else
    runScript "could not make an instance" "layer-store" [ db; store ]
  if System.IO.Directory.Exists policy then
    copyDir policy (System.IO.Path.Combine(dir, "policy"))
  { dir = dir; cli = cliPath () }

/// A fresh instance, seeded from the template: a layer over it (`layer-store`).
let create () : T = seeded false

/// The file every instance's store links to.
let templatePath () : string = fst (template.Force())

/// Make <param store> a layer over <param source> (`layer-store`).
let layerStore (source : string) (store : string) : unit =
  runScript "could not make a layer" "layer-store" [ source; store ]

/// Give <param i> a fresh store, as `create` makes one.
let relayer (i : T) : unit =
  layerStore (templatePath ()) (System.IO.Path.Combine(i.dir, "data.db"))

/// A fresh instance with a store file of its own: for a test of how `dark test run` shares a
/// store that is not a layer.
let createWithOwnFile () : T = seeded true

/// Fails the test if <param i>'s store binds a name to a hash with no body, as it is or after a
/// re-fold of its log.
let checkBindings (i : T) : unit =
  checkTemplate ()
  runScript
    "the store's bindings are broken"
    "check-store-bindings"
    [ System.IO.Path.Combine(i.dir, "data.db"); fst (template.Force()); i.cli ]

/// The store at <param path>, which may be an instance's, opened in this process.
///
/// Every instance's store shares the template's file (`layer-store`) and keeps its changes in its
/// WAL, so this connection never checkpoints. One at a time: SQLite shares a file's WAL index by
/// inode, so two instances open at once here would read each other's changes.
type Store(path : string) =
  static let gate = new System.Threading.SemaphoreSlim(1)
  do gate.Wait()
  let conn =
    try
      let conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={path};Pooling=False"
        )
      conn.Open()
      LibDB.Sqlite.neverCheckpoint conn
      conn
    with _ ->
      gate.Release() |> ignore<int>
      reraise ()
  member _.Connection = conn
  interface System.IDisposable with
    member _.Dispose() =
      conn.Dispose()
      gate.Release() |> ignore<int>

let dispose (i : T) : unit =
  try
    System.IO.Directory.Delete(homeOf i.dir, true)
  with _ ->
    () // a leaked instance goes with the rest of instanceRoot when the process exits

/// How long one command may take before the test says so, rather than hanging the run.
/// Generous on purpose: one `test run` launches a CLI per test, one after another, and a
/// loaded box makes each of them slow.
let private commandTimeout = System.TimeSpan.FromMinutes 5.0

/// Run one command and return (exitCode, stdout, stderr).
let rec runRaw (i : T) (args : string list) : Task<int * string * string> =
  runRawWithBytes i args None

/// `runRaw`, with `input` written to the command's stdin before it is closed.
and runRawWithInput
  (i : T)
  (args : string list)
  (input : Option<string>)
  : Task<int * string * string> =
  runRawWithBytes i args (input |> Option.map System.Text.Encoding.UTF8.GetBytes)

/// `runRawWithInput`, for input that need not be text.
and runRawWithBytes
  (i : T)
  (args : string list)
  (input : Option<byte[]>)
  : Task<int * string * string> =
  task {
    let psi = System.Diagnostics.ProcessStartInfo()
    psi.FileName <- i.cli
    for arg in args do
      psi.ArgumentList.Add arg
    psi.UseShellExecute <- false
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    childEnv i.dir psi

    use p = System.Diagnostics.Process.Start psi
    // Closed straight after any input: a command that asks must find nothing there and
    // refuse, which is what several of these tests assert. An open pipe would make it wait.
    match input with
    | Some bytes -> p.StandardInput.BaseStream.Write(bytes, 0, bytes.Length)
    | None -> ()
    p.StandardInput.Close()

    // Both streams drained concurrently: a child that fills one pipe while we read the
    // other deadlocks.
    let stdout = p.StandardOutput.ReadToEndAsync()
    let stderr = p.StandardError.ReadToEndAsync()

    let exited = p.WaitForExitAsync()
    let! _ = Task.WhenAny(exited, Task.Delay commandTimeout)

    if not p.HasExited then
      p.Kill true
      Expecto.Tests.failtestf
        "timed out after %A: dark %s"
        commandTimeout
        (String.concat " " args)

    let! out = stdout
    let! err = stderr
    return (p.ExitCode, out, err)
  }

/// What a command printed. A failure EXITS NONZERO and explains itself on stdout, so
/// dropping stdout on a nonzero exit would hide the very text these tests read; stderr is
/// only surfaced when stdout said nothing.
/// Both streams, stdout then stderr. Not stdout alone: a refusal's reason is on stderr, often beside
/// stdout lines, and dropping it hides what the test asserts on. Two pipes cannot be interleaved after
/// the fact; a test about order or about which stream a line is on wants `runRaw`.
let bothStreams (out : string) (err : string) : string =
  match out.Trim(), err.Trim() with
  | "", e -> e
  | o, "" -> o
  | o, e -> o + "\n" + e

let run (i : T) (args : string list) : Task<string> =
  task {
    let! (_code, out, err) = runRaw i args
    return bothStreams out err
  }

let runWithExit (i : T) (args : string list) : Task<string * int64> =
  task {
    let! (code, out, err) = runRaw i args
    return (bothStreams out err, int64 code)
  }

/// Start a command in this instance and return at once, with its stdout and stderr for the caller
/// to read, and `env` set on top of the instance's own. The caller stops it with `stop`.
let startUndrainedWithEnv
  (i : T)
  (env : List<string * string>)
  (args : string list)
  : System.Diagnostics.Process =
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- i.cli
  for arg in args do
    psi.ArgumentList.Add arg
  psi.UseShellExecute <- false
  psi.RedirectStandardInput <- true
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  childEnv i.dir psi
  for (name, value) in env do
    psi.Environment[name] <- value
  let p = System.Diagnostics.Process.Start psi
  p.StandardInput.Close()
  p

let startUndrained (i : T) (args : string list) : System.Diagnostics.Process =
  startUndrainedWithEnv i [] args

/// Start a command that keeps running (`serve`) in this instance and return at once. What it
/// prints is drained and dropped, so a full pipe never stops it. The caller stops it with `stop`.
let start (i : T) (args : string list) : System.Diagnostics.Process =
  let p = startUndrained i args
  p.OutputDataReceived.Add(ignore<System.Diagnostics.DataReceivedEventArgs>)
  p.ErrorDataReceived.Add(ignore<System.Diagnostics.DataReceivedEventArgs>)
  p.BeginOutputReadLine()
  p.BeginErrorReadLine()
  p

/// Stop a process `start` started, and everything it started.
let stop (p : System.Diagnostics.Process) : unit =
  try
    if not p.HasExited then
      p.Kill true
      p.WaitForExit 10000 |> ignore<bool>
  with _ ->
    ()
  p.Dispose()

/// A test with an install of its own, disposed whether it passed or not.
///
/// No `testSequenced` around these: the store is this test's, so there is nothing to
/// serialise against.
let instanceTest (name : string) (body : T -> Task<unit>) : Expecto.Test =
  Expecto.Tests.testTask name {
    let i = create ()
    try
      do! body i
      checkBindings i
    finally
      dispose i
  }
