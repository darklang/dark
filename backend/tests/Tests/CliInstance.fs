/// A throwaway Darklang install, for tests that only read what a command PRINTED.
///
/// The CLI suites drive `executeCliCommand` in this process, against the one store
/// `LibDB.Sqlite` points at. That is why they are `testSequenced`: one process has one
/// store, so every store-writing test waits for the one before it, and 217 of them are
/// most of the suite's wall clock.
///
/// A command run as a CHILD has none of that. `DARK_CONFIG_RUNDIR` and `HOME` already
/// decide where an install lives, so an instance is a directory and a copy of the store.
/// A command costs a process start rather than a dispatch, call it a third more, and in
/// exchange the tests run as wide as everything else does.
///
/// The Dark side of this is `Darklang.Cli.Tests.Instance`, same idea for tests written in
/// `.dark`. This one exists because Expecto is what runs tests in parallel.
module Tests.CliInstance

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

/// Where an instance lives, and what drives it.
type T = { dir : string; cli : string }

/// The binary under test.
///
/// `run-backend-tests` sets this, because only it knows whether this is a `--published`
/// run. The fallback is the debug build, which is what a bare `dotnet run` of the suite
/// would have.
let private cliPath () : string =
  let path =
    match System.Environment.GetEnvironmentVariable "DARK_TEST_CLI" with
    | null
    | "" -> "backend/Build/out/Cli/Debug/net10.0/Cli"
    | path -> path

  // Named rather than left to fail as "cannot start process": the two ways this goes wrong
  // are a `--published` run before the publish step and a debug run before any build, and
  // neither says so on its own.
  if not (System.IO.File.Exists path) then
    Expecto.Tests.failtestf
      "no CLI to drive at %s (DARK_TEST_CLI); build it, or run through scripts/run-backend-tests"
      path

  path

let private instanceRoot () : string =
  System.IO.Path.Combine(LibConfig.Config.runDir, "cli-instances")

let private backup (source : string) (dest : string) : unit =
  for suffix in [ ""; "-wal"; "-shm" ] do
    if System.IO.File.Exists(dest + suffix) then
      System.IO.File.Delete(dest + suffix)

  use src =
    new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={source};Mode=ReadOnly")
  src.Open()
  use dst =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={dest};Mode=ReadWriteCreate"
    )
  dst.Open()
  src.BackupDatabase dst

/// A fresh directory, whatever was there before.
let private freshDir (path : string) : string =
  if System.IO.Directory.Exists path then System.IO.Directory.Delete(path, true)
  System.IO.Directory.CreateDirectory path |> ignore<System.IO.DirectoryInfo>
  path

let private childEnv
  (dir : string)
  (psi : System.Diagnostics.ProcessStartInfo)
  : unit =
  // Absolute, and ending in a separator: `LibConfig` appends the database name to this
  // directly, and refuses a relative one outright.
  psi.Environment["DARK_CONFIG_RUNDIR"] <-
    dir + string System.IO.Path.DirectorySeparatorChar
  psi.Environment["DARK_CONFIG_DB_NAME"] <- "data.db"
  psi.Environment["HOME"] <- dir
  psi.Environment["DARK_CONFIG_TELEMETRY_EXPORTER"] <- "none"

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
/// so anything a test does through `dark eval` would be refused; the in-process harness
/// grants the same thing in `buildState`, for the same reason.
///
/// `branches` rather than `version` for the warming command: `version` checks GitHub for a
/// newer release, deliberately, so it is a network round trip rather than a cheap way to
/// touch the store.
let private template : Lazy<string * string> =
  lazy
    (let root = instanceRoot ()
     System.IO.Directory.CreateDirectory root |> ignore<System.IO.DirectoryInfo>

     let warm = freshDir (System.IO.Path.Combine(root, "template-warm"))
     backup LibDB.Sqlite.currentDbPath (System.IO.Path.Combine(warm, "data.db"))

     prepare warm [ "branches" ]
     prepare warm [ "permissions"; "allow"; "package-write" ]

     // Through SQLite again: the warming run left its work in that store's own WAL.
     let db = System.IO.Path.Combine(root, "template.db")
     backup (System.IO.Path.Combine(warm, "data.db")) db
     db, System.IO.Path.Combine(warm, "policy"))

let rec private copyDir (source : string) (dest : string) : unit =
  System.IO.Directory.CreateDirectory dest |> ignore<System.IO.DirectoryInfo>
  for file in System.IO.Directory.GetFiles source do
    System.IO.File.Copy(
      file,
      System.IO.Path.Combine(dest, System.IO.Path.GetFileName file)
    )
  for dir in System.IO.Directory.GetDirectories source do
    copyDir dir (System.IO.Path.Combine(dest, System.IO.Path.GetFileName dir))

/// A fresh instance, seeded from the template.
let create () : T =
  let dir =
    System.IO.Path.Combine(instanceRoot (), System.Guid.NewGuid().ToString("N"))
  System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
  let (db, policy) = template.Force()
  System.IO.File.Copy(db, System.IO.Path.Combine(dir, "data.db"))
  if System.IO.Directory.Exists policy then
    copyDir policy (System.IO.Path.Combine(dir, "policy"))
  { dir = dir; cli = cliPath () }

let dispose (i : T) : unit =
  try
    System.IO.Directory.Delete(i.dir, true)
  with _ ->
    () // a leaked instance is under rundir, where it is visible and cheap to sweep

/// How long one command may take before the test says so, rather than hanging the run.
let private commandTimeout = System.TimeSpan.FromMinutes 2.0

/// Run one command and return (exitCode, stdout, stderr).
let runRaw (i : T) (args : string list) : Task<int * string * string> =
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
    // Closed immediately: a command that asks must find nothing there and refuse, which is
    // what several of these tests assert. An open pipe would make it wait instead.
    p.StandardInput.Close()

    // Both streams drained concurrently: a child that fills one pipe while we read the
    // other deadlocks.
    let stdout = p.StandardOutput.ReadToEndAsync()
    let stderr = p.StandardError.ReadToEndAsync()

    let exited = p.WaitForExitAsync()
    let! _ = Task.WhenAny(exited, Task.Delay commandTimeout)

    if not p.HasExited then
      p.Kill true
      return
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
let run (i : T) (args : string list) : Task<string> =
  task {
    let! (_code, out, err) = runRaw i args
    let out = out.Trim()
    if out <> "" then return out else return err.Trim()
  }

let runWithExit (i : T) (args : string list) : Task<string * int64> =
  task {
    let! (code, out, err) = runRaw i args
    let out = out.Trim()
    return ((if out <> "" then out else err.Trim()), int64 code)
  }

/// A test with an install of its own, disposed whether it passed or not.
///
/// No `testSequenced` around these: the store is this test's, so there is nothing to
/// serialise against.
let instanceTest (name : string) (body : T -> Task<unit>) : Expecto.Test =
  Expecto.Tests.testTask name {
    let i = create ()
    try
      do! body i
    finally
      dispose i
  }
