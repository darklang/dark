/// The machine's Dark processes: one file per OS process under the instance's rundir, written
/// when the CLI starts and removed when it exits, so `dark ps` from any shell can list what is
/// running on the box (a serve, a daemon, another terminal's TUI), not only itself. A file whose
/// pid is gone is stale and dropped by the next reader; that is what survives a crash or a kill.
///
/// This is the host side; `dark ps` renders it. What is inside another OS process (its own Dark
/// process tree) is that process's, not the registry's: the registry is the outer ring, one row
/// per OS process, and reaching into a row is a signal (`ps cancel` sends INT, the Ctrl-C path
/// that suspends a traced run; `ps kill` sends KILL).
module LibExecution.HostRegistry

open System
open System.IO

type Entry =
  {
    pid : int
    /// What a system monitor shows (`HostProcess.setProcessTitle`).
    title : string
    /// The command line as launched.
    command : string
    /// The branch the process runs on, when it had one at startup.
    branch : string
    started : DateTime
  }

let mutable private directory : string = ""

let private alive (pid : int) : bool =
  if OperatingSystem.IsLinux() then
    Directory.Exists $"/proc/{pid}"
  else
    try
      Diagnostics.Process.GetProcessById(pid) |> ignore<Diagnostics.Process>
      true
    with _ ->
      false

// Written with the writer and read with the document, never the reflecting serializer, which
// the published (AOT) binary does not have.
let private serialize (e : Entry) : string =
  use buffer = new MemoryStream()
  (use w = new Text.Json.Utf8JsonWriter(buffer)
   w.WriteStartObject()
   w.WriteNumber("pid", e.pid)
   w.WriteString("title", e.title)
   w.WriteString("command", e.command)
   w.WriteString("branch", e.branch)
   w.WriteString("started", e.started.ToString "O")
   w.WriteEndObject())
  Text.Encoding.UTF8.GetString(buffer.ToArray())

let private parse (json : string) : Option<Entry> =
  try
    use doc = Text.Json.JsonDocument.Parse json
    let root = doc.RootElement
    let text (name : string) = root.GetProperty(name).GetString()
    Some
      { pid = root.GetProperty("pid").GetInt32()
        title = text "title"
        command = text "command"
        branch = text "branch"
        started =
          DateTime.Parse(
            text "started",
            null,
            Globalization.DateTimeStyles.RoundtripKind
          ) }
  with _ ->
    None

/// Where the files live: `<rundir>/run/ps`, beside the daemons' pidfiles. Set once by the CLI.
let setDirectory (rundir : string) : unit =
  directory <- Path.Combine(rundir, "run", "ps")

/// Record this process. Once, at startup, after the title is known. Never raises: a rundir that
/// cannot be written means this process is simply not listed.
let register (title : string) (command : string) (branch : string) : unit =
  if directory <> "" then
    try
      Directory.CreateDirectory directory |> ignore<DirectoryInfo>
      let pid = Environment.ProcessId
      let path = Path.Combine(directory, $"{pid}.json")
      File.WriteAllText(
        path,
        serialize
          { pid = pid
            title = title
            command = command
            branch = branch
            started = DateTime.UtcNow }
      )
      let remove () =
        try
          File.Delete path
        with _ ->
          ()
      AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> remove ())
      // A TERM does not always reach ProcessExit through a blocking wait; remove the file
      // first, then let the default termination proceed.
      if not (OperatingSystem.IsWindows()) then
        Runtime.InteropServices.PosixSignalRegistration.Create(
          Runtime.InteropServices.PosixSignal.SIGTERM,
          (fun ctx ->
            remove ()
            ctx.Cancel <- false)
        )
        |> ignore<Runtime.InteropServices.PosixSignalRegistration>
    with _ ->
      ()

/// Every Dark process on the machine that is still alive, oldest first. Stale files are removed
/// on the way.
let list () : Entry list =
  if directory = "" || not (Directory.Exists directory) then
    []
  else
    Directory.GetFiles(directory, "*.json")
    |> Array.choose (fun path ->
      match
        parse (
          try
            File.ReadAllText path
          with _ ->
            ""
        )
      with
      | Some e when alive e.pid -> Some e
      | _ ->
        (try
          File.Delete path
         with _ ->
           ())
        None)
    |> Array.sortBy (fun e -> e.started)
    |> Array.toList
