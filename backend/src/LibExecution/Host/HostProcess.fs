/// Host-owned child processes.
///
/// The process table lives here, behind the checked boundary; guest code holds
/// only opaque integer handles and reaches the table through
/// `Host.perform`. Moved from the CLI builtins so the door owns the resource,
/// not the code asking to use it.
module LibExecution.HostProcess

open Prelude
open System.IO
open System.Runtime.InteropServices

/// Private CLI entry point. Establish ownership before starting the worker,
/// including when the caller substitutes a wrapper executable.
let launchOwnedProcess (program : string) (args : List<string>) : int =
  if HostPosix.isPosix then
    match HostPosix.execInNewSession program args with
    | Error(_, message) ->
      System.Console.Error.WriteLine
        $"Could not launch child process worker: {message}"
      127
    | Ok() -> 0 // exec does not return on success
  else
    invalidOp "Supervised processes are unsupported on this platform"

/// One managed launch and its supervised child. No capturing or waiting here:
/// shared Dark consumes nonblocking owned pipe descriptors using syscall leaves.
type Child =
  { proc : System.Diagnostics.Process
    program : string
    args : List<string>
    descriptors : List<int>
    waitGate : obj
    // Shared by the waiters and disposal, under `waitGate`.
    mutable waiters : int
    mutable closing : bool }

let disposeChild (child : Child) : unit =
  lock child.waitGate (fun () ->
    if not child.closing then
      child.closing <- true
      try
        if not child.proc.HasExited then child.proc.Kill()
      with _ ->
        ()
      // An already-authorized blocking wait must remain usable while cleanup
      // kills the child. Dispose the supervisor after its last waiter returns.
      if child.waiters = 0 then child.proc.Dispose())

let startChild
  (program : string)
  (args : List<string>)
  (inheritTerminal : bool)
  : Child =
  if System.OperatingSystem.IsWindows() || System.OperatingSystem.IsBrowser() then
    raise (
      System.PlatformNotSupportedException("owned syscall child pipes require Unix")
    )
  let psi = System.Diagnostics.ProcessStartInfo(program)
  psi.UseShellExecute <- false
  for arg in args do
    psi.ArgumentList.Add arg
  psi.RedirectStandardInput <- not inheritTerminal
  psi.RedirectStandardOutput <- not inheritTerminal
  psi.RedirectStandardError <- not inheritTerminal
  let proc = System.Diagnostics.Process.Start psi
  // Filled as each descriptor is made, so a failure partway closes the ones already made.
  let duplicated = System.Collections.Generic.List<int>()
  let duplicate (stream : System.IO.Stream) =
    let pipe = stream :?> System.IO.Pipes.PipeStream
    let handle = pipe.SafePipeHandle
    // DangerousAddRef reports through a byref whether it took the reference.
    let mutable held = false
    try
      handle.DangerousAddRef(&held)
      let fd = int (handle.DangerousGetHandle())
      let command = if System.OperatingSystem.IsMacOS() then 67 else 1030
      let copy = HostKernel.fcntl_raw (fd, command, 0)
      if copy < 0 then raise (System.ComponentModel.Win32Exception(-copy))
      duplicated.Add copy
      let flags = HostKernel.fcntl_raw (copy, 3, 0)
      if flags < 0 then raise (System.ComponentModel.Win32Exception(-flags))
      let nonblocking = if System.OperatingSystem.IsMacOS() then 4 else 2048
      let rc = HostKernel.fcntl_raw (copy, 4, flags ||| nonblocking)
      if rc < 0 then raise (System.ComponentModel.Win32Exception(-rc))
      copy
    finally
      if held then handle.DangerousRelease()
  try
    let descriptors =
      if inheritTerminal then
        []
      else
        let descriptors =
          [ duplicate proc.StandardInput.BaseStream
            duplicate proc.StandardOutput.BaseStream
            duplicate proc.StandardError.BaseStream ]
        // Only the duplicated descriptors own the pipe ends now. In particular,
        // closing owned stdin delivers EOF rather than retaining a hidden writer.
        proc.StandardInput.Dispose()
        proc.StandardOutput.Dispose()
        proc.StandardError.Dispose()
        descriptors
    { proc = proc
      program = program
      args = args
      descriptors = descriptors
      waitGate = obj ()
      waiters = 0
      closing = false }
  with _ ->
    for fd in duplicated do
      HostKernel.close_raw fd |> ignore<int>
    try
      if not proc.HasExited then proc.Kill()
    with _ ->
      ()
    proc.Dispose()
    reraise ()

let pollChild (child : Child) : Option<int> =
  if child.proc.HasExited then Some child.proc.ExitCode else None

let prepareChildWait (child : Child) : unit -> int =
  lock child.waitGate (fun () ->
    if child.closing then raise (System.ObjectDisposedException("owned child"))
    child.waiters <- child.waiters + 1)
  fun () ->
    try
      child.proc.WaitForExit()
      child.proc.ExitCode
    finally
      lock child.waitGate (fun () ->
        child.waiters <- child.waiters - 1
        if child.closing && child.waiters = 0 then child.proc.Dispose())

let killChild (child : Child) : unit =
  if not child.proc.HasExited then child.proc.Kill()

/// Runtime executable identity, checked as a read of its returned path by Host.
let currentExecutablePath () : string =
  System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName

/// Hash the executable and those of its installed runtime dependencies <param keep> accepts.
/// This works for both a single NativeAOT binary and a managed build with DLLs.
let private hashRuntime (keep : string -> bool) : string =
  let executable = currentExecutablePath ()
  let directory = Path.GetDirectoryName executable
  let dependencies =
    Directory.GetFiles(directory, "*", SearchOption.AllDirectories)
    |> Array.filter (fun path ->
      (path.EndsWith(".dll")
       || path.EndsWith(".deps.json")
       || path.EndsWith(".runtimeconfig.json")
       || path.EndsWith(".dylib")
       || (Path.GetFileName path).Contains(".so"))
      && keep path)
    |> Array.sort
  use hash =
    System.Security.Cryptography.IncrementalHash.CreateHash(
      System.Security.Cryptography.HashAlgorithmName.SHA256
    )
  hash.AppendData(
    System.Text.Encoding.UTF8.GetBytes RuntimeInformation.FrameworkDescription
  )
  for path in Array.append [| executable |] dependencies do
    use file = File.OpenRead path
    hash.AppendData(System.Security.Cryptography.SHA256.HashData file)
  System.Convert.ToHexStringLower(hash.GetHashAndReset())

/// The executable and all its installed runtime dependencies, hashed once per process.
let private runtimeHashValue = lazy (hashRuntime (fun _ -> true))

let runtimeHash () : string = runtimeHashValue.Value

/// `runtimeHash` with the DLLs of <param assemblies> left out, each named without `.dll`.
let runtimeHashWithout (assemblies : Set<string>) : string =
  hashRuntime (fun path ->
    not (
      path.EndsWith(".dll")
      && Set.contains (Path.GetFileNameWithoutExtension path) assemblies
    ))

/// <param files>, paths under <param root>, hashed by path and content. A missing file hashes as
/// missing, which differs from any content it had.
let sourcesHash (root : string) (files : Set<string>) : string =
  use hash =
    System.Security.Cryptography.IncrementalHash.CreateHash(
      System.Security.Cryptography.HashAlgorithmName.SHA256
    )
  for file in files do
    hash.AppendData(System.Text.Encoding.UTF8.GetBytes(file + "\n"))
    let path = Path.Combine(root, file)
    if File.Exists path then
      hash.AppendData(
        System.Security.Cryptography.SHA256.HashData(File.ReadAllBytes path)
      )
    else
      hash.AppendData(System.Text.Encoding.UTF8.GetBytes "missing\n")
  System.Convert.ToHexStringLower(hash.GetHashAndReset())

// Set once at startup, by a host whose worker entry point is another executable.
let mutable private processLauncher : Option<string> = None

/// Configure at host startup when another executable provides the worker entry point.
let setProcessLauncher (path : string) : unit =
  processLauncher <- Some(Path.GetFullPath path)

/// CLI hosts launch themselves unless startup configured a different executable.
let processLauncherPath (overrideVariable : Option<string>) : string =
  // Resolving the executable is part of the Native process operation, as PATH
  // resolution is for ProcessRun. It does not add an EnvRead requirement.
  let overridePath =
    overrideVariable
    |> Option.bind (fun name ->
      match System.Environment.GetEnvironmentVariable name with
      | null
      | "" -> None
      | path -> Some(Path.GetFullPath path))
  match overridePath |> Option.orElse processLauncher with
  | None -> currentExecutablePath ()
  | Some path -> path

let private startInfo
  (program : string)
  (args : List<string>)
  (redirectInput : bool)
  : System.Diagnostics.ProcessStartInfo =
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- program
  for arg in args do
    psi.ArgumentList.Add arg
  psi.UseShellExecute <- false
  psi.RedirectStandardInput <- redirectInput
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  psi.CreateNoWindow <- true
  psi

/// Captured child output is bounded so a flooding child cannot exhaust host
/// memory: read to the cap, keep draining past it (so the child never blocks
/// on a full pipe) but store no more. A resource limit, not a policy one.
let private maxCapturedOutput = 64 * 1024 * 1024

let private readCappedWithCancellation
  (cancellation : System.Threading.CancellationToken)
  (reader : System.IO.StreamReader)
  : System.Threading.Tasks.Task<string> =
  task {
    let builder = System.Text.StringBuilder()
    let buffer = Array.zeroCreate<char> 8192
    // A loop, not a recursive task: reads that complete synchronously would grow the stack.
    let mutable reading = true
    while reading do
      let! n = reader.ReadAsync(System.Memory<char>(buffer), cancellation)
      if n = 0 then
        reading <- false
      elif builder.Length < maxCapturedOutput then
        builder.Append(buffer, 0, min n (maxCapturedOutput - builder.Length))
        |> ignore<System.Text.StringBuilder>
    return builder.ToString()
  }

/// Prepare a file once and reuse it until disposal. Readers that never write can
/// share the file itself, read-only; anyone else gets a copy. File.Copy uses
/// filesystem cloning where supported, with a portable fallback.
let fileSnapshot
  (snapshot : string -> Result<unit, string>)
  : HostTypes.FileSnapshot =
  let gate = obj ()
  // Shared by every copy, link and close, under `gate`.
  let mutable disposed = false
  let mutable directory : Option<string> = None
  // Copies run outside the lock so that several can happen at once. Closing
  // waits for them, since it deletes the file they read.
  let mutable copying = 0
  let baseline =
    lazy
      (let dir = Directory.CreateTempSubdirectory("dark-snapshot-").FullName
       directory <- Some dir
       let path = Path.Combine(dir, "baseline")
       snapshot path |> Result.map (fun () -> path))
  let readOnly =
    lazy
      (match baseline.Value with
       | Error message -> Error message
       | Ok _ when System.OperatingSystem.IsWindows() ->
         Error "A shared snapshot needs Unix file modes"
       | Ok path ->
         File.SetUnixFileMode(path, UnixFileMode.UserRead)
         // Privileged users may bypass file modes. Verify that writes are refused.
         try
           use _ = new FileStream(path, FileMode.Open, FileAccess.Write)
           Error
             "This user can write read-only files, so the snapshot can't be shared"
         with :? System.UnauthorizedAccessException ->
           let info = FileInfo path
           Ok(path, info.Length, info.LastWriteTimeUtc))
  // Once shared, the file must stay as it was: every later reader starts from it.
  // Only something that makes it writable again can change it, and then copying
  // and linking refuse rather than hand the change to everyone after.
  let changed () =
    readOnly.IsValueCreated
    && (match readOnly.Value with
        | Ok(path, length, written) ->
          let info = FileInfo path
          File.GetUnixFileMode path <> UnixFileMode.UserRead
          || info.Length <> length
          || info.LastWriteTimeUtc <> written
        | Error _ -> false)
  let usable () =
    if disposed then
      Error "The file snapshot has already been closed"
    elif changed () then
      Error
        "The file snapshot changed after it was shared, so nothing can start from it"
    else
      Ok()
  let close () =
    lock gate (fun () ->
      disposed <- true
      while copying > 0 do
        System.Threading.Monitor.Wait gate |> ignore<bool>
      match directory with
      | None -> ()
      | Some dir ->
        Directory.Delete(dir, true)
        directory <- None)
  let onExit =
    System.EventHandler(fun _ _ ->
      try
        close ()
      with _ ->
        ())
  System.AppDomain.CurrentDomain.ProcessExit.AddHandler onExit
  { new HostTypes.FileSnapshot with
      member _.CopyTo target =
        let source =
          lock gate (fun () ->
            usable ()
            |> Result.bind (fun () ->
              try
                baseline.Value
                |> Result.map (fun path ->
                  copying <- copying + 1
                  path)
              with e ->
                Error e.Message))
        match source with
        | Error message -> Error message
        | Ok path ->
          try
            try
              File.Copy(path, target)
              // A copy keeps the snapshot's mode, which is read-only once shared.
              if not (System.OperatingSystem.IsWindows()) then
                File.SetUnixFileMode(
                  target,
                  UnixFileMode.UserRead ||| UnixFileMode.UserWrite
                )
              Ok()
            with e ->
              Error e.Message
          finally
            lock gate (fun () ->
              copying <- copying - 1
              System.Threading.Monitor.PulseAll gate)
      member _.LinkTo target =
        lock gate (fun () ->
          usable ()
          |> Result.bind (fun () ->
            try
              readOnly.Value
              |> Result.map (fun (path, _, _) ->
                File.CreateSymbolicLink(target, path) |> ignore<FileSystemInfo>)
            with e ->
              Error e.Message))
      member _.Changed() = lock gate changed
      member _.Dispose() =
        try
          close ()
        finally
          System.AppDomain.CurrentDomain.ProcessExit.RemoveHandler onExit }


/// How many times slower than an idle machine a child runs right now: the 1-minute load
/// average per core, at least 1 and at most <param ceiling>. 1 where there is no
/// `/proc/loadavg`.
let private loadFactor (ceiling : float) : float =
  try
    let load1 =
      System.Double.Parse(
        (File.ReadAllText "/proc/loadavg").Split(' ')[0],
        System.Globalization.CultureInfo.InvariantCulture
      )
    load1 / float System.Environment.ProcessorCount |> max 1.0 |> min ceiling
  with _ ->
    1.0

/// Wait for <param task> for <param timeoutMs> of idle-machine time. Each second waited
/// counts as 1/<c loadFactor> of a second, so on a machine running five times more work
/// than it has cores the wait lasts up to five times as long, and never more than that:
/// a child that is stuck rather than slow is still stopped. Returns the wall-clock
/// milliseconds waited when the budget ran out.
let private waitScaledByLoad
  (task : System.Threading.Tasks.Task)
  (timeoutMs : int)
  : Option<int64> =
  let maxFactor = 5.0
  let clock = System.Diagnostics.Stopwatch.StartNew()
  let rec wait (budget : float) : Option<int64> =
    if budget <= 0.0 then
      Some clock.ElapsedMilliseconds
    else
      let factor = loadFactor maxFactor
      let slice = min 1000.0 (budget * factor) |> ceil |> int
      if task.Wait slice then None else wait (budget - float slice / factor)
  wait (float timeoutMs)

/// Supervise a child and its descendants in a disposable directory. Preparation
/// happens before the deadline, which covers both process exit and output pipes.
/// The deadline stretches with machine load (see <c waitScaledByLoad>).
let runInWorkspace
  (launcher : string)
  (prefix : string)
  (prepare : string -> Result<HostTypes.ProcessWorkspace, string>)
  (timeoutMs : int)
  : Result<int * string * string * Option<byte[]> * List<string>, int * string> =
  let dir = Directory.CreateTempSubdirectory(prefix).FullName
  // Shared with the Ctrl-C and exit handlers below, which stop whatever child is running.
  let mutable child : System.Diagnostics.Process option = None
  use cancellation = new System.Threading.CancellationTokenSource()
  let stop () =
    let stopGroup (p : System.Diagnostics.Process) =
      if HostPosix.isPosix then
        // The launcher creates a group whose ID is its PID; that group remains
        // addressable after the worker exits and its children are reparented.
        match HostPosix.kill (-p.Id) 9 with
        | Ok()
        | Error(3, _) -> [] // ESRCH: no group, including a launcher not yet ready
        | Error(_, message) ->
          [ $"Could not stop child process process group: {message}" ]
      else
        []
    match child with
    | Some p ->
      let workerErrors =
        try
          // Traverse while the worker is alive: nested workers may own separate
          // groups. Also handles cancellation before the launcher is ready.
          if not p.HasExited then p.Kill true
          if p.WaitForExit 5000 then
            []
          else
            [ "Child process worker did not exit after being killed" ]
        with e ->
          [ $"Could not stop child process worker: {e.Message}" ]
      let groupErrors =
        try
          // The group survives its leader and catches reparented descendants.
          stopGroup p
        with e ->
          [ $"Could not stop child process process group: {e.Message}" ]
      workerErrors @ groupErrors
    | None -> []
  let removeDirectory () =
    try
      Directory.Delete(dir, true)
      []
    with e ->
      [ $"Could not remove child process directory {dir}: {e.Message}" ]
  let onCancel =
    System.ConsoleCancelEventHandler(fun _ _ -> stop () |> ignore<List<string>>)
  let onExit =
    System.EventHandler(fun _ _ ->
      stop () |> ignore<List<string>>
      removeDirectory () |> ignore<List<string>>)
  System.Console.CancelKeyPress.AddHandler onCancel
  System.AppDomain.CurrentDomain.ProcessExit.AddHandler onExit
  let cleanup () : List<string> =
    cancellation.Cancel()
    let stopErrors = stop ()
    let disposeErrors =
      match child with
      | Some p ->
        try
          p.Dispose()
          []
        with e ->
          [ $"Could not dispose child process worker: {e.Message}" ]
      | None -> []
    child <- None
    System.Console.CancelKeyPress.RemoveHandler onCancel
    System.AppDomain.CurrentDomain.ProcessExit.RemoveHandler onExit
    stopErrors @ disposeErrors @ removeDirectory ()
  // Every failure is caught and becomes an `Error`, so cleanup always runs next.
  let outcome =
    try
      match prepare dir with
      | Error message -> Error(-1, message)
      | Ok spec ->
        for directory in spec.directories do
          Directory.CreateDirectory(Path.Combine(dir, directory))
          |> ignore<DirectoryInfo>
        for path, bytes in spec.files do
          File.WriteAllBytes(Path.Combine(dir, path), bytes)
        let resultPath = Path.Combine(dir, spec.resultFile)
        let psi =
          startInfo
            launcher
            ([ "--process-launch"; spec.program ] @ spec.arguments)
            true
        for key, value in spec.environment do
          match value with
          | None -> psi.Environment.Remove key |> ignore<bool>
          | Some value -> psi.Environment[key] <- value
        let p = System.Diagnostics.Process.Start psi
        child <- Some p
        p.StandardInput.Close()
        let stdout = readCappedWithCancellation cancellation.Token p.StandardOutput
        let stderr = readCappedWithCancellation cancellation.Token p.StandardError
        // One deadline covers exit AND both pipes. A descendant can hold a
        // pipe open after the worker exits, so waiting only for exit is unsafe.
        let finished =
          System.Threading.Tasks.Task.WhenAll
            [| p.WaitForExitAsync(cancellation.Token)
               stdout :> System.Threading.Tasks.Task
               stderr :> System.Threading.Tasks.Task |]
        match waitScaledByLoad finished timeoutMs with
        | Some waited ->
          Error(
            110,
            $"Child process timed out after {waited} ms, a {timeoutMs} ms budget stretched for machine load (worker or output still open)"
          )
        | None ->
          let result =
            if File.Exists resultPath then
              if (FileInfo resultPath).Length > int64 maxCapturedOutput then
                Exception.raiseInternal
                  "Child process result exceeds the size limit"
                  []
              Some(File.ReadAllBytes resultPath)
            else
              None
          Ok(p.ExitCode, stdout.Result, stderr.Result, result)
    with e ->
      Error(-1, $"Child process failed: {e.Message}")
  let cleanupErrors = cleanup ()
  match outcome with
  | Ok(code, stdout, stderr, result) ->
    Ok(code, stdout, stderr, result, cleanupErrors)
  | Error(code, message) ->
    Error(code, String.concat "\n" (message :: cleanupErrors))

/// The name this process was started under: the first word of `/proc/self/cmdline`, read before
/// `setProcessTitle` replaces it. Linux only. Not `Environment.ProcessPath`, which is the file the
/// kernel ran, the same for `exec -a todo dark ...` as for a plain `dark`.
let invokedName () : Option<string> =
  if System.OperatingSystem.IsLinux() then
    try
      let cmdline = File.ReadAllText "/proc/self/cmdline"
      Some(Path.GetFileName(cmdline.Split(char 0)[0]))
    with _ ->
      None
  else
    None

/// `ps aux`, `top -c` and htop read the memory the kernel copied argv into, so the title is written
/// over it, cut to fit. Nothing past the arguments is touched.
let private setCommandLine (title : string) : unit =
  let stat = File.ReadAllText "/proc/self/stat"
  // Fields count from 3 after the `(comm)`, which can hold spaces; arg_start is field 48.
  let fields = stat.Substring(stat.LastIndexOf ')' + 2).Split ' '
  let argStart = nativeint (uint64 fields[48 - 3])
  let room = int (nativeint (uint64 fields[49 - 3]) - argStart)
  let bytes = System.Text.Encoding.UTF8.GetBytes title
  let buffer = Array.zeroCreate<byte> room
  Array.blit bytes 0 buffer 0 (min bytes.Length (room - 1))
  Marshal.Copy(buffer, 0, argStart, room)

/// What `ps`, `top` and htop show for this process: <param full> as its command line and
/// <param short> as its name (`comm`: `top` without `-c`, `ps -e`), which the kernel cuts at 15
/// bytes. Linux only; on macOS the process keeps its command line. Trusted CLI presentation only,
/// never a guest process primitive.
let setProcessTitle (short : string) (full : string) : unit =
  if System.OperatingSystem.IsLinux() then
    try
      let bytes = System.Text.Encoding.UTF8.GetBytes short
      File.WriteAllText(
        "/proc/self/comm",
        System.Text.Encoding.UTF8.GetString(bytes, 0, min bytes.Length 15)
      )
      setCommandLine full
    with _ ->
      ()
