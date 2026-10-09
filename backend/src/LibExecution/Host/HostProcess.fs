/// Host-owned child processes.
///
/// The process table lives here, behind the checked boundary; guest code holds
/// only opaque integer handles and reaches the table through
/// `Host.perform`. Moved from the CLI builtins so the door owns the resource,
/// not the code asking to use it.
module LibExecution.HostProcess

open System.Collections.Concurrent
open System.IO
open System.Runtime.InteropServices

open Prelude

// F# generates failwith inside P/Invoke stubs; Prelude bans the built-in one.
let private failwith (message : string) : 'a = raise (System.Exception message)

/// Windows keeps descendants in a job even after the worker exits. The parent
/// owns the job; the launcher joins it before it can start any test code.
module private WindowsJob =
  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type BasicLimits =
    { perProcessTime : int64
      perJobTime : int64
      flags : uint32
      minWorkingSet : unativeint
      maxWorkingSet : unativeint
      activeProcesses : uint32
      affinity : unativeint
      priority : uint32
      scheduling : uint32 }

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type IoCounters =
    { readOperations : uint64
      writeOperations : uint64
      otherOperations : uint64
      readBytes : uint64
      writeBytes : uint64
      otherBytes : uint64 }

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type ExtendedLimits =
    { basic : BasicLimits
      io : IoCounters
      processMemory : unativeint
      jobMemory : unativeint
      peakProcessMemory : unativeint
      peakJobMemory : unativeint }

  [<DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
  extern nativeint private CreateJobObjectW(nativeint attributes, string name)

  [<DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
  extern nativeint private OpenJobObjectW(
    uint32 access,
    bool inheritHandle,
    string name
  )

  [<DllImport("kernel32.dll", SetLastError = true)>]
  extern bool private SetInformationJobObject(
    nativeint job,
    int infoClass,
    ExtendedLimits& limits,
    uint32 length
  )

  [<DllImport("kernel32.dll", SetLastError = true)>]
  extern bool private AssignProcessToJobObject(nativeint job, nativeint processHandle)

  [<DllImport("kernel32.dll", SetLastError = true)>]
  extern bool private TerminateJobObject(nativeint job, uint32 exitCode)

  let private check success =
    if not success then
      raise (System.ComponentModel.Win32Exception(Marshal.GetLastPInvokeError()))

  let private handle raw =
    check (raw <> 0n)
    new Microsoft.Win32.SafeHandles.SafeFileHandle(raw, true)

  let create name =
    let job = handle (CreateJobObjectW(0n, name))
    try
      let mutable limits = Unchecked.defaultof<ExtendedLimits>
      limits <- { limits with basic = { limits.basic with flags = 0x2000u } } // KILL_ON_JOB_CLOSE
      check (
        SetInformationJobObject(
          job.DangerousGetHandle(),
          9,
          &limits,
          uint32 (Marshal.SizeOf<ExtendedLimits>())
        )
      )
      job
    with e ->
      job.Dispose()
      Exception.reraise e

  let join name =
    use job = handle (OpenJobObjectW(1u, false, name)) // JOB_OBJECT_ASSIGN_PROCESS
    use worker = System.Diagnostics.Process.GetCurrentProcess()
    check (AssignProcessToJobObject(job.DangerousGetHandle(), worker.Handle))

  let terminate (job : Microsoft.Win32.SafeHandles.SafeFileHandle) =
    check (TerminateJobObject(job.DangerousGetHandle(), 1u))

/// Private CLI entry point. Establish ownership before starting the worker,
/// including when a test substitutes a wrapper executable.
let launchIsolatedWorker
  (jobName : string)
  (program : string)
  (args : List<string>)
  : int =
  if HostLibc.isPosix then
    match HostLibc.execInNewSession program args with
    | Error(_, message) ->
      System.Console.Error.WriteLine
        $"Could not launch isolated test worker: {message}"
      127
    | Ok() -> 0 // exec does not return on success
  elif System.OperatingSystem.IsWindows() then
    WindowsJob.join jobName
    let psi = System.Diagnostics.ProcessStartInfo(program)
    psi.UseShellExecute <- false
    for arg in args do
      psi.ArgumentList.Add arg
    use worker = System.Diagnostics.Process.Start psi
    worker.WaitForExit()
    worker.ExitCode
  else
    invalidOp "Isolated test processes are unsupported on this platform"

type private ProcessInfo =
  { Process : System.Diagnostics.Process
    StandardInput : StreamWriter
    StandardOutput : StreamReader
    StandardError : StreamReader
    mutable OutputBuffer : string
    mutable ErrorBuffer : string }

let private processHandles = ConcurrentDictionary<int64, ProcessInfo>()
let mutable private nextProcessId = 1L

let private getNextProcessId () =
  System.Threading.Interlocked.Increment(&nextProcessId)

let private disposeProcessInfo (processInfo : ProcessInfo) : unit =
  try
    processInfo.StandardInput.Dispose()
  with _ ->
    ()
  try
    processInfo.StandardOutput.Dispose()
  with _ ->
    ()
  try
    processInfo.StandardError.Dispose()
  with _ ->
    ()
  try
    processInfo.Process.Dispose()
  with _ ->
    ()

/// Gracefully terminates a process with cross-platform support
let private terminateProcess (proc : System.Diagnostics.Process) : unit =
  if not proc.HasExited then
    try
      if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
        // On Windows, try CloseMainWindow first
        proc.CloseMainWindow() |> ignore<bool>
        if not (proc.WaitForExit 2000) then
          proc.Kill()
          proc.WaitForExit()
      else
        // On Unix systems, send SIGTERM first, then SIGKILL
        proc.Kill() // .NET Kill() sends SIGTERM on Unix
        if not (proc.WaitForExit 3000) then
          // Force kill with SIGKILL if SIGTERM didn't work
          proc.Kill true // true = entireProcessTree
          proc.WaitForExit()
    with _ ->
      // Force kill if graceful termination fails
      try
        proc.Kill()
      with _ ->
        ()

let killAllSpawnedProcesses () =
  let processIds = processHandles.Keys |> Seq.toList
  for processId in processIds do
    match processHandles.TryGetValue processId with
    | true, processInfo ->
      try
        terminateProcess processInfo.Process
        processHandles.TryRemove(processId) |> ignore<bool * ProcessInfo>
        disposeProcessInfo processInfo
      with _ ->
        // Even if process is dead, remove from our tracking
        processHandles.TryRemove processId |> ignore<bool * ProcessInfo>
        disposeProcessInfo processInfo
    | _ -> ()

// Register cleanup handler to kill all processes when application exits
// This prevents orphaned processes if the exe crashes or exits unexpectedly
let mutable private cleanupRegistered = false

let private registerCleanupHandler () =
  if not cleanupRegistered then
    cleanupRegistered <- true
    System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
      killAllSpawnedProcesses ())
    System.Console.CancelKeyPress.Add(fun _ -> killAllSpawnedProcesses ())

/// The path of the running host executable. Introspection of the host's own
/// process, not guest authority; callers stay Native-gated.
let currentExecutablePath () : string =
  System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName

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

let private readCapped reader =
  readCappedWithCancellation System.Threading.CancellationToken.None reader

/// Run a resolved executable to completion: (exitCode, stdout, stderr). Both
/// streams are drained concurrently to avoid a full-pipe deadlock. With a
/// timeout, a child still running when it elapses is killed and the call
/// fails with ETIMEDOUT. Uses .NET Process.Start (posix_spawn underneath)
/// because raw fork() is unsafe in a managed runtime.
let run
  (program : string)
  (args : List<string>)
  (timeoutMs : Option<int>)
  : Result<int * string * string, int * string> =
  use p = System.Diagnostics.Process.Start(startInfo program args false)
  let stdoutTask = readCapped p.StandardOutput
  let stderrTask = readCapped p.StandardError
  let finished =
    match timeoutMs with
    | None ->
      p.WaitForExit()
      true
    | Some timeoutMs -> p.WaitForExit timeoutMs
  if finished then
    Ok(p.ExitCode, stdoutTask.Result, stderrTask.Result)
  else
    p.Kill()
    p.WaitForExit()
    Error(110, "Process timed out") // ETIMEDOUT

/// One immutable baseline per run, created only when a test actually executes.
/// File.Copy already uses filesystem cloning where supported, with a portable
/// copy fallback. The store owner must finish and close its backup first.
let testStoreSnapshot
  (snapshot : string -> Result<unit, string>)
  : HostTypes.TestStoreSnapshot =
  let gate = obj ()
  let mutable disposed = false
  let mutable directory : Option<string> = None
  let baseline =
    lazy
      (let dir = Directory.CreateTempSubdirectory("dark-test-run-").FullName
       directory <- Some dir
       let path = Path.Combine(dir, "data.db")
       snapshot path |> Result.map (fun () -> path))
  let close () =
    lock gate (fun () ->
      disposed <- true
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
  { new HostTypes.TestStoreSnapshot with
      member _.CopyTo target =
        lock gate (fun () ->
          if disposed then
            Error "The package test run has already ended"
          else
            try
              match baseline.Value with
              | Error message -> Error message
              | Ok path ->
                File.Copy(path, target)
                Ok()
            with e ->
              Error e.Message)
      member _.Dispose() =
        try
          close ()
        finally
          System.AppDomain.CurrentDomain.ProcessExit.RemoveHandler onExit }


/// Run one typed test callback in a disposable copy of the current store.
/// Request/result bytes use the runtime codec; stdout is never the protocol.
/// The deadline covers worker execution and output, after snapshot preparation.
let runIsolatedTest
  (launcher : string)
  (snapshot : string -> Result<unit, string>)
  (branch : System.Guid)
  (request : byte[])
  (policyStore : byte[])
  (access : byte[])
  (timeoutMs : int)
  (columns : int)
  (rows : int)
  : Result<int * string * string * Option<byte[]> * List<string>, int * string> =
  let dir = Directory.CreateTempSubdirectory("dark-test-").FullName
  let mutable child : System.Diagnostics.Process option = None
  let jobName = "dark-test-" + System.Guid.NewGuid().ToString("N")
  let mutable job : Microsoft.Win32.SafeHandles.SafeFileHandle option = None
  use cancellation = new System.Threading.CancellationTokenSource()
  let stop () =
    let errors = ResizeArray<string>()
    let stopGroup (p : System.Diagnostics.Process) =
      if HostLibc.isPosix then
        // The launcher creates a group whose ID is its PID; that group remains
        // addressable after the worker exits and its children are reparented.
        match HostLibc.kill (-p.Id) 9 with
        | Ok()
        | Error(3, _) -> () // ESRCH: no group, including a launcher not yet ready
        | Error(_, message) ->
          errors.Add $"Could not stop isolated test process group: {message}"
    match child with
    | Some p ->
      try
        // Traverse while the worker is alive: nested workers may own separate
        // groups. Also handles cancellation before the launcher is ready.
        if not p.HasExited then p.Kill true
        if not (p.WaitForExit 5000) then
          errors.Add "Isolated test worker did not exit after being killed"
      with e ->
        errors.Add $"Could not stop isolated test worker: {e.Message}"
      try
        // The group survives its leader and catches reparented descendants.
        stopGroup p
      with e ->
        errors.Add $"Could not stop isolated test process group: {e.Message}"
    | None -> ()
    match job with
    | Some handle ->
      try
        WindowsJob.terminate handle
      with e ->
        errors.Add $"Could not stop isolated test job: {e.Message}"
    | None -> ()
    List.ofSeq errors
  let removeDirectory () =
    try
      Directory.Delete(dir, true)
      []
    with e ->
      [ $"Could not remove isolated test directory {dir}: {e.Message}" ]
  let onCancel =
    System.ConsoleCancelEventHandler(fun _ _ -> stop () |> ignore<List<string>>)
  let onExit =
    System.EventHandler(fun _ _ ->
      stop () |> ignore<List<string>>
      removeDirectory () |> ignore<List<string>>)
  System.Console.CancelKeyPress.AddHandler onCancel
  System.AppDomain.CurrentDomain.ProcessExit.AddHandler onExit
  let mutable cleanupErrors = []
  let outcome =
    try
      try
        match snapshot (Path.Combine(dir, "data.db")) with
        | Error message -> Error(-1, message)
        | Ok() ->
          let inputPath = Path.Combine(dir, "request.bin")
          let resultPath = Path.Combine(dir, "result.bin")
          File.WriteAllBytes(inputPath, request)
          let policyDir = Directory.CreateDirectory(Path.Combine(dir, "policy"))
          File.WriteAllBytes(
            Path.Combine(policyDir.FullName, "policies.bin"),
            policyStore
          )
          File.WriteAllBytes(Path.Combine(policyDir.FullName, "access.bin"), access)
          let executable =
            match
              System.Environment.GetEnvironmentVariable "DARK_CLI_UNDER_TEST"
            with
            | null
            | "" -> launcher
            | path -> Path.GetFullPath path
          if System.OperatingSystem.IsWindows() then
            job <- Some(WindowsJob.create jobName)
          let psi =
            startInfo
              launcher
              [ "--test-process-launch"
                jobName
                executable
                "--branch"
                string branch
                "--test-worker"
                inputPath
                resultPath ]
              true
          // Keep platform/runtime variables but replace instance state.
          for key in [ "DARK_MATTER_WRITE_SECRET"; "DARK_BRANCH" ] do
            psi.Environment.Remove key |> ignore<bool>
          for key, value in
            [ "DARK_CONFIG_RUNDIR", dir + string Path.DirectorySeparatorChar
              "DARK_CONFIG_DB_NAME", "data.db"
              "HOME", dir
              "XDG_CONFIG_HOME", Path.Combine(dir, "config")
              "TMPDIR", Path.Combine(dir, "tmp")
              "DARK_CLI_UNDER_TEST", executable
              "COLUMNS", string columns
              "LINES", string rows ] do
            psi.Environment[key] <- value
          Directory.CreateDirectory(psi.Environment["TMPDIR"])
          |> ignore<DirectoryInfo>
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
          if not (finished.Wait timeoutMs) then
            Error(
              110,
              $"Isolated test timed out after {timeoutMs} ms (worker or output still open)"
            )
          else
            let result =
              if File.Exists resultPath then
                if (FileInfo resultPath).Length > int64 maxCapturedOutput then
                  Exception.raiseInternal
                    "Isolated test result exceeds the size limit"
                    []
                Some(File.ReadAllBytes resultPath)
              else
                None
            Ok(p.ExitCode, stdout.Result, stderr.Result, result)
      with e ->
        Error(-1, $"Isolated test failed: {e.Message}")
    finally
      cancellation.Cancel()
      cleanupErrors <- stop ()
      match child with
      | Some p ->
        try
          p.Dispose()
        with e ->
          cleanupErrors <-
            cleanupErrors
            @ [ $"Could not dispose isolated test worker: {e.Message}" ]
      | None -> ()
      child <- None
      job |> Option.iter (fun handle -> handle.Dispose())
      job <- None
      System.Console.CancelKeyPress.RemoveHandler onCancel
      System.AppDomain.CurrentDomain.ProcessExit.RemoveHandler onExit
      cleanupErrors <- cleanupErrors @ removeDirectory ()
  match outcome with
  | Ok(code, stdout, stderr, result) ->
    Ok(code, stdout, stderr, result, cleanupErrors)
  | Error(code, message) ->
    Error(code, String.concat "\n" (message :: cleanupErrors))

/// Run a resolved executable on this terminal, inheriting stdin, stdout and
/// stderr, and return its exit code. `run` captures the streams, which is right
/// for a tool whose output you want and useless for one that draws: an editor
/// with a redirected stdout paints into a pipe and reads keys from nowhere.
let runInteractive (program : string) (args : List<string>) : int =
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- program
  for arg in args do
    psi.ArgumentList.Add arg
  psi.UseShellExecute <- false
  psi.RedirectStandardInput <- false
  psi.RedirectStandardOutput <- false
  psi.RedirectStandardError <- false
  use p = System.Diagnostics.Process.Start psi
  p.WaitForExit()
  p.ExitCode

/// Start an interactive process and register it; returns its opaque handle.
let spawn (program : string) (args : List<string>) : int64 =
  let p = System.Diagnostics.Process.Start(startInfo program args true)
  let processId = getNextProcessId ()

  // Register cleanup handler for the first process spawned
  registerCleanupHandler ()

  let processInfo =
    { Process = p
      StandardInput = p.StandardInput
      StandardOutput = p.StandardOutput
      StandardError = p.StandardError
      OutputBuffer = ""
      ErrorBuffer = "" }

  processHandles.TryAdd(processId, processInfo) |> ignore<bool>
  processId

/// Send input to a process and read output. Reads after input wait briefly for
/// a response; errors come back as an outcome triple.
let io (processId : int64) (input : string) : int * string * string =
  match processHandles.TryGetValue processId with
  | true, processInfo when not processInfo.Process.HasExited ->
    try
      // Send input if provided
      if input <> "" then
        processInfo.StandardInput.WriteLine(input)
        processInfo.StandardInput.Flush()

      // Wait for output (blocking read until we get some response)
      let stdout = System.Text.StringBuilder()
      let stderr = System.Text.StringBuilder()

      if input <> "" then
        // When we send input, we expect output - wait for the process to respond
        // Use a more robust approach: wait for complete lines of output
        try
          let mutable attempts = 0
          let mutable gotCompleteResponse = false

          while not gotCompleteResponse && attempts < 100 do // Max 10 seconds wait
            System.Threading.Thread.Sleep(100)
            attempts <- attempts + 1

            // Read all available stdout
            let mutable continueReading = true
            while continueReading && not processInfo.StandardOutput.EndOfStream do
              let peek = processInfo.StandardOutput.Peek()
              if peek >= 0 then
                let char = processInfo.StandardOutput.Read() |> char
                stdout.Append(char) |> ignore<System.Text.StringBuilder>
                // If we got a newline, we might have a complete response
                if char = '\n' then gotCompleteResponse <- true
              else
                continueReading <- false

            // Read all available stderr
            while not processInfo.StandardError.EndOfStream do
              let peek = processInfo.StandardError.Peek()
              if peek >= 0 then
                let char = processInfo.StandardError.Read() |> char
                stderr.Append(char) |> ignore<System.Text.StringBuilder>
                if char = '\n' then gotCompleteResponse <- true
              else
                continueReading <- false

            // If we have substantial output, consider it complete
            if stdout.Length > 10 || stderr.Length > 0 then
              gotCompleteResponse <- true
        with _ ->
          () // If reading fails, just continue
      else
        // Just reading without sending input - do a quick non-blocking read
        let mutable continueReading = true
        while continueReading && not processInfo.StandardOutput.EndOfStream do
          let peek = processInfo.StandardOutput.Peek()
          if peek >= 0 then
            let char = processInfo.StandardOutput.Read() |> char
            stdout.Append(char) |> ignore<System.Text.StringBuilder>
          else
            continueReading <- false

        continueReading <- true
        while continueReading && not processInfo.StandardError.EndOfStream do
          let peek = processInfo.StandardError.Peek()
          if peek >= 0 then
            let char = processInfo.StandardError.Read() |> char
            stderr.Append(char) |> ignore<System.Text.StringBuilder>
          else
            continueReading <- false

      // Update buffers
      processInfo.OutputBuffer <- processInfo.OutputBuffer + stdout.ToString()
      processInfo.ErrorBuffer <- processInfo.ErrorBuffer + stderr.ToString()

      let exitCode =
        if processInfo.Process.HasExited then processInfo.Process.ExitCode else 0
      (exitCode, stdout.ToString(), stderr.ToString())
    with ex ->
      (-1, "", $"Process IO error: {ex.Message}")
  | true, processInfo ->
    processHandles.TryRemove processId |> ignore<bool * ProcessInfo>
    disposeProcessInfo processInfo
    (-1, "", "Process not found or has exited")
  | false, _ -> (-1, "", "Process not found")

/// Terminate a spawned process and return its final outcome. Never throws.
let terminate (processId : int64) : int * string * string =
  match processHandles.TryGetValue processId with
  | true, processInfo ->
    try
      let exitCode =
        if not processInfo.Process.HasExited then
          terminateProcess processInfo.Process
          processInfo.Process.ExitCode
        else
          processInfo.Process.ExitCode

      // Read any remaining output
      let remainingStdout =
        try
          processInfo.StandardOutput.ReadToEnd()
        with _ ->
          ""
      let remainingStderr =
        try
          processInfo.StandardError.ReadToEnd()
        with _ ->
          ""

      let finalStdout = processInfo.OutputBuffer + remainingStdout
      let finalStderr = processInfo.ErrorBuffer + remainingStderr

      processHandles.TryRemove processId |> ignore<bool * ProcessInfo>
      disposeProcessInfo processInfo

      (exitCode, finalStdout, finalStderr)
    with ex ->
      processHandles.TryRemove processId |> ignore<bool * ProcessInfo>
      disposeProcessInfo processInfo
      (-1, "", $"Process termination error: {ex.Message}")
  | false, _ -> (-1, "", "Process not found")


// ───────── the process's own title ─────────

/// What a system monitor shows for this process: `dark` plus what it runs (`dark serve`, `dark
/// apps sync`, `dark eval`), so a person looking at the box can tell them apart; `dark ps` is
/// the other half of the "what is running" story.
///
/// Linux: the kernel's `comm` (what `top`, `pgrep -x` and the desktop monitors show), 15 bytes,
/// written to `/proc/self/comm`. The full command line (`ps aux`) is the original argv memory,
/// which managed code cannot rewrite without `CAP_SYS_RESOURCE`, so it stays as launched.
/// Elsewhere: nothing; there is no portable way and it is not worth a native call.
let setProcessTitle (title : string) : unit =
  if System.OperatingSystem.IsLinux() then
    try
      let comm =
        let bytes = System.Text.Encoding.UTF8.GetBytes title
        if bytes.Length <= 15 then
          title
        else
          System.Text.Encoding.UTF8.GetString(bytes, 0, 15)
      System.IO.File.WriteAllText("/proc/self/comm", comm)
    with _ ->
      ()
