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

let private readCapped
  (reader : System.IO.StreamReader)
  : System.Threading.Tasks.Task<string> =
  task {
    let builder = System.Text.StringBuilder()
    let buffer = Array.zeroCreate<char> 8192
    let mutable reading = true
    while reading do
      let! n = reader.ReadAsync(buffer, 0, buffer.Length)
      if n = 0 then
        reading <- false
      elif builder.Length < maxCapturedOutput then
        builder.Append(buffer, 0, min n (maxCapturedOutput - builder.Length))
        |> ignore<System.Text.StringBuilder>
    return builder.ToString()
  }

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
