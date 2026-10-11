/// Host-owned child processes.
///
/// The process table lives here, behind the checked boundary; guest code holds
/// only opaque integer handles and reaches the table through
/// `Host.perform`. Moved from the CLI builtins so the door owns the resource,
/// not the code asking to use it.
module LibExecution.HostProcess

open Prelude

/// One managed launch and its supervised child. No capturing or waiting here:
/// shared Dark consumes nonblocking owned pipe descriptors using syscall leaves.
type Child =
  { proc : System.Diagnostics.Process
    program : string
    args : List<string>
    descriptors : List<int>
    waitGate : obj
    mutable waiters : int
    mutable closing : bool }

let disposeChild (child : Child) : unit =
  lock child.waitGate (fun () ->
    if not child.closing then
      child.closing <- true
      try if not child.proc.HasExited then child.proc.Kill() with _ -> ()
      // An already-authorized blocking wait must remain usable while cleanup
      // kills the child. Dispose the supervisor after its last waiter returns.
      if child.waiters = 0 then child.proc.Dispose())

let startChild (program : string) (args : List<string>) (inheritTerminal : bool) : Child =
  if System.OperatingSystem.IsWindows() || System.OperatingSystem.IsBrowser() then
    raise (System.PlatformNotSupportedException("owned syscall child pipes require Unix"))
  let psi = System.Diagnostics.ProcessStartInfo(program)
  psi.UseShellExecute <- false
  for arg in args do psi.ArgumentList.Add arg
  psi.RedirectStandardInput <- not inheritTerminal
  psi.RedirectStandardOutput <- not inheritTerminal
  psi.RedirectStandardError <- not inheritTerminal
  let proc = System.Diagnostics.Process.Start psi
  let duplicated = System.Collections.Generic.List<int>()
  let duplicate (stream : System.IO.Stream) =
    let pipe = stream :?> System.IO.Pipes.PipeStream
    let handle = pipe.SafePipeHandle
    let mutable held = false
    try
      handle.DangerousAddRef(&held)
      let fd = int (handle.DangerousGetHandle())
      let command = if System.OperatingSystem.IsMacOS() then 67 else 1030
      let copy = HostKernel.fcntl_raw(fd, command, 0)
      if copy < 0 then raise (System.ComponentModel.Win32Exception(-copy))
      duplicated.Add copy
      let flags = HostKernel.fcntl_raw(copy, 3, 0)
      if flags < 0 then raise (System.ComponentModel.Win32Exception(-flags))
      let nonblocking = if System.OperatingSystem.IsMacOS() then 4 else 2048
      let rc = HostKernel.fcntl_raw(copy, 4, flags ||| nonblocking)
      if rc < 0 then raise (System.ComponentModel.Win32Exception(-rc))
      copy
    finally
      if held then handle.DangerousRelease()
  try
    let descriptors =
      if inheritTerminal then []
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
    { proc = proc; program = program; args = args; descriptors = descriptors
      waitGate = obj (); waiters = 0; closing = false }
  with _ ->
    for fd in duplicated do HostKernel.close_raw fd |> ignore<int>
    try if not proc.HasExited then proc.Kill() with _ -> ()
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

/// Trusted CLI presentation only, never a guest process primitive.
let setProcessTitle (title : string) : unit =
  if System.OperatingSystem.IsLinux() then
    try
      let comm =
        let bytes = System.Text.Encoding.UTF8.GetBytes title
        if bytes.Length <= 15 then title
        else System.Text.Encoding.UTF8.GetString(bytes, 0, 15)
      System.IO.File.WriteAllText("/proc/self/comm", comm)
    with _ -> ()
