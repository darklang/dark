/// The checked host boundary: audit summaries, the server bind, temporary
/// paths, links met at operation time, and the protected policy directory.
module Tests.Host

open Expecto
open Prelude
open TestUtils.PTShortcuts

module Permission = LibExecution.Permissions
module LocalFile = LibDB.LocalFile
module HT = LibExecution.HostTypes
module HostHttp = LibExecution.HostHttp

let private only (item : 'a) : Permission.Scope<'a> = Permission.Scope.Only item

let private setTimesNow path =
  if System.OperatingSystem.IsMacOS() then
    let seconds = System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()
    LibExecution.HostPosix.setattrlistTimes path seconds 0L seconds 0L
  else
    LibExecution.HostPosix.utimensAt path 0L 1073741823L 0L 1073741823L

let private request (method : string) (url : string) : Permission.Request =
  match Permission.Request.http method url with
  | Ok request -> request
  | Error error -> Exception.raiseInternal error []

let auditRecordsOperationsAndDecisions =
  test "the audit summary names the operation, redacting payloads and queries" {
    Expect.equal
      (HT.describeOperation (HT.Operation.FileWrite("/tmp/x", [| 1uy |])))
      "file-write /tmp/x"
      "file-write is summarized without its payload"
    Expect.equal
      (HT.describeOperation (HT.Operation.Posix(HT.PosixOp.Kill(42, 9))))
      "posix kill 42 9"
      "posix ops are summarized"
    Expect.equal
      (HT.describeOperation (
        HT.Operation.HttpRequest(
          HT.HttpProfile.Guest,
          "GET",
          "https://x/y?token=s",
          [],
          [||]
        )
      ))
      "http GET https://x/y?<redacted>"
      "query strings are redacted from the audit line"
    let httpOp =
      HT.Operation.HttpRequest(
        HT.HttpProfile.Guest,
        "GET",
        "https://x/y?token=s",
        [],
        [||]
      )
    Expect.equal
      (HT.redactAuditDetail httpOp "GET https://x/y?token=s")
      "GET https://x/y?<redacted>"
      "denial details are redacted too"
  }

let httpServerBindUsesTheSuppliedAccess =
  testTask "HTTP server bind is denied before touching the socket" {
    let access = Permission.Access.start Permission.Policy.denyAll
    let! outcome =
      LibExecution.Host.perform
        Permission.NoRelax
        access
        (HT.Operation.HttpServerBind 0)
    match outcome with
    | HT.Outcome.Denied(Permission.Layer.Instance, _, _, _) -> ()
    | other -> failtest $"expected an instance denial, got {other}"
  }

let rawDescriptorsRequireNativeAtHostBoundary =
  testTask "raw descriptors require Native before any kernel call" {
    let operations =
      [ HT.PosixOp.FdRead(HT.FileDescriptor.Raw -1, 1)
        HT.PosixOp.FdWrite(HT.FileDescriptor.Raw -1, [| 1uy |])
        HT.PosixOp.FdClose(HT.FileDescriptor.Raw -1)
        HT.PosixOp.Fstat(HT.FileDescriptor.Raw -1)
        HT.PosixOp.Ppoll([ HT.FileDescriptor.Raw -1, 1 ], 0)
        HT.PosixOp.KeventVnode(HT.FileDescriptor.Raw -1, [], 1, 0)
        HT.PosixOp.CopyFileRange(
          HT.FileDescriptor.Raw -1,
          HT.FileDescriptor.Raw -1,
          1
        ) ]
    for operation in operations do
      let! outcome =
        LibExecution.Host.perform
          Permission.NoRelax
          (Permission.Access.start Permission.Policy.denyAll)
          (HT.Operation.Posix operation)
      match outcome with
      | HT.Outcome.Denied(Permission.Layer.Instance, _, _, _) -> ()
      | other -> failtest $"expected denial before EBADF, got {other}"
    let! allowed =
      LibExecution.Host.perform
        Permission.NoRelax
        (Permission.Access.start Permission.Policy.allowAll)
        (HT.Operation.Posix(HT.PosixOp.FdRead(HT.FileDescriptor.Raw -1, 1)))
    match allowed with
    | HT.Outcome.Failed failure ->
      Expect.equal failure.errno 9 "authorized invalid fd reaches kernel"
    | other -> failtest $"expected EBADF after authorization, got {other}"
  }

let scopedDescriptorsRecheckAccessAndOwnership =
  testTask "scoped descriptors enforce mode, current access, ownership and close" {
    let path =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-fd-{System.Guid.NewGuid()}"
      )
    System.IO.File.WriteAllText(path, "abc")
    let owner = LibExecution.Host.createDescriptorSession ()
    let foreign = LibExecution.Host.createDescriptorSession ()
    let readOnly =
      Permission.Policy.create
        [ Permission.Rule.File(Permission.AccessKind.Read, only path) ]
        []
      |> Permission.Access.start
    let denied = Permission.Access.start Permission.Policy.denyAll
    let all = Permission.Access.start Permission.Policy.allowAll
    let perform session access op =
      LibExecution.Host.performInSession
        session
        Permission.NoRelax
        access
        (HT.Operation.Posix op)
    let ebadf outcome =
      match outcome with
      | HT.Outcome.Failed failure ->
        Expect.equal failure.errno 9 "invalid capability"
      | other -> failtest $"expected EBADF, got {other}"
    try
      let! opened = perform owner readOnly (HT.PosixOp.Open(path, 0, 0))
      let token =
        match opened with
        | HT.Outcome.Success(HT.Response.Descriptor token) -> token
        | other -> failtestf "expected scoped open without Native, got %A" other
      let fd = HT.FileDescriptor.Scoped token
      let! unownedVnode =
        perform
          owner
          all
          (HT.PosixOp.KeventVnode(fd, [ HT.FileDescriptor.Raw -1, 37, 127 ], 0, 0))
      ebadf unownedVnode
      let! read = perform owner readOnly (HT.PosixOp.FdRead(fd, 3))
      match read with
      | HT.Outcome.Success(HT.Response.Bytes bytes) ->
        Expect.equal bytes [| 97uy; 98uy; 99uy |] "read bytes"
      | other -> failtest $"expected scoped read, got {other}"
      let! stat = perform owner readOnly (HT.PosixOp.Fstat fd)
      match stat with
      | HT.Outcome.Success(HT.Response.Bytes bytes) ->
        Expect.isNonEmpty bytes "raw stat record"
      | other -> failtest $"expected scoped fstat, got {other}"
      let! write = perform owner all (HT.PosixOp.FdWrite(fd, [| 0uy |]))
      ebadf write
      let! stolen = perform foreign all (HT.PosixOp.FdRead(fd, 1))
      ebadf stolen
      let! revoked = perform owner denied (HT.PosixOp.FdRead(fd, 1))
      match revoked with
      | HT.Outcome.Denied _ -> ()
      | other -> failtest $"expected current-access denial, got {other}"
      let! closed = perform owner denied (HT.PosixOp.FdClose fd)
      Expect.equal
        closed
        (HT.Outcome.Success HT.Response.Unit)
        "cleanup needs no new grant"
      let! again = perform owner all (HT.PosixOp.FdClose fd)
      ebadf again
      let! stale = perform owner all (HT.PosixOp.Fstat fd)
      ebadf stale
    finally
      owner.Dispose()
      foreign.Dispose()
      System.IO.File.Delete path
  }

let descriptorLeasesDoNotSerializeUnrelatedFiles =
  testTask "a held descriptor lease does not block another owned descriptor" {
    let entered = new System.Threading.ManualResetEventSlim(false)
    let release = new System.Threading.ManualResetEventSlim(false)
    let owner = new LibExecution.HostDescriptors.Session(fun _ -> Ok())
    let entry fd : LibExecution.HostDescriptors.Entry =
      { fd = fd
        paths = [ "/tmp/lease-test" ]
        processes = []
        reads = true
        writes = false }
    let first = owner.Register(entry 100)
    let second = owner.Register(entry 101)
    let held =
      System.Threading.Tasks.Task.Run(fun () ->
        owner.WithLeases(
          [ first ],
          fun () ->
            entered.Set()
            release.Wait()
        ))
    try
      Expect.isTrue (entered.Wait 2000) "first lease acquired"
      let independent =
        System.Threading.Tasks.Task.Run(fun () ->
          owner.WithLeases([ second ], (fun () -> owner.Find second)))
      let! finished =
        System.Threading.Tasks.Task.WhenAny(
          independent,
          System.Threading.Tasks.Task.Delay 2000
        )
      Expect.isTrue
        (obj.ReferenceEquals(finished, independent))
        "second lease must not wait for first"
      let! result = independent
      Expect.isSome result "second capability stays valid"
    finally
      release.Set()
      held.GetAwaiter().GetResult()
      owner.Dispose()
      entered.Dispose()
      release.Dispose()
  }

let ownedChildrenRetainScopedProcessAuthority =
  testTask "owned children and pipes recheck process grants and execution ownership" {
    if System.OperatingSystem.IsLinux() || System.OperatingSystem.IsMacOS() then
      let owner = LibExecution.Host.createDescriptorSession ()
      let foreign = LibExecution.Host.createDescriptorSession ()
      let program = "/bin/sh"
      let args = [ "-c"; "read line; printf '%s' \"$line\"" ]
      let access =
        Permission.Policy.create
          [ Permission.Rule.Process { executable = only program; args = only args } ]
          []
        |> Permission.Access.start
      let denied = Permission.Access.start Permission.Policy.denyAll
      let perform session access op =
        LibExecution.Host.performInSession session Permission.NoRelax access op
      let ebadf outcome =
        match outcome with
        | HT.Outcome.Failed error ->
          Expect.equal error.errno 9 "foreign or closed token"
        | other -> failtestf "expected EBADF, got %A" other
      let revoked outcome =
        match outcome with
        | HT.Outcome.Denied _ -> ()
        | other ->
          failtestf "expected current process permission denial, got %A" other
      try
        let! started =
          perform owner access (HT.Operation.ChildStart(program, args, false))
        let child =
          match started with
          | HT.Outcome.Success(HT.Response.ChildHandle token) -> token
          | other ->
            failtestf
              "expected owned launch without Native/File permissions, got %A"
              other
        let! ports = perform owner access (HT.Operation.ChildDescriptors child)
        let stdin, stdout =
          match ports with
          | HT.Outcome.Success(HT.Response.ChildDescriptors [ stdin; stdout; _ ]) ->
            stdin, stdout
          | other -> failtestf "expected owned pipe descriptors, got %A" other
        let! stolen =
          perform
            foreign
            (Permission.Access.start Permission.Policy.allowAll)
            (HT.Operation.ChildPoll child)
        ebadf stolen
        let! deniedPoll = perform owner denied (HT.Operation.ChildPoll child)
        revoked deniedPoll
        let! deniedRead =
          perform
            owner
            denied
            (HT.Operation.Posix(
              HT.PosixOp.FdRead(HT.FileDescriptor.Scoped stdout, 32)
            ))
        revoked deniedRead
        let! written =
          perform
            owner
            access
            (HT.Operation.Posix(
              HT.PosixOp.FdWrite(
                HT.FileDescriptor.Scoped stdin,
                [| 104uy; 105uy; 10uy |]
              )
            ))
        Expect.equal
          written
          (HT.Outcome.Success(HT.Response.Written 3))
          "owned pipe write needs only the retained process grant"
        do! System.Threading.Tasks.Task.Delay 100
        let! read =
          perform
            owner
            access
            (HT.Operation.Posix(
              HT.PosixOp.FdRead(HT.FileDescriptor.Scoped stdout, 32)
            ))
        Expect.equal
          read
          (HT.Outcome.Success(HT.Response.Bytes [| 104uy; 105uy |]))
          "raw bytes, no host capture algorithm"
        let! closedPort =
          perform owner denied (HT.Operation.ChildClosePort(child, 0))
        Expect.equal
          closedPort
          (HT.Outcome.Success HT.Response.Unit)
          "pipe cleanup needs no revoked grant"
        let! closed = perform owner denied (HT.Operation.ChildClose child)
        Expect.equal
          closed
          (HT.Outcome.Success HT.Response.Unit)
          "owned child cleanup needs no revoked grant"
        let! stale = perform owner access (HT.Operation.ChildPoll child)
        ebadf stale
        let! staleFd =
          perform
            owner
            access
            (HT.Operation.Posix(
              HT.PosixOp.FdRead(HT.FileDescriptor.Scoped stdout, 32)
            ))
        ebadf staleFd
      finally
        owner.Dispose()
        foreign.Dispose()
  }

let ownedChildWaitAllowsCleanup =
  testTask "blocking child wait releases the child lease so cleanup can terminate it" {
    if System.OperatingSystem.IsLinux() || System.OperatingSystem.IsMacOS() then
      let owner = LibExecution.Host.createDescriptorSession ()
      let all = Permission.Access.start Permission.Policy.allowAll
      let denied = Permission.Access.start Permission.Policy.denyAll
      let perform access op =
        LibExecution.Host.performInSession owner Permission.NoRelax access op
      try
        let! started =
          perform
            all
            (HT.Operation.ChildStart("/bin/sh", [ "-c"; "sleep 10" ], true))
        let token =
          match started with
          | HT.Outcome.Success(HT.Response.ChildHandle token) -> token
          | other -> failtestf "expected launch, got %A" other
        let child = owner.FindChild token |> Option.get
        let waiting = perform all (HT.Operation.ChildWait token)
        Expect.isTrue
          (System.Threading.SpinWait.SpinUntil(
            (fun () -> lock child.child.waitGate (fun () -> child.child.waiters > 0)),
            2000
          ))
          "wait retains the managed supervisor before releasing the ownership lease"
        let closing = perform denied (HT.Operation.ChildClose token)
        let! winner =
          System.Threading.Tasks.Task.WhenAny(
            closing,
            System.Threading.Tasks.Task.Delay 2000
          )
        Expect.isTrue
          (obj.ReferenceEquals(winner, closing))
          "cleanup must not block behind the child wait"
        let! closed = closing
        Expect.equal
          closed
          (HT.Outcome.Success HT.Response.Unit)
          "cleanup after revocation"
        let! finished =
          System.Threading.Tasks.Task.WhenAny(
            waiting,
            System.Threading.Tasks.Task.Delay 2000
          )
        Expect.isTrue
          (obj.ReferenceEquals(finished, waiting))
          "termination unblocks the retained waiter"
        let! outcome = waiting
        match outcome with
        | HT.Outcome.Success(HT.Response.ChildStatus(Some _)) -> ()
        | other -> failtestf "expected supervised completion, got %A" other
      finally
        owner.Dispose()
  }

let watchQueuesRetainScopedPathAuthority =
  testTask "watch queue registration and reads retain current path authority" {
    if System.OperatingSystem.IsLinux() then
      let path =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-watch-{System.Guid.NewGuid()}"
        )
      System.IO.File.WriteAllText(path, "before")
      let owner = LibExecution.Host.createDescriptorSession ()
      let foreign = LibExecution.Host.createDescriptorSession ()
      let access =
        Permission.Policy.create
          [ Permission.Rule.File(Permission.AccessKind.Read, only path)
            Permission.Rule.Effect LibExecution.Effects.Effect.Clock ]
          []
        |> Permission.Access.start
      let denied = Permission.Access.start Permission.Policy.denyAll
      let perform session access op =
        LibExecution.Host.performInSession
          session
          Permission.NoRelax
          access
          (HT.Operation.Posix op)
      try
        let! opened = perform owner denied (HT.PosixOp.InotifyInit1 526336)
        let fd =
          match opened with
          | HT.Outcome.Success(HT.Response.Descriptor token) ->
            HT.FileDescriptor.Scoped token
          | other -> failtestf "expected owned empty event queue, got %A" other
        let! rejected =
          perform owner denied (HT.PosixOp.InotifyAddWatch(fd, path, 2))
        match rejected with
        | HT.Outcome.Denied _ -> ()
        | other -> failtestf "expected path denial, got %A" other
        if not (System.IO.Directory.Exists "/proc/self/fd") then
          skiptest
            "held-inode event registration requires real /proc/self/fd; the adapter fails closed without it"
        let! added = perform owner access (HT.PosixOp.InotifyAddWatch(fd, path, 2))
        match added with
        | HT.Outcome.Success(HT.Response.Written wd) ->
          Expect.isGreaterThanOrEqual wd 0 "watch registered"
        | other -> failtestf "expected scoped watch without Native, got %A" other
        System.IO.File.WriteAllText(path, "after")
        let! ready = perform owner access (HT.PosixOp.Ppoll([ fd, 1 ], 0))
        match ready with
        | HT.Outcome.Success(HT.Response.Bytes record) ->
          Expect.equal record.Length 8 "raw pollfd"
          Expect.equal (int record[6] &&& 1) 1 "queued change is readable"
        | other -> failtestf "expected poll readiness, got %A" other
        let! revoked = perform owner denied (HT.PosixOp.FdRead(fd, 4096))
        match revoked with
        | HT.Outcome.Denied _ -> ()
        | other -> failtestf "queue read must recheck watched path, got %A" other
        let! stolen = perform foreign access (HT.PosixOp.FdRead(fd, 4096))
        match stolen with
        | HT.Outcome.Failed failure ->
          Expect.equal failure.errno 9 "foreign queue is not owned"
        | other -> failtestf "expected foreign capability rejection, got %A" other
        let! changes = perform owner access (HT.PosixOp.FdRead(fd, 4096))
        match changes with
        | HT.Outcome.Success(HT.Response.Bytes bytes) ->
          Expect.isGreaterThanOrEqual bytes.Length 16 "raw inotify event"
        | other -> failtestf "expected raw change records, got %A" other
        let! closed = perform owner denied (HT.PosixOp.FdClose fd)
        Expect.equal
          closed
          (HT.Outcome.Success HT.Response.Unit)
          "revocation does not prevent cleanup"
      finally
        owner.Dispose()
        foreign.Dispose()
        System.IO.File.Delete path
  }

let systemFactsDoNotGrantSensitiveQueries =
  testTask "host facts do not grant arbitrary sysctl or foreign affinity access" {
    let access = Permission.Access.start Permission.Policy.denyAll
    for op in
      [ HT.PosixOp.SysctlBytes([ 1; 14 ], 256); HT.PosixOp.SchedGetaffinity(1, 128) ] do
      let! outcome =
        LibExecution.Host.perform Permission.NoRelax access (HT.Operation.Posix op)
      match outcome with
      | HT.Outcome.Denied _ -> ()
      | other -> failtest $"expected Native denial before kernel query, got {other}"
    let! local =
      LibExecution.Host.perform
        Permission.NoRelax
        access
        (HT.Operation.Posix(HT.PosixOp.SchedGetaffinity(0, 128)))
    match local with
    | HT.Outcome.Success(HT.Response.Bytes bytes) ->
      Expect.isNonEmpty bytes "local affinity mask"
    | HT.Outcome.Failed failure ->
      Expect.isTrue (failure.errno = 38 || failure.errno = 78) "unsupported target"
    | other -> failtest $"local host fact must not need Native, got {other}"
  }

let tempPrefixDoesNotAuthorizeARandomSibling =
  testTask "mkstemp authorizes its containing directory, not its prefix" {
    let prefix = $"/tmp/dark-permission-{System.Guid.NewGuid()}-"
    let policy =
      Permission.Policy.create
        [ Permission.Rule.File(Permission.AccessKind.Write, only prefix) ]
        []
    let! outcome =
      LibExecution.Host.perform
        Permission.NoRelax
        (Permission.Access.start policy)
        (HT.Operation.Posix(HT.PosixOp.Mkstemp prefix))
    match outcome with
    | HT.Outcome.Denied(Permission.Layer.Instance, _, resource, _) ->
      Expect.stringContains
        resource
        "/tmp"
        "the request is for the directory that will contain the random file"
    | other -> failtest $"expected the prefix-only rule to be denied, got {other}"
  }

let readlinkMayInspectTheFinalSymlink =
  testTask "readlink rejects linked ancestors but permits the final link" {
    let directory =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-readlink-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory directory |> ignore<System.IO.DirectoryInfo>
    let target = System.IO.Path.Combine(directory, "target")
    let link = System.IO.Path.Combine(directory, "link")
    try
      System.IO.File.WriteAllText(target, "target")
      System.IO.File.CreateSymbolicLink(link, target)
      |> ignore<System.IO.FileSystemInfo>
      let policy =
        Permission.Policy.create
          [ Permission.Rule.File(Permission.AccessKind.Read, only link) ]
          []
      let! outcome =
        LibExecution.Host.perform
          Permission.NoRelax
          (Permission.Access.start policy)
          (HT.Operation.Posix(HT.PosixOp.ReadlinkBytes(link, 4096)))
      match outcome with
      | HT.Outcome.Success(HT.Response.Bytes actual) ->
        Expect.equal
          actual
          (System.Text.Encoding.UTF8.GetBytes target)
          "readlink returns raw stored target bytes"
      | other -> failtest $"expected readlink success, got {other}"
    finally
      System.IO.Directory.Delete(directory, true)
  }

let posixWalkRefusesALinkMetAtOperationTime =
  testTask
    "the POSIX host meets a link at operation time and refuses it, without the pre-check" {
    // The boundary's lexical pre-check can be raced: a component swapped for
    // a link after the check. The bridge walks the directories again with
    // O_NOFOLLOW at operation time, so calling it directly through a link,
    // as a race would, must fail rather than follow.
    if LibExecution.HostPosix.isPosix then
      let directory =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-walk-{System.Guid.NewGuid()}"
        )
      let real = System.IO.Path.Combine(directory, "real")
      let link = System.IO.Path.Combine(directory, "link")
      let file = System.IO.Path.Combine(real, "f")
      let finalLink = System.IO.Path.Combine(real, "final-link")
      System.IO.Directory.CreateDirectory real |> ignore<System.IO.DirectoryInfo>
      try
        System.IO.File.WriteAllText(file, "data")
        System.IO.Directory.CreateSymbolicLink(link, real)
        |> ignore<System.IO.FileSystemInfo>
        System.IO.File.CreateSymbolicLink(finalLink, file)
        |> ignore<System.IO.FileSystemInfo>
        let viaLink = System.IO.Path.Combine(link, "f")
        Expect.isError
          (LibExecution.HostPosix.unlink viaLink)
          "unlink through a linked ancestor is refused by the walk"
        Expect.isTrue (System.IO.File.Exists file) "and the file is untouched"
        Expect.isError
          (LibExecution.HostPosix.stat viaLink)
          "stat through a linked ancestor is refused"
        Expect.isError
          (LibExecution.HostPosix.listDir link)
          "listing a linked directory entry is refused"
        Expect.throws
          (fun () -> LibExecution.HostPosix.readAllBytes viaLink |> ignore<byte[]>)
          "reading through a linked ancestor is refused"
        let originalMode = System.IO.File.GetUnixFileMode file
        let originalMtime = System.DateTime.UnixEpoch.AddDays 1.0
        System.IO.File.SetLastWriteTimeUtc(file, originalMtime)
        Expect.isError
          (LibExecution.HostPosix.chmod finalLink 0o777)
          "chmod refuses a linked final component"
        Expect.equal
          (System.IO.File.GetUnixFileMode file)
          originalMode
          "chmod did not reach the link target"
        Expect.isError
          (setTimesNow finalLink)
          "utimes refuses a linked final component"
        Expect.equal
          (System.IO.File.GetLastWriteTimeUtc file)
          originalMtime
          "utimes did not reach the link target"
        // The same operations on the real path work, through the same walk.
        System.IO.File.SetUnixFileMode(file, System.IO.UnixFileMode.None)
        Expect.isOk (setTimesNow file) "utimes works without file read permission"
        Expect.isOk
          (LibExecution.HostPosix.chmod file 0o600)
          "chmod can recover a mode-000 file"
        Expect.isOk (LibExecution.HostPosix.stat file) "stat on the real path"
        Expect.equal
          (LibExecution.HostPosix.listDir real |> Result.map Set.ofList)
          (Ok(Set.ofList [ "f"; "final-link" ]))
          "listing on the real path"
        match
          LibExecution.HostPosix.mkstemp (System.IO.Path.Combine(real, "tmp-"))
        with
        | Ok(fd, path) ->
          LibExecution.HostPosix.fdClose fd |> ignore<Result<unit, int * string>>
          Expect.isTrue (System.IO.File.Exists path) "mkstemp created the file"
          Expect.equal
            (System.IO.Path.GetDirectoryName path)
            real
            "mkstemp created it in the walked directory"
        | Error(_, message) -> failtest $"mkstemp failed: {message}"
        Expect.equal
          (LibExecution.HostPosix.statEntry (System.IO.Path.Combine(real, "missing")))
          (Ok(false, false))
          "a missing entry is reported, not an error"
        Expect.equal
          (LibExecution.HostPosix.statEntry real)
          (Ok(true, true))
          "a directory is reported as one"
        Expect.isOk (LibExecution.HostPosix.unlink file) "unlink on the real path"
        Expect.isFalse (System.IO.File.Exists file) "and it is gone"
      finally
        System.IO.Directory.Delete(directory, true)
  }

let localPolicyPathsRejectTraversal =
  test "local policy storage is isolated from guest filesystem paths" {
    let policyPath = LocalFile.path "policies.bin"
    Expect.isOk policyPath "the known policy filename"
    match policyPath with
    | Ok path ->
      Expect.isTrue
        (LibExecution.HostSecurity.isPolicyPath path)
        "the policy store is inside the protected host directory"
      Expect.isTrue
        (LibExecution.HostSecurity.isPolicyPath (
          System.IO.Path.Combine(path, "..", "other-policy-state")
        ))
        "normalization cannot traverse out and back into an unprotected spelling"
      let darklangDirectory =
        System.IO.Path.GetDirectoryName(System.IO.Path.GetDirectoryName path)
      Expect.isTrue
        (LibExecution.HostSecurity.canAffectPolicyPath darklangDirectory)
        "renaming the .darklang ancestor cannot expose and replace the policy"
    | Error _ -> ()
    Expect.isError (LocalFile.path "../policies.bin") "parent traversal"
    Expect.isError (LocalFile.path "nested/policies.bin") "nested path"
  }

/// Protect the store's ancestors from entry moves. An exact-path guard allowed
/// a writable parent to be renamed, modified elsewhere, and renamed back.
/// Sequencing plus `packageDbPathForTesting` isolates the process-global path.
let packageStoreAncestorsAreProtected =
  testSequenced
  <| testTask "renaming or removing an ancestor of the package store is refused" {
    let root =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-store-{System.Guid.NewGuid()}"
      )
    let rundir = System.IO.Path.Combine(root, "rundir")
    let other = System.IO.Path.Combine(root, "other")
    System.IO.Directory.CreateDirectory rundir |> ignore<System.IO.DirectoryInfo>
    System.IO.Directory.CreateDirectory other |> ignore<System.IO.DirectoryInfo>
    let db = System.IO.Path.Combine(rundir, "data.db")
    System.IO.File.WriteAllText(db, "placeholder")
    let restorePackageDbPath = LibExecution.HostSecurity.packageDbPathForTesting db
    // Everything under `root` is writable: the attack needs no more than that.
    let access =
      Permission.Access.start (
        Permission.Policy.create
          [ Permission.Rule.File(Permission.AccessKind.Write, only root) ]
          []
      )
    let perform op = LibExecution.Host.perform Permission.NoRelax access op
    let expectRefused (label : string) (outcome : HT.Outcome) =
      match outcome with
      | HT.Outcome.Rejected message ->
        Expect.stringContains message "package store" $"{label}: names the store"
      | other -> failtest $"{label}: expected the guard to refuse, got {other}"
    let expectSuccess (label : string) (outcome : HT.Outcome) =
      match outcome with
      | HT.Outcome.Success _ -> ()
      | other -> failtest $"{label}: expected success, got {other}"
    try
      // Step 1 of the attack: move the store's parent.
      let! moved =
        perform (HT.Operation.Posix(HT.PosixOp.Rename(rundir, other + "2")))
      expectRefused "rename the store's parent" moved
      Expect.isTrue (System.IO.File.Exists db) "the store did not move"
      // A rename whose DESTINATION is an ancestor is refused on that endpoint.
      let! onto = perform (HT.Operation.Posix(HT.PosixOp.Rename(other, rundir)))
      expectRefused "rename onto the store's parent" onto
      // Removing the parent is refused too.
      let! removed = perform (HT.Operation.Posix(HT.PosixOp.Rmdir rundir))
      expectRefused "rmdir the store's parent" removed
      for flags in [ 0; 128; 512 ] do
        let! unlinked =
          perform (HT.Operation.Posix(HT.PosixOp.UnlinkAt(rundir, flags)))
        expectRefused "unlinkat the store's parent" unlinked
      // Any ancestor, not just the immediate parent.
      let! rootMoved =
        perform (HT.Operation.Posix(HT.PosixOp.Rename(root, root + "-moved")))
      expectRefused "rename a higher ancestor" rootMoved
      // The over-deny controls: siblings, and files beside the store in its
      // own directory, are still ordinary writable paths.
      let! sibling =
        perform (HT.Operation.Posix(HT.PosixOp.Rename(other, other + "-renamed")))
      expectSuccess "rename an unrelated sibling directory" sibling
      let! beside =
        perform (
          HT.Operation.FileWrite(
            System.IO.Path.Combine(rundir, "notes.txt"),
            [| 1uy |]
          )
        )
      expectSuccess "write a file beside the store" beside
      // An existing store directory must reach the OS (EEXIST), not the guard.
      let! mkdirExisting =
        perform (HT.Operation.Posix(HT.PosixOp.Mkdir(rundir, 0o755)))
      match mkdirExisting with
      | HT.Outcome.Rejected message ->
        failtest $"mkdir of the store's existing directory hit the guard: {message}"
      | _ -> ()
    finally
      restorePackageDbPath.Dispose()
      System.IO.Directory.Delete(root, true)
  }

/// The root directory is a directory. The libc walk hands every operation a
/// parent descriptor plus a final name, which `/` does not have, so listing
/// and stat on it failed with "no final component" and `pathExists "/"` was
/// false. Creating an entry AT the root is still refused, since there is
/// nothing to name.
let rootDirectoryIsReadable =
  testTask "listing and stat work on the filesystem root" {
    if LibExecution.HostPosix.isPosix then
      let access = Permission.Access.start Permission.Policy.allowAll
      let perform op = LibExecution.Host.perform Permission.NoRelax access op
      let! listed = perform (HT.Operation.DirectoryList "/")
      match listed with
      | HT.Outcome.Success _ -> ()
      | other -> failtest $"listing / failed: {other}"
      let! stat = perform (HT.Operation.FileStat "/")
      match stat with
      | HT.Outcome.Success(HT.Response.Stat(exists, isDirectory)) ->
        Expect.isTrue exists "/ exists"
        Expect.isTrue isDirectory "/ is a directory"
      | other -> failtest $"stat of / failed: {other}"
      // Creating AT the root is refused before the kernel is reached: `/` is an
      // ancestor of the policy directory, so the boundary's guard answers.
      // Either refusal is fine; a success would mean the root case leaked
      // into an entry-creating operation.
      let! created = perform (HT.Operation.Posix(HT.PosixOp.Mkdir("/", 0o755)))
      match created with
      | HT.Outcome.Failed _
      | HT.Outcome.Rejected _ -> ()
      | other -> failtest $"mkdir of / must not succeed, got {other}"
  }

/// Exercise the syscall transport through real descriptor and filesystem behavior.
let posixKernelRoundTrip =
  test "POSIX descriptor and metadata operations round trip" {
    if LibExecution.HostPosix.isPosix then
      let parent =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-kernel-{System.Guid.NewGuid():N}"
        )
      System.IO.Directory.CreateDirectory parent |> ignore<System.IO.DirectoryInfo>
      let ok result =
        match result with
        | Ok value -> value
        | Error(errno, message) -> failtestf "errno %d: %s" errno message
      let posix = LibExecution.HostPosix.O_RDWR ||| LibExecution.HostPosix.O_CREAT
      let file = System.IO.Path.Combine(parent, "original")
      let moved = System.IO.Path.Combine(parent, "renamed")
      try
        let fd = LibExecution.HostPosix.openFile file posix 0o600 |> ok
        try
          let bytes = [| 0uy; 255uy; 10uy; 42uy |]
          Expect.equal
            (LibExecution.HostPosix.fdWrite fd bytes |> ok)
            4
            "write count"
          Expect.equal (LibExecution.HostPosix.fdSeek fd 0L 0 |> ok) 0L "seek"
          Expect.equal (LibExecution.HostPosix.fdRead fd 4 |> ok) bytes "binary read"
          LibExecution.HostPosix.flock fd LibExecution.HostPosix.LOCK_EX |> ok
          LibExecution.HostPosix.flock fd LibExecution.HostPosix.LOCK_UN |> ok
        finally
          LibExecution.HostPosix.fdClose fd |> ignore<Result<unit, int * string>>
        let mode, size, _ = LibExecution.HostPosix.stat file |> ok
        Expect.equal size 4L "kernel stat size"
        Expect.equal (mode &&& 0o777) 0o600 "kernel stat permissions"
        Expect.equal
          (LibExecution.HostPosix.fileOwner file |> ok)
          (string (LibExecution.HostPosix.getuid ()))
          "kernel stat uid"
        LibExecution.HostPosix.rename file moved |> ok
        let link = System.IO.Path.Combine(parent, "link")
        LibExecution.HostPosix.symlink moved link |> ok
        Expect.equal
          (LibExecution.HostPosix.readlinkBytes link 4096 |> ok)
          (System.Text.Encoding.UTF8.GetBytes moved)
          "symlink bytes"
        let directory =
          LibExecution.HostPosix.mkdtemp (System.IO.Path.Combine(parent, "dir-"))
          |> ok
        Expect.equal
          (LibExecution.HostPosix.listDir directory |> ok)
          []
          "empty directory"
        Expect.equal
          (LibExecution.HostPosix.listDir parent |> ok |> Set.ofList)
          (Set.ofList [ "renamed"; "link"; System.IO.Path.GetFileName directory ])
          "directory records"
        LibExecution.HostPosix.syncDirectory parent
        LibExecution.HostPosix.unlink link |> ok
        LibExecution.HostPosix.unlink moved |> ok
        LibExecution.HostPosix.rmdir directory |> ok
        Expect.equal
          (LibExecution.HostPosix.kill (LibExecution.HostPosix.getpid ()) 0 |> ok)
          ()
          "signal zero"
        if System.OperatingSystem.IsMacOS() then
          Expect.isNonEmpty
            (LibExecution.HostPosix.sysctlBytes [ 1; 1 ] 256 |> ok)
            "raw OS identity"
        else
          Expect.equal
            (LibExecution.HostPosix.unameBytes () |> ok |> Array.length)
            390
            "raw utsname record"
        match LibExecution.HostPosix.fdRead -1 1 with
        | Error(errno, _) ->
          Expect.equal errno 9 "negative kernel errno is mapped to EBADF"
        | Ok _ -> failtest "invalid descriptor must fail"
      finally
        System.IO.Directory.Delete(parent, true)
  }

let rawClocksRequireClockPermission =
  testTask "raw clocks and sleeps retain the Clock gate at Host" {
    for operation in
      [ HT.PosixOp.ClockGettime -1
        HT.PosixOp.Gettimeofday
        HT.PosixOp.MachAbsoluteTime
        HT.PosixOp.MachTimebaseInfo
        HT.PosixOp.Nanosleep(0L, 0L) ] do
      let! outcome =
        LibExecution.Host.perform
          Permission.NoRelax
          (Permission.Access.start Permission.Policy.denyAll)
          (HT.Operation.Posix operation)
      match outcome with
      | HT.Outcome.Denied(Permission.Layer.Instance, _, _, _) -> ()
      | other -> failtest $"expected Clock denial before kernel errno, got {other}"
  }

// A worker can exit while its descendants keep running, with or without open
// output pipes. Cleanup must stop those descendants in every completion path.
let isolatedTestBoundsOutputCollection =
  testSequenced
  <| test "isolated test cleanup stops descendants after worker exit or timeout" {
    if LibExecution.HostPosix.isPosix then
      let launcher = System.Environment.GetEnvironmentVariable "DARK_TEST_CLI"
      Expect.isFalse
        (System.String.IsNullOrEmpty launcher)
        "test runner supplies the CLI"
      let processIsRunning pid =
        try
          use child = System.Diagnostics.Process.GetProcessById pid
          if child.HasExited then
            false
          elif System.OperatingSystem.IsLinux() then
            // Orphans are reaped by PID 1. A zombie has exited even if that
            // reaper has not collected it yet (common in dev containers).
            let stat = System.IO.File.ReadAllText $"/proc/{pid}/stat"
            stat[stat.LastIndexOf(')') + 2] <> 'Z'
          else
            true
        with
        | :? System.ArgumentException
        | :? System.IO.FileNotFoundException
        | :? System.IO.DirectoryNotFoundException -> false
        | :? System.IO.IOException when System.OperatingSystem.IsLinux() ->
          // The process can be reaped after opening stat but before reading it.
          // If its entry still exists, keep polling rather than assume it exited.
          System.IO.Directory.Exists $"/proc/{pid}"
      for label, redirect, ending, expectedCode in
        [ "inherited output", "", "exit 0", None
          "successful worker", ">/dev/null 2>&1", "exit 0", Some 0
          "failed worker", ">/dev/null 2>&1", "exit 17", Some 17
          "running worker", "", "wait", None
          "nested worker group", "", "wait", None ] do
        let dir =
          System.IO.Directory.CreateTempSubdirectory("dark-worker-check-").FullName
        let script = System.IO.Path.Combine(dir, "worker")
        let pidFile = System.IO.Path.Combine(dir, "child.pid")
        try
          let body =
            if label = "nested worker group" then
              let nested = System.IO.Path.Combine(dir, "nested-worker")
              System.IO.File.WriteAllText(
                nested,
                $"echo $$ > '{pidFile}'\nsleep 30 &\necho $! >> '{pidFile}'\nwait\n"
              )
              let quote (value : string) = "'" + value.Replace("'", "'\"'\"'") + "'"
              $"{quote launcher} --process-launch /bin/sh {quote nested} &\nwait\n"
            else
              $"sleep 30 {redirect} &\necho $! > '{pidFile}'\n{ending}\n"
          System.IO.File.WriteAllText(script, "#!/bin/sh\n" + body)
          System.IO.File.SetUnixFileMode(
            script,
            System.IO.UnixFileMode.UserRead ||| System.IO.UnixFileMode.UserExecute
          )
          let outcome =
            LibExecution.HostProcess.runInWorkspace
              launcher
              "dark-worker-check-"
              (fun _ ->
                Ok(
                  { program = script
                    arguments = []
                    environment = []
                    files = []
                    directories = []
                    resultFile = "result.bin" }
                  : HT.ProcessWorkspace
                ))
              2000
          match expectedCode, outcome with
          | None, Error(110, message) ->
            Expect.stringContains message "timed out" $"{label}: wait is bounded"
          | Some expected, Ok(code, _, _, _, cleanupErrors) ->
            Expect.equal code expected $"{label}: worker exit is preserved"
            Expect.isEmpty cleanupErrors $"{label}: cleanup succeeded"
          | _ -> failtest $"{label}: unexpected outcome {outcome}"
          Expect.isTrue
            (System.IO.File.Exists pidFile)
            $"{label}: worker actually ran"
          let pids =
            System.IO.File.ReadAllLines pidFile |> Array.map System.Int32.Parse
          Expect.equal
            pids.Length
            (if label = "nested worker group" then 2 else 1)
            "all descendants started"
          for pid in pids do
            Expect.isTrue
              (System.Threading.SpinWait.SpinUntil(
                (fun () -> not (processIsRunning pid)),
                5000
              ))
              $"{label}: descendant must stop without test-side cleanup"
        finally
          // Failure-only safety net: the assertion above must pass before this
          // cleanup runs, so it cannot hide a leaked descendant.
          if System.IO.File.Exists pidFile then
            for pidText in System.IO.File.ReadAllLines pidFile do
              let pid = System.Int32.Parse pidText
              if processIsRunning pid then
                try
                  use child = System.Diagnostics.Process.GetProcessById pid
                  child.Kill()
                with :? System.ArgumentException ->
                  ()
          System.IO.Directory.Delete(dir, true)
  }

let isolatedTestPreservesFailureWhenCleanupFails =
  test "isolated test reports snapshot and cleanup failures together" {
    let mutable directory = ""
    let snapshot (path : string) =
      directory <- System.IO.Path.GetDirectoryName path
      // Replacing the directory with a file makes directory cleanup fail
      // deterministically, including when the tests run as root.
      System.IO.Directory.Delete directory
      System.IO.File.WriteAllText(directory, "cleanup obstruction")
      Error "original snapshot failure"
    try
      match
        LibExecution.HostProcess.runInWorkspace
          "unused launcher"
          "dark-worker-check-"
          (fun dir -> snapshot (System.IO.Path.Combine(dir, "data.db")))
          1000
      with
      | Error(_, message) ->
        Expect.stringContains
          message
          "original snapshot failure"
          "preserves the original failure"
        Expect.stringContains
          message
          "Could not remove child process directory"
          "also reports cleanup"
      | other -> failtest $"expected both failures, got {other}"
    finally
      if directory <> "" then System.IO.File.Delete directory
  }

let testRunSnapshot =
  test "test run reuses an immutable baseline and closes its scope" {
    let dir =
      System.IO.Directory.CreateTempSubdirectory("test-baseline-check-").FullName
    let mutable backups = 0
    let mutable source = "initial store"
    let mutable baselineDirectory = ""
    let backup (path : string) =
      backups <- backups + 1
      baselineDirectory <- System.IO.Path.GetDirectoryName path
      System.IO.File.WriteAllText(path, source)
      Ok()
    try
      use baseline = LibExecution.HostProcess.fileSnapshot backup
      Expect.equal backups 0 "cached-only runs do not back up the store"
      let first = System.IO.Path.Combine(dir, "first.db")
      let second = System.IO.Path.Combine(dir, "second.db")
      Expect.equal (baseline.CopyTo first) (Ok()) "first worker"
      source <- "parent changed"
      System.IO.File.WriteAllText(first, "test wrote here")
      Expect.equal (baseline.CopyTo second) (Ok()) "second worker"
      Expect.equal backups 1 "only one online backup per run"
      Expect.equal
        (System.IO.File.ReadAllText second)
        "initial store"
        "neither parent edits nor sibling writes change the baseline"
      baseline.Dispose()
      Expect.isFalse
        (System.IO.Directory.Exists baselineDirectory)
        "baseline cleaned up"
      Expect.isError
        (baseline.CopyTo(System.IO.Path.Combine(dir, "late.db")))
        "closed scope"
      use nextRun = LibExecution.HostProcess.fileSnapshot backup
      let next = System.IO.Path.Combine(dir, "next.db")
      Expect.equal (nextRun.CopyTo next) (Ok()) "next run gets its own baseline"
      Expect.equal backups 2 "a later run never reuses the old baseline"
      Expect.equal
        (System.IO.File.ReadAllText next)
        "parent changed"
        "fresh starting store"
    finally
      System.IO.Directory.Delete(dir, true)
  }

let sharedTestRunSnapshot =
  test "a shared baseline is read-only, and copies of it stay writable" {
    let dir =
      System.IO.Directory.CreateTempSubdirectory("test-baseline-share-").FullName
    let mutable backups = 0
    let backup (path : string) =
      backups <- backups + 1
      System.IO.File.WriteAllText(path, "initial store")
      Ok()
    let at name = System.IO.Path.Combine(dir, name)
    try
      use baseline = LibExecution.HostProcess.fileSnapshot backup
      match baseline.LinkTo(at "first.db") with
      | Error message ->
        // Root writes read-only files anyway, so it must not be offered a link.
        Expect.stringContains message "can write read-only files" "only root refuses"
      | Ok() ->
        Expect.equal
          (System.IO.File.ReadAllText(at "first.db"))
          "initial store"
          "a link reads the baseline"
        Expect.throwsT<System.UnauthorizedAccessException>
          (fun () -> System.IO.File.WriteAllText(at "first.db", "test wrote here"))
          "a link can't be written"
        Expect.equal (baseline.LinkTo(at "second.db")) (Ok()) "linked again"
      // Copies happen outside the snapshot's lock, so several can be made at once.
      let copies =
        [ 1..8 ]
        |> List.map (fun n ->
          System.Threading.Tasks.Task.Run(fun () ->
            baseline.CopyTo(at $"copy{n}.db")))
        |> List.map (fun copy -> copy.Result)
      Expect.allEqual copies (Ok()) "every copy"
      System.IO.File.WriteAllText(at "copy1.db", "test wrote here")
      Expect.equal
        (System.IO.File.ReadAllText(at "copy2.db"))
        "initial store"
        "a copy is writable, and its own"
      Expect.equal backups 1 "one backup serves shares and copies"
      baseline.Dispose()
      Expect.isError (baseline.LinkTo(at "late.db")) "closed scope"
    finally
      System.IO.Directory.Delete(dir, true)
  }

let changedSharedSnapshot =
  test "a shared baseline that is changed is neither linked nor copied again" {
    let dir =
      System.IO.Directory.CreateTempSubdirectory("test-baseline-changed-").FullName
    let backup (path : string) =
      System.IO.File.WriteAllText(path, "initial store")
      Ok()
    let at name = System.IO.Path.Combine(dir, name)
    try
      use baseline = LibExecution.HostProcess.fileSnapshot backup
      match baseline.LinkTo(at "first.db") with
      | Error message ->
        Expect.stringContains message "can write read-only files" "only root refuses"
      | Ok() ->
        Expect.isFalse (baseline.Changed()) "unchanged after linking"
        // The one way a reader can change it: make it writable again first.
        System.IO.File.SetUnixFileMode(
          at "first.db",
          System.IO.UnixFileMode.UserRead ||| System.IO.UnixFileMode.UserWrite
        )
        System.IO.File.WriteAllText(at "first.db", "test wrote here")
        Expect.isTrue (baseline.Changed()) "the change is noticed"
        Expect.isError
          (baseline.LinkTo(at "second.db"))
          "no link to a changed baseline"
        Expect.isError (baseline.CopyTo(at "copy.db")) "and no copy of it either"
    finally
      System.IO.Directory.Delete(dir, true)
  }

let failedTestRunSnapshot =
  test "a failed baseline is not retried and its partial directory is removed" {
    let mutable backups = 0
    let mutable directory = ""
    let backup (path : string) =
      backups <- backups + 1
      directory <- System.IO.Path.GetDirectoryName path
      System.IO.File.WriteAllText(path, "partial backup")
      Error "backup failed"
    use baseline = LibExecution.HostProcess.fileSnapshot backup
    Expect.equal
      (baseline.CopyTo "unused")
      (Error "backup failed")
      "failure is preserved"
    Expect.equal
      (baseline.CopyTo "unused")
      (Error "backup failed")
      "same run, same failure"
    Expect.equal backups 1 "never silently change the run's starting store"
    baseline.Dispose()
    Expect.isFalse
      (System.IO.Directory.Exists directory)
      "partial baseline cleaned up"
  }

/// Each opt-in widens the range it names and nothing else. Asserted as values rather than by
/// installing a config, which is process-wide and would contradict the Dark table asserting
/// these same addresses are refused by default.
let httpWideningsAreScoped =
  testTask "each HTTP opt-in widens only its own range" {
    let ip (s : string) = System.Net.IPAddress.Parse s
    let off = HostHttp.defaultConfig
    let loopback = HostHttp.guestConfigWidenedBy [ HostHttp.loopbackWidening ]
    let priv = HostHttp.guestConfigWidenedBy [ HostHttp.privateNetworkWidening ]
    let both =
      HostHttp.guestConfigWidenedBy
        [ HostHttp.loopbackWidening; HostHttp.privateNetworkWidening ]

    let loopbacks = [ "127.0.0.1"; "::1" ]
    // 100.64/10 is RFC 6598 shared space, which mesh VPNs and carrier NAT assign from.
    let privates =
      [ "10.0.0.5"; "172.16.0.5"; "192.168.1.5"; "100.64.0.5"; "fd00::1" ]
    let never = [ "169.254.169.254"; "0.0.0.0"; "fe80::1" ]

    for a in loopbacks @ privates @ never do
      Expect.isFalse (off.allowedIP (ip a)) $"default refuses {a}"
    for a in loopbacks do
      Expect.isTrue (loopback.allowedIP (ip a)) $"http.loopback allows {a}"
      Expect.isFalse
        (priv.allowedIP (ip a))
        $"http.private does not imply loopback: {a}"
    for a in privates do
      Expect.isTrue (priv.allowedIP (ip a)) $"http.private allows {a}"
      Expect.isFalse
        (loopback.allowedIP (ip a))
        $"http.loopback does not imply private: {a}"
    for a in never do
      Expect.isFalse (both.allowedIP (ip a)) $"no opt-in widens {a}"

    // The host half moves with the IP half, or the literal a person types is refused.
    Expect.isTrue (loopback.allowedHost "localhost") "http.loopback allows the name"
    Expect.isTrue (priv.allowedHost "10.0.0.5") "http.private allows the literal"
    Expect.isFalse
      (priv.allowedHost "localhost")
      "http.private does not allow localhost"
  }

let tests =
  testList
    "host"
    [ posixKernelRoundTrip
      httpWideningsAreScoped
      testRunSnapshot
      sharedTestRunSnapshot
      changedSharedSnapshot
      failedTestRunSnapshot
      isolatedTestBoundsOutputCollection
      isolatedTestPreservesFailureWhenCleanupFails
      auditRecordsOperationsAndDecisions
      httpServerBindUsesTheSuppliedAccess
      rawDescriptorsRequireNativeAtHostBoundary
      scopedDescriptorsRecheckAccessAndOwnership
      descriptorLeasesDoNotSerializeUnrelatedFiles
      ownedChildrenRetainScopedProcessAuthority
      ownedChildWaitAllowsCleanup
      watchQueuesRetainScopedPathAuthority
      systemFactsDoNotGrantSensitiveQueries
      rawClocksRequireClockPermission
      tempPrefixDoesNotAuthorizeARandomSibling
      readlinkMayInspectTheFinalSymlink
      posixWalkRefusesALinkMetAtOperationTime
      localPolicyPathsRejectTraversal
      packageStoreAncestorsAreProtected
      rootDirectoryIsReadable ]
