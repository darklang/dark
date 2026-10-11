/// The checked host boundary: audit summaries, the server bind, temporary
/// paths, links met at operation time, and the protected policy directory.
module Tests.Host

open Expecto
open Prelude
open TestUtils.PTShortcuts

module Permission = LibExecution.Permissions
module LocalFile = LibDB.LocalFile
module HT = LibExecution.HostTypes

let private only (item : 'a) : Permission.Scope<'a> = Permission.Scope.Only item

let private setTimesNow path =
  if System.OperatingSystem.IsMacOS() then
    let seconds = System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()
    LibExecution.HostPosix.setattrlistTimes path seconds 0L seconds 0L
  else LibExecution.HostPosix.utimensAt path 0L 1073741823L 0L 1073741823L

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
        HT.PosixOp.CopyFileRange(HT.FileDescriptor.Raw -1, HT.FileDescriptor.Raw -1, 1) ]
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
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"dark-fd-{System.Guid.NewGuid()}")
    System.IO.File.WriteAllText(path, "abc")
    let owner = LibExecution.Host.createDescriptorSession ()
    let foreign = LibExecution.Host.createDescriptorSession ()
    let readOnly =
      Permission.Policy.create
        [ Permission.Rule.File(Permission.AccessKind.Read, only path) ] []
      |> Permission.Access.start
    let denied = Permission.Access.start Permission.Policy.denyAll
    let all = Permission.Access.start Permission.Policy.allowAll
    let perform session access op =
      LibExecution.Host.performInSession session Permission.NoRelax access (HT.Operation.Posix op)
    let ebadf outcome =
      match outcome with
      | HT.Outcome.Failed failure -> Expect.equal failure.errno 9 "invalid capability"
      | other -> failtest $"expected EBADF, got {other}"
    try
      let! opened = perform owner readOnly (HT.PosixOp.Open(path, 0, 0))
      let token =
        match opened with
        | HT.Outcome.Success(HT.Response.Descriptor token) -> token
        | other -> failtestf "expected scoped open without Native, got %A" other
      let fd = HT.FileDescriptor.Scoped token
      let! read = perform owner readOnly (HT.PosixOp.FdRead(fd, 3))
      match read with
      | HT.Outcome.Success(HT.Response.Bytes bytes) -> Expect.equal bytes [| 97uy; 98uy; 99uy |] "read bytes"
      | other -> failtest $"expected scoped read, got {other}"
      let! stat = perform owner readOnly (HT.PosixOp.Fstat fd)
      match stat with
      | HT.Outcome.Success(HT.Response.Bytes bytes) -> Expect.isNonEmpty bytes "raw stat record"
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
      Expect.equal closed (HT.Outcome.Success HT.Response.Unit) "cleanup needs no new grant"
      let! again = perform owner all (HT.PosixOp.FdClose fd)
      ebadf again
      let! stale = perform owner all (HT.PosixOp.Fstat fd)
      ebadf stale
    finally
      owner.Dispose()
      foreign.Dispose()
      System.IO.File.Delete path
  }

let systemFactsDoNotGrantSensitiveQueries =
  testTask "host facts do not grant arbitrary sysctl or foreign affinity access" {
    let access = Permission.Access.start Permission.Policy.denyAll
    for op in
      [ HT.PosixOp.SysctlBytes([ 1; 14 ], 256)
        HT.PosixOp.SchedGetaffinity(1, 128) ] do
      let! outcome = LibExecution.Host.perform Permission.NoRelax access (HT.Operation.Posix op)
      match outcome with
      | HT.Outcome.Denied _ -> ()
      | other -> failtest $"expected Native denial before kernel query, got {other}"
    let! local = LibExecution.Host.perform Permission.NoRelax access
                   (HT.Operation.Posix(HT.PosixOp.SchedGetaffinity(0, 128)))
    match local with
    | HT.Outcome.Success(HT.Response.Bytes bytes) -> Expect.isNonEmpty bytes "local affinity mask"
    | HT.Outcome.Failed failure -> Expect.isTrue (failure.errno = 38 || failure.errno = 78) "unsupported target"
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
        Expect.equal actual (System.Text.Encoding.UTF8.GetBytes target) "readlink returns raw stored target bytes"
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
        Expect.isOk
          (setTimesNow file)
          "utimes works without file read permission"
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
          Expect.isNonEmpty (LibExecution.HostPosix.sysctlBytes [ 1; 1 ] 256 |> ok) "raw OS identity"
        else
          Expect.equal (LibExecution.HostPosix.unameBytes () |> ok |> Array.length) 390 "raw utsname record"
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
      [ HT.PosixOp.ClockGettime -1; HT.PosixOp.Gettimeofday
        HT.PosixOp.MachAbsoluteTime; HT.PosixOp.MachTimebaseInfo
        HT.PosixOp.Nanosleep(0L, 0L) ] do
      let! outcome =
        LibExecution.Host.perform Permission.NoRelax
          (Permission.Access.start Permission.Policy.denyAll)
          (HT.Operation.Posix operation)
      match outcome with
      | HT.Outcome.Denied(Permission.Layer.Instance, _, _, _) -> ()
      | other -> failtest $"expected Clock denial before kernel errno, got {other}"
  }

let tests =
  testList
    "host"
    [ posixKernelRoundTrip
      auditRecordsOperationsAndDecisions
      httpServerBindUsesTheSuppliedAccess
      rawDescriptorsRequireNativeAtHostBoundary
      scopedDescriptorsRecheckAccessAndOwnership
      systemFactsDoNotGrantSensitiveQueries
      rawClocksRequireClockPermission
      tempPrefixDoesNotAuthorizeARandomSibling
      readlinkMayInspectTheFinalSymlink
      posixWalkRefusesALinkMetAtOperationTime
      localPolicyPathsRejectTraversal
      packageStoreAncestorsAreProtected
      rootDirectoryIsReadable ]
