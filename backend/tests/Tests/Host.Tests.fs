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
          (HT.Operation.Posix(HT.PosixOp.Readlink link))
      match outcome with
      | HT.Outcome.Success(HT.Response.Path actual) ->
        Expect.equal actual target "readlink returns the stored target"
      | other -> failtest $"expected readlink success, got {other}"
    finally
      System.IO.Directory.Delete(directory, true)
  }

let libcWalkRefusesALinkMetAtOperationTime =
  testTask
    "the libc bridge meets a link at operation time and refuses it, without the pre-check" {
    // The boundary's lexical pre-check can be raced: a component swapped for
    // a link after the check. The bridge walks the directories again with
    // O_NOFOLLOW at operation time, so calling it directly through a link,
    // as a race would, must fail rather than follow.
    if LibExecution.HostLibc.isPosix then
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
          (LibExecution.HostLibc.unlink viaLink)
          "unlink through a linked ancestor is refused by the walk"
        Expect.isTrue (System.IO.File.Exists file) "and the file is untouched"
        Expect.isError
          (LibExecution.HostLibc.stat viaLink)
          "stat through a linked ancestor is refused"
        Expect.isError
          (LibExecution.HostLibc.listDir link)
          "listing a linked directory entry is refused"
        Expect.throws
          (fun () -> LibExecution.HostLibc.readAllBytes viaLink |> ignore<byte[]>)
          "reading through a linked ancestor is refused"
        let originalMode = System.IO.File.GetUnixFileMode file
        let originalMtime = System.DateTime.UnixEpoch.AddDays 1.0
        System.IO.File.SetLastWriteTimeUtc(file, originalMtime)
        Expect.isError
          (LibExecution.HostLibc.chmod finalLink 0o777)
          "chmod refuses a linked final component"
        Expect.equal
          (System.IO.File.GetUnixFileMode file)
          originalMode
          "chmod did not reach the link target"
        Expect.isError
          (LibExecution.HostLibc.utimesNow finalLink)
          "utimes refuses a linked final component"
        Expect.equal
          (System.IO.File.GetLastWriteTimeUtc file)
          originalMtime
          "utimes did not reach the link target"
        // The same operations on the real path work, through the same walk.
        System.IO.File.SetUnixFileMode(file, System.IO.UnixFileMode.None)
        Expect.isOk
          (LibExecution.HostLibc.utimesNow file)
          "utimes works without file read permission"
        Expect.isOk
          (LibExecution.HostLibc.chmod file 0o600)
          "chmod can recover a mode-000 file"
        Expect.isOk (LibExecution.HostLibc.stat file) "stat on the real path"
        Expect.equal
          (LibExecution.HostLibc.listDir real |> Result.map Set.ofList)
          (Ok(Set.ofList [ "f"; "final-link" ]))
          "listing on the real path"
        match
          LibExecution.HostLibc.mkstemp (System.IO.Path.Combine(real, "tmp-"))
        with
        | Ok(fd, path) ->
          LibExecution.HostLibc.fdClose fd |> ignore<Result<unit, int * string>>
          Expect.isTrue (System.IO.File.Exists path) "mkstemp created the file"
          Expect.equal
            (System.IO.Path.GetDirectoryName path)
            real
            "mkstemp created it in the walked directory"
        | Error(_, message) -> failtest $"mkstemp failed: {message}"
        Expect.equal
          (LibExecution.HostLibc.statEntry (System.IO.Path.Combine(real, "missing")))
          (Ok(false, false))
          "a missing entry is reported, not an error"
        Expect.equal
          (LibExecution.HostLibc.statEntry real)
          (Ok(true, true))
          "a directory is reported as one"
        Expect.isOk (LibExecution.HostLibc.unlink file) "unlink on the real path"
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
    if LibExecution.HostLibc.isPosix then
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
      // Creating AT the root is refused before libc is reached: `/` is an
      // ancestor of the policy directory, so the boundary's guard answers.
      // Either refusal is fine; a success would mean the root case leaked
      // into an entry-creating operation.
      let! created = perform (HT.Operation.Posix(HT.PosixOp.Mkdir("/", 0o755)))
      match created with
      | HT.Outcome.Failed _
      | HT.Outcome.Rejected _ -> ()
      | other -> failtest $"mkdir of / must not succeed, got {other}"
  }

let tests =
  testList
    "host"
    [ auditRecordsOperationsAndDecisions
      httpServerBindUsesTheSuppliedAccess
      tempPrefixDoesNotAuthorizeARandomSibling
      readlinkMayInspectTheFinalSymlink
      libcWalkRefusesALinkMetAtOperationTime
      localPolicyPathsRejectTraversal
      packageStoreAncestorsAreProtected
      rootDirectoryIsReadable ]
