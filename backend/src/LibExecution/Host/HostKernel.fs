/// Private, typed transport to the kernel. Only trusted host modules may call
/// this; Darklang has no generic syscall, native pointer, or buffer capability.
module internal LibExecution.HostKernel

open System
open System.Runtime.InteropServices

let private failwith (s : string) : 'a = raise (Exception(s))

[<DllImport("dark_syscalls", EntryPoint = "dark_syscall")>]
extern int64 private invoke(
  int operation,
  int64 a,
  int64 b,
  int64 c,
  int64 d,
  int64 e,
  int64 f
)

/// Store the kernel errno in .NET's managed slot, never libc errno TLS.
let private result (rc : int64) : int64 =
  if rc < 0L then
    Marshal.SetLastPInvokeError(int -rc)
    -1L
  else
    rc

let private call op a b c d e f = invoke (op, a, b, c, d, e, f) |> result
let private pointer (p : IntPtr) = p.ToInt64()
let private withText (text : string) (f : int64 -> 'a) : 'a =
  let p = Marshal.StringToCoTaskMemUTF8 text
  try
    f (pointer p)
  finally
    Marshal.FreeCoTaskMem p

let private withBytes (bytes : byte[]) (f : int64 -> 'a) : 'a =
  let pinned = GCHandle.Alloc(bytes, GCHandleType.Pinned)
  try
    f (pointer (pinned.AddrOfPinnedObject()))
  finally
    pinned.Free()

let openat_raw (fd : int, path : string, flags : int, mode : int) =
  withText path (fun p -> int (call 0 (int64 fd) p (int64 flags) (int64 mode) 0L 0L))
let read_raw (fd : int, bytes : byte[], count : int) =
  withBytes bytes (fun p -> int (call 1 (int64 fd) p (int64 count) 0L 0L 0L))
let write_raw (fd : int, bytes : byte[], count : int) =
  withBytes bytes (fun p -> int (call 2 (int64 fd) p (int64 count) 0L 0L 0L))
let close_raw (fd : int) = int (call 3 (int64 fd) 0L 0L 0L 0L 0L)
let lseek_raw (fd : int, offset : int64, whence : int) =
  call 4 (int64 fd) offset (int64 whence) 0L 0L 0L
let fstatat_raw (fd : int, path : string, buf : IntPtr, flags : int) =
  withText path (fun p ->
    int (call 5 (int64 fd) p (pointer buf) (int64 flags) 0L 0L))
let fchdir_raw (fd : int) = int (call 7 (int64 fd) 0L 0L 0L 0L 0L)
let mkdirat_raw (fd : int, path : string, mode : int) =
  withText path (fun p -> int (call 8 (int64 fd) p (int64 mode) 0L 0L 0L))
let unlinkat_raw (fd : int, path : string, flags : int) =
  withText path (fun p -> int (call 9 (int64 fd) p (int64 flags) 0L 0L 0L))
let renameat_raw (oldFd : int, oldPath : string, newFd : int, newPath : string) =
  withText oldPath (fun a ->
    withText newPath (fun b -> int (call 10 (int64 oldFd) a (int64 newFd) b 0L 0L)))
let fchmodat_raw (fd : int, path : string, mode : int, flags : int) =
  withText path (fun p ->
    int (call 11 (int64 fd) p (int64 mode) (int64 flags) 0L 0L))
let utimensat_raw (fd : int, path : string, times : IntPtr, flags : int) =
  withText path (fun p ->
    int (call 12 (int64 fd) p (pointer times) (int64 flags) 0L 0L))
let symlinkat_raw (target : string, fd : int, path : string) =
  withText target (fun a ->
    withText path (fun b -> int (call 13 a (int64 fd) b 0L 0L 0L)))
let readlinkat_raw (fd : int, path : string, bytes : byte[], count : int) =
  withText path (fun a ->
    withBytes bytes (fun b -> int (call 14 (int64 fd) a b (int64 count) 0L 0L)))
let flock_raw (fd : int, operation : int) =
  int (call 15 (int64 fd) (int64 operation) 0L 0L 0L 0L)
let getdents_raw (fd : int, bytes : byte[], position : IntPtr) =
  withBytes bytes (fun p ->
    int (call 16 (int64 fd) p (int64 bytes.Length) (pointer position) 0L 0L))
let ioctl_raw (fd : int, request : uint64, argument : IntPtr) =
  int (call 17 (int64 fd) (int64 request) (pointer argument) 0L 0L 0L)
let kill_raw (pid : int, signal : int) =
  int (call 18 (int64 pid) (int64 signal) 0L 0L 0L 0L)
let getpid_raw () = int (call 19 0L 0L 0L 0L 0L 0L)
let getuid_raw () = uint32 (call 20 0L 0L 0L 0L 0L 0L)
let unameBytes_raw (bytes : byte[]) =
  withBytes bytes (fun p -> int (call 21 p 0L 0L 0L 0L 0L))
let schedGetaffinity_raw (pid : int, bytes : byte[]) =
  withBytes bytes (fun p -> int (call 35 (int64 pid) (int64 bytes.Length) p 0L 0L 0L))

let getentropy_raw (bytes : byte[]) =
  withBytes bytes (fun ptr -> int (call 34 ptr (int64 bytes.Length) 0L 0L 0L 0L))
let openat2_raw (fd : int, path : string, how : byte[], size : unativeint) =
  withText path (fun a ->
    withBytes how (fun b -> call 22 (int64 fd) a b (int64 size) 0L 0L))
let fchmodat2_raw (fd : int, path : string, mode : int, flags : int) =
  withText path (fun p ->
    int (call 23 (int64 fd) p (int64 mode) (int64 flags) 0L 0L))
let fsync_raw (fd : int) = int (call 24 (int64 fd) 0L 0L 0L 0L 0L)
let copyFileRange_raw (source : int, destination : int, count : int) =
  call 27 (int64 source) 0L (int64 destination) 0L (int64 count) 0L
let ftruncate_raw (fd : int, length : int64) =
  int (call 28 (int64 fd) length 0L 0L 0L 0L)
let fchmod_raw (fd : int, mode : int) =
  int (call 29 (int64 fd) (int64 mode) 0L 0L 0L 0L)
let fstat_raw (fd : int, bytes : byte[]) =
  withBytes bytes (fun p -> int (call 30 (int64 fd) p 0L 0L 0L 0L))
let pread_raw (fd : int, bytes : byte[], offset : int64) =
  withBytes bytes (fun p ->
    int (call 31 (int64 fd) p (int64 bytes.Length) offset 0L 0L))
let pwrite_raw (fd : int, bytes : byte[], offset : int64) =
  withBytes bytes (fun p ->
    int (call 32 (int64 fd) p (int64 bytes.Length) offset 0L 0L))
let fclonefileat_raw (source : int, parent : int, name : string, flags : int) =
  withText name (fun p ->
    int (call 33 (int64 source) (int64 parent) p (int64 flags) 0L 0L))
let sysctl_raw (mib : byte[], buf : IntPtr, size : IntPtr) =
  withBytes mib (fun p -> int (call 25 p (int64 (mib.Length / 4)) (pointer buf) (pointer size) 0L 0L))
let setattrlistat_raw (fd : int, path : string, attrs : byte[]) =
  // ATTR_CMN_ACCTIME | ATTR_CMN_MODTIME; both set to the caller's timestamp.
  let attributes = Array.zeroCreate<byte> 24
  BitConverter.GetBytes(5us).CopyTo(attributes, 0)
  BitConverter.GetBytes(0x1400u).CopyTo(attributes, 4)
  withText path (fun p ->
    withBytes attributes (fun a ->
      withBytes attrs (fun b ->
        int (call 26 (int64 fd) p a b (int64 attrs.Length) 1L))))
