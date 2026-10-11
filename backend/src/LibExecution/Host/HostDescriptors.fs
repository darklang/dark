/// Execution-owned descriptor capabilities. The host boundary checks requests
/// while holding each descriptor's lease, so close/reuse cannot redirect an
/// authorized operation or block unrelated descriptors behind a waiting read.
module LibExecution.HostDescriptors

open Prelude

type Entry =
  { fd : int
    paths : List<string>
    reads : bool
    writes : bool }

type private Lease =
  { mutable entry : Entry; gate : obj; mutable closed : bool }

let tokenFloor : bigint = 1I <<< 128

type Session(close : int -> Result<unit, int * string>) =
  let gate = obj ()
  let entries = System.Collections.Generic.Dictionary<bigint, Lease>()
  let mutable disposed = false

  let dispose () =
    let owned =
      lock gate (fun () ->
        if disposed then []
        else
          disposed <- true
          entries |> Seq.map (fun pair -> pair.Key, pair.Value) |> Seq.toList)
    // Never wait for a descriptor while holding the dictionary lock. A caller
    // holding that descriptor may still need the dictionary to consume it.
    for token, lease in owned do
      lock lease.gate (fun () ->
        if not lease.closed then
          lease.closed <- true
          try close lease.entry.fd |> ignore<Result<unit, int * string>> with _ -> ()
        lock gate (fun () -> entries.Remove token |> ignore<bool>))

  /// Acquire in token order for operations using multiple descriptors. Close
  /// and disposal use the same leases, preventing fd reuse during a syscall.
  member _.WithLeases(tokens : List<bigint>, run : unit -> 'a) : 'a =
    let leases =
      lock gate (fun () ->
        tokens |> List.distinct |> List.sort
        |> List.choose (fun token ->
          match entries.TryGetValue token with
          | true, lease -> Some lease
          | _ -> None))
    let rec acquire remaining =
      match remaining with
      | [] -> run ()
      | lease :: rest -> lock lease.gate (fun () -> acquire rest)
    acquire leases

  /// Only called after the checked open, with its exact normalized path.
  member _.Register(entry : Entry) : bigint =
    lock gate (fun () ->
      if disposed then
        close entry.fd |> ignore<Result<unit, int * string>>
        invalidOp "descriptor session is disposed"
      let rec fresh () =
        let bytes = System.Security.Cryptography.RandomNumberGenerator.GetBytes 16
        let token = tokenFloor + System.Numerics.BigInteger(bytes, true, true)
        if entries.ContainsKey token then fresh () else token
      let token = fresh ()
      entries.Add(token, { entry = entry; gate = obj (); closed = false })
      token)

  member _.Find(token : bigint) : Option<Entry> =
    lock gate (fun () ->
      match entries.TryGetValue token with
      | true, lease when not disposed && not lease.closed -> Some lease.entry
      | _ -> None)

  /// A successful kernel watch registration adds the exact checked resource
  /// to the queue's authority. All later queue reads/waits recheck every path.
  /// Called with that queue's lease held.
  member _.WatchPath(fd : int, path : string) : unit =
    lock gate (fun () ->
      for lease in entries.Values do
        if lease.entry.fd = fd && not lease.closed then
          lease.entry <- { lease.entry with paths = List.distinct (path :: lease.entry.paths) })

  /// The caller holds leases on the queue and every registered vnode. Keep
  /// their resource checks attached to the queue after registration returns.
  member _.WatchDescriptors(queue : int, sources : List<int>) : unit =
    lock gate (fun () ->
      let paths =
        entries.Values |> Seq.filter (fun lease -> List.contains lease.entry.fd sources)
        |> Seq.collect (fun lease -> lease.entry.paths) |> Seq.toList
      for lease in entries.Values do
        if lease.entry.fd = queue && not lease.closed then
          lease.entry <- { lease.entry with paths = List.distinct (paths @ lease.entry.paths) })

  /// Consume ownership before close, including when close returns EINTR.
  member _.Consume(token : bigint) : unit =
    lock gate (fun () ->
      match entries.TryGetValue token with
      | true, lease ->
        lease.closed <- true
        entries.Remove token |> ignore<bool>
      | _ -> ())

  member _.Dispose() = dispose ()
  override _.Finalize() = dispose ()

  interface System.IDisposable with
    member _.Dispose() = dispose ()
