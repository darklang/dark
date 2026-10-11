/// Execution-owned descriptor capabilities. The host boundary checks requests
/// while holding Gate, so close/reuse cannot redirect an authorized operation.
module LibExecution.HostDescriptors

open Prelude

type Entry = { fd : int; path : string; reads : bool; writes : bool }

let tokenFloor : bigint = 1I <<< 128

type Session(close : int -> Result<unit, int * string>) =
  let gate = obj ()
  let entries = System.Collections.Generic.Dictionary<bigint, Entry>()
  let mutable disposed = false

  let dispose () =
    lock gate (fun () ->
      if not disposed then
        disposed <- true
        for entry in entries.Values do
          try close entry.fd |> ignore<Result<unit, int * string>> with _ -> ()
        entries.Clear())

  member _.Gate = gate

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
      entries.Add(token, entry)
      token)

  member _.Find(token : bigint) : Option<Entry> =
    lock gate (fun () ->
      match entries.TryGetValue token with
      | true, entry -> Some entry
      | _ -> None)

  /// Consume ownership before close, including when close returns EINTR.
  member _.Consume(token : bigint) : unit =
    lock gate (fun () -> entries.Remove token |> ignore<bool>)

  member _.Dispose() = dispose ()
  override _.Finalize() = dispose ()

  interface System.IDisposable with
    member _.Dispose() = dispose ()
