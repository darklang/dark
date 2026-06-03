/// PackageCaps — content-addressed cache of a package fn's effective capabilities.
///
/// A fn's effective caps are a pure function of its content hash, so we compute once and store forever
/// (the CLI runs cold per command, so an in-memory memo wouldn't survive). `effectiveSpecs` is the
/// read-through entry point: cache hit ⇒ one lookup; miss ⇒ walk the call graph, fold, store. Specs are
/// newline-joined (`''` = pure); the `package_caps` table lives in `schema.sql`.
module LibDB.PackageCaps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module Cap = LibExecution.Capabilities
module Analysis = LibExecution.CapabilityAnalysis
module PMPT = LibDB.ProgramTypes
module CapBin = LibSerialization.Binary.Serializers.Capabilities


// The cache stores the STRUCTURED caps (the reflection-free binary form, base64'd into the text column)
// — F# never renders the grant-spec language. The `package_caps` row is a pure function of the hash, so
// kill-and-fill is safe (a stale/old-format row just gets recomputed).
let private toB64 (caps : Cap.Capabilities) : string =
  use ms = new System.IO.MemoryStream()
  use w = new System.IO.BinaryWriter(ms)
  CapBin.write w caps
  w.Flush()
  System.Convert.ToBase64String(ms.ToArray())

let private fromB64 (s : string) : Cap.Capabilities =
  use ms = new System.IO.MemoryStream(System.Convert.FromBase64String s)
  use r = new System.IO.BinaryReader(ms)
  CapBin.read r


/// The cached effective caps for a fn hash, or None if not computed yet.
let get (hash : string) : Task<Option<Cap.Capabilities>> =
  task {
    let! rows =
      Sql.query "SELECT caps FROM package_caps WHERE hash = @h"
      |> Sql.parameters [ "h", Sql.string hash ]
      |> Sql.executeAsync (fun read -> read.string "caps")

    match rows with
    | caps :: _ -> return Some(fromB64 caps)
    | [] -> return None
  }


/// Cache a fn's computed caps. Write-once (re-inserting the same hash is a no-op).
let put (hash : string) (caps : Cap.Capabilities) : Task<unit> =
  Sql.query
    "INSERT INTO package_caps (hash, caps) VALUES (@h, @c) ON CONFLICT(hash) DO NOTHING"
  |> Sql.parameters [ "h", Sql.string hash; "c", Sql.string (toB64 caps) ]
  |> Sql.executeStatementAsync


/// Pre-load every package fn reachable from `root` (the async part), returning a lookup for them — so
/// the capability fold over the call graph can then run synchronously.
let private loadClosure
  (root : PT.Hash)
  : Ply<PT.Hash -> Option<PT.PackageFn.PackageFn>> =
  uply {
    let loaded =
      System.Collections.Generic.Dictionary<PT.Hash, PT.PackageFn.PackageFn>()
    let visited = System.Collections.Generic.HashSet<PT.Hash>()

    let rec load (h : PT.Hash) : Ply<unit> =
      uply {
        if not (visited.Contains h) then
          visited.Add h |> ignore<bool>

          match! PMPT.Fn.get h with
          | None -> ()
          | Some fn ->
            loaded[h] <- fn

            let callees =
              Analysis.fnRefs fn.body
              |> List.choose (fun fqfn ->
                match fqfn with
                | PT.FQFnName.Package p -> Some p
                | PT.FQFnName.Builtin _ -> None)

            for callee in callees do
              do! load callee
      }

    do! load root

    return
      fun h ->
        match loaded.TryGetValue h with
        | true, fn -> Some fn
        | false, _ -> None
  }


/// The effective (transitive) caps of a package fn — read-through the cache. `capsFor` looks up a
/// builtin's declared caps by flat name (the caller builds it from the runtime's builtin map).
let effectiveCaps
  (capsFor : string -> Cap.Capabilities)
  (hashStr : string)
  : Ply<Cap.Capabilities> =
  uply {
    match! get hashStr with
    | Some caps -> return caps
    | None ->
      let root = PT.Hash hashStr
      let! getFn = loadClosure root
      let caps = Analysis.effectiveCapsOfFn capsFor getFn root
      do! put hashStr caps
      return caps
  }
