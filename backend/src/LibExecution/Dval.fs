/// Simple pass-through functions for creating Dvals
module LibExecution.Dval

open Prelude

open LibExecution.RuntimeTypes
module VT = ValueType


let int8 (i : int8) = DInt8 i
let uint8 (i : uint8) = DUInt8 i
let int16 (i : int16) = DInt16 i
let uint16 (i : uint16) = DUInt16 i
let int32 (i : int32) = DInt32 i
let uint32 (i : uint32) = DUInt32 i
let int64 (i : int64) = DInt64 i
let uint64 (i : uint64) = DUInt64 i
let int128 (i : System.Int128) = DInt128 i
let uint128 (i : System.UInt128) = DUInt128 i

let string (s : string) = DString s

let uuid (s : System.Guid) = DUuid s

let list (typ : KnownType) (list : List<Dval>) : Dval = DList(VT.known typ, list)

let dict (typ : KnownType) (entries : List<string * Dval>) : Dval =
  DDict(VT.known typ, Map entries)

let dictFromMap (typ : KnownType) (entries : Map<string, Dval>) : Dval =
  DDict(VT.known typ, entries)


let optionType () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.option ())

let optionSome (innerType : KnownType) (dv : Dval) : Dval =
  DEnum(optionType (), optionType (), [ VT.known innerType ], "Some", [ dv ])

let optionNone (innerType : KnownType) : Dval =
  DEnum(optionType (), optionType (), [ VT.known innerType ], "None", [])

let option (innerType : KnownType) (dv : Option<Dval>) : Dval =
  match dv with
  | Some dv -> optionSome innerType dv
  | None -> optionNone innerType



let resultType () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.result ())


let resultOk (okType : KnownType) (errorType : KnownType) (dvOk : Dval) : Dval =
  DEnum(
    resultType (),
    resultType (),
    [ ValueType.Known okType; ValueType.Known errorType ],
    "Ok",
    [ dvOk ]
  )

let resultError
  (okType : KnownType)
  (errorType : KnownType)
  (dvError : Dval)
  : Dval =

  DEnum(
    resultType (),
    resultType (),
    [ ValueType.Known okType; ValueType.Known errorType ],
    "Error",
    [ dvError ]
  )

let result
  (okType : KnownType)
  (errorType : KnownType)
  (dv : Result<Dval, Dval>)
  : Dval =
  match dv with
  | Ok dv -> resultOk okType errorType dv
  | Error dv -> resultError okType errorType dv


/// Mint a fresh ephemeral blob: register the bytes in the exeState's
/// blob store and return a DBlob that references them. Caller retains
/// no direct handle on the byte[] past this point.
let newEphemeralBlob (exeState : ExecutionState) (bytes : byte[]) : Dval =
  let id = System.Guid.NewGuid()
  exeState.blobStore[id] <- bytes
  DBlob(Ephemeral id)


/// Resolve a BlobRef to its bytes. Ephemerals read from the VM store;
/// persistent refs hit package_blobs via the PM.
let readBlobBytes
  (exeState : ExecutionState)
  (pm : PackageManager)
  (ref : BlobRef)
  : Ply.Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral id ->
      match exeState.blobStore.TryGetValue id with
      | true, bytes -> return bytes
      | false, _ ->
        return
          Exception.raiseInternal "Ephemeral blob not found in store" [ "id", id ]
    | Persistent(hash, _length) ->
      let! bytes = pm.getBlob hash
      match bytes with
      | Some b -> return b
      | None ->
        return
          Exception.raiseInternal
            "Persistent blob not found in package_blobs"
            [ "hash", hash ]
  }


/// Mint a fresh DStream from a pull function. Single-consumer by
/// construction — the disposed flag and lock guard concurrent `next`
/// calls. See thinking/blobs-and-streams/30-phase-2.md.
let newStream (elemType : ValueType) (next : unit -> Ply.Ply<Option<Dval>>) : Dval =
  DStream(FromIO(next, elemType), ref false, obj ())


/// Pull the next element from a stream. Returns [None] when the
/// stream is exhausted; subsequent calls after exhaustion return
/// [None] (single-consumer — once drained, stays drained).
///
/// Thread-safe via the stream's monitor lock; concurrent callers
/// serialize. Chunk 2.6 adds Mapped/Filtered/Take/Concat walking.
let readStreamNext (dv : Dval) : Ply.Ply<Option<Dval>> =
  uply {
    match dv with
    | DStream(impl, disposed, lockObj) ->
      // Monitor ensures single-consumer semantics.
      let taken = System.Threading.Monitor.TryEnter(lockObj, 0)
      if not taken then
        return
          Exception.raiseInternal "stream: concurrent consumers not supported" []
      try
        if disposed.Value then
          return None
        else
          match impl with
          | FromIO(next, _elemType) ->
            let! result = next ()
            match result with
            | Some _ -> return result
            | None ->
              disposed.Value <- true
              return None
      finally
        System.Threading.Monitor.Exit(lockObj)
    | _ -> return Exception.raiseInternal "readStreamNext: expected DStream" []
  }


/// SHA-256 hex digest of the given bytes — content-addressing for
/// blob promotion. See thinking/blobs-and-streams/00-design.md.
let sha256Hex (bytes : byte[]) : string =
  use sha = System.Security.Cryptography.SHA256.Create()
  let digest = sha.ComputeHash(bytes)
  System.Convert.ToHexStringLower(digest)


/// Promote any ephemeral blobs reachable from [dv] to persistent:
/// hash the bytes (SHA-256), write them to the content-addressed
/// store via [insert], and swap the ref. Idempotent by design — the
/// insert path uses `INSERT OR IGNORE`, so promoting the same bytes
/// twice writes to `package_blobs` once.
///
/// Called before serializing a Dval through any persistence boundary
/// (val commit, User DB write, trace capture). Plain `DBlob` writers
/// (binary, JSON) still raise on ephemeral — promote first, serialize
/// second.
///
/// TODO chunk L.1: trace capture path should also call this.
let promoteBlobs
  (exeState : ExecutionState)
  (insert : string -> byte[] -> Ply.Ply<unit>)
  (dv : Dval)
  : Ply.Ply<Dval> =
  uply {
    let rec go (dv : Dval) : Ply.Ply<Dval> =
      uply {
        match dv with
        | DBlob(Ephemeral id) ->
          let mutable bs : byte[] = null
          if exeState.blobStore.TryGetValue(id, &bs) then
            let h = sha256Hex bs
            // NB: Dval.int64 is a Dval-builder in this module, so we
            // fully-qualify the int64 primitive conversion here.
            let n : int64 = System.Convert.ToInt64 bs.Length
            do! insert h bs
            return DBlob(Persistent(h, n))
          else
            return
              Exception.raiseInternal
                "Ephemeral blob not found in store during promotion"
                [ "id", id ]
        | DBlob(Persistent _)
        | DUnit
        | DBool _
        | DInt8 _
        | DUInt8 _
        | DInt16 _
        | DUInt16 _
        | DInt32 _
        | DUInt32 _
        | DInt64 _
        | DUInt64 _
        | DInt128 _
        | DUInt128 _
        | DFloat _
        | DChar _
        | DString _
        | DDateTime _
        | DUuid _
        | DApplicable _
        | DDB _
        | DStream _ -> return dv
        | DList(vt, items) ->
          let! items' = items |> Ply.List.mapSequentially go
          return DList(vt, items')
        | DDict(vt, entries) ->
          let! entries' =
            entries
            |> Map.toList
            |> Ply.List.mapSequentially (fun (k, v) ->
              uply {
                let! v' = go v
                return (k, v')
              })
          return DDict(vt, Map.ofList entries')
        | DTuple(a, b, rest) ->
          let! a' = go a
          let! b' = go b
          let! rest' = rest |> Ply.List.mapSequentially go
          return DTuple(a', b', rest')
        | DRecord(src, rt, typeArgs, fields) ->
          let! fields' =
            fields
            |> Map.toList
            |> Ply.List.mapSequentially (fun (k, v) ->
              uply {
                let! v' = go v
                return (k, v')
              })
          return DRecord(src, rt, typeArgs, Map.ofList fields')
        | DEnum(src, rt, typeArgs, caseName, fields) ->
          let! fields' = fields |> Ply.List.mapSequentially go
          return DEnum(src, rt, typeArgs, caseName, fields')
      }
    return! go dv
  }


let byteArrayToDvalList (bytes : byte[]) : Dval =
  bytes
  |> Array.toList
  |> List.map (fun b -> DUInt8(byte b))
  |> fun dvalList -> DList(VT.uint8, dvalList)

let dlistToByteArray (dvalList : List<Dval>) : byte[] =
  dvalList
  |> List.map (fun dval ->
    match dval with
    | DUInt8 b -> b
    | _ -> (Exception.raiseInternal "Invalid type in byte list") [])
  |> Array.ofList
