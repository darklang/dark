/// Types used during program analysis/traces
module LibExecution.AnalysisTypes

open Prelude

module RT = RuntimeTypes

// --------------------
// Analysis result
// --------------------
type InputVars = List<string * RT.Dval>

type FunctionArgHash = string
type HashVersion = int
type FnName = string
type FunctionResult = FnName * id * FunctionArgHash * HashVersion * RT.Dval

/// TraceIDs are a UUID with a very specific structure, that is designed so that when
/// it is lexicographically sorted, that the sort order is reverse chronological.
/// This allows us to sort traces by time in the Google Cloud Storage, which only
/// supports lexicographical sorting. It also allows us to omit timestamps in the DB
/// and use trace ordering and indexing. Traces are also stored in the index better
/// (as in, more compactly) as a result.
module TraceID =
  [<Struct>]
  type T =
    | TraceID of System.Guid

    override this.ToString() : string =
      match this with
      | TraceID(guid) -> guid.ToString()

  let fromTimestamp (timestamp : NodaTime.Instant) : T =
    // We follow ULIDs, which have 48 bits of timestamp (milliseconds since the
    // epoch) and 80 bits of randomness. Unlike ULIDs, we don't require monotonically
    // increasing values in the same millisecond, so we ignore that part.  We invert
    // the timestamp so that it's lexicographically ordered in reverse (that is, when
    // we sort it lexicographically, the most recent traces will be first).
    let bytes = Array.zeroCreate<byte> 16
    let timestampSpan = System.Span<byte>(bytes, 0, 6)
    let randomSpan = System.Span<byte>(bytes, 6, 10)

    // Timestamp part
    let timestamp = timestamp.ToUnixTimeMilliseconds()
    let timestamp = ~~~timestamp

    // System.Guid has an odd internal format: first an int32, then an int16, another
    // int16, then 8 bytes. We need to be careful if we want the bits to end up in
    // the right places, hence the odd indexes below (eg the int32 is read as 4
    // bytes at once from the byte span, so we need to put the bytes in in reverse).
    timestampSpan[3] <- byte ((timestamp >>> 40) &&& 0xFFL)
    timestampSpan[2] <- byte ((timestamp >>> 32) &&& 0xFFL)
    timestampSpan[1] <- byte ((timestamp >>> 24) &&& 0xFFL)
    timestampSpan[0] <- byte ((timestamp >>> 16) &&& 0xFFL)
    timestampSpan[5] <- byte ((timestamp >>> 8) &&& 0xFFL)
    timestampSpan[4] <- byte ((timestamp >>> 0) &&& 0xFFL)

    // Random part
    // The first two bytes here get rewritten (reversed) by the Guid constructor, but
    // it doesn't matter since this is random anyway
    System.Security.Cryptography.RandomNumberGenerator.Fill(randomSpan)

    TraceID(System.Guid(bytes))


  /// Create a new TraceID. All traceIDs should be created using this unless they
  /// already exist (in which case they should have been created with this in the
  /// first place). This has some exceptions such as old traces and default traces,
  /// but we should be able to get away with those cases.
  let create () : T = fromTimestamp (NodaTime.Instant.now ())

  let toUUID (t : T) : System.Guid =
    match t with
    | TraceID g -> g

  let fromUUID (g : System.Guid) : T = TraceID g

  let toTimestamp (traceID : T) : NodaTime.Instant =
    let bytes = (toUUID traceID).ToByteArray()
    let timestamp : uint64 =
      0xffff000000000000UL
      // See fromTimestamp for the format
      ||| (uint64 bytes[3] <<< 40)
      ||| (uint64 bytes[2] <<< 32)
      ||| (uint64 bytes[1] <<< 24)
      ||| (uint64 bytes[0] <<< 16)
      ||| (uint64 bytes[5] <<< 8)
      ||| (uint64 bytes[4] <<< 0)
    timestamp |> (~~~) |> int64 |> NodaTime.Instant.FromUnixTimeMilliseconds


type TraceData = { input : InputVars; functionResults : List<FunctionResult> }

type Trace = TraceID.T * TraceData


// TODO: this isn't currently used - figure out what to do here
// type AnalysisRequest =
//   { requestID : int
//     requestTime : NodaTime.Instant
//     tlid : tlid
//     traceID : TraceID.T
//     traceData : TraceData
//     dbs : List<RT.DB.T>
//     expr : RT.Expr
//     packageFns : List<RT.PackageFn.T>
//     packageTypes : List<RT.PackageType.T>
//     packageConstants : List<RT.PackageConstant.T>
//     secrets : List<RT.Secret.T> }

// type AnalysisResults = System.Collections.Generic.Dictionary<id, RT.Dval>
