/// Types used during program analysis/traces
module LibExecution.AnalysisTypes

open Prelude

module RT = RuntimeTypes

// --------------------
// Dval store - save per-tl analysis results
// --------------------
type ExecutionResult =
  | ExecutedResult of RT.Dval
  | NonExecutedResult of RT.Dval

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
      | TraceID (guid) -> guid.ToString()


  /// Create a new TraceID. All traceIDs should be created using this unless they
  /// already exist (in which case they should have been created with this in the
  /// first place). This has some exceptions such as old traces and default traces,
  /// but we should be able to get away with those cases.
  let create () : T =
    // We follow ULIDs, which have 48 bits of timestamp (milliseconds since the
    // epoch) and 80 bits of randomness. Unlike ULIDs, we don't require monotonically
    // increasing values in the same millisecond, so we ignore that part.  We invert
    // the timestamp so that it's lexicographically ordered in reverse (that is, when
    // we sort it lexicographically, ther most recent traces will be first).
    let bytes = Array.zeroCreate<byte> 16
    let timestampSpan = System.Span<byte>(bytes, 0, 6)
    let randomSpan = System.Span<byte>(bytes, 6, 10)

    // Timestamp part
    let timestamp = (NodaTime.Instant.now ()).ToUnixTimeMilliseconds()
    let timestamp = ~~~timestamp

    // When we convert to a guid, its internal format means it reads things a bit
    // funny - first an in32, then an int16, another int16, then 8 bytes.  We want to
    // ensure the timestamp is in the right place so we write to the appropriate
    // index for the guid to mess with it.
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

  let toUUID (t : T) : System.Guid =
    match t with
    | TraceID g -> g

  let fromUUID (g : System.Guid) : T = TraceID g


type TraceData =
  { input : InputVars
    timestamp : NodaTime.Instant
    function_results : List<FunctionResult> }

type Trace = TraceID.T * TraceData

type AnalysisRequest =
  { requestID : int
    requestTime : NodaTime.Instant
    tlid : tlid
    traceID : TraceID.T
    traceData : TraceData
    userFns : List<RT.UserFunction.T>
    userTypes : List<RT.UserType.T>
    dbs : List<RT.DB.T>
    expr : RT.Expr
    packageFns : List<RT.Package.Fn>
    secrets : List<RT.Secret.T> }

type AnalysisResults = System.Collections.Generic.Dictionary<id, ExecutionResult>
