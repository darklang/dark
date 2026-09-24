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

/// A trace id is a plain random UUID.
///
/// It used to carry an inverted millisecond timestamp in its first six bytes, so that sorting
/// ids lexicographically sorted traces newest-first. That was for Google Cloud Storage, which
/// could only list keys in lexicographic order; the store is SQLite now and every listing
/// orders by `timestamp` (indexed) or `rowid`, so nothing reads order out of the id.
///
/// What the old shape cost, once a trace id became a thing a PERSON types -- `traces resume`,
/// `traces values`, `traces fork` all take one -- is that two runs made in the same
/// millisecond agreed for a dozen characters, so short ids came back ambiguous. Random gives
/// eight characters that are as good as unique, the way a process id already is.
module TraceID =
  [<Struct>]
  type T =
    | TraceID of System.Guid

    override this.ToString() : string =
      match this with
      | TraceID(guid) -> guid.ToString()

  let create () : T = TraceID(System.Guid.NewGuid())

  let toUUID (t : T) : System.Guid =
    match t with
    | TraceID g -> g

  let fromUUID (g : System.Guid) : T = TraceID g


type TraceData = { input : InputVars; functionResults : List<FunctionResult> }

type Trace = TraceID.T * TraceData
