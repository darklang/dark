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

/// A trace id is a plain random UUID. Nothing reads order or time out of it: every listing
/// orders by `timestamp` (indexed) or `rowid`.
///
/// Random matters because a trace id is a thing a PERSON types -- `traces resume`, `traces
/// values` and `traces fork` all take one -- and the CLI takes the shortest prefix that is
/// unique. Anything with structure in front (a timestamp, say) makes two runs from the same
/// moment agree for a dozen characters and every short id ambiguous. Random gives eight good
/// characters, the way a process id already does.
///
/// If traces ever sync between instances, this is the thing to revisit: a content hash would
/// make two instances that recorded the same run agree on its id.
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
