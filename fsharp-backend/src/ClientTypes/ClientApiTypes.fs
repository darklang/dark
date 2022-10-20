module ClientTypes.Api

open Prelude


module DB =
  module Unlocked =
    type Response = { unlocked_dbs : tlid list }

  module StatsV1 =
    type Request = { tlids : tlid list }

    module Response =
      type Stat = { count : int; example : Option<Runtime.Dval.T * string> }

      type T = Map<string, Stat>


module Execution =
  module FunctionV1 =
    type Request =
      { tlid : tlid
        trace_id : Analysis.TraceID
        caller_id : id
        args : Runtime.Dval.T list
        fnname : string }

    type Response =
      { result : Runtime.Dval.T
        hash : string
        hashVersion : int
        touched_tlids : tlid list
        unlocked_dbs : tlid list }


  module HandlerV1 =
    type Request =
      { tlid : tlid
        trace_id : Analysis.TraceID
        input : List<string * Runtime.Dval.T> }

    type Response = { touched_tlids : tlid list }


module F404 =
  module List =
    type Response = { f404s : List<Trace.F404> }

  module Delete =
    type Request = { space : string; path : string; modifier : string }

    type Response = { result : string }


module Packages =
  module ListV1 =
    type Response = List<Program.Package.Fn>


module Secrets =
  type Secret = { name : string; value : string }

  module DeleteV1 =
    type Request = { name : string }
    type Response = { secrets : List<Secret> }

  module InsertV1 =
    type Request = Secret
    type Response = { secrets : List<Secret> }


module Toplevels =
  module Delete =
    type Request = { tlid : tlid }

    type Response = { result : string }


module Traces =
  module GetTraceDataV1 =
    type Request = { tlid : tlid; traceID : Analysis.TraceID }

    module Response =
      type InputVars = List<string * Runtime.Dval.T>
      type FunctionArgHash = string
      type HashVersion = int
      type FnName = string

      type FunctionResult =
        FnName * id * FunctionArgHash * HashVersion * Runtime.Dval.T

      type TraceData =
        { input : InputVars
          timestamp : NodaTime.Instant
          functionResults : List<FunctionResult> }

      type Trace = Analysis.TraceID * TraceData

      type T = { trace : Trace }

  module GetAllTraces =
    type Request = unit
    type Response = { traces : List<tlid * Analysis.TraceID> }


module Tunnels =
  module Register =
    type Request = { tunnelHost : Option<string> }

    type Response = { success : bool }


module Workers =
  module WorkerStats =
    type Request = { tlid : tlid }

    type Response = { count : int }

  module Scheduler =
    // TODO: pull in response type
    type Request = { name : string; schedule : string }
