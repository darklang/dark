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

module Tunnels =
  module Register =
    type Request = { tunnelHost : Option<string> }

    type Response = { success : bool }

module Workers =
  module WorkerStats =
    type Request = { tlid : tlid }

    type Response = { count : int }

  module Scheduler =
    type Request = { name : string; schedule : string }
    // TODO: pull in response type
