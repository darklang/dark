module ClientTypes.Api

open Prelude

module DB =
  module Unlocked =
    type Response = { unlocked_dbs : List<tlid> }

  module StatsV1 =
    type Request = { tlids : List<tlid> }

    module Response =
      type Stat = { count : int; example : Option<Runtime.Dval.T * string> }

      type T = Map<string, Stat>


module Execution =
  module FunctionV1 =
    type Request =
      { tlid : tlid
        trace_id : System.Guid
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
        trace_id : System.Guid
        input : List<string * Runtime.Dval.T> }

    type Response = { touched_tlids : tlid list }


module F404 =
  module List =
    type Response = { f404s : List<Trace.F404> }

  module Delete =
    type Request = { space : string; path : string; modifier : string }
    type Response = { result : string }


module InitialLoad =
  module V1 =
    type Response =
      { handlers : List<Program.Handler.T>
        deletedHandlers : List<Program.Handler.T>
        dbs : List<Program.DB.T>
        deletedDBs : List<Program.DB.T>
        userFunctions : List<Program.UserFunction.T>
        deletedUserFunctions : List<Program.UserFunction.T>
        userTypes : List<Program.UserType.T>
        deletedUserTypes : List<Program.UserType.T>
        unlockedDBs : List<tlid>
        permission : Option<Authorization.Permission>
        opCtrs : Map<System.Guid, int>
        account : Authorization.UserInfo
        canvasList : List<string>
        orgs : List<string>
        orgCanvasList : List<string>
        workerSchedules : Worker.WorkerStates
        creationDate : NodaTime.Instant
        secrets : List<Secret.T> }

module Ops =
  module AddOpV1 =
    type Request = Ops.AddOpParamsV1
    type Response = Ops.AddOpResultV1


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
    type Request = { tlid : tlid; traceID : System.Guid }

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

      type Trace = System.Guid * TraceData

      type T = { trace : Trace }

  module GetAllTraces =
    type Response = { traces : List<tlid * System.Guid> }


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
    type Response = Worker.WorkerStates
