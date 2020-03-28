type t = Types.FunctionExecutionT.t [@@deriving show]

type apiParams = Types.FunctionExecutionT.apiParams [@@deriving show]

type apiResult = Types.FunctionExecutionT.apiResult [@@deriving show]

type msg = Types.FunctionExecutionT.msg [@@deriving show]

type effect =
  | TraceUpdateFunctionResult of
      { tlid : TLID.t
      ; traceID : Types.traceID
      ; callerID : ID.t
      ; fnName : string
      ; hash : string
      ; hashVersion : int
      ; dval : Types.dval }
  | OverrideTraces of Types.traces
  | SetUnlockedDBs of Tc.StrSet.t
  | HandleAPIError of Types.apiError
  | MakeAPICall of
      { body : Js.Json.t
      ; callback : (Js.Json.t, string Tea.Http.error) Tea.Result.t -> Types.msg
      ; endpoint : string }
  | Select of TLID.t * Types.tlidSelectTarget

(* Returns the ids being executed within [tlid] *)
val withinTLID : TLID.t -> t -> ID.t list

val update : msg -> t -> t * effect list
