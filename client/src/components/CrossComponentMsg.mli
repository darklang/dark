type 'msg t =
  | CCMMany of 'msg t list
  | CCMTraceUpdateFunctionResult of
      { tlid : TLID.t
      ; traceID : Types.traceID
      ; callerID : ID.t
      ; fnName : string
      ; hash : string
      ; hashVersion : int
      ; dval : Types.dval }
  | CCMTraceOverrideTraces of Types.traces
  | CCMUnlockedDBsSetUnlocked of Tc.StrSet.t
  | CCMHandleAPIError of Types.apiError
  | CCMMakeAPICall of
      { body : Js.Json.t
      ; callback : (Js.Json.t, string Tea.Http.error) Tea.Result.t -> 'msg
      ; endpoint : string }
  | CCMSelect of TLID.t * Types.tlidSelectTarget
  | CCMNothing

(* Convert a [t] of one message type into a Types.msg *)
val map : f:('msg -> Types.msg) -> 'msg t -> Types.msg t
