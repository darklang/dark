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

let rec map ~(f : 'msg -> Types.msg) (msg : 'msg t) : Types.msg t =
  match msg with
  | CCMMakeAPICall {body; callback; endpoint} ->
      CCMMakeAPICall
        {body; endpoint; callback = (fun result -> f (callback result))}
  | CCMMany msgs ->
      CCMMany (Tc.List.map ~f:(fun msg -> map ~f msg) msgs)
  | CCMTraceUpdateFunctionResult x ->
      CCMTraceUpdateFunctionResult x
  | CCMHandleAPIError apiError ->
      CCMHandleAPIError apiError
  | CCMTraceOverrideTraces x ->
      CCMTraceOverrideTraces x
  | CCMUnlockedDBsSetUnlocked x ->
      CCMUnlockedDBsSetUnlocked x
  | CCMSelect (tlid, target) ->
      CCMSelect (tlid, target)
  | CCMNothing ->
      CCMNothing
