open Prelude
include Types.FunctionExecutionT

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

let withinTLID tlid t =
  List.filterMap t ~f:(fun (tlid_, id) ->
      if tlid_ = tlid then Some id else None)


let recordExecutionEnd tlid id t = List.filter t ~f:(( <> ) (tlid, id))

let recordExecutionStart tlid id t =
  t |> recordExecutionEnd tlid id |> ( @ ) [(tlid, id)]


let update (msg : msg) (t : t) : t * effect list =
  match msg with
  | ExecuteFunction (p, moveToCaller) ->
      let selectionTarget : tlidSelectTarget =
        (* Note that the intent here is to make the live value visible, which
         * is a side-effect of placing the caret right after the function name
         * in the handler where the function is being called.  We're relying on
         * the length of the function name representing the offset into the
         * tokenized function call node corresponding to this location. Eg:
         * foo|v1 a b *)
        STCaret {astRef = ARFnCall p.callerID; offset = String.length p.fnName}
      in
      let select =
        if moveToCaller = MoveToCaller
        then [Select (p.tlid, selectionTarget)]
        else []
      in
      ( recordExecutionStart p.tlid p.callerID t
      , [ MakeAPICall
            { endpoint = "/execute_function"
            ; body = Encoders.executeFunctionAPIParams p
            ; callback =
                (fun result ->
                  FunctionExecutionMsg
                    (APICallback
                       ( p
                       , match result with
                         | Ok js ->
                             Ok (Decoders.executeFunctionAPIResult js)
                         | Error v ->
                             Error v ))) } ]
        @ select )
  | APICallback (p, Ok (dval, hash, hashVersion, tlids, unlockedDBs)) ->
      let traces =
        List.map
          ~f:(fun tlid -> (TLID.toString tlid, [(p.traceID, Error NoneYet)]))
          tlids
      in
      ( recordExecutionEnd p.tlid p.callerID t
      , [ TraceUpdateFunctionResult
            { tlid = p.tlid
            ; traceID = p.traceID
            ; callerID = p.callerID
            ; fnName = p.fnName
            ; hash
            ; hashVersion
            ; dval }
        ; OverrideTraces (StrDict.fromList traces)
        ; SetUnlockedDBs unlockedDBs ] )
  | APICallback (p, Error err) ->
      ( recordExecutionEnd p.tlid p.callerID t
      , [ HandleAPIError
            { context = "ExecuteFunction"
            ; importance = ImportantError
            ; requestParams = Some (Encoders.executeFunctionAPIParams p)
            ; reload = false
            ; originalError = err } ] )
