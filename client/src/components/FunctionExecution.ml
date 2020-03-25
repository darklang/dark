open Prelude

type t = Types.functionExecution [@@deriving show]

type apiParams = Types.executeFunctionAPIParams [@@deriving show]

type apiResult = Types.executeFunctionAPIResult [@@deriving show]

type msg = Types.functionExecutionMsg [@@deriving show]

let withinTLID tlid t =
  List.filterMap t ~f:(fun (tlid_, id) ->
      if tlid_ = tlid then Some id else None)


let recordExecutionEnd tlid id t = List.filter t ~f:(( <> ) (tlid, id))

let recordExecutionStart tlid id t =
  t |> recordExecutionEnd tlid id |> ( @ ) [(tlid, id)]


let update (msg : msg) (t : t) : t * msg CrossComponentMsg.t =
  match msg with
  | FunctionExecutionExecuteFunction p ->
      ( recordExecutionStart p.efpTLID p.efpCallerID t
      , CCMMakeAPICall
          { endpoint = "/execute_function"
          ; body = Encoders.executeFunctionAPIParams p
          ; callback =
              (fun result ->
                Types.FunctionExecutionAPICallback
                  ( p
                  , match result with
                    | Ok js ->
                        Ok (Decoders.executeFunctionAPIResult js)
                    | Error v ->
                        Error v )) } )
  | FunctionExecutionAPICallback
      (p, Ok (dval, hash, hashVersion, tlids, unlockedDBs)) ->
      let traces =
        List.map
          ~f:(fun tlid ->
            (TLID.toString tlid, [(p.efpTraceID, Error Types.NoneYet)]))
          tlids
      in
      ( recordExecutionEnd p.efpTLID p.efpCallerID t
      , CCMMany
          [ CrossComponentMsg.CCMTraceUpdateFunctionResult
              { tlid = p.efpTLID
              ; traceID = p.efpTraceID
              ; callerID = p.efpCallerID
              ; fnName = p.efpFnName
              ; hash
              ; hashVersion
              ; dval }
          ; CCMTraceOverrideTraces (StrDict.fromList traces)
          ; CCMUnlockedDBsSetUnlocked unlockedDBs ] )
  | FunctionExecutionAPICallback (p, Error err) ->
      ( recordExecutionEnd p.efpTLID p.efpCallerID t
      , CCMHandleAPIError
          { context = "ExecuteFunction"
          ; importance = ImportantError
          ; requestParams = Some (Encoders.executeFunctionAPIParams p)
          ; reload = false
          ; originalError = err } )
