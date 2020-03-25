open Prelude
include Types.FunctionExecutionT

let withinTLID tlid t =
  List.filterMap t ~f:(fun (tlid_, id) ->
      if tlid_ = tlid then Some id else None)


let recordExecutionEnd tlid id t = List.filter t ~f:(( <> ) (tlid, id))

let recordExecutionStart tlid id t =
  t |> recordExecutionEnd tlid id |> ( @ ) [(tlid, id)]


let teaResult2TcResult (r : ('a, 'b) Tea.Result.t) : ('b, 'a) Tc.Result.t =
  match r with Ok v -> Ok v | Error e -> Error e


let tcResult2TeaResult (r : ('a, 'b) Tc.Result.t) : ('b, 'a) Tea.Result.t =
  match r with Ok v -> Ok v | Error e -> Error e


let update (msg : msg) (t : t) : t * modification =
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
        then Select (p.tlid, selectionTarget)
        else NoChange
      in
      ( recordExecutionStart p.tlid p.callerID t
      , Many
          [ ReplaceAllModificationsWithThisOne
              (fun m ->
                let body = Encoders.executeFunctionAPIParams p in
                let endpoint = "/execute_function" in
                let callback result =
                  FunctionExecutionMsg
                    (APICallback
                       ( p
                       , result
                         |> teaResult2TcResult
                         |> Tc.Result.map Decoders.executeFunctionAPIResult
                         |> tcResult2TeaResult ))
                in
                (m, API.apiCallDirect m ~body ~callback endpoint))
          ; select ] )
  | APICallback (p, Ok (dval, hash, hashVersion, tlids, unlockedDBs)) ->
      let traces =
        List.map
          ~f:(fun tlid -> (TLID.toString tlid, [(p.traceID, Error NoneYet)]))
          tlids
      in
      ( recordExecutionEnd p.tlid p.callerID t
      , Many
          [ UpdateTraceFunctionResult
              (p.tlid, p.traceID, p.callerID, p.fnName, hash, hashVersion, dval)
          ; OverrideTraces (StrDict.fromList traces)
          ; SetUnlockedDBs unlockedDBs ] )
  | APICallback (p, Error err) ->
      ( recordExecutionEnd p.tlid p.callerID t
      , HandleAPIError
          { context = "ExecuteFunction"
          ; importance = ImportantError
          ; requestParams = Some (Encoders.executeFunctionAPIParams p)
          ; reload = false
          ; originalError = err } )
