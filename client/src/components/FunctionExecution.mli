type t = Types.functionExecution [@@deriving show]

type apiParams = Types.executeFunctionAPIParams [@@deriving show]

type apiResult = Types.executeFunctionAPIResult [@@deriving show]

type msg = Types.functionExecutionMsg [@@deriving show]

(* Returns the ids being executed within [tlid] *)
val withinTLID : TLID.t -> t -> ID.t list

val update : msg -> t -> t * msg CrossComponentMsg.t list

(* | FunctionExecutionMsg (ExecuteFunctionButton (tlid, id, name)) -> *)
(*     let selectionTarget : tlidSelectTarget = *)
(* Note that the intent here is to make the live value visible, which
 * is a side-effect of placing the caret right after the function name
 * in the handler where the function is being called.  We're relying on
 * the length of the function name representing the offset into the
 * tokenized function call node corresponding to this location. Eg:
 * foo|v1 a b *)
(*       STCaret {astRef = ARFnCall id; offset = String.length name} *)
(*     in *)
(*     Many *)
(*       [ Model.updateFunctionExecutionMod *)
(*           (FunctionExecution.recordExecutionStart tlid id) *)
(*       ; ExecutingFunctionAPICall (tlid, id, name) *)
(*       ; Select (tlid, selectionTarget) ] *)
(* | FunctionExecutionMsg (ExecuteFromWithin p) -> *)
(*     Many *)
(*       [ Model.updateFunctionExecutionMod *)
(*           (FunctionExecution.recordExecutionStart p.tlid p.callerID) *)
(*       ; MakeCmd (API.executeFunction m p) ] *)
(*  *)
