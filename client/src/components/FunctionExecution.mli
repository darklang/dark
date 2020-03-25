type t = Types.functionExecution [@@deriving show]

type apiParams = Types.executeFunctionAPIParams [@@deriving show]

type apiResult = Types.executeFunctionAPIResult [@@deriving show]

type msg = Types.functionExecutionMsg [@@deriving show]

(* Returns the ids being executed within [tlid] *)
val withinTLID : TLID.t -> t -> ID.t list

val update : msg -> t -> t * msg CrossComponentMsg.t
