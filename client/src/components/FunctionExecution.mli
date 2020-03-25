type t = Types.FunctionExecutionT.t [@@deriving show]

type apiParams = Types.FunctionExecutionT.apiParams [@@deriving show]

type apiResult = Types.FunctionExecutionT.apiResult [@@deriving show]

type msg = Types.FunctionExecutionT.msg [@@deriving show]

(* Returns the ids being executed within [tlid] *)
val withinTLID : TLID.t -> t -> ID.t list

val update : msg -> t -> t * msg CrossComponentMsg.t
