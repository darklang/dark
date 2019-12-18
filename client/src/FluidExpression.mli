type t = Types.fluidExpr

val toNexpr : ?inPipe:bool -> t -> Types.expr

val id : t -> Types.id

val find : Types.id -> t -> t option

val findParent : Types.id -> t -> t option

val isEmpty : t -> bool

val hasEmptyWithId : Types.id -> t -> bool

val isBlank : t -> bool
(* Updates *)
(* val update : f:(t -> t) -> Types.id -> t -> t *)
(* val removePipe : Types.id -> t -> int -> t *)
(* val removeListSepToken : Types.id -> t -> int -> t *)
(* val insertInList : index:int -> newExpr:t -> Types.id -> t -> t *)
(* val addBlankToList : Types.id -> t -> t *)
(* val renameVariableUses : string -> string -> t -> t *)
(* val removeVariableUse : string -> t -> t *)
