type t = Types.fluidToken

val fakeid : Types.id

val id : t -> Types.id

val validID : Types.id -> bool

val analysisID : t -> Types.id

val parentExprID : t -> Types.id

val isAppendable : t -> bool

val isAtom : t -> bool

val isAutocompletable : t -> bool

val isBlank : t -> bool

val isErrorDisplayable : t -> bool

val isLet : t -> bool

val isTextToken : t -> bool

val isStringToken : t -> bool

val isSkippable : t -> bool

val isNewline : t -> bool

val matches : t -> t -> bool

val toCssClasses : t -> string list

val toDebugInfo : t -> string

val toText : t -> string

val toTestText : t -> string

val toTypeName : t -> string
