val focusItem : int -> Types.msg Tea.Cmd.t

type t = Types.fluidAutocompleteState [@@ppx.deriving show]

type item = Types.fluidAutocompleteItem [@@ppx.deriving show]

type data = Types.fluidAutocompleteData [@@ppx.deriving show]

type query = TLID.t * Types.fluidTokenInfo [@@ppx.deriving show]

type props = {functions : Types.functionsType}

val asName : item -> string

val asTypeStrings : item -> string list * string

val isVariable : item -> bool

val isField : item -> bool

val isFnCall : item -> bool

val isCreateFn : item -> bool

val item : data -> item

val highlightedWithValidity : t -> data option

val highlighted : t -> item option

val init : t

val regenerate : Types.model -> t -> TLID.t * Types.fluidTokenInfo -> t

val numCompletions : t -> int

val selectUp : t -> t

val selectDown : t -> t

val documentationForItem :
  Types.fluidAutocompleteData -> Types.msg Vdom.t list option

val isOpened : t -> bool

val updateAutocompleteVisibility : Types.model -> Types.model

(* only exposed for tests *)
type fullQuery =
  { tl : Types.toplevel
  ; ti : Types.fluidTokenInfo
  ; fieldList : string list
  ; pipedDval : Types.dval option
  ; queryString : string }

val refilter : props -> fullQuery -> t -> item list -> t
