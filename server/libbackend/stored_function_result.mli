open Core


val store :
  Uuid.t * string * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load :
  Uuid.t * string * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  (Types.RuntimeT.dval * Time.t) option
