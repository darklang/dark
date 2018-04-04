open Core


val store :
  string * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load :
  string * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval option
