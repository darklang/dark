module LibExecution.DvalUtils

open LibExecution.RuntimeTypes

// Creating Dvals


// Extracting data from Dvals

let asList (dv : Dval) : Option<List<Dval>> =
  match dv with
  | DList(_, l) -> Some l
  | _ -> None

let asDict (dv : Dval) : Option<Map<string, Dval>> =
  match dv with
  | DDict d -> Some d
  | _ -> None

let asTuple2 (dv : Dval) : Option<Dval * Dval> =
  match dv with
  | DTuple(first, second, _) -> Some(first, second)
  | _ -> None

let asTuple3 (dv : Dval) : Option<Dval * Dval * Dval> =
  match dv with
  | DTuple(first, second, [ third ]) -> Some(first, second, third)
  | _ -> None

let asString (dv : Dval) : Option<string> =
  match dv with
  | DString s -> Some s
  | _ -> None

let asInt (dv : Dval) : Option<int64> =
  match dv with
  | DInt i -> Some i
  | _ -> None

let asFloat (dv : Dval) : Option<double> =
  match dv with
  | DFloat f -> Some f
  | _ -> None

let asBool (dv : Dval) : Option<bool> =
  match dv with
  | DBool b -> Some b
  | _ -> None

let asUuid (dv : Dval) : Option<System.Guid> =
  match dv with
  | DUuid u -> Some u
  | _ -> None
