module LibExecution.DvalUtils

open LibExecution.RuntimeTypes

// Creating Dvals



let enum
  (typeName : TypeName.TypeName)
  (caseName : string)
  (fields : List<Dval>)
  : Dval =
  match List.find Dval.isFake fields with
  | Some v -> v
  | None -> DEnum(typeName, typeName, caseName, fields)



let optionType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Option" ] "Option" 0

let optionSome (dv : Dval) : Dval =
  if Dval.isFake dv then dv else DEnum(optionType, optionType, "Some", [ dv ])

let optionNone : Dval = DEnum(optionType, optionType, "None", [])

// Wraps in an Option after checking that the value is not a fakeval
let option (dv : Option<Dval>) : Dval =
  match dv with
  | Some dv -> optionSome dv // checks isFake
  | None -> optionNone

// CLEANUP - this fn was unused so I commented it out
// remove? or will it be handy?
// let dict (fields : List<string * Dval>) : Dval =
//   // Give a warning for duplicate keys
//   List.fold
//     (DDict(Map.empty))
//     (fun m (k, v) ->
//       match m, k, v with
//       // TYPESCLEANUP: remove hacks
//       // If we're propagating a fakeval keep doing it. We handle it without this line but let's be certain
//       | m, _k, _v when isFake m -> m
//       // Errors should propagate (but only if we're not already propagating an error)
//       | DDict _, _, v when isFake v -> v
//       // Skip empty rows
//       | _, "", _ -> DError(SourceNone, $"Empty key: {k}")
//       // Error if the key appears twice
//       | DDict m, k, _v when Map.containsKey k m ->
//         DError(SourceNone, $"Duplicate key: {k}")
//       // Otherwise add it
//       | DDict m, k, v -> DDict(Map.add k v m)
//       // If we haven't got a DDict we're propagating an error so let it go
//       | m, _, _ -> m)
//     fields


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
