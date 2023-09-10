// -- Creating Dvals --
// Dvals should never be constructed that contain fakevals - the fakeval
// should always propagate (though, there are specific cases in the
// interpreter where they are discarded instead of propagated; still they are
// never put into other dvals). These static members check before creating the values.
// additionally, these functions fill in any ValueTypes relevant to a Dval,
// failing if they conflict with any expected valueType.

// Soon, some of thse are going to need a `Types` argument, which will be
// needed to look up types (i.e. for DEnum construction).
// When that happens, I suspect:
// - we'll need to pass in a `types: Types` argument to several of these fns
// - this whole module will become recursively defined

// These could just as well be renamed DvalCreators
// , but that name feels bad to me?
//
// maybe `CreateDval` or `DvalMaker` or something?
// i.e. `CreateDval.enum ...` seems reasonable-ish
module LibExecution.DvalUtils

open LibExecution.RuntimeTypes



// type DvalCreator = draft of an idea...
//   { list: List<Dval> -> Dval }

let int (i : int) = DInt(int64 i)


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

let record
  (typeName : TypeName.TypeName)
  // TODO: (typeArgs: List<ValueType>)
  (fields : List<string * Dval>)
  : Dval =
  // Give a warning for duplicate keys
  List.fold
    (fun m (k, v) ->
      match m, k, v with
      // TYPESCLEANUP: remove hacks
      // If we're propagating a fakeval keep doing it. We handle it without this line but let's be certain
      | m, _k, _v when Dval.isFake m -> m
      // Errors should propagate (but only if we're not already propagating an error)
      | DRecord _, _, v when Dval.isFake v -> v
      // Skip empty rows
      | _, "", _ -> DError(SourceNone, RuntimeError.oldError $"Empty key {k}")
      // Error if the key appears twice
      | DRecord(_, _, _typeArgsTODO, m), k, _v when Map.containsKey k m ->
        DError(SourceNone, RuntimeError.oldError $"Duplicate key: {k}")
      // Otherwise add it
      | DRecord(tn, o, _typeArgsTODO, m), k, v ->
        DRecord(tn, o, valueTypesTODO, Map.add k v m)
      // If we haven't got a DDict we're propagating an error so let it go
      | m, _, _ -> m)
    (DRecord(typeName, typeName, valueTypesTODO, Map.empty))
    fields


let enum
  (typeName : TypeName.TypeName)
  // TODO: (typeArgs: List<ValueType>)
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


let resultType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Result" ] "Result" 0

let resultOk (dv : Dval) : Dval =
  if Dval.isFake dv then dv else DEnum(resultType, resultType, "Ok", [ dv ])
let resultError (dv : Dval) : Dval =
  if Dval.isFake dv then dv else DEnum(resultType, resultType, "Error", [ dv ])

// Wraps in a Result after checking that the value is not a fakeval
let result (dv : Result<Dval, Dval>) : Dval =
  match dv with
  | Ok dv -> resultOk dv
  | Error dv -> resultError dv
