module LibExecution.CommonToDarkTypes

open RuntimeTypes
module PackageIDs = LibExecution.PackageIDs


module Option =
  /// Convert a Darklang Option<'a> (DEnum) to F# Option<'a>
  let fromDT (f : Dval -> 'a) (d : Dval) : Option<'a> =
    match d with
    | DEnum(FQTypeName.Package id, _, _, "Some", [ value ]) when
      id = PackageIDs.Type.Stdlib.option
      ->
      Some(f value)

    | DEnum(FQTypeName.Package id, _, _, "None", []) when
      id = PackageIDs.Type.Stdlib.option
      ->
      None

    | _ -> Exception.raiseInternal "Invalid Option" [ "dval", d ]


module Result =
  let toDT
    (okType : KnownType)
    (errorType : KnownType)
    (r : Result<'a, 'b>)
    (f : 'a -> Dval)
    (errToDTFn : 'b -> Dval)
    : Dval =
    match r with
    | Ok v -> Dval.resultOk okType errorType (f v)
    | Error err -> Dval.resultError okType errorType (errToDTFn err)


  let fromDT (f : Dval -> 'a) (d : Dval) (errToDTFn : Dval -> 'b) : Result<'a, 'b> =
    match d with
    | DEnum(tn, _, _typeArgsDEnumTODO, "Ok", [ v ]) when tn = Dval.resultType ->
      Ok(f v)

    | DEnum(tn, _, _typeArgsDEnumTODO, "Error", [ v ]) when tn = Dval.resultType ->
      Error(errToDTFn v)

    | _ -> Exception.raiseInternal "Invalid Result" []
