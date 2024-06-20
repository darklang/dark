module LibExecution.CommonToDarkTypes

open RuntimeTypes

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
