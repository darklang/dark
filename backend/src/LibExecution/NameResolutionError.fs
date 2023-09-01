module LibExecution.NameResolutionError

open Prelude

module RT = RuntimeTypes
module D = DvalDecoder


type ErrorType =
  | NotFound
  | ExpectedEnumButNot
  | ExpectedRecordButNot
  | MissingModuleName
  | InvalidPackageName

type NameType =
  | Function
  | Type
  | Constant

type Error =
  { errorType : ErrorType
    nameType : NameType
    // The `.`-delimited name _parts_ e.g. `List.fakeFunction` is `["List";
    // "fakeFunction"]`
    names : List<string> }

/// to RuntimeError
module RTE =
  module ErrorType =
    let toDT (et : ErrorType) : RT.Dval =
      let nameTypeName = RT.RuntimeError.name [ "NameResolution" ] "ErrorType" 0
      let caseName =
        match et with
        | NotFound -> "NotFound"
        | ExpectedEnumButNot -> "ExpectedEnumButNot"
        | ExpectedRecordButNot -> "ExpectedRecordButNot"
        | MissingModuleName -> "MissingModuleName"
        | InvalidPackageName -> "InvalidPackageName"
      RT.Dval.enum nameTypeName caseName []

    let fromDT (dv : RT.Dval) : ErrorType =
      match dv with
      | RT.DEnum(_, _, "NotFound", []) -> NotFound
      | RT.DEnum(_, _, "ExpectedEnumButNot", []) -> ExpectedEnumButNot
      | RT.DEnum(_, _, "ExpectedRecordButNot", []) -> ExpectedRecordButNot
      | RT.DEnum(_, _, "MissingModuleName", []) -> MissingModuleName
      | RT.DEnum(_, _, "InvalidPackageName", []) -> InvalidPackageName
      | _ -> Exception.raiseInternal "Invalid ErrorType" []

  module NameType =
    let toDT (nt : NameType) : RT.Dval =
      let nameTypeName = RT.RuntimeError.name [ "NameResolution" ] "NameType" 0
      let caseName =
        match nt with
        | Function -> "Function"
        | Type -> "Type"
        | Constant -> "Constant"
      RT.Dval.enum nameTypeName caseName []

    let fromDT (dv : RT.Dval) : NameType =
      match dv with
      | RT.DEnum(_, _, "Function", []) -> Function
      | RT.DEnum(_, _, "Type", []) -> Type
      | RT.DEnum(_, _, "Constant", []) -> Constant
      | _ -> Exception.raiseInternal "Invalid NameType" []

  module Error =
    let toDT (e : Error) : RT.Dval =
      let errorTypeName = RT.RuntimeError.name [ "NameResolution" ] "Error" 0
      let fields =
        [ "errorType", ErrorType.toDT e.errorType
          "nameType", NameType.toDT e.nameType
          "names", (e.names |> List.map RT.DString |> RT.Dval.list RT.valueTypeTODO) ]

      RT.Dval.record errorTypeName fields

    let fromDT (dv : RT.Dval) : Error =
      match dv with
      | RT.DRecord(_, _, m) ->
        let errorType = m |> D.field "errorType" |> ErrorType.fromDT
        let nameType = m |> D.field "nameType" |> NameType.fromDT
        let names = m |> D.stringListField "names"

        { errorType = errorType; nameType = nameType; names = names }

      | _ -> Exception.raiseInternal "Expected DRecord" []

  let toRuntimeError (e : Error) : RT.RuntimeError =
    Error.toDT e |> RT.RuntimeError.nameResolutionError

  let fromRuntimeError (re : RT.RuntimeError) : Error =
    // TODO: this probably doesn't unwrap the type
    // see above function
    RT.RuntimeError.toDT re |> Error.fromDT
