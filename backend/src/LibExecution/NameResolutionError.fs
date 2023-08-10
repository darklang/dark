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

// to RuntimeError
module RTE =
  module ErrorType =
    let toDT (et : ErrorType) =
      let nameTypeName = RT.RuntimeError.name [ "NameResolution" ] "ErrorType" 0
      let caseName =
        match et with
        | NotFound -> "NotFound"
        | ExpectedEnumButNot -> "ExpectedEnumButNot"
        | ExpectedRecordButNot -> "ExpectedRecordButNot"
        | MissingModuleName -> "MissingModuleName"
        | InvalidPackageName -> "InvalidPackageName"
      RT.Dval.enum nameTypeName caseName []

  module NameType =
    let toDT (nt : NameType) =
      let nameTypeName = RT.RuntimeError.name [ "NameResolution" ] "NameType" 0
      let caseName =
        match nt with
        | Function -> "Function"
        | Type -> "Type"
        | Constant -> "Constant"
      RT.Dval.enum nameTypeName caseName []

  module Error =
    let toDT (e : Error) : RT.Dval =
      let errorTypeName = RT.RuntimeError.name [ "NameResolution" ] "Error" 0
      let fields =
        [ "errorType", ErrorType.toDT e.errorType
          "nameType", NameType.toDT e.nameType
          "names", (e.names |> List.map RT.DString |> RT.DList) ]

      RT.Dval.record errorTypeName fields

    let fromDT (dv : RT.Dval) : Error =
      match dv with
      | RT.DRecord(_, _, m) ->
        let errorType = m |> D.field "errorType" |> ErrorType.fromDT
        let nameType = m |> D.field "nameType" |> NameType.fromDT
        let names = m |> D.field "names" |> D.stringList |> RT.DList

        { errorType = errorType; nameType = nameType; names = names }

      | _ -> Exception.raiseInternal "Expected DRecord" []

  let toRuntimeError (e : Error) : RT.RuntimeError =
    Error.toDT e |> RT.RuntimeError.fromDT
