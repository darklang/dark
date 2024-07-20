module LibExecution.NameResolutionError

open Prelude

module RT = RuntimeTypes
module VT = RT.ValueType
//module D = DvalDecoder
//module RT2DT = RuntimeTypesToDarkTypes


type ErrorType =
  | NotFound of List<string>
  | ExpectedEnumButNot of packageTypeID : uuid
  | ExpectedRecordButNot of packageTypeID : uuid
  | MissingEnumModuleName of caseName : string
  | InvalidPackageName of List<string>

type NameType =
  | Function
  | Type
  | Constant

type Error = { errorType : ErrorType; nameType : NameType }

/// to RuntimeError
module RTE =
  // module ErrorType =
  //   let typeName =
  //     RT.FQTypeName.fqPackage
  //       PackageIDs.Type.LanguageTools.RuntimeError.NameResolution.errorType

  //   let toDT (et : ErrorType) : RT.Dval =
  //     let (caseName, fields) =
  //       match et with
  //       | NotFound names ->
  //         "NotFound", [ RT.DList(VT.string, List.map RT.DString names) ]
  //       | ExpectedEnumButNot packageTypeID ->
  //         "ExpectedEnumButNot", [ RT.DUuid packageTypeID ]
  //       | ExpectedRecordButNot packageTypeID ->
  //         "ExpectedRecordButNot", [ RT.DUuid packageTypeID ]
  //       | MissingEnumModuleName caseName ->
  //         "MissingEnumModuleName", [ RT.DString caseName ]
  //       | InvalidPackageName names ->
  //         "InvalidPackageName", [ RT.DList(VT.string, List.map RT.DString names) ]

  //     RT.DEnum(typeName, typeName, [], caseName, fields)

  //   let fromDT (dv : RT.Dval) : ErrorType =
  //     let string (dv : RT.Dval) : string =
  //       match dv with
  //       | RT.DString s -> s
  //       | _ -> Exception.raiseInternal "Invalid ErrorType" []

  //     match dv with
  //     | RT.DEnum(_, _, [], "NotFound", [ RT.DList(_, names) ]) ->
  //       NotFound(List.map string names)
  //     | RT.DEnum(_, _, [], "ExpectedEnumButNot", [ RT.DUuid packageTypeID ]) ->
  //       ExpectedEnumButNot packageTypeID
  //     | RT.DEnum(_, _, [], "ExpectedRecordButNot", [ RT.DUuid packageTypeID ]) ->
  //       ExpectedRecordButNot packageTypeID
  //     | RT.DEnum(_, _, [], "MissingEnumModuleName", [ RT.DString caseName ]) ->
  //       MissingEnumModuleName caseName
  //     | RT.DEnum(_, _, [], "InvalidPackageName", [ RT.DList(_, names) ]) ->
  //       InvalidPackageName(List.map string names)
  //     | _ -> Exception.raiseInternal "Invalid ErrorType" []

  // module NameType =
  //   let typeName =
  //     RT.FQTypeName.fqPackage
  //       PackageIDs.Type.LanguageTools.RuntimeError.NameResolution.nameType
  //   let toDT (nt : NameType) : RT.Dval =
  //     let (caseName, fields) =
  //       match nt with
  //       | Function -> "Function", []
  //       | Type -> "Type", []
  //       | Constant -> "Constant", []

  //     RT.DEnum(typeName, typeName, [], caseName, fields)

  //   let fromDT (dv : RT.Dval) : NameType =
  //     match dv with
  //     | RT.DEnum(_, _, [], "Function", []) -> Function
  //     | RT.DEnum(_, _, [], "Type", []) -> Type
  //     | RT.DEnum(_, _, [], "Constant", []) -> Constant
  //     | _ -> Exception.raiseInternal "Invalid NameType" []

  // module Error =
  //   let typeName =
  //     RT.FQTypeName.fqPackage
  //       PackageIDs.Type.LanguageTools.RuntimeError.NameResolution.error
  //   let toDT (e : Error) : RT.Dval =
  //     let fields =
  //       [ ("errorType", ErrorType.toDT e.errorType)
  //         ("nameType", NameType.toDT e.nameType) ]
  //     RT.DRecord(typeName, typeName, [], Map fields)

  //   let fromDT (dv : RT.Dval) : Error =
  //     match dv with
  //     | RT.DRecord(_, _, _, m) ->
  //       let errorType = m |> D.field "errorType" |> ErrorType.fromDT
  //       let nameType = m |> D.field "nameType" |> NameType.fromDT
  //       { errorType = errorType; nameType = nameType }
  //     | _ -> Exception.raiseInternal "Expected DRecord" []

  let toRuntimeError (_e : Error) : RT.RuntimeError =
    //Error.toDT e |> RT.RuntimeError.nameResolutionError
    "TODO" |> RT.RuntimeError.oldError

  // let fromRuntimeError (re : RT.RuntimeError) : Error =
  //   // TODO: this probably doesn't unwrap the type
  //   // see above function
  //   RT.RuntimeError.toDT re |> Error.fromDT
