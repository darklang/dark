module ProgramTypes2DarkTypes

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

let ptTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : FQTypeName.T =
  FQTypeName.packageTypeName'
    "Darklang"
    (NonEmptyList.ofList ([ "Stdlib"; "ProgramTypes" ] @ submodules))
    name
    version
  |> FQTypeName.Package


module FQTypeName =
  module UserTypeName =
    let toDT (u : PT.FQTypeName.UserTypeName) : Dval =
      DRecord(
        ptTyp [ "FQTypeName" ] "UserTypeName" 0,
        Map
          [ "modules", DList(List.map DString u.modules)
            "typ", DString u.typ
            "version", DInt u.version ]
      )


module TypeReference =
  let rec toDT (t : PT.TypeReference) : Dval =
    let name, fields =
      match t with
      | PT.TInt -> "TInt", []
      | _ ->
        Exception.raiseInternal
          "Mapping TypeReference currently only supported for TInt"
          []

    DEnum(ptTyp [] "TypeReference" 0, name, fields)


module CustomType =
  let toDT (d : PT.CustomType.T) : Dval =
    match d with
    | PT.CustomType.Alias typeRef ->
      DEnum(ptTyp [ "CustomType" ] "T" 0, "Alias", [ TypeReference.toDT typeRef ])

    | PT.CustomType.Record _
    | PT.CustomType.Enum _ ->
      Exception.raiseInternal
        "Mapping CustomType currently only supported for Alias"
        []

module UserType =
  let toDT (tlid : tlid) (userType : PT.UserType.T) : Dval =
    DRecord(
      ptTyp [] "UserType" 0,
      Map
        [ "tlid", DInt(int64 tlid)
          "name", FQTypeName.UserTypeName.toDT userType.name
          "definition", CustomType.toDT userType.definition ]
    )
