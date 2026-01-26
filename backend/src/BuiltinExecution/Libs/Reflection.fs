module BuiltinExecution.Libs.Reflection

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


let fns : List<BuiltInFn> =
  [ { name = fn "reflect" 0
      typeParams = []
      parameters = [ Param.make "dv" (TVariable "a") "" ]
      returnType =
        TCustomType(
          Ok(
            FQTypeName.fqPackage
              LibExecution.PackageIDs.Type.LanguageTools.RuntimeTypes.dval
          ),
          []
        )
      description = "Returns a meta representation of the real underlying dval"
      fn =
        function
        | _, _, _, [ dv ] ->
          dv |> LibExecution.RuntimeTypesToDarkTypes.Dval.toDT |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "reflectType" 0
      typeParams = []
      parameters = [ Param.make "dv" (TVariable "a") "" ]
      returnType =
        TCustomType(
          Ok(
            FQTypeName.fqPackage
              LibExecution.PackageIDs.Type.LanguageTools.RuntimeTypes.valueType
          ),
          []
        )
      description =
        "Returns the ValueType of a value without the full Dval structure. More lightweight than reflect for type inspection."
      fn =
        function
        | _, _, _, [ dv ] ->
          dv
          |> Dval.toValueType
          |> LibExecution.RuntimeTypesToDarkTypes.ValueType.toDT
          |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "typeOf" 0
      typeParams = []
      parameters = [ Param.make "dv" (TVariable "a") "" ]
      returnType = TString
      description =
        "Returns a human-readable type name as a string (e.g., 'Int64', 'List<String>', 'Dict<Bool>'). Lightweight alternative to full reflection."
      fn =
        function
        | _, _, _, [ dv ] ->
          let rec typeToString (vt : ValueType) : string =
            match vt with
            | ValueType.Unknown -> "Unknown"
            | ValueType.Known kt ->
              match kt with
              | KTUnit -> "Unit"
              | KTBool -> "Bool"
              | KTInt8 -> "Int8"
              | KTUInt8 -> "UInt8"
              | KTInt16 -> "Int16"
              | KTUInt16 -> "UInt16"
              | KTInt32 -> "Int32"
              | KTUInt32 -> "UInt32"
              | KTInt64 -> "Int64"
              | KTUInt64 -> "UInt64"
              | KTInt128 -> "Int128"
              | KTUInt128 -> "UInt128"
              | KTFloat -> "Float"
              | KTChar -> "Char"
              | KTString -> "String"
              | KTUuid -> "Uuid"
              | KTDateTime -> "DateTime"
              | KTList inner -> $"List<{typeToString inner}>"
              | KTDict inner -> $"Dict<{typeToString inner}>"
              | KTTuple(first, second, theRest) ->
                let types =
                  [ first; second ] @ theRest |> List.map typeToString |> String.concat ", "
                $"({types})"
              | KTCustomType(typeName, typeArgs) ->
                let name =
                  match typeName with
                  | FQTypeName.Package id -> $"Package({id})"
                let args =
                  if List.isEmpty typeArgs then
                    ""
                  else
                    "<" + (typeArgs |> List.map typeToString |> String.concat ", ") + ">"
                name + args
              | KTFn(args, ret) ->
                let argStr =
                  args
                  |> NEList.toList
                  |> List.map typeToString
                  |> String.concat ", "
                $"({argStr}) -> {typeToString ret}"
              | KTDB inner -> $"DB<{typeToString inner}>"

          dv |> Dval.toValueType |> typeToString |> DString |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "typeName" 0
      typeParams = []
      parameters = [ Param.make "dv" (TVariable "a") "" ]
      returnType =
        TCustomType(
          Ok(FQTypeName.stdlib "Option" 0),
          [ TCustomType(
              Ok(
                FQTypeName.fqPackage
                  LibExecution.PackageIDs.Type.LanguageTools.RuntimeTypes.FQTypeName
                    .fqTypeName
              ),
              []
            ) ]
        )
      description =
        "Returns the FQTypeName for custom types (records and enums). Returns None for primitives and built-in types."
      fn =
        function
        | _, _, _, [ dv ] ->
          let typeName =
            match dv with
            | DRecord(_, typeName, _, _) -> Some typeName
            | DEnum(_, typeName, _, _, _) -> Some typeName
            | _ -> None

          match typeName with
          | Some name ->
            LibExecution.RuntimeTypesToDarkTypes.FQTypeName.toDT name
            |> Dval.optionSome (VT.known LibExecution.RuntimeTypesToDarkTypes.FQTypeName.knownType)
          | None ->
            Dval.optionNone (VT.known LibExecution.RuntimeTypesToDarkTypes.FQTypeName.knownType)
          |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
