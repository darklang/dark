module LibBinarySerialization.Serializers.PT.PackageFn

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.PT.Common


module Name =
  let write (w : BinaryWriter) (name : PackageFn.Name) : unit =
    String.write w name.owner
    List.write w String.write name.modules
    String.write w name.name

  let read (r : BinaryReader) : PackageFn.Name =
    let owner = String.read r
    let modules = List.read r String.read
    let name = String.read r
    { owner = owner; modules = modules; name = name }


module Parameter =
  let write (w : BinaryWriter) (p : PackageFn.Parameter) : unit =
    String.write w p.name
    TypeReference.write w p.typ
    String.write w p.description

  let read (r : BinaryReader) : PackageFn.Parameter =
    let name = String.read r
    let typ = TypeReference.read r
    let description = String.read r
    { name = name; typ = typ; description = description }


let write (w : BinaryWriter) (p : PackageFn.PackageFn) : unit =
  LibBinarySerialization.Serializers.Common.Hash.write w p.hash
  Name.write w p.name
  LibBinarySerialization.Serializers.PT.Expr.Expr.write w p.body
  LibBinarySerialization.Serializers.Common.List.write w String.write p.typeParams
  NEList.write Parameter.write w p.parameters
  TypeReference.write w p.returnType
  String.write w p.description
  Deprecation.write w FQFnName.write p.deprecated

let read (r : BinaryReader) : PackageFn.PackageFn =
  let hash = LibBinarySerialization.Serializers.Common.Hash.read r
  let name = Name.read r
  let body = LibBinarySerialization.Serializers.PT.Expr.Expr.read r
  let typeParams = LibBinarySerialization.Serializers.Common.List.read r String.read
  let parameters = NEList.read Parameter.read r
  let returnType = TypeReference.read r
  let description = String.read r
  let deprecated = Deprecation.read r FQFnName.read
  { hash = hash
    name = name
    body = body
    typeParams = typeParams
    parameters = parameters
    returnType = returnType
    description = description
    deprecated = deprecated }
