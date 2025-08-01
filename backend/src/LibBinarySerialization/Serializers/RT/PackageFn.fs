module LibBinarySerialization.Serializers.RT.PackageFn

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.RT.Common


module Parameter =
  let write (w : BinaryWriter) (p : PackageFn.Parameter) =
    String.write w p.name
    TypeReference.write w p.typ

  let read (r : BinaryReader) : PackageFn.Parameter =
    let name = String.read r
    let typ = TypeReference.read r
    { name = name; typ = typ }


let write (w : BinaryWriter) (fn : PackageFn.PackageFn) =
  Guid.write w fn.id
  List.write w String.write fn.typeParams
  NEList.write Parameter.write w fn.parameters
  TypeReference.write w fn.returnType
  Instructions.write w fn.body

let read (r : BinaryReader) : PackageFn.PackageFn =
  let id = Guid.read r
  let typeParams = List.read r String.read
  let parameters = NEList.read Parameter.read r
  let returnType = TypeReference.read r
  let body = Instructions.read r
  { id = id
    typeParams = typeParams
    parameters = parameters
    returnType = returnType
    body = body }
