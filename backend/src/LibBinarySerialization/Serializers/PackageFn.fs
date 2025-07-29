/// PackageFn serialization for custom binary format
module LibBinarySerialization.Serializers.PackageFn

open System
open System.IO
open Prelude
open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Primitives
open LibBinarySerialization.SerializedTypes
open TypeReference
open Expr
open PackageType
open Common

module Name =
  let write (writer : BinaryWriter) (name : PackageFn.Name) : unit =
    Writer.writeString writer name.owner
    Writer.writeList writer Writer.writeString name.modules
    Writer.writeString writer name.name

  let read (reader : BinaryReader) : PackageFn.Name =
    let owner = Reader.readString reader
    let modules = Reader.readList reader Reader.readString
    let name = Reader.readString reader
    { owner = owner; modules = modules; name = name }

module Parameter =
  let write (writer : BinaryWriter) (p : PackageFn.Parameter) : unit =
    Writer.writeString writer p.name
    writeTypeReference writer p.typ
    Writer.writeString writer p.description

  let read (reader : BinaryReader) : PackageFn.Parameter =
    let name = Reader.readString reader
    let typ = readTypeReference reader
    let description = Reader.readString reader
    { name = name; typ = typ; description = description }

module NEListParameter =
  let write
    (writer : BinaryWriter)
    (parameters : NEList<PackageFn.Parameter>)
    : unit =
    Parameter.write writer parameters.head
    Writer.writeList writer Parameter.write parameters.tail

  let read (reader : BinaryReader) : NEList<PackageFn.Parameter> =
    let head = Parameter.read reader
    let tail = Reader.readList reader Parameter.read
    { head = head; tail = tail }

let writeFQFnName (writer : BinaryWriter) (name : FQFnName.FQFnName) : unit =
  FQFnName.write writer name

let readFQFnName (reader : BinaryReader) : FQFnName.FQFnName = FQFnName.read reader

let write (writer : BinaryWriter) (packageFn : PackageFn.PackageFn) : unit =
  Writer.writeGuid writer packageFn.id
  Name.write writer packageFn.name
  writeExpr writer packageFn.body
  Writer.writeList writer Writer.writeString packageFn.typeParams
  NEListParameter.write writer packageFn.parameters
  writeTypeReference writer packageFn.returnType
  Writer.writeString writer packageFn.description
  PackageType.Deprecation.write writer writeFQFnName packageFn.deprecated

let read (reader : BinaryReader) : PackageFn.PackageFn =
  let id = Reader.readGuid reader
  let name = Name.read reader
  let body = readExpr reader
  let typeParams = Reader.readList reader Reader.readString
  let parameters = NEListParameter.read reader
  let returnType = readTypeReference reader
  let description = Reader.readString reader
  let deprecated = PackageType.Deprecation.read reader readFQFnName
  { id = id
    name = name
    body = body
    typeParams = typeParams
    parameters = parameters
    returnType = returnType
    description = description
    deprecated = deprecated }
