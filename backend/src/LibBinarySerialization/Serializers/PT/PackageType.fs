module LibBinarySerialization.Serializers.PT.PackageType

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.PT.Common


module Name =
  let write (w : BinaryWriter) (n : PackageType.Name) : unit =
    String.write w n.owner
    List.write w String.write n.modules
    String.write w n.name

  let read (r : BinaryReader) : PackageType.Name =
    let owner = String.read r
    let modules = List.read r String.read
    let name = String.read r
    { owner = owner; modules = modules; name = name }


module TypeDeclaration =
  module RecordField =
    let write (w : BinaryWriter) (f : TypeDeclaration.RecordField) : unit =
      String.write w f.name
      TypeReference.write w f.typ
      String.write w f.description

    let read (r : BinaryReader) : TypeDeclaration.RecordField =
      let name = String.read r
      let typ = TypeReference.read r
      let description = String.read r
      { name = name; typ = typ; description = description }


  module EnumField =
    let write (w : BinaryWriter) (f : TypeDeclaration.EnumField) : unit =
      TypeReference.write w f.typ
      Option.write w String.write f.label
      String.write w f.description

    let read (r : BinaryReader) : TypeDeclaration.EnumField =
      let typ = TypeReference.read r
      let label = Option.read r String.read
      let description = String.read r
      { typ = typ; label = label; description = description }


  module EnumCase =
    let write (w : BinaryWriter) (c : TypeDeclaration.EnumCase) : unit =
      String.write w c.name
      List.write w EnumField.write c.fields
      String.write w c.description

    let read (r : BinaryReader) : TypeDeclaration.EnumCase =
      let name = String.read r
      let fields = List.read r EnumField.read
      let description = String.read r
      { name = name; fields = fields; description = description }


  module Definition =
    let write (w : BinaryWriter) (d : TypeDeclaration.Definition) : unit =
      match d with
      | TypeDeclaration.Alias typeRef ->
        w.Write(0uy)
        TypeReference.write w typeRef
      | TypeDeclaration.Record fields ->
        w.Write(1uy)
        NEList.write RecordField.write w fields
      | TypeDeclaration.Enum cases ->
        w.Write(2uy)
        NEList.write EnumCase.write w cases

    let read (r : BinaryReader) : TypeDeclaration.Definition =
      match r.ReadByte() with
      | 0uy -> TypeDeclaration.Alias(TypeReference.read r)
      | 1uy -> TypeDeclaration.Record(NEList.read RecordField.read r)
      | 2uy -> TypeDeclaration.Enum(NEList.read EnumCase.read r)
      | b ->
        raise (
          BinaryFormatException(
            CorruptedData $"Invalid TypeDeclaration.Definition tag: {b}"
          )
        )


  let write (w : BinaryWriter) (d : TypeDeclaration.T) : unit =
    List.write w String.write d.typeParams
    Definition.write w d.definition

  let read (r : BinaryReader) : TypeDeclaration.T =
    let typeParams = List.read r String.read
    let definition = Definition.read r
    { typeParams = typeParams; definition = definition }


let write (w : BinaryWriter) (t : PackageType.PackageType) : unit =
  LibBinarySerialization.Serializers.Common.Hash.write w t.hash
  Name.write w t.name
  TypeDeclaration.write w t.declaration
  String.write w t.description
  Deprecation.write w FQTypeName.write t.deprecated

let read (r : BinaryReader) : PackageType.PackageType =
  let hash = LibBinarySerialization.Serializers.Common.Hash.read r
  let name = Name.read r
  let declaration = TypeDeclaration.read r
  let description = String.read r
  let deprecated = Deprecation.read r FQTypeName.read
  { hash = hash
    name = name
    declaration = declaration
    description = description
    deprecated = deprecated }
