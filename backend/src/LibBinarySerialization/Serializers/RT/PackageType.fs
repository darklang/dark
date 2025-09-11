module LibBinarySerialization.Serializers.RT.PackageType

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

module PackageIDs = LibExecution.PackageIDs

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.RT.Common


module RecordField =
  let write (w : BinaryWriter) (f : TypeDeclaration.RecordField) : unit =
    String.write w f.name
    TypeReference.write w f.typ

  let read (r : BinaryReader) : TypeDeclaration.RecordField =
    let name = String.read r
    let typ = TypeReference.read r
    { name = name; typ = typ }


module EnumCase =
  let write (w : BinaryWriter) (c : TypeDeclaration.EnumCase) : unit =
    String.write w c.name
    List.write w TypeReference.write c.fields

  let read (r : BinaryReader) : TypeDeclaration.EnumCase =
    let name = String.read r
    let fields = List.read r TypeReference.read
    { name = name; fields = fields }


module Definition =
  let write (w : BinaryWriter) (d : TypeDeclaration.Definition) : unit =
    match d with
    | TypeDeclaration.Alias typeRef ->
      w.Write 0uy
      TypeReference.write w typeRef
    | TypeDeclaration.Record fields ->
      w.Write 1uy
      NEList.write RecordField.write w fields
    | TypeDeclaration.Enum cases ->
      w.Write 2uy
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


module TypeDeclaration =
  let write (w : BinaryWriter) (d : TypeDeclaration.T) : unit =
    List.write w String.write d.typeParams
    Definition.write w d.definition

  let read (r : BinaryReader) : TypeDeclaration.T =
    let typeParams = List.read r String.read
    let definition = Definition.read r
    { typeParams = typeParams; definition = definition }


let write (w : BinaryWriter) (t : PackageType.PackageType) =
  LibBinarySerialization.Serializers.Common.Hash.write w t.hash
  TypeDeclaration.write w t.declaration

let read (r : BinaryReader) : PackageType.PackageType =
  let hash = LibBinarySerialization.Serializers.Common.Hash.read r
  let declaration = TypeDeclaration.read r
  { hash = hash; declaration = declaration }
