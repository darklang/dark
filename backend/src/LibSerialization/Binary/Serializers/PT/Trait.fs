/// `Trait.Trait` and `Impl.Impl`: format v3 items. A v2 blob never carries them.
module LibSerialization.Binary.Serializers.PT.Trait

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common


module Method =
  let write (w : BinaryWriter) (m : Trait.Method) : unit =
    String.write w m.name
    LibSerialization.Binary.Serializers.Common.List.write w String.write m.typeParams
    NEList.write LibSerialization.Binary.Serializers.PT.PackageFn.Parameter.write w m.parameters
    TypeReference.write w m.returnType
    Option.write w LibSerialization.Binary.Serializers.Effects.write m.permissionCeiling
    String.write w m.description

  let read (r : BinaryReader) : Trait.Method =
    let name = String.read r
    let typeParams = LibSerialization.Binary.Serializers.Common.List.read r String.read
    let parameters =
      NEList.read LibSerialization.Binary.Serializers.PT.PackageFn.Parameter.read r
    let returnType = TypeReference.read r
    let permissionCeiling = Option.read r LibSerialization.Binary.Serializers.Effects.read
    let description = String.read r
    { name = name
      typeParams = typeParams
      parameters = parameters
      returnType = returnType
      permissionCeiling = permissionCeiling
      description = description }


let write (w : BinaryWriter) (t : Trait.Trait) : unit =
  Hash.write w t.hash
  NEList.write String.write w t.typeParams
  LibSerialization.Binary.Serializers.Common.List.write w TypeReference.Bound.write t.bounds
  NEList.write Method.write w t.methods
  String.write w t.description

let read (r : BinaryReader) : Trait.Trait =
  let hash = Hash.read r
  let typeParams = NEList.read String.read r
  let bounds = LibSerialization.Binary.Serializers.Common.List.read r TypeReference.Bound.read
  let methods = NEList.read Method.read r
  let description = String.read r
  { hash = hash
    typeParams = typeParams
    bounds = bounds
    methods = methods
    description = description }


module Impl =
  let write (w : BinaryWriter) (i : Impl.Impl) : unit =
    Hash.write w i.hash
    NameResolution.write FQTraitName.write w i.trait_
    LibSerialization.Binary.Serializers.Common.List.write w TypeReference.write i.traitTypeArgs
    TypeReference.write w i.self
    LibSerialization.Binary.Serializers.Common.List.write w String.write i.typeParams
    LibSerialization.Binary.Serializers.Common.List.write w TypeReference.Bound.write i.bounds
    LibSerialization.Binary.Serializers.Common.List.write
      w
      (fun w (m, nr) ->
        String.write w m
        NameResolution.write FQFnName.write w nr)
      i.methods
    String.write w i.description

  let read (r : BinaryReader) : Impl.Impl =
    let hash = Hash.read r
    let trait_ = NameResolution.read FQTraitName.read r
    let traitTypeArgs = LibSerialization.Binary.Serializers.Common.List.read r TypeReference.read
    let self = TypeReference.read r
    let typeParams = LibSerialization.Binary.Serializers.Common.List.read r String.read
    let bounds = LibSerialization.Binary.Serializers.Common.List.read r TypeReference.Bound.read
    let methods =
      LibSerialization.Binary.Serializers.Common.List.read r (fun r ->
        let m = String.read r
        let nr = NameResolution.read FQFnName.read r
        (m, nr))
    let description = String.read r
    { hash = hash
      trait_ = trait_
      traitTypeArgs = traitTypeArgs
      self = self
      typeParams = typeParams
      bounds = bounds
      methods = methods
      description = description }
