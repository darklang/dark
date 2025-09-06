module LibBinarySerialization.Serializers.RT.PackageValue

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

module PackageIDs = LibExecution.PackageIDs

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.RT.Common

let write (w : BinaryWriter) (c : PackageValue.PackageValue) : unit =
  LibBinarySerialization.Serializers.Common.Hash.write w c.hash
  Dval.write w c.body

let read (r : BinaryReader) : PackageValue.PackageValue =
  let hash = LibBinarySerialization.Serializers.Common.Hash.read r
  let body = Dval.read r
  { hash = hash; body = body }
