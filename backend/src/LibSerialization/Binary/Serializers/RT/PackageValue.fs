module LibSerialization.Binary.Serializers.RT.PackageValue

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.RT.Common

let write (w : BinaryWriter) (c : PackageValue.PackageValue) : unit =
  ContentHash.write w c.hash
  Dval.write w c.body

let read (r : BinaryReader) : PackageValue.PackageValue =
  let hash = ContentHash.read r
  let body = Dval.read r
  { hash = hash; body = body }
