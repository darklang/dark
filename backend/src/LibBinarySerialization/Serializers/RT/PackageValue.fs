module LibBinarySerialization.Serializers.RT.PackageValue

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.RT.Common

let write (w : BinaryWriter) (c : PackageValue.PackageValue) : unit =
  Guid.write w c.id
  Dval.write w c.body

let read (r : BinaryReader) : PackageValue.PackageValue =
  let id = Guid.read r
  let body = Dval.read r
  { id = id; body = body }
