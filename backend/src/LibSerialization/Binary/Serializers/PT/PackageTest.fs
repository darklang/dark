module LibSerialization.Binary.Serializers.PT.PackageTest

open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common

let private writeExpectedError (w : BinaryWriter) (error : ExpectedError) : unit =
  match error with
  | ExpectedError.RuntimeError message ->
    w.Write(0uy)
    String.write w message
  | ExpectedError.SqlCompilerError message ->
    w.Write(1uy)
    String.write w message

let private readExpectedError (r : BinaryReader) : ExpectedError =
  match r.ReadByte() with
  | 0uy -> ExpectedError.RuntimeError(String.read r)
  | 1uy -> ExpectedError.SqlCompilerError(String.read r)
  | tag -> Exception.raiseInternal "Invalid expected test error tag" [ "tag", tag ]

let write (w : BinaryWriter) (test : PackageTest.PackageTest) : unit =
  Hash.write w test.hash
  LibSerialization.Binary.Serializers.PT.Expr.Expr.write w test.body
  String.write w test.description
  Option.write w writeExpectedError test.expectedError
  List.write
    w
    (fun w (name, typ) ->
      String.write w name
      TypeReference.write w typ)
    test.testDBs

let read (version : uint32) (r : BinaryReader) : PackageTest.PackageTest =
  { hash = Hash.read r
    body = LibSerialization.Binary.Serializers.PT.Expr.Expr.read version r
    description = String.read r
    expectedError = Option.read r readExpectedError
    testDBs = List.read r (fun r -> String.read r, TypeReference.read r) }
