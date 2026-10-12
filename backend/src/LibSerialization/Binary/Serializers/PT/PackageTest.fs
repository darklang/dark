module LibSerialization.Binary.Serializers.PT.PackageTest

open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common

let private legacyDBMarker = 0xDA7ABA5Eu
let private extensionMarker = 0xDA7ABA5Fu
let private extensionVersion = 1uy

let write (w : BinaryWriter) (test : PackageTest.PackageTest) : unit =
  Hash.write w test.hash
  LibSerialization.Binary.Serializers.PT.Expr.Expr.write w test.body
  String.write w test.description
  // Preserve the legacy slot for ordinary runtime-error assertions.
  let runtimeError =
    test.expectedError
    |> Option.bind (function
      | ExpectedError.RuntimeError message -> Some message
      | ExpectedError.SqlCompilerError _ -> None)
  Option.write w String.write runtimeError
  Option.write
    w
    LibSerialization.Binary.Serializers.Effects.write
    test.permissionCeiling
  let sqlError =
    test.expectedError
    |> Option.bind (function
      | ExpectedError.SqlCompilerError message -> Some message
      | ExpectedError.RuntimeError _ -> None)
  // PackageTest values also occur inside PackageOps, so an extension needs
  // a marker and version rather than relying on EOF.
  if not (List.isEmpty test.testDBs) || Option.isSome sqlError then
    w.Write extensionMarker
    w.Write extensionVersion
    List.write
      w
      (fun w (name, typ) ->
        String.write w name
        TypeReference.write w typ)
      test.testDBs
    Option.write w String.write sqlError

let read (version : uint32) (r : BinaryReader) : PackageTest.PackageTest =
  let hash = Hash.read r
  let body = LibSerialization.Binary.Serializers.PT.Expr.Expr.read version r
  let description = String.read r
  let runtimeError = Option.read r String.read
  let permissionCeiling =
    Option.read r LibSerialization.Binary.Serializers.Effects.read
  let testDBs, sqlError =
    let start = r.BaseStream.Position
    if r.BaseStream.Length - start < 4L then
      [], None
    else
      match r.ReadUInt32() with
      | marker when marker = legacyDBMarker ->
        List.read r (fun r -> String.read r, TypeReference.read r), None
      | marker when marker = extensionMarker ->
        let version = r.ReadByte()
        if version <> extensionVersion then
          Exception.raiseInternal
            "Unsupported package test extension version"
            [ "version", version ]
        let dbs = List.read r (fun r -> String.read r, TypeReference.read r)
        let sqlError = Option.read r String.read
        dbs, sqlError
      | _ ->
        r.BaseStream.Position <- start
        [], None
  let expectedError =
    match runtimeError, sqlError with
    | None, None -> None
    | Some message, None -> Some(ExpectedError.RuntimeError message)
    | None, Some message -> Some(ExpectedError.SqlCompilerError message)
    | Some _, Some _ ->
      Exception.raiseInternal "Package test has two expected errors" []
  { hash = hash
    body = body
    description = description
    expectedError = expectedError
    testDBs = testDBs
    permissionCeiling = permissionCeiling }
