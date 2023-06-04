module Tests.BinarySerialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open Tablecloth
open TestUtils.TestUtils

module File = LibBackend.File
module Config = LibBackend.Config
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization

module Values = SerializationTestValues

module RoundtripTests =
  let toplevelRoundtripTest =
    testMany
      "serializeToplevels"
      (fun tl ->
        let tlid = PT.Toplevel.toTLID tl
        tl
        |> BinarySerialization.serializeToplevel
        |> BinarySerialization.deserializeToplevel tlid
        |> (=) tl)
      (List.map (fun x -> x, true) Values.ProgramTypes.toplevels)

  let oplistRoundtripTest =
    test "roundtrip oplists" {
      let actual =
        Values.ProgramTypes.oplist
        |> BinarySerialization.serializeOplist 0UL
        |> BinarySerialization.deserializeOplist 0UL
      Expect.equal actual Values.ProgramTypes.oplist ""
    }

module ConsistentSerializationTests =
  type Format =
    { name : string
      serializer : PT.Oplist -> byte array
      deserializer : byte array -> PT.Oplist
      prettyPrinter : Option<PT.Oplist -> string>
      prefix : string
      suffix : string
      prettyPrinterSuffix : string }

  let formats =
    [ { name = "BinarySerialization"
        serializer = BinarySerialization.serializeOplist 0UL
        deserializer = BinarySerialization.deserializeOplist 0UL
        prettyPrinter = Some(BinarySerialization.Test.serializeOplistToJson 0UL)
        prefix = "oplist-binary"
        suffix = ".bin"
        prettyPrinterSuffix = ".json" } ]

  let nameFor (f : Format) (pretty : bool) (version : string) =
    let (pretty, suffix) =
      if pretty then ("-pretty", f.prettyPrinterSuffix) else "", f.suffix
    $"{f.prefix}{pretty}-{version}{suffix}"

  /// Generates timestamped test files for binary serialization. These files are used
  /// to prove that the binary serialization format is compatible.  When we change the
  /// format, we should still be able to read the old files in addition to the new ones
  /// (though they will not necessarily have the same output). If we make changes to
  /// the binary serialization format (or to the test cases), we generate the files
  /// and commit them.
  let generateTestFiles () : unit =
    formats
    |> List.iter (fun f ->
      let output = f.serializer Values.ProgramTypes.oplist

      let sha1 =
        System.Security.Cryptography.SHA1.HashData(System.ReadOnlySpan output)
        |> SimpleBase.Base16.LowerCase.Encode

      File.writefileBytes Config.Serialization (nameFor f false sha1) output
      File.writefileBytes Config.Serialization (nameFor f false "latest") output

      f.prettyPrinter
      |> Option.tap (fun s ->
        let jsonData = s Values.ProgramTypes.oplist
        File.writefile Config.Serialization (nameFor f true sha1) jsonData
        File.writefile Config.Serialization (nameFor f true "latest") jsonData))

  let testTestFiles =
    formats
    |> List.map (fun f ->
      test "check test files are correct" {
        f.prettyPrinter
        |> Option.tap (fun s ->
          let expected =
            File.readfile Config.Serialization (nameFor f true "latest")
          let actual = s Values.ProgramTypes.oplist
          Expect.equal actual expected "check generates the same json")

        // Check that the generated binary data matches what we have saved. This ensures
        // the format has not changed.
        let actual = f.serializer Values.ProgramTypes.oplist
        let expected =
          File.readfileBytes Config.Serialization (nameFor f false "latest")
        Expect.equal actual expected "check can read the saved file"

        // Check that all .bin files can be parsed and give us the expected answer (this
        // might not be true as we get more formats, so this may need to be adapted)
        File.lsPattern Config.Serialization "{f.prefix}.*{f.suffix}"
        |> List.iter (fun filename ->
          let actual =
            File.readfileBytes Config.Serialization filename |> f.deserializer
          Expect.equal
            actual
            Values.ProgramTypes.oplist
            "deserialize should  match latest format")
      })

let generateTestFiles () =
  // Enabled in dev so we can see changes as git diffs
  // Disabled in CI so changes will fail the tests
  if Config.serializationGenerateTestData then
    ConsistentSerializationTests.generateTestFiles ()

let tests =
  testList
    "Binary Serialization"
    [ RoundtripTests.toplevelRoundtripTest
      RoundtripTests.oplistRoundtripTest
      testList "consistent serialization" ConsistentSerializationTests.testTestFiles ]
