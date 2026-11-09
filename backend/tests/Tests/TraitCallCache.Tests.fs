/// The reload's trait-call cache is keyed on the source files of the builtins the at-rest checker
/// runs (`CheckerSources`), not on the whole of the assemblies they are in.
module Tests.TraitCallCache

open Expecto
open System.Threading.Tasks

open Prelude

open TestUtils.TestUtils

module CheckerSources = Builtins.Matter.Libs.PM.CheckerSources
module TraitCalls = Builtins.Matter.Libs.PM.TraitCalls

let private outside = TraitCalls.outsideTheChecker
let private root = LibConfig.Dirs.sourceRootDir
let private pureLib name = $"backend/src/Builtins/Builtins.Pure/Libs/{name}.fs"

let private checkerFiles () : Task<Set<string>> =
  task {
    let! state = executionStateFor pmPT false
    return! CheckerSources.checkerFiles outside state pmPT |> Ply.toTask
  }

let tests =
  testList
    "TraitCallCache"
    [ testTask "the checker's files take in what it calls, and its helpers" {
        let! (files : Set<string>) = checkerFiles ()
        Expect.contains files (pureLib "Dict") "the checker calls Dict.get"
        Expect.contains
          files
          "backend/src/Builtins/Builtins.Pure/CaseMapping.fs"
          "String.toLowercase calls CaseMapping, which defines no builtin"
        Expect.isFalse
          (files.Contains(pureLib "Bytes"))
          "the checker calls no Bytes builtin"
        Expect.isFalse
          (files.Contains(pureLib "Math"))
          "the checker calls no Math builtin"
      }

      testTask "every builtin's file is where the checker's files are looked for" {
        let! state = executionStateFor pmPT false
        let missing =
          CheckerSources.allFiles outside state
          |> Set.filter (fun f ->
            not (System.IO.File.Exists(System.IO.Path.Combine(root, f))))
        Expect.isEmpty missing "a builtin's module is named for its file"
      }

      testTask
        "an edit to a file the checker runs changes the key, and to any other does not" {
        let! (files : Set<string>) = checkerFiles ()
        let copied = files |> Set.add (pureLib "Bytes") |> Set.add (pureLib "Math")
        let dir = System.IO.Directory.CreateTempSubdirectory "trait-call-cache-"
        try
          for f in copied do
            let target = System.IO.Path.Combine(dir.FullName, f)
            System.IO.Directory.CreateDirectory(
              System.IO.Path.GetDirectoryName target
            )
            |> ignore<System.IO.DirectoryInfo>
            System.IO.File.Copy(System.IO.Path.Combine(root, f), target)
          let key () = LibExecution.HostProcess.sourcesHash dir.FullName files
          let edit name =
            System.IO.File.AppendAllText(
              System.IO.Path.Combine(dir.FullName, pureLib name),
              "\n// edited\n"
            )
          let before = key ()
          edit "Bytes"
          edit "Math"
          Expect.equal (key ()) before "Bytes.fs and Math.fs are not the checker's"
          edit "Dict"
          Expect.notEqual (key ()) before "Dict.fs is the checker's"
        finally
          dir.Delete true
      } ]
