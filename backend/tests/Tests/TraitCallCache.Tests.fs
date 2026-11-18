/// The reload's trait-call cache is keyed on the source files of the builtins the at-rest checker
/// runs (`CheckerSources`), not on the whole of the assemblies they are in, and misses once an
/// impl a body resolved to changes.
module Tests.TraitCallCache

open Expecto
open System.Threading.Tasks

open Prelude

open TestUtils.TestUtils

module CheckerSources = Builtins.Matter.Libs.PM.CheckerSources
module TraitCalls = Builtins.Matter.Libs.PM.TraitCalls
module PT = LibExecution.ProgramTypes

let private outside = TraitCalls.outsideTheChecker
let private root = LibConfig.Dirs.sourceRootDir
let private pureLib name = $"backend/src/Builtins/Builtins.Pure/Libs/{name}.fs"

let private checkerFiles () : Task<Set<string>> =
  task {
    let state = executionStateFor pmPT false
    return CheckerSources.checkerFiles outside state pmPT
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
        let state = executionStateFor pmPT false
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
      }
      testTask "a reload checks a body again once an impl it resolved to changes" {
        // Fresh names per run: a type is content-addressed, and an earlier run's answers would
        // otherwise already be in the cache.
        let r = System.Random.Shared.Next(1_000, 1_000_000_000)
        let m = "TraitCallCacheImplEdit"
        let q = $"Darklang.{m}"
        let tag = $"tag{r}"
        let author = Tests.Interpreter.BoundMemo.authorStep
        let! _ = author "type" $"module {q}\n\ntype Box = {{ boxWidth{r}: Int64 }}"
        let! _ =
          author "trait" $"module {q}\n\ntrait Tag<'a> =\n  let {tag}(v: 'a): String"
        let impl answer =
          author
            "impl"
            $"module {q}\n\nimpl {q}.Tag for {q}.Box =\n  let {tag}(b: {q}.Box): String = \"{answer}\""
        let! implOps = impl "one"
        let! _ =
          author
            "caller"
            $"module {q}\n\nlet caller(): String = {q}.Tag.{tag}({q}.Box {{ boxWidth{r} = 1 }})"
        let at name : PT.PackageLocation =
          { owner = "Darklang"; modules = [ m ]; name = name }
        let bound what =
          implOps
          |> List.pick (fun op ->
            match op with
            | PT.PackageOp.SetName(at, target, _) when what target -> Some at
            | _ -> None)
        let implAt =
          bound (function
            | PT.Reference.PackageTraitImpl _ -> true
            | _ -> false)
        let methodAt =
          bound (function
            | PT.Reference.PackageFn _ -> true
            | _ -> false)
        let found (what : string) (hash : Option<'a>) : 'a =
          hash |> Option.defaultWith (fun () -> failtest $"no {what} in the store")

        // The tree as the store holds it now, as a reload would hand it over: the refresh may
        // have re-hashed what the parses returned.
        let reload () =
          task {
            let typeHash = found "type" (pmPT.findType (at "Box"))
            let traitHash = found "trait" (pmPT.findTrait (at "Tag"))
            let implHash = found "impl" (pmPT.findTraitImpl implAt)
            let methodHash = found "method" (pmPT.findFn methodAt)
            let callerHash = found "caller" (pmPT.findFn (at "caller"))
            let fn h = PT.PackageOp.AddFn(found "fn" (pmPT.getFn h))
            let ops =
              [ PT.PackageOp.AddType(found "type" (pmPT.getType typeHash))
                PT.PackageOp.SetName(
                  at "Box",
                  PT.Reference.PackageType typeHash,
                  None
                )
                PT.PackageOp.AddTrait(found "trait" (pmPT.getTrait traitHash))
                PT.PackageOp.SetName(
                  at "Tag",
                  PT.Reference.PackageTrait traitHash,
                  None
                )
                PT.PackageOp.AddTraitImpl(found "impl" (pmPT.getTraitImpl implHash))
                PT.PackageOp.SetName(
                  implAt,
                  PT.Reference.PackageTraitImpl implHash,
                  None
                )
                fn methodHash
                PT.PackageOp.SetName(
                  methodAt,
                  PT.Reference.PackageFn methodHash,
                  None
                )
                fn callerHash
                PT.PackageOp.SetName(
                  at "caller",
                  PT.Reference.PackageFn callerHash,
                  None
                ) ]
            let! resolved, _ =
              TraitCalls.resolveTraitCalls
                (executionStateFor pmPT false)
                PT.BranchId.Main
                pmPT
                (Some "trait-call-cache-test")
                ops
            let body =
              resolved
              |> List.pick (function
                | PT.PackageOp.AddFn fn when fn.hash = callerHash -> Some fn.body
                | _ -> None)
            let (PT.Hash methodHash) = methodHash
            return sprintf "%A" body, methodHash
          }

        let! first, firstMethod = reload ()
        Expect.stringContains first firstMethod "the call resolves to the impl"
        let! again, _ = reload ()
        Expect.equal again first "and to the same one from the cache"

        let! _ = impl "two"
        let! afterEdit, secondMethod = reload ()
        Expect.notEqual secondMethod firstMethod "the edit is a new method fn"
        Expect.stringContains
          afterEdit
          secondMethod
          "after the impl changes, the call resolves to the new one"
      } ]
