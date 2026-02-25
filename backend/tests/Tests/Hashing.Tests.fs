module Tests.Hashing

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

module PT = LibExecution.ProgramTypes
module Hashing = LibSerialization.Hashing


// ── Helpers ──────────────────────────────────────────────────────────────

let private makeFn (body : PT.Expr) : PT.PackageFn.PackageFn =
  testPackageFn [] (NEList.singleton "x") PT.TInt64 body

let private makeType
  (def : PT.TypeDeclaration.Definition)
  : PT.PackageType.PackageType =
  { id = System.Guid.NewGuid()
    hash = PT.ContentHash ""
    declaration = { typeParams = []; definition = def }
    description = ""
    deprecated = PT.NotDeprecated }

let private makeValue (body : PT.Expr) : PT.PackageValue.PackageValue =
  { id = System.Guid.NewGuid()
    hash = PT.ContentHash ""
    body = body
    description = ""
    deprecated = PT.NotDeprecated }


// ── Tests ────────────────────────────────────────────────────────────────

let tests =
  testList
    "Hashing"
    [ testList
        "computeTypeHash"
        [ test "determinism: same type hashed twice gives same hash" {
            let typ =
              makeType (
                PT.TypeDeclaration.Record(
                  NEList.singleton
                    { name = "x"; typ = PT.TInt64; description = "field" }
                )
              )
            let h1 = Hashing.computeTypeHash Hashing.Normal typ
            let h2 = Hashing.computeTypeHash Hashing.Normal typ
            Expect.equal h1 h2 "same type should hash identically"
          }

          test "different content gives different hash" {
            let typ1 =
              makeType (
                PT.TypeDeclaration.Record(
                  NEList.singleton { name = "x"; typ = PT.TInt64; description = "" }
                )
              )
            let typ2 =
              makeType (
                PT.TypeDeclaration.Record(
                  NEList.singleton { name = "y"; typ = PT.TString; description = "" }
                )
              )
            let h1 = Hashing.computeTypeHash Hashing.Normal typ1
            let h2 = Hashing.computeTypeHash Hashing.Normal typ2
            Expect.notEqual h1 h2 "different types should hash differently"
          }

          test "id does not affect hash" {
            let def =
              PT.TypeDeclaration.Record(
                NEList.singleton { name = "a"; typ = PT.TBool; description = "" }
              )
            let typ1 = { makeType def with id = System.Guid.NewGuid() }
            let typ2 = { makeType def with id = System.Guid.NewGuid() }
            let h1 = Hashing.computeTypeHash Hashing.Normal typ1
            let h2 = Hashing.computeTypeHash Hashing.Normal typ2
            Expect.equal h1 h2 "different ids should not affect hash"
          }

          test "description does not affect hash" {
            let def =
              PT.TypeDeclaration.Record(
                NEList.singleton { name = "a"; typ = PT.TBool; description = "" }
              )
            let typ1 = { makeType def with description = "first" }
            let typ2 = { makeType def with description = "second" }
            let h1 = Hashing.computeTypeHash Hashing.Normal typ1
            let h2 = Hashing.computeTypeHash Hashing.Normal typ2
            Expect.equal h1 h2 "description should not affect hash"
          } ]


      testList
        "computeFnHash"
        [ test "determinism: same fn hashed twice gives same hash" {
            let fn = makeFn (eInt64 42)
            let h1 = Hashing.computeFnHash Hashing.Normal fn
            let h2 = Hashing.computeFnHash Hashing.Normal fn
            Expect.equal h1 h2 "same fn should hash identically"
          }

          test "different body gives different hash" {
            let fn1 = makeFn (eInt64 42)
            let fn2 = makeFn (eInt64 99)
            let h1 = Hashing.computeFnHash Hashing.Normal fn1
            let h2 = Hashing.computeFnHash Hashing.Normal fn2
            Expect.notEqual h1 h2 "different bodies should hash differently"
          }

          test "AST node IDs do not affect hash" {
            let fn1 = makeFn (PT.EInt64(1UL, 42))
            let fn2 = makeFn (PT.EInt64(9999UL, 42))
            let h1 = Hashing.computeFnHash Hashing.Normal fn1
            let h2 = Hashing.computeFnHash Hashing.Normal fn2
            Expect.equal h1 h2 "AST node IDs should not affect hash"
          } ]


      testList
        "computeValueHash"
        [ test "determinism" {
            let v = makeValue (eInt64 7)
            let h1 = Hashing.computeValueHash Hashing.Normal v
            let h2 = Hashing.computeValueHash Hashing.Normal v
            Expect.equal h1 h2 "same value should hash identically"
          } ]


      testList
        "computeOpHash"
        [ test "returns ContentHash" {
            let fn = makeFn (eInt64 1)
            let op = PT.PackageOp.AddFn fn
            let hash = Hashing.computeOpHash op
            let (PT.ContentHash h) = hash
            Expect.isTrue (h.Length = 64) "should be 64 hex chars (SHA-256)"
          }

          test "determinism" {
            let fn = makeFn (eInt64 1)
            let op = PT.PackageOp.AddFn fn
            let h1 = Hashing.computeOpHash op
            let h2 = Hashing.computeOpHash op
            Expect.equal h1 h2 "same op should hash identically"
          } ]


      testList
        "computeCommitHash"
        [ test "determinism" {
            let opHash1 = PT.ContentHash "aabb"
            let opHash2 = PT.ContentHash "ccdd"
            let parent = Some(PT.ContentHash "0011")
            let h1 = Hashing.computeCommitHash parent [ opHash1; opHash2 ]
            let h2 = Hashing.computeCommitHash parent [ opHash1; opHash2 ]
            Expect.equal h1 h2 "same inputs should give same commit hash"
          }

          test "op order independence (sorted internally)" {
            let opHash1 = PT.ContentHash "aabb"
            let opHash2 = PT.ContentHash "ccdd"
            let parent = Some(PT.ContentHash "0011")
            let h1 = Hashing.computeCommitHash parent [ opHash1; opHash2 ]
            let h2 = Hashing.computeCommitHash parent [ opHash2; opHash1 ]
            Expect.equal h1 h2 "op order should not matter"
          }

          test "different parent gives different hash" {
            let ops = [ PT.ContentHash "aabb" ]
            let h1 = Hashing.computeCommitHash (Some(PT.ContentHash "0011")) ops
            let h2 = Hashing.computeCommitHash (Some(PT.ContentHash "0022")) ops
            Expect.notEqual h1 h2 "different parent should give different hash"
          }

          test "empty commit (no ops, just parent)" {
            let parent = Some(PT.ContentHash "0011")
            let h1 = Hashing.computeCommitHash parent []
            let h2 = Hashing.computeCommitHash parent []
            Expect.equal h1 h2 "empty commit should be deterministic"
          } ]


      testList
        "findSCCs (Tarjan)"
        [ test "single node, no edges" {
            let sccs = Hashing.findSCCs [ 1 ] (fun _ -> [])
            Expect.equal (List.length sccs) 1 "should have 1 SCC"
            Expect.equal sccs[0].head 1 "single node"
            Expect.equal sccs[0].tail [] "no tail"
          }

          test "linear chain (no cycles)" {
            // A -> B -> C
            let edges =
              function
              | 1 -> [ 2 ]
              | 2 -> [ 3 ]
              | _ -> []
            let sccs = Hashing.findSCCs [ 1; 2; 3 ] edges
            Expect.equal (List.length sccs) 3 "3 separate SCCs"
          }

          test "cycle A->B->C->A gives one SCC" {
            let edges =
              function
              | 1 -> [ 2 ]
              | 2 -> [ 3 ]
              | 3 -> [ 1 ]
              | _ -> []
            let sccs = Hashing.findSCCs [ 1; 2; 3 ] edges
            Expect.equal (List.length sccs) 1 "one SCC"
            let scc = sccs[0]
            let members = Set.ofList (scc.head :: scc.tail)
            Expect.equal members (Set.ofList [ 1; 2; 3 ]) "all three in SCC"
          }

          test "two separate cycles" {
            // A<->B, C<->D
            let edges =
              function
              | 1 -> [ 2 ]
              | 2 -> [ 1 ]
              | 3 -> [ 4 ]
              | 4 -> [ 3 ]
              | _ -> []
            let sccs = Hashing.findSCCs [ 1; 2; 3; 4 ] edges
            Expect.equal (List.length sccs) 2 "two SCCs"
          } ]


      testList
        "SCC batch hashing"
        [ test "two mutually-recursive types get stable hashes" {
            let id1 = System.Guid.NewGuid()
            let id2 = System.Guid.NewGuid()
            let typ1 =
              { (makeType (PT.TypeDeclaration.Alias PT.TInt64)) with id = id1 }
            let typ2 =
              { (makeType (PT.TypeDeclaration.Alias PT.TString)) with id = id2 }

            let types =
              [ (id1, (typ1, "Test.A")); (id2, (typ2, "Test.B")) ] |> Map.ofList

            let getDeps id =
              if id = id1 then [ id2 ]
              elif id = id2 then [ id1 ]
              else []

            let hashes1 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps
            let hashes2 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps

            Expect.equal hashes1 hashes2 "SCC hashes should be deterministic"
            Expect.notEqual
              (Map.find id1 hashes1)
              (Map.find id2 hashes1)
              "different items in SCC should have different hashes"
          } ] ]
