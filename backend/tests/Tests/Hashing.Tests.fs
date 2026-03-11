module Tests.Hashing

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

module PT = LibExecution.ProgramTypes
module PackageLocation = LibPackageManager.PackageLocation
open LibSerialization.Hashing


// ── Helpers ──────────────────────────────────────────────────────────────

let private makeFn (body : PT.Expr) : PT.PackageFn.PackageFn =
  testPackageFn [] (NEList.singleton "x") PT.TInt64 body

let private makeType
  (def : PT.TypeDeclaration.Definition)
  : PT.PackageType.PackageType =
  { hash = PT.Hash ""
    declaration = { typeParams = []; definition = def }
    description = ""
    deprecated = PT.NotDeprecated }

let private makeValue (body : PT.Expr) : PT.PackageValue.PackageValue =
  { hash = PT.Hash ""; body = body; description = ""; deprecated = PT.NotDeprecated }


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
        [ test "returns Hash" {
            let fn = makeFn (eInt64 1)
            let op = PT.PackageOp.AddFn fn
            let hash = Hashing.computeOpHash op
            let (PT.Hash h) = hash
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
            let opHash1 = PT.Hash "aabb"
            let opHash2 = PT.Hash "ccdd"
            let parent = Some(PT.Hash "0011")
            let h1 =
              Hashing.computeCommitHash
                parent
                [ opHash1; opHash2 ]
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            let h2 =
              Hashing.computeCommitHash
                parent
                [ opHash1; opHash2 ]
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            Expect.equal h1 h2 "same inputs should give same commit hash"
          }

          test "op order independence (sorted internally)" {
            let opHash1 = PT.Hash "aabb"
            let opHash2 = PT.Hash "ccdd"
            let parent = Some(PT.Hash "0011")
            let h1 =
              Hashing.computeCommitHash
                parent
                [ opHash1; opHash2 ]
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            let h2 =
              Hashing.computeCommitHash
                parent
                [ opHash2; opHash1 ]
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            Expect.equal h1 h2 "op order should not matter"
          }

          test "different parent gives different hash" {
            let ops = [ PT.Hash "aabb" ]
            let h1 =
              Hashing.computeCommitHash
                (Some(PT.Hash "0011"))
                ops
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            let h2 =
              Hashing.computeCommitHash
                (Some(PT.Hash "0022"))
                ops
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            Expect.notEqual h1 h2 "different parent should give different hash"
          }

          test "empty commit (no ops, just parent)" {
            let parent = Some(PT.Hash "0011")
            let h1 =
              Hashing.computeCommitHash
                parent
                []
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
            let h2 =
              Hashing.computeCommitHash
                parent
                []
                (System.Guid.Parse "00000000-0000-0000-0000-000000000001")
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
        "placeholder hashes (toFQN-based)"
        [ test "same location gives same FQN" {
            let loc : PT.PackageLocation =
              { owner = "Test"; modules = [ "Mod" ]; name = "Foo" }
            Expect.equal
              (PackageLocation.toFQN loc)
              (PackageLocation.toFQN loc)
              "FQN should be deterministic"
          }

          test "different locations give different FQNs" {
            let loc1 : PT.PackageLocation =
              { owner = "Test"; modules = [ "Mod" ]; name = "Foo" }
            let loc2 : PT.PackageLocation =
              { owner = "Test"; modules = [ "Mod" ]; name = "Bar" }
            Expect.notEqual
              (PackageLocation.toFQN loc1)
              (PackageLocation.toFQN loc2)
              "different names should differ"
          }

          test "FQN format matches expected pattern" {
            let loc : PT.PackageLocation =
              { owner = "Darklang"; modules = [ "Stdlib"; "List" ]; name = "map" }
            Expect.equal
              (PackageLocation.toFQN loc)
              "Darklang.Stdlib.List.map"
              "FQN should be owner.modules.name"
          }

          test "FQN-based SHA-256 produces valid hash" {
            let loc : PT.PackageLocation =
              { owner = "Test"; modules = [ "Mod" ]; name = "Foo" }
            let nameKey = PackageLocation.toFQN loc
            let nameBytes =
              System.Security.Cryptography.SHA256.HashData(
                System.Text.Encoding.UTF8.GetBytes(nameKey)
              )
            let hash =
              PT.Hash(
                System.BitConverter
                  .ToString(nameBytes)
                  .Replace("-", "")
                  .ToLowerInvariant()
              )
            let (PT.Hash h) = hash
            Expect.isTrue (h.Length = 64) "should be 64 hex chars (SHA-256)"
          } ]


      testList
        "SCC batch hashing"
        [ test "two mutually-recursive types get stable hashes" {
            let id1 = PT.Hash "test-scc-type-1"
            let id2 = PT.Hash "test-scc-type-2"
            let typ1 =
              { (makeType (PT.TypeDeclaration.Alias PT.TInt64)) with hash = id1 }
            let typ2 =
              { (makeType (PT.TypeDeclaration.Alias PT.TString)) with hash = id2 }

            // Maps keyed by FQN; tuple value is (item, oldHash)
            let types =
              [ ("Test.A", (typ1, id1)); ("Test.B", (typ2, id2)) ] |> Map.ofList

            let getDeps fqn =
              if fqn = "Test.A" then [ "Test.B" ]
              elif fqn = "Test.B" then [ "Test.A" ]
              else []

            let hashes1 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps
            let hashes2 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps

            Expect.equal hashes1 hashes2 "SCC hashes should be deterministic"
            Expect.notEqual
              (Map.find "Test.A" hashes1)
              (Map.find "Test.B" hashes1)
              "different items in SCC should have different hashes"
          }


          test "3-node cycle A->B->C->A gets stable hashes" {
            let idA = PT.Hash "test-3cycle-A"
            let idB = PT.Hash "test-3cycle-B"
            let idC = PT.Hash "test-3cycle-C"
            let typA =
              { (makeType (PT.TypeDeclaration.Alias PT.TInt64)) with hash = idA }
            let typB =
              { (makeType (PT.TypeDeclaration.Alias PT.TString)) with hash = idB }
            let typC =
              { (makeType (PT.TypeDeclaration.Alias PT.TBool)) with hash = idC }

            let types =
              [ ("Test.A", (typA, idA))
                ("Test.B", (typB, idB))
                ("Test.C", (typC, idC)) ]
              |> Map.ofList

            // A->B->C->A cycle
            let getDeps fqn =
              if fqn = "Test.A" then [ "Test.B" ]
              elif fqn = "Test.B" then [ "Test.C" ]
              elif fqn = "Test.C" then [ "Test.A" ]
              else []

            let hashes1 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps
            let hashes2 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps

            // Deterministic
            Expect.equal hashes1 hashes2 "3-node SCC hashes should be deterministic"

            // All three items should get distinct hashes despite being in the same SCC
            let hashA = Map.find "Test.A" hashes1
            let hashB = Map.find "Test.B" hashes1
            let hashC = Map.find "Test.C" hashes1
            Expect.notEqual hashA hashB "A and B should have different hashes"
            Expect.notEqual hashB hashC "B and C should have different hashes"
            Expect.notEqual hashA hashC "A and C should have different hashes"
          }


          test "3-node cycle is order-independent" {
            let idA = PT.Hash "test-3cycle-A"
            let idB = PT.Hash "test-3cycle-B"
            let idC = PT.Hash "test-3cycle-C"
            let typA =
              { (makeType (PT.TypeDeclaration.Alias PT.TInt64)) with hash = idA }
            let typB =
              { (makeType (PT.TypeDeclaration.Alias PT.TString)) with hash = idB }
            let typC =
              { (makeType (PT.TypeDeclaration.Alias PT.TBool)) with hash = idC }

            let getDeps fqn =
              if fqn = "Test.A" then [ "Test.B" ]
              elif fqn = "Test.B" then [ "Test.C" ]
              elif fqn = "Test.C" then [ "Test.A" ]
              else []

            // Declaration order 1: A, B, C
            let types1 =
              [ ("Test.A", (typA, idA))
                ("Test.B", (typB, idB))
                ("Test.C", (typC, idC)) ]
              |> Map.ofList

            // Declaration order 2: C, A, B
            let types2 =
              [ ("Test.C", (typC, idC))
                ("Test.A", (typA, idA))
                ("Test.B", (typB, idB)) ]
              |> Map.ofList

            let hashes1 =
              Hashing.computeHashesWithSCCs types1 Map.empty Map.empty getDeps
            let hashes2 =
              Hashing.computeHashesWithSCCs types2 Map.empty Map.empty getDeps

            Expect.equal
              (Map.find "Test.A" hashes1)
              (Map.find "Test.A" hashes2)
              "A hash should be same regardless of declaration order"
            Expect.equal
              (Map.find "Test.B" hashes1)
              (Map.find "Test.B" hashes2)
              "B hash should be same regardless of declaration order"
            Expect.equal
              (Map.find "Test.C" hashes1)
              (Map.find "Test.C" hashes2)
              "C hash should be same regardless of declaration order"
          }


          test "self-recursive type does not infinite loop and gets stable hash" {
            let idTStr = "test-self-recursive"
            let idT = PT.Hash idTStr
            // A type that references itself (like a linked list node)
            let typ =
              { (makeType (
                  PT.TypeDeclaration.Record(
                    NEList.ofListUnsafe
                      ""
                      []
                      [ { name = "value"; typ = PT.TInt64; description = "" }
                        { name = "next"
                          typ =
                            PT.TCustomType(
                              PT.NameResolution.ok (PT.FQTypeName.fqPackage idTStr),
                              []
                            )
                          description = "" } ]
                  )
                )) with
                  hash = idT }

            let types = [ ("Test.T", (typ, idT)) ] |> Map.ofList

            // Self-loop: T depends on T
            let getDeps fqn = if fqn = "Test.T" then [ "Test.T" ] else []

            let hashes1 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps
            let hashes2 =
              Hashing.computeHashesWithSCCs types Map.empty Map.empty getDeps

            // Should terminate and produce deterministic results
            Expect.equal
              hashes1
              hashes2
              "self-recursive type hash should be deterministic"

            // Should produce a valid hash
            let (PT.Hash h) = Map.findUnsafe "Test.T" hashes1
            Expect.isTrue (h.Length = 64) "should be 64 hex chars (SHA-256)"
          }


          test "mixed cycle: type and fn that mutually depend on each other" {
            let idTyp = PT.Hash "test-mixed-type"
            let idFn = PT.Hash "test-mixed-fn"

            // A type that (via getDeps) depends on the function
            let typ =
              { (makeType (PT.TypeDeclaration.Alias PT.TInt64)) with hash = idTyp }

            // A function that (via getDeps) depends on the type
            let fn = { (makeFn (eInt64 42)) with hash = idFn }

            let types = [ ("Test.MyType", (typ, idTyp)) ] |> Map.ofList
            let fns = [ ("Test.myFn", (fn, idFn)) ] |> Map.ofList

            // MyType depends on myFn, myFn depends on MyType
            let getDeps fqn =
              if fqn = "Test.MyType" then [ "Test.myFn" ]
              elif fqn = "Test.myFn" then [ "Test.MyType" ]
              else []

            let hashes1 = Hashing.computeHashesWithSCCs types fns Map.empty getDeps
            let hashes2 = Hashing.computeHashesWithSCCs types fns Map.empty getDeps

            // Deterministic
            Expect.equal hashes1 hashes2 "mixed SCC hashes should be deterministic"

            // Type and fn should get distinct hashes
            Expect.notEqual
              (Map.find "Test.MyType" hashes1)
              (Map.find "Test.myFn" hashes1)
              "type and fn in mixed SCC should have different hashes"

            // Both hashes should be valid SHA-256
            let (PT.Hash hTyp) = Map.findUnsafe "Test.MyType" hashes1
            let (PT.Hash hFn) = Map.findUnsafe "Test.myFn" hashes1
            Expect.isTrue (hTyp.Length = 64) "type hash should be 64 hex chars"
            Expect.isTrue (hFn.Length = 64) "fn hash should be 64 hex chars"
          } ] ]
