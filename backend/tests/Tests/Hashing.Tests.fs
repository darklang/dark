module Tests.Hashing

open Expecto
open Prelude

open TestUtils.TestUtils
open TestUtils.PTShortcuts

module PT = LibExecution.ProgramTypes
module PackageLocation = LibDB.PackageLocation
open LibSerialization.Hashing


// ── Helpers ──────────────────────────────────────────────────────────────

let private makeFn (body : PT.Expr) : PT.PackageFn.PackageFn =
  testPackageFn [] (NEList.singleton "x") PT.TInt64 body

let private makeType
  (def : PT.TypeDeclaration.Definition)
  : PT.PackageType.PackageType =
  { hash = PT.Hash ""
    declaration = { typeParams = []; definition = def }
    description = "" }

let private makeValue (body : PT.Expr) : PT.PackageValue.PackageValue =
  { hash = PT.Hash ""; body = body; description = "" }


// ── meaning-stable hashing: bound-variable names don't affect the hash (alpha-normalization) ──

let private fnOf
  (paramNames : List<string>)
  (body : PT.Expr)
  : PT.PackageFn.PackageFn =
  let ps =
    match paramNames with
    | [] -> NEList.singleton "unused"
    | h :: t -> NEList.ofList h t
  testPackageFn [] ps PT.TInt64 body

// the LIVE content hash — `computeFnHash` alpha-normalizes internally, so this *is* the meaning-stable hash.
// (`normalizeFn` stays public + idempotent; the last test pins that directly.)
let private h (paramNames : List<string>) (body : PT.Expr) : PT.Hash =
  Hashing.computeFnHash Hashing.Normal (fnOf paramNames body)

let private mpVar (name : string) : PT.MatchPattern = PT.MPVariable(gid (), name)

let private caseOf (pat : PT.MatchPattern) (rhs : PT.Expr) : PT.MatchCase =
  { pat = pat; whenCondition = None; rhs = rhs }

let private alphaNormTests =
  testList
    "meaning-stable (alpha-normalization)"
    [ test
        "parameter names don't affect the hash (a parameter use is positional, EArg)" {
        // the body references parameters by position (EArg index), and the parameter name isn't hashed, so
        // two fns identical up to parameter names share one content hash. (Meaning-preservation — which
        // parameter goes where — is pinned by the next test and the let/lambda/match tests below.)
        let body = eTuple (eArg 0) (eArg 1) [] // (param0, param1) — positional
        Expect.equal
          (h [ "x"; "y" ] body)
          (h [ "a"; "b" ] body)
          "same meaning, same hash — the function's identity ignores incidental parameter names"
      }

      test "parameter POSITION is meaning: `(arg0, arg1)` ≠ `(arg1, arg0)`" {
        Expect.notEqual
          (h [ "x"; "y" ] (eTuple (eArg 0) (eArg 1) []))
          (h [ "x"; "y" ] (eTuple (eArg 1) (eArg 0) []))
          "which parameter goes where is meaning — the positional reference keeps it"
      }

      test "let binder rename: `let x = 1 in x` ≡ `let y = 1 in y`" {
        let lx = eLet (lpVar "x") (eInt64 1L) (eVar "x")
        let ly = eLet (lpVar "y") (eInt64 1L) (eVar "y")
        Expect.equal (h [] lx) (h [] ly) "same meaning, same hash"
      }

      test "lambda binder rename: `fun x -> x` ≡ `fun y -> y`" {
        let lx = eLambda (gid ()) [ lpVar "x" ] (eVar "x")
        let ly = eLambda (gid ()) [ lpVar "y" ] (eVar "y")
        Expect.equal (h [] lx) (h [] ly) "alpha-equivalent lambdas hash equal"
      }

      test "lambda meaning preserved: `fun x y -> x` ≠ `fun x y -> y`" {
        let first = eLambda (gid ()) [ lpVar "x"; lpVar "y" ] (eVar "x")
        let second = eLambda (gid ()) [ lpVar "x"; lpVar "y" ] (eVar "y")
        Expect.notEqual
          (h [] first)
          (h [] second)
          "returning the first vs the second argument is a real difference"
      }

      test "match binder rename: `match 0 with | x -> x` ≡ `| y -> y`" {
        let mx = eMatch (eInt64 0L) [ caseOf (mpVar "x") (eVar "x") ]
        let my = eMatch (eInt64 0L) [ caseOf (mpVar "y") (eVar "y") ]
        Expect.equal (h [] mx) (h [] my) "alpha-equivalent match cases hash equal"
      }

      test "match binder position matters: `| (x,y) -> x` ≢ `| (x,y) -> y`" {
        // guards that multi-binder normalization keeps WHICH bound var the rhs uses — not just that it
        // renames binders. Both patterns bind the same two names; only the rhs's choice differs.
        let tuplePat = PT.MPTuple(gid (), mpVar "x", mpVar "y", [])
        let mFirst = eMatch (eInt64 0L) [ caseOf tuplePat (eVar "x") ]
        let mSecond = eMatch (eInt64 0L) [ caseOf tuplePat (eVar "y") ]
        Expect.notEqual
          (h [] mFirst)
          (h [] mSecond)
          "which tuple-bound variable the rhs returns is a real difference"
      }

      test
        "free variables are preserved (a free var is a real reference, not a binder)" {
        // `z` / `w` are neither parameters nor locally bound — they must survive normalization distinctly
        Expect.notEqual
          (h [] (eVar "z"))
          (h [] (eVar "w"))
          "two different free variables stay different after normalization"
      }

      test "shadowing: inner-use ≢ outer-use; and inner-use is alpha-stable" {
        // let _ = 1 in let _ = 2 in <use>
        let useInnerXY =
          eLet (lpVar "x") (eInt64 1L) (eLet (lpVar "y") (eInt64 2L) (eVar "y"))
        let useInnerAB =
          eLet (lpVar "a") (eInt64 1L) (eLet (lpVar "b") (eInt64 2L) (eVar "b"))
        let useOuter =
          eLet (lpVar "a") (eInt64 1L) (eLet (lpVar "b") (eInt64 2L) (eVar "a"))
        Expect.equal
          (h [] useInnerXY)
          (h [] useInnerAB)
          "using the inner binding is alpha-stable across renames"
        Expect.notEqual
          (h [] useInnerAB)
          (h [] useOuter)
          "using the inner vs the outer binding is a real difference (shadowing respected)"
      }

      test
        "normalization is idempotent (re-normalizing an already-normalized expr is a structural no-op)" {
        // Compare the normalized EXPR structures directly (not through computeFnHash, which normalizes
        // internally). normalizeExpr preserves ids and only renames binders, so a second pass over the
        // canonical form must be identity. Use real binders (let + lambda + a shadowing inner let).
        let body =
          eLet
            (lpVar "outer")
            (eInt64 1L)
            (eLambda
              (gid ())
              [ lpVar "p" ]
              (eLet (lpVar "inner") (eVar "p") (eVar "inner")))
        let once = Hashing.normalizeExpr body
        let twice = Hashing.normalizeExpr once
        Expect.equal
          twice
          once
          "normalizeExpr (normalizeExpr e) is structurally identical to normalizeExpr e"
      } ]


let private typeHashTests =
  testList
    "computeTypeHash"
    [ test "determinism: same type hashed twice gives same hash" {
        let typ =
          makeType (
            PT.TypeDeclaration.Record(
              NEList.singleton { name = "x"; typ = PT.TInt64; description = "field" }
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


let private fnHashTests =
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


let private valueHashTests =
  testList
    "computeValueHash"
    [ test "determinism" {
        let v = makeValue (eInt64 7)
        let h1 = Hashing.computeValueHash Hashing.Normal v
        let h2 = Hashing.computeValueHash Hashing.Normal v
        Expect.equal h1 h2 "same value should hash identically"
      } ]


let private opHashTests =
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


let private sccTests =
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


let private placeholderHashTests =
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


let private sccBatchTests =
  testList
    "SCC batch hashing"
    [ test "two mutually-recursive types get stable hashes" {
        let id1 = PT.Hash "test-scc-type-1"
        let id2 = PT.Hash "test-scc-type-2"
        let typ1 =
          { (makeType (PT.TypeDeclaration.Alias PT.TInt64)) with hash = id1 }
        let typ2 =
          { (makeType (PT.TypeDeclaration.Alias PT.TString)) with hash = id2 }

        // Maps keyed by FQN; tuple value is (item, oldHash, location)
        let types =
          [ ("Test.A", (typ1, id1, None)); ("Test.B", (typ2, id2, None)) ]
          |> Map.ofList

        let getDeps fqn =
          if fqn = "Test.A" then [ "Test.B" ]
          elif fqn = "Test.B" then [ "Test.A" ]
          else []

        let hashes1 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            Map.empty
            Map.empty
            getDeps
        let hashes2 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            Map.empty
            Map.empty
            getDeps

        Expect.equal hashes1 hashes2 "SCC hashes should be deterministic"
        Expect.notEqual
          (Map.find "Test.A" hashes1)
          (Map.find "Test.B" hashes1)
          "different items in SCC should have different hashes"
      }

      test "SCC name refs are keyed by location when old hashes collide" {
        let sharedHash = PT.Hash "test-scc-shared-old-hash"
        let locA : PT.PackageLocation =
          { owner = "Test"; modules = [ "Scc" ]; name = "A" }
        let locB : PT.PackageLocation =
          { owner = "Test"; modules = [ "Scc" ]; name = "B" }

        let valueRef (loc : PT.PackageLocation) =
          PT.EValue(
            gid (),
            { originalName = []
              resolved =
                Ok { name = PT.FQValueName.Package sharedHash; location = Some loc } }
          )

        let mode : Canonical.HashRefMode =
          { subst = Canonical.emptySubstitution
            sccNames =
              { byLocation = [ locA, "Test.Scc.A"; locB, "Test.Scc.B" ] |> Map.ofList
                byHash = [ sharedHash, "Test.Scc.B" ] |> Map.ofList } }

        let hashA = Hashing.computeValueHash mode (makeValue (valueRef locA))
        let hashB = Hashing.computeValueHash mode (makeValue (valueRef locB))

        Expect.notEqual
          hashA
          hashB
          "same-hash SCC refs at different locations should canonicalize to different FQN refs"
      }

      test "3-node cycle A->B->C->A gets stable hashes" {
        let idA = PT.Hash "test-3cycle-A"
        let idB = PT.Hash "test-3cycle-B"
        let idC = PT.Hash "test-3cycle-C"
        let typA =
          { (makeType (PT.TypeDeclaration.Alias PT.TInt64)) with hash = idA }
        let typB =
          { (makeType (PT.TypeDeclaration.Alias PT.TString)) with hash = idB }
        let typC = { (makeType (PT.TypeDeclaration.Alias PT.TBool)) with hash = idC }

        let types =
          [ ("Test.A", (typA, idA, None))
            ("Test.B", (typB, idB, None))
            ("Test.C", (typC, idC, None)) ]
          |> Map.ofList

        // A->B->C->A cycle
        let getDeps fqn =
          if fqn = "Test.A" then [ "Test.B" ]
          elif fqn = "Test.B" then [ "Test.C" ]
          elif fqn = "Test.C" then [ "Test.A" ]
          else []

        let hashes1 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            Map.empty
            Map.empty
            getDeps
        let hashes2 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            Map.empty
            Map.empty
            getDeps

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
        let typC = { (makeType (PT.TypeDeclaration.Alias PT.TBool)) with hash = idC }

        let getDeps fqn =
          if fqn = "Test.A" then [ "Test.B" ]
          elif fqn = "Test.B" then [ "Test.C" ]
          elif fqn = "Test.C" then [ "Test.A" ]
          else []

        // Declaration order 1: A, B, C
        let types1 =
          [ ("Test.A", (typA, idA, None))
            ("Test.B", (typB, idB, None))
            ("Test.C", (typC, idC, None)) ]
          |> Map.ofList

        // Declaration order 2: C, A, B
        let types2 =
          [ ("Test.C", (typC, idC, None))
            ("Test.A", (typA, idA, None))
            ("Test.B", (typB, idB, None)) ]
          |> Map.ofList

        let hashes1 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types1
            Map.empty
            Map.empty
            getDeps
        let hashes2 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types2
            Map.empty
            Map.empty
            getDeps

        Expect.equal
          (Map.find "Test.A" hashes1)
          (Map.find "Test.A" hashes2)
          "A hash same regardless of order"
        Expect.equal
          (Map.find "Test.B" hashes1)
          (Map.find "Test.B" hashes2)
          "B hash same regardless of order"
        Expect.equal
          (Map.find "Test.C" hashes1)
          (Map.find "Test.C" hashes2)
          "C hash same regardless of order"
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

        let types = [ ("Test.T", (typ, idT, None)) ] |> Map.ofList

        // Self-loop: T depends on T
        let getDeps fqn = if fqn = "Test.T" then [ "Test.T" ] else []

        let hashes1 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            Map.empty
            Map.empty
            getDeps
        let hashes2 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            Map.empty
            Map.empty
            getDeps

        Expect.equal
          hashes1
          hashes2
          "self-recursive type hash should be deterministic"

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

        let types = [ ("Test.MyType", (typ, idTyp, None)) ] |> Map.ofList
        let fns = [ ("Test.myFn", (fn, idFn, None)) ] |> Map.ofList

        // MyType depends on myFn, myFn depends on MyType
        let getDeps fqn =
          if fqn = "Test.MyType" then [ "Test.myFn" ]
          elif fqn = "Test.myFn" then [ "Test.MyType" ]
          else []

        let hashes1 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            fns
            Map.empty
            getDeps
        let hashes2 =
          Hashing.computeHashesWithSCCs
            Canonical.emptySubstitution
            types
            fns
            Map.empty
            getDeps

        Expect.equal hashes1 hashes2 "mixed SCC hashes should be deterministic"
        Expect.notEqual
          (Map.find "Test.MyType" hashes1)
          (Map.find "Test.myFn" hashes1)
          "type and fn in mixed SCC should have different hashes"

        let (PT.Hash hTyp) = Map.findUnsafe "Test.MyType" hashes1
        let (PT.Hash hFn) = Map.findUnsafe "Test.myFn" hashes1
        Expect.isTrue (hTyp.Length = 64) "type hash should be 64 hex chars"
        Expect.isTrue (hFn.Length = 64) "fn hash should be 64 hex chars"
      } ]


let tests =
  testList
    "Hashing"
    [ typeHashTests
      fnHashTests
      valueHashTests
      opHashTests
      sccTests
      placeholderHashTests
      sccBatchTests
      alphaNormTests ]
