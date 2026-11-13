/// Propagation: the F# pieces the cascade is built from.
///
/// The cascade itself is tested in Dark, in `packages/darklang/tests/scm/propagation.dark`,
/// through the same `SCM.Propagation.repointDependents` the save path runs. These two stay
/// here because they feed the AST rewriter and the dependency extractor synthetic hashes
/// that no authored source can produce.
module Tests.Propagation

open Expecto

open Prelude

module PT = LibExecution.ProgramTypes


let dictionaryKeyReferences =
  test "dictionary keys rewrite and expose function type and value references" {
    let hash name = PT.Hash name
    let body =
      PT.EDict(
        1UL,
        [ PT.EApply(
            2UL,
            PT.EFnName(
              3UL,
              PT.NameResolution.ok (PT.FQFnName.Package(hash "fn")),
              [],
              []
            ),
            [ PT.TCustomType(
                PT.NameResolution.ok (PT.FQTypeName.Package(hash "type")),
                []
              ) ],
            NEList.singleton (
              PT.EValue(
                4UL,
                PT.NameResolution.ok (PT.FQValueName.Package(hash "key"))
              )
            )
          ),
          PT.EValue(5UL, PT.NameResolution.ok (PT.FQValueName.Package(hash "value"))) ]
      )
    let value : PT.PackageValue.PackageValue =
      { hash = hash "container"; body = body; description = "" }
    let names = [ "fn"; "type"; "key"; "value" ]
    let mapping =
      { LibDB.AstTransformer.emptyMapping with
          byHash = names |> List.map (fun n -> hash n, hash $"new-{n}") |> Map.ofList }
    let transformed = LibDB.AstTransformer.transformValue mapping value
    let dependencies = LibDB.DependencyExtractor.extractFromValue transformed
    Expect.equal
      (dependencies |> List.map _.hash |> Set.ofList)
      (names |> List.map (fun n -> hash $"new-{n}") |> Set.ofList)
      "references in the key and value are rewritten and retained in the graph"
  }

let traitMethodBoundIsADependency =
  test "a trait method's own bound is a dependency of the trait" {
    let bound : PT.Bound =
      { param = "b"
        trait_ =
          { trait_ = PT.NameResolution.ok (PT.FQTraitName.Package(PT.Hash "compare"))
            typeArgs = [] } }
    let method : PT.Trait.Method =
      { name = "pair"
        typeParams = [ "b" ]
        bounds = [ bound ]
        parameters =
          NEList.singleton { name = "v"; typ = PT.TVariable "a"; description = "" }
        returnType = PT.TVariable "b"
        description = "" }
    let t : PT.Trait.Trait =
      { hash = PT.Hash "pairs"
        typeParams = NEList.singleton "a"
        bounds = []
        methods = NEList.singleton method
        description = "" }
    Expect.contains
      (LibDB.DependencyExtractor.extractFromTrait t |> List.map _.hash)
      (PT.Hash "compare")
      "so propagation repoints the trait when the bounding trait moves"
  }


let tests =
  testList "Propagation" [ dictionaryKeyReferences; traitMethodBoundIsADependency ]
