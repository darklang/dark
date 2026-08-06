/// Builtin functions for working with the dev-time Package Manager
///   (_not_ the run-time PM)
///
/// A ProgramTypes Package Manager is taken as a parameter, so that:
/// - the Cloud runtime can use the Cloud PM (just accessing DB directly)
/// - the CLI runtime can use the HTTP-bound PM
///   (which calls upon endpoints in the dark-packages canvas)
///
/// Previously, the non-Cloud package manager was supported by Dark package fns that
/// made HTTPClient calls to the Cloud-hosted PM, but: since Darklang doesn't really
/// have a caching mechanism, it made more sense to have the HTTP-access be inside
/// of builtin functions.
///
/// At run-time, we use the PM to support the parser flow, and pretty-printing (i.e.
/// to grab the name of a package type). We do those operations quite a lot, so it's
/// important that the operations are reasonably fast, which we can't curently do
/// without some sort of such caching.
module Builtins.Matter.Libs.PM.Packages

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module D = LibExecution.DvalDecoder
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PackageRefs = LibExecution.PackageRefs
module C2DT = LibExecution.CommonToDarkTypes
module VT = LibExecution.ValueType
module NR = LibExecution.RuntimeTypes.NameResolution
module RTPM = LibDB.RuntimeTypes
module PMPT = LibDB.ProgramTypes
module Branches = LibDB.Branches
module Execution = LibExecution.Execution

let statsTypeName () = FQTypeName.fqPackage (PackageRefs.Type.DarkPackages.stats ())

let private repointListKT =
  KTList(ValueType.Known(PT2DT.PropagateRepoint.knownType ()))


// TODO: review/reconsider the accessibility of these fns
/// A branch parameter as the PM layer wants it: `""` means main, anything else names a branch.
///
/// The branch is a PARAMETER here rather than process state, so a caller can ask about a branch it isn't
/// sitting on -- which is what the LSP and any daemon need, and what stops a reader assuming the ambient
/// branch is the one they meant.
let private branchOfParam (branchId : string) : Option<string> =
  if branchId = "main" then None else Some branchId


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ // types
    { name = fn "pmFindType" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Tries to find a package type, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [| DString branchId; location |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            // On a BRANCH, resolve against the overlay so a name authored earlier on the same branch
            // resolves (intra-branch reference fix). With NO branch active, use the exact original
            // main lookup (PMPT.Type.find) -- byte-identical, cache-free -- so non-branch resolution is
            // unchanged (the branch overlay PM's cached base would otherwise go stale across a shared
            // process, e.g. in the test harness).
            let! result =
              match branchId with
              | "main" -> PMPT.Type.find location
              | id -> (LibDB.PackageManager.ptForBranch (Some id)).findType location
            return
              result
              |> Option.map PT2DT.Hash.toDT
              |> Dval.option (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetType" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.PackageType.typeName ()), []))
      description = "Returns a package type, by hash, if it exists"
      fn =
        let optType = KTCustomType((PT2DT.PackageType.typeName ()), [])
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getType hash
            return result |> Option.map PT2DT.PackageType.toDT |> Dval.option optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // values
    { name = fn "pmFindValue" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Tries to find a package value, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [| DString branchId; location |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            // Branch overlay when on a branch (intra-branch refs); exact original main lookup otherwise.
            let! result =
              match branchId with
              | "main" -> PMPT.Value.find location
              | id -> (LibDB.PackageManager.ptForBranch (Some id)).findValue location
            return
              result
              |> Option.map PT2DT.Hash.toDT
              |> Dval.option (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetValue" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (
          TCustomType(NR.ok (PT2DT.PackageValue.typeName ()), [])
        )
      description = "Returns a package value, by hash, if it exists"
      fn =
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getValue hash
            return
              result
              |> Option.map PT2DT.PackageValue.toDT
              |> Dval.option (KTCustomType((PT2DT.PackageValue.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Find all value IDs that have a specific ValueType
    { name = fn "pmFindValuesByValueType" 0
      typeParams = []
      parameters =
        [ Param.make
            "valueType"
            (TCustomType(NR.ok (RT2DT.ValueType.typeName ()), []))
            "The ValueType to search for" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Returns a list of value hashes that have the given ValueType. "
        + "Uses exact match on the serialized type for efficient lookup."
      fn =
        (function
        | _, _, _, [| valueTypeDval |] ->
          uply {
            let vt = RT2DT.ValueType.fromDT valueTypeDval
            let! valueIds = RTPM.Value.findByValueType vt
            return
              DList(
                VT.known (PT2DT.Hash.knownType ()),
                valueIds |> List.map RT2DT.Hash.toDT
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Evaluate a package value by its UUID
    { name = fn "pmEvaluateValue" 0
      typeParams = []
      parameters =
        [ Param.make
            "valueHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package value to evaluate" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "Evaluates a package value by its hash and returns the result. "
        + "Returns None if the value doesn't exist or fails to evaluate."
      fn =
        (function
        | exeState, _, _, [| hashDval |] ->
          uply {
            let (PT.Hash hash) = PT2DT.Hash.fromDT hashDval
            let valueName = FQValueName.Package(Hash hash)
            let instrs : Instructions =
              { registerCount = 1
                instructions = [ LoadValue(0, valueName) ]
                resultIn = 0 }

            let! result = Execution.executeExpr exeState instrs
            match result with
            | Ok dval ->
              match Dval.toValueType dval with
              | ValueType.Known kt -> return Dval.optionSome kt dval
              | ValueType.Unknown -> return Dval.optionSome KTUnit dval
            | Error _ -> return Dval.optionNone KTUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Functions
    { name = fn "pmFindFn" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Tries to find a package function, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [| DString branchId; location |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            // Branch overlay when on a branch (intra-branch refs); exact original main lookup otherwise.
            let! result =
              match branchId with
              | "main" -> PMPT.Fn.find location
              | id -> (LibDB.PackageManager.ptForBranch (Some id)).findFn location
            return
              result
              |> Option.map PT2DT.Hash.toDT
              |> Dval.option (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetFn" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.PackageFn.typeName ()), []))
      description = "Returns a package function, by hash, if it exists"
      fn =
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getFn hash
            return
              result
              |> Option.map PT2DT.PackageFn.toDT
              |> Dval.option (KTCustomType((PT2DT.PackageFn.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Resolve a package fn's dotted name to a callable value (Applicable), so a name that only exists as a
    // STRING (a CLI arg) can be passed as a function without eval'ing a source string. This is what lets
    // `dark serve` hand a router to `Stdlib.HttpServer.serve` directly (no `cliEvaluateExpression`).
    // CLEANUP(applicableByName): `dark eval` already resolves a dotted name to a runnable value, so this builtin
    // can eventually fold into eval and be deleted (one fewer builtin — the fewer-builtins rule). ~1hr; the one
    // wrinkle is keeping an expression that produces nothing printing nothing. Left for a tidy pass.
    { name = fn "applicableByName" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make "name" TString "dotted package fn name, e.g. Stdlib.List.map" ]
      returnType =
        TypeReference.result
          (TFn(NEList.singleton (TVariable "a"), TVariable "b"))
          TString
      description =
        "Resolves a package function by its dotted <param name> to a callable value, "
        + "as a Result — Error (a plain-English message) if there's no such function. "
        + "Lets a caller (e.g. `dark serve`) report a bad name cleanly instead of "
        + "crashing."
      fn =
        (function
        | _, _, _, [| DString branchId; DString name |] ->
          uply {
            let okKT = KTFn(NEList.singleton ValueType.Unknown, ValueType.Unknown)
            let err (msg : string) = Dval.resultError okKT KTString (DString msg)
            // dotted name → owner.modules….fnName (native pattern-match; Prelude's List.last is Option-safe)
            match name.Split('.') |> Array.toList |> List.rev with
            | fnName :: revOwnerMods ->
              match List.rev revOwnerMods with
              | owner :: modules ->
                let location : PT.PackageLocation =
                  { owner = owner; modules = modules; name = fnName }
                // Through the branch overlay, like `pmSearch` and the `pmFind*` trio. The closure `pm` is
                // MAIN's package manager, so serving a router authored on a branch answered "No function
                // named ..." about a fn that was plainly there -- and the message went on to ask whether it
                // was defined on this branch, which it was.
                let branchPM =
                  LibDB.PackageManager.ptForBranch (branchOfParam branchId)
                match! branchPM.findFn location with
                | Some fqPkg ->
                  let rtName = FQFnName.Package(PT2RT.FQFnName.Package.toRT fqPkg)
                  let namedFn : ApplicableNamedFn =
                    { name = rtName
                      typeSymbolTable = TST.empty
                      typeArgs = []
                      argsSoFar = [] }
                  return
                    Dval.resultOk okKT KTString (DApplicable(AppNamedFn namedFn))
                | None -> return err $"No function named {name}"
              | [] -> return err $"Not a package function name: {name}"
            | [] -> return err "Empty router name"
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSearch" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make
            "query"
            (TCustomType(NR.ok (PT2DT.Search.SearchQuery.typeName ()), []))
            "" ]
      returnType = TCustomType(NR.ok (PT2DT.Search.SearchResults.typeName ()), [])
      description = "Search for packages based on the given query."
      fn =
        function
        | _, _, _, [| DString branchId; query as DRecord(_, _, _, _fields) |] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            // Route through the active branch overlay (core when no branch is active) so a branch's
            // items show up in ls/view/tree/search, not just eval. No-op for main.
            let pm = LibDB.PackageManager.ptForBranch (branchOfParam branchId)
            let! results = pm.search searchQuery
            return PT2DT.Search.SearchResults.toDT results
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSearchNames" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TString "the branch to search on; \"\" is main"
          Param.make
            "query"
            (TCustomType(NR.ok (PT2DT.Search.SearchQuery.typeName ()), []))
            "" ]
      returnType =
        TTuple(TList TString, TList TString, [ TList TString; TList TString ])
      description =
        "Search, returning only names: (direct submodules, types, values, fns). Submodules are already
         reduced to the direct children of the query's module and sorted."
      fn =
        function
        | _, _, _, [| DString branchId; query as DRecord(_, _, _, _fields) |] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            // Through the branch overlay: a branch is its ops laid over main, not a scope to walk up.
            let pm = LibDB.PackageManager.ptForBranch (branchOfParam branchId)
            let! results = pm.search searchQuery

            // Mirrors `Query.getDirectSubmodules`: drop the current path, keep the next segment, dedupe,
            // sort. Reduced here rather than in Dark because at the root of a large store this is hundreds
            // of module paths collapsing to a handful of names, and the search already has them all.
            let depth = List.length searchQuery.currentModule
            // `List.skip` throws past the end; Dark's `List.drop` yields []. Match Dark.
            let rec dropN n (xs : List<string>) =
              if n <= 0 then
                xs
              else
                match xs with
                | [] -> []
                | _ :: rest -> dropN (n - 1) rest
            let submodules =
              results.submodules
              |> List.choose (fun modulePath ->
                match dropN depth modulePath with
                | next :: _ when next <> "" -> Some next
                | _ -> None)
              |> List.distinct
              |> List.sort

            let names (locations : List<PT.LocatedItem<'a>>) =
              locations |> List.map (fun i -> i.location.name)

            let toDList (xs : List<string>) =
              xs |> List.map DString |> Dval.list KTString

            return
              DTuple(
                toDList submodules,
                toDList (names results.types),
                [ toDList (names results.values); toDList (names results.fns) ]
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSearchNamesAndHashes" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TString "the branch to search on; \"\" is main"
          Param.make
            "query"
            (TCustomType(NR.ok (PT2DT.Search.SearchQuery.typeName ()), []))
            "" ]
      returnType =
        let nameAndHash =
          TList(TTuple(TString, TCustomType(NR.ok (PT2DT.Hash.typeName ()), []), []))
        TTuple(TList TString, nameAndHash, [ nameAndHash; nameAndHash ])
      description =
        "Search, returning (direct submodules, types, values, fns) as (name, hash) pairs. Like
         pmSearchNames but keeps each item's hash, which listings need for deprecation marks."
      fn =
        function
        | _, _, _, [| DString branchId; query as DRecord(_, _, _, _fields) |] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            // Through the branch overlay: a branch is its ops laid over main, not a scope to walk up.
            let pm = LibDB.PackageManager.ptForBranch (branchOfParam branchId)
            let! results = pm.search searchQuery

            let depth = List.length searchQuery.currentModule
            // `List.skip` throws past the end; Dark's `List.drop` yields []. Match Dark.
            let rec dropN n (xs : List<string>) =
              if n <= 0 then
                xs
              else
                match xs with
                | [] -> []
                | _ :: rest -> dropN (n - 1) rest
            let submodules =
              results.submodules
              |> List.choose (fun modulePath ->
                match dropN depth modulePath with
                | next :: _ when next <> "" -> Some next
                | _ -> None)
              |> List.distinct
              |> List.sort

            let pairKT =
              KTTuple(VT.string, ValueType.Known(PT2DT.Hash.knownType ()), [])

            let toDList (xs : List<string>) =
              xs |> List.map DString |> Dval.list KTString

            let pairs
              (locations : List<PT.LocatedItem<'a>>)
              (hashOf : 'a -> PT.Hash)
              =
              locations
              |> List.map (fun i ->
                DTuple(
                  DString i.location.name,
                  PT2DT.Hash.toDT (hashOf i.entity),
                  []
                ))
              |> Dval.list pairKT

            return
              DTuple(
                toDList submodules,
                pairs results.types (fun (t : PT.PackageType.PackageType) -> t.hash),
                [ pairs results.values (fun (v : PT.PackageValue.PackageValue) ->
                    v.hash)
                  pairs results.fns (fun (f : PT.PackageFn.PackageFn) -> f.hash) ]
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Location lookups — returns ALL locations for a hash
    // Hands the compiled instruction stream to Dark so it can be rendered there
    // (`PrettyPrinter.RuntimeTypes.instructions`). Data only: no formatting happens in F#, because a
    // disassembly listing is exactly the kind of thing Dark should own. The PT side needs no equivalent
    // builtin -- `pmGetFn` already returns the tree.
    { name = fn "pmFnInstructions" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (
          TCustomType(NR.ok (RT2DT.Instructions.typeName ()), [])
        )
      description =
        "Returns the register-machine instructions a package function compiles to, "
        + "or None if there's no such function."
      fn =
        (function
        | exeState, _, _, [| hashDval |] ->
          uply {
            let (PT.Hash hashStr) = PT2DT.Hash.fromDT hashDval
            match! exeState.fns.package (Hash hashStr) with
            | None -> return Dval.optionNone (RT2DT.Instructions.knownType ())
            | Some rtFn ->
              return
                RT2DT.Instructions.toDT rtFn.body
                |> Dval.optionSome (RT2DT.Instructions.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByType" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
      description = "Returns all locations of a package type by its hash"
      fn =
        (function
        | _, _, _, [| DString branchId; hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! fromMain = pm.getTypeLocations hash
            // Branch overlay next: a branch's SetNames never fold into `locations`, so without this a
            // branch-authored item has no name to render.
            let named =
              LibDB.PackageManager.locationsFor
                (branchOfParam branchId)
                PT.ItemKind.Type
                hash
                fromMain
            // Last resort, a name it USED to have. Reached when viewing a superseded version, whose name
            // has moved on to the newer one -- `<hash:d6f972b3>` says nothing about what you're looking at.
            let! result =
              if List.isEmpty named then
                PMPT.Type.getLocationsEverNamed hash
              else
                Ply named
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByValue" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
      description = "Returns all locations of a package value by its hash"
      fn =
        (function
        | _, _, _, [| DString branchId; hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! fromMain = pm.getValueLocations hash
            // Branch overlay next: a branch's SetNames never fold into `locations`, so without this a
            // branch-authored item has no name to render.
            let named =
              LibDB.PackageManager.locationsFor
                (branchOfParam branchId)
                PT.ItemKind.Value
                hash
                fromMain
            // Last resort, a name it USED to have. Reached when viewing a superseded version, whose name
            // has moved on to the newer one -- `<hash:d6f972b3>` says nothing about what you're looking at.
            let! result =
              if List.isEmpty named then
                PMPT.Value.getLocationsEverNamed hash
              else
                Ply named
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByFn" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
      description = "Returns all locations of a package function by its hash"
      fn =
        (function
        | _, _, _, [| DString branchId; hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! fromMain = pm.getFnLocations hash
            // Branch overlay next: a branch's SetNames never fold into `locations`, so without this a
            // branch-authored item has no name to render.
            let named =
              LibDB.PackageManager.locationsFor
                (branchOfParam branchId)
                PT.ItemKind.Fn
                hash
                fromMain
            // Last resort, a name it USED to have. Reached when viewing a superseded version, whose name
            // has moved on to the newer one -- `<hash:d6f972b3>` says nothing about what you're looking at.
            let! result =
              if List.isEmpty named then
                PMPT.Fn.getLocationsEverNamed hash
              else
                Ply named
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Get ALL previous (deprecated) hashes at a location - used for propagation
    { name = fn "pmGetAllPreviousHashes" 0
      typeParams = []
      parameters =
        [ Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            ""
          Param.make
            "itemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, or value" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Returns all hashes that have ever been at a location across the branch chain"
      fn =
        (function
        | _, _, _, [| location; itemKindDval |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let itemKind = PT2DT.ItemKind.fromDT itemKindDval
            let modulesStr = location.modules |> String.concat "."
            let! result =
              LibDB.Queries.getAllPreviousHashes
                location.owner
                modulesStr
                location.name
                (itemKind.toString ())
            return
              result
              |> List.map PT2DT.Hash.toDT
              |> Dval.list (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Bind a name back to content that ALREADY exists in the store.
    //
    // This is the primitive under the propagation toggle. Propagation runs on every edit, so pinning
    // something is usually a request to UNDO a repoint that already happened -- and undoing it means
    // pointing the name back at the version it had, which is still in the store because nothing is ever
    // deleted.
    //
    // It emits a RESOLVE op, not a SetName, and that is not an implementation detail. Ops are
    // content-addressed, so `SetName(name -> the old hash)` is byte-identical to the op that first bound it:
    // it INSERT-OR-IGNOREs and folds NOTHING, so the rollback silently doesn't happen. Resolve exists
    // precisely to say "this binding again, but now I mean it", and a pin is the same act as a conflict
    // override -- a human overruling what the machine picked.
    //
    // Emitting an op rather than touching `locations` is what makes the undo sync, audit and conflict like
    // any other authoring. Deleting ops instead would do none of that, and would be unsafe besides: a pinned
    // dependent may be the only thing still referencing the version being deleted.
    { name = fn "pmSetName" 0
      typeParams = []
      parameters =
        [ Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "The name to bind"
          Param.make
            "itemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, or value"
          Param.make
            "hash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Existing content to bind the name to" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Binds a name to content already in the store, as a SetName op. Errors if the content isn't there."
      fn =
        (function
        | _, _, _, [| locationDval; itemKindDval; hashDval |] ->
          uply {
            let loc = PT2DT.PackageLocation.fromDT locationDval
            let kind = PT2DT.ItemKind.fromDT itemKindDval
            let hash = PT2DT.Hash.fromDT hashDval

            // Refuse to bind a name to content that isn't there. A dangling binding folds into a location
            // row that resolves to nothing, which fails at CALL time, far from the cause.
            let! exists =
              uply {
                match kind with
                | PT.ItemKind.Type ->
                  let! t = LibDB.PackageManager.pt.getType hash
                  return Option.isSome t
                | PT.ItemKind.Fn ->
                  let! f = LibDB.PackageManager.pt.getFn hash
                  return Option.isSome f
                | PT.ItemKind.Value ->
                  let! v = LibDB.PackageManager.pt.getValue hash
                  return Option.isSome v
              }

            if not exists then
              return
                DString "no item with that hash in the store"
                |> Dval.resultError KTUnit KTString
            else
              let reference =
                match kind with
                | PT.ItemKind.Type -> PT.Reference.PackageType hash
                | PT.ItemKind.Fn -> PT.Reference.PackageFn hash
                | PT.ItemKind.Value -> PT.Reference.PackageValue hash

              // The decision id makes the op distinct from the SetName that originally created this
              // binding. It's provenance, never a lookup key -- the fold ignores it.
              //
              // It carries a TIMESTAMP, so pinning the same name back to the same hash twice produces two
              // ops rather than one. That's deliberate and it's where a pin differs from a conflict
              // resolution: resolving conflict #7 the same way twice is one decision stated twice, but
              // pin -> follow -> pin is genuinely three, and the third has to fold or the rollback silently
              // doesn't happen. Without the stamp the op is byte-identical to the first pin's, and
              // content-addressed ops dedup.
              let decisionId =
                let mods = String.concat "." loc.modules
                let now = System.DateTime.UtcNow.ToString("o")
                $"pin:{loc.owner}.{mods}.{loc.name}:{hash}:{now}"

              let ops = [ PT.PackageOp.Resolve(decisionId, loc, reference) ]

              // Same branch source authoring uses, so a rebind lands where the edit that caused it landed.
              //
              // This one takes no branch parameter and uses the process default deliberately: `pmSetName`
              // is called as part of authoring, which is already happening on whatever branch the caller is
              // on. A parameter here would be a second way to say the same thing, and the two could differ.
              match LibDB.PackageManager.currentBranchId () with
              | None ->
                let! _ = LibDB.Inserts.insertAndApplyOps ops
                ()
              | Some branchId ->
                let! _ = LibDB.Branches.storeDeltaOps branchId ops
                let! parentId = LibDB.Branches.parentOf branchId
                do! LibDB.Branches.recordNameBases branchId parentId ops

              return Dval.resultOk KTUnit KTString DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Execute propagation of an update to all dependents
    { name = fn "pmPropagate" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to resolve against; \"\" is main. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"
          Param.make
            "sourceLocation"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "Location of the updated item"
          Param.make
            "sourceItemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, or value"
          Param.make
            "fromSourceHashes"
            (TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])))
            "All deprecated hashes at this location"
          Param.make
            "toSourceHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "New hash of the source item" ]
      returnType =
        TypeReference.result
          (TList(TCustomType(NR.ok (PT2DT.PropagateRepoint.typeName ()), [])))
          TString
      description =
        "Propagates an update to all dependents, creating new versions with updated references. Returns the repoints it made."
      fn =
        (function
        | _,
          _,
          _,
          // the branchId param is vestigial (always main's Uuid); the real branch is process state
          [| DString branchId
             sourceLocation
             sourceItemKindDval
             DList(_, fromSourceHashDvals)
             toSourceHashDval |] ->
          uply {
            let sourceLocation = PT2DT.PackageLocation.fromDT sourceLocation
            let sourceItemKind = PT2DT.ItemKind.fromDT sourceItemKindDval
            let fromSourceHashes = fromSourceHashDvals |> List.map PT2DT.Hash.fromDT

            // The BRANCH context is the process's, not the (vestigial, always-main) Uuid parameter --
            // same source `scmAddOps` uses, so authoring and propagation can't disagree about where they are.
            let branch = branchOfParam branchId

            let! result =
              LibDB.Propagation.propagate
                branch
                sourceLocation
                sourceItemKind
                fromSourceHashes
                (PT2DT.Hash.fromDT toSourceHashDval)

            match result with
            | Ok(Some(propagationResult, ops)) ->
              match branch with
              | None ->
                // Marked as PROPAGATED, not authored. It's the only point at which the difference is known.
                let! _ = LibDB.Inserts.insertAndApplyPropagatedOps ops
                ()
              | Some branchId ->
                // On a branch the repoints are BRANCH ops: stored effective=0 and tagged to the frontier,
                // never folded into main's `locations`. That's the isolation guarantee -- a cascade that
                // leaked into main would be worse than one that didn't happen.
                //
                // A repoint can create a branch-local version of a MAIN item (main's `dep` gets a branch
                // copy pointing at the branch's `base`, main's copy untouched). Recording name bases for
                // them is what lets a later merge tell that apart from a divergence.
                let! _ = LibDB.Branches.storeDeltaOps branchId ops
                let! parentId = LibDB.Branches.parentOf branchId
                do! LibDB.Branches.recordNameBases branchId parentId ops
                // Fold the CONTENT (never the SetNames) so the new versions resolve and carry their
                // dependency edges, exactly as branch authoring does.
                let contentOps =
                  ops
                  |> List.filter (fun op ->
                    match op with
                    | PT.PackageOp.AddValue _
                    | PT.PackageOp.AddFn _
                    | PT.PackageOp.AddType _ -> true
                    | _ -> false)
                if not (List.isEmpty contentOps) then
                  do! LibDB.PackageOpPlayback.applyOps contentOps
                // Refresh the process overlay so a later eval in THIS process sees the repoints.
                let! all = LibDB.Branches.loadDeltaOps branchId
                LibDB.PackageManager.setBranchOverlay all

              let repointsDval =
                propagationResult.repoints
                |> List.map PT2DT.PropagateRepoint.toDT
                |> Dval.list (PT2DT.PropagateRepoint.knownType ())

              return Dval.resultOk repointListKT KTString repointsDval
            | Ok None ->
              // No dependents: nothing moved, which is a result, not an error.
              return
                Dval.resultOk
                  repointListKT
                  KTString
                  (Dval.list (PT2DT.PropagateRepoint.knownType ()) [])
            | Error errMsg ->
              return Dval.resultError repointListKT KTString (DString errMsg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }



    // Deprecation info used by ls/tree/search in a single DB round-trip:
    // (allDeprecatedHashes, hiddenHashes). Hidden = deprecated AND has no
    // live direct caller (a caller is "live" iff it's not itself deprecated).
    { name = fn "pmGetDeprecationSets" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TTuple(
          TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])),
          TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])),
          []
        )
      description =
        "Tuple (allDeprecated, hidden) of package hashes. Not branch-scoped: deprecation keys on
        CONTENT, so it applies to a hash wherever that hash is named. "
        + "hidden ⊆ allDeprecated — deprecated items with no live direct caller."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            let! sets = LibDB.Queries.getDeprecationSets ()
            let hashListDval (hashes : Set<PT.Hash>) =
              hashes
              |> Set.toList
              |> List.map PT2DT.Hash.toDT
              |> Dval.list (PT2DT.Hash.knownType ())
            return
              DTuple(hashListDval sets.allDeprecated, hashListDval sets.hidden, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Current deprecation state for a package item on a branch chain.
    // None = not deprecated on this chain (or explicitly un-deprecated by a
    // child branch). Some (kind, message) returns the Dark-side
    // DeprecationKind enum so callers can format it however they want.
    { name = fn "pmGetCurrentDeprecation" 0
      typeParams = []
      parameters =
        [ Param.make "itemHash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) ""
          Param.make
            "itemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, or value" ]
      returnType =
        TypeReference.option (
          TTuple(
            TCustomType(NR.ok (PT2DT.DeprecationKind.typeName ()), []),
            TString,
            []
          )
        )
      description =
        "Current deprecation state for an item, by hash. Not branch-scoped: deprecation keys on
        CONTENT, so two names holding the same bytes are deprecated together. "
        + "None = not deprecated. Some ((kind, message)) otherwise."
      fn =
        (function
        | _, _, _, [| hashDval; itemKindDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let itemKind = PT2DT.ItemKind.fromDT itemKindDval
            let! result = LibDB.Queries.getCurrentDeprecation hash itemKind
            let tupleKT =
              KTTuple(
                VT.known (PT2DT.DeprecationKind.knownType ()),
                VT.known KTString,
                []
              )
            match result with
            | None -> return Dval.optionNone tupleKT
            | Some(kind, message) ->
              return
                Dval.optionSome
                  tupleKT
                  (DTuple(PT2DT.DeprecationKind.toDT kind, DString message, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
