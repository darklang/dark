/// Read and check reference names before saving operations.
module LibDB.ReferenceNamesContent

open System.Threading.Tasks
open Fumble
open Prelude
open LibDB.Sqlite
module PT = LibExecution.ProgramTypes
module AT = LibDB.AstTransformer
module BS = LibSerialization.Binary.Serialization

type Names = List<PT.Reference * Option<PT.PackageLocation>>

/// We read these names back in the same AST traversal order.
/// Keep repeated uses so each reference gets its own saved name.
let private captureWithResult visit item =
  let names = ResizeArray<_>()
  let collect reference location =
    reference |> Option.iter (fun reference -> names.Add(reference, location))
    location
  let result = visit { AT.emptyMapping with referenceName = Some collect } item
  result, List.ofSeq names

let capture visit item : Names = captureWithResult visit item |> snd

let private visitDeclaration mapping op =
  let visit transform item target =
    let _ = transform mapping item
    Some target
  match op with
  | PT.PackageOp.AddType item ->
    visit AT.transformType item (PT.PackageType item.hash)
  | PT.PackageOp.AddFn item -> visit AT.transformFn item (PT.PackageFn item.hash)
  | PT.PackageOp.AddValue item ->
    visit AT.transformValue item (PT.PackageValue item.hash)
  | PT.PackageOp.AddTrait item ->
    visit AT.transformTrait item (PT.PackageTrait item.hash)
  | PT.PackageOp.AddTraitImpl item ->
    visit AT.transformImpl item (PT.PackageTraitImpl item.hash)
  | PT.PackageOp.AddTest item ->
    visit AT.transformTest item (PT.PackageTest item.hash)
  | _ -> None

/// Collect reference names from an `AddFn`, `AddType`, or another declaration op.
let declaration op =
  let target, names = captureWithResult visitDeclaration op
  target |> Option.map (fun target -> target, names)

/// A call to `later()` may not resolve until `later` has been saved.
/// Keep its place in the AST so resolving it does not move the saved names
/// for other references onto the wrong calls.
let private referenceSlots op =
  let slots = ResizeArray<_>()
  let collect reference location =
    slots.Add(reference |> Option.map (fun reference -> reference, location))
    location
  let _ = visitDeclaration { AT.emptyMapping with referenceName = Some collect } op
  List.ofSeq slots

/// For `later() + twice()`, resolving `later` adds its name while keeping
/// `twice` attached to the second call. Match references by their AST positions:
/// two calls can have the same target hash and still use different aliases.
let refreshNames before after =
  let oldIndexes, oldCount =
    referenceSlots before
    |> List.mapFold
      (fun count old ->
        match old with
        | Some _ -> (Some count, count + 1)
        | None -> (None, count))
      0
  let positions =
    List.map2
      (fun index current ->
        current
        |> Option.map (fun (reference, location) -> reference, location, index))
      oldIndexes
      (referenceSlots after)
    |> List.choose (fun slot -> slot)
  fun (names : Names) ->
    let names = List.toArray names
    if names.Length <> oldCount then
      Exception.raiseInternal
        "Reference snapshot does not match its original declaration"
        []
    positions
    |> List.map (fun (reference, location, index) ->
      reference,
      match index with
      | Some index -> snd names[index]
      | None -> location)

let private read table deserialize visit (hash : PT.Hash) =
  let (PT.Hash value) = hash
  let bytes =
    Sql.query $"SELECT pt_def FROM {table} WHERE hash = @hash"
    |> Sql.parameters [ "hash", Sql.string value ]
    |> Sql.executeRowOption (fun row -> row.bytes "pt_def")
  bytes |> Option.map (deserialize hash >> capture visit)

/// Read a stored declaration and collect the package names its references use.
let storedNames target : Option<Names> =
  match target with
  | PT.PackageType hash ->
    read "package_types" BS.PT.PackageType.deserialize AT.transformType hash
  | PT.PackageFn hash ->
    read "package_functions" BS.PT.PackageFn.deserialize AT.transformFn hash
  | PT.PackageValue hash ->
    read "package_values" BS.PT.PackageValue.deserialize AT.transformValue hash
  | PT.PackageTrait hash ->
    read "package_traits" BS.PT.Trait.deserialize AT.transformTrait hash
  | PT.PackageTraitImpl hash ->
    read "package_trait_impls" BS.PT.TraitImpl.deserialize AT.transformImpl hash
  | PT.PackageTest hash ->
    read "package_tests" BS.PT.PackageTest.deserialize AT.transformTest hash

/// `storedNames` of each distinct target, read once each, in order.
let storedNamesEach
  (targets : List<PT.Reference>)
  : List<PT.Reference * Option<Names>> =
  targets
  |> List.distinct
  |> List.map (fun target ->
    let names = storedNames target
    (target, names))

/// Read the names from stored declaration bodies, before branch metadata is applied.
let storedSnapshots (sources : List<PT.PackageLocation * PT.Reference>) =
  let sources = List.distinct sources
  let originals = sources |> List.map snd |> storedNamesEach
  let originals = Map.ofList originals
  sources
  |> List.choose (fun (location, target) ->
    Microsoft.FSharp.Collections.Map.find target originals
    |> Option.map (fun names ->
      ({ location = location; target = target; names = names; restating = None }
      : PT.ReferenceNamesSnapshot)))

/// Check the snapshots before any operations are saved.
/// Each snapshot must list the declaration's references in the same order.
/// The declaration can already be stored or be part of this batch.
let validate (ops : List<PT.PackageOp>) : Option<string> =
  let snapshots = ops |> List.collect PT.packageOpReferenceSnapshots
  if List.isEmpty snapshots then
    None
  else
    let bodies =
      ops
      |> List.choose declaration
      |> List.fold
        (fun map (target, names) ->
          if Map.containsKey target map then map else Map.add target names map)
        Map.empty
    // Stops reading at the first problem.
    let _, problem =
      snapshots
      |> List.fold
        (fun (storedByTarget, problem) snapshot ->
          if Option.isSome problem then
            (storedByTarget, problem)
          else
            let target = snapshot.target
            let names = snapshot.names
            let storedByTarget =
              if Map.containsKey target storedByTarget then
                storedByTarget
              else
                let stored = storedNames target
                Map.add target stored storedByTarget
            let stored = Microsoft.FSharp.Collections.Map.find target storedByTarget
            let original =
              stored |> Option.orElseWith (fun () -> Map.tryFind target bodies)
            let problem =
              match original with
              | None ->
                Some "reference snapshot names a declaration that is not available"
              | Some original when List.map fst original <> List.map fst names ->
                Some
                  "reference snapshot does not match the declaration's references"
              | Some _ -> None
            (storedByTarget, problem))
        (Map.empty, None)
    problem
