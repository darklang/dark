/// The F# `ProgramTypes` and its Dark mirror (`languageTools/programTypes.dark`) are two hand-written
/// copies of the same shapes. This fails the build when they stop matching.
///
/// Drift is not a compile error. `ProgramTypesToDarkTypes` pairs cases up by NAME and field POSITION, so a
/// case on one side only crashes a converter at runtime, whenever someone first reaches it.
///
/// Scope is the pinned ABI types -- what the F# kernel embeds in the values it builds, the
/// `type/LanguageTools.ProgramTypes.*` entries in package-ref-hashes.txt. Unpinned helpers may differ.
module Tests.PTConformance

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes

let private isRecord (t : System.Type) = Reflection.FSharpType.IsRecord t
let private isUnion (t : System.Type) = Reflection.FSharpType.IsUnion t

/// The Dark-side names allowed to differ, and why. An entry here is a decision to tolerate drift.
let private accepted : Map<string, string> = Map.empty

let private pinPrefix = "type/LanguageTools.ProgramTypes."


/// The pinned ABI types, read from the same file the kernel reads. Reading the pins rather than a
/// hand-list means this can't quietly stop covering a type someone adds.
let private pinnedProgramTypes () : List<string * string> =
  let path =
    System.IO.Path.Combine(
      __SOURCE_DIRECTORY__,
      "..",
      "..",
      "src",
      "LibExecution",
      "package-ref-hashes.txt"
    )
    |> System.IO.Path.GetFullPath

  if not (System.IO.File.Exists path) then
    []
  else
    System.IO.File.ReadAllLines path
    |> Array.toList
    |> List.choose (fun line ->
      match line.Trim().Split('|') with
      | [| fqn; hash |] when fqn.StartsWith pinPrefix ->
        Some(fqn.Substring pinPrefix.Length, hash)
      | _ -> None)


/// Every type LibExecution defines under ProgramTypes, keyed by its name relative to that module, so
/// `FQFnName.Builtin` finds `PT.FQFnName.Builtin`. Scanned rather than hand-listed, so moving a type
/// can't silently drop it from the check.
let private fsharpTypesByName () : Map<string, System.Type> =
  let marker = "LibExecution.ProgramTypes."

  // Nested F# types are `Module+Nested`, and generics carry an arity suffix the Dark name doesn't have.
  let relativeName (t : System.Type) : Option<string> =
    let full = if isNull t.FullName then "" else t.FullName
    let dotted = full.Replace("+", ".")

    let dotted =
      match dotted.IndexOf '`' with
      | -1 -> dotted
      | i -> dotted.Substring(0, i)

    if dotted.StartsWith marker then
      let rel = dotted.Substring marker.Length
      // Compiler-generated companions aren't part of the shape.
      if rel.Contains "@" || rel.EndsWith ".Tags" then None else Some rel
    else
      None

  typeof<PT.PackageLocation>.Assembly.GetTypes()
  |> Array.toList
  |> List.choose (fun t ->
    match relativeName t with
    | Some n -> Some(n, t)
    | None -> None)
  // A name can collide: a module's static class and a type of the same name. Shaped ones sort last, and
  // `Map.ofList` keeps the last, so the collision resolves towards the type the Dark mirror mirrors.
  |> List.sortBy (fun (_, t) -> if isRecord t || isUnion t then 1 else 0)
  |> Map.ofList


/// The F# type a Dark name refers to, allowing for two conventions that differ deliberately:
///   - F# writes a module's principal type as `Module.T`; Dark writes the bare name, or `Module.Module`
///   - F# type ABBREVIATIONS are erased at compile time, so an alias legitimately has no F# type at all
let private findFsharp
  (types : Map<string, System.Type>)
  (darkName : string)
  : Option<System.Type> =
  let principal =
    match darkName.Split('.') |> Array.toList with
    | [ outer; inner ] when outer = inner -> [ outer + ".T" ]
    | _ -> []

  let found =
    (darkName :: (darkName + ".T") :: principal)
    |> List.choose (fun c -> Map.tryFind c types)

  // Prefer one that carries a shape: `DB` resolves to the module's static class as well as to `DB.T`,
  // and the module class would compare as a shapeless alias against Dark's record.
  match found |> List.tryFind (fun t -> isRecord t || isUnion t) with
  | Some t -> Some t
  | None -> List.tryHead found


/// A type's shape in the only terms worth comparing: what kind it is, and its members with their field
/// counts. Field TYPES are deliberately not compared -- the two sides spell them differently by design
/// (NEList vs List, Int64 vs Int). Names and arity are where the converters actually break.
type private Shape =
  { kind : string // "record", "union" or "alias"
    members : Map<string, int> } // field name -> 0, or case name -> field count

let private alias = { kind = "alias"; members = Map.empty }

let private fsharpShape (t : System.Type) : Shape =
  if isRecord t then
    { kind = "record"
      members =
        Reflection.FSharpType.GetRecordFields t
        |> Array.map (fun f -> (f.Name, 0))
        |> Map.ofArray }
  elif isUnion t then
    { kind = "union"
      members =
        Reflection.FSharpType.GetUnionCases t
        |> Array.map (fun c -> (c.Name, c.GetFields().Length))
        |> Map.ofArray }
  else
    alias

let private darkShape (decl : PT.TypeDeclaration.T) : Shape =
  match decl.definition with
  | PT.TypeDeclaration.Definition.Alias _ -> alias
  | PT.TypeDeclaration.Definition.Record fields ->
    { kind = "record"
      members =
        fields |> NEList.toList |> List.map (fun f -> (f.name, 0)) |> Map.ofList }
  | PT.TypeDeclaration.Definition.Enum cases ->
    { kind = "union"
      members =
        cases
        |> NEList.toList
        |> List.map (fun c -> (c.name, List.length c.fields))
        |> Map.ofList }


/// One complaint, or None when the two agree.
let private drift (name : string) (fs : Shape) (dark : Shape) : Option<string> =
  let names (s : Shape) = s.members |> Map.keys |> Set.ofSeq
  let onlyIn a b = Set.difference (names a) (names b) |> Set.toList

  if fs.kind <> dark.kind then
    Some $"{name}: F# is a {fs.kind}, Dark is a {dark.kind}"
  elif names fs <> names dark then
    Some
      $"{name}: members differ -- only in F#: {onlyIn fs dark}; only in Dark: {onlyIn dark fs}"
  else
    // Same members, so check arity: the converters marshal fields POSITIONALLY, and a case that gained
    // one on a single side is a crash waiting for whoever reaches it.
    let mismatched =
      names fs
      |> Set.toList
      |> List.choose (fun m ->
        let a = Map.findUnsafe m fs.members
        let b = Map.findUnsafe m dark.members
        if a = b then None else Some $"{m} (F# {a}, Dark {b})")

    if List.isEmpty mismatched then
      None
    else
      Some $"""{name}: field counts differ -- {String.concat ", " mismatched}"""


let conformance =
  testTask
    "PT/RT conformance: the F# kernel and the Dark mirror agree on the pinned ABI shapes" {
    let pinned = pinnedProgramTypes ()

    // An empty pin file (a fresh clone, before reload-packages) would make this vacuously pass, which is
    // worse than failing: green while checking nothing.
    Expect.isGreaterThan
      (List.length pinned)
      10
      "expected the ABI pins to be populated (run reload-packages)"

    let fsharpTypes = fsharpTypesByName ()
    let mutable problems : List<string> = []

    for (name, hash) in pinned do
      if not (Map.containsKey name accepted) then
        let! darkType = LibDB.ProgramTypes.Type.get (PT.Hash hash) |> Ply.toTask

        match darkType, findFsharp fsharpTypes name with
        | None, _ ->
          problems <-
            $"{name}: pinned hash {hash} isn't in the store -- the pin and the packages disagree"
            :: problems

        // No F# type is EXPECTED when the Dark side is an alias, since abbreviations are erased.
        // Anything else pinned but F#-less is real: the pin exists because F# embeds it.
        | Some(dt : PT.PackageType.PackageType), None ->
          match darkShape dt.declaration with
          | s when s.kind = "alias" -> ()
          | s ->
            problems <-
              $"{name}: pinned as an ABI type but has no F# counterpart (Dark is a {s.kind})"
              :: problems

        | Some(dt : PT.PackageType.PackageType), Some fsType ->
          match drift name (fsharpShape fsType) (darkShape dt.declaration) with
          | Some problem -> problems <- problem :: problems
          | None -> ()

    if not (List.isEmpty problems) then
      let listed = problems |> List.rev |> String.concat "\n  "
      Expect.isTrue
        false
        $"F#/Dark ProgramTypes drift ({List.length problems}):\n  {listed}\n\nFix the mirror, or add an entry to `accepted` with the reason."
  }


let tests = testList "PTConformance" [ conformance ]
