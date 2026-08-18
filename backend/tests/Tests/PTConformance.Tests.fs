/// PT/RT conformance: a build-failing drift alarm between the F# kernel's ProgramTypes and the Dark mirror
/// (`packages/darklang/languageTools/programTypes.dark`), which are two hand-maintained copies of the same
/// shapes.
///
/// It matters twice over:
///   - the converters (ProgramTypesToDarkTypes) marshal between the two by CASE NAME and field position. A
///     case that exists on one side and not the other is a runtime failure in a converter, not a type error.
///   - a pinned ABI type whose shape moved breaks the kernel-hash "identity <=> frozen structure" rule,
///     and this is where you'd find out.
///
/// Scope is the PINNED ABI types (the `type/LanguageTools.ProgramTypes.*` entries in package-ref-hashes.txt)
/// -- what the F# kernel embeds in the values it constructs. Unpinned helper types are free to differ.
module Tests.PTConformance

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module PackageRefs = LibExecution.PackageRefs


/// Known, accepted divergences: the name on the Dark side, and WHY it's allowed to differ. Anything not in
/// here is a failure, and an entry here is a decision to tolerate drift.
let private accepted : Map<string, string> = Map.empty


/// The pinned ABI type names, Dark-side (`LanguageTools.ProgramTypes.X`), read from the same committed pin
/// file the kernel reads. Using the pins rather than a hand-list means this can't quietly stop covering a
/// type someone adds to the ABI.
let private pinnedProgramTypes () : List<string * string> =
  // Same file the kernel reads. `__SOURCE_DIRECTORY__` is backend/tests/Tests, so walk up to backend/.
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
      | [| fqn; hash |] when fqn.StartsWith "type/LanguageTools.ProgramTypes." ->
        Some(fqn.Substring("type/LanguageTools.ProgramTypes.".Length), hash)
      | _ -> None)


/// Every type the LibExecution assembly defines under ProgramTypes, keyed by its dotted name relative to the
/// ProgramTypes module -- so `FQFnName.Builtin` finds the F# `PT.FQFnName.Builtin`. Scanned rather than
/// hand-listed: a hand-list would silently stop matching the moment someone moved a type.
let private fsharpTypesByName () : Map<string, System.Type> =
  let asm = typeof<PT.PackageLocation>.Assembly
  let marker = "LibExecution.ProgramTypes"

  asm.GetTypes()
  |> Array.toList
  |> List.choose (fun t ->
    let full = if isNull t.FullName then "" else t.FullName
    // Nested F# modules/types are `Namespace.Module+Nested+Type`; normalise both separators to dots.
    // Generics carry a runtime arity suffix (`NameResolution\`1`) that the Dark name doesn't have.
    let dotted = full.Replace("+", ".")
    let dotted =
      match dotted.IndexOf '`' with
      | -1 -> dotted
      | i -> dotted.Substring(0, i)

    if dotted.StartsWith(marker + ".") then
      let rel = dotted.Substring(marker.Length + 1)
      // Skip compiler-generated companions (`Tags`, closures) -- they aren't part of the shape.
      if rel.Contains "@" || rel.EndsWith ".Tags" then None else Some(rel, t)
    else
      None)
  // A name can collide (a module's static class and a type of the same name). Keep whichever actually
  // carries a shape, since that's the one the Dark mirror is mirroring.
  |> List.fold
    (fun (m : Map<string, System.Type>) (name, t) ->
      match Map.tryFind name m with
      | Some existing when
        Reflection.FSharpType.IsRecord existing
        || Reflection.FSharpType.IsUnion existing
        ->
        m
      | _ -> Map.add name t m)
    Map.empty


/// The F# type a Dark name refers to, allowing for two conventions that differ deliberately:
///   - F# writes a module's principal type as `Module.T`; Dark writes it as the bare name, or `Module.Module`
///   - F# type ABBREVIATIONS (`type Package = Hash`) are erased at compile time and have no runtime type at
///     all, so "no F# counterpart" is expected for anything the Dark side declares as an alias
let private findFsharp
  (types : Map<string, System.Type>)
  (darkName : string)
  : Option<System.Type> =
  let candidates =
    [ darkName
      darkName + ".T"
      // `TypeDeclaration.TypeDeclaration` -> `TypeDeclaration.T`
      (match darkName.Split('.') |> Array.toList with
       | [ outer; inner ] when outer = inner -> outer + ".T"
       | _ -> darkName) ]

  let found = candidates |> List.choose (fun c -> Map.tryFind c types)

  // Prefer a candidate that actually carries a shape. `DB` resolves to the MODULE's static class as well as
  // to `DB.T`, and the module class would compare as a shapeless alias against Dark's record.
  match
    found
    |> List.tryFind (fun t ->
      Reflection.FSharpType.IsRecord t || Reflection.FSharpType.IsUnion t)
  with
  | Some t -> Some t
  | None -> List.tryHead found


/// The F# shape of a type, in the only terms worth comparing: is it a record or a union, and what are its
/// member names. Field TYPES are deliberately not compared -- the two sides spell types differently by
/// design (NEList vs List, Int64 vs Int). Names and arity are where the converters actually break.
type private Shape =
  | RecordShape of Set<string>
  | UnionShape of Map<string, int> // case name -> field count
  | AliasShape
  | Unknown of string

let private fsharpShape (t : System.Type) : Shape =
  if Reflection.FSharpType.IsRecord(t) then
    Reflection.FSharpType.GetRecordFields(t)
    |> Array.map (fun f -> f.Name)
    |> Set.ofArray
    |> RecordShape
  elif Reflection.FSharpType.IsUnion(t) then
    Reflection.FSharpType.GetUnionCases(t)
    |> Array.map (fun c -> (c.Name, c.GetFields().Length))
    |> Map.ofArray
    |> UnionShape
  elif t.IsClass || t.IsValueType then
    AliasShape
  else
    Unknown t.Name

let private darkShape (decl : PT.TypeDeclaration.T) : Shape =
  match decl.definition with
  | PT.TypeDeclaration.Definition.Alias _ -> AliasShape
  | PT.TypeDeclaration.Definition.Record fields ->
    fields
    |> NEList.toList
    |> List.map (fun f -> f.name)
    |> Set.ofList
    |> RecordShape
  | PT.TypeDeclaration.Definition.Enum cases ->
    cases
    |> NEList.toList
    |> List.map (fun c -> (c.name, List.length c.fields))
    |> Map.ofList
    |> UnionShape


/// Compare one type. Returns a human-readable complaint, or None when they agree.
let private compare (name : string) (fs : Shape) (dark : Shape) : Option<string> =
  match fs, dark with
  | RecordShape a, RecordShape b ->
    if a = b then
      None
    else
      let onlyFs = Set.difference a b |> Set.toList
      let onlyDark = Set.difference b a |> Set.toList
      Some
        $"{name}: record fields differ -- only in F#: {onlyFs}; only in Dark: {onlyDark}"

  | UnionShape a, UnionShape b ->
    let aNames = a |> Map.keys |> Set.ofSeq
    let bNames = b |> Map.keys |> Set.ofSeq
    if aNames <> bNames then
      let onlyFs = Set.difference aNames bNames |> Set.toList
      let onlyDark = Set.difference bNames aNames |> Set.toList
      Some
        $"{name}: union cases differ -- only in F#: {onlyFs}; only in Dark: {onlyDark}"
    else
      // Same cases: check arity, since the converters marshal fields POSITIONALLY. A case that gained a
      // field on one side only is a converter crash waiting for whoever hits that case.
      let arityMismatches =
        aNames
        |> Set.toList
        |> List.choose (fun case ->
          let fsArity = Map.findUnsafe case a
          let darkArity = Map.findUnsafe case b
          if fsArity = darkArity then
            None
          else
            Some $"{case} (F# {fsArity} field(s), Dark {darkArity})")
      if List.isEmpty arityMismatches then
        None
      else
        Some
          $"""{name}: case arity differs -- {String.concat ", " arityMismatches}"""

  | AliasShape, AliasShape -> None
  // A record on one side and a union on the other is the loudest possible drift.
  | a, b -> Some $"{name}: kind differs -- F# is {a}, Dark is {b}"


let conformance =
  testTask
    "PT/RT conformance: the F# kernel and the Dark mirror agree on the pinned ABI shapes" {
    let pinned = pinnedProgramTypes ()

    // An empty pin file (fresh clone before reload-packages) would make this vacuously pass, which is worse
    // than failing: it would look green while checking nothing.
    Expect.isGreaterThan
      (List.length pinned)
      10
      "expected the ABI pins to be populated (run reload-packages)"

    let fsharpTypes = fsharpTypesByName ()
    let mutable problems : List<string> = []

    for (name, hash) in pinned do
      if Map.containsKey name accepted then
        ()
      else
        let! darkType = LibDB.ProgramTypes.Type.get (PT.Hash hash) |> Ply.toTask

        match darkType, findFsharp fsharpTypes name with
        | None, _ ->
          problems <-
            $"{name}: pinned hash {hash} isn't in the store -- the pin and the packages disagree"
            :: problems
        | Some(dt : PT.PackageType.PackageType), None ->
          // No runtime type is EXPECTED when the Dark side is an alias: F# type abbreviations are erased.
          // Anything else pinned but F#-less is real -- the pin exists because F# embeds it.
          match darkShape dt.declaration with
          | AliasShape -> ()
          | shape ->
            problems <-
              $"{name}: pinned as an ABI type but has no F# counterpart (Dark is {shape})"
              :: problems
        | Some(dt : PT.PackageType.PackageType), Some fsType ->
          match compare name (fsharpShape fsType) (darkShape dt.declaration) with
          | Some problem -> problems <- problem :: problems
          | None -> ()

    if not (List.isEmpty problems) then
      let listed = problems |> List.rev |> String.concat "\n  "
      Expect.isTrue
        false
        $"F#/Dark ProgramTypes drift ({List.length problems}):\n  {listed}\n\nFix the mirror, or add an entry to `accepted` with the reason."
  }


let tests = testList "PTConformance" [ conformance ]
