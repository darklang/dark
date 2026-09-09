/// Reading and writing the prose a `DocPart` names, and the key a location's doc is stored under.
///
/// One implementation, shared by the fold (which reaches through a serialized blob) and by the
/// branch overlay (which reaches through a loaded item). Two would drift, and it is the READ that
/// decides whether an incoming edit descends from what a store holds or diverges from it -- so a
/// disagreement between them would be a disagreement about what counts as a conflict.
///
/// Every read answers `None` when the part does not fit: a record field on a function, a parameter
/// the signature does not have. That is what makes an op for a part that no longer exists fold to
/// nothing rather than raise, which matters because such an op can only arrive from another
/// instance and one bad op must not refuse the batch it came in.
module LibDB.Docs

open System.Threading.Tasks
open FSharp.Control.Tasks
open Fumble

open Prelude
open LibExecution.ProgramTypes
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes


// ---------------------
// The storage key: (location, kind, within)
// ---------------------

/// Which KIND of part, as `location_docs` stores it.
let kind (part : PT.DocPart) : string =
  match part with
  | PT.WholeItem -> "item"
  | PT.RecordField _ -> "record-field"
  | PT.EnumCase _ -> "enum-case"
  | PT.Parameter _ -> "parameter"

/// WHICH part, by its name in the declaration; "" for the declaration's own doc. With `kind`, and
/// the location, the key.
let within (part : PT.DocPart) : string =
  match part with
  | PT.WholeItem -> ""
  | PT.RecordField n
  | PT.EnumCase n -> n
  | PT.Parameter i -> string i

/// How it reads in a listing or a conflict's reason.
let describe (part : PT.DocPart) : string =
  match part with
  | PT.WholeItem -> "the item"
  | PT.RecordField n -> $"field {n}"
  | PT.EnumCase n -> $"case {n}"
  | PT.Parameter i -> $"parameter {i + 1}"


// ---------------------
// The lens
// ---------------------

let private named
  (name : string)
  (nameOf : 'a -> string)
  (docOf : 'a -> string)
  (items : NEList<'a>)
  : Option<string> =
  items |> NEList.find (fun i -> nameOf i = name) |> Option.map docOf

let private redoc
  (name : string)
  (nameOf : 'a -> string)
  (setDoc : 'a -> 'a)
  (items : NEList<'a>)
  : NEList<'a> =
  items |> NEList.map (fun i -> if nameOf i = name then setDoc i else i)


let inFn (part : PT.DocPart) (fn : PT.PackageFn.PackageFn) : Option<string> =
  match part with
  | PT.WholeItem -> Some fn.description
  | PT.Parameter index ->
    fn.parameters
    |> NEList.toList
    |> List.tryItem index
    |> Option.map (fun p -> p.description)
  | PT.RecordField _
  | PT.EnumCase _ -> None

let onFn
  (part : PT.DocPart)
  (text : string)
  (fn : PT.PackageFn.PackageFn)
  : PT.PackageFn.PackageFn =
  match part with
  | PT.WholeItem -> { fn with description = text }
  | PT.Parameter index ->
    { fn with
        parameters =
          fn.parameters
          |> NEList.mapWithIndex (fun i p ->
            if i = index then { p with description = text } else p) }
  | PT.RecordField _
  | PT.EnumCase _ -> fn


let inType (part : PT.DocPart) (t : PT.PackageType.PackageType) : Option<string> =
  match part, t.declaration.definition with
  | PT.WholeItem, _ -> Some t.description
  | PT.RecordField name, PT.TypeDeclaration.Record fields ->
    named
      name
      (fun (f : PT.TypeDeclaration.RecordField) -> f.name)
      (fun f -> f.description)
      fields
  | PT.EnumCase name, PT.TypeDeclaration.Enum cases ->
    named
      name
      (fun (c : PT.TypeDeclaration.EnumCase) -> c.name)
      (fun c -> c.description)
      cases
  | _ -> None

let onType
  (part : PT.DocPart)
  (text : string)
  (t : PT.PackageType.PackageType)
  : PT.PackageType.PackageType =
  let redefine definition =
    { t with declaration = { t.declaration with definition = definition } }

  match part, t.declaration.definition with
  | PT.WholeItem, _ -> { t with description = text }
  | PT.RecordField name, PT.TypeDeclaration.Record fields ->
    redoc
      name
      (fun (f : PT.TypeDeclaration.RecordField) -> f.name)
      (fun f -> { f with description = text })
      fields
    |> PT.TypeDeclaration.Record
    |> redefine
  | PT.EnumCase name, PT.TypeDeclaration.Enum cases ->
    redoc
      name
      (fun (c : PT.TypeDeclaration.EnumCase) -> c.name)
      (fun c -> { c with description = text })
      cases
    |> PT.TypeDeclaration.Enum
    |> redefine
  | _ -> t


let inValue (part : PT.DocPart) (v : PT.PackageValue.PackageValue) : Option<string> =
  match part with
  | PT.WholeItem -> Some v.description
  | _ -> None

let onValue
  (part : PT.DocPart)
  (text : string)
  (v : PT.PackageValue.PackageValue)
  : PT.PackageValue.PackageValue =
  match part with
  | PT.WholeItem -> { v with description = text }
  | _ -> v


// ---------------------
// What a location says of its own
// ---------------------

/// The part a stored (kind, within) pair names, or None for a row this build cannot read.
let partOf (kind : string) (within : string) : Option<PT.DocPart> =
  match kind with
  | "item" -> Some PT.WholeItem
  | "record-field" -> Some(PT.RecordField within)
  | "enum-case" -> Some(PT.EnumCase within)
  | "parameter" ->
    match System.Int32.TryParse within with
    | true, i -> Some(PT.Parameter i)
    | _ -> None
  | _ -> None


/// Every doc <param location> has OF ITS OWN, keyed by (kind, within).
///
/// Empty for almost every name: a doc a name has not overridden lives in the declaration, which is
/// where a reader finds it without asking. This answers the other question -- "does this NAME say
/// something else" -- which is what makes ten `ParseError` types able to mean ten things.
///
/// Main's rows with the branch's on top. A branch's `UpdateDoc` never folds into `location_docs`
/// (that table is main's, the same way `locations` is), so the branch half is read from its ops.
let docsAt
  (branchId : PT.BranchId)
  (location : PT.PackageLocation)
  : Task<List<PT.DocPart * string>> =
  task {
    let modules = String.concat "." location.modules

    let! rows =
      Sql.query
        "SELECT kind, within, text FROM location_docs
         WHERE owner = @owner AND modules = @modules AND name = @name"
      |> Sql.parameters
        [ "owner", Sql.string location.owner
          "modules", Sql.string modules
          "name", Sql.string location.name ]
      |> Sql.executeAsync (fun read ->
        (read.string "kind", read.string "within", read.string "text"))

    let fromMain =
      rows
      |> List.choose (fun (k, w, text) ->
        partOf k w |> Option.map (fun part -> part, text))
      |> Map.ofList

    let! all =
      if branchId.IsMain then
        Task.FromResult fromMain
      else
        task {
          let! ops = Branches.chainOverlayOps branchId

          return
            ops
            |> List.fold
              (fun acc op ->
                match op with
                | PT.PackageOp.UpdateDoc(loc, part, text, _, _) when loc = location ->
                  Map.add part text acc
                | _ -> acc)
              fromMain
        }

    return Map.toList all
  }


/// The same, for MANY locations at once, as the map a search result is patched from.
///
/// One query, because a listing is a few hundred rows and a query each would be a few hundred
/// queries. Empty when no name in the batch has ever had a doc edited, which is the ordinary case
/// and the one that has to stay cheap.
let docsForLocations
  (branchId : PT.BranchId)
  (locations : List<PT.PackageLocation>)
  : Task<Map<PT.PackageLocation, List<PT.DocPart * string>>> =
  task {
    if List.isEmpty locations then
      return Map.empty
    else
      let keyed = locations |> List.distinct

      let keyOf (l : PT.PackageLocation) =
        String.concat "\u0000" [ l.owner; String.concat "." l.modules; l.name ]

      let keyParams =
        keyed |> List.mapi (fun i l -> ($"key_{i}", Sql.string (keyOf l)))

      let keyClause =
        keyed |> List.mapi (fun i _ -> $"@key_{i}") |> String.concat ", "

      let! rows =
        Sql.query
          $"""
          SELECT owner, modules, name, kind, within, text
          FROM location_docs
          WHERE owner || char(0) || modules || char(0) || name IN ({keyClause})
          """
        |> Sql.parameters keyParams
        |> Sql.executeAsync (fun read ->
          ((read.string "owner", read.string "modules", read.string "name"),
           (read.string "kind", read.string "within", read.string "text")))

      let byLocation =
        keyed
        |> List.choose (fun l ->
          let mine =
            rows
            |> List.filter (fun ((o, m, n), _) ->
              o = l.owner && m = String.concat "." l.modules && n = l.name)
            |> List.choose (fun (_, (k, w, text)) ->
              partOf k w |> Option.map (fun part -> part, text))

          if List.isEmpty mine then None else Some(l, mine))
        |> Map.ofList

      if branchId.IsMain then
        return byLocation
      else
        let! ops = Branches.chainOverlayOps branchId
        let wanted = Set.ofList keyed

        return
          ops
          |> List.fold
            (fun acc op ->
              match op with
              | PT.PackageOp.UpdateDoc(loc, part, text, _, _) when
                Set.contains loc wanted
                ->
                let standing =
                  Map.tryFind loc acc
                  |> Option.defaultValue []
                  |> List.filter (fun (p, _) -> p <> part)

                Map.add loc (standing @ [ part, text ]) acc
              | _ -> acc)
            byLocation
  }
