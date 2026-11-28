/// Apply edits to `packages/*.dark` to the dev store without a full reload.
///
/// A reload parses every file and fills the store twice. Most edits change a declaration or two.
/// This diffs each changed file against the text the store was loaded from, declaration by
/// declaration, and lands the changed ones through `dark import`'s own code
/// (`ModuleCommand.defineBundled`), so their dependents are repointed the way an authored edit
/// repoints them. Declarations removed from a file, or in a deleted file, are unbound, as a reload
/// drops them. Then it commits them as the baseline, as a reload does.
///
/// It refuses, and the build runs the full reload instead, when:
/// - the store has no record of what it was loaded from, or its ops changed since (your own `dark`
///   edits, say);
/// - something live still uses a removed declaration: the reload is what reports that;
/// - a trait or impl was added or removed, which can change how existing calls resolve;
/// - a type changed that more than `typeDependentsLimit` items depend on, since a type's dependents
///   are repointed without a check;
/// - the edits have more than `dependentsLimit` dependents in all, where a reload is no slower;
/// - a file does not parse, or `dark import` fails.
[<RequireQualifiedAccess>]
module LocalExec.PackageEdits

open System.Threading.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module WT = LibParser.WrittenTypes
module Exe = LibExecution.Execution
module PM = LibDB.PackageManager


let dependentsLimit = 5000L
let typeDependentsLimit = 50L

exception private Refused of string

let private refuse (reason : string) : 'a = raise (Refused reason)

let private dotted (parts : List<string>) = String.concat "." parts


// ------------------------------------------------------------------
// What the store was loaded from
// ------------------------------------------------------------------

let private recordPath () = LibConfig.Config.dbPath + ".package-sources.json"

let private root () = LibConfig.ConfigDsl.absoluteDir "DARK_CONFIG_ROOT_DIR"

let private sha256 (text : string) : string =
  System.Text.Encoding.UTF8.GetBytes text
  |> System.Security.Cryptography.SHA256.HashData
  |> System.Convert.ToHexString

/// Every op the store holds, in order: anything that writes one moves it.
let private fingerprint () : string =
  let rows =
    Sql.query "SELECT id || '|' || branch_id AS op FROM package_ops ORDER BY rowid"
    |> Sql.executeRows (fun read -> read.string "op")
  let all = String.concat "\n" rows
  $"{List.length rows}:{sha256 all}"

/// Say what the store now holds: <param files> (`LoadPackagesFromDisk.files`) and its ops.
let writeRecord (files : List<string * string>) : unit =
  let fingerprint = fingerprint ()
  let path = recordPath ()
  do
    use stream = System.IO.File.Create(path + ".tmp")
    use w = new System.Text.Json.Utf8JsonWriter(stream)
    w.WriteStartObject()
    w.WriteString("fingerprint", fingerprint)
    w.WriteStartObject("files")
    for (file, text) in files do
      w.WriteString(System.IO.Path.GetRelativePath(root (), file), text)
    w.WriteEndObject()
    w.WriteEndObject()
  System.IO.File.Move(path + ".tmp", path, true)

let private readRecord () : string * Map<string, string> =
  let text =
    try
      System.IO.File.ReadAllText(recordPath ())
    with :? System.IO.FileNotFoundException ->
      refuse "the store has no record of the files it was loaded from"
  use doc = System.Text.Json.JsonDocument.Parse text
  let files =
    doc.RootElement.GetProperty("files").EnumerateObject()
    |> Seq.map (fun p -> (p.Name, p.Value.GetString()))
    |> Map.ofSeq
  (doc.RootElement.GetProperty("fingerprint").GetString(), files)


// ------------------------------------------------------------------
// A file's declarations
// ------------------------------------------------------------------

type private Decl =
  {
    kind : string
    name : string
    /// From the line after the declaration before it, so its comments and doc come along.
    text : string
    /// A nested module's first line and what it declares.
    inner : Option<string * List<Decl>>
  }

/// Lines with their trailing whitespace dropped, and no blank lines at either end.
let private normalise (text : string) : string =
  text.Split('\n')
  |> Array.map (fun l -> l.TrimEnd())
  |> String.concat "\n"
  |> fun t -> t.Trim('\n')

/// The declarations of <param text>, by the parser that a reload uses: (the header's module path,
/// its declarations).
let private parse (path : string) (text : string) : List<string> * List<Decl> =
  let lineStarts =
    text
    |> Seq.indexed
    |> Seq.choose (fun (i, c) -> if c = '\n' then Some(i + 1) else None)
    |> Seq.append [ 0 ]
    |> Array.ofSeq
  // Declarations always end a line, so each one's text runs to the end of the line it ends on.
  let afterLine (row : int) : int =
    if row + 1 < lineStarts.Length then lineStarts[row + 1] else text.Length
  let slice (a : int) (b : int) = text.Substring(a, max 0 (b - a))

  let rec level (start : int) (decls : List<WT.Declaration>) : List<Decl> =
    let chunks =
      decls
      |> List.mapFold
        (fun (from : int) d ->
          let kind, name, (range : WT.Range) =
            match d with
            | WT.DFunction f -> "fn", f.name.name, f.range
            | WT.DValue v -> "value", v.name.name, v.range
            | WT.DType t -> "type", t.name.name, t.range
            | WT.DTest t -> "test", t.name.name, t.range
            | WT.DTrait t -> "trait", t.name.name, t.range
            | WT.DImpl i ->
              "impl", String.concat "." (WT.implMemberPath [] i), i.range
            | WT.DModule m -> "module", snd m.name, m.range
            | WT.DExpr _
            | WT.DAssertion _ ->
              refuse $"{path} has an expression where a declaration goes"
          let until = afterLine range.end_.row
          let inner =
            match d with
            | WT.DModule m ->
              let headerEnd = afterLine (fst m.name).end_.row
              Some(slice from headerEnd, level headerEnd m.declarations)
            | _ -> None
          ({ kind = kind
             name = name
             text = normalise (slice from until)
             inner = inner },
           until))
        start
      |> fst
    // A file may open the same nested module more than once; `diff` tells those apart by order.
    chunks
    |> List.filter (fun c -> c.kind <> "module")
    |> List.countBy (fun c -> c.kind, c.name)
    |> List.tryFind (fun (_, n) -> n > 1)
    |> Option.iter (fun ((_, name), _) -> refuse $"{path} declares {name} twice")
    chunks

  match LibParser.Parser.parseFor LibParser.Validation.Package text with
  | Error _ -> refuse $"{path} does not parse"
  | Ok validated ->
    let sf = LibParser.Validation.ValidatedSourceFile.toWrittenTypes validated
    match level 0 sf.declarations with
    | [ { kind = "module"; name = name; inner = Some(_, decls) } ] ->
      (name.Split('.') |> List.ofArray, decls)
    | [] -> ([], [])
    | _ -> refuse $"{path} isn't one `module A.B` of declarations"


// ------------------------------------------------------------------
// What changed
// ------------------------------------------------------------------

type private Change =
  { added : bool; kind : string; modules : List<string>; name : string }

let rec private changesResolution (d : Decl) : bool =
  d.kind = "trait"
  || d.kind = "impl"
  || d.inner |> Option.exists (snd >> List.exists changesResolution)

/// Every declaration in <param d>, nested modules opened up, as something that goes.
let rec private removals (modules : List<string>) (d : Decl) : List<Change> =
  match d.inner with
  | Some(_, inner) -> inner |> List.collect (removals (modules @ [ d.name ]))
  | None when changesResolution d ->
    refuse
      $"a trait or impl was removed ({d.name}), which can change how existing calls resolve"
  | None -> [ { added = false; kind = d.kind; modules = modules; name = d.name } ]

/// The source to hand `dark import` for one module level, what it adds or changes (every new or
/// changed declaration, inside the nested modules it is in), and what is no longer there.
let rec private diff
  (modules : List<string>)
  (olds : List<Decl>)
  (news : List<Decl>)
  : string * List<Change> * List<Change> =
  // Nested module blocks pair up in order; anything else by its name.
  let keyed (decls : List<Decl>) =
    decls
    |> List.mapFold
      (fun (opened : Map<string, int>) d ->
        if d.kind = "module" then
          let nth = (Map.tryFind d.name opened |> Option.defaultValue 0) + 1
          ((d.kind, d.name, nth), d), Map.add d.name nth opened
        else
          ((d.kind, d.name, 1), d), opened)
      Map.empty
    |> fst
  let olds = keyed olds
  let news = keyed news
  let newKeys = news |> List.map fst |> Set.ofList
  let gone =
    olds
    |> List.filter (fun (k, _) -> not (Set.contains k newKeys))
    |> List.collect (fun (_, d) -> removals modules d)
  let oldOf = Map.ofList olds
  let parts =
    news
    |> List.map (fun (key, d) ->
      match Map.tryFind key oldOf, d.inner with
      | None, _ when changesResolution d ->
        refuse
          $"a trait or impl was added ({d.name}), which can change how existing calls resolve"
      | None, _ ->
        d.text,
        [ { added = true; kind = d.kind; modules = modules; name = d.name } ],
        []
      | Some o, _ when o.text = d.text -> "", [], []
      | Some { inner = Some(_, oldInner) }, Some(header, newInner) ->
        let source, changes, removed = diff (modules @ [ d.name ]) oldInner newInner
        (if List.isEmpty changes then "" else header + source), changes, removed
      | Some _, _ ->
        d.text,
        [ { added = false; kind = d.kind; modules = modules; name = d.name } ],
        [])
  let sources =
    parts
    |> List.filter (fun (_, changes, _) -> not (List.isEmpty changes))
    |> List.map (fun (source, _, _) -> source)
  (String.concat "\n\n" sources,
   parts |> List.collect (fun (_, changes, _) -> changes),
   gone @ (parts |> List.collect (fun (_, _, removed) -> removed)))


// ------------------------------------------------------------------
// The store
// ------------------------------------------------------------------

let private location (change : Change) : PT.PackageLocation =
  match change.modules with
  | owner :: modules ->
    { owner = owner; modules = modules; name = change.name.Trim('`') }
  | [] -> refuse $"{change.name} is outside any module"

/// The hashes live at what <param change> names now.
let private liveAt (change : Change) : List<string> =
  // An impl is at its trait's name in its type's module (`WT.implMemberPath`), and its methods
  // are the fns in the module below that.
  let places =
    match change.kind, change.modules with
    | "impl", owner :: modules ->
      let trait_, typeName =
        match change.name.Split('.') |> List.ofArray |> List.rev with
        | trait_ :: typeName -> trait_, dotted (List.rev typeName)
        | [] -> refuse $"can't read the impl {change.name}"
      let inType =
        if List.tryLast modules = Some typeName then
          modules
        else
          modules @ [ typeName ]
      [ [ owner; String.concat "." inType; trait_ ]
        [ owner; String.concat "." (inType @ [ trait_ ]); "" ] ]
    | _, owner :: modules ->
      [ [ owner; String.concat "." modules; change.name.Trim('`') ] ]
    | _ -> []
  let roots =
    Sql.query
      "SELECT l.item_hash FROM json_each(@places) j JOIN locations l
         ON l.owner = json_extract(j.value, '$[0]')
        AND l.modules = json_extract(j.value, '$[1]')
        AND (json_extract(j.value, '$[2]') = '' OR l.name = json_extract(j.value, '$[2]'))
       WHERE l.branch_id = @main AND l.live = 1"
    |> Sql.parameters
      [ "places", Sql.string (jsonRows places)
        "main", Sql.string (string PT.BranchId.Main) ]
    |> Sql.executeRows (fun read -> read.string "item_hash")
  if List.isEmpty roots then
    refuse
      $"the store has nothing at {dotted (change.modules @ [ change.name ])}, which it was loaded with"
  roots

/// How many live items depend on what <param change> names now, transitively.
let private dependents (change : Change) : int64 =
  let roots = jsonRows (liveAt change |> List.map (fun r -> [ r ]))
  Sql.query
    "WITH RECURSIVE dep(h) AS (
       SELECT json_extract(value, '$[0]') FROM json_each(@roots)
       UNION
       SELECT d.item_hash FROM package_dependencies d JOIN dep ON d.depends_on_hash = dep.h)
     SELECT COUNT(DISTINCT l.item_hash) AS n FROM dep JOIN locations l
       ON l.item_hash = dep.h AND l.live = 1 AND l.branch_id = @main
     WHERE dep.h NOT IN (SELECT json_extract(value, '$[0]') FROM json_each(@roots))"
  |> Sql.parameters
    [ "roots", Sql.string roots; "main", Sql.string (string PT.BranchId.Main) ]
  |> Sql.executeRow (fun read -> read.int64 "n")

/// The live items, other than those at <param locations>, that call one of <param locations> by
/// name. By name, not hash: the same body at another name is a different item.
let private users (locations : List<PT.PackageLocation>) : List<string> =
  let places =
    locations
    |> List.map (fun l -> [ l.owner; String.concat "." l.modules; l.name ])
  Sql.query
    "WITH gone(owner, modules, name) AS (
       SELECT json_extract(value, '$[0]'), json_extract(value, '$[1]'), json_extract(value, '$[2]')
         FROM json_each(@places))
     SELECT DISTINCT l.owner || '.' || l.modules || '.' || l.name AS name
       FROM package_dependencies d
       JOIN gone g ON d.depends_on_owner = g.owner AND d.depends_on_modules = g.modules
                  AND d.depends_on_name = g.name
       JOIN locations l ON l.item_hash = d.item_hash AND l.live = 1 AND l.branch_id = @main
      WHERE NOT EXISTS (SELECT 1 FROM gone o
                         WHERE o.owner = l.owner AND o.modules = l.modules AND o.name = l.name)"
  |> Sql.parameters
    [ "places", Sql.string (jsonRows places)
      "main", Sql.string (string PT.BranchId.Main) ]
  |> Sql.executeRows (fun read -> read.string "name")


/// The state `defineBundled` runs in: the CLI's builtins, with `scmAddOps` the one that lets
/// `Darklang` names change.
let private exeState () : RT.ExecutionState =
  let builtins = Builtins.allWith PM.pt
  let addOps = Builtins.Matter.Libs.PM.PackageOps.bundledAddOps PM.pt
  let notify _ _ _ _ = ()
  let reportException _ _ _ _ = ()
  let fns =
    builtins.fns
    |> Seq.map (fun kv ->
      if kv.Key = addOps.name then
        System.Collections.Generic.KeyValuePair(kv.Key, addOps)
      else
        kv)
    |> RT.Dictionary
  { Exe.createState
      { builtins with fns = fns }
      PM.rt
      Exe.noTracing
      reportException
      notify with
      access =
        LibExecution.Permissions.Access.start
          LibExecution.Permissions.Policy.allowAll }


let private applyEdits () : string =
  let started = System.Diagnostics.Stopwatch.StartNew()
  let recorded, recordedFiles = readRecord ()
  let now = fingerprint ()
  if now <> recorded then refuse "the store's ops changed since it was loaded"
  let files = LoadPackagesFromDisk.files ()
  let current =
    files
    |> List.map (fun (file, text) ->
      (System.IO.Path.GetRelativePath(root (), file), text))
    |> Map.ofList
  let deleted =
    recordedFiles
    |> Map.toList
    |> List.filter (fun (file, _) -> not (Map.containsKey file current))
    |> List.collect (fun (file, text) ->
      let modules, olds = parse file text
      olds |> List.collect (removals modules))

  let diffs =
    current
    |> Map.toList
    |> List.choose (fun (file, text) ->
      let before = Map.tryFind file recordedFiles
      if before = Some text then
        None
      else
        let modules, news = parse file text
        let oldModules, olds =
          match before with
          | Some b -> parse file b
          | None -> (modules, [])
        if oldModules <> modules then refuse $"{file}'s module line changed"
        let source, changes, removed = diff modules olds news
        Some(file, modules, source, changes, removed))
  let work =
    diffs
    |> List.filter (fun (_, _, _, changes, _) -> not (List.isEmpty changes))
    |> List.map (fun (file, modules, source, changes, _) ->
      (file, modules, source, changes))

  // A declaration that moved to another file is defined there, not removed.
  let defined =
    work
    |> List.collect (fun (_, _, _, changes) -> changes)
    |> List.map location
    |> Set.ofList
  let removed =
    deleted @ (diffs |> List.collect (fun (_, _, _, _, removed) -> removed))
    |> List.filter (fun c -> not (Set.contains (location c) defined))
    |> List.distinctBy location

  let counts =
    work
    |> List.collect (fun (_, _, _, changes) -> changes)
    |> List.filter (fun c -> not c.added && c.kind <> "test")
    |> List.map (fun c ->
      let n = dependents c
      if c.kind = "type" && n > typeDependentsLimit then
        refuse
          $"type {dotted (c.modules @ [ c.name ])} changed and {n} items depend on it"
      n)
  let total = List.sum counts
  if total > dependentsLimit then
    refuse $"the changes have {total} dependents, over {dependentsLimit}"

  if not (List.isEmpty work && List.isEmpty removed) then
    let state = exeState ()
    let define =
      match PM.resolveFnRef "Darklang.Cli.Packages.ModuleCommand.defineBundled" with
      | Some fn -> fn
      | None ->
        refuse "the store has no Darklang.Cli.Packages.ModuleCommand.defineBundled"
    work
    |> List.iter (fun (file, modules, source, _) ->
      let args =
        NEList.ofList
          (RT.DString("/" + String.concat "." modules))
          [ RT.DString source ]
      match
        LibExecution.HostEvents.runHere (fun () ->
          Exe.executeFunction state define [] args)
      with
      | Ok(RT.DInt64 0L) -> ()
      | Ok _ -> refuse $"`dark import` did not apply {file}"
      | Error(rte, _) ->
        let message =
          LibExecution.HostEvents.runHere (fun () ->
            Exe.runtimeErrorMessage state rte)
        refuse $"`dark import` did not apply {file}: {message}")

    // After the edits, so a caller rewritten in the same change no longer counts as a user.
    if not (List.isEmpty removed) then
      let bound = removed |> List.map (fun c -> location c, liveAt c)
      let stillUsed = users (List.map fst bound)
      if not (List.isEmpty stillUsed) then
        refuse
          $"{List.length stillUsed} live item(s) still use what was removed, such as {stillUsed[0]}"
      let unbinds =
        bound
        |> List.collect (fun (loc, hashes) ->
          hashes |> List.map (fun h -> PT.PackageOp.Unbind(loc, Some(PT.Hash h))))
      LibDB.Queries.ensureAuthoringClockIsCurrent ()
      match LibDB.Inserts.insertBundledOps PT.BranchId.Main unbinds with
      // An op is its content, so removing the same version a second time is the op already in
      // the log, which folds nothing.
      | Ok n when n < int64 (List.length unbinds) ->
        refuse "a removed declaration was removed before with the same body"
      | Ok _ -> ()
      | Error reason -> refuse $"removing declarations failed: {reason}"

    let _ = LibDB.Inserts.commitAllAsBaseline "package reload (baseline)"
    LibDB.PackageRefsGenerator.generate true
    match LibDB.Seed.evaluateAllValues (Builtins.all ()) PM.rt with
    | Ok() -> ()
    | Error _ -> refuse "some values failed to evaluate"

  writeRecord files
  let decls = work |> List.sumBy (fun (_, _, _, changes) -> List.length changes)
  let gone = if List.isEmpty removed then "" else $", removed {List.length removed}"
  $"applied {decls} declaration(s) from {List.length work} file(s){gone}, {total} dependent(s), in {started.Elapsed.TotalSeconds:F1}s; no full reload"


/// Apply the edits, and say in a line starting `packages:` which path the build took.
let apply () : Result<unit, string> =
  try
    let said = applyEdits ()
    print $"packages: {said}"
    Ok()
  with Refused reason ->
    print $"packages: full reload: {reason}"
    Error reason
