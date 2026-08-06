/// Differential gate for the two WrittenTypes→ProgramTypes lowerings.
///
/// The F# lowering (`LibParser.WrittenTypesToProgramTypes`, used by execution:
/// package loading, testfiles, CLI) and the Dark lowering
/// (`languageTools/writtenTypesToProgramTypes.dark`, used by tooling: LSP,
/// round-trip, formatting) must produce the same ProgramTypes for the same
/// source — identical except for node ids (which canonical serialization
/// ignores). Any other difference is drift, the class of bug this gate catches.
///
/// Two tests: a broad sweep over REAL corpus expressions (fn/value bodies in
/// packages/darklang that reparse cleanly as one expression, spread-sampled to
/// bound runtime, lowered with an empty context), and a few pinned cases lowered
/// under a real (module, fn, params) context — catching divergences that only
/// surface once names resolve (self-calls, a match pattern shadowing a param).
module Tests.WrittenTypesLoweringParity

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PackageRefs = LibExecution.PackageRefs
module P = LibParser.Parser
module WT = LibParser.WrittenTypes
module WT2PT = LibParser.WrittenTypesToProgramTypes
module NR = LibParser.NameResolver
module Canonical = LibSerialization.Hashing.Canonical

// --- id-independent canonical comparison ---

/// id-independent canonical bytes (the content-hash serialization)
let private canonicalBytes (e : PT.Expr) : byte[] =
  use ms = new System.IO.MemoryStream()
  use w = new System.IO.BinaryWriter(ms)
  Canonical.writeExpr Canonical.Normal w e
  w.Flush()
  ms.ToArray()

// --- lockstep differ: report the first divergent node, human-readably ---
let private ctorName (x : obj) : string =
  let s = sprintf "%A" x
  s.Split([| ' '; '('; '\n' |])[0]

let rec private kidsE (e : PT.Expr) : List<PT.Expr> =
  match e with
  | PT.EString(_, contents) ->
    contents
    |> List.choose (function
      | PT.StringInterpolation e -> Some e
      | _ -> None)
  | PT.EIf(_, c, t, e2) -> [ c; t ] @ Option.toList e2
  | PT.EPipe(_, lhs, parts) ->
    lhs
    :: (parts
        |> List.collect (function
          | PT.EPipeLambda(_, _, b) -> [ b ]
          | PT.EPipeInfix(_, _, e) -> [ e ]
          | PT.EPipeFnCall(_, _, _, args) -> args
          | PT.EPipeEnum(_, _, _, fields) -> fields
          | PT.EPipeVariable(_, _, args) -> args))
  | PT.EMatch(_, arg, cases) ->
    arg
    :: (cases |> List.collect (fun c -> Option.toList c.whenCondition @ [ c.rhs ]))
  | PT.ELet(_, _, v, b) -> [ v; b ]
  | PT.EStatement(_, a, b) -> [ a; b ]
  | PT.EList(_, es) -> es
  | PT.EDict(_, kvs) -> List.map snd kvs
  | PT.ETuple(_, a, b, rest) -> a :: b :: rest
  | PT.EApply(_, f, _, args) -> f :: NEList.toList args
  | PT.ELambda(_, _, body) -> [ body ]
  | PT.EInfix(_, _, l, r) -> [ l; r ]
  | PT.ERecord(_, _, _, fields) -> List.map snd fields
  | PT.ERecordFieldAccess(_, r, _) -> [ r ]
  | PT.ERecordUpdate(_, r, ups) -> r :: (NEList.toList ups |> List.map snd)
  | PT.EEnum(_, _, _, _, fields) -> fields
  | _ -> []

let private fqFnStr (nr : PT.NameResolution<PT.FQFnName.FQFnName>) : string =
  let orig = String.concat "." nr.originalName
  match nr.resolved with
  | Ok r ->
    let n =
      match r.name with
      | PT.FQFnName.Builtin b -> $"Builtin({b.name},v{b.version})"
      | PT.FQFnName.Package(PT.Hash h) -> $"Pkg({h.Substring(0, 12)}…)"
    $"{orig}=Ok {n} loc=%A{r.location}"
  | Error e -> $"{orig}=Error %A{e}"

let private fqValStr (nr : PT.NameResolution<PT.FQValueName.FQValueName>) : string =
  let orig = String.concat "." nr.originalName
  match nr.resolved with
  | Ok r ->
    let n =
      match r.name with
      | PT.FQValueName.Builtin b -> $"Builtin({b.name},v{b.version})"
      | PT.FQValueName.Package(PT.Hash h) -> $"Pkg({h.Substring(0, 12)}…)"
    $"{orig}=Ok {n}"
  | Error e -> $"{orig}=Error %A{e}"

/// details of the name-ish payload for nodes whose difference is not in child exprs
let private nodeDetail (e : PT.Expr) : string =
  match e with
  | PT.EFnName(_, nr) -> $"EFnName {fqFnStr nr}"
  | PT.EValue(_, nr) -> $"EValue {fqValStr nr}"
  | PT.EVariable(_, v) -> $"EVariable {v}"
  | PT.EArg(_, i) -> $"EArg {i}"
  | PT.EPipe(_, _, parts) ->
    parts
    |> List.map (function
      | PT.EPipeFnCall(_, nr, tas, _) ->
        $"EPipeFnCall {fqFnStr nr} tas={List.length tas}"
      | PT.EPipeVariable(_, v, _) -> $"EPipeVariable {v}"
      | PT.EPipeLambda _ -> "EPipeLambda"
      | PT.EPipeInfix _ -> "EPipeInfix"
      | PT.EPipeEnum(_, nr, cn, _) -> sprintf "EPipeEnum %A.%s" nr cn)
    |> String.concat "; "
    |> sprintf "EPipe parts: %s"
  | PT.ERecord(_, nr, tas, _) -> sprintf "ERecord %A tas=%d" nr (List.length tas)
  | PT.EEnum(_, nr, tas, cn, _) ->
    sprintf "EEnum %A tas=%d case=%s" nr (List.length tas) cn
  | PT.EApply(_, _, tas, _) -> sprintf "EApply tas=%A" tas
  | PT.ELambda(_, pats, _) -> sprintf "ELambda pats=%A" (NEList.toList pats)
  | PT.ELet(_, pat, _, _) -> sprintf "ELet pat=%A" pat
  | PT.EMatch(_, _, cases) ->
    sprintf "EMatch pats=%A" (cases |> List.map (fun c -> c.pat))
  | other -> sprintf "%A" other

let rec private diffExpr (a : PT.Expr) (b : PT.Expr) : Option<string> =
  if canonicalBytes a = canonicalBytes b then
    None
  else
    let ka, kb = kidsE a, kidsE b
    if ctorName a = ctorName b && List.length ka = List.length kb then
      match List.zip ka kb |> List.tryPick (fun (x, y) -> diffExpr x y) with
      | Some d -> Some d
      | None -> Some $"[{ctorName a}] fs: {nodeDetail a}\n     dark: {nodeDetail b}"
    else
      Some $"fs {ctorName a} ({nodeDetail a}) vs dark {ctorName b} ({nodeDetail b})"


// --- corpus snippet harvesting ---

let private sliceRange (src : string) (r : LibParser.Tokenizer.TokenRange) : string =
  let lines = src.Split '\n'
  let safeLine i = if i >= 0 && i < lines.Length then lines[i] else ""
  let clamp (s : string) i = max 0 (min i s.Length)
  if r.start.row = r.end_.row then
    let line = safeLine r.start.row
    line.Substring(
      clamp line r.start.column,
      clamp line r.end_.column - clamp line r.start.column
    )
  else
    let first =
      (safeLine r.start.row).Substring(clamp (safeLine r.start.row) r.start.column)
    let middle = [ for i in r.start.row + 1 .. r.end_.row - 1 -> safeLine i ]
    let lastLine = safeLine r.end_.row
    let last = lastLine.Substring(0, clamp lastLine r.end_.column)
    String.concat "\n" (first :: middle @ [ last ])

let rec private harvest (src : string) (d : WT.Declaration) : List<string> =
  match d with
  | WT.DFunction f -> [ sliceRange src (WT.exprRange f.body) ]
  | WT.DValue v -> [ sliceRange src (WT.exprRange v.body) ]
  | WT.DModule m -> m.declarations |> List.collect (harvest src)
  | _ -> []

/// a snippet participates only if it reparses cleanly, standalone, as exactly
/// one expression (multi-statement bodies split at top level and are skipped)
let private parseSingleExpr (snippet : string) : Option<WT.Expr> =
  let r = P.parse snippet
  match r.parsed, r.diagnostics with
  | Some(WT.SourceFile { declarations = []; exprsToEval = [ e ] }), [] -> Some e
  | _ -> None

let tests =
  // SEQUENCED, and not because these tests touch the store: they don't, they only
  // read it. It's that resolving a name is a read of `locations`, and other tests in
  // the parallel phase rewrite the op log and re-fold that projection out from under
  // them. During such a window a lookup for something as ordinary as
  // `Stdlib.printLine` misses, whichever side happened to ask, and the differential
  // reports a lowering divergence that isn't one. That failure moves around with
  // test timing and with which snippets the sample happens to land on, so it reads
  // as a mysterious intermittent parser bug.
  testSequenced
  <| testList
    "WrittenTypesLoweringParity"
    [ testTask "F# and Dark lowerings agree on corpus expressions (modulo node ids)" {
        let root =
          [ // Escape hatch for bisecting this test: the sample is `corpus/400` spread across the whole
            // tree, so ANY edit to the corpus moves which snippets get checked, and
            // a pre-existing divergence can appear or vanish for reasons unrelated
            // to the change under test. Point this at a pristine checkout to compare
            // the same snippets across two versions of the lowering.
            System.Environment.GetEnvironmentVariable "DARK_PARITY_CORPUS"
            "../packages/darklang"
            "packages/darklang"
            "/home/dark/app/packages/darklang" ]
          |> List.filter (fun p -> not (isNull p))
          |> List.tryFind System.IO.Directory.Exists
        match root with
        | None -> () // not a full checkout — nothing to gate
        | Some root ->
          let snippets =
            System.IO.Directory.GetFiles(
              root,
              "*.dark",
              System.IO.SearchOption.AllDirectories
            )
            |> Array.sort
            |> Array.toList
            |> List.collect (fun f ->
              let src = System.IO.File.ReadAllText f
              match (P.parse src).parsed with
              | Some(WT.SourceFile sf) ->
                sf.declarations
                |> List.collect (harvest src)
                |> List.map (fun s -> (f, s))
              | None -> [])
            |> List.choose (fun (f, snip) ->
              parseSingleExpr snip |> Option.map (fun e -> (f, snip, e)))
          // spread-sample across the whole corpus to bound runtime
          let target = 400
          let step = max 1 (List.length snippets / target)
          let sample =
            snippets
            |> List.mapi (fun i s -> (i, s))
            |> List.filter (fun (i, _) -> i % step = 0)
            |> List.map snd

          let! (exeState : RT.ExecutionState) =
            executionStateFor pmPT false Map.empty
          let parseFnName =
            RT.FQFnName.fqPackage (
              PackageRefs.Fn.LanguageTools.Parser.parsePTExpr ()
            )
          let fsBuiltins = localBuiltIns pmPT
          let ctx : WT2PT.Context =
            { currentFnName = None; argMap = Map.empty; localBindings = Set.empty }

          let mismatches = ResizeArray<string * string>()
          let mutable darkSideErrors = 0
          for (f, snip, wtExpr) in sample do
            let! (fsPT : PT.Expr) =
              WT2PT.Expr.toPT fsBuiltins pmPT NR.OnMissing.Allow [] ctx wtExpr
              |> Ply.toTask
            let! darkResult =
              LibExecution.Execution.executeFunction
                exeState
                parseFnName
                []
                (NEList.singleton (RT.DString snip))
            let! darkDval = unwrapExecutionResult exeState darkResult |> Ply.toTask
            match darkDval with
            | RT.DEnum(tn, _, _, "Ok", [ exprDval ]) when tn = Dval.resultType () ->
              let darkPT = PT2DT.Expr.fromDT exprDval
              if canonicalBytes fsPT <> canonicalBytes darkPT then
                if mismatches.Count < 8 then
                  let d = diffExpr fsPT darkPT |> Option.defaultValue "?"
                  print $"DRILL {f}:\n  {d}"
                mismatches.Add(f, snip)
            | other ->
              // the Dark path failed where the F# path parsed — also a finding,
              // but tracked separately from lowering mismatches
              if darkSideErrors < 3 then
                let shown =
                  if snip.Length > 120 then snip.Substring(0, 120) else snip
                print $"DARK-ERR {f}: %A{other}\n  snippet: {shown}"
              darkSideErrors <- darkSideErrors + 1

          if mismatches.Count > 0 then
            let detail =
              mismatches
              |> Seq.truncate 5
              |> Seq.map (fun (f, s) ->
                let shown = if s.Length > 200 then s.Substring(0, 200) + "…" else s
                $"{f}:\n{shown}")
              |> String.concat "\n---\n"
            failtest
              $"{mismatches.Count} lowering divergences between F# and Dark WT2PT:\n{detail}"
      }

      // The differential above lowers with an EMPTY module/fn context, so it is blind
      // to divergences that only appear once names resolve in a real fn — the pipe
      // fn-first resolution, qualified self-calls, and a match pattern shadowing a
      // parameter. This pins those by lowering hand-picked snippets under a real
      // (module, fn-name, params) context through BOTH lowerings and asserting they
      // agree — the coverage that would have caught the earlier Dark-lowering breaks.
      testTask
        "F# and Dark lowerings agree under a real fn context (self-call, match-shadow)" {
        let! (exeState : RT.ExecutionState) = executionStateFor pmPT false Map.empty
        let fsBuiltins = localBuiltIns pmPT
        let inCtxFn =
          RT.FQFnName.fqPackage (
            PackageRefs.Fn.LanguageTools.Parser.parsePTExprInContext ()
          )
        // (label, currentModule, currentFnName, params, snippet)
        let cases
          : List<string * List<string> * List<string> * List<string> * string> =
          [ // a match pattern that shadows a parameter must bind the PATTERN, not the
            // parameter (EArg) — the Dark bug produced EArg here
            ("match-shadows-param",
             [ "M" ],
             [ "Tests"; "M"; "f" ],
             [ "x" ],
             "match 99L with | x -> x")
            // a QUALIFIED self-call is a normal reference, not ESelf (matches F#)
            ("qualified-self-call",
             [ "M" ],
             [ "Tests"; "M"; "f" ],
             [ "n" ],
             "Tests.M.f n")
            // a pipe fn-call with explicit type args (`|> f<T>`) — the type args must
            // lower onto the EPipeFnCall node identically on both sides (guards the move
            // of pipe type args off the fn identifier in the Dark WrittenTypes)
            ("pipe-fn-call-type-args",
             [ "M" ],
             [],
             [ "x" ],
             "x |> Stdlib.Json.serialize<Bool>") ]
        let mismatches = ResizeArray<string>()
        for (label, cmod, cfn, prms, snip) in cases do
          let ctx : WT2PT.Context =
            { currentFnName = (if List.isEmpty cfn then None else Some cfn)
              argMap = prms |> List.mapi (fun i p -> (p, i)) |> Map.ofList
              localBindings = Set.empty }
          match (P.parse snip).parsed with
          | Some(WT.SourceFile { exprsToEval = [ e ] }) ->
            let! (fsPT : PT.Expr) =
              WT2PT.Expr.toPT fsBuiltins pmPT NR.OnMissing.Allow cmod ctx e
              |> Ply.toTask
            let strList xs = xs |> List.map RT.DString |> Dval.list RT.KTString
            let args =
              NEList.ofList
                (RT.DString "") // branch: main, explicitly, so a concurrently-switched process can't move it
                [ RT.DString "Tests"
                  strList cmod
                  strList cfn
                  strList prms
                  RT.DString snip ]
            let! darkResult =
              LibExecution.Execution.executeFunction exeState inCtxFn [] args
            let! darkDval = unwrapExecutionResult exeState darkResult |> Ply.toTask
            match darkDval with
            | RT.DEnum(tn, _, _, "Ok", [ exprDval ]) when tn = Dval.resultType () ->
              let darkPT = PT2DT.Expr.fromDT exprDval
              if canonicalBytes fsPT <> canonicalBytes darkPT then
                let d = diffExpr fsPT darkPT |> Option.defaultValue "?"
                mismatches.Add($"{label} `{snip}`: {d}")
            | other -> mismatches.Add($"{label} `{snip}`: dark-side error {other}")
          | _ -> mismatches.Add($"{label} `{snip}`: F# parse failed")
        if mismatches.Count > 0 then
          let detail = String.concat "\n" mismatches
          failtest $"context-dependent lowering divergences:\n{detail}"
      } ]
