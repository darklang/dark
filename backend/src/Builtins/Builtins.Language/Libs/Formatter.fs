/// `Builtin.formatterParse`: a `.dark` file as `LanguageTools.Formatter.File`, for `dark fmt`.
///
/// Each declaration is lowered the way the package loader lowers it, so what the printer prints
/// is what loading the file would store. What lowering drops is kept beside it: the `//`
/// comments, the line every node starts on, and the signatures of local fns, whose parameter
/// types the parser throws away.
module Builtins.Language.Libs.Formatter

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module Refs = LibExecution.PackageRefs.Type.LanguageTools.Formatter
module NR = LibExecution.RuntimeTypes.NameResolution
module Tok = LibParser.Tokenizer
module Lex = LibParser.Lexer
module P = LibParser.Parser
module WT = LibParser.WrittenTypes
module WT2PT = LibParser.WrittenTypesToProgramTypes
module NRslv = LibParser.NameResolver
module Validation = LibParser.Validation
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PMPT = LibDB.ProgramTypes

type private Ranges = System.Collections.Generic.Dictionary<id, WT.Range>

let private tn (r : unit -> string) : FQTypeName.FQTypeName =
  FQTypeName.fqPackage (r ())

let private record
  (typeName : FQTypeName.FQTypeName)
  (fields : List<string * Dval>)
  : Dval =
  DRecord(typeName, typeName, [], RecordFields.ofList fields)

let private enum
  (typeName : FQTypeName.FQTypeName)
  (case : string)
  (fields : List<Dval>)
  =
  DEnum(typeName, typeName, [], case, fields)


/// A packed source position: row * 1,000,000 + column. One Int64 where a pair would be two
/// values and a tuple.
let private packed (p : Tok.Pos) : int64 = int64 p.row * 1000000L + int64 p.column

/// The token starting at <param p>, the last of them if several do, found by halving: the lexer
/// gives tokens in source order.
let private tokenStartingAt
  (tokens : Lex.SpannedToken[])
  (p : Tok.Pos)
  : Option<Lex.SpannedToken> =
  let key = packed p
  let rec last (i : int) =
    if i + 1 < tokens.Length && packed tokens[i + 1].range.start = key then
      last (i + 1)
    else
      Some tokens[i]
  let rec go (lo : int) (hi : int) =
    if lo > hi then
      None
    else
      let mid = (lo + hi) / 2
      let at = packed tokens[mid].range.start
      if at = key then last mid
      elif at < key then go (mid + 1) hi
      else go lo (mid - 1)
  go 0 (tokens.Length - 1)

/// The ids of the nodes that start at <param p>, in the order lowering noted them.
let private nodesStartingAt (ranges : Ranges) (p : Tok.Pos) : List<id> =
  let key = packed p
  ranges
  |> Seq.filter (fun kv -> packed kv.Value.start = key)
  |> Seq.map (fun kv -> kv.Key)
  |> List.ofSeq

/// Every `//` comment and what is around it: the token before it (its text, and the line it ends
/// on), the line the last thing before it ended on, and where the code after it starts -- the
/// next token and the rest of that token's line, up to three more. `Formatter.placeComments`
/// decides from these which code each one goes with.
let private rawComments (tokens : Lex.SpannedToken[]) : List<Dval> =
  let isComment (c : Lex.Trivia) =
    c.kind = Lex.LineComment || c.kind = Lex.BlockComment
  [ for k in 0 .. tokens.Length - 1 do
      let token = tokens[k]
      if List.exists isComment token.leadingTrivia then
        let previousToken, previousEnd =
          if k > 0 then
            tokens[k - 1].text, int64 tokens[k - 1].range.end_.row
          else
            "", -1L
        let nextStarts =
          [ for j in k .. min (k + 3) (tokens.Length - 1) do
              if tokens[j].range.start.row = token.range.start.row then
                yield DInt64(packed tokens[j].range.start) ]
          |> Dval.list KTInt64
        let (_, found) =
          token.leadingTrivia
          |> List.fold
            (fun (lastRow : int64, acc : List<Dval>) (c : Lex.Trivia) ->
              if isComment c then
                let comment =
                  record
                    (tn Refs.rawComment)
                    [ "text", DString(c.text.TrimEnd())
                      "line", DInt64(int64 c.range.start.row)
                      "column", DInt64(int64 c.range.start.column)
                      "previousLine", DInt64 lastRow
                      "previousToken", DString previousToken
                      "previousEnd", DInt64 previousEnd
                      "nextLine", DInt64(int64 token.range.start.row)
                      "nextStarts", nextStarts ]
                (int64 c.range.end_.row, comment :: acc)
              else
                (int64 c.range.end_.row, acc))
            (previousEnd, [])
        yield! List.rev found ]


/// The places a comment can belong to code: where the tokens `rawComments` lists after a comment
/// start, and the lines comments are on. Placing comments asks about nodes there and nowhere else.
let private commentSpots
  (tokens : Lex.SpannedToken[])
  : System.Collections.Generic.HashSet<int64> *
    System.Collections.Generic.HashSet<int64>
  =
  let starts = System.Collections.Generic.HashSet<int64>()
  let rows = System.Collections.Generic.HashSet<int64>()
  let isComment (c : Lex.Trivia) =
    c.kind = Lex.LineComment || c.kind = Lex.BlockComment
  for k in 0 .. tokens.Length - 1 do
    let token = tokens[k]
    if List.exists isComment token.leadingTrivia then
      for j in k .. min (k + 3) (tokens.Length - 1) do
        if tokens[j].range.start.row = token.range.start.row then
          starts.Add(packed tokens[j].range.start) |> ignore<bool>
      for c in token.leadingTrivia do
        if isComment c then rows.Add(int64 c.range.start.row) |> ignore<bool>
  (starts, rows)


let private declRange (d : WT.Declaration) : WT.Range =
  match d with
  | WT.DFunction f -> f.range
  | WT.DValue v -> v.range
  | WT.DModule m -> m.range
  | WT.DType t -> t.range
  | WT.DTrait t -> t.range
  | WT.DImpl i -> i.range
  | WT.DTest t -> t.range
  | WT.DExpr e -> WT.exprRange e
  | WT.DAssertion a -> a.range


type private Context =
  {
    builtins : Builtins
    pm : PT.PackageManager
    lines : string[]
    /// Where each line starts in the source, by line, in characters as Dark counts them.
    lineStarts : int[]
    /// The tokens in source order, for the `///` lines above a declaration (`tokenStartingAt`).
    tokens : Lex.SpannedToken[]
    ranges : Ranges
    /// Each fn, value and test declaration's span and module path, to resolve a local fn's
    /// types where it is.
    fnSpans : ResizeArray<WT.Range * List<string>>
  }


/// The first line of <param d>, counting the `///` lines above it.
let private firstRow (ctx : Context) (r : WT.Range) : int =
  match tokenStartingAt ctx.tokens r.start with
  | Some token ->
    token.leadingTrivia
    |> List.filter (fun t -> t.kind = Lex.DocComment)
    |> List.map (fun t -> t.range.start.row)
    |> List.fold min r.start.row
  | None -> r.start.row


let rec private declarations
  (ctx : Context)
  (path : List<string>)
  (ds : List<WT.Declaration>)
  : Ply<List<Dval>> =
  ds
  |> Ply.List.mapSequentially (fun d ->
    uply {
      let range = declRange d
      let first = firstRow ctx range
      let onMissing = NRslv.OnMissing.Allow
      let lowered (f : string -> List<string> -> Ply<Dval>) : Ply<Dval> =
        uply {
          match path with
          | [] -> return enum (tn Refs.item) "Verbatim" []
          | owner :: modules ->
            try
              return! f owner modules
            with _ ->
              return enum (tn Refs.item) "Verbatim" []
        }
      let! item =
        match d with
        | WT.DModule m ->
          uply {
            let header =
              List.isEmpty path
              && m.keywordModule.start.column = 0
              && (match m.declarations with
                  | inner :: _ -> (declRange inner).start.column = 0
                  | [] -> true)
            let! inner =
              declarations ctx (path @ WT.moduleNameParts m) m.declarations
            return
              enum
                (tn Refs.item)
                "Module"
                [ DString(snd m.name)
                  DBool header
                  Dval.list (KTCustomType(tn Refs.declaration, [])) inner ]
          }
        | WT.DFunction fn ->
          ctx.fnSpans.Add((fn.range, path))
          lowered (fun owner modules ->
            uply {
              let wt = WT.packageFn owner modules fn
              let! pt =
                WT2PT.PackageFn.toPTWithRanges
                  ctx.builtins
                  ctx.pm
                  onMissing
                  (owner :: modules)
                  (Some ctx.ranges)
                  wt
              return
                enum
                  (tn Refs.item)
                  "Fn"
                  [ DString fn.name.name; PT2DT.PackageFn.toDT pt ]
            })
        | WT.DValue v ->
          ctx.fnSpans.Add((v.range, path))
          lowered (fun owner modules ->
            uply {
              let wt = WT.packageValue owner modules v
              let! pt =
                WT2PT.PackageValue.toPTNoting
                  ctx.builtins
                  ctx.pm
                  onMissing
                  (owner :: modules)
                  (Some ctx.ranges)
                  wt
              return
                enum
                  (tn Refs.item)
                  "Value"
                  [ DString v.name.name; PT2DT.PackageValue.toDT pt ]
            })
        | WT.DTest t ->
          ctx.fnSpans.Add((t.range, path))
          lowered (fun owner modules ->
            uply {
              let wt = WT.packageTest owner modules t
              let! pt =
                WT2PT.PackageTest.toPTNoting
                  ctx.builtins
                  ctx.pm
                  onMissing
                  (owner :: modules)
                  (Some ctx.ranges)
                  wt
              return
                enum
                  (tn Refs.item)
                  "Test"
                  [ DString t.name.name; PT2DT.PackageTest.toDT pt ]
            })
        | WT.DType t ->
          lowered (fun owner modules ->
            uply {
              let wt = WT.packageType owner modules t
              let! pt =
                WT2PT.PackageType.toPT ctx.pm onMissing (owner :: modules) wt
              return
                enum
                  (tn Refs.item)
                  "Type"
                  [ DString t.name.name; PT2DT.PackageType.toDT pt ]
            })
        | WT.DTrait t ->
          lowered (fun owner modules ->
            uply {
              let wt = WT.packageTrait owner modules t
              let! pt = WT2PT.Trait.toPT ctx.pm onMissing (owner :: modules) wt
              return
                enum
                  (tn Refs.item)
                  "Trait"
                  [ DString t.name.name; PT2DT.Trait.toDT pt ]
            })
        | WT.DImpl i ->
          lowered (fun _ _ ->
            uply {
              match WT.implMemberPath path i with
              | owner :: rest ->
                let memberPath = owner :: rest
                let wt = WT.packageImpl owner rest i
                let! pt =
                  WT2PT.TraitImpl.toPT ctx.builtins ctx.pm onMissing memberPath wt
                // Each method as written: a fn declared in the block, or one it names.
                let! members =
                  i.members
                  |> Ply.List.mapSequentially (fun m ->
                    uply {
                      match m with
                      | WT.IMethod fn ->
                        ctx.fnSpans.Add((fn.range, memberPath))
                        let! ptFn =
                          WT2PT.PackageFn.toPTWithRanges
                            ctx.builtins
                            ctx.pm
                            onMissing
                            memberPath
                            (Some ctx.ranges)
                            (WT.packageFn owner rest fn)
                        return
                          enum
                            (tn Refs.implMember)
                            "Declared"
                            [ DString fn.name.name; PT2DT.PackageFn.toDT ptFn ]
                      | WT.IAlias v ->
                        return
                          enum
                            (tn Refs.implMember)
                            "Named"
                            [ DString v.name.name ]
                    })
                return
                  enum
                    (tn Refs.item)
                    "Impl"
                    [ PT2DT.TraitImpl.toDT pt
                      Dval.list KTString (List.map DString memberPath)
                      Dval.list (KTCustomType(tn Refs.implMember, [])) members ]
              | [] -> return enum (tn Refs.item) "Verbatim" []
            })
        | WT.DExpr _
        | WT.DAssertion _ -> Ply(enum (tn Refs.item) "Verbatim" [])
      let last = min range.end_.row (ctx.lines.Length - 1)
      return
        record
          (tn Refs.declaration)
          [ "item", item
            "row", DInt64(int64 first)
            "endRow", DInt64(int64 range.end_.row)
            "column", DInt64(int64 range.start.column)
            "startOffset", DInt64(int64 ctx.lineStarts[first])
            "endOffset",
            DInt64(
              int64 (ctx.lineStarts[last] + String.lengthInEgcs ctx.lines[last])
            ) ]
    })


/// The signature of each local fn, `let f(x: T): R =`, by the id of the lambda it lowered to.
///
/// The parser keeps only the lambda, so the types are read back from the tokens: the
/// parameter list and return type are parsed again as a fn header, and lowered where the fn is.
let private localFns
  (ctx : Context)
  (source : string)
  (tokens : Lex.SpannedToken[])
  : Ply<List<string * Dval>> =
  let offsetOf (p : Tok.Pos) : int =
    let mutable offset = 0
    for row in 0 .. p.row - 1 do
      offset <- offset + ctx.lines[row].Length + 1
    offset + p.column
  let declStarts =
    ctx.fnSpans
    |> Seq.map (fun (r, _) -> (r.start.row, r.start.column))
    |> Set.ofSeq
  let candidates =
    [ for k in 0 .. tokens.Length - 3 do
        match tokens[k].token, tokens[k + 1].token, tokens[k + 2].token with
        | Tok.TLet, Tok.TIdent _, Tok.TLParen when
          not (
            Set.contains
              (tokens[k].range.start.row, tokens[k].range.start.column)
              declStarts
          )
          ->
          // the `=` that ends the header: the first one outside any parentheses
          let mutable depth = 0
          let mutable j = k + 2
          let mutable equals = -1
          while equals < 0 && j < tokens.Length do
            match tokens[j].token with
            | Tok.TLParen -> depth <- depth + 1
            | Tok.TRParen -> depth <- depth - 1
            | Tok.TEquals when depth = 0 -> equals <- j
            | _ -> ()
            j <- j + 1
          if equals > 0 then yield (k + 2, equals)
        | _ -> () ]
  candidates
  |> Ply.List.filterMapSequentially (fun (openParen, equals) ->
    uply {
      let openPos = tokens[openParen].range.start
      let header =
        source.Substring(
          offsetOf openPos,
          offsetOf tokens[equals].range.start - offsetOf openPos
        )
      let hasReturn =
        tokens[openParen..equals] |> Array.exists (fun t -> t.token = Tok.TColon)
        && (let mutable depth = 0
            let mutable found = false
            for t in tokens[openParen..equals] do
              match t.token with
              | Tok.TLParen -> depth <- depth + 1
              | Tok.TRParen -> depth <- depth - 1
              | Tok.TColon when depth = 0 -> found <- true
              | _ -> ()
            found)
      let path =
        ctx.fnSpans
        |> Seq.tryFind (fun (r, _) ->
          (r.start.row, r.start.column) <= (openPos.row, openPos.column)
          && (openPos.row, openPos.column) <= (r.end_.row, r.end_.column))
        |> Option.map snd
        |> Option.defaultValue []
      let mini =
        "module Formatter\nlet f"
        + header.Replace("\n", " ")
        + (if hasReturn then "" else ": Unit")
        + " = ()"
      match
        nodesStartingAt ctx.ranges openPos, P.parseFor Validation.Package mini
      with
      | [ lambdaId ], Ok validated when not (List.isEmpty path) ->
        let sf = Validation.ValidatedSourceFile.toWrittenTypes validated
        match sf.declarations with
        | [ WT.DModule { declarations = [ WT.DFunction fn ] } ] ->
          let wt = WT.packageFn "Formatter" [] fn
          let onMissing = NRslv.OnMissing.Allow
          let! parameters =
            wt.parameters
            |> NEList.toList
            |> Ply.List.mapSequentially (fun p ->
              uply {
                let! typ = WT2PT.TypeReference.toPT ctx.pm onMissing path p.typ
                return DTuple(DString p.name, PT2DT.TypeReference.toDT typ, [])
              })
          let! returnType =
            WT2PT.TypeReference.toPT ctx.pm onMissing path wt.returnType
          let typeRefKT = PT2DT.TypeReference.knownType ()
          let localFn =
            record
              (tn Refs.localFn)
              [ "parameters",
                Dval.list (KTTuple(VT.string, VT.known typeRefKT, [])) parameters
                "returnType",
                (if hasReturn then
                   Dval.optionSome typeRefKT (PT2DT.TypeReference.toDT returnType)
                 else
                   Dval.optionNone typeRefKT) ]
          return Some(string lambdaId, localFn)
        | _ -> return None
      | _ -> return None
    })


/// What lowering a `[a, ...xs, b]` list leaves behind, by node id: the call to `Stdlib.List.flatten`
/// or `concat` it became, which spans the brackets (`call`), and each element written after a
/// `...`: `alone` when it was alone in its brackets, `[...xs]`, which lowers to the element
/// itself, and `element` otherwise.
let private spreads
  (tokens : Lex.SpannedToken[])
  (ranges : Ranges)
  : List<string * Dval> =
  let isOpen (t : Tok.Token) =
    match t with
    | Tok.TLParen
    | Tok.TLBracket
    | Tok.TLBrace -> true
    | _ -> false
  let isClose (t : Tok.Token) =
    match t with
    | Tok.TRParen
    | Tok.TRBracket
    | Tok.TRBrace -> true
    | _ -> false
  // The index of the bracket enclosing token <param k>, looking in direction <param step>.
  let rec enclosing (k : int) (step : int) (depth : int) : Option<int> =
    if k < 0 || k >= tokens.Length then
      None
    else
      let t = tokens[k].token
      let opens = if step > 0 then isOpen t else isClose t
      let closes = if step > 0 then isClose t else isOpen t
      if closes && depth = 0 then Some k
      elif closes then enclosing (k + step) step (depth - 1)
      elif opens then enclosing (k + step) step (depth + 1)
      else enclosing (k + step) step depth
  let pos (p : Tok.Pos) = (p.row, p.column)
  let startingAt (p : Tok.Pos) =
    let key = packed p
    ranges |> Seq.filter (fun kv -> packed kv.Value.start = key) |> List.ofSeq
  [ for k in 0 .. tokens.Length - 2 do
      if tokens[k].token = Tok.TDotDotDot then
        match enclosing (k - 1) -1 0, enclosing (k + 1) 1 0 with
        | Some opening, Some closing when
          tokens[opening].token = Tok.TLBracket
          && tokens[closing].token = Tok.TRBracket
          ->
          let closeEnd = pos tokens[closing].range.end_
          let closeStart = pos tokens[closing].range.start
          // The call the list lowered to, and the list inside it: they span the brackets.
          for kv in startingAt tokens[opening].range.start do
            if pos kv.Value.end_ = closeEnd then yield string kv.Key, DString "call"
          // The element: the largest node starting after the `...` that ends inside them.
          let element =
            startingAt tokens[k + 1].range.start
            |> List.filter (fun kv -> pos kv.Value.end_ <= closeStart)
            |> List.sortByDescending (fun kv -> pos kv.Value.end_)
            |> List.tryHead
          match element with
          | Some kv ->
            let next =
              tokens
              |> Array.tryFind (fun t -> pos t.range.start >= pos kv.Value.end_)
            let alone =
              opening = k - 1
              && (match next with
                  | Some t -> pos t.range.start = closeStart
                  | None -> false)
            yield string kv.Key, DString(if alone then "alone" else "element")
          | None -> ()
        | _ -> () ]


/// How each plain string and char literal was written, quotes included, by node id, and which
/// calls were written as `-x` or `~x`: lowering
/// keeps only the text, and `"\u001b"` should print back as `"\u001b"`, not as another escape.
let private spellings
  (tokens : Lex.SpannedToken[])
  (ranges : Ranges)
  : List<string * Dval> =
  ranges
  |> Seq.choose (fun kv ->
    match tokenStartingAt tokens kv.Value.start with
    | Some t ->
      let whole = t.range.end_ = kv.Value.end_
      match t.token with
      | Tok.TStringLit _ when whole && not (t.text.StartsWith("\"\"\"")) ->
        Some(string kv.Key, DString t.text)
      | Tok.TCharLit _ when whole -> Some(string kv.Key, DString t.text)
      // `-x` and `~x` lower to a call of the trait method; the call starts at the operator.
      | Tok.TMinus
      | Tok.TBitNot -> Some(string kv.Key, DString t.text)
      | _ -> None
    | None -> None)
  |> List.ofSeq


/// The text between the interpolations of each `$"..."`, as written, by node id: what
/// `spellings` keeps for a plain string. Empty texts are left out, as lowering leaves them out.
let private textParts
  (tokens : Lex.SpannedToken[])
  (ranges : Ranges)
  : List<string * Dval> =
  let split (inner : string) : Option<List<string>> =
    let n = inner.Length
    let rec go (i : int) (start : int) (acc : List<string>) =
      if i >= n then
        Some(List.rev (inner.Substring(start) :: acc))
      elif inner[i] = '\\' then
        go (i + 2) start acc
      elif
        (inner[i] = '{' || inner[i] = '}') && i + 1 < n && inner[i + 1] = inner[i]
      then
        go (i + 2) start acc
      elif inner[i] = '{' then
        match Lex.findInterpExprClose inner n (i + 1) with
        | -1 -> None
        | close ->
          go (close + 1) (close + 1) (inner.Substring(start, i - start) :: acc)
      else
        go (i + 1) start acc
    go 0 0 []
  ranges
  |> Seq.choose (fun kv ->
    match tokenStartingAt tokens kv.Value.start with
    | Some t when
      t.token = Tok.TInterpString
      && t.range.end_ = kv.Value.end_
      && not (t.text.StartsWith("$\"\"\""))
      && t.text.Length >= 3
      ->
      split (t.text.Substring(2, t.text.Length - 3))
      |> Option.map (fun parts ->
        string kv.Key,
        parts
        |> List.filter (fun p -> p <> "")
        |> List.map DString
        |> Dval.list KTString)
    | _ -> None)
  |> List.ofSeq


/// <param pm> remembering what each name lookup found, misses included, for the one file being
/// lowered: the resolver asks for a name in scope after scope, the same few hundred names again and
/// again, and most of the answers are "not there".
let private rememberingNames (pm : PT.PackageManager) : PT.PackageManager =
  let remember (find : PT.PackageLocation -> Ply<Option<'a>>) =
    let seen =
      System.Collections.Concurrent.ConcurrentDictionary<PT.PackageLocation, Option<'a>>()
    fun (location : PT.PackageLocation) ->
      match seen.TryGetValue location with
      | true, found -> Ply found
      | _ ->
        uply {
          let! found = find location
          seen[location] <- found
          return found
        }
  { pm with
      findType = remember pm.findType
      findValue = remember pm.findValue
      findFn = remember pm.findFn
      findTrait = remember pm.findTrait
      findTraitImpl = remember pm.findTraitImpl
      findTest = remember pm.findTest }


let private parse
  (state : ExecutionState)
  (source : string)
  : Ply<Result<Dval, string>> =
  uply {
    match P.parseFor Validation.Package source, Lex.tokenize source with
    | Error diagnostics, _ ->
      let message =
        match P.parseFor Validation.Script source with
        // A script parses where a file of declarations doesn't: say that, not the first
        // thing about it that a file of declarations can't have.
        | Ok _ ->
          "a script: `format` lays out files of declarations (`module ...` and what is in it), and leaves scripts as they are"
        | Error _ ->
          match diagnostics with
          | d :: _ -> P.renderDiagnostic source d
          | [] -> "the file doesn't parse"
      return Error message
    | _, Error message -> return Error message
    | Ok validated, Ok(tokenList, _) ->
      let tokens = Array.ofList tokenList
      let lines = source.Split('\n')
      let ctx =
        { builtins = state.builtins
          pm = rememberingNames (LibDB.PackageManager.ptForBranch state.branchId)
          lines = lines
          lineStarts =
            lines
            // In characters as Dark counts them (`String.slice`), not UTF-16 units.
            |> Array.scan
              (fun offset (line : string) -> offset + String.lengthInEgcs line + 1)
              0
          tokens = tokens
          ranges = Ranges()
          fnSpans = ResizeArray() }
      let sf = Validation.ValidatedSourceFile.toWrittenTypes validated
      let! decls = declarations ctx [] sf.declarations
      let! locals = localFns ctx source tokens
      // The range of every node a comment could belong to, in the order lowering noted them: a
      // node's parts before it.
      let (starts, rows) = commentSpots tokens
      let ranges =
        ctx.ranges
        |> Seq.filter (fun kv ->
          starts.Contains(packed kv.Value.start)
          || rows.Contains(int64 kv.Value.end_.row))
        |> Seq.map (fun kv ->
          DTuple(
            DString(string kv.Key),
            DInt64(packed kv.Value.start),
            [ DInt64(packed kv.Value.end_) ]
          ))
        |> List.ofSeq
      // The strings written between triple quotes, which lowering doesn't keep.
      let tripleQuoted =
        ctx.ranges
        |> Seq.filter (fun kv ->
          let start = kv.Value.start
          match tokenStartingAt ctx.tokens start with
          | Some t -> t.text.StartsWith("\"\"\"") || t.text.StartsWith("$\"\"\"")
          | None -> false)
        |> Seq.map (fun kv -> DString(string kv.Key))
        |> List.ofSeq
      let spreads = spreads tokens ctx.ranges
      let spellings = spellings tokens ctx.ranges
      let textParts = textParts tokens ctx.ranges
      return
        Ok(
          record
            (tn Refs.file)
            [ "declarations", Dval.list (KTCustomType(tn Refs.declaration, [])) decls
              "text", DString source
              "rawComments",
              Dval.list (KTCustomType(tn Refs.rawComment, [])) (rawComments tokens)
              "ranges", Dval.list (KTTuple(VT.string, VT.int64, [ VT.int64 ])) ranges
              "localFns", Dval.stringDict (KTCustomType(tn Refs.localFn, [])) locals
              "tripleQuoted", Dval.list KTString tripleQuoted
              "spreads", Dval.stringDict KTString spreads
              "spellings", Dval.stringDict KTString spellings
              "textParts", Dval.stringDict (KTList(VT.known KTString)) textParts ]
        )
  }


/// The text of every `//` comment in <param source>, in order, trimmed: what formatting must
/// keep. Empty if it doesn't tokenize.
let private commentTexts (source : string) : List<string> =
  match Lex.tokenize source with
  | Ok(tokens, _) ->
    [ for t in tokens do
        for c in t.leadingTrivia do
          match c.kind with
          | Lex.LineComment
          | Lex.BlockComment -> yield c.text.Trim()
          | Lex.DocComment -> () ]
  | Error _ -> []


let fns () : List<BuiltInFn> =
  [ { name = fn "formatterComments" 0
      typeParams = []
      parameters = [ Param.make "source" TString "" ]
      returnType = TList TString
      description =
        "The text of every `//` comment in <param source>, in order and trimmed: what formatting has to keep."
      fn =
        (function
        | _, _, _, [| DString source |] ->
          commentTexts source |> List.map DString |> Dval.list KTString |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "formatterParse" 0
      typeParams = []
      parameters =
        [ Param.make "source" TString "The text of a package `.dark` file" ]
      returnType =
        TypeReference.result (TCustomType(NR.ok (tn Refs.file), [])) TString
      description =
        "Parses and lowers a package file for the formatter: each declaration as the loader would store it, with the comments, node lines and local fn signatures lowering drops. An error is the parser's first diagnostic."
      fn =
        (function
        | state, _, _, [| DString source |] ->
          uply {
            let fileKT = KTCustomType(tn Refs.file, [])
            match! parse state source with
            | Ok file -> return Dval.resultOk fileKT KTString file
            | Error message ->
              return Dval.resultError fileKT KTString (DString message)
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
