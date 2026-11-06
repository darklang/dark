/// Regular expressions, without backtracking.
///
/// Every pattern runs under `RegexOptions.NonBacktracking`, which matches in time
/// linear in the input, so a pattern cannot hang the way `(a+)+$` does under a
/// backtracking engine. That is the RE2 trade: backreferences, lookarounds, atomic
/// groups, conditionals and `\G` are refused with `UnsupportedSyntax` rather than
/// run. A timeout still bounds every call, for huge inputs.
///
/// One builtin, `regexScan`, does all the matching. `Stdlib.Regex` builds every
/// other fn from the text between matches and the matches themselves.
module Builtins.Pure.Libs.Regex

open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution


/// The longest one call may spend matching. Matching is linear, so this bounds input
/// size, not pattern shape.
let private timeout = System.TimeSpan.FromSeconds 1.0


type private RegexError =
  /// The reason, and the character of the pattern it points at.
  | InvalidPattern of message : string * position : int
  /// A construct that needs backtracking, which this engine does not do.
  | UnsupportedSyntax of construct : string
  /// A match boundary falls between the two halves of a surrogate pair.
  | InvalidUnicodeMatch
  | MatchLimitExceeded


let private errorTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Regex.regexError ())

let private matchTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Regex.match_ ())

let private captureTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Regex.capture ())

let private rangeTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Regex.range ())


let private errorToDT (e : RegexError) : Dval =
  let (caseName, fields) =
    match e with
    | InvalidPattern(message, position) ->
      "InvalidPattern", [ DString message; DInt64(int64 position) ]
    | UnsupportedSyntax construct -> "UnsupportedSyntax", [ DString construct ]
    | InvalidUnicodeMatch -> "InvalidUnicodeMatch", []
    | MatchLimitExceeded -> "MatchLimitExceeded", []
  let typeName = errorTypeName ()
  DEnum(typeName, typeName, [], caseName, fields)


/// Character (grapheme cluster) positions in one string, counted the way
/// `String.length` and `String.slice` count them. The cluster table is built on
/// first use, and never for a string whose clusters are its chars.
type private Positions(s : string) =
  let charwise = String.isCharwise s
  let mutable starts : int[] = null

  member private _.Starts =
    if isNull starts then
      starts <- System.Globalization.StringInfo.ParseCombiningCharacters s
    starts

  /// The character a boundary at UTF-16 offset `i` starts: the one containing it.
  member this.Floor(i : int) : int =
    if charwise then
      i
    elif i >= s.Length then
      this.Starts.Length
    else
      let k = System.Array.BinarySearch(this.Starts, i)
      if k >= 0 then k else ~~~k - 1

  /// The character a boundary at UTF-16 offset `i` ends: just past the one
  /// containing it.
  member this.Ceiling(i : int) : int =
    if charwise then
      i
    elif i >= s.Length then
      this.Starts.Length
    else
      let k = System.Array.BinarySearch(this.Starts, i)
      if k >= 0 then k else ~~~k


let private cache =
  System.Collections.Concurrent.ConcurrentDictionary<struct (string * RegexOptions), Regex>()

/// Plenty for a program's working set of patterns. Past it, start over rather than
/// track use: building a non-backtracking regex is the expensive part, not looking
/// one up.
let private cacheLimit = 256

/// Why a pattern does not parse. Read off the error code rather than the message:
/// the CLI is built with `UseSystemResourceKeys`, so its exception messages are
/// resource keys.
let private parseReason (e : RegexParseException) : string =
  match e.Error with
  | RegexParseError.AlternationHasTooManyConditions ->
    "a conditional has too many alternatives"
  | RegexParseError.AlternationHasMalformedCondition -> "malformed condition"
  | RegexParseError.InvalidUnicodePropertyEscape -> "invalid \\p or \\P escape"
  | RegexParseError.MalformedUnicodePropertyEscape -> "malformed \\p or \\P escape"
  | RegexParseError.UnrecognizedEscape -> "unrecognized escape"
  | RegexParseError.UnrecognizedControlCharacter -> "unrecognized control character"
  | RegexParseError.MissingControlCharacter -> "missing control character after \\c"
  | RegexParseError.InsufficientOrInvalidHexDigits -> "too few or invalid hex digits"
  | RegexParseError.QuantifierOrCaptureGroupOutOfRange -> "number too large"
  | RegexParseError.UndefinedNamedReference -> "reference to an undefined group name"
  | RegexParseError.UndefinedNumberedReference -> "reference to an undefined group"
  | RegexParseError.MalformedNamedReference -> "malformed named reference"
  | RegexParseError.UnescapedEndingBackslash -> "ends in an unescaped backslash"
  | RegexParseError.UnterminatedComment -> "unterminated comment"
  | RegexParseError.InvalidGroupingConstruct -> "invalid group"
  | RegexParseError.AlternationHasNamedCapture -> "a conditional holds a named group"
  | RegexParseError.AlternationHasComment -> "a conditional holds a comment"
  | RegexParseError.AlternationHasMalformedReference ->
    "a conditional has a malformed reference"
  | RegexParseError.AlternationHasUndefinedReference ->
    "a conditional refers to an undefined group"
  | RegexParseError.CaptureGroupNameInvalid -> "invalid group name"
  | RegexParseError.CaptureGroupOfZero -> "group 0 cannot be named"
  | RegexParseError.UnterminatedBracket -> "unterminated ["
  | RegexParseError.ExclusionGroupNotLast -> "a subtracted set must come last"
  | RegexParseError.ReversedCharacterRange -> "range in reverse order"
  | RegexParseError.ShorthandClassInCharacterRange ->
    "a class like \\d cannot end a range"
  | RegexParseError.InsufficientClosingParentheses -> "not enough )"
  | RegexParseError.ReversedQuantifierRange -> "{x,y} with x greater than y"
  | RegexParseError.NestedQuantifiersNotParenthesized -> "nested quantifier"
  | RegexParseError.QuantifierAfterNothing -> "quantifier after nothing"
  | RegexParseError.InsufficientOpeningParentheses -> "too many )"
  | RegexParseError.UnrecognizedUnicodeProperty -> "unrecognized Unicode property"
  | _ -> "invalid pattern"

/// What a non-backtracking engine refused. .NET says only in the message, which is a
/// resource key under `UseSystemResourceKeys` (`...,
/// ExpressionDescription_Backreference`) and a sentence without it, so look for
/// words both forms contain.
let private refusal (message : string) : RegexError =
  let m = message.ToLowerInvariant()
  let has (word : string) = m.Contains(word, System.StringComparison.Ordinal)
  // Conditionals first: one form is described as a "conditional backreference".
  if has "conditional" || has "ifthenelse" then
    UnsupportedSyntax "conditional"
  elif has "balancing" then
    UnsupportedSyntax "balancing group"
  elif has "atomic" then
    UnsupportedSyntax "atomic group"
  elif has "lookaround" || has "lookahead" then
    UnsupportedSyntax "lookahead or lookbehind"
  elif has "contiguous" then
    UnsupportedSyntax "\\G"
  elif has "backreference" then
    UnsupportedSyntax "backreference"
  // The other refusal is size: the automaton the pattern needs is too large.
  else
    InvalidPattern("pattern too large", 0)

let private compile
  (pattern : string)
  (caseInsensitive : bool)
  (multiline : bool)
  : Result<Regex, RegexError> =
  let options =
    RegexOptions.NonBacktracking
    ||| RegexOptions.CultureInvariant
    ||| (if caseInsensitive then RegexOptions.IgnoreCase else RegexOptions.None)
    ||| (if multiline then RegexOptions.Multiline else RegexOptions.None)
  let key = struct (pattern, options)
  match cache.TryGetValue key with
  | true, regex -> Ok regex
  | _ ->
    try
      let regex = Regex(pattern, options, timeout)
      if cache.Count >= cacheLimit then cache.Clear()
      cache[key] <- regex
      Ok regex
    with
    | :? RegexParseException as e ->
      let position = Positions(pattern).Floor(min e.Offset pattern.Length)
      Error(InvalidPattern(parseReason e, position))
    | :? System.NotSupportedException as e -> Error(refusal e.Message)


/// True when UTF-16 offset `i` falls between the halves of a surrogate pair.
let private splitsPair (s : string) (i : int) : bool =
  i > 0
  && i < s.Length
  && System.Char.IsHighSurrogate s[i - 1]
  && System.Char.IsLowSurrogate s[i]


/// The text between matches, and the matches. There is always one more piece than
/// match.
let private scan
  (regex : Regex)
  (input : string)
  (firstOnly : bool)
  : Result<List<Dval> * List<Dval>, RegexError> =
  let positions = Positions(input)
  let rangeType = rangeTypeName ()
  let captureType = captureTypeName ()
  let matchType = matchTypeName ()
  let captureKT = KTCustomType(captureType, [])
  let optionCaptureKT = KTCustomType(Dval.optionType (), [ VT.known captureKT ])

  // Unnamed groups go in `captures`, in order, and named ones in `namedCaptures`.
  // .NET numbers named groups after all the unnamed ones, so one list would misorder
  // them.
  let groups =
    regex.GetGroupNumbers()
    |> Array.filter (fun n -> n <> 0)
    |> Array.map (fun n ->
      let name = regex.GroupNameFromNumber n
      struct (n, (if name = string n then None else Some name)))

  let range (start : int) (finish : int) : Dval =
    DRecord(
      rangeType,
      rangeType,
      [],
      RecordFields.ofList
        [ "start", DInt64(int64 (positions.Floor start))
          "end", DInt64(int64 (positions.Ceiling finish)) ]
    )

  let capture (g : Group) : Option<Dval> =
    if g.Success then
      Some(
        DRecord(
          captureType,
          captureType,
          [],
          RecordFields.ofList
            [ "text", DString g.Value; "range", range g.Index (g.Index + g.Length) ]
        )
      )
    else
      None

  let validGroup (g : Group) : bool =
    not g.Success
    || not (splitsPair input g.Index || splitsPair input (g.Index + g.Length))

  let toDT (m : Match) : Dval =
    let mutable captures = []
    let mutable named = []
    for struct (n, name) in Array.rev groups do
      let c = Dval.option captureKT (capture m.Groups[n])
      match name with
      | None -> captures <- c :: captures
      | Some name -> named <- (name, c) :: named
    DRecord(
      matchType,
      matchType,
      [],
      RecordFields.ofList
        [ "text", DString m.Value
          "range", range m.Index (m.Index + m.Length)
          "captures", Dval.list optionCaptureKT captures
          "namedCaptures", Dval.stringDict optionCaptureKT named ]
    )

  try
    let mutable pieces = []
    let mutable matches = []
    let mutable last = 0
    let mutable m = regex.Match input
    let mutable broken = false
    while m.Success && not broken do
      // Group 0 is the match itself.
      let mutable valid = true
      for i in 0 .. m.Groups.Count - 1 do
        if valid && not (validGroup m.Groups[i]) then valid <- false
      if valid then
        pieces <- DString(input.Substring(last, m.Index - last)) :: pieces
        matches <- toDT m :: matches
        last <- m.Index + m.Length
        m <- if firstOnly then Match.Empty else m.NextMatch()
      else
        broken <- true
    if broken then
      Error InvalidUnicodeMatch
    else
      pieces <- DString(input.Substring last) :: pieces
      Ok(List.rev pieces, List.rev matches)
  with :? RegexMatchTimeoutException ->
    Error MatchLimitExceeded


/// The text between matches only, for the fns that never look at a match. Several
/// times cheaper than `scan`: the engine skips the pass that finds groups, and no
/// `Match` values are built.
let private scanPieces
  (regex : Regex)
  (input : string)
  (firstOnly : bool)
  : Result<List<Dval>, RegexError> =
  try
    let mutable pieces = []
    let mutable last = 0
    let mutable broken = false
    let mutable stop = false
    let mutable e = regex.EnumerateMatches input
    while not stop && e.MoveNext() do
      let m = e.Current
      if splitsPair input m.Index || splitsPair input (m.Index + m.Length) then
        broken <- true
        stop <- true
      else
        pieces <- DString(input.Substring(last, m.Index - last)) :: pieces
        last <- m.Index + m.Length
        if firstOnly then stop <- true
    if broken then
      Error InvalidUnicodeMatch
    else
      pieces <- DString(input.Substring last) :: pieces
      Ok(List.rev pieces)
  with :? RegexMatchTimeoutException ->
    Error MatchLimitExceeded


let fns () : List<BuiltInFn> =
  [ { name = fn "regexScan" 0
      typeParams = []
      parameters =
        [ Param.make "pattern" TString "The regular expression"
          Param.make "caseInsensitive" TBool "Ignore case when matching"
          Param.make "multiline" TBool "Let ^ and $ match at every line break"
          Param.make "input" TString "The string to search"
          Param.make "firstOnly" TBool "Stop after the first match"
          Param.make
            "withMatches"
            TBool
            "Also build the matches. Without them the list of matches is empty, and the call is much cheaper" ]
      returnType =
        TypeReference.result
          (TTuple(
            TList TString,
            TList(TCustomType(NR.ok (matchTypeName ()), [])),
            []
          ))
          (TCustomType(NR.ok (errorTypeName ()), []))
      description =
        "Matches <param pattern> against <param input> without backtracking. Returns the text between matches, which always has one more entry than the matches, and the matches when <param withMatches> asks for them. A bad or unsupported pattern, a match that splits a surrogate pair, and a match that runs out of time are errors."
      fn =
        (function
        | _,
          _,
          _,
          [| DString pattern
             DBool caseInsensitive
             DBool multiline
             DString input
             DBool firstOnly
             DBool withMatches |] ->
          let matchKT = KTCustomType(matchTypeName (), [])
          let okKT =
            KTTuple(
              VT.known (KTList VT.string),
              VT.known (KTList(VT.known matchKT)),
              []
            )
          let errorKT = KTCustomType(errorTypeName (), [])
          let result =
            compile pattern caseInsensitive multiline
            |> Result.bind (fun regex ->
              if withMatches then
                scan regex input firstOnly
              else
                scanPieces regex input firstOnly |> Result.map (fun p -> p, []))
          match result with
          | Ok(pieces, matches) ->
            DTuple(Dval.list KTString pieces, Dval.list matchKT matches, [])
            |> Dval.resultOk okKT errorKT
            |> Ply
          | Error e -> errorToDT e |> Dval.resultError okKT errorKT |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
