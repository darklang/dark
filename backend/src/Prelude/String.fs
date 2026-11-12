module String

/// Whether `c` is a cluster boundary on both sides when its neighbour is one of these too: every
/// char below U+0300 except CR. That is ASCII, Latin-1 and Latin Extended, so precomposed accented
/// letters as well as plain text.
///
/// Every such char has Grapheme_Cluster_Break Other, Control or LF, and no Indic_Conjunct_Break.
/// The rules that join two chars all need one of them to be something else: CR before LF, an
/// Extend, ZWJ or SpacingMark after, a Prepend before, a Hangul or regional-indicator pair. U+0300
/// is the first combining mark. `Prelude.Tests` checks every pair against `StringInfo`.
let inline private isSimple (c : char) : bool = c < '\u0300' && c <> '\r'

/// The UTF-16 offset just past the cluster that starts at `offset`, which is below `s.Length`.
///
/// Two simple chars in a row end a cluster without asking `StringInfo`, which is most of the cost
/// of walking text that is mostly letters.
let clusterEnd (s : string) (offset : int) : int =
  let next = offset + 1
  if isSimple s[offset] && (next = s.Length || isSimple s[next]) then
    next
  else
    offset
    + System.Globalization.StringInfo.GetNextTextElementLength(
      System.MemoryExtensions.AsSpan(s, offset)
    )

/// The offset `count` clusters on from the boundary `offset`, or `s.Length` if the text runs out
/// first.
let rec clusterOffset (s : string) (offset : int) (count : int) : int =
  if count <= 0 || offset >= s.Length then
    offset
  else
    clusterOffset s (clusterEnd s offset) (count - 1)

/// True when the string's extended grapheme clusters are exactly its chars, so a char-wise
/// operation gives the same answer as the EGC-correct one.
///
/// Purely a performance guard: the EGC implementations build one string per cluster, and most
/// strings a program splits or measures are paths, keys, identifiers and words. Correctness never
/// depends on which branch is taken, so if in doubt, return false.
///
/// CR is rejected along with everything from U+0300 up, because CRLF is a single cluster.
/// Rejecting outright rather than looking ahead keeps this to a few vectorized scans. The ASCII
/// check goes first because it is the cheapest scan, and it answers for most text.
let isCharwise (s : string) : bool =
  let span = System.MemoryExtensions.AsSpan s
  (System.Text.Ascii.IsValid span
   || System.MemoryExtensions.IndexOfAnyExceptInRange(span, char 0, '\u02FF') < 0)
  && s.IndexOf('\r') < 0

/// One string for each char below U+0300, so splitting text into its characters allocates no
/// string for a letter.
let private simpleStrings : string array =
  Array.init 0x300 (fun i -> System.String(char i, 1))

/// The cluster of `s` from `start` to `stop`.
let private cluster (s : string) (start : int) (stop : int) : string =
  if stop = start + 1 && s[start] < '\u0300' then
    simpleStrings[int s[start]]
  else
    s.Substring(start, stop - start)

let rec private clustersOnto
  (f : string -> 'a)
  (s : string)
  (offset : int)
  (acc : List<'a>)
  : List<'a> =
  if offset >= s.Length then
    acc
  else
    let stop = clusterEnd s offset
    clustersOnto f s stop (f (cluster s offset stop) :: acc)

let rec private charsOnto
  (f : string -> 'a)
  (s : string)
  (i : int)
  (acc : List<'a>)
  : List<'a> =
  if i < 0 then acc else charsOnto f s (i - 1) (f simpleStrings[int s[i]] :: acc)

/// `f` of each cluster of `s`, last first.
let mapEgcsReversed (f : string -> 'a) (s : string) : List<'a> =
  clustersOnto f s 0 []

/// `f` of each extended grapheme cluster (essentially a visible screen character) of `s`, in order.
let mapEgcs (f : string -> 'a) (s : string) : List<'a> =
  if isCharwise s then
    // Built from the end, so the list needs no reversing.
    charsOnto f s (s.Length - 1) []
  else
    List.rev (mapEgcsReversed f s)

/// The extended grapheme clusters of `s`, in order.
let egcs (s : string) : List<string> = mapEgcs id s

/// The extended grapheme clusters of `s`, last first.
let egcsReversed (s : string) : List<string> = mapEgcsReversed id s

/// Return Some(c) if the string is a single EGC character, otherwise None
let toEgcChar (str : string) : Option<string> =
  if str <> "" && clusterEnd str 0 = str.Length then Some str else None


let splitOnNewline (str : string) : List<string> =
  if str = "" then
    []
  else
    str.Split([| "\n"; "\r\n" |], System.StringSplitOptions.None) |> Array.toList

let rec private countClusters (s : string) (offset : int) (count : int) : int =
  if offset >= s.Length then
    count
  else
    countClusters s (clusterEnd s offset) (count + 1)

/// The number of extended grapheme clusters in `s`. Steps from cluster to cluster rather than
/// asking `StringInfo` for `LengthInTextElements`, which builds an index of every cluster first.
let lengthInEgcs (s : string) : int =
  if isCharwise s then s.Length else countClusters s 0 0

/// Clamp a Dark `Int64` index against a cluster length, the way `String.slice` always has: a negative
/// index counts from the end, then the result is clamped to [0, len].
///
/// Clamps in the `Int64` domain and only then narrows -- narrowing first raises `OutOfRange` on an
/// index past int32, where this clamps. `String.slice "abc" 0 4503599627370498` is "abc", and there
/// is a testfile line for it.
let private normalizeEgcIndex (len : int) (i : int64) : int =
  // `len + i` cannot overflow for a negative `i`: `len` is at most int32.
  let i = if i < 0L then int64 len + i else i
  if i < 0L then 0
  elif i > int64 len then len
  else int i

/// A non-negative index, clamped to `s.Length`. No string has more clusters than chars, so a
/// walk from the start that runs out of text stops where clamping to the length would.
let private clampToChars (s : string) (i : int64) : int =
  if i > int64 s.Length then s.Length else int i

/// The clusters of `s` from index `first` up to `last`, as `String.slice` counts them: a negative
/// index counts from the end, and both are clamped to the string.
///
/// With both indices from the start there is no need to know the length, so the walk stops at
/// `last` rather than going on to the end of the string.
let sliceEgcs (s : string) (first : int64) (last : int64) : string =
  if isCharwise s then
    let first = normalizeEgcIndex s.Length first
    let last = normalizeEgcIndex s.Length last
    if first >= last then "" else s.Substring(first, last - first)
  else
    let first, last =
      if first >= 0L && last >= 0L then
        clampToChars s first, clampToChars s last
      else
        let len = lengthInEgcs s
        normalizeEgcIndex len first, normalizeEgcIndex len last
    if first >= last then
      ""
    else
      let start = clusterOffset s 0 first
      s.Substring(start, clusterOffset s start (last - first) - start)

/// `s` without its first `count` clusters, or all of it when `count` is negative.
let dropFirstEgcs (s : string) (count : int64) : string =
  if count <= 0L then
    s
  elif isCharwise s then
    (if count >= int64 s.Length then "" else s.Substring(int count))
  else
    s.Substring(clusterOffset s 0 (clampToChars s count))

/// Whether walking clusters from the boundary `offset` lands exactly on `target`.
let rec landsOn (s : string) (target : int) (offset : int) : bool =
  if offset >= target then
    offset = target
  else
    landsOn s target (clusterEnd s offset)

/// The first offset at or after `offset` whose char is U+0300 or above or is CR, or `s.Length`.
/// Every char before it, except perhaps the last, is a cluster of its own: that one may combine
/// with the char after. See `String.clusterEnd`.
let private simpleRunEnd (s : string) (offset : int) : int =
  let rest = System.MemoryExtensions.AsSpan(s, offset)
  let other = System.MemoryExtensions.IndexOfAnyExceptInRange(rest, char 0, '\u02FF')
  let runEnd = if other < 0 then s.Length else offset + other
  let cr = s.IndexOf('\r', offset, runEnd - offset)
  if cr < 0 then runEnd else cr

/// The UTF-16 offset of the first `needle` in `s` at or after `from` that starts and ends on
/// cluster boundaries, or -1. `needle` is not empty.
///
/// `offset` is a cluster boundary at or before `from`, and `index` the number of clusters before
/// it. Both move forward as the search walks, so a search that carries on from a match does not
/// walk the text again. Searches ordinally and walks only as far as a hit, so a miss costs what
/// ordinal `IndexOf` does, and a run of simple chars is jumped rather than walked.
let nextEgcMatch
  (s : string)
  (needle : string)
  (from : int)
  (offset : byref<int>)
  (index : byref<int>)
  : int =
  let mutable at = s.IndexOf(needle, from, System.StringComparison.Ordinal)
  let mutable found = -1
  let mutable runEnd = offset
  while found < 0 && at >= 0 do
    while offset < at do
      if offset >= runEnd then runEnd <- simpleRunEnd s offset
      // Jump over simple chars, one cluster per char, rather than asking for each cluster.
      let jump = min at (runEnd - 1)
      if jump > offset then
        index <- index + (jump - offset)
        offset <- jump
      else
        offset <- clusterEnd s offset
        index <- index + 1
    if offset = at && landsOn s (at + needle.Length) at then
      found <- at
    else
      at <- s.IndexOf(needle, at + 1, System.StringComparison.Ordinal)
  found

/// The cluster index of the first whole-cluster `needle` in `s`, or -1. `needle` is not empty.
let egcIndexOf (s : string) (needle : string) : int =
  if isCharwise s then
    s.IndexOf(needle, System.StringComparison.Ordinal)
  else
    let mutable offset = 0
    let mutable index = 0
    if nextEgcMatch s needle 0 &offset &index < 0 then -1 else index

/// Whether the first clusters of `subject` are the clusters of `prefix`.
///
/// Prefix tests are cluster tests in Dark, not char tests: "\U0001F471" is an ordinal prefix of
/// "\U0001F471\U0001F3FB" but not a cluster-prefix of it. An ordinal prefix whose end is a cluster
/// boundary of `subject` is a cluster-prefix: every cluster before it was found looking only at
/// chars the two strings share.
let startsWithEgcs (subject : string) (prefix : string) : bool =
  let p = prefix.Length
  subject.StartsWith(prefix, System.StringComparison.Ordinal)
  && (p = 0
      || p = subject.Length
      || (isSimple subject[p - 1] && isSimple subject[p])
      || landsOn subject p 0)

/// Whether the last clusters of `subject` are the clusters of `suffix`: an ordinal suffix that
/// starts on a cluster boundary of `subject`. After a simple char, only the next char decides
/// whether a cluster starts there, so most suffixes are checked without walking `subject`.
let endsWithEgcs (subject : string) (suffix : string) : bool =
  let start = subject.Length - suffix.Length
  subject.EndsWith(suffix, System.StringComparison.Ordinal)
  && (start = 0
      || (isSimple subject[start - 1] && clusterEnd subject (start - 1) = start)
      || landsOn subject start 0)

let normalize (s : string) : string = s.Normalize()

/// <param a> then <param b>, normalized: two normalized strings can join into one that is not.
/// What `stringAppend` and String `+` compute, in the builtins and in the VM.
let append (a : string) (b : string) : string = normalize (a + b)

/// Orders two strings by code point, which is also the order of their UTF-8 bytes. Only the sign
/// of the result is meaningful.
///
/// Ordinal UTF-16 order differs only where a surrogate pair meets a char in U+E000..U+FFFF: the
/// pair is the larger code point, but its first unit is the smaller char.
let compareByCodepoint (a : string) (b : string) : int =
  let i =
    System.MemoryExtensions.CommonPrefixLength(
      System.MemoryExtensions.AsSpan a,
      System.MemoryExtensions.AsSpan b
    )
  if i = a.Length || i = b.Length then
    a.Length - b.Length
  else
    // Moves the surrogates above U+E000..U+FFFF and leaves everything below U+D800 alone.
    let rank (c : char) =
      let n = int c
      if n < 0xD800 then n
      elif n >= 0xE000 then n - 0x800
      else n + 0x2000
    rank a[i] - rank b[i]


/// `s` repeated `n` times; "" when `n` is not positive or `s` is empty.
///
/// Shared so the `stringRepeat` builtin and the interpreter's operator fast path cannot drift apart.
/// A single character is the common case in a TUI -- padding a row out to a column -- and
/// `System.String(char, count)` is one allocation and a fill, where concatenating a sequence of `n`
/// strings builds the sequence and then the result.
let repeat (s : string) (n : int) : string =
  if n <= 0 || s = "" then ""
  elif s.Length = 1 then System.String(s[0], n)
  else System.String.Concat(Seq.replicate n s)

// Ordinal (not culture-aware): with InvariantGlobalization=true on AOT
// builds, `InvariantCultureIgnoreCase` is restricted to invariant rules
// anyway, but `Ordinal` is unambiguous and matches what the callers
// (HTTP headers, hostnames, SSRF-allowlist tokens) actually want.
// Keeps Debug / Release / AOT behavior identical.
let equalsCaseInsensitive (s1 : string) (s2 : string) : bool =
  System.String.Equals(s1, s2, System.StringComparison.OrdinalIgnoreCase)

let replace (old : string) (newStr : string) (s : string) : string =
  s.Replace(old, newStr)

let take (count : int) (str : string) : string =
  if count >= str.Length then str else str.Substring(0, count)

let isCapitalized (s : string) : bool =
  if s.Length = 0 then
    false
  else
    let firstChar = s[0]
    firstChar >= 'A' && firstChar <= 'Z'

let toLowercase (s : string) : string = s.ToLower()

let toUppercase (s : string) : string = s.ToUpper()

let trim (s : string) : string = s.Trim()

let endsWith (suffix : string) (s : string) : bool = s.EndsWith(suffix)

let startsWith (prefix : string) (s : string) : bool = s.StartsWith(prefix)

let isEmpty (s : string) : bool = s = ""

let split (on : string) (s : string) : List<string> =
  // Splitting an empty string with `Split` produces `[""]`, which is unexpected
  if s = "" then [] else s.Split(on) |> List.ofArray

let trimLeft (s : string) : string = s.TrimStart()

let dropLeft (count : int) (s : string) : string =
  if count >= s.Length then "" else s.Substring(count)

let dropRight (count : int) (s : string) : string =
  if count >= s.Length then "" else s.Substring(0, s.Length - count)

let contains (substring : string) (s : string) : bool = s.Contains(substring)
