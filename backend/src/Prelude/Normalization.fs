/// Unicode normalization (UAX #15): NFC, NFD, NFKC and NFKD.
///
/// .NET's `String.Normalize` calls ICU, and the shipped CLI is built with
/// `InvariantGlobalization=true`, which leaves ICU out and makes `Normalize` return its input
/// unchanged. So this does it in F#, from the tables in `NormalizationData`, and every build
/// normalizes the same way. It gives what `String.Normalize` gives with ICU, which the tests check
/// for every codepoint and for Unicode's NormalizationTest.txt.
///
/// Most strings are already normalized, and most of those are ASCII, so the work is ordered by
/// how likely it is to be needed: a vectorized scan for anything that could change, then a quick
/// check of the rest (UAX #15 section 9), and only then the full algorithm, from the last point
/// the quick check found safe.
module Normalization

open System.Collections.Generic
open System.Text

module D = NormalizationData

// Hangul syllables decompose and compose arithmetically (Unicode chapter 3.12).
let private sBase = 0xAC00
let private lBase = 0x1100
let private vBase = 0x1161
let private tBase = 0x11A7
let private lCount = 19
let private vCount = 21
let private tCount = 28
let private nCount = vCount * tCount
let private sCount = lCount * nCount

/// `D.properties` as two arrays: where each range starts, and its combining class with its
/// quick-check bits above it.
let private propertyRanges : Lazy<int[] * int[]> =
  lazy
    let rows = D.properties.Length / 3
    Array.init rows (fun r -> D.properties[3 * r]),
    Array.init rows (fun r -> D.properties[3 * r + 1] ||| D.properties[3 * r + 2])

let private propertiesFromRanges (cp : int) : int =
  let starts, values = propertyRanges.Force()
  let i = System.Array.BinarySearch(starts, cp)
  values[if i >= 0 then i else ~~~i - 1]

/// `propertiesFromRanges` for every char, laid out flat: 128 KB, filled in about 0.05 ms the
/// first time a string needs it, and a lookup is then an index instead of a search.
let private bmpProperties : Lazy<uint16[]> =
  lazy
    let table = Array.zeroCreate<uint16> 0x10000
    let starts, values = propertyRanges.Force()
    starts
    |> Array.iteri (fun i start ->
      if start < 0x10000 then
        let stop =
          if i + 1 < starts.Length then min 0x10000 starts[i + 1] else 0x10000
        System.Array.Fill(table, uint16 values[i], start, stop - start))
    table

/// The combining class (low byte) and quick-check bits of `cp`.
let private properties (cp : int) : int =
  if cp < 0x10000 then
    int (bmpProperties.Force()[cp])
  else
    propertiesFromRanges cp

let private combiningClass (cp : int) : int = properties cp &&& 0xFF

let private isHangulSyllable (cp : int) : bool = cp >= sBase && cp < sBase + sCount

/// A decomposition table's codepoints, and where each one's row starts.
let private rowsOf (table : int[]) : int[] * int[] =
  let rec starts (i : int) (acc : List<int>) =
    if i >= table.Length then
      acc.ToArray()
    else
      acc.Add i
      starts (i + 2 + table[i + 1]) acc
  let rows = starts 0 (List())
  Array.map (fun row -> table[row]) rows, rows

let private canonicalRows = lazy (rowsOf D.canonicalDecompositions)

let private compatibilityRows = lazy (rowsOf D.compatibilityDecompositions)

/// `D.compositions`' pairs, each packed as `first <<< 21 ||| second`, in the table's order.
let private compositionPairs : Lazy<int64[]> =
  lazy
    Array.init (D.compositions.Length / 3) (fun r ->
      (int64 D.compositions[3 * r] <<< 21) ||| int64 D.compositions[3 * r + 1])

/// Calls `f` on each codepoint of `cp`'s full decomposition, or on `cp` if it has none. Only a
/// codepoint that fails the NFD or NFKD quick check has one.
let private decompose (compatibility : bool) (f : int -> unit) (cp : int) : unit =
  let mapped (table : int[]) (rows : Lazy<int[] * int[]>) : bool =
    let keys, starts = rows.Force()
    let i = System.Array.BinarySearch(keys, cp)
    if i >= 0 then
      let row = starts[i]
      for j in row + 2 .. row + 1 + table[row + 1] do
        f table[j]
    i >= 0
  if properties cp &&& (if compatibility then D.nfkdFails else D.nfdFails) = 0 then
    f cp
  elif isHangulSyllable cp then
    let s = cp - sBase
    f (lBase + s / nCount)
    f (vBase + (s % nCount) / tCount)
    if s % tCount <> 0 then f (tBase + s % tCount)
  elif
    not (
      (compatibility && mapped D.compatibilityDecompositions compatibilityRows)
      || mapped D.canonicalDecompositions canonicalRows
    )
  then
    f cp

/// The primary composite of `first` and `second`, if there is one. Only a codepoint that fails
/// the NFC quick check can be the second half of one.
let private compose (first : int) (second : int) : ValueOption<int> =
  if properties second &&& D.nfcFails = 0 then
    ValueNone
  elif
    first >= lBase
    && first < lBase + lCount
    && second >= vBase
    && second < vBase + vCount
  then
    ValueSome(sBase + ((first - lBase) * vCount + (second - vBase)) * tCount)
  elif
    isHangulSyllable first
    && (first - sBase) % tCount = 0
    && second > tBase
    && second < tBase + tCount
  then
    ValueSome(first + second - tBase)
  else
    let i =
      System.Array.BinarySearch(
        compositionPairs.Force(),
        (int64 first <<< 21) ||| int64 second
      )
    if i >= 0 then ValueSome D.compositions[3 * i + 2] else ValueNone

/// ICU refuses U+FFFE, as well as a lone surrogate.
let private isRefused (rune : Rune) : bool = rune.Value = 0xFFFE

/// The codepoints of `s`. A lone surrogate or U+FFFE is an error, as it is for `String.Normalize`.
let private codepoints (s : string) : int[] =
  let rec go (i : int) (acc : List<int>) =
    if i >= s.Length then
      acc.ToArray()
    else
      match Rune.DecodeFromUtf16(System.MemoryExtensions.AsSpan(s, i)) with
      | System.Buffers.OperationStatus.Done, rune, used when not (isRefused rune) ->
        acc.Add rune.Value
        go (i + used) acc
      | _ ->
        raise (
          System.ArgumentException(
            "String contains invalid Unicode code points.",
            "strInput"
          )
        )
  go 0 (List(s.Length))

/// Decomposes every codepoint, putting each run of combining marks in canonical order as it
/// goes: a mark moves back past the marks of a higher class before it. Marks of one class keep
/// their order.
let private decomposeAll (compatibility : bool) (cps : int[]) : List<int> =
  let out = List<int>(cps.Length)
  let insert (cp : int) =
    let ccc = combiningClass cp
    let rec position (i : int) =
      if ccc <> 0 && i > 0 && combiningClass out[i - 1] > ccc then
        position (i - 1)
      else
        i
    out.Insert(position out.Count, cp)
  cps |> Array.iter (decompose compatibility insert)
  out

/// Canonical composition (UAX #15 section 3.11): each mark joins the last starter before it,
/// unless something between them blocks it, a starter or a mark of the same class or higher.
let private composeAll (cps : List<int>) : List<int> =
  let out = List<int>(cps.Count)
  // `starter` is where the last starter is in `out` (-1 for none yet), and `lastClass` the class
  // of the last character kept after it (0 when there is none: the next one is adjacent to it).
  let rec go (i : int) (starter : int) (lastClass : int) =
    if i < cps.Count then
      let cp = cps[i]
      let ccc = combiningClass cp
      let composite =
        if starter >= 0 && (lastClass = 0 || lastClass < ccc) then
          compose out[starter] cp
        else
          ValueNone
      match composite with
      | ValueSome composite ->
        out[starter] <- composite
        go (i + 1) starter lastClass
      | ValueNone ->
        out.Add cp
        if ccc = 0 then go (i + 1) (out.Count - 1) 0 else go (i + 1) starter ccc
  go 0 -1 0
  out

let private toText (cps : List<int>) : string =
  let sb = StringBuilder(cps.Count)
  cps
  |> Seq.iter (fun cp ->
    if cp < 0x10000 then
      sb.Append(char cp) |> ignore
    else
      sb.Append(Rune(cp).ToString()) |> ignore)
  sb.ToString()

/// The first char that could change under `form`: everything below it is a starter that is
/// already in that form and joins nothing.
let private firstCandidate (form : NormalizationForm) : int =
  match form with
  | NormalizationForm.FormC -> 0x300
  | NormalizationForm.FormD -> 0xC0
  | _ -> 0xA0

let private failsFlag (form : NormalizationForm) : int =
  match form with
  | NormalizationForm.FormC -> D.nfcFails
  | NormalizationForm.FormD -> D.nfdFails
  | NormalizationForm.FormKC -> D.nfkcFails
  | _ -> D.nfkdFails

/// The quick check from `start`. None when `s` is already in the form; otherwise the index to
/// normalize from, the start of the last starter that passed, since nothing before it can change.
let private quickCheck
  (form : NormalizationForm)
  (s : string)
  (start : int)
  : Option<int> =
  let fails = failsFlag form
  let rec go (i : int) (lastClass : int) (safe : int) =
    if i >= s.Length then
      None
    else
      match Rune.DecodeFromUtf16(System.MemoryExtensions.AsSpan(s, i)) with
      | System.Buffers.OperationStatus.Done, rune, used when not (isRefused rune) ->
        let bits = properties rune.Value
        let ccc = bits &&& 0xFF
        if bits &&& fails <> 0 || (ccc <> 0 && lastClass > ccc) then
          Some safe
        else
          go (i + used) ccc (if ccc = 0 then i else safe)
      | _ -> Some safe
  // The char before `start` is below `firstCandidate`, so it is a safe place to begin.
  go start 0 (max 0 (start - 1))

/// `s` in the given normalization form, as `s.Normalize(form)` gives with ICU.
let normalizeTo (form : NormalizationForm) (s : string) : string =
  let start =
    System.MemoryExtensions.IndexOfAnyExceptInRange(
      System.MemoryExtensions.AsSpan s,
      char 0,
      char (firstCandidate form - 1)
    )
  if start < 0 then
    s
  else
    match quickCheck form s start with
    | None -> s
    | Some from ->
      let compatibility =
        form = NormalizationForm.FormKC || form = NormalizationForm.FormKD
      let composing =
        form = NormalizationForm.FormC || form = NormalizationForm.FormKC
      let decomposed = decomposeAll compatibility (codepoints (s.Substring from))
      let normalized = if composing then composeAll decomposed else decomposed
      s.Substring(0, from) + toText normalized

/// NFC, the form Dark keeps every string in.
let normalize (s : string) : string = normalizeTo NormalizationForm.FormC s

/// True when a codepoint starts at `i` that is a starter no composition can absorb: NFC never
/// changes anything across it, so the NFC of `s` is the NFC of the text before it followed by
/// the NFC of the text from it.
let private isNfcBoundary (s : string) (i : int) : bool =
  int s[i] < 0x300
  || match Rune.DecodeFromUtf16(System.MemoryExtensions.AsSpan(s, i)) with
     | System.Buffers.OperationStatus.Done, rune, _ ->
       properties rune.Value &&& (0xFF ||| D.nfcFails) = 0
     | _ -> false

/// `s`, made of `count` pieces, `piece 0` to `piece (count - 1)`, each already NFC, as every Dark
/// string is: in NFC. Text can change only around a join, between the last boundary before it
/// and the first one after it, so only that is normalized. The cost then follows the pieces, not
/// the whole result, which keeps building a string piece by piece linear.
let private normalizeJoins
  (s : string)
  (count : int)
  (piece : int -> string)
  : string =
  let rec boundaryBefore (i : int) =
    if i <= 0 || isNfcBoundary s i then max i 0 else boundaryBefore (i - 1)
  let rec boundaryAfter (i : int) =
    if i >= s.Length || isNfcBoundary s i then i else boundaryAfter (i + 1)
  // The windows around each join, merged where they overlap, last first.
  let rec windows (k : int) (join : int) (acc : list<int * int>) =
    if k >= count - 1 then
      acc
    else
      let join = join + piece(k).Length
      if join <= 0 || join >= s.Length || isNfcBoundary s join then
        windows (k + 1) join acc
      else
        let start, stop = boundaryBefore join, boundaryAfter join
        match acc with
        | (prevStart, prevStop) :: rest when start < prevStop ->
          windows (k + 1) join ((prevStart, max prevStop stop) :: rest)
        | _ -> windows (k + 1) join ((start, stop) :: acc)
  match windows 0 0 [] with
  | [] -> s
  | lastFirst ->
    let normalized =
      lastFirst
      |> List.rev
      |> List.map (fun (start, stop) ->
        let window = s.Substring(start, stop - start)
        // text holding a lone surrogate or U+FFFE stays as it came, as it did coming in
        let n =
          try
            normalize window
          with :? System.ArgumentException ->
            window
        start, stop, window, n)
    if
      normalized
      |> List.forall (fun (_, _, window, n) -> obj.ReferenceEquals(window, n))
    then
      s
    else
      let sb = StringBuilder(s.Length)
      let copied =
        normalized
        |> List.fold
          (fun from (start, stop, _, n) ->
            sb.Append(s, from, start - from).Append(n : string) |> ignore
            stop)
          0
      sb.Append(s, copied, s.Length - copied).ToString()

/// The NFC of `pieces` joined, each already NFC.
let concat (pieces : string[]) : string =
  normalizeJoins (System.String.Concat pieces) pieces.Length (fun i -> pieces[i])

/// The NFC of `parts` joined with `separator` between each, all already NFC.
let join (separator : string) (parts : string[]) : string =
  normalizeJoins
    (System.String.Join(separator, parts))
    (2 * parts.Length - 1)
    (fun i -> if i % 2 = 0 then parts[i / 2] else separator)
