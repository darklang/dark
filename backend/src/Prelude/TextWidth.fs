/// Cells occupied by plain text in a fixed-width grid, over extended grapheme clusters.
///
/// A Unicode property of the text, not a terminal capability: the answer is the same wherever it is
/// rendered. Reached from Dark as `Stdlib.String.displayWidth`, and
/// `Builtins.Cli.Libs.TerminalText` layers escape handling on top for rows that carry styling.
module TextWidth

let private isRegionalIndicator (value : int) : bool =
  value >= 0x1F1E6 && value <= 0x1F1FF

/// Cells occupied by one extended grapheme cluster.
let ofCluster (cluster : string) : int =
  // Fast path: a lone printable ASCII character is always one column, and is most of what a frame
  // measures. The table lookup below is comparatively expensive; taking it every time costs
  // hundreds of milliseconds a frame.
  if cluster.Length = 1 && cluster[0] >= ' ' && cluster[0] <= '~' then
    1
  else
    let mutable runes = cluster.EnumerateRunes()
    let mutable widestScalar = 0
    let mutable regionalIndicators = 0
    let mutable hasEmojiVariationSelector = false

    while runes.MoveNext() do
      let rune = runes.Current
      let value = rune.Value

      if isRegionalIndicator value then regionalIndicators <- regionalIndicators + 1

      if value = 0xFE0F then hasEmojiVariationSelector <- true

      let scalarWidth =
        Wcwidth.UnicodeCalculator.GetWidth(
          rune,
          System.Nullable Wcwidth.Unicode.Version_17_0_0
        )
        |> max 0

      widestScalar <- max widestScalar scalarWidth

    if regionalIndicators >= 2 then
      // Individually narrow, but a flag pair is one wide glyph.
      2
    elif hasEmojiVariationSelector && widestScalar = 1 then
      // VS16 asks for emoji presentation of an otherwise narrow character, e.g. U+2764.
      2
    else
      widestScalar

/// Cells occupied by plain, single-line text. Strip escapes first; controls measure zero.
///
/// `String.toEgcSeq |> Seq.sumBy ofCluster` allocates a string for every grapheme cluster, which for
/// ASCII text is one per character, to hand `ofCluster` something whose width it answers by
/// comparison. A printable ASCII character followed by another ASCII character is one column and
/// cannot be part of a longer cluster, so it needs no cluster built at all -- the same rule
/// `TerminalText.styledWidth` uses, and for the same reason. The only multi-character ASCII cluster
/// is CRLF, whose first half is not printable, so it still takes the cluster path and measures as it
/// always did.
let ofString (text : string) : int =
  let mutable total = 0
  let mutable i = 0

  while i < text.Length do
    let c = text[i]
    if c >= ' ' && c <= '~' && (i + 1 >= text.Length || text[i + 1] < '\u0080') then
      total <- total + 1
      i <- i + 1
    else
      let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
      total <- total + ofCluster cluster
      i <- i + cluster.Length

  total

/// Whether text contains an ASCII/Unicode control character.
let containsControl (text : string) : bool = text |> Seq.exists System.Char.IsControl
