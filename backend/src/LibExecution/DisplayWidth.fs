/// Measure text in terminal columns: Unicode width data over extended grapheme
/// clusters, with ANSI-aware variants for styled text. Pure computation over
/// strings -- nothing here touches a terminal.
///
/// Used from Builtins.Pure (`stringInspectText`, on the hot path of every printed
/// value) and Builtins.Cli (the styled-text terminal builtins). Moving it into
/// either breaks value printing in every host that doesn't register the other --
/// that was the Wasm REPL, once. Keep it where both can reach it.
module LibExecution.DisplayWidth

let private isRegionalIndicator (value : int) : bool =
  value >= 0x1F1E6 && value <= 0x1F1FF

let private clusterWidth (cluster : string) : int =
  // Fast path: a lone printable ASCII character is always one column. The Wcwidth table lookup below is
  // comparatively expensive, and this is the overwhelmingly common case: measuring a full-screen frame the
  // slow way costs hundreds of milliseconds per frame, which is the difference between a TUI that keeps up
  // with your keyboard and one that doesn't.
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
      // Regional indicators are individually narrow but flag pairs occupy one
      // wide terminal glyph.
      2
    elif hasEmojiVariationSelector && widestScalar = 1 then
      // VS16 requests emoji presentation for otherwise narrow characters such
      // as U+2764 HEAVY BLACK HEART.
      2
    else
      widestScalar

/// Return the number of terminal columns occupied by plain, single-line text.
///
/// ANSI control sequences must be removed before calling this function.
/// Control characters have width zero.
let ofString (text : string) : int =
  text |> String.toEgcSeq |> Seq.sumBy clusterWidth

/// Return whether text contains an ASCII/Unicode control character.
let containsControl (text : string) : bool = text |> Seq.exists System.Char.IsControl

/// Skip one escape sequence or control character starting at `i`, returning the next index.
///
/// `keep` receives an SGR sequence that should be preserved. Everything else is dropped: a non-SGR CSI
/// whole, and a non-CSI escape just its ESC byte, so ordinary text after it stays visible.
let private skipEscape (text : string) (i : int) (keep : string -> unit) : int =
  let len = text.Length
  let isFinal (c : char) = c >= '@' && c <= '~'

  if i + 1 < len && text[i + 1] = '[' then
    let mutable j = i + 2
    let mutable validParams = true
    let mutable ended = false
    while j < len && not ended do
      let c = text[j]
      if isFinal c then
        ended <- true
      else
        if not (System.Char.IsDigit c || c = ';' || c = ':') then
          validParams <- false
        j <- j + 1
    if ended && text[j] = 'm' && validParams then
      keep (text.Substring(i, j - i + 1))
    if ended then j + 1 else len
  else
    i + 1

/// Terminal columns occupied by text that may carry SGR styling.
///
/// `ofString` is only meaningful for control-free text; this skips escapes and control characters and sums
/// the rest, so a styled row can be measured in one call.
let styledWidth (text : string) : int =
  let mutable total = 0
  let mutable i = 0

  while i < text.Length do
    if text[i] = '\u001b' then
      i <- skipEscape text i (fun _ -> ())
    elif System.Char.IsControl text[i] then
      i <- i + 1
    else
      let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
      total <- total + clusterWidth cluster
      i <- i + cluster.Length

  total

/// Clip styled text to `maxWidth` terminal columns, keeping ANSI styling and dropping everything else.
///
/// Colour/style escapes (`ESC[...m`) are retained and cost no columns. Every other escape and control
/// character is dropped, so dynamic content can't move the cursor or change modes, and a double-width
/// cluster is never split in half.
///
/// Native because this needs a display-width lookup per character, and a full-screen frame is hundreds
/// of clipped spans.
let clipToWidth (text : string) (maxWidth : int) : string =
  if maxWidth <= 0 then
    ""
  else
    let out = System.Text.StringBuilder()
    let append (v : string) = out.Append(v) |> ignore<System.Text.StringBuilder>
    let mutable remaining = maxWidth
    let mutable i = 0

    while i < text.Length && remaining > 0 do
      if text[i] = '\u001b' then
        i <- skipEscape text i append
      elif System.Char.IsControl text[i] then
        i <- i + 1
      else
        let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
        let width = clusterWidth cluster
        if width > remaining then
          remaining <- 0
        else
          append cluster
          remaining <- remaining - width
          i <- i + cluster.Length

    out.ToString()


/// Wrap text into explicit terminal-width rows, preserving any ANSI styling it carries.
///
/// "SGR" is the ANSI escape that sets colour and style (`ESC[1;31m` and friends). Those are kept and
/// cost zero columns; every other escape and control character is dropped. A grapheme cluster is never
/// split, styling active at a wrap is reapplied at the start of the next row (the frame renderer resets
/// styling after each row), and an empty input yields one empty row.
///
/// Native rather than Dark because it runs on the interactive prompt on *every keystroke*, once per
/// character, and each character needs a display-width lookup.
let wrapStyled (text : string) (maxWidth : int) : string list =
  let width = max 1 maxWidth
  let completed = ResizeArray<string>()
  let current = System.Text.StringBuilder()
  let mutable activeStyle = ""
  let mutable currentWidth = 0
  let mutable wrapPending = false
  let mutable i = 0

  while i < text.Length do
    if text[i] = '\u001b' then
      let mutable kept = ""
      let next = skipEscape text i (fun s -> kept <- s)
      // Mirrors Dark's `nextActiveStyle`: a full reset clears accumulated styling, any other retained
      // SGR appends to it, and a dropped (non-SGR) escape leaves it alone.
      if kept = "\u001b[0m" then activeStyle <- ""
      elif kept <> "" then activeStyle <- activeStyle + kept
      current.Append(kept) |> ignore<System.Text.StringBuilder>
      i <- next
    elif System.Char.IsControl text[i] then
      i <- i + 1
    else
      let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
      let charWidth = clusterWidth cluster
      let shouldWrap =
        wrapPending || (currentWidth > 0 && currentWidth + charWidth > width)

      if shouldWrap then
        completed.Add(current.ToString())
        current.Clear() |> ignore<System.Text.StringBuilder>
        current.Append(activeStyle) |> ignore<System.Text.StringBuilder>

      current.Append(cluster) |> ignore<System.Text.StringBuilder>
      currentWidth <- if shouldWrap then charWidth else currentWidth + charWidth
      wrapPending <- currentWidth >= width
      i <- i + cluster.Length

  completed.Add(current.ToString())
  List.ofSeq completed

/// Zero-based cursor position immediately after plain text.
///
/// A cursor exactly after the final column is reported at column zero of the following row, matching
/// terminal wrap behaviour. Plain text only: callers pass an already-stripped prompt prefix, so there
/// is no escape handling here.
let positionAfter (text : string) (maxWidth : int) : int * int =
  let width = max 1 maxWidth
  let mutable row = 0
  let mutable column = 0
  let mutable wrapPending = false
  let mutable i = 0

  while i < text.Length do
    let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
    let charWidth = clusterWidth cluster
    let startRow = if wrapPending then row + 1 else row
    let startColumn = if wrapPending then 0 else column
    let shouldWrap = startColumn > 0 && startColumn + charWidth > width
    row <- if shouldWrap then startRow + 1 else startRow
    column <- if shouldWrap then charWidth else startColumn + charWidth
    wrapPending <- column >= width
    i <- i + cluster.Length

  if wrapPending then (row + 1, 0) else (row, column)
