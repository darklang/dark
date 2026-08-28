/// Measuring, clipping, wrapping and stripping rows that may carry SGR styling. `TextWidth` does the
/// plain text underneath.
///
/// Also the CLI's escape-sanitizing boundary: rows are cleaned on the way out, so a package name
/// can't move the cursor or reset the terminal.
///
/// Native because each of these walks a row a character at a time, and a Dark loop pays a builtin
/// call per character. Layout above the level of one row stays in Dark, measuring whole spans.
module Builtins.Cli.Libs.TerminalText

/// The byte that terminates a CSI sequence.
let inline private isCsiFinal (c : char) = c >= '@' && c <= '~'


/// Skip one escape or control character at `i`, returning only the next index.
///
/// `skipEscape` below also decides whether the sequence is SGR worth keeping, which costs a
/// `Char.IsDigit` for every parameter character. Only a caller that keeps the sequence needs that,
/// and the two hottest callers -- `styledWidth` and `stripSgr` -- throw it away. `styledWidth` runs
/// 289 times a frame on strings that are typically a colour prefix, a short word and a reset, so
/// that is most of a parameter scan per call spent on a question nobody asks.
///
/// The index it returns is the same one `skipEscape` returns; only the inspection is skipped.
let private skipEscapeOnly (text : string) (i : int) : int =
  let len = text.Length

  if i + 1 < len && text[i + 1] = '[' then
    let mutable j = i + 2
    let mutable ended = false
    while j < len && not ended do
      if isCsiFinal text[j] then ended <- true else j <- j + 1
    if ended then j + 1 else len
  else
    i + 1


/// Skip one escape or control character at `i`, returning the next index.
///
/// `keep` receives the START and LENGTH of an SGR sequence worth preserving. Everything else is
/// dropped: a non-SGR CSI whole, a non-CSI escape just its ESC byte, so ordinary text after it stays
/// visible.
///
/// Bounds rather than a string, because two of the four callers throw the sequence away and this
/// runs once per escape: `styledWidth` measures ~289 rows to build one frame and each carries a
/// couple of sequences, so handing it a `Substring` was hundreds of allocations a frame that nothing
/// ever read. The caller that genuinely needs a string builds one; the one appending to a
/// `StringBuilder` no longer needs an intermediate at all.
let private skipEscape (text : string) (i : int) (keep : int -> int -> unit) : int =
  let len = text.Length

  if i + 1 < len && text[i + 1] = '[' then
    let mutable j = i + 2
    let mutable validParams = true
    let mutable ended = false
    while j < len && not ended do
      let c = text[j]
      if isCsiFinal c then
        ended <- true
      else
        if not (System.Char.IsDigit c || c = ';' || c = ':') then
          validParams <- false
        j <- j + 1
    if ended && text[j] = 'm' && validParams then keep i (j - i + 1)
    if ended then j + 1 else len
  else
    i + 1

/// Terminal columns occupied by a row that may carry SGR styling.
///
/// `TextWidth.ofString` only means anything on control-free text; this skips the escapes for you.
let styledWidth (text : string) : int =
  let mutable total = 0
  let mutable i = 0

  while i < text.Length do
    let c = text[i]
    if c = '\u001b' then
      i <- skipEscapeOnly text i
    elif System.Char.IsControl c then
      i <- i + 1
    // A printable ASCII character followed by another ASCII character cannot be part of a longer
    // grapheme cluster, so it is exactly one column and needs no cluster extracted. The only
    // multi-character ASCII cluster is CRLF, and both of its halves are control characters caught
    // above. `TextWidth.ofCluster` already fast-paths the *width* of an ASCII cluster; this skips
    // building the cluster at all, which `GetNextTextElement` does one allocation per character.
    elif
      c >= ' ' && c <= '~' && (i + 1 >= text.Length || text[i + 1] < '\u0080')
    then
      total <- total + 1
      i <- i + 1
    else
      let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
      total <- total + TextWidth.ofCluster cluster
      i <- i + cluster.Length

  total

/// Drop all styling and control sequences, leaving the text a reader would see.
let stripSgr (text : string) : string =
  let out = System.Text.StringBuilder()
  let mutable i = 0

  while i < text.Length do
    if text[i] = '\u001b' then
      i <- skipEscapeOnly text i
    elif System.Char.IsControl text[i] then
      i <- i + 1
    else
      out.Append(text[i]) |> ignore<System.Text.StringBuilder>
      i <- i + 1

  out.ToString()

/// Clip a row to `maxWidth` columns, keeping SGR and dropping everything else.
///
/// SGR costs no columns. Other escapes and control characters go, so dynamic content can't move the
/// cursor, and a double-width cluster is never split in half.
let clipToWidth (text : string) (maxWidth : int) : string =
  if maxWidth <= 0 then
    ""
  else
    let out = System.Text.StringBuilder()
    let append (v : string) = out.Append(v) |> ignore<System.Text.StringBuilder>
    let keepSgr (start : int) (len : int) =
      out.Append(text, start, len) |> ignore<System.Text.StringBuilder>
    let mutable remaining = maxWidth
    let mutable i = 0

    while i < text.Length && remaining > 0 do
      if text[i] = '\u001b' then
        i <- skipEscape text i keepSgr
      elif System.Char.IsControl text[i] then
        i <- i + 1
      else
        let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
        let width = TextWidth.ofCluster cluster
        if width > remaining then
          remaining <- 0
        else
          append cluster
          remaining <- remaining - width
          i <- i + cluster.Length

    out.ToString()

/// Break a row into terminal-width rows at the column, keeping any styling it carries.
///
/// Styling active at a wrap is restated on the next row, since the frame renderer resets after each
/// one. Clusters are never split. Empty input yields one empty row.
let wrapAtColumn (text : string) (maxWidth : int) : string list =
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
      let next =
        skipEscape text i (fun start len -> kept <- text.Substring(start, len))
      // A full reset clears the accumulated styling; anything else retained appends to it.
      if kept = "\u001b[0m" then activeStyle <- ""
      elif kept <> "" then activeStyle <- activeStyle + kept
      current.Append(kept) |> ignore<System.Text.StringBuilder>
      i <- next
    elif System.Char.IsControl text[i] then
      i <- i + 1
    else
      let c = text[i]
      // A printable ASCII character followed by another ASCII character is one column and one
      // character, and cannot combine into a longer cluster -- so nothing needs extracting. The same
      // rule `styledWidth` and `TextWidth.ofString` use, and for the same reason:
      // `GetNextTextElement` allocates a string per character, and the text being wrapped here is
      // descriptions and help text, which is nearly all ASCII.
      let plain =
        c >= ' ' && c <= '~' && (i + 1 >= text.Length || text[i + 1] < '\u0080')
      let cluster =
        if plain then
          ""
        else
          System.Globalization.StringInfo.GetNextTextElement(text, i)
      let charWidth = if plain then 1 else TextWidth.ofCluster cluster
      let shouldWrap =
        wrapPending || (currentWidth > 0 && currentWidth + charWidth > width)

      if shouldWrap then
        completed.Add(current.ToString())
        current.Clear() |> ignore<System.Text.StringBuilder>
        current.Append(activeStyle) |> ignore<System.Text.StringBuilder>

      if plain then
        current.Append(c) |> ignore<System.Text.StringBuilder>
      else
        current.Append(cluster) |> ignore<System.Text.StringBuilder>

      currentWidth <- if shouldWrap then charWidth else currentWidth + charWidth
      wrapPending <- currentWidth >= width
      i <- i + (if plain then 1 else cluster.Length)

  completed.Add(current.ToString())
  List.ofSeq completed

/// Zero-based cursor position immediately after plain text.
///
/// A cursor exactly past the final column is reported at column zero of the next row, matching what
/// the terminal does. Plain text only: callers pass an already-stripped prompt prefix.
let positionAfter (text : string) (maxWidth : int) : int * int =
  let width = max 1 maxWidth
  let mutable row = 0
  let mutable column = 0
  let mutable wrapPending = false
  let mutable i = 0

  while i < text.Length do
    let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
    let charWidth = TextWidth.ofCluster cluster
    let startRow = if wrapPending then row + 1 else row
    let startColumn = if wrapPending then 0 else column
    let shouldWrap = startColumn > 0 && startColumn + charWidth > width
    row <- if shouldWrap then startRow + 1 else startRow
    column <- if shouldWrap then charWidth else startColumn + charWidth
    wrapPending <- column >= width
    i <- i + cluster.Length

  if wrapPending then (row + 1, 0) else (row, column)
