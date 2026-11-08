/// Measuring, clipping, wrapping and stripping rows that may carry SGR styling. `TextWidth` does the
/// plain text underneath.
///
/// Also the CLI's escape-sanitizing boundary: rows are cleaned on the way out, so a package name
/// can't move the cursor or reset the terminal.
///
/// Native because each of these walks a row a character at a time, and a Dark loop pays a builtin
/// call per character. Layout above the level of one row stays in Dark, measuring whole spans.
///
/// Each walk is a module-level tail-recursive function over its index and state. The compiler turns
/// a self tail call into a jump, so it runs as the same loop a `mutable` version would, with no
/// closure. Output rows are still built in a StringBuilder, one cluster at a time.
module Builtins.Cli.Libs.TerminalText

open System.Text

/// The byte that terminates a CSI sequence.
let inline private isCsiFinal (c : char) = c >= '@' && c <= '~'

/// A printable ASCII character followed by another ASCII character cannot be part of a longer
/// grapheme cluster, so it is exactly one column and needs no cluster extracted. The only
/// multi-character ASCII cluster is CRLF, and both of its halves are control characters, which
/// callers catch first. `TextWidth.ofCluster` already fast-paths the *width* of an ASCII cluster;
/// this skips building the cluster at all, which `GetNextTextElement` does one allocation per
/// character.
let inline private isPlainAscii (text : string) (i : int) =
  let c = text[i]
  c >= ' ' && c <= '~' && (i + 1 >= text.Length || text[i + 1] < '\u0080')


/// The index just past the CSI final byte at or after `j`, or the end of the text if there is none.
let rec private csiEnd (text : string) (j : int) : int =
  if j >= text.Length then text.Length
  elif isCsiFinal text[j] then j + 1
  else csiEnd text (j + 1)

/// Skip one escape or control character at `i`. Like `skipEscape` but without deciding whether the
/// sequence is SGR: `styledWidth` and `stripSgr` discard it, so the per-character IsDigit scan is
/// wasted there. Returns the same index.
let private skipEscapeOnly (text : string) (i : int) : int =
  if i + 1 < text.Length && text[i + 1] = '[' then csiEnd text (i + 2) else i + 1


/// `csiEnd`, also saying whether the sequence is SGR: only digits, `;` and `:` before a final `m`.
let rec private sgrCsiEnd
  (text : string)
  (j : int)
  (validParams : bool)
  : struct (int * bool) =
  if j >= text.Length then
    struct (text.Length, false)
  else
    let c = text[j]
    if isCsiFinal c then
      struct (j + 1, validParams && c = 'm')
    else
      sgrCsiEnd
        text
        (j + 1)
        (validParams && (System.Char.IsDigit c || c = ';' || c = ':'))

/// Skip one escape or control character at `i`, returning the next index and whether the skipped
/// span is an SGR sequence worth preserving.
///
/// Everything else is dropped: a non-SGR CSI whole, a non-CSI escape just its ESC byte, so ordinary
/// text after it stays visible. A caller that keeps the SGR reads it as `text[i .. next - 1]`.
let private skipEscape (text : string) (i : int) : struct (int * bool) =
  if i + 1 < text.Length && text[i + 1] = '[' then
    sgrCsiEnd text (i + 2) true
  else
    struct (i + 1, false)


let rec private widthFrom (text : string) (i : int) (total : int) : int =
  if i >= text.Length then
    total
  elif text[i] = '\u001b' then
    widthFrom text (skipEscapeOnly text i) total
  elif System.Char.IsControl text[i] then
    widthFrom text (i + 1) total
  elif isPlainAscii text i then
    widthFrom text (i + 1) (total + 1)
  else
    let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
    widthFrom text (i + cluster.Length) (total + TextWidth.ofCluster cluster)

/// Terminal columns occupied by a row that may carry SGR styling.
///
/// `TextWidth.ofString` only means anything on control-free text; this skips the escapes for you.
let styledWidth (text : string) : int = widthFrom text 0 0


let rec private appendVisible
  (text : string)
  (out : StringBuilder)
  (i : int)
  : unit =
  if i < text.Length then
    if text[i] = '\u001b' then
      appendVisible text out (skipEscapeOnly text i)
    elif System.Char.IsControl text[i] then
      appendVisible text out (i + 1)
    else
      out.Append(text[i]) |> ignore<StringBuilder>
      appendVisible text out (i + 1)

/// Drop all styling and control sequences, leaving the text a reader would see.
let stripSgr (text : string) : string =
  let out = StringBuilder()
  appendVisible text out 0
  out.ToString()


let rec private appendClipped
  (text : string)
  (out : StringBuilder)
  (i : int)
  (remaining : int)
  : unit =
  if i < text.Length && remaining > 0 then
    if text[i] = '\u001b' then
      let struct (next, sgr) = skipEscape text i
      if sgr then out.Append(text, i, next - i) |> ignore<StringBuilder>
      appendClipped text out next remaining
    elif System.Char.IsControl text[i] then
      appendClipped text out (i + 1) remaining
    else
      let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
      let width = TextWidth.ofCluster cluster
      // A cluster that doesn't fit ends the row: a double-width one is never split in half.
      if width <= remaining then
        out.Append(cluster) |> ignore<StringBuilder>
        appendClipped text out (i + cluster.Length) (remaining - width)

/// Clip a row to `maxWidth` columns, keeping SGR and dropping everything else.
///
/// SGR costs no columns. Other escapes and control characters go, so dynamic content can't move the
/// cursor, and a double-width cluster is never split in half.
let clipToWidth (text : string) (maxWidth : int) : string =
  if maxWidth <= 0 then
    ""
  else
    let out = StringBuilder()
    appendClipped text out 0 maxWidth
    out.ToString()


/// One step of `wrapAtColumn`. `current` is the row being built, `completed` the rows before it,
/// newest first. `lastSpaceEnd` is where the row could break at a word boundary: the builder index
/// just past the last space, with `widthAtSpaceEnd` the display width up to and including it, and -1
/// when the row has no space yet.
let rec private wrapFrom
  (text : string)
  (width : int)
  (current : StringBuilder)
  (completed : string list)
  (activeStyle : string)
  (currentWidth : int)
  (wrapPending : bool)
  (lastSpaceEnd : int)
  (widthAtSpaceEnd : int)
  (i : int)
  : string list =
  if i >= text.Length then
    List.rev (current.ToString() :: completed)
  elif text[i] = '\u001b' then
    let struct (next, sgr) = skipEscape text i
    let activeStyle =
      if sgr then
        let kept = text.Substring(i, next - i)
        current.Append(kept) |> ignore<StringBuilder>
        // A full reset clears the accumulated styling; anything else retained appends to it.
        if kept = "\u001b[0m" then "" else activeStyle + kept
      else
        activeStyle
    wrapFrom
      text
      width
      current
      completed
      activeStyle
      currentWidth
      wrapPending
      lastSpaceEnd
      widthAtSpaceEnd
      next
  elif System.Char.IsControl text[i] then
    wrapFrom
      text
      width
      current
      completed
      activeStyle
      currentWidth
      wrapPending
      lastSpaceEnd
      widthAtSpaceEnd
      (i + 1)
  else
    let plain = isPlainAscii text i
    let cluster =
      if plain then
        ""
      else
        System.Globalization.StringInfo.GetNextTextElement(text, i)
    let charWidth = if plain then 1 else TextWidth.ofCluster cluster
    let shouldWrap =
      wrapPending || (currentWidth > 0 && currentWidth + charWidth > width)

    let struct (completed, currentWidth, lastSpaceEnd, widthAtSpaceEnd) =
      if not shouldWrap then
        struct (completed, currentWidth, lastSpaceEnd, widthAtSpaceEnd)
      // Break at the row's last space when one exists (and it is not the very end, which would
      // carry nothing): the head keeps everything before the space, the tail moves down with the
      // active styling restated. A row with no space -- one word wider than the pane -- still
      // breaks at the column.
      elif lastSpaceEnd > activeStyle.Length && lastSpaceEnd < current.Length then
        let whole = current.ToString()
        let head = whole.Substring(0, lastSpaceEnd - 1) // drop the break space itself
        let tail = whole.Substring(lastSpaceEnd)
        current.Clear().Append(activeStyle).Append(tail) |> ignore<StringBuilder>
        struct (head :: completed, currentWidth - widthAtSpaceEnd, -1, 0)
      else
        let row = current.ToString()
        current.Clear().Append(activeStyle) |> ignore<StringBuilder>
        struct (row :: completed, 0, -1, 0)

    if plain then
      current.Append(text[i]) |> ignore<StringBuilder>
    else
      current.Append(cluster) |> ignore<StringBuilder>

    let currentWidth = currentWidth + charWidth
    let struct (lastSpaceEnd, widthAtSpaceEnd) =
      if plain && text[i] = ' ' then
        struct (current.Length, currentWidth)
      else
        struct (lastSpaceEnd, widthAtSpaceEnd)

    wrapFrom
      text
      width
      current
      completed
      activeStyle
      currentWidth
      (currentWidth >= width)
      lastSpaceEnd
      widthAtSpaceEnd
      (i + (if plain then 1 else cluster.Length))

/// Break a row into terminal-width rows at the column, keeping any styling it carries.
///
/// Styling active at a wrap is restated on the next row, since the frame renderer resets after each
/// one. Clusters are never split, and neither are WORDS when a space offers a break point: the wrap
/// prefers the last space in the row, and falls back to the exact column only for a word wider than
/// the whole row. Empty input yields one empty row.
let wrapAtColumn (text : string) (maxWidth : int) : string list =
  wrapFrom text (max 1 maxWidth) (StringBuilder()) [] "" 0 false -1 0 0


let rec private positionFrom
  (text : string)
  (width : int)
  (row : int)
  (column : int)
  (wrapPending : bool)
  (i : int)
  : int * int =
  if i >= text.Length then
    if wrapPending then (row + 1, 0) else (row, column)
  else
    let cluster = System.Globalization.StringInfo.GetNextTextElement(text, i)
    let charWidth = TextWidth.ofCluster cluster
    let startRow = if wrapPending then row + 1 else row
    let startColumn = if wrapPending then 0 else column
    let shouldWrap = startColumn > 0 && startColumn + charWidth > width
    let column = if shouldWrap then charWidth else startColumn + charWidth
    positionFrom
      text
      width
      (if shouldWrap then startRow + 1 else startRow)
      column
      (column >= width)
      (i + cluster.Length)

/// Zero-based cursor position immediately after plain text.
///
/// A cursor exactly past the final column is reported at column zero of the next row, matching what
/// the terminal does. Plain text only: callers pass an already-stripped prompt prefix.
let positionAfter (text : string) (maxWidth : int) : int * int =
  positionFrom text (max 1 maxWidth) 0 0 false 0
