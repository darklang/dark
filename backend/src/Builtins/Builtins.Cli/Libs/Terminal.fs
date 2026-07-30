module Builtins.Cli.Libs.Terminal

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


/// Restores terminal state if the process exits before Dark can clean up.
///
/// Dark supplies the restoration sequence; the host stores it and invokes it
/// from process lifecycle callbacks.
module TerminalRestoreGuard =
  let private gate = obj ()
  let mutable private restoreSequence : string option = None

  let private snapshot () : string option = lock gate (fun () -> restoreSequence)

  /// Store the fallback restoration sequence after pending output completes.
  ///
  /// Waiting prevents a new fallback from overtaking an earlier terminal
  /// restoration.
  let arm (sequence : string) : unit =
    NonBlockingConsole.wait ()
    lock gate (fun () -> restoreSequence <- Some sequence)

  /// Disarm fallback restoration after pending output completes.
  let disarm () : unit =
    NonBlockingConsole.wait ()
    lock gate (fun () -> restoreSequence <- None)

  /// Write the active restoration sequence, if any.
  ///
  /// The injected writer allows this behavior to be tested without exiting.
  let restoreWith (write : string -> unit) : unit =
    match snapshot () with
    | Some sequence -> write sequence
    | None -> ()

  let private restoreToTerminal () : unit =
    try
      restoreWith (fun sequence ->
        System.Console.Out.Write sequence
        System.Console.Out.Flush())
    with _ ->
      // Cleanup must never replace the failure that caused it.
      ()

  do
    System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> restoreToTerminal ())
    System.AppDomain.CurrentDomain.UnhandledException.Add(fun _ ->
      restoreToTerminal ())
    System.Console.CancelKeyPress.Add(fun _ -> restoreToTerminal ())


/// Measure plain text in terminal columns using Unicode width data and
/// extended grapheme clusters.
module DisplayWidth =
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

        if isRegionalIndicator value then
          regionalIndicators <- regionalIndicators + 1

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
  let containsControl (text : string) : bool =
    text |> Seq.exists System.Char.IsControl

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

  /// Clip styled text to `maxWidth` terminal columns, keeping SGR and dropping everything else.
  ///
  /// Same contract as the Dark implementation this replaces: numeric SGR is retained and costs no columns,
  /// every other escape and control character is dropped so dynamic content can't move the cursor or change
  /// modes, and a double-width cluster is never split in half.
  ///
  /// Native because the Dark version needs a width call per character. Fine for one prompt row, far too slow
  /// for a full-screen frame, which is hundreds of clipped spans.
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


/// Report raw terminal facts used by Dark's TUI availability policy.
module TerminalCapabilities =
  let isInputTerminal () : bool = not System.Console.IsInputRedirected

  let isOutputTerminal () : bool = not System.Console.IsOutputRedirected

  let terminalName () : string =
    match System.Environment.GetEnvironmentVariable "TERM" with
    | null -> ""
    | value -> value


/// Read one terminal dimension, preferring an explicit environment override.
let private getDimension
  (envVar : string)
  (consoleFn : unit -> int)
  (fallback : int)
  : int64 =
  try
    match System.Environment.GetEnvironmentVariable envVar with
    | null ->
      let value = consoleFn ()
      if value > 0 then int64 value else int64 fallback
    | envValue ->
      match System.Int32.TryParse envValue with
      | true, value when value > 0 -> int64 value
      | _ -> int64 fallback
  with _ ->
    int64 fallback


/// Read the kernel's current terminal window size without a process spawn.
let private tryUnixTerminalSize () : (int64 * int64) option =
  [ 1; 0; 2 ] |> List.tryPick Posix.Libc.tryTerminalWindowSize


/// Return the current terminal size.
///
/// Prefer the Unix terminal API, then fall back to environment values,
/// `System.Console`, or 80×24.
let terminalSize () : int64 * int64 =
  match tryUnixTerminalSize () with
  | Some size -> size
  | None ->
    let width = getDimension "COLUMNS" (fun () -> System.Console.WindowWidth) 80
    let height = getDimension "LINES" (fun () -> System.Console.WindowHeight) 24
    (width, height)


let fns () : List<BuiltInFn> =
  [ { name = fn "cliTerminalSize" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TTuple(TInt, TInt, [])
      description = "Sample terminal width and height together as (columns, rows)"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let (width, height) = terminalSize ()
          DTuple(Dval.int (bigint width), Dval.int (bigint height), []) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliGetLogDir" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the absolute path to the CLI log directory"
      fn =
        (function
        | _, _, [], [ DUnit ] -> DString(LibConfig.Config.logDir) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalRestoreArm" 0
      typeParams = []
      parameters =
        [ Param.make
            "restoreSequence"
            TString
            "The terminal restoration sequence supplied by the Dark TUI runtime" ]
      returnType = TUnit
      description =
        "Arm emergency terminal restoration for host failure or process termination"
      fn =
        (function
        | _, _, _, [ DString sequence ] ->
          TerminalRestoreGuard.arm sequence
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "cliTerminalRestoreDisarm" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TUnit
      description = "Flush normal terminal cleanup and disarm fallback restoration"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          TerminalRestoreGuard.disarm ()
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "cliTerminalStyledWidth" 0
      typeParams = []
      parameters =
        [ Param.make "text" TString "One logical row, possibly carrying SGR styling" ]
      returnType = TInt
      description = "Terminal columns occupied by text that may carry SGR styling"
      fn =
        (function
        | _, _, _, [ DString text ] ->
          text |> DisplayWidth.styledWidth |> bigint |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalClipToWidth" 0
      typeParams = []
      parameters =
        [ Param.make "text" TString "One logical row, possibly carrying SGR styling"
          Param.make "maxWidth" TInt "Terminal columns to clip to" ]
      returnType = TString
      description =
        "Clip styled text to a column count, keeping SGR and dropping other escapes and control characters"
      fn =
        (function
        | _, vm, _, [ DString text; DInt maxWidth ] ->
          DString(DisplayWidth.clipToWidth text (intToInt32 vm maxWidth)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalInspectText" 0
      typeParams = []
      parameters = [ Param.make "text" TString "One candidate logical terminal row" ]
      returnType = TTuple(TInt, TBool, [])
      description =
        "Return (display width when control-free, contains control characters)"
      fn =
        (function
        | _, _, _, [ DString text ] ->
          DTuple(
            text |> DisplayWidth.ofString |> bigint |> Dval.int,
            text |> DisplayWidth.containsControl |> DBool,
            []
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalSessionInfo" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TTuple(TBool, TBool, [ TString ])
      description = "Return (input is terminal, output is terminal, TERM value)"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          DTuple(
            TerminalCapabilities.isInputTerminal () |> DBool,
            TerminalCapabilities.isOutputTerminal () |> DBool,
            [ TerminalCapabilities.terminalName () |> DString ]
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
