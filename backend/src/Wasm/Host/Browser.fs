/// The seam between the CLI running in the tab and the page around it.
///
/// Output: `Console.Out` is redirected to a writer that hands every chunk to
/// `darkTerm.write` in JS (an xterm.js instance). Input: JS pushes keys in through
/// `PushKey`/`PushPaste`; the browser `stdinReadKey` awaits the next one. Size: JS
/// tells us the terminal's columns and rows whenever they change.
///
/// Everything here is single-threaded by construction (Blazor WebAssembly has one
/// thread), so the queue and the waiter need no locking.
module Darklang.Wasm.Host.Browser

open System
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.JSInterop

let mutable private js : IJSInProcessRuntime option = None

/// Called once from Program.Main with the host's runtime.
let init (runtime : IJSInProcessRuntime) : unit = js <- Some runtime

/// Output is buffered here and drained by the page (`DrainOutput`) on a short timer,
/// never pushed through a synchronous JS call. A synchronous .NET -> JS call made from
/// inside a resumed task continuation left the next await unable to suspend (it fell
/// into a blocking wait, which a browser thread cannot do), and the interpreter prints
/// from exactly there. Draining from JS also batches a whole TUI frame into one write.
let private pending = Text.StringBuilder()

/// While set, output goes here instead of the terminal: the command panel runs a one-shot
/// command beside the workbench and shows its output in its own pane.
let mutable private capture : Text.StringBuilder option = None

/// Queue text for the page's terminal (or the capture, while one is open).
let writeToTerminal (text : string) : unit =
  match capture with
  | Some c -> c.Append text |> ignore<Text.StringBuilder>
  | None -> pending.Append text |> ignore<Text.StringBuilder>

/// Run <param f> with output captured; returns (result, everything it printed).
let captured (f : unit -> Threading.Tasks.Task<'a>) : Threading.Tasks.Task<'a * string> =
  task {
    let c = Text.StringBuilder()
    capture <- Some c
    try
      let! r = f ()
      return (r, c.ToString())
    finally
      capture <- None
  }

/// JS -> .NET: everything written since the last drain.
[<JSInvokable>]
let DrainOutput () : string =
  if pending.Length = 0 then
    ""
  else
    let s = pending.ToString()
    pending.Clear() |> ignore<Text.StringBuilder>
    s

/// Boot diagnostics to the browser console; reaches DevTools even while the main thread
/// is busy. Only for use outside the interpreter (boot, exception reports): see above.
let log (text : string) : unit =
  match js with
  | Some j -> j.InvokeVoid("console.log", "[dark] " + text)
  | None -> ()

/// A TextWriter that feeds the output buffer. Installed as Console.Out and
/// Console.Error; `NonBlockingConsole` writes straight to Console in the browser, so
/// every `print`/`printLine` and every frame the TUI paints lands here.
type TerminalWriter() =
  inherit IO.TextWriter()
  override _.Encoding = Text.Encoding.UTF8
  override _.Write(c : char) = writeToTerminal (string c)
  override _.Write(s : string) = writeToTerminal s
  override _.Write(buffer : char[], index : int, count : int) =
    writeToTerminal (String(buffer, index, count))
  override _.WriteLine(s : string) = writeToTerminal (s + "\n")
  override _.WriteLine() = writeToTerminal "\n"

/// One keyboard event, already in the shape `Console.ReadKey` would report it.
type KeyEvent =
  { key : ConsoleKeyInfo
    /// The whole text of a paste, when this event is one.
    paste : string option }

let private queue = Queue<KeyEvent>()
let mutable private waiter : TaskCompletionSource<KeyEvent> option = None

let private deliver (ev : KeyEvent) : unit =
  match waiter with
  | Some w ->
    waiter <- None
    // Completed synchronously, so the interpreter resumes INSIDE this JS -> .NET call and
    // runs until it next waits for a key. Resuming it from the thread pool instead (a
    // TaskCompletionSource with RunContinuationsAsynchronously) worked for that one turn,
    // and then the next suspension fell into a blocking wait, which is fatal on the browser's
    // single thread. A suspension from an interop call context suspends cleanly.
    w.SetResult ev
  | None -> queue.Enqueue ev

/// The next key event: immediately if one is queued, otherwise when JS pushes one.
let nextKey () : Task<KeyEvent> =
  if queue.Count > 0 then
    Task.FromResult(queue.Dequeue())
  else
    let tcs = TaskCompletionSource<KeyEvent>()
    waiter <- Some tcs
    tcs.Task

let mutable private columns = 80L
let mutable private rows = 24L

let terminalSize () : int64 * int64 = (columns, rows)

/// Map a DOM `KeyboardEvent.key` (plus modifiers) to what `Console.ReadKey` would
/// have returned for the same keystroke on a Unix terminal.
let private toConsoleKeyInfo
  (domKey : string)
  (ctrl : bool)
  (alt : bool)
  (shift : bool)
  : ConsoleKeyInfo =
  let make (ch : char) (key : ConsoleKey) = ConsoleKeyInfo(ch, key, shift, alt, ctrl)
  match domKey with
  | "Enter" -> make '\r' ConsoleKey.Enter
  | "Backspace" -> make '\b' ConsoleKey.Backspace
  | "Tab" -> make '\t' ConsoleKey.Tab
  | "Escape" -> make '\u001b' ConsoleKey.Escape
  | " " -> make ' ' ConsoleKey.Spacebar
  | "ArrowUp" -> make '\u0000' ConsoleKey.UpArrow
  | "ArrowDown" -> make '\u0000' ConsoleKey.DownArrow
  | "ArrowLeft" -> make '\u0000' ConsoleKey.LeftArrow
  | "ArrowRight" -> make '\u0000' ConsoleKey.RightArrow
  | "Home" -> make '\u0000' ConsoleKey.Home
  | "End" -> make '\u0000' ConsoleKey.End
  | "PageUp" -> make '\u0000' ConsoleKey.PageUp
  | "PageDown" -> make '\u0000' ConsoleKey.PageDown
  | "Insert" -> make '\u0000' ConsoleKey.Insert
  | "Delete" -> make '\u0000' ConsoleKey.Delete
  | "F1" -> make '\u0000' ConsoleKey.F1
  | "F2" -> make '\u0000' ConsoleKey.F2
  | "F3" -> make '\u0000' ConsoleKey.F3
  | "F4" -> make '\u0000' ConsoleKey.F4
  | "F5" -> make '\u0000' ConsoleKey.F5
  | "F6" -> make '\u0000' ConsoleKey.F6
  | "F7" -> make '\u0000' ConsoleKey.F7
  | "F8" -> make '\u0000' ConsoleKey.F8
  | "F9" -> make '\u0000' ConsoleKey.F9
  | "F10" -> make '\u0000' ConsoleKey.F10
  | "F11" -> make '\u0000' ConsoleKey.F11
  | "F12" -> make '\u0000' ConsoleKey.F12
  | k when k.Length = 1 ->
    let c = k[0]
    let upper = Char.ToUpperInvariant c
    let key =
      if upper >= 'A' && upper <= 'Z' then
        enum<ConsoleKey> (int upper)
      elif c >= '0' && c <= '9' then
        enum<ConsoleKey> (int ConsoleKey.D0 + int c - int '0')
      else
        ConsoleKey.NoName
    // A terminal reports Ctrl+<letter> as the control character; `Ctrl+C` is '\u0003'.
    let ch =
      if ctrl && upper >= 'A' && upper <= 'Z' then char (int upper - 64) else c
    make ch key
  | _ ->
    // Shift/Ctrl/Alt on their own, media keys, IME: nothing a CLI acts on.
    make '\u0000' ConsoleKey.NoName

/// JS -> .NET: one keydown.
[<JSInvokable>]
let PushKey (domKey : string, ctrl : bool, alt : bool, shift : bool) : unit =
  deliver { key = toConsoleKeyInfo domKey ctrl alt shift; paste = None }

/// JS -> .NET: pasted text. Reported the way the real host reports a paste: the
/// key is the first printable character, the text is the whole paste.
[<JSInvokable>]
let PushPaste (text : string) : unit =
  if not (String.IsNullOrEmpty text) then
    let first =
      text |> Seq.tryFind (fun c -> not (Char.IsControl c)) |> Option.defaultValue ' '
    deliver { key = toConsoleKeyInfo (string first) false false false; paste = Some text }

/// JS -> .NET: the terminal was (re)sized. Also wakes the read loop with a key nobody
/// handles, which is how SIGWINCH gets a repaint on the real host.
[<JSInvokable>]
let SetTerminalSize (cols : int, rws : int) : unit =
  let changed = int64 cols <> columns || int64 rws <> rows
  columns <- int64 (max 1 cols)
  rows <- int64 (max 1 rws)
  // Only a read that is already waiting needs waking; a loop that is busy samples the
  // size on its next pass, and a queued wake-up would be read as a keystroke later.
  if changed && waiter.IsSome then
    deliver
      { key = ConsoleKeyInfo('\u0000', ConsoleKey.NoName, false, false, false)
        paste = None }
