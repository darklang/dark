/// Which platforms are switched on for this instance.
///
/// Deliberately NOT part of `PolicyStore`, and the distinction is the point. A policy answers
/// "what may this code touch"; activation answers "what is even here". They meet in the error
/// message and nowhere else. Keeping them apart means turning a platform off is not a permission
/// grant to be reviewed, and turning one on does not silently widen a policy.
///
/// It lives in the same host-owned directory for the same reason the policy does: package code
/// must not be able to switch its own platform on. It is a separate file so that adding it costs
/// no policy format version, which would have reset every existing approval to deny-all.
module LibDB.Activation

open System
open Prelude

/// One name per line, after a magic-and-version header line. Plain text on purpose: this is a
/// short list a person may reasonably want to read or edit with a text editor, and unlike the
/// policy it carries no structure that a hand edit could corrupt in an interesting way.
let private fileName = "activated"

let private header = "DARK-PLATFORM-ACTIVATION 1"

/// `None` means every platform this build shipped, which is the default and today's behavior.
/// `Some names` means those, plus whatever the runtime treats as always-on.
///
/// The difference between `None` and `Some []` is real: the first is "I have not chosen", the
/// second is "I chose almost nothing". A missing file must not be mistaken for the latter, or a
/// deleted file would silently near-brick the CLI.
type Activation = Option<Set<string>>

/// The names a present file lists. A file that is present is a choice, so this never answers
/// "never chosen"; only the file's absence means that.
///
/// A file whose header we do not recognize reads as the empty set, not as everything. Failing
/// open here would mean a corrupted byte re-enables every platform an instance had switched off,
/// which is the same reason an unparseable policy file reads as deny-all.
let parse (text : string) : Set<string> =
  let lines =
    text.Split('\n')
    |> Array.map (fun line -> line.Trim())
    |> Array.filter (fun line -> line <> "" && not (line.StartsWith "#"))
    |> List.ofArray
  match lines with
  | first :: rest when first = header -> Set.ofList rest
  | _ -> Set.empty

let render (names : Set<string>) : string =
  header + "\n" + (names |> Set.toList |> String.concat "\n") + "\n"

/// Parsed-activation cache keyed on the file's (mtime, ctime, length) stamp, the same shape
/// `PolicyStore` uses and for the same reason: this is read once per guest run, and the usual
/// answer is "the file is not there".
///
/// Worth the ten lines because the miss path is not free: `LocalFile.read` reaches a missing file
/// by opening it and catching `FileNotFoundException`, and a thrown .NET exception captures a
/// stack trace. Asking `stamp` first is a stat. Another process's write changes the stamp and is
/// picked up on the next read, so the cache does not make a stale answer durable.
let private cacheLock = obj ()

let mutable private cache : Option<Option<struct (int64 * int64 * int64)> * Activation> =
  None

let private readFile () : Activation =
  match LocalFile.read fileName with
  | LocalFile.Missing -> None
  | LocalFile.Read bytes ->
    try
      Some(parse (Text.Encoding.UTF8.GetString bytes))
    with _ ->
      Some Set.empty
  | LocalFile.Unreadable _ -> Some Set.empty

/// Read the activation, or `None` when the instance has never chosen.
let get () : Activation =
  let stamp = LocalFile.stamp fileName
  lock cacheLock (fun () ->
    match cache with
    | Some(cached, activation) when cached = stamp -> activation
    | _ ->
      let activation = if stamp = None then None else readFile ()
      cache <- Some(stamp, activation)
      activation)

/// Write, having already taken the cross-process lock.
let private writeUnderLock (activation : Activation) : unit =
  lock cacheLock (fun () -> cache <- None)
  match activation with
  | None ->
    match LocalFile.path fileName with
    | Ok filePath -> (try IO.File.Delete filePath with _ -> ())
    | Error _ -> ()
  | Some names ->
    LocalFile.writeAtomic fileName (Text.Encoding.UTF8.GetBytes(render names))

/// Replace the activation wholesale. `None` clears the choice, restoring "everything".
let set (activation : Activation) : unit =
  LocalFile.withExclusiveLock (fun () -> writeUnderLock activation)

/// One locked read-modify-write, the shape `PolicyStore.update` uses and for the same reason:
/// reading outside the lock and writing inside it means two concurrent
/// `dark platforms activate` calls can each compute from the same starting set and the second
/// write silently drops the first one's platform. The read here deliberately bypasses the cache,
/// since the point is to see what is on disk right now.
let private modify (change : Activation -> Activation) : unit =
  LocalFile.withExclusiveLock (fun () -> writeUnderLock (change (readFile ())))

/// Turn one platform on, from whatever the instance had chosen before.
///
/// Activating from `None` (never chosen, so everything is on) is not a narrowing: it records the
/// full set the caller can see plus this one. The caller passes `shipped` rather than us reading
/// it, because this module is below the platform catalog and should stay there.
let activate (shipped : Set<string>) (name : string) : unit =
  modify (fun current ->
    Some(Set.add name (current |> Option.defaultValue shipped)))

let deactivate (shipped : Set<string>) (name : string) : unit =
  modify (fun current ->
    Some(Set.remove name (current |> Option.defaultValue shipped)))

/// Drop a name from the choice without recording one, for a platform that has gone away.
///
/// Different from `deactivate`, which is a decision: this is tidying up after the thing the choice
/// named stopped existing. An instance that had never chosen still has not chosen, so `None` stays
/// `None` rather than becoming "everything except that", which would silently narrow somebody who
/// merely uninstalled something.
let forget (name : string) : unit =
  modify (fun current -> current |> Option.map (Set.remove name))
