/// The authoring clock. ONE definition, deliberately: every `origin_ts` written anywhere in the process
/// comes from here.
///
/// It used to be two. Main authoring called this counter; branch authoring wrote `strftime('now')` inline in
/// SQL, because `Branches.fs` compiles before `Inserts.fs` and couldn't reach the F# one. That's a real bug
/// rather than untidiness: the counter can run AHEAD of the wall clock (that's the point of it), so a branch
/// op authored after a burst of main authoring could take an EARLIER stamp than the ops it followed, and then
/// lose the LWW it should have won. `Branches.fs` already had a comment noting it couldn't use
/// `strftime('now')` in one place for exactly this reason; the other two places still did.
///
/// Cross-instance LWW compares these stamps between machines, so a peer's clock is out of our hands. Within
/// one process, though, the ordering should be exact, and it is only exact if there's one source.
module LibDB.OriginTs

/// A process-monotonic authoring stamp: millisecond wall clock, but returns `max(nowMs, last+1ms)` so it
/// never repeats within a batch. Otherwise same-ms ops would tie and the LWW in `applySetName` would break it
/// by content hash -- silently reordering local sequential edits (rename v1 then v2 could leave v1 winning).
/// Strictly-increasing stamps mean the later edit always wins.
///
/// The format matches the schema default `strftime('%Y-%m-%dT%H:%M:%fZ')` so it stays lexically comparable
/// against rows written before this existed, and against peers.
let private originTsLock = System.Object()
let mutable private lastOriginTs = System.DateTime.MinValue

/// Advance the clock past a stamp we RECEIVED, so anything authored here afterwards sorts after it.
///
/// This is the step that makes the stamp a LOGICAL clock rather than a wall clock with a monotonic tweak,
/// and without it cross-instance LWW has a nasty failure. A peer whose clock runs ahead lands ops with
/// stamps this instance cannot reach, so your LATER edit to the same name loses the comparison and quietly
/// disappears -- not once, but every time, for as long as their clock stays ahead. Observing their stamp
/// means your next edit sorts after theirs, which is simply the truth: you edited after seeing it.
///
/// What it deliberately does NOT do is fix a genuinely CONCURRENT edit, where neither side saw the other.
/// Someone has to win that one, the tie-break is deterministic so both instances agree, and the loser is
/// recorded as a conflict rather than lost. No clock can turn concurrency into an order; it can only stop
/// us from mistaking "their clock is fast" for "they edited later".
///
/// An unparseable stamp is ignored rather than raising. It arrives from the wire, and a peer sending
/// nonsense should not be able to stop us importing.
let observe (stamp : string) : unit =
  match
    System.DateTime.TryParse(
      stamp,
      System.Globalization.CultureInfo.InvariantCulture,
      System.Globalization.DateTimeStyles.AdjustToUniversal
      ||| System.Globalization.DateTimeStyles.AssumeUniversal
    )
  with
  | true, parsed ->
    lock originTsLock (fun () ->
      if parsed > lastOriginTs then lastOriginTs <- parsed)
  | _ -> ()


let next () : string =
  lock originTsLock (fun () ->
    let nowMs =
      let n = System.DateTime.UtcNow
      System.DateTime(
        n.Ticks - (n.Ticks % System.TimeSpan.TicksPerMillisecond),
        System.DateTimeKind.Utc
      )
    let next =
      if nowMs > lastOriginTs then nowMs else lastOriginTs.AddMilliseconds 1.0
    lastOriginTs <- next
    next.ToString(
      "yyyy-MM-ddTHH:mm:ss.fffZ",
      System.Globalization.CultureInfo.InvariantCulture
    ))
