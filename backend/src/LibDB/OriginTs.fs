/// The authoring clock. ONE definition, deliberately: every `origin_ts` written anywhere in the process
/// comes from here, and NOTHING may write `strftime('now')` inline in SQL instead. This counter runs AHEAD
/// of the wall clock by design, so a second source hands out stamps EARLIER than ops that preceded them,
/// and those ops then lose an LWW they should have won.
///
/// Cross-instance LWW compares these stamps between machines, so a peer's clock is out of our hands. Within
/// one process, though, the ordering should be exact, and it is only exact if there's one source.
module LibDB.OriginTs

let private originTsLock = System.Object()
let mutable private lastOriginTs = System.DateTime.MinValue

/// Advance the clock past a stamp we RECEIVED, so anything authored here afterwards sorts after it. This
/// is what makes the stamp a LOGICAL clock: without it, a peer whose clock runs ahead lands stamps this
/// instance cannot reach, and every later edit of ours to the same name loses the LWW.
///
/// It deliberately does NOT fix a genuinely CONCURRENT edit, where neither side saw the other. Someone has
/// to win that one; the tie-break is deterministic so both instances agree, and the loser is recorded as a
/// conflict rather than lost.
///
/// An unparseable stamp is ignored rather than raising: it arrives from the wire, and a peer sending
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


/// A process-monotonic authoring stamp: millisecond wall clock, but returns `max(nowMs, last+1ms)` so it
/// never repeats within a batch. Same-ms ops would otherwise tie, and the LWW in `applySetName` breaks a
/// tie by content hash, silently reordering local sequential edits.
///
/// The format matches the schema default `strftime('%Y-%m-%dT%H:%M:%fZ')` so it stays lexically comparable
/// against rows written before this existed, and against peers.
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
