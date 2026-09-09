/// The timestamp last-writer-wins staleness rule, in ONE place.
///
/// Distributed instances editing the same name must pick the SAME winner without coordinating. A candidate
/// is stale (loses) when its authoring stamp is older, or -- on an exact tie -- when its content hash is the
/// lower of the two (a portable, instance-independent tiebreak). An identical binding already live is
/// stale too: there is nothing to win, and the fold keeps what it has with its earliest stamp.
///
/// Two places apply it: the op-fold (`PackageOpPlayback.applySetNameFrom`) and divergence detection
/// (`SCM.Conflicts.incomingWins`, in Dark). Keeping the rule here means the F# copy cannot drift;
/// `Tests.Lww` holds the Dark one to it.
///
/// Stamps are `yyyy-MM-ddTHH:mm:ss.fffZ` strings, so lexical `<` is already chronological -- no parsing.
module LibDB.Lww


/// True iff binding (newTs, newHash) loses to the live binding (curTs, curHash) under timestamp-LWW.
let isStale
  (newTs : string)
  (newHash : string)
  (curTs : string)
  (curHash : string)
  : bool =
  newTs < curTs || (newTs = curTs && newHash <= curHash)


/// An Unbind against a live BINDING on an exact stamp tie: the binding wins. SetName/SetName ties
/// break on the hash; a tombstone has no hash to offer, so without a rule the SECOND arrival won
/// and two stores that saw the pair in different orders diverged forever. Strict `>` is that rule,
/// used by both sides of the pair in `PackageOpPlayback` (applyUnbind, and the unboundAfter check).
let unbindBeatsBinding (unbindTs : string) (bindingTs : string) : bool =
  unbindTs > bindingTs


/// The same rule from the other side: does the INCOMING binding beat the live one?
///
/// `SCM.Conflicts.incomingWins` asks exactly this in Dark, where recording a conflict has to name the
/// same winner the fold will pick. Expressed here so the two can be tested against each other
/// (`Tests.Lww`) rather than kept in step by hand.
///
/// `curTs = ""` means the live binding carries no stamp, which is a binding that cannot defend itself.
let incomingWins
  (newTs : string)
  (newHash : string)
  (curTs : string)
  (curHash : string)
  : bool =
  if curTs = "" then true else not (isStale newTs newHash curTs curHash)
