module Lookup

/// The value at `key`, for any collection with a `TryGetValue(key, &value)` member:
/// `Dictionary`, `ConcurrentDictionary`, `Map` and `ConditionalWeakTable` alike.
///
/// Lets a caller `match` on a lookup without an out-param of its own. `inline`
/// resolves the member at each call site, so a call compiles to the bare
/// `TryGetValue`; unlike `match d.TryGetValue k with | true, v ->` it allocates
/// nothing.
let inline tryFindV (key : 'k) (collection : ^c) : 'v voption =
  let mutable value = Unchecked.defaultof<'v>
  if
    (^c : (member TryGetValue : 'k * byref<'v> -> bool) (collection, key, &value))
  then
    ValueSome value
  else
    ValueNone
