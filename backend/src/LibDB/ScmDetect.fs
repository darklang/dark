/// Conflict DETECTION: which names two divergent op sets both moved since the base they last agreed
/// on, and which side the fold will keep. The one copy of it; merge, rebase and the preview all ask
/// here, and so does Dark (`SCM.Conflicts.detect`, through `scmDetect`).
///
/// A conflict is a RECONCILIATION event: a merge or a rebase folds two op sets against a common BASE,
/// and BOTH sides rebound the same name to DIFFERENT hashes since then. Detection never decides what
/// APPLIES -- the fold does, through `LibExecution.Lww` -- it decides what gets RECORDED, and names
/// the winner by asking the same rule the fold asks.
///
/// Everything here is a pure function of its inputs, and the ids it mints are content-derived, so
/// re-detecting the same divergence yields the same id and recording it again changes nothing.
module LibDB.ScmDetect

open Prelude

/// One side's binding of a name: the hash it points at, and the stamp of the op that bound it.
type Binding =
  {
    owner : string
    modules : string
    name : string
    itemType : string
    hash : string
    originTs : string
    /// Who wrote it, when known; "" for anything read from the local projection.
    author : string
    /// The hash this binding replaced, when the op said.
    previous : Option<string>
  }

/// A name ENDING, as the other side told it.
type Removal =
  { owner : string
    modules : string
    name : string
    originTs : string
    author : string
    previous : Option<string> }

/// A recorded per-name base: the hash two sides last agreed on ("" = the name did not exist).
type NameBase =
  { owner : string; modules : string; name : string; baseHash : string }

/// One competing version of a name. `side` is "local" or "incoming".
type Candidate =
  { side : string
    hash : string
    removed : bool
    text : string
    originTs : string
    author : string }

type Conflict =
  { id : string
    owner : string
    modules : string
    name : string
    itemType : string
    part : string
    kind : string
    candidates : List<Candidate>
    autoResolvedTo : string
    reason : string
    status : string
    resolvedBy : string }


/// The join key for a name across the sides. A name cannot contain "/".
let bindingKey (owner : string) (modules : string) (name : string) : string =
  $"{owner}/{modules}/{name}"

/// The dotted name a person reads.
let fqn (owner : string) (modules : string) (name : string) : string =
  if modules = "" then $"{owner}.{name}" else $"{owner}.{modules}.{name}"

let private sha256Hex (s : string) : string =
  System.Security.Cryptography.SHA256.HashData(System.Text.Encoding.UTF8.GetBytes s)
  |> System.Convert.ToHexString
  |> fun h -> h.ToLowerInvariant()

/// A stable short id: the first 8 hex of sha256 over the location and both candidate hashes, sorted so
/// the id does not depend on which side happened to be local. Recorded ids from before this lived in F#
/// were minted the same way, byte for byte, which is what lets a re-detection dedupe against them.
///
/// NFC-normalised, because the Dark version was: it built the material with `+`, and Dark's string `+`
/// normalises its result. A name stored in another form hashes to the id Dark gave it.
let conflictId
  (owner : string)
  (modules : string)
  (name : string)
  (hashes : List<string>)
  : string =
  let sorted =
    hashes |> List.sortWith (fun a b -> System.String.CompareOrdinal(a, b))
  let material = $"{owner}/{modules}/{name}|" + String.concat "," sorted
  (sha256Hex (String.normalize material)).Substring(0, 8)

/// Does the INCOMING binding beat the LOCAL one? The fold's own rule.
let incomingWins (incoming : Binding) (local : Binding) : bool =
  LibExecution.Lww.incomingWins
    incoming.originTs
    incoming.hash
    local.originTs
    local.hash

/// Why the auto-resolution picked what it did.
let resolutionReason (incoming : Binding) (local : Binding) : string =
  if local.originTs = "" then "no-local-stamp"
  elif incoming.originTs = local.originTs then "tie-break-by-hash"
  else "later-origin-ts"


/// A name conflicts when BOTH sides moved it, and there are two ways to establish that.
///
/// WITH a recorded base, the base decides: incoming != local, incoming != base, local != base. An
/// incoming edit made on top of the local version (its `previous` is the local hash) is never one.
///
/// WITHOUT one, the bindings still say enough: both replaced the SAME predecessor and disagree about
/// what replaced it (`same-name-different-hash`), or both replaced NOTHING (`independent-creation`).
/// `None` also means "no predecessor recorded", so both rules need both sides to say it.
///
/// The answer follows <param incoming>'s order.
let detect
  (localLabel : string)
  (incomingLabel : string)
  (local : List<Binding>)
  (incoming : List<Binding>)
  (bases : List<NameBase>)
  : List<Conflict> =
  // Last entry wins for a key, as a Dict built from entries does.
  let localByKey =
    local
    |> List.fold
      (fun (m : Map<string, Binding>) b ->
        Map.add (bindingKey b.owner b.modules b.name) b m)
      Map.empty
  let baseByKey =
    bases
    |> List.fold
      (fun (m : Map<string, string>) b ->
        Map.add (bindingKey b.owner b.modules b.name) b.baseHash m)
      Map.empty

  incoming
  |> List.choose (fun inc ->
    let k = bindingKey inc.owner inc.modules inc.name
    let bothCreatedFromNothing = inc.previous = None && inc.hash <> ""

    let bothMovedTheSameParent (loc : Binding) : bool =
      match inc.previous, loc.previous with
      | Some incPrev, Some locPrev -> incPrev = locPrev && inc.hash <> loc.hash
      | _ -> false

    let descendsFromLocal (loc : Binding) : bool =
      match inc.previous with
      | Some incPrev -> incPrev = loc.hash
      | None -> false

    let conflictWith (loc : Binding) (kind : string) : Conflict =
      { id = conflictId inc.owner inc.modules inc.name [ loc.hash; inc.hash ]
        owner = inc.owner
        modules = inc.modules
        name = inc.name
        itemType = inc.itemType
        part = ""
        kind = kind
        candidates =
          [ { side = "local"
              hash = loc.hash
              removed = false
              text = ""
              originTs = loc.originTs
              author = localLabel }
            { side = "incoming"
              hash = inc.hash
              removed = false
              text = ""
              originTs = inc.originTs
              author = if inc.author = "" then incomingLabel else inc.author } ]
        autoResolvedTo = if incomingWins inc loc then inc.hash else loc.hash
        reason = resolutionReason inc loc
        status = "pending"
        resolvedBy = "" }

    match Map.tryFind k localByKey, Map.tryFind k baseByKey with
    | Some loc, None when
      (bothCreatedFromNothing && loc.previous = None && inc.hash <> loc.hash)
      || bothMovedTheSameParent loc
      ->
      let kind =
        if bothMovedTheSameParent loc then
          "same-name-different-hash"
        else
          "independent-creation"
      Some(conflictWith loc kind)
    | Some loc, Some baseHash ->
      if descendsFromLocal loc then
        None
      elif
        (inc.hash = loc.hash || inc.hash = baseHash || loc.hash = baseHash)
        && not (bothMovedTheSameParent loc)
      then
        None
      else
        Some(conflictWith loc "same-name-different-hash")
    | _ -> None)


/// A removal that arrives for a name you have SINCE edited. A removal of exactly what you hold
/// records nothing. A binding beats a removal on an exact stamp tie (`Lww.unbindBeatsBinding`).
let detectRemovals
  (localLabel : string)
  (incomingLabel : string)
  (local : List<Binding>)
  (removals : List<Removal>)
  : List<Conflict> =
  let localByKey =
    local
    |> List.fold
      (fun (m : Map<string, Binding>) b ->
        Map.add (bindingKey b.owner b.modules b.name) b m)
      Map.empty

  removals
  |> List.choose (fun rem ->
    match Map.tryFind (bindingKey rem.owner rem.modules rem.name) localByKey with
    | Some loc when rem.previous <> Some loc.hash ->
      let removalWins =
        LibExecution.Lww.unbindBeatsBinding rem.originTs loc.originTs
      Some
        { id = conflictId loc.owner loc.modules loc.name [ loc.hash; "" ]
          owner = loc.owner
          modules = loc.modules
          name = loc.name
          itemType = loc.itemType
          part = ""
          kind = "removed-vs-edited"
          candidates =
            [ { side = "local"
                hash = loc.hash
                removed = false
                text = ""
                originTs = loc.originTs
                author = localLabel }
              { side = "incoming"
                hash = ""
                removed = true
                text = ""
                originTs = rem.originTs
                author = if rem.author = "" then incomingLabel else rem.author } ]
          autoResolvedTo = if removalWins then "" else loc.hash
          reason = if removalWins then "removal is newer" else "your edit is newer"
          status = "pending"
          resolvedBy = "" }
    | _ -> None)


/// The other direction of <fn detectRemovals>: the LOCAL side ended a name the INCOMING side has since
/// edited. Only with a recorded base, and only for a name the local side held at that base: then both
/// moved it, one to nothing and one to a new version. Whichever is newer wins, as the fold has it
/// (`Lww.unbindBeatsBinding`), and the other is recorded.
let detectEndedLocally
  (localLabel : string)
  (incomingLabel : string)
  (ended : List<Removal>)
  (incoming : List<Binding>)
  (bases : List<NameBase>)
  : List<Conflict> =
  let endedByKey =
    ended
    |> List.fold
      (fun (m : Map<string, Removal>) r ->
        Map.add (bindingKey r.owner r.modules r.name) r m)
      Map.empty
  let baseByKey =
    bases
    |> List.fold
      (fun (m : Map<string, string>) b ->
        Map.add (bindingKey b.owner b.modules b.name) b.baseHash m)
      Map.empty

  incoming
  |> List.choose (fun inc ->
    let k = bindingKey inc.owner inc.modules inc.name
    match Map.tryFind k endedByKey, Map.tryFind k baseByKey with
    | Some rem, Some baseHash when baseHash <> "" && inc.hash <> baseHash ->
      let removalWins =
        LibExecution.Lww.unbindBeatsBinding rem.originTs inc.originTs
      Some
        { id = conflictId inc.owner inc.modules inc.name [ inc.hash; "" ]
          owner = inc.owner
          modules = inc.modules
          name = inc.name
          itemType = inc.itemType
          part = ""
          kind = "removed-vs-edited"
          candidates =
            [ { side = "local"
                hash = ""
                removed = true
                text = ""
                originTs = rem.originTs
                author = if rem.author = "" then localLabel else rem.author }
              { side = "incoming"
                hash = inc.hash
                removed = false
                text = ""
                originTs = inc.originTs
                author = if inc.author = "" then incomingLabel else inc.author } ]
          autoResolvedTo = if removalWins then "" else inc.hash
          reason = if removalWins then "removal is newer" else "the edit is newer"
          status = "pending"
          resolvedBy = "" }
    | _ -> None)

/// Candidates as the `conflicts.candidates` column holds them: the JSON `Stdlib.Json.serialize`
/// writes for a `List<SCM.Conflicts.Candidate>` -- compact, fields in name order, relaxed escaping --
/// so a row written here reads exactly like one Dark wrote.
let candidatesJson (cs : List<Candidate>) : string =
  let options =
    System.Text.Json.JsonWriterOptions(
      Indented = false,
      SkipValidation = true,
      Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )
  use stream = new System.IO.MemoryStream()
  (use w = new System.Text.Json.Utf8JsonWriter(stream, options)
   w.WriteStartArray()
   for c in cs do
     w.WriteStartObject()
     w.WriteString("author", c.author)
     w.WriteString("hash", c.hash)
     w.WriteString("originTs", c.originTs)
     w.WriteBoolean("removed", c.removed)
     w.WriteString("side", c.side)
     w.WriteString("text", c.text)
     w.WriteEndObject()
   w.WriteEndArray())
  System.Text.Encoding.UTF8.GetString(stream.ToArray())

/// The stored candidates, or none when they do not parse, as the Dark reader has always taken them.
let parseCandidates (json : string) : List<Candidate> =
  try
    use doc = System.Text.Json.JsonDocument.Parse json
    if doc.RootElement.ValueKind <> System.Text.Json.JsonValueKind.Array then
      []
    else
      let read (e : System.Text.Json.JsonElement) : Option<Candidate> =
        let str (n : string) =
          match e.TryGetProperty n with
          | true, v when v.ValueKind = System.Text.Json.JsonValueKind.String ->
            Some(v.GetString())
          | _ -> None
        let boolean (n : string) =
          match e.TryGetProperty n with
          | true, v when v.ValueKind = System.Text.Json.JsonValueKind.True ->
            Some true
          | true, v when v.ValueKind = System.Text.Json.JsonValueKind.False ->
            Some false
          | _ -> None
        match
          str "side",
          str "hash",
          boolean "removed",
          str "text",
          str "originTs",
          str "author"
        with
        | Some side, Some hash, Some removed, Some text, Some ts, Some author ->
          Some
            { side = side
              hash = hash
              removed = removed
              text = text
              originTs = ts
              author = author }
        | _ -> None
      let parsed = doc.RootElement.EnumerateArray() |> Seq.map read |> List.ofSeq
      if List.forall Option.isSome parsed then List.choose identity parsed else []
  with _ ->
    []
