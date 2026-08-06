/// A branch's identity, on its own, below everything that has an opinion about branches.
///
/// Its own file because `RuntimeTypes` and `ProgramTypes` both need it and `RuntimeTypes` compiles first.
module LibExecution.Branching

open Prelude

/// A branch's identity.
///
/// DISTINCT from a branch NAME, from a content hash and from a relay url. All three are strings, and all
/// three have been passed where this belongs; while this was a string too, the compiler could not tell
/// them apart, so the mistakes surfaced as plausible wrong answers rather than as errors.
///
/// MIRRORS `Darklang.SCM.Ids`. The rule both sides keep: an id is compared against `Main`, never against
/// a literal. Twenty-odd literal `"main"` tests were what let the last spelling change break eleven call
/// sites that all read as correct, and one SQL predicate was still selecting on a spelling nothing had
/// written for months.
type BranchId =
  /// Named `Id`, not `BranchId`: a case with the same name as its type shadows the type for QUALIFIED
  /// access, and `PT.BranchId.Main` then fails with "'Main' is not defined". (`ProgramTypes.Hash` is
  /// shaped that way; nothing references its module, which is why nobody has hit it there.)
  | Id of System.Guid

  // Explicit ToString -- F# unions' default override goes through StructuredPrintfImpl
  // reflection, which is broken under AOT trimming. Same reason as `Hash`.
  override this.ToString() = let (Id g) = this in string g

  /// Main's branch id: an ordinary branch id, a uuid like every other. Well-known rather than minted,
  /// because main is the one branch that exists before anything creates it, and it has no `branches` row
  /// to mint an id into.
  static member Main : BranchId =
    Id(System.Guid "00000000-0000-0000-0000-000000000001")

  /// Main's branch NAME. What a person types and reads; resolved to an id at the edge. It used to also
  /// BE the id, which is what made a name and an id interchangeable.
  static member MainName : string = "main"

  /// Parse an id that arrived from outside: SQL text, a wire bundle, a CLI argument, Dark. `None` rather
  /// than an exception, because every one of those can carry something that is not an id at all.
  static member Parse(s : string) : Option<BranchId> =
    match System.Guid.TryParse s with
    | true, g -> Some(Id g)
    | false, _ -> None

  /// Parse where a non-id is a bug rather than bad input: a value this process itself wrote.
  static member ParseUnsafe(s : string) : BranchId =
    match BranchId.Parse s with
    | Some id -> id
    | None -> Exception.raiseInternal $"not a branch id: '{s}'" [ "value", s ]

  /// The optional form the package manager passes around, where `None` means main.
  static member OfOption(branchId : Option<BranchId>) : BranchId =
    branchId |> Option.defaultValue BranchId.Main

  /// The inverse of `OfOption`.
  static member ToOption(branchId : BranchId) : Option<BranchId> =
    if branchId = BranchId.Main then None else Some branchId

  member this.IsMain : bool = this = BranchId.Main

  /// The raw uuid, for the boundaries that carry one: Dark (where a branch id is a `Uuid`) and any
  /// serializer that writes a guid rather than text.
  member this.Guid : System.Guid = let (Id g) = this in g
