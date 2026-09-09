/// A branch's identity, on its own, below everything that has an opinion about branches.
///
/// Its own file because `RuntimeTypes` and `ProgramTypes` both need it and `RuntimeTypes` compiles first.
module LibExecution.Branching

open Prelude

/// A branch's identity. A type, not a string: distinct from a branch NAME, a content
/// hash and a relay url, which are all strings.
///
/// MIRRORS `Darklang.SCM.Branch.mainBranchId`. Rule on both sides: compare against `Main`, never
/// against a literal "main".
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

  /// Main's branch NAME. What a person types and reads, and nothing below the edge, which resolves it
  /// to `Main` on the way in. A name is never an id, main's included.
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

  member this.IsMain : bool = this = BranchId.Main

  /// The raw uuid, for the boundaries that carry one: Dark (where a branch id is a `Uuid`) and any
  /// serializer that writes a guid rather than text.
  member this.Guid : System.Guid = let (Id g) = this in g
