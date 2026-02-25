/// Content-addressable hashing for package items.
///
/// Canonical serializers produce deterministic bytes for hashing by skipping
/// identity-irrelevant fields (AST node IDs, description, deprecated, originalName).
/// Re-uses leaf serializers from the existing binary format.
///
/// Includes Tarjan's SCC detection for handling mutually-recursive definitions
/// and batch SCC hashing with FQN-based name substitution.
module rec LibSerialization.Hashing

open System.IO
open System.Security.Cryptography
open Prelude
module PT = LibExecution.ProgramTypes

// Re-use leaf serializers from existing binary serializers
module Common = LibSerialization.Binary.Serializers.Common
module PTC = LibSerialization.Binary.Serializers.PT.Common
module ExprS = LibSerialization.Binary.Serializers.PT.Expr

// Local helpers to avoid F# resolving PT.ContentHash to the type instead of the module
let private contentHashFromSHA256Bytes (bytes : byte array) : PT.ContentHash =
  PT.ContentHash(System.Convert.ToHexString(bytes).ToLowerInvariant())

let private contentHashToHexString (PT.ContentHash h) : string = h


/// Controls how package references are written during hashing.
/// Normal: write resolved UUIDs (standard case)
/// SccNameRef: for intra-SCC refs, write FQN string instead of UUID
type HashRefMode =
  | Normal
  | SccNameRef of Map<uuid, string>


// =====================
// Canonical writers
// =====================

/// Skip originalName, only write resolved value (or error)
let writeCanonicalNameResolution
  (writeValue : BinaryWriter -> 'a -> unit)
  (w : BinaryWriter)
  (nr : PT.NameResolution<'a>)
  =
  // Skip: List.write w String.write nr.originalName
  match nr.resolved with
  | Ok value ->
    w.Write(0uy)
    writeValue w value
  | Error error ->
    w.Write(1uy)
    PTC.NameResolutionError.write w error


/// Write FQTypeName, checking HashRefMode for SCC substitution
let writeCanonicalFQTypeName
  (mode : HashRefMode)
  (w : BinaryWriter)
  (name : PT.FQTypeName.FQTypeName)
  =
  match name with
  | PT.FQTypeName.Package p ->
    match mode with
    | SccNameRef sccMap when Map.containsKey p sccMap ->
      w.Write(1uy) // SCC name-ref tag
      Common.String.write w (Map.findUnsafe p sccMap)
    | _ ->
      w.Write(0uy)
      PTC.FQTypeName.Package.write w p


/// Write FQFnName, checking HashRefMode for SCC substitution
let writeCanonicalFQFnName
  (mode : HashRefMode)
  (w : BinaryWriter)
  (name : PT.FQFnName.FQFnName)
  =
  match name with
  | PT.FQFnName.Builtin b ->
    w.Write(0uy)
    PTC.FQFnName.Builtin.write w b
  | PT.FQFnName.Package p ->
    match mode with
    | SccNameRef sccMap when Map.containsKey p sccMap ->
      w.Write(2uy) // SCC name-ref tag
      Common.String.write w (Map.findUnsafe p sccMap)
    | _ ->
      w.Write(1uy)
      PTC.FQFnName.Package.write w p


/// Write FQValueName, checking HashRefMode for SCC substitution
let writeCanonicalFQValueName
  (mode : HashRefMode)
  (w : BinaryWriter)
  (name : PT.FQValueName.FQValueName)
  =
  match name with
  | PT.FQValueName.Builtin b ->
    w.Write(0uy)
    PTC.FQValueName.Builtin.write w b
  | PT.FQValueName.Package p ->
    match mode with
    | SccNameRef sccMap when Map.containsKey p sccMap ->
      w.Write(2uy) // SCC name-ref tag
      Common.String.write w (Map.findUnsafe p sccMap)
    | _ ->
      w.Write(1uy)
      PTC.FQValueName.Package.write w p


/// Canonical TypeReference: delegates to existing structure but uses
/// canonical FQTypeName for custom types
let writeCanonicalTypeReference
  (mode : HashRefMode)
  (w : BinaryWriter)
  (t : PT.TypeReference)
  : unit =
  match t with
  | PT.TInt64 -> w.Write 0uy
  | PT.TUInt64 -> w.Write 1uy
  | PT.TInt8 -> w.Write 2uy
  | PT.TUInt8 -> w.Write 3uy
  | PT.TInt16 -> w.Write 4uy
  | PT.TUInt16 -> w.Write 5uy
  | PT.TInt32 -> w.Write 6uy
  | PT.TUInt32 -> w.Write 7uy
  | PT.TInt128 -> w.Write 8uy
  | PT.TUInt128 -> w.Write 9uy
  | PT.TFloat -> w.Write 10uy
  | PT.TBool -> w.Write 11uy
  | PT.TUnit -> w.Write 12uy
  | PT.TString -> w.Write 13uy
  | PT.TList inner ->
    w.Write 14uy
    writeCanonicalTypeReference mode w inner
  | PT.TDict inner ->
    w.Write 15uy
    writeCanonicalTypeReference mode w inner
  | PT.TDB inner ->
    w.Write 16uy
    writeCanonicalTypeReference mode w inner
  | PT.TDateTime -> w.Write 17uy
  | PT.TChar -> w.Write 18uy
  | PT.TUuid -> w.Write 19uy
  | PT.TCustomType(typeName, typeArgs) ->
    w.Write 20uy
    writeCanonicalNameResolution (writeCanonicalFQTypeName mode) w typeName
    Common.List.write w (writeCanonicalTypeReference mode) typeArgs
  | PT.TVariable name ->
    w.Write 21uy
    Common.String.write w name
  | PT.TFn(paramTypes, returnType) ->
    w.Write 22uy
    Common.NEList.write (writeCanonicalTypeReference mode) w paramTypes
    writeCanonicalTypeReference mode w returnType
  | PT.TTuple(first, second, rest) ->
    w.Write 23uy
    writeCanonicalTypeReference mode w first
    writeCanonicalTypeReference mode w second
    Common.List.write w (writeCanonicalTypeReference mode) rest


/// Canonical LetPattern: skip uint64 id on each variant
let writeCanonicalLetPattern (w : BinaryWriter) (pattern : PT.LetPattern) =
  match pattern with
  | PT.LPVariable(_id, name) ->
    w.Write 0uy
    Common.String.write w name
  | PT.LPUnit _id -> w.Write 1uy
  | PT.LPTuple(_id, first, second, rest) ->
    w.Write 2uy
    writeCanonicalLetPattern w first
    writeCanonicalLetPattern w second
    Common.List.write w writeCanonicalLetPattern rest


/// Canonical MatchPattern: skip uint64 id on each variant
let writeCanonicalMatchPattern (w : BinaryWriter) (pattern : PT.MatchPattern) =
  match pattern with
  | PT.MPVariable(_id, name) ->
    w.Write 0uy
    Common.String.write w name
  | PT.MPEnum(_id, caseName, fieldPats) ->
    w.Write 1uy
    Common.String.write w caseName
    Common.List.write w writeCanonicalMatchPattern fieldPats
  | PT.MPInt64(_id, value) ->
    w.Write 2uy
    w.Write value
  | PT.MPUInt64(_id, value) ->
    w.Write 3uy
    w.Write value
  | PT.MPInt8(_id, value) ->
    w.Write 4uy
    w.Write value
  | PT.MPUInt8(_id, value) ->
    w.Write 5uy
    w.Write value
  | PT.MPInt16(_id, value) ->
    w.Write 6uy
    w.Write value
  | PT.MPUInt16(_id, value) ->
    w.Write 7uy
    w.Write value
  | PT.MPInt32(_id, value) ->
    w.Write 8uy
    w.Write value
  | PT.MPUInt32(_id, value) ->
    w.Write 9uy
    w.Write value
  | PT.MPInt128(_id, value) ->
    w.Write 10uy
    Common.String.write w (string value)
  | PT.MPUInt128(_id, value) ->
    w.Write 11uy
    Common.String.write w (string value)
  | PT.MPBool(_id, value) ->
    w.Write 12uy
    w.Write value
  | PT.MPChar(_id, value) ->
    w.Write 13uy
    Common.String.write w value
  | PT.MPString(_id, value) ->
    w.Write 14uy
    Common.String.write w value
  | PT.MPFloat(_id, sign, whole, fractional) ->
    w.Write 15uy
    PTC.Sign.write w sign
    Common.String.write w whole
    Common.String.write w fractional
  | PT.MPUnit _id -> w.Write 16uy
  | PT.MPTuple(_id, first, second, rest) ->
    w.Write 17uy
    writeCanonicalMatchPattern w first
    writeCanonicalMatchPattern w second
    Common.List.write w writeCanonicalMatchPattern rest
  | PT.MPList(_id, patterns) ->
    w.Write 18uy
    Common.List.write w writeCanonicalMatchPattern patterns
  | PT.MPListCons(_id, head, tail) ->
    w.Write 19uy
    writeCanonicalMatchPattern w head
    writeCanonicalMatchPattern w tail
  | PT.MPOr(_id, patterns) ->
    w.Write 20uy
    Common.NEList.write writeCanonicalMatchPattern w patterns


/// Canonical StringSegment: delegates to canonical expr
let writeCanonicalStringSegment
  (mode : HashRefMode)
  (w : BinaryWriter)
  (segment : PT.StringSegment)
  =
  match segment with
  | PT.StringText text ->
    w.Write 0uy
    Common.String.write w text
  | PT.StringInterpolation expr ->
    w.Write 1uy
    writeCanonicalExpr mode w expr


/// Canonical MatchCase
let writeCanonicalMatchCase
  (mode : HashRefMode)
  (w : BinaryWriter)
  (case : PT.MatchCase)
  =
  writeCanonicalMatchPattern w case.pat
  Common.Option.write w (writeCanonicalExpr mode) case.whenCondition
  writeCanonicalExpr mode w case.rhs


/// Canonical PipeExpr: skip uint64 id, use canonical NR/expr
let writeCanonicalPipeExpr
  (mode : HashRefMode)
  (w : BinaryWriter)
  (pipeExpr : PT.PipeExpr)
  =
  match pipeExpr with
  | PT.EPipeVariable(_id, name, args) ->
    w.Write 0uy
    Common.String.write w name
    Common.List.write w (writeCanonicalExpr mode) args
  | PT.EPipeLambda(_id, pats, body) ->
    w.Write 1uy
    Common.NEList.write writeCanonicalLetPattern w pats
    writeCanonicalExpr mode w body
  | PT.EPipeInfix(_id, infix, expr) ->
    w.Write 2uy
    ExprS.Infix.write w infix
    writeCanonicalExpr mode w expr
  | PT.EPipeFnCall(_id, fnName, typeArgs, args) ->
    w.Write 3uy
    writeCanonicalNameResolution (writeCanonicalFQFnName mode) w fnName
    Common.List.write w (writeCanonicalTypeReference mode) typeArgs
    Common.List.write w (writeCanonicalExpr mode) args
  | PT.EPipeEnum(_id, typeName, caseName, fields) ->
    w.Write 4uy
    writeCanonicalNameResolution (writeCanonicalFQTypeName mode) w typeName
    Common.String.write w caseName
    Common.List.write w (writeCanonicalExpr mode) fields


/// Canonical Expr: skip uint64 id on all variants, use canonical NR for refs
let writeCanonicalExpr (mode : HashRefMode) (w : BinaryWriter) (expr : PT.Expr) =
  match expr with
  | PT.EInt64(_id, value) ->
    w.Write 0uy
    w.Write value
  | PT.EUInt64(_id, value) ->
    w.Write 1uy
    w.Write value
  | PT.EInt8(_id, value) ->
    w.Write 2uy
    w.Write value
  | PT.EUInt8(_id, value) ->
    w.Write 3uy
    w.Write value
  | PT.EInt16(_id, value) ->
    w.Write 4uy
    w.Write value
  | PT.EUInt16(_id, value) ->
    w.Write 5uy
    w.Write value
  | PT.EInt32(_id, value) ->
    w.Write 6uy
    w.Write value
  | PT.EUInt32(_id, value) ->
    w.Write 7uy
    w.Write value
  | PT.EInt128(_id, value) ->
    w.Write 8uy
    Common.String.write w (string value)
  | PT.EUInt128(_id, value) ->
    w.Write 9uy
    Common.String.write w (string value)
  | PT.EBool(_id, value) ->
    w.Write 10uy
    w.Write value
  | PT.EString(_id, segments) ->
    w.Write 11uy
    Common.List.write w (writeCanonicalStringSegment mode) segments
  | PT.EChar(_id, value) ->
    w.Write 12uy
    Common.String.write w value
  | PT.EFloat(_id, sign, whole, fractional) ->
    w.Write 13uy
    PTC.Sign.write w sign
    Common.String.write w whole
    Common.String.write w fractional
  | PT.EUnit _id -> w.Write 14uy
  | PT.EValue(_id, nameRes) ->
    w.Write 15uy
    writeCanonicalNameResolution (writeCanonicalFQValueName mode) w nameRes
  | PT.ELet(_id, pattern, rhs, body) ->
    w.Write 16uy
    writeCanonicalLetPattern w pattern
    writeCanonicalExpr mode w rhs
    writeCanonicalExpr mode w body
  | PT.EIf(_id, cond, thenExpr, elseExpr) ->
    w.Write 17uy
    writeCanonicalExpr mode w cond
    writeCanonicalExpr mode w thenExpr
    Common.Option.write w (writeCanonicalExpr mode) elseExpr
  | PT.ELambda(_id, pats, body) ->
    w.Write 18uy
    Common.NEList.write writeCanonicalLetPattern w pats
    writeCanonicalExpr mode w body
  | PT.ERecordFieldAccess(_id, expr, field) ->
    w.Write 19uy
    writeCanonicalExpr mode w expr
    Common.String.write w field
  | PT.EVariable(_id, name) ->
    w.Write 20uy
    Common.String.write w name
  | PT.EApply(_id, fn, typeArgs, args) ->
    w.Write 21uy
    writeCanonicalExpr mode w fn
    Common.List.write w (writeCanonicalTypeReference mode) typeArgs
    Common.NEList.write (writeCanonicalExpr mode) w args
  | PT.EList(_id, exprs) ->
    w.Write 22uy
    Common.List.write w (writeCanonicalExpr mode) exprs
  | PT.ERecord(_id, typeName, typeArgs, fields) ->
    w.Write 23uy
    writeCanonicalNameResolution (writeCanonicalFQTypeName mode) w typeName
    Common.List.write w (writeCanonicalTypeReference mode) typeArgs
    Common.List.write
      w
      (fun w (name, expr) ->
        Common.String.write w name
        writeCanonicalExpr mode w expr)
      fields
  | PT.ERecordUpdate(_id, record, updates) ->
    w.Write 24uy
    writeCanonicalExpr mode w record
    Common.NEList.write
      (fun w (name, expr) ->
        Common.String.write w name
        writeCanonicalExpr mode w expr)
      w
      updates
  | PT.EPipe(_id, expr, pipes) ->
    w.Write 25uy
    writeCanonicalExpr mode w expr
    Common.List.write w (writeCanonicalPipeExpr mode) pipes
  | PT.EEnum(_id, typeName, typeArgs, caseName, fields) ->
    w.Write 26uy
    writeCanonicalNameResolution (writeCanonicalFQTypeName mode) w typeName
    Common.List.write w (writeCanonicalTypeReference mode) typeArgs
    Common.String.write w caseName
    Common.List.write w (writeCanonicalExpr mode) fields
  | PT.EMatch(_id, expr, cases) ->
    w.Write 27uy
    writeCanonicalExpr mode w expr
    Common.List.write w (writeCanonicalMatchCase mode) cases
  | PT.ETuple(_id, first, second, rest) ->
    w.Write 28uy
    writeCanonicalExpr mode w first
    writeCanonicalExpr mode w second
    Common.List.write w (writeCanonicalExpr mode) rest
  | PT.EInfix(_id, op, left, right) ->
    w.Write 29uy
    ExprS.Infix.write w op
    writeCanonicalExpr mode w left
    writeCanonicalExpr mode w right
  | PT.EDict(_id, pairs) ->
    w.Write 30uy
    Common.List.write
      w
      (fun w (key, value) ->
        Common.String.write w key
        writeCanonicalExpr mode w value)
      pairs
  | PT.EFnName(_id, nameRes) ->
    w.Write 31uy
    writeCanonicalNameResolution (writeCanonicalFQFnName mode) w nameRes
  | PT.EStatement(_id, first, next) ->
    w.Write 32uy
    writeCanonicalExpr mode w first
    writeCanonicalExpr mode w next
  | PT.ESelf _id -> w.Write 33uy
  | PT.EArg(_id, index) ->
    w.Write 34uy
    w.Write index


// =====================
// Canonical type declaration writers
// =====================

/// Canonical Parameter: name + canonical type ref (skip description)
let writeCanonicalParameter
  (mode : HashRefMode)
  (w : BinaryWriter)
  (p : PT.PackageFn.Parameter)
  =
  Common.String.write w p.name
  writeCanonicalTypeReference mode w p.typ

/// Canonical RecordField: name + canonical type ref (skip description)
let writeCanonicalRecordField
  (mode : HashRefMode)
  (w : BinaryWriter)
  (f : PT.TypeDeclaration.RecordField)
  =
  Common.String.write w f.name
  writeCanonicalTypeReference mode w f.typ

/// Canonical EnumField: canonical type ref + label (skip description)
let writeCanonicalEnumField
  (mode : HashRefMode)
  (w : BinaryWriter)
  (f : PT.TypeDeclaration.EnumField)
  =
  writeCanonicalTypeReference mode w f.typ
  Common.Option.write w Common.String.write f.label

/// Canonical EnumCase: name + canonical fields (skip description)
let writeCanonicalEnumCase
  (mode : HashRefMode)
  (w : BinaryWriter)
  (c : PT.TypeDeclaration.EnumCase)
  =
  Common.String.write w c.name
  Common.List.write w (writeCanonicalEnumField mode) c.fields

/// Canonical TypeDeclaration: typeParams + canonical definition
let writeCanonicalTypeDeclaration
  (mode : HashRefMode)
  (w : BinaryWriter)
  (d : PT.TypeDeclaration.T)
  =
  Common.List.write w Common.String.write d.typeParams
  match d.definition with
  | PT.TypeDeclaration.Alias typeRef ->
    w.Write(0uy)
    writeCanonicalTypeReference mode w typeRef
  | PT.TypeDeclaration.Record fields ->
    w.Write(1uy)
    Common.NEList.write (writeCanonicalRecordField mode) w fields
  | PT.TypeDeclaration.Enum cases ->
    w.Write(2uy)
    Common.NEList.write (writeCanonicalEnumCase mode) w cases


// =====================
// Hash computation helpers
// =====================

/// Serialize to bytes using a writer function, then SHA-256 hash to ContentHash
let private hashWithWriter (writerFn : BinaryWriter -> unit) : PT.ContentHash =
  use ms = new MemoryStream()
  use w = new BinaryWriter(ms)
  writerFn w
  let bytes = ms.ToArray()
  SHA256.HashData(bytes) |> contentHashFromSHA256Bytes


// =====================
// Hash computation functions
// =====================

/// Hash a PackageType (skip id, description, deprecated)
let computeTypeHash
  (mode : HashRefMode)
  (t : PT.PackageType.PackageType)
  : PT.ContentHash =
  hashWithWriter (fun w ->
    w.Write(0uy) // tag: type
    writeCanonicalTypeDeclaration mode w t.declaration)


/// Hash a PackageFn (skip id, description, deprecated, param descriptions)
let computeFnHash
  (mode : HashRefMode)
  (fn : PT.PackageFn.PackageFn)
  : PT.ContentHash =
  hashWithWriter (fun w ->
    w.Write(1uy) // tag: fn
    writeCanonicalExpr mode w fn.body
    Common.List.write w Common.String.write fn.typeParams
    Common.NEList.write (writeCanonicalParameter mode) w fn.parameters
    writeCanonicalTypeReference mode w fn.returnType)


/// Hash a PackageValue (skip id, description, deprecated)
let computeValueHash
  (mode : HashRefMode)
  (v : PT.PackageValue.PackageValue)
  : PT.ContentHash =
  hashWithWriter (fun w ->
    w.Write(2uy) // tag: value
    writeCanonicalExpr mode w v.body)


/// Hash a PackageOp (reuse existing PackageOp.write — ops have no metadata to skip)
let computeOpHash (op : PT.PackageOp) : PT.ContentHash =
  hashWithWriter (fun w ->
    LibSerialization.Binary.Serializers.PT.PackageOp.write w op)


/// Hash a commit: hash(parentHash + sorted(opHashes))
let computeCommitHash
  (parentHash : PT.ContentHash option)
  (opHashes : List<PT.ContentHash>)
  : PT.ContentHash =
  hashWithWriter (fun w ->
    Common.Option.write w PTC.ContentHash.write parentHash
    let sorted = opHashes |> List.map contentHashToHexString |> List.sort
    Common.List.write w Common.String.write sorted)


// =====================
// Tarjan's SCC algorithm
// =====================

/// Find all strongly-connected components in a dependency graph.
/// Returns list of SCCs, each an NEList of node IDs.
let findSCCs
  (nodes : List<'id>)
  (edges : 'id -> List<'id>)
  : List<NEList<'id>> when 'id : equality and 'id : comparison =

  let mutable index = 0
  let mutable stack : List<'id> = []
  let mutable indices = Map.empty<'id, int>
  let mutable lowlinks = Map.empty<'id, int>
  let mutable onStack = Set.empty<'id>
  let mutable result : List<NEList<'id>> = []

  let nodeSet = Set.ofList nodes

  let rec strongConnect (v : 'id) =
    indices <- Map.add v index indices
    lowlinks <- Map.add v index lowlinks
    index <- index + 1
    stack <- v :: stack
    onStack <- Set.add v onStack

    for w in edges v do
      if Set.contains w nodeSet then
        if not (Map.containsKey w indices) then
          strongConnect w
          lowlinks <-
            Map.add
              v
              (min (Map.findUnsafe v lowlinks) (Map.findUnsafe w lowlinks))
              lowlinks
        elif Set.contains w onStack then
          lowlinks <-
            Map.add
              v
              (min (Map.findUnsafe v lowlinks) (Map.findUnsafe w indices))
              lowlinks

    if Map.findUnsafe v lowlinks = Map.findUnsafe v indices then
      let mutable scc = []
      let mutable keepPopping = true
      while keepPopping do
        match stack with
        | w :: rest ->
          stack <- rest
          onStack <- Set.remove w onStack
          scc <- w :: scc
          if w = v then keepPopping <- false
        | [] -> keepPopping <- false

      match scc with
      | head :: tail -> result <- { head = head; tail = tail } :: result
      | [] -> ()

  for node in nodes do
    if not (Map.containsKey node indices) then strongConnect node

  result |> List.rev


// =====================
// SCC batch hashing
// =====================

type private ItemInfo =
  | TypeItem of PT.PackageType.PackageType * string
  | FnItem of PT.PackageFn.PackageFn * string
  | ValueItem of PT.PackageValue.PackageValue * string

let private getItemFQN (item : ItemInfo) : string =
  match item with
  | TypeItem(_, fqn) -> fqn
  | FnItem(_, fqn) -> fqn
  | ValueItem(_, fqn) -> fqn

let private computeItemHash (mode : HashRefMode) (item : ItemInfo) : PT.ContentHash =
  match item with
  | TypeItem(t, _) -> computeTypeHash mode t
  | FnItem(fn, _) -> computeFnHash mode fn
  | ValueItem(v, _) -> computeValueHash mode v

let private serializeItemCanonical
  (mode : HashRefMode)
  (item : ItemInfo)
  : byte array =
  use ms = new MemoryStream()
  use w = new BinaryWriter(ms)
  match item with
  | TypeItem(t, _) ->
    w.Write(0uy)
    writeCanonicalTypeDeclaration mode w t.declaration
  | FnItem(fn, _) ->
    w.Write(1uy)
    writeCanonicalExpr mode w fn.body
    Common.List.write w Common.String.write fn.typeParams
    Common.NEList.write (writeCanonicalParameter mode) w fn.parameters
    writeCanonicalTypeReference mode w fn.returnType
  | ValueItem(v, _) ->
    w.Write(2uy)
    writeCanonicalExpr mode w v.body
  ms.ToArray()


/// Given a set of package items and their dependencies, compute hashes
/// handling SCCs via batch hashing with name-ref substitution.
let computeHashesWithSCCs
  (types : Map<uuid, PT.PackageType.PackageType * string>)
  (fns : Map<uuid, PT.PackageFn.PackageFn * string>)
  (values : Map<uuid, PT.PackageValue.PackageValue * string>)
  (getDeps : uuid -> List<uuid>)
  : Map<uuid, PT.ContentHash> =

  // Build unified item map
  let items =
    Map.fold
      (fun acc id (t, fqn) -> Map.add id (TypeItem(t, fqn)) acc)
      Map.empty
      types
    |> Map.fold (fun acc id (fn, fqn) -> Map.add id (FnItem(fn, fqn)) acc)
    <| fns
    |> Map.fold (fun acc id (v, fqn) -> Map.add id (ValueItem(v, fqn)) acc)
    <| values

  let allIds = items |> Map.keys |> Seq.toList

  // Detect SCCs
  let sccs = findSCCs allIds getDeps

  // Compute hashes per SCC
  let mutable hashMap = Map.empty<uuid, PT.ContentHash>

  for scc in sccs do
    let sccIds = scc.head :: scc.tail

    if List.length sccIds = 1 then
      // Single-item SCC: hash normally
      let id = scc.head
      let item = Map.findUnsafe id items
      let hash = computeItemHash Normal item
      hashMap <- Map.add id hash hashMap
    else
      // Multi-item SCC: batch hash with FQN substitution
      // Build uuid→FQN map for intra-SCC references
      let sccNameMap =
        sccIds
        |> List.map (fun id -> id, getItemFQN (Map.findUnsafe id items))
        |> Map.ofList

      let mode = SccNameRef sccNameMap

      // Sort by FQN for determinism
      let sortedItems =
        sccIds
        |> List.map (fun id -> (id, Map.findUnsafe id items))
        |> List.sortBy (fun (_, item) -> getItemFQN item)

      // Concatenate canonical bytes of all items in the SCC
      let groupBytes =
        sortedItems
        |> List.map (fun (_, item) -> serializeItemCanonical mode item)
        |> Array.concat

      let groupHash = SHA256.HashData(groupBytes) |> contentHashFromSHA256Bytes

      // Each item's hash = hash(groupHash + FQN)
      for (id, item) in sortedItems do
        let fqn = getItemFQN item
        let itemHash =
          hashWithWriter (fun w ->
            PTC.ContentHash.write w groupHash
            Common.String.write w fqn)
        hashMap <- Map.add id itemHash hashMap

  hashMap
