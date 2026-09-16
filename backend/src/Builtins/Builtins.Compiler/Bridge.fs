module Builtins.Compiler.Bridge

// The ProgramTypes -> compiler-AST bridge (plan §6), the durable path that
// avoids re-parsing text. It is a TOTAL function: every construct it
// understands it lowers; everything else returns a structured, categorized
// hard-fail (never a guess, never a silent drop). Each `Error` string is a
// blocker record: "unsupported-<what>: <detail>" — greppable and rank-able.
//
// Scope: Int/Bool/String/Char arithmetic + if/let/vars/params, cross-fn calls
// (whole call-graph), and NON-generic custom types (records + enums: type defs,
// construction, field access). Still hard-failing: pattern match (EMatch),
// generics, lists/tuples/dicts, pipes, lambdas, builtins.
//
// `isSum` (below) is the type environment: a map from a custom type's content
// hash to whether it's a sum type (true) or record (false). The builtin builds
// it by fetching the transitive type-def closure; only types in the map lower.

open Prelude

module PT = LibExecution.ProgramTypes

// AST here is the airlifted compiler's AST (top-level module in LibCompiler).

/// The type environment: what a referenced custom type is. Records/enums lower
/// to a named TRecord/TSum + a TypeDef; a (non-generic) alias inlines to its
/// already-bridged target type.
type TypeEntry =
  | TERecord
  | TESum
  | TEAlias of AST.Type

/// Threaded through expression bridging. `Params` maps EArg indices to the
/// enclosing fn's compiler-side param names; `Self` is the current fn's compiled
/// name (for ESelf recursion); `Values` inlines referenced package constants.
/// How a wire-marshalable builtin arg is stringified on the compiled side.
type WireArg =
  | WAString
  | WAInt
  | WABool
  | WAFloat
  | WAUnit // a Unit param carries no data; sent as the literal "unit"
  /// A container/custom-type arg, marshalled with marshalTyped into the same wire
  /// format the daemon decodes with wireToDvalTyped. Until this existed the seam
  /// carried only scalars in the ARG direction, which is why altJsonFormat (arg: a
  /// Json enum), cliExecute, fileRead and ~300 others couldn't route at all.
  | WATyped of AST.Type

/// How a builtin's wire response is unmarshaled back into a native value.
type WireRet =
  | WRUnit
  | WRString
  | WRInt // Int64
  | WRBool
  /// Any container (Option/Result/List of marshalable types), decoded recursively
  /// over the bridged return type. Replaces the old fixed Option/Result-of-String
  /// cases with a general, composable path.
  | WRTyped of AST.Type

type BridgeCtx =
  { Params : string[]
    Self : string
    /// Package values available as shared nullary fns (see Compiler.valueFnDef).
    /// Only membership matters — an EValue lowers to a CALL of that fn, so the
    /// body itself never travels through here (that inlining is what exploded).
    Values : Set<string>
    /// Why a referenced package value ISN'T in `Values`. A value whose own body
    /// fails to bridge is dropped, and every fn referencing it then failed with a
    /// useless "package value not resolved" that hid the actual blocker. Carry the
    /// real error so the fn reports the root cause instead of the symptom.
    ValueErrors : Map<string, string>
    /// Effectful builtins routable through the host RPC seam: name -> (per-arg
    /// wire types, return wire type). Built from the live runtime.
    Effectful : Map<string, WireArg list * WireRet>
    /// Emitted type definitions, needed to marshal a record/enum ARG into the wire
    /// format (marshalTyped dispatches on the def).
    TypeDefs : Map<string, AST.TypeDef> }

let private err (category : string) (detail : string) : Result<'a, string> =
  Error $"unsupported-{category}: {detail}"

/// Collect a list of Results, short-circuiting on the first Error.
let private allOk (results : List<Result<'a, string>>) : Result<List<'a>, string> =
  (Ok [], results)
  ||> List.fold (fun acc r ->
    match acc, r with
    | Error e, _ -> Error e
    | Ok _, Error e -> Error e
    | Ok xs, Ok x -> Ok(xs @ [ x ]))

/// Deterministic compiler-side identifier for a package fn / type, from its
/// content hash — so a whole call/type graph lowers with consistent names.
// The "fn." prefix is deliberately dotted: the compiler's typechecker treats
// dotted names as module-scoped (like `Stdlib.List.map`) and INFERS type args
// for bare generic calls, whereas a dotless name is a "local user-defined
// generic" that `RequireExplicitTypeArgsForBareCalls` demands explicit type args
// for. Package fns are qualified, so module-scoped is correct — and it unblocks
// every bare generic call site. The dot is only a resolver signal: x64 labels are
// internal Map<string,pos> keys (never emitted as assembler text), so it's
// codegen-safe and normalizes to the same binary.
let nameFor (hash : string) : string = "fn." + hash
// Dotted, NOT "T_<hash>": the compiler mangles a type into a specialized fn's name
// and parses it back with `mangled.Split('_')` (tryParseMangledType). An underscore
// in our name collides with that separator, so `T_<hash>` came back as the type named
// "T" -> `TRecord("T", [])` -> "payloadSize: Record type 'T' not found in typeReg".
// A dot is safe for the same reason it is in nameFor: the compiler's own type names
// are already dotted (Stdlib.Option.Option) and contain no underscore, so they
// round-trip as a single token.
let nameForType (hash : string) : string = "T." + hash

/// A package value is emitted ONCE as a shared nullary fn (see Compiler.valueFnDef)
/// and every reference calls it. Previously each EValue spliced the value's whole
/// bridged body in, and since a value's body contains its dependencies' bodies,
/// chains compounded multiplicatively — 55GB on a 50-fn batch. Dotted for the same
/// reason as nameFor/nameForType: the compiler mangles names and splits them on '_'.
let valueFnName (hash : string) : string = "__val." + hash

// Pure Darklang stdlib fns for which the compiler ships a REALLY-equivalent native
// implementation. A call to one is emitted as a call to the compiler's native fn,
// and its Dark definition is NOT bridged. This is deliberately restricted to fns
// that (a) do NOT currently compile as Dark and (b) have a native equivalent whose
// signature (arg order + lambda arg order) is verified identical — so routing is
// purely additive (the Dark version was failing anyway) and semantics match. The
// four here are all defined via a bare-`[]` fold accumulator (or a missing helper)
// the compiler can't infer; the native versions annotate `empty<b>()` and compile.
// Keyed by (owner, modules, name); the native name is `modules...name` joined by ".".
// Equivalence is additionally gated by the differential run-sweep (0 diffs required).
let private nativeStdlibRoutes : Set<string * string list * string> =
  [ "map"; "filter"; "indexedMap"; "reverse" ]
  |> List.map (fun n -> ("Darklang", [ "Stdlib"; "List" ], n))
  |> Set.ofList

/// If a resolved fn ref points at an allowlisted stdlib fn, the compiler's native
/// name to route it to; else None.
let routeNativeName (loc : PT.PackageLocation) : string option =
  if Set.contains (loc.owner, loc.modules, loc.name) nativeStdlibRoutes then
    Some(String.concat "." (loc.modules @ [ loc.name ]))
  else
    None

/// Route a resolved fn-name reference (checks its binding location).
let routeResolved (resolved : PT.ResolvedName<PT.FQFnName.FQFnName>) : string option =
  match resolved.location with
  | Some loc -> routeNativeName loc
  | None -> None

// Dark's Option/Result are ordinary package types, but the compiler ships its OWN
// native Stdlib.Option.Option / Stdlib.Result.Result (its stdlib and the daemon's
// unmarshaller use them). Bridging Dark's versions to a distinct T_<hash> splits
// Option/Result into two non-unifying types AND double-defines the Some/None/Ok/
// Error constructors. So map both hashes to the native names everywhere and skip
// emitting a TypeDef for them (buildPieces filters via isNativeType).
let optionTypeHash : string = LibExecution.PackageRefs.Type.Stdlib.option ()
let resultTypeHash : string = LibExecution.PackageRefs.Type.Stdlib.result ()

let isNativeType (h : string) : bool = h = optionTypeHash || h = resultTypeHash

/// The compiler-side type name for a package type hash: the native Option/Result
/// for those two, else a T_<hash>.
let compilerTypeName (h : string) : string =
  if h = optionTypeHash then "Stdlib.Option.Option"
  elif h = resultTypeHash then "Stdlib.Result.Result"
  else nameForType h

/// Pure-tier builtin migration (row 1 of the seam's dispatch table, see
/// notes/compiler-merge/BUILTINS-ARCHITECTURE.md): a call to one of these
/// main-repo builtins lowers to a call to the compiler's OWN native stdlib fn,
/// which implements the same Darklang semantics on ~12 intrinsics. Curated and
/// conservative — the type checker rejects any signature mismatch (safe), and
/// each entry must be differential-tested (compiled vs interpreter) before it's
/// trusted for execution. Effectful builtins are NOT here — those await the
/// runtime seam (daemon). Keyed by the main-repo builtin's bare name.
let builtinToStdlib : Map<string, string> =
  Map
    [ // `unwrap` is a compiler INTRINSIC, not a stdlib fn: the typechecker
      // special-cases the name `Builtin.unwrap` (1.5_TypeChecking.fs) and lowers
      // it to a variant-tag check that yields the Some/Ok payload or panics on
      // None/Error (2_AST_to_ANF.fs). Our Option/Result are already the native
      // types, so the arg matches. Emitting `AST.Call("Builtin.unwrap", [arg])`
      // (same shape as a routed stdlib call) is exactly what the compiler wants.
      "unwrap", "Builtin.unwrap"
      // String
      // NOT ROUTED — verified NOT equivalent, each a silent wrong answer on non-ASCII
      // (found by the equivalence harness with a non-ASCII input; ASCII hides all three):
      //   stringLength      -> compiler's String.length counts UTF-8 BYTES (it reads the
      //                        length prefix; __string_hash loops it with getByteAt),
      //                        Dark counts CHARACTERS. "héllo日本" -> 12 vs 7.
      //   stringToUppercase -> compiler's toUpperCase is ASCII-only: "héllo" -> "HéLLO",
      //                        Dark gives "HÉLLO".
      //   stringToLowercase -> same, ASCII-only: "HÉLLO" -> "hÉllo" vs "héllo".
      // Routing these made compiled code return wrong answers rather than fail. Leave
      // them unrouted (those fns just don't compile) until the compiler's String stdlib
      // is unicode-aware.
      "stringIsEmpty", "Stdlib.String.isEmpty"
      "stringContains", "Stdlib.String.contains"
      "stringJoin", "Stdlib.String.join"
      "stringSplit", "Stdlib.String.split"
      "stringSlice", "Stdlib.String.slice"
      "stringTrim", "Stdlib.String.trim"
      "stringTrimStart", "Stdlib.String.trimStart"
      "stringTrimEnd", "Stdlib.String.trimEnd"
      "stringReverse", "Stdlib.String.reverse"
      "stringStartsWith", "Stdlib.String.startsWith"
      "stringEndsWith", "Stdlib.String.endsWith"
      "stringPrepend", "Stdlib.String.prepend"
      "stringIndexOf", "Stdlib.String.indexOf"
      "stringPadStart", "Stdlib.String.padStart"
      "stringPadEnd", "Stdlib.String.padEnd"
      "stringRepeat", "Stdlib.String.repeat"
      // List
      "listLength", "Stdlib.List.length"
      "listAppend", "Stdlib.List.append"
      "listReverse", "Stdlib.List.reverse"
      "listMap", "Stdlib.List.map"
      "listFilter", "Stdlib.List.filter"
      "listFold", "Stdlib.List.fold"
      "listFlatten", "Stdlib.List.flatten"
      "listIsEmpty", "Stdlib.List.isEmpty"
      "listTake", "Stdlib.List.take"
      "listDrop", "Stdlib.List.drop"
      "listPush", "Stdlib.List.push"
      "listPushBack", "Stdlib.List.pushBack"
      "listSingleton", "Stdlib.List.singleton"
      "listGetAt", "Stdlib.List.getAt"
      "listMember", "Stdlib.List.member"
      // Bool
      "boolNot", "Stdlib.Bool.not"
      "boolAnd", "Stdlib.Bool.and"
      "boolOr", "Stdlib.Bool.or"
      "boolXor", "Stdlib.Bool.xor"
      // Int (lowered to Int64, matching TInt->TInt64)
      "intToString", "Stdlib.Int64.toString"
      "int64ToString", "Stdlib.Int64.toString"
      "intGreaterThan", "Stdlib.Int64.greaterThan"
      "int64GreaterThan", "Stdlib.Int64.greaterThan"
      "intGreaterThanOrEqualTo", "Stdlib.Int64.greaterThanOrEqualTo"
      "intLessThan", "Stdlib.Int64.lessThan"
      "int64LessThan", "Stdlib.Int64.lessThan"
      "intDivide", "Stdlib.Int64.div"
      "int64Divide", "Stdlib.Int64.div"
      "intMax", "Stdlib.Int64.max"
      "intMin", "Stdlib.Int64.min"
      "intAbsoluteValue", "Stdlib.Int64.absoluteValue"
      // Option / Result
      "optionMap", "Stdlib.Option.map"
      "optionWithDefault", "Stdlib.Option.withDefault"
      "optionAndThen", "Stdlib.Option.andThen"
      "optionIsSome", "Stdlib.Option.isSome"
      "optionIsNone", "Stdlib.Option.isNone"
      "resultMap", "Stdlib.Result.map"
      "resultWithDefault", "Stdlib.Result.withDefault"
      "resultAndThen", "Stdlib.Result.andThen"
      "resultIsOk", "Stdlib.Result.isOk"
      "resultIsError", "Stdlib.Result.isError"
      // Int8
      "int8LessThan", "Stdlib.Int8.lessThan"
      "int8GreaterThan", "Stdlib.Int8.greaterThan"
      "int8GreaterThanOrEqualTo", "Stdlib.Int8.greaterThanOrEqualTo"
      "int8LessThanOrEqualTo", "Stdlib.Int8.lessThanOrEqualTo"
      "int8ToString", "Stdlib.Int8.toString"
      "int8Add", "Stdlib.Int8.add"
      "int8Subtract", "Stdlib.Int8.sub"
      "int8Multiply", "Stdlib.Int8.mul"
      "int8Divide", "Stdlib.Int8.div"
      "int8Mod", "Stdlib.Int8.mod"
      // Int16
      "int16LessThan", "Stdlib.Int16.lessThan"
      "int16GreaterThan", "Stdlib.Int16.greaterThan"
      "int16GreaterThanOrEqualTo", "Stdlib.Int16.greaterThanOrEqualTo"
      "int16LessThanOrEqualTo", "Stdlib.Int16.lessThanOrEqualTo"
      "int16ToString", "Stdlib.Int16.toString"
      "int16Add", "Stdlib.Int16.add"
      "int16Subtract", "Stdlib.Int16.sub"
      "int16Multiply", "Stdlib.Int16.mul"
      "int16Divide", "Stdlib.Int16.div"
      "int16Mod", "Stdlib.Int16.mod"
      // Int32
      "int32LessThan", "Stdlib.Int32.lessThan"
      "int32GreaterThan", "Stdlib.Int32.greaterThan"
      "int32GreaterThanOrEqualTo", "Stdlib.Int32.greaterThanOrEqualTo"
      "int32LessThanOrEqualTo", "Stdlib.Int32.lessThanOrEqualTo"
      "int32ToString", "Stdlib.Int32.toString"
      "int32Add", "Stdlib.Int32.add"
      "int32Subtract", "Stdlib.Int32.sub"
      "int32Multiply", "Stdlib.Int32.mul"
      "int32Divide", "Stdlib.Int32.div"
      "int32Mod", "Stdlib.Int32.mod"
      // UInt8
      "uint8LessThan", "Stdlib.UInt8.lessThan"
      "uint8GreaterThan", "Stdlib.UInt8.greaterThan"
      "uint8GreaterThanOrEqualTo", "Stdlib.UInt8.greaterThanOrEqualTo"
      "uint8LessThanOrEqualTo", "Stdlib.UInt8.lessThanOrEqualTo"
      "uint8ToString", "Stdlib.UInt8.toString"
      "uint8Add", "Stdlib.UInt8.add"
      "uint8Subtract", "Stdlib.UInt8.sub"
      "uint8Multiply", "Stdlib.UInt8.mul"
      "uint8Divide", "Stdlib.UInt8.div"
      "uint8Mod", "Stdlib.UInt8.mod"
      // UInt16
      "uint16LessThan", "Stdlib.UInt16.lessThan"
      "uint16GreaterThan", "Stdlib.UInt16.greaterThan"
      "uint16GreaterThanOrEqualTo", "Stdlib.UInt16.greaterThanOrEqualTo"
      "uint16LessThanOrEqualTo", "Stdlib.UInt16.lessThanOrEqualTo"
      "uint16ToString", "Stdlib.UInt16.toString"
      "uint16Add", "Stdlib.UInt16.add"
      "uint16Subtract", "Stdlib.UInt16.sub"
      "uint16Multiply", "Stdlib.UInt16.mul"
      "uint16Divide", "Stdlib.UInt16.div"
      "uint16Mod", "Stdlib.UInt16.mod"
      // UInt32
      "uint32LessThan", "Stdlib.UInt32.lessThan"
      "uint32GreaterThan", "Stdlib.UInt32.greaterThan"
      "uint32GreaterThanOrEqualTo", "Stdlib.UInt32.greaterThanOrEqualTo"
      "uint32LessThanOrEqualTo", "Stdlib.UInt32.lessThanOrEqualTo"
      "uint32ToString", "Stdlib.UInt32.toString"
      "uint32Add", "Stdlib.UInt32.add"
      "uint32Subtract", "Stdlib.UInt32.sub"
      "uint32Multiply", "Stdlib.UInt32.mul"
      "uint32Divide", "Stdlib.UInt32.div"
      "uint32Mod", "Stdlib.UInt32.mod"
      // UInt64
      "uint64LessThan", "Stdlib.UInt64.lessThan"
      "uint64GreaterThan", "Stdlib.UInt64.greaterThan"
      "uint64GreaterThanOrEqualTo", "Stdlib.UInt64.greaterThanOrEqualTo"
      "uint64LessThanOrEqualTo", "Stdlib.UInt64.lessThanOrEqualTo"
      "uint64ToString", "Stdlib.UInt64.toString"
      "uint64Add", "Stdlib.UInt64.add"
      "uint64Subtract", "Stdlib.UInt64.sub"
      "uint64Multiply", "Stdlib.UInt64.mul"
      "uint64Divide", "Stdlib.UInt64.div"
      "uint64Mod", "Stdlib.UInt64.mod"
      // Dict. Dark's key is always String, which is the compiler's `k`, and Dark's
      // Option return IS the compiler's native Option (already unified). Only pairs
      // whose SEMANTICS match are routed, not merely their signatures:
      //
      // NOT routed, and why (each was verified, not assumed):
      //  - dictSet: Dark RAISES if the key already exists; native Dict.set overwrites.
      //    `dictSetOverridingDuplicates` is the real equivalent of native set.
      //  - dictFromList: Dark returns Option (None on duplicate keys); native
      //    fromList overwrites and returns a bare Dict. fromListOverwritingDuplicates
      //    is the real equivalent.
      //  - dictKeys/dictValues/dictToList: ORDER DIFFERS. Dark's Dict is an F# Map
      //    (key-sorted iteration); the compiler's is a HAMT (hash order). Measured:
      //    compiled ["apple","zebra","mango"] vs interpreted ["apple","mango","zebra"].
      //  - dictMerge: native merge is __mergeHelper(dict2, dict1) — the conflict
      //    winner needs proving before it can be called equivalent.
      "dictGet", "Stdlib.Dict.get"
      "dictMember", "Stdlib.Dict.contains"
      "dictSize", "Stdlib.Dict.size"
      "dictRemove", "Stdlib.Dict.remove"
      "dictSetOverridingDuplicates", "Stdlib.Dict.set"
      "dictFromListOverwritingDuplicates", "Stdlib.Dict.fromList"
      // Float
      "floatMultiply", "Stdlib.Float.multiply"
      // Compiler INTRINSICS (2_AST_to_ANF.fs lowers these names to an ANF op ->
      // MIR -> LIR -> x64), not stdlib fns. Equivalent by type: the bridge maps
      // PT.TInt/TInt64 -> AST.TInt64 and PT.TFloat -> AST.TFloat64, so Dark's
      // intToFloat (TInt -> TFloat) IS Int64.toFloat (Int64 -> Float64).
      "intToFloat", "Stdlib.Int64.toFloat"
      "int64ToFloat", "Stdlib.Int64.toFloat"
      "floatSqrt", "Stdlib.Float.sqrt"
      // Bytes. stringToBlob is a UTF-8 encode; the compiler stores a String as UTF-8
      // bytes already, so Stdlib.String.toBytes (added for this) is a byte copy and
      // matches Encoding.UTF8.GetBytes exactly. Verified on non-ASCII.
      //
      // blobToBytes is deliberately NOT routed: it returns List<UInt8>, while the
      // native Stdlib.Bytes.toList returns List<Int64>. Same bytes, different element
      // type -> not equivalent, and the difference would be invisible until it wasn't.
      "stringToBlob", "Stdlib.String.toBytes"
      // Int64 (full)
      "int64LessThan", "Stdlib.Int64.lessThan"
      "int64GreaterThan", "Stdlib.Int64.greaterThan"
      "int64GreaterThanOrEqualTo", "Stdlib.Int64.greaterThanOrEqualTo"
      "int64LessThanOrEqualTo", "Stdlib.Int64.lessThanOrEqualTo"
      "int64ToString", "Stdlib.Int64.toString"
      "int64Add", "Stdlib.Int64.add"
      "int64Subtract", "Stdlib.Int64.sub"
      "int64Multiply", "Stdlib.Int64.mul"
      "int64Divide", "Stdlib.Int64.div"
      "int64Mod", "Stdlib.Int64.mod"
      "int64Power", "Stdlib.Int64.power"
      "int64Negate", "Stdlib.Int64.negate"
      "int64AbsoluteValue", "Stdlib.Int64.absoluteValue"
      "int64Max", "Stdlib.Int64.max"
      "int64Min", "Stdlib.Int64.min"
      "int64Clamp", "Stdlib.Int64.clamp"
      "int64BitwiseAnd", "Stdlib.Int64.bitwiseAnd"
      "int64BitwiseOr", "Stdlib.Int64.bitwiseOr"
      "int64BitwiseXor", "Stdlib.Int64.bitwiseXor"
      "int64BitwiseNot", "Stdlib.Int64.bitwiseNot"
      "int64ShiftLeft", "Stdlib.Int64.shiftLeft"
      "int64ShiftRight", "Stdlib.Int64.shiftRight"
      "int64IsEven", "Stdlib.Int64.isEven"
      "int64IsOdd", "Stdlib.Int64.isOdd"
      // Int8 (extra ops)
      "int8Power", "Stdlib.Int8.power"
      "int8Negate", "Stdlib.Int8.negate"
      "int8AbsoluteValue", "Stdlib.Int8.absoluteValue"
      "int8Max", "Stdlib.Int8.max"
      "int8Min", "Stdlib.Int8.min"
      "int8Clamp", "Stdlib.Int8.clamp"
      "int8BitwiseAnd", "Stdlib.Int8.bitwiseAnd"
      "int8BitwiseOr", "Stdlib.Int8.bitwiseOr"
      "int8BitwiseXor", "Stdlib.Int8.bitwiseXor"
      "int8BitwiseNot", "Stdlib.Int8.bitwiseNot"
      "int8ShiftLeft", "Stdlib.Int8.shiftLeft"
      "int8ShiftRight", "Stdlib.Int8.shiftRight"
      "int8IsEven", "Stdlib.Int8.isEven"
      "int8IsOdd", "Stdlib.Int8.isOdd"
      // Int16 (extra ops)
      "int16Power", "Stdlib.Int16.power"
      "int16Negate", "Stdlib.Int16.negate"
      "int16AbsoluteValue", "Stdlib.Int16.absoluteValue"
      "int16Max", "Stdlib.Int16.max"
      "int16Min", "Stdlib.Int16.min"
      "int16Clamp", "Stdlib.Int16.clamp"
      "int16BitwiseAnd", "Stdlib.Int16.bitwiseAnd"
      "int16BitwiseOr", "Stdlib.Int16.bitwiseOr"
      "int16BitwiseXor", "Stdlib.Int16.bitwiseXor"
      "int16BitwiseNot", "Stdlib.Int16.bitwiseNot"
      "int16ShiftLeft", "Stdlib.Int16.shiftLeft"
      "int16ShiftRight", "Stdlib.Int16.shiftRight"
      "int16IsEven", "Stdlib.Int16.isEven"
      "int16IsOdd", "Stdlib.Int16.isOdd"
      // Int32 (extra ops)
      "int32Power", "Stdlib.Int32.power"
      "int32Negate", "Stdlib.Int32.negate"
      "int32AbsoluteValue", "Stdlib.Int32.absoluteValue"
      "int32Max", "Stdlib.Int32.max"
      "int32Min", "Stdlib.Int32.min"
      "int32Clamp", "Stdlib.Int32.clamp"
      "int32BitwiseAnd", "Stdlib.Int32.bitwiseAnd"
      "int32BitwiseOr", "Stdlib.Int32.bitwiseOr"
      "int32BitwiseXor", "Stdlib.Int32.bitwiseXor"
      "int32BitwiseNot", "Stdlib.Int32.bitwiseNot"
      "int32ShiftLeft", "Stdlib.Int32.shiftLeft"
      "int32ShiftRight", "Stdlib.Int32.shiftRight"
      "int32IsEven", "Stdlib.Int32.isEven"
      "int32IsOdd", "Stdlib.Int32.isOdd"
      // UInt8 (extra ops)
      "uint8Power", "Stdlib.UInt8.power"
      "uint8Negate", "Stdlib.UInt8.negate"
      "uint8AbsoluteValue", "Stdlib.UInt8.absoluteValue"
      "uint8Max", "Stdlib.UInt8.max"
      "uint8Min", "Stdlib.UInt8.min"
      "uint8Clamp", "Stdlib.UInt8.clamp"
      "uint8BitwiseAnd", "Stdlib.UInt8.bitwiseAnd"
      "uint8BitwiseOr", "Stdlib.UInt8.bitwiseOr"
      "uint8BitwiseXor", "Stdlib.UInt8.bitwiseXor"
      "uint8BitwiseNot", "Stdlib.UInt8.bitwiseNot"
      "uint8ShiftLeft", "Stdlib.UInt8.shiftLeft"
      "uint8ShiftRight", "Stdlib.UInt8.shiftRight"
      "uint8IsEven", "Stdlib.UInt8.isEven"
      "uint8IsOdd", "Stdlib.UInt8.isOdd"
      // UInt16 (extra ops)
      "uint16Power", "Stdlib.UInt16.power"
      "uint16Negate", "Stdlib.UInt16.negate"
      "uint16AbsoluteValue", "Stdlib.UInt16.absoluteValue"
      "uint16Max", "Stdlib.UInt16.max"
      "uint16Min", "Stdlib.UInt16.min"
      "uint16Clamp", "Stdlib.UInt16.clamp"
      "uint16BitwiseAnd", "Stdlib.UInt16.bitwiseAnd"
      "uint16BitwiseOr", "Stdlib.UInt16.bitwiseOr"
      "uint16BitwiseXor", "Stdlib.UInt16.bitwiseXor"
      "uint16BitwiseNot", "Stdlib.UInt16.bitwiseNot"
      "uint16ShiftLeft", "Stdlib.UInt16.shiftLeft"
      "uint16ShiftRight", "Stdlib.UInt16.shiftRight"
      "uint16IsEven", "Stdlib.UInt16.isEven"
      "uint16IsOdd", "Stdlib.UInt16.isOdd"
      // UInt32 (extra ops)
      "uint32Power", "Stdlib.UInt32.power"
      "uint32Negate", "Stdlib.UInt32.negate"
      "uint32AbsoluteValue", "Stdlib.UInt32.absoluteValue"
      "uint32Max", "Stdlib.UInt32.max"
      "uint32Min", "Stdlib.UInt32.min"
      "uint32Clamp", "Stdlib.UInt32.clamp"
      "uint32BitwiseAnd", "Stdlib.UInt32.bitwiseAnd"
      "uint32BitwiseOr", "Stdlib.UInt32.bitwiseOr"
      "uint32BitwiseXor", "Stdlib.UInt32.bitwiseXor"
      "uint32BitwiseNot", "Stdlib.UInt32.bitwiseNot"
      "uint32ShiftLeft", "Stdlib.UInt32.shiftLeft"
      "uint32ShiftRight", "Stdlib.UInt32.shiftRight"
      "uint32IsEven", "Stdlib.UInt32.isEven"
      "uint32IsOdd", "Stdlib.UInt32.isOdd"
      // UInt64 (extra ops)
      "uint64Power", "Stdlib.UInt64.power"
      "uint64Negate", "Stdlib.UInt64.negate"
      "uint64AbsoluteValue", "Stdlib.UInt64.absoluteValue"
      "uint64Max", "Stdlib.UInt64.max"
      "uint64Min", "Stdlib.UInt64.min"
      "uint64Clamp", "Stdlib.UInt64.clamp"
      "uint64BitwiseAnd", "Stdlib.UInt64.bitwiseAnd"
      "uint64BitwiseOr", "Stdlib.UInt64.bitwiseOr"
      "uint64BitwiseXor", "Stdlib.UInt64.bitwiseXor"
      "uint64BitwiseNot", "Stdlib.UInt64.bitwiseNot"
      "uint64ShiftLeft", "Stdlib.UInt64.shiftLeft"
      "uint64ShiftRight", "Stdlib.UInt64.shiftRight"
      "uint64IsEven", "Stdlib.UInt64.isEven"
      "uint64IsOdd", "Stdlib.UInt64.isOdd"
      // Char
      "charToString", "Stdlib.Char.toString"
      "charToLowercase", "Stdlib.Char.toLowercase"
      "charToUppercase", "Stdlib.Char.toUppercase"
      "charToAsciiCode", "Stdlib.Char.toCode"
      "charFromAsciiCode", "Stdlib.Char.fromCode"
      "charIsDigit", "Stdlib.Char.isDigit"
      "charIsLetter", "Stdlib.Char.isLetter"
      "charIsWhitespace", "Stdlib.Char.isWhitespace"
      "charIsAlphanumeric", "Stdlib.Char.isAlphanumeric"
      "charIsLowercase", "Stdlib.Char.isLowercase"
      "charIsUppercase", "Stdlib.Char.isUppercase"
      "boolToString", "Stdlib.Bool.toString" ]

/// The package-type hash a type-name resolution points at.
let private typeHash
  (nr : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  : Result<string, string> =
  match nr.resolved with
  | Error _ -> err "type" "unresolved type name"
  | Ok resolved ->
    match resolved.name with
    | PT.FQTypeName.Package(PT.Hash h) -> Ok h

let private typeHashOpt
  (nr : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  : List<string> =
  match nr.resolved with
  | Ok resolved ->
    match resolved.name with
    | PT.FQTypeName.Package(PT.Hash h) -> [ h ]
  | Error _ -> []

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

let rec bridgeType
  (types : Map<string, TypeEntry>)
  (t : PT.TypeReference)
  : Result<AST.Type, string> =
  let recurse = bridgeType types
  match t with
  | PT.TInt64 -> Ok AST.TInt64
  | PT.TInt8 -> Ok AST.TInt8
  | PT.TInt16 -> Ok AST.TInt16
  | PT.TInt32 -> Ok AST.TInt32
  | PT.TInt128 -> Ok AST.TInt128
  | PT.TUInt8 -> Ok AST.TUInt8
  | PT.TUInt16 -> Ok AST.TUInt16
  | PT.TUInt32 -> Ok AST.TUInt32
  | PT.TUInt64 -> Ok AST.TUInt64
  | PT.TUInt128 -> Ok AST.TUInt128
  // Dark's default Int is arbitrary-precision; the compiler has no bigint, so
  // we lower it to Int64. Sound for values in range; wraps at the Int64 edges.
  | PT.TInt -> Ok AST.TInt64
  | PT.TBool -> Ok AST.TBool
  | PT.TString -> Ok AST.TString
  | PT.TFloat -> Ok AST.TFloat64
  | PT.TChar -> Ok AST.TChar
  | PT.TUnit -> Ok AST.TUnit
  | PT.TCustomType(nr, typeArgs) ->
    typeHash nr
    |> Result.bind (fun h ->
      // Option/Result -> native compiler sum types, independent of the type env.
      if isNativeType h then
        typeArgs
        |> List.map recurse
        |> allOk
        |> Result.map (fun args -> AST.TSum(compilerTypeName h, args))
      else
      match Map.tryFind h types with
      // Not in the type env => unfetched / unsupported: hard-fail cleanly.
      | None -> err "type" "TCustomType"
      | Some(TEAlias target) ->
        if List.isEmpty typeArgs then Ok target
        else err "generics" "generic type alias"
      | Some entry ->
        typeArgs
        |> List.map recurse
        |> allOk
        |> Result.map (fun args ->
          match entry with
          | TESum -> AST.TSum(nameForType h, args)
          | _ -> AST.TRecord(nameForType h, args)))
  | PT.TVariable v -> Ok(AST.TVar v)
  | PT.TList inner -> recurse inner |> Result.map AST.TList
  | PT.TTuple(a, b, rest) ->
    (a :: b :: rest) |> List.map recurse |> allOk |> Result.map AST.TTuple
  // Dark's Dict is always String-keyed (PT.TDict carries only a value type). The
  // compiler's Dict IS key-generic -- Dict<k,v> over a HAMT, with __hash<k> and
  // __key_eq<k> monomorphized per key type, and __Hash.dark ships __hash_str
  // (FNV-1a over UTF-8 bytes) + __key_eq_str. So String keys are fully supported;
  // the "K=Int64 for now" note on AST.TDict is stale.
  | PT.TDict v -> recurse v |> Result.map (fun v' -> AST.TDict(AST.TString, v'))
  | PT.TFn(args, ret) ->
    (NEList.toList args |> List.map recurse |> allOk, recurse ret)
    |> fun (a, r) ->
      a |> Result.bind (fun args' -> r |> Result.map (fun ret' -> AST.TFunction(args', ret')))
  | PT.TBlob -> Ok AST.TBytes
  | PT.TDB _ -> err "type" "TDB"
  // Host types with no compiler representation are carried as their canonical
  // string form. Sound for well-typed programs: Dark treats a Uuid/DateTime
  // opaquely (pass, store, compare-by-equality), and every builtin that produces
  // or consumes one marshals through the daemon (Guid/ISO string round-trip), so
  // the compiled value stays in lockstep with the interpreter's.
  | PT.TUuid -> Ok AST.TString
  | PT.TDateTime -> Ok AST.TString
  | other -> err "type" (other.GetType().Name)

// ---------------------------------------------------------------------------
// Infix operators
// ---------------------------------------------------------------------------

let private bridgeInfix (op : PT.Infix) : Result<AST.BinOp, string> =
  match op with
  | PT.InfixFnCall PT.ArithmeticPlus -> Ok AST.Add
  | PT.InfixFnCall PT.ArithmeticMinus -> Ok AST.Sub
  | PT.InfixFnCall PT.ArithmeticMultiply -> Ok AST.Mul
  | PT.InfixFnCall PT.ArithmeticDivide -> Ok AST.Div
  | PT.InfixFnCall PT.ArithmeticModulo -> Ok AST.Mod
  | PT.InfixFnCall PT.ComparisonGreaterThan -> Ok AST.Gt
  | PT.InfixFnCall PT.ComparisonGreaterThanOrEqual -> Ok AST.Gte
  | PT.InfixFnCall PT.ComparisonLessThan -> Ok AST.Lt
  | PT.InfixFnCall PT.ComparisonLessThanOrEqual -> Ok AST.Lte
  | PT.InfixFnCall PT.ComparisonEquals -> Ok AST.Eq
  | PT.InfixFnCall PT.ComparisonNotEquals -> Ok AST.Neq
  | PT.InfixFnCall PT.StringConcat -> Ok AST.StringConcat
  | PT.InfixFnCall PT.ArithmeticPower -> err "infix" "power (no compiler BinOp)"
  | PT.BinOp PT.BinOpAnd -> Ok AST.And
  | PT.BinOp PT.BinOpOr -> Ok AST.Or

// ---------------------------------------------------------------------------
// Expressions
//
// `paramNames[i]` gives the compiler-side name for the i-th parameter; Dark
// bodies reference params positionally via EArg(id, index).
// ---------------------------------------------------------------------------

/// Multi-field enum payloads / record constructions become a single tuple
/// payload on the compiler side (its Constructor takes one optional payload).
let tuplePayloadPublic (xs : List<AST.Expr>) : AST.Expr option =
  match xs with
  | [] -> None
  | [ x ] -> Some x
  | many -> Some(AST.TupleLiteral many)

let private tuplePayload (xs : List<AST.Expr>) : AST.Expr option =
  match xs with
  | [] -> None
  | [ x ] -> Some x
  | many -> Some(AST.TupleLiteral many)

/// Multi-field enum patterns become a single tuple sub-pattern (mirrors the
/// tuple-payload convention used when constructing enums).
let private tuplePatPayload (ps : List<AST.Pattern>) : AST.Pattern option =
  match ps with
  | [] -> None
  | [ p ] -> Some p
  | many -> Some(AST.PTuple many)

let rec bridgePattern (p : PT.MatchPattern) : Result<AST.Pattern, string> =
  let r = bridgePattern
  match p with
  | PT.MPUnit _ -> Ok AST.PUnit
  | PT.MPBool(_, b) -> Ok(AST.PBool b)
  | PT.MPInt64(_, n) -> Ok(AST.PInt64 n)
  | PT.MPInt(_, big) ->
    if big >= bigint System.Int64.MinValue && big <= bigint System.Int64.MaxValue then
      Ok(AST.PInt64(int64 big))
    else
      err "pattern" "Int outside Int64 range"
  | PT.MPInt8(_, n) -> Ok(AST.PInt8Literal n)
  | PT.MPInt16(_, n) -> Ok(AST.PInt16Literal n)
  | PT.MPInt32(_, n) -> Ok(AST.PInt32Literal n)
  | PT.MPInt128(_, n) -> Ok(AST.PInt128Literal n)
  | PT.MPUInt8(_, n) -> Ok(AST.PUInt8Literal n)
  | PT.MPUInt16(_, n) -> Ok(AST.PUInt16Literal n)
  | PT.MPUInt32(_, n) -> Ok(AST.PUInt32Literal n)
  | PT.MPUInt64(_, n) -> Ok(AST.PUInt64Literal n)
  | PT.MPUInt128(_, n) -> Ok(AST.PUInt128Literal n)
  | PT.MPString(_, s) -> Ok(AST.PString s)
  | PT.MPChar(_, c) -> Ok(AST.PChar c)
  | PT.MPVariable(_, "_") -> Ok AST.PWildcard
  | PT.MPVariable(_, name) -> Ok(AST.PVar name)
  | PT.MPTuple(_, a, b, rest) ->
    (a :: b :: rest) |> List.map r |> allOk |> Result.map AST.PTuple
  | PT.MPList(_, pats) -> pats |> List.map r |> allOk |> Result.map AST.PList
  | PT.MPListCons(_, head, tail) ->
    r head
    |> Result.bind (fun h -> r tail |> Result.map (fun t -> AST.PListCons([ h ], t)))
  | PT.MPEnum(_, caseName, fieldPats) ->
    fieldPats
    |> List.map r
    |> allOk
    |> Result.map (fun ps -> AST.PConstructor(caseName, tuplePatPayload ps))
  | PT.MPFloat _ -> err "pattern" "MPFloat"
  | PT.MPOr _ -> err "pattern" "nested MPOr"

/// Bind a let-pattern over an already-bridged value and body. Tuple patterns
/// desugar to a temp binding plus per-element tuple-access lets. The temp name
/// is reused across nesting levels safely: each RHS is evaluated (reading the
/// outer temp) before the inner temp shadows it.
let rec private bindPattern
  (pat : PT.LetPattern)
  (value : AST.Expr)
  (body : AST.Expr)
  : Result<AST.Expr, string> =
  match pat with
  | PT.LPVariable(_, name) -> Ok(AST.Let(name, value, body))
  | PT.LPWildcard _ -> Ok(AST.Let("__dark_wild", value, body))
  | PT.LPUnit _ -> Ok(AST.Let("__dark_unit", value, body))
  | PT.LPTuple(_, first, second, rest) ->
    let tmp = "__dark_tuple_tmp"
    let subs = first :: second :: rest
    let rec build (i : int) (ps : List<PT.LetPattern>) : Result<AST.Expr, string> =
      match ps with
      | [] -> Ok body
      | p :: more ->
        build (i + 1) more
        |> Result.bind (fun inner -> bindPattern p (AST.TupleAccess(AST.Var tmp, i)) inner)
    build 0 subs |> Result.map (fun inner -> AST.Let(tmp, value, inner))

/// Whether a bridged return type can be recursively unmarshaled from the wire.
let rec isMarshalableType (t : AST.Type) : bool =
  match t with
  | AST.TString | AST.TInt64 | AST.TBool | AST.TUnit | AST.TBytes -> true
  | AST.TSum("Stdlib.Option.Option", [ inner ]) -> isMarshalableType inner
  | AST.TSum("Stdlib.Result.Result", [ a; b ]) -> isMarshalableType a && isMarshalableType b
  | AST.TList inner -> isMarshalableType inner
  | _ -> false

/// Recursively decode a wire response (an AST String expr) into a native value of
/// compiler type `t`. Compound children were escaped by the daemon (escWire), so
/// each is unescaped before decoding. `d` keeps the let-bound temp unique per depth.
/// Instantiate a type def's params with the args from a USE site: a def stores
/// `Only of 'a`, but the value at hand is `Scope<String>`. Without this, walking a
/// generic type's def hits a bare TVar and looks unserializable.
let rec substTVarsB (m : Map<string, AST.Type>) (t : AST.Type) : AST.Type =
  let s = substTVarsB m
  match t with
  | AST.TVar v -> Map.tryFind v m |> Option.defaultValue t
  | AST.TRecord(n, args) -> AST.TRecord(n, List.map s args)
  | AST.TSum(n, args) -> AST.TSum(n, List.map s args)
  | AST.TList inner -> AST.TList(s inner)
  | AST.TTuple ts -> AST.TTuple(List.map s ts)
  | AST.TDict(k, v) -> AST.TDict(s k, s v)
  | AST.TFunction(args, ret) -> AST.TFunction(List.map s args, s ret)
  | other -> other

/// Zip a def's type params against a use site's type args (tolerating arity drift).
let defSubstB (tps : string list) (args : AST.Type list) : Map<string, AST.Type> =
  tps
  |> List.mapi (fun i tp -> (tp, List.tryItem i args))
  |> List.choose (fun (tp, a) -> a |> Option.map (fun a -> (tp, a)))
  |> Map.ofList

/// The MIRROR of unmarshalTyped: build an expression that serializes a native value
/// of type `t` into the daemon's wire format (see Compiler.dvalToWire, which is the
/// spec). This is what makes equivalence PROVABLE: the compiled program emits the
/// exact same canonical encoding the interpreter's Dval does, so the two can be
/// compared as strings — instead of comparing stdout, where the compiler prints
/// records/nested containers as empty and most fns simply can't be checked.
///
/// Must stay byte-identical to dvalToWire: elements/fields escaped and joined by
/// "\n", enums as `Case` or `Case\n<escaped payload>`, Unit as "unit".
/// The marshaller emitted for a non-generic type. A recursive type (Json = ... |
/// Array of List<Json>) cannot be encoded by an INLINE expression — inlining it
/// generates AST forever. Emit one real function per type instead, so recursion
/// becomes a CALL and terminates naturally.
/// Dotted, like every other name we hand the compiler (it splits mangled names on '_').
let marshalFnName (typeName : string) : string = "__marshal." + typeName

/// The decode-side twin of marshalFnName: one `__unmarshal.<T>(s: String) : T` per
/// non-generic custom type, so a RECURSIVE type decodes by calling itself rather than
/// generating AST forever (the same reason the marshal side needs a fn, learned when an
/// inlined Json serializer core-dumped the CLI).
let unmarshalFnName (typeName : string) = "__unmarshal." + typeName

let rec marshalTypedSeen
  (inFnBody : bool)
  (seen : Set<string>)
  (defs : Map<string, AST.TypeDef>)
  (d : int)
  (t : AST.Type)
  (v : AST.Expr)
  : Result<AST.Expr, string> =
  // RECURSION GUARD. marshalTyped INLINES the serializer, so a recursive type
  // (Stdlib.AltJson.Json = ... | Array of List<Json>) generates AST forever and blows
  // the stack — it core-dumped the CLI. A recursive type genuinely cannot be encoded
  // by a finite inlined expression; it needs a recursive serializer FUNCTION emitted
  // per type. Until that exists, refuse cleanly.
  let recursionGuard (name : string) =
    if Set.contains name seen then Some(Error $"recursive type {name} (needs a recursive serializer fn)")
    else None
  // nested positions are never "in the fn body we're generating"
  let marshalTyped d t v = marshalTypedSeen false seen defs d t v
  let call n args = AST.Call(n, AST.NonEmptyList.fromList args)
  let esc e = call "Stdlib.hostRpcEscape" [ e ]
  let join xs sep = call "Stdlib.String.join" [ xs; AST.StringLiteral sep ]
  let cat a b = AST.BinOp(AST.StringConcat, a, b)
  match t with
  | AST.TString -> Ok v
  | AST.TChar -> Ok v // a Char is already its own text
  | AST.TInt64 -> Ok(call "Stdlib.Int64.toString" [ v ])
  | AST.TInt8 -> Ok(call "Stdlib.Int8.toString" [ v ])
  | AST.TInt16 -> Ok(call "Stdlib.Int16.toString" [ v ])
  | AST.TInt32 -> Ok(call "Stdlib.Int32.toString" [ v ])
  | AST.TUInt8 -> Ok(call "Stdlib.UInt8.toString" [ v ])
  | AST.TUInt16 -> Ok(call "Stdlib.UInt16.toString" [ v ])
  | AST.TUInt32 -> Ok(call "Stdlib.UInt32.toString" [ v ])
  | AST.TUInt64 -> Ok(call "Stdlib.UInt64.toString" [ v ])
  | AST.TBool -> Ok(call "Stdlib.Bool.toString" [ v ])
  // A Float travels as its IEEE-754 BIT PATTERN, not as text. The wire is a comparison
  // medium, not Dark semantics: its only job is `same value <-> same bytes`, and a
  // rendering cannot do that. Stdlib.Float.toString here is 12-significant-digit lossy,
  // so 123456789012345.0 and 123456789012346.0 render IDENTICALLY -- a rendering wire
  // can't see a miscompile it can't print. Bits are exact, and make NaN/Inf/-0.0 fall
  // out for free. Mirrored by dvalToWire's DFloat and wireToDval's TFloat.
  | AST.TFloat64 ->
    // Sent as two 32-bit HALVES, "hi:lo", not one UInt64.
    //
    // The compiler's unsigned comparisons are SIGNED: MIR.Lt lowers to LIR.LT (setl)
    // with no operand-type awareness. So Stdlib.UInt64.toString -- `if n < 10UL then
    // digit else recurse(n/10)` -- takes the `n < 10UL` branch for any n >= 2^63 (which
    // reads as negative) and returns __digitToString's `| _ -> "?"` fallthrough. Every
    // NEGATIVE float sets the sign bit, so its bits exceed 2^63 and stringified as a
    // bare "?". Proven: 5.0 (0x4014…, < 2^63) renders fine, -5.0 (0xC014…) gives "?".
    //
    // Both halves are < 2^32, hence < 2^63, so they stringify correctly under signed
    // comparison. `>>` is a logical shift here (Float.__isNegative relies on that for
    // `bits >> 63UL == 1UL`). Fixing the codegen properly is the real answer -- see
    // notes/compiler-merge/PROGRESSION-LOG.md.
    let bv = "__fb" + string d
    let b = AST.Var bv
    let hi = AST.BinOp(AST.Shr, b, AST.UInt64Literal 32UL)
    // lo = b - (hi << 32), NOT `b &&& 0xFFFFFFFFUL`: x64's `AND r/m64, imm32`
    // sign-extends the immediate, so masking with 0xFFFFFFFF ANDs against all-ones and
    // is a no-op -- the compiled side returned the whole 64-bit value as `lo` while the
    // interpreter returned the true low half. Sub + a shift-by-32 use only small
    // immediates and are exact.
    let lo = AST.BinOp(AST.Sub, b, AST.BinOp(AST.Shl, hi, AST.UInt64Literal 32UL))
    Ok(
      AST.Let(
        bv,
        call "Stdlib.Float.toBits" [ v ],
        cat
          (cat (call "Stdlib.UInt64.toString" [ hi ]) (AST.StringLiteral ":"))
          (call "Stdlib.UInt64.toString" [ lo ])))
  // `unit` regardless of the value, but keep v evaluated so effects/typing hold.
  | AST.TUnit -> Ok(AST.Let("__mu" + string d, v, AST.StringLiteral "unit"))
  | AST.TList inner ->
    let ev = "__me" + string d
    marshalTyped (d + 1) inner (AST.Var ev)
    |> Result.map (fun body ->
      let lam = AST.Lambda(AST.NonEmptyList.fromList [ (ev, inner) ], esc body)
      join (AST.TypeApp("Stdlib.List.map", [ inner; AST.TString ], AST.NonEmptyList.fromList [ v; lam ])) "\n")
  // Bytes marshal exactly like List<Int64>: the daemon's dvalToWire emits a Blob's
  // byte values one per line, so decompose with the native Stdlib.Bytes.toList and
  // reuse the TList encoder above rather than writing a second encoding to keep in
  // sync. Note Bytes.toList yields Int64, not UInt8 -- that mismatch is exactly why
  // the blobToBytes builtin is NOT routed; here it's internal to the wire format and
  // both sides agree on the digits, so it's sound.
  | AST.TBytes -> marshalTyped d (AST.TList AST.TInt64) (call "Stdlib.Bytes.toList" [ v ])
  | AST.TTuple ts ->
    // Tuples have no generic map; index each slot and concat with "\n".
    ts
    |> List.mapi (fun i el -> marshalTyped (d + 1) el (AST.TupleAccess(v, i)) |> Result.map esc)
    |> allOk
    |> Result.map (fun parts ->
      match parts with
      | [] -> AST.StringLiteral ""
      | first :: rest -> rest |> List.fold (fun acc p -> cat (cat acc (AST.StringLiteral "\n")) p) first)
  | AST.TSum("Stdlib.Option.Option", [ inner ]) ->
    let xv = "__mo" + string d
    marshalTyped (d + 1) inner (AST.Var xv)
    |> Result.map (fun body ->
      AST.Match(
        v,
        [ { Patterns = AST.NonEmptyList.singleton (AST.PConstructor("None", None))
            Guard = None
            Body = AST.StringLiteral "None" }
          { Patterns =
              AST.NonEmptyList.singleton (AST.PConstructor("Some", Some(AST.PVar xv)))
            Guard = None
            Body = cat (AST.StringLiteral "Some\n") (esc body) } ]
      ))
  | AST.TSum("Stdlib.Result.Result", [ okT; errT ]) ->
    let ov = "__mrk" + string d
    let evv = "__mre" + string d
    match marshalTyped (d + 1) okT (AST.Var ov), marshalTyped (d + 1) errT (AST.Var evv) with
    | Ok okBody, Ok errBody ->
      Ok(
        AST.Match(
          v,
          [ { Patterns = AST.NonEmptyList.singleton (AST.PConstructor("Ok", Some(AST.PVar ov)))
              Guard = None
              Body = cat (AST.StringLiteral "Ok\n") (esc okBody) }
            { Patterns =
                AST.NonEmptyList.singleton (AST.PConstructor("Error", Some(AST.PVar evv)))
              Guard = None
              Body = cat (AST.StringLiteral "Error\n") (esc errBody) } ]
        )
      )
    | Error e, _ -> Error e
    | _, Error e -> Error e
  // Records and user enums dispatch on the DEFINITION, not on whether the type says
  // TRecord or TSum. Callers that know a type only by hash (rtToMarshalable, which is
  // built from the runtime and has no type env) can't tell record from sum — but the
  // def can, so accept either tag and let the def decide.
  // A non-generic custom type is marshalled by its emitted fn — one definition,
  // called wherever needed, so recursion terminates. `inFnBody` is the one place we
  // expand inline: when GENERATING that fn's own body.
  | (AST.TRecord(name, []) | AST.TSum(name, [])) when
    not inFnBody && (match Map.tryFind name defs with Some _ -> true | None -> false) ->
    Ok(AST.Call(marshalFnName name, AST.NonEmptyList.singleton v))
  | AST.TRecord(name, targs) | AST.TSum(name, targs) when
    (match Map.tryFind name defs with
     | Some(AST.RecordDef _) -> true
     | _ -> false) ->
    match recursionGuard name with
    | Some e -> e
    | None ->
    match Map.tryFind name defs with
    | Some(AST.RecordDef(_, tps, fields)) ->
      let sub = defSubstB tps targs
      let inner d t v = marshalTypedSeen false (Set.add name seen) defs d t v
      fields
      |> List.sortBy fst
      |> List.map (fun (fname, ft) ->
        inner (d + 1) (substTVarsB sub ft) (AST.RecordAccess(v, fname)) |> Result.map esc)
      |> allOk
      |> Result.map (fun parts ->
        match parts with
        | [] -> AST.StringLiteral ""
        | first :: rest ->
          rest |> List.fold (fun acc p -> cat (cat acc (AST.StringLiteral "\n")) p) first)
    | _ -> Error $"marshal: no record def {name}"
  // A user enum: match every variant and emit `Case` / `Case\n<escaped fields>`.
  // dvalToWire escapes each FIELD separately, so a multi-field variant (which the
  // bridge packs into a tuple payload) must be destructured, not encoded as a tuple.
  | AST.TSum(name, targs) | AST.TRecord(name, targs) ->
    match recursionGuard name with
    | Some e -> e
    | None ->
    match Map.tryFind name defs with
    | Some(AST.SumTypeDef(_, tps, variants)) ->
      let sub = defSubstB tps targs
      let marshalTyped d t v = marshalTypedSeen false (Set.add name seen) defs d t v
      variants
      |> List.map (fun (variant : AST.Variant) ->
        match variant.Payload with
        | None ->
          Ok
            { AST.Patterns = AST.NonEmptyList.singleton (AST.PConstructor(variant.Name, None))
              AST.Guard = None
              AST.Body = AST.StringLiteral variant.Name }
        | Some(AST.TTuple elemTs0) ->
          // multi-field variant: bind each slot, escape each, join
          let elemTs = elemTs0 |> List.map (substTVarsB sub)
          let names = elemTs |> List.mapi (fun i _ -> $"__mv{d}_{i}")
          List.zip names elemTs
          |> List.map (fun (n, et) -> marshalTyped (d + 1) et (AST.Var n) |> Result.map esc)
          |> allOk
          |> Result.map (fun parts ->
            let body =
              parts
              |> List.fold
                (fun acc p -> cat (cat acc (AST.StringLiteral "\n")) p)
                (AST.StringLiteral variant.Name)
            { AST.Patterns =
                AST.NonEmptyList.singleton (
                  AST.PConstructor(variant.Name, Some(AST.PTuple(names |> List.map AST.PVar)))
                )
              AST.Guard = None
              AST.Body = body })
        | Some pt0 ->
          let pt = substTVarsB sub pt0
          let n = $"__mv{d}"
          marshalTyped (d + 1) pt (AST.Var n)
          |> Result.map (fun body ->
            { AST.Patterns =
                AST.NonEmptyList.singleton (AST.PConstructor(variant.Name, Some(AST.PVar n)))
              AST.Guard = None
              AST.Body = cat (cat (AST.StringLiteral variant.Name) (AST.StringLiteral "\n")) (esc body) }))
      |> allOk
      |> Result.map (fun cases -> AST.Match(v, cases))
    | _ -> Error $"marshal: no sum def {name}"
  | other -> Error $"marshal: {other}"

let marshalTyped (defs : Map<string, AST.TypeDef>) (d : int) (t : AST.Type) (v : AST.Expr) =
  marshalTypedSeen false Set.empty defs d t v

/// Which marshallers does this expression actually call? Emitting a marshaller for
/// EVERY type put a `__marshal.T` fn into every program — and one that fails to
/// typecheck breaks programs that never marshal anything (it cost 11 of 60 sampled
/// fns). Emit only what's reachable.
let rec marshalCallsInExpr (e : AST.Expr) : Set<string> =
  let r = marshalCallsInExpr
  let many xs = xs |> List.map r |> Set.unionMany
  match e with
  | AST.Call(n, args) ->
    let here =
      if n.StartsWith "__marshal." then Set.singleton (n.Substring 10) else Set.empty
    Set.union here (many (AST.NonEmptyList.toList args))
  | AST.TypeApp(_, _, args) -> many (AST.NonEmptyList.toList args)
  | AST.Apply(f, args) -> Set.union (r f) (many (AST.NonEmptyList.toList args))
  | AST.Let(_, v, b) -> Set.union (r v) (r b)
  | AST.If(c, t, e2) -> Set.unionMany [ r c; r t; r e2 ]
  | AST.BinOp(_, a, b) -> Set.union (r a) (r b)
  | AST.UnaryOp(_, a) -> r a
  | AST.Lambda(_, b) -> r b
  | AST.Match(s2, cases) ->
    Set.union (r s2) (cases |> List.map (fun (c : AST.MatchCase) -> r c.Body) |> Set.unionMany)
  | AST.RecordLiteral(_, fs) -> many (List.map snd fs)
  | AST.RecordAccess(rec_, _) -> r rec_
  | AST.RecordUpdate(rec_, ups) -> Set.union (r rec_) (many (List.map snd ups))
  | AST.Constructor(_, _, p) -> p |> Option.map r |> Option.defaultValue Set.empty
  | AST.TupleLiteral xs -> many xs
  | AST.TupleAccess(t, _) -> r t
  | AST.ListLiteral xs -> many xs
  | AST.ListCons(hs, t) -> Set.union (many hs) (r t)
  | AST.Closure(_, caps) -> many caps
  | AST.InterpolatedString _ -> Set.empty // interpolations can't contain a marshaller call
  | _ -> Set.empty

/// One marshaller FunctionDef per non-generic type def: `__marshal.T(v: T) : String`.
/// Its body expands T inline (inFnBody=true) while every nested custom type becomes a
/// call — so a recursive type emits a finite, self-referential function.
/// Generic types are skipped: their marshaller would need a marshalling fn per type
/// param (higher-order), which isn't built yet — they stay inline, which is fine as
/// long as they aren't recursive.
let marshalFnDefsFor (defs : Map<string, AST.TypeDef>) (needed : Set<string>) : List<AST.FunctionDef> =
  defs
  |> Map.toList
  |> List.filter (fun (name, _) -> Set.contains name needed)
  |> List.choose (fun (name, def) ->
    let tps =
      match def with
      | AST.RecordDef(_, tps, _) -> Some tps
      | AST.SumTypeDef(_, tps, _) -> Some tps
      | AST.TypeAlias _ -> None
    match tps with
    | Some [] ->
      let selfT =
        match def with
        | AST.SumTypeDef _ -> AST.TSum(name, [])
        | _ -> AST.TRecord(name, [])
      match marshalTypedSeen true Set.empty defs 0 selfT (AST.Var "v") with
      | Ok body ->
        Some(
          { Name = marshalFnName name
            TypeParams = []
            Params = AST.NonEmptyList.singleton ("v", selfT)
            ReturnType = AST.TString
            Body = body } : AST.FunctionDef
        )
      | Error _ -> None
    | _ -> None)

/// Transitively close the set: a marshaller's body may call other marshallers.
let marshalFnDefs (defs : Map<string, AST.TypeDef>) (seed : Set<string>) : List<AST.FunctionDef> =
  let mutable needed = seed
  let mutable changed = true
  while changed do
    let fns = marshalFnDefsFor defs needed
    let more =
      fns |> List.map (fun f -> marshalCallsInExpr f.Body) |> Set.unionMany |> Set.union needed
    changed <- more <> needed
    needed <- more
  marshalFnDefsFor defs needed

/// Can marshalTyped encode this type — and if NOT, WHY? Returning a reason rather
/// than a bool matters: "unprovable: TRecord(T.a6fa…)" tells you nothing, and a vague
/// bucket is exactly what hid the last two big blockers. Name the culprit instead.
let rec serializableReasonSeen
  (seen : Set<string>)
  (defs : Map<string, AST.TypeDef>)
  (t : AST.Type)
  : Result<unit, string> =
  let serializableReason defs t = serializableReasonSeen seen defs t
  let all xs = xs |> List.map (serializableReason defs) |> allOk |> Result.map (fun _ -> ())
  match t with
  | AST.TString | AST.TChar | AST.TBool | AST.TUnit | AST.TFloat64
  | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
  | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
  // Bytes is serializable unconditionally: marshalTypedSeen encodes it as its byte
  // values via Stdlib.Bytes.toList, mirroring dvalToWire's DBlob case. Kept in step
  // with isMarshalableType and marshalTypedSeen -- all three must agree on TBytes or
  // a fn is either falsely unprovable (here) or falsely gated (there).
  | AST.TBytes -> Ok()
  | AST.TList inner -> serializableReason defs inner
  | AST.TTuple ts -> all ts
  | AST.TSum("Stdlib.Option.Option", [ inner ]) -> serializableReason defs inner
  | AST.TSum("Stdlib.Result.Result", [ a; b ]) -> all [ a; b ]
  // A non-generic type is serialized via its emitted marshaller fn, so recursion is
  // fine — stop descending once we've seen it (the fn calls itself).
  | (AST.TRecord(name, []) | AST.TSum(name, [])) when Set.contains name seen -> Ok()
  | AST.TRecord(name, targs) | AST.TSum(name, targs) when
    (match Map.tryFind name defs with
     | Some(AST.RecordDef _) -> true
     | _ -> false) ->
    if Set.contains name seen then Error $"recursive type {name}" else
    match Map.tryFind name defs with
    | Some(AST.RecordDef(_, tps, fields)) ->
      let sub = defSubstB tps targs
      fields
      |> List.map (fun (fname, ft) ->
        serializableReasonSeen (Set.add name seen) defs (substTVarsB sub ft)
        |> Result.mapError (fun e -> $"{name}.{fname}: {e}"))
      |> allOk
      |> Result.map (fun _ -> ())
    | _ -> Error $"no def for record {name}"
  | AST.TSum(name, targs) | AST.TRecord(name, targs) ->
    if Set.contains name seen then Error $"recursive type {name}" else
    match Map.tryFind name defs with
    | Some(AST.SumTypeDef(_, tps, variants)) ->
      let sub = defSubstB tps targs
      variants
      |> List.map (fun (v : AST.Variant) ->
        match v.Payload with
        | None -> Ok()
        | Some p ->
          serializableReasonSeen (Set.add name seen) defs (substTVarsB sub p)
          |> Result.mapError (fun e -> $"{name}.{v.Name}: {e}"))
      |> allOk
      |> Result.map (fun _ -> ())
    | _ -> Error $"no def for sum {name}"
  | other -> Error $"{other}"

let serializableReason (defs : Map<string, AST.TypeDef>) (t : AST.Type) : Result<unit, string> =
  serializableReasonSeen Set.empty defs t

let isSerializableType (defs : Map<string, AST.TypeDef>) (t : AST.Type) : bool =
  match serializableReason defs t with
  | Ok() -> true
  | Error _ -> false

/// Decode a wire response (an AST String expr) into a native value of compiler type `t`.
/// TOTAL: an undecodable type is an Error, never a guess. It used to end in `| _ -> src`,
/// which hands compiled code the raw wire String typed as whatever `t` claimed -- sound
/// only while every isMarshalableType case had a decoder here, an invariant a comment
/// cannot enforce (widening the gate for TBytes made that "unreachable" line reachable).
/// Now the two can't drift silently: a missing decoder is a hard-fail with a named type.
let rec unmarshalTypedSeen
  (inFnBody : bool)
  (seen : Set<string>)
  (defs : Map<string, AST.TypeDef>)
  (d : int)
  (t : AST.Type)
  (src : AST.Expr)
  : Result<AST.Expr, string> =
  let unmarshalTypedR d t src = unmarshalTypedSeen false seen defs d t src
  let rv = "__rpc_" + string d
  let r = AST.Var rv
  let len e = AST.Call("Stdlib.String.length", AST.NonEmptyList.singleton e)
  let slice e a b = AST.Call("Stdlib.String.slice", AST.NonEmptyList.fromList [ e; a; b ])
  let starts e p = AST.Call("Stdlib.String.startsWith", AST.NonEmptyList.fromList [ e; AST.StringLiteral p ])
  let unesc e = AST.Call("Stdlib.hostRpcUnescape", AST.NonEmptyList.singleton e)
  let parseInt e = AST.Call("Stdlib.hostRpcParseInt", AST.NonEmptyList.singleton e)
  let optT = "Stdlib.Option.Option"
  let resT = "Stdlib.Result.Result"
  // Split a wire string into its "\n"-separated parts, and index one out. Every part was
  // escaped by the encoder, so each occupies exactly one line and positional indexing is
  // sound. getAt returns an Option; the wire is well-formed by construction (dvalToWire
  // wrote it), so "" is an unreachable default rather than a guess at a missing field.
  let split e = AST.Call("Stdlib.String.split", AST.NonEmptyList.fromList [ e; AST.StringLiteral "\n" ])
  let nth parts i =
    AST.TypeApp(
      "Stdlib.Option.withDefault",
      [ AST.TString ],
      AST.NonEmptyList.fromList
        [ AST.TypeApp(
            "Stdlib.List.getAt",
            [ AST.TString ],
            AST.NonEmptyList.fromList [ parts; AST.Int64Literal(int64 i) ])
          AST.StringLiteral "" ])
  match t with
  | AST.TString -> Ok src
  | AST.TInt64 -> Ok(parseInt src)
  | AST.TBool -> Ok(AST.BinOp(AST.Eq, src, AST.StringLiteral "true"))
  | AST.TUnit -> Ok(AST.Let(rv, src, AST.UnitLiteral))
  | AST.TSum("Stdlib.Option.Option", [ inner ]) ->
    // Match, not If, and the payload bound to a var first -- for the same two reasons as
    // the custom-type decoders. (a) An If in atom position needs BOTH branches eagerly
    // safe, and `body` can be an If itself (the TList decoder is `if wire == "" then []
    // else map …`), which is illegal as a constructor ARG. (b) An If would also decode
    // the payload on the None path, where there is no payload. Match arms are lazy.
    let sv = "__uo" + string d
    unmarshalTypedR (d + 1) inner (unesc (slice r (AST.Int64Literal 5L) (len r)))
    |> Result.map (fun body ->
      AST.Let(
        rv,
        src,
        AST.Match(
          starts r "Some\n",
          [ { AST.Patterns = AST.NonEmptyList.singleton (AST.PBool true)
              AST.Guard = None
              AST.Body = AST.Let(sv, body, AST.Constructor(optT, "Some", Some(AST.Var sv))) }
            { AST.Patterns = AST.NonEmptyList.singleton AST.PWildcard
              AST.Guard = None
              AST.Body = AST.Constructor(optT, "None", None) } ])))
  | AST.TSum("Stdlib.Result.Result", [ okT; errT ]) ->
    match
      unmarshalTypedR (d + 1) errT (unesc (slice r (AST.Int64Literal 6L) (len r))),
      unmarshalTypedR (d + 1) okT (unesc (slice r (AST.Int64Literal 3L) (len r)))
      with
    | Ok errBody, Ok okBody ->
      // Match + bound payloads, same reasoning as the Option case above.
      let ev2 = "__ure" + string d
      let ov2 = "__uro" + string d
      Ok(
        AST.Let(
          rv,
          src,
          AST.Match(
            starts r "Error\n",
            [ { AST.Patterns = AST.NonEmptyList.singleton (AST.PBool true)
                AST.Guard = None
                AST.Body = AST.Let(ev2, errBody, AST.Constructor(resT, "Error", Some(AST.Var ev2))) }
              { AST.Patterns = AST.NonEmptyList.singleton AST.PWildcard
                AST.Guard = None
                AST.Body = AST.Let(ov2, okBody, AST.Constructor(resT, "Ok", Some(AST.Var ov2))) } ])))
    | Error e, _ | _, Error e -> Error e
  | AST.TList inner ->
    let ev = "__rpce_" + string d
    unmarshalTypedR (d + 1) inner (unesc (AST.Var ev))
    |> Result.map (fun body ->
      let lam = AST.Lambda(AST.NonEmptyList.fromList [ (ev, AST.TString) ], body)
      let split = AST.Call("Stdlib.String.split", AST.NonEmptyList.fromList [ r; AST.StringLiteral "\n" ])
      AST.Let(
        rv,
        src,
        AST.If(
          AST.BinOp(AST.Eq, r, AST.StringLiteral ""),
          AST.ListLiteral [],
          AST.TypeApp("Stdlib.List.map", [ AST.TString; inner ], AST.NonEmptyList.fromList [ split; lam ]))))
  // The inverse of marshalTypedSeen's TFloat64 case: the wire is "hi:lo", the two 32-bit
  // halves of the IEEE-754 bit pattern. Recombine (hi << 32 + lo) and reinterpret.
  // Addition, not BitOr, and Int64 throughout: the halves are disjoint so `+` and `|`
  // agree, and this stays clear of both codegen bugs (unsigned compare, and the
  // sign-extending AND imm32).
  | AST.TFloat64 ->
    let fv = "__uf" + string d
    let hiE = parseInt (nth (AST.Var fv) 0)
    let loE = parseInt (nth (AST.Var fv) 1)
    Ok(
      AST.Let(
        fv,
        AST.Call("Stdlib.String.split", AST.NonEmptyList.fromList [ src; AST.StringLiteral ":" ]),
        AST.Call(
          "Stdlib.Float.fromBits",
          AST.NonEmptyList.singleton (
            AST.BinOp(AST.Add, AST.BinOp(AST.Shl, hiE, AST.Int64Literal 32L), loE)))))
  // The inverse of marshalTypedSeen's TTuple case: each slot escaped, joined by "\n".
  // Without this, ANY type containing a tuple failed to decode -- and since a failed
  // decoder is silently dropped by unmarshalFnDefsFor while the CALL to it is still
  // emitted, that surfaced as a dangling "no variable named __unmarshal.T.<hash>".
  // Json's `Object of List<(String * Json)>` is exactly that shape.
  | AST.TTuple elemTs ->
    let tv = "__ut" + string d
    elemTs
    |> List.mapi (fun i et ->
      unmarshalTypedR (d + 1) et (unesc (nth (AST.Var tv) i)) |> Result.map (fun b -> ($"__utp{d}_{i}", b)))
    |> allOk
    |> Result.map (fun parts ->
      let tup = AST.TupleLiteral(parts |> List.map (fst >> AST.Var))
      let bound = parts |> List.rev |> List.fold (fun acc (n, b) -> AST.Let(n, b, acc)) tup
      AST.Let(tv, split src, bound))
  // The exact inverse of marshalTypedSeen's TBytes case: the wire is the byte values
  // one per line, so reuse the TList decoder (which already maps "" -> []) and rebuild
  // with the native Bytes.fromList; the empty wire lands on Bytes.create(0).
  | AST.TBytes ->
    unmarshalTypedR d (AST.TList AST.TInt64) src
    |> Result.map (fun l -> AST.Call("Stdlib.Bytes.fromList", AST.NonEmptyList.singleton l))
  // A non-generic custom type decodes via its emitted fn -- one definition, called
  // wherever needed, so recursion terminates. `inFnBody` is the single place we expand
  // inline: while generating that fn's own body.
  | (AST.TRecord(name, []) | AST.TSum(name, [])) when
    not inFnBody && (match Map.tryFind name defs with Some _ -> true | None -> false) ->
    // Only emit the CALL if the DEF will actually generate. unmarshalFnDefsFor drops a
    // failing decoder (`| Error _ -> None`) while this call site emitted the call anyway,
    // so the program died with a dangling "no variable named __unmarshal.T.<hash>" — an
    // error that names the symbol and hides the cause. It cost two debug cycles to learn
    // it meant "Json contains a Float and floats don't decode". Verify here instead, so
    // the real reason propagates as a normal blocker.
    //
    // `seen` terminates the recursion: while generating T's own body, a nested T is
    // already being generated, so take the call without re-verifying.
    if Set.contains name seen then
      Ok(AST.Call(unmarshalFnName name, AST.NonEmptyList.singleton src))
    else
      let selfT =
        match Map.tryFind name defs with
        | Some(AST.SumTypeDef _) -> AST.TSum(name, [])
        | _ -> AST.TRecord(name, [])
      // `seen`, NOT `Set.add name seen` — mirror how unmarshalFnDefsFor generates the
      // body (with an empty seen). Adding the name first makes the record/sum case's own
      // recursion guard fire immediately and report every type as "recursive". The body's
      // `inner` adds the name for its CHILDREN, so a nested self-reference still lands on
      // the call case above and terminates.
      match unmarshalTypedSeen true seen defs 0 selfT (AST.Var "s") with
      | Ok _ -> Ok(AST.Call(unmarshalFnName name, AST.NonEmptyList.singleton src))
      | Error e -> Error $"{name}: {e}"
  // A record: fields were escaped individually and joined by "\n" in NAME order, so each
  // field occupies exactly one line. Split, unescape, decode positionally, rebuild.
  | AST.TRecord(name, targs) | AST.TSum(name, targs) when
    (match Map.tryFind name defs with
     | Some(AST.RecordDef _) -> true
     | _ -> false) ->
    if Set.contains name seen then Error $"unmarshal: recursive {name}" else
    match Map.tryFind name defs with
    | Some(AST.RecordDef(_, tps, fields)) ->
      let sub = defSubstB tps targs
      let pv = "__up" + string d
      let inner d t v = unmarshalTypedSeen false (Set.add name seen) defs d t v
      fields
      |> List.sortBy fst
      |> List.mapi (fun i (fname, ft) ->
        inner (d + 1) (substTVarsB sub ft) (unesc (nth (AST.Var pv) i))
        |> Result.map (fun body -> (fname, $"__uf{d}_{i}", body)))
      |> allOk
      |> Result.map (fun decodedFields ->
        // bind every field, then build the record from vars -- record fields are atom
        // positions too, and a field decoder can be an If (see the variant case).
        let rec2 =
          AST.RecordLiteral(name, decodedFields |> List.map (fun (fn2, vn, _) -> (fn2, AST.Var vn)))
        let bound =
          decodedFields
          |> List.rev
          |> List.fold (fun acc (_, vn, b) -> AST.Let(vn, b, acc)) rec2
        AST.Let(pv, split src, bound))
    | _ -> Error $"unmarshal: no record def {name}"
  // A user enum: the wire is `Case` or `Case\n<escaped fields>`. Split off the first
  // line as the case name, then dispatch. The final `else` is the last variant: the wire
  // is well-formed by construction (dvalToWire wrote it), and an if-chain has to produce
  // a value of the type on every path.
  | AST.TSum(name, targs) | AST.TRecord(name, targs) ->
    if Set.contains name seen then Error $"unmarshal: recursive {name}" else
    match Map.tryFind name defs with
    | Some(AST.SumTypeDef(_, tps, variants)) ->
      let sub = defSubstB tps targs
      let inner d t v = unmarshalTypedSeen false (Set.add name seen) defs d t v
      let sv = "__us" + string d
      let iv = "__ui" + string d
      let cv = "__uc" + string d
      let lv = "__ul" + string d
      let rv2 = "__ur" + string d
      let sVar = AST.Var sv
      let iVar = AST.Var iv
      let restVar = AST.Var rv2
      let decodeVariant (variant : AST.Variant) : Result<AST.Expr, string> =
        match variant.Payload with
        | None -> Ok(AST.Constructor(name, variant.Name, None))
        | Some(AST.TTuple elemTs0) ->
          // multi-field: each field escaped separately, so split the payload too
          let elemTs = elemTs0 |> List.map (substTVarsB sub)
          let rpv = "__urp" + string d
          elemTs
          |> List.mapi (fun i et ->
            inner (d + 1) et (unesc (nth (AST.Var rpv) i)) |> Result.map (fun b -> ($"__uvt{d}_{i}", b)))
          |> allOk
          |> Result.map (fun parts ->
            // bind each slot, then build the tuple from vars (atom positions)
            let tup = AST.TupleLiteral(parts |> List.map (fst >> AST.Var))
            let bound =
              parts
              |> List.rev
              |> List.fold (fun acc (n, b) -> AST.Let(n, b, acc)) (AST.Constructor(name, variant.Name, Some tup))
            AST.Let(rpv, split restVar, bound))
        | Some pt0 ->
          // Hoist the decoded payload into a Let so the Constructor ARG is a plain var.
          // A decoder body can be an If (the TList case is `if wire == "" then [] else
          // map …`), and an If in ATOM position — which a constructor arg is — needs both
          // branches eagerly safe. Let-value position is lowered lazily, so binding first
          // sidesteps it. Same reason the record/tuple cases below bind every part.
          let pv = "__uv" + string d
          inner (d + 1) (substTVarsB sub pt0) (unesc restVar)
          |> Result.map (fun body ->
            AST.Let(pv, body, AST.Constructor(name, variant.Name, Some(AST.Var pv))))
      variants
      |> List.map (fun v -> decodeVariant v |> Result.map (fun e -> (v.Name, e)))
      |> allOk
      |> Result.bind (fun decoded ->
        match List.rev decoded with
        | [] -> Error $"unmarshal: no variants for {name}"
        | (_, lastBody) :: revRest ->
          // MATCH on the case name, not an if-chain. An `If` in atom position is lowered
          // to IfValue only when BOTH branches are safe to evaluate eagerly, and these
          // branches call __unmarshal — so the ANF pass rightly refused ("If expression
          // requires lazy branch lowering"). Eagerly decoding every variant would be
          // wrong anyway, and would never terminate for a recursive type. Match arms are
          // lazy, which is also why the marshal side uses Match. The wildcard arm is the
          // last variant: the wire is well-formed by construction (dvalToWire wrote it),
          // and every arm must produce a value of the type.
          let arms =
            (revRest
             |> List.rev
             |> List.map (fun (vname, body) ->
               { AST.Patterns = AST.NonEmptyList.singleton (AST.PString vname)
                 AST.Guard = None
                 AST.Body = body }))
            @ [ { AST.Patterns = AST.NonEmptyList.singleton AST.PWildcard
                  AST.Guard = None
                  AST.Body = lastBody } ]
          let chain = AST.Match(AST.Var cv, arms)
          // No `If` anywhere: an If in atom position needs both branches eagerly safe,
          // and every branch here would hold a `slice` Call. Default indexOf to LENGTH
          // instead of -1 and the branches vanish -- slice clamps, so with no newline
          // slice(s,0,len) is the whole string and slice(s,len+1,len) is "".
          Ok(
            AST.Let(
              sv,
              src,
              AST.Let(
                lv,
                AST.Call("Stdlib.String.length", AST.NonEmptyList.singleton sVar),
                AST.Let(
                  iv,
                  AST.TypeApp(
                    "Stdlib.Option.withDefault",
                    [ AST.TInt64 ],
                    AST.NonEmptyList.fromList
                      [ AST.Call("Stdlib.String.indexOf", AST.NonEmptyList.fromList [ sVar; AST.StringLiteral "\n" ])
                        AST.Var lv ]),
                  AST.Let(
                    cv,
                    AST.Call("Stdlib.String.slice", AST.NonEmptyList.fromList [ sVar; AST.Int64Literal 0L; iVar ]),
                    AST.Let(
                      rv2,
                      AST.Call(
                        "Stdlib.String.slice",
                        AST.NonEmptyList.fromList
                          [ sVar; AST.BinOp(AST.Add, iVar, AST.Int64Literal 1L); AST.Var lv ]),
                      chain)))))))
    | _ -> Error $"unmarshal: no sum def {name}"
  | other -> Error $"unmarshalable-return: {other}"

let unmarshalTypedR (d : int) (t : AST.Type) (src : AST.Expr) : Result<AST.Expr, string> =
  unmarshalTypedSeen false Set.empty Map.empty d t src

/// Same shape as unmarshalTypedR but with the type env, so custom types decode.
let unmarshalTypedD (defs : Map<string, AST.TypeDef>) (d : int) (t : AST.Type) (src : AST.Expr) =
  unmarshalTypedSeen false Set.empty defs d t src

/// Which unmarshallers does this expression call? Mirror of marshalCallsInExpr, and for
/// the same reason: emitting one per type puts a `__unmarshal.T` into every program, and
/// a single one that fails to typecheck breaks programs that decode nothing (that cost
/// 11 of 60 sampled fns when the marshal side did it). Emit only what's reachable.
let rec unmarshalCallsInExpr (e : AST.Expr) : Set<string> =
  let r = unmarshalCallsInExpr
  let many xs = xs |> List.map r |> Set.unionMany
  match e with
  | AST.Call(n, args) ->
    let here =
      if n.StartsWith "__unmarshal." then Set.singleton (n.Substring 12) else Set.empty
    Set.union here (many (AST.NonEmptyList.toList args))
  | AST.TypeApp(_, _, args) -> many (AST.NonEmptyList.toList args)
  | AST.Apply(f, args) -> Set.union (r f) (many (AST.NonEmptyList.toList args))
  | AST.Let(_, v, b) -> Set.union (r v) (r b)
  | AST.If(c, t, e2) -> Set.unionMany [ r c; r t; r e2 ]
  | AST.BinOp(_, a, b) -> Set.union (r a) (r b)
  | AST.UnaryOp(_, a) -> r a
  | AST.Lambda(_, b) -> r b
  | AST.Match(v, cases) -> Set.union (r v) (cases |> List.map (fun c -> r c.Body) |> Set.unionMany)
  | AST.ListLiteral xs -> many xs
  | AST.TupleLiteral xs -> many xs
  | AST.TupleAccess(v, _) -> r v
  | AST.RecordLiteral(_, fields) -> many (fields |> List.map snd)
  | AST.RecordAccess(v, _) -> r v
  | AST.RecordUpdate(v, ups) -> Set.union (r v) (many (ups |> List.map snd))
  | AST.Constructor(_, _, Some p) -> r p
  | AST.InterpolatedString _ -> Set.empty // interpolations can't contain an unmarshaller call
  | _ -> Set.empty

/// One `__unmarshal.<T>(s: String) : T` per NON-GENERIC custom type in `needed`.
/// Generic types are skipped for the same reason the marshal side skips them: they'd
/// need a decoding fn per type param (higher-order), which doesn't exist yet.
let unmarshalFnDefsFor (defs : Map<string, AST.TypeDef>) (needed : Set<string>) : List<AST.FunctionDef> =
  defs
  |> Map.toList
  |> List.filter (fun (name, _) -> Set.contains name needed)
  |> List.choose (fun (name, def) ->
    let tps =
      match def with
      | AST.RecordDef(_, tps, _) -> Some tps
      | AST.SumTypeDef(_, tps, _) -> Some tps
      | AST.TypeAlias _ -> None
    match tps with
    | Some [] ->
      let selfT =
        match def with
        | AST.SumTypeDef _ -> AST.TSum(name, [])
        | _ -> AST.TRecord(name, [])
      match unmarshalTypedSeen true Set.empty defs 0 selfT (AST.Var "s") with
      | Ok body ->
        Some(
          { Name = unmarshalFnName name
            TypeParams = []
            Params = AST.NonEmptyList.singleton ("s", AST.TString)
            ReturnType = selfT
            Body = body } : AST.FunctionDef
        )
      | Error _ -> None
    | _ -> None)

/// Transitively close the set: an unmarshaller's body may call other unmarshallers.
let unmarshalFnDefs (defs : Map<string, AST.TypeDef>) (seed : Set<string>) : List<AST.FunctionDef> =
  let mutable needed = seed
  let mutable changed = true
  while changed do
    let fns = unmarshalFnDefsFor defs needed
    let more =
      fns |> List.map (fun f -> unmarshalCallsInExpr f.Body) |> Set.unionMany |> Set.union needed
    changed <- more <> needed
    needed <- more
  unmarshalFnDefsFor defs needed

/// Unmarshal a host-RPC wire response into a native value per its return wire
/// type. Scalars are decoded inline; every container routes through the recursive
/// unmarshalTypedR over the bridged return type, which hard-fails rather than guess.
let private unmarshalReturn
  (defs : Map<string, AST.TypeDef>)
  (ret : WireRet)
  (call : AST.Expr)
  : Result<AST.Expr, string> =
  match ret with
  | WRString -> Ok call
  | WRInt -> Ok(AST.Call("Stdlib.hostRpcParseInt", AST.NonEmptyList.singleton call))
  | WRBool -> Ok(AST.BinOp(AST.Eq, call, AST.StringLiteral "true"))
  | WRUnit -> Ok(AST.Let("__rpc_ignore", call, AST.UnitLiteral))
  | WRTyped t -> unmarshalTypedD defs 0 t call

let rec bridgeExpr (ctx : BridgeCtx) (e : PT.Expr) : Result<AST.Expr, string> =
  let recurse = bridgeExpr ctx
  match e with
  | PT.EInt64(_, n) -> Ok(AST.Int64Literal n)
  | PT.EInt8(_, n) -> Ok(AST.Int8Literal n)
  | PT.EUInt8(_, n) -> Ok(AST.UInt8Literal n)
  | PT.EInt16(_, n) -> Ok(AST.Int16Literal n)
  | PT.EUInt16(_, n) -> Ok(AST.UInt16Literal n)
  | PT.EInt32(_, n) -> Ok(AST.Int32Literal n)
  | PT.EUInt32(_, n) -> Ok(AST.UInt32Literal n)
  | PT.EUInt64(_, n) -> Ok(AST.UInt64Literal n)
  | PT.EInt128(_, n) -> Ok(AST.Int128Literal n)
  | PT.EUInt128(_, n) -> Ok(AST.UInt128Literal n)
  | PT.EInt(_, big) ->
    if big >= bigint System.Int64.MinValue && big <= bigint System.Int64.MaxValue then
      Ok(AST.Int64Literal(int64 big))
    else
      err "literal" "Int outside Int64 range"
  | PT.EBool(_, b) -> Ok(AST.BoolLiteral b)
  | PT.EFloat(_, sign, whole, fraction) ->
    // Same construction the interpreter uses (Prelude.makeFloat), so a compiled
    // float literal is bit-identical to the interpreted one.
    Ok(AST.FloatLiteral(makeFloat sign whole fraction))
  | PT.EUnit _ -> Ok AST.UnitLiteral
  | PT.EString(_, [ PT.StringText s ]) -> Ok(AST.StringLiteral s)
  | PT.EString(_, segments) ->
    segments
    |> List.map (fun seg ->
      match seg with
      | PT.StringText s -> Ok(AST.StringText s)
      | PT.StringInterpolation e -> recurse e |> Result.map AST.StringExpr)
    |> allOk
    |> Result.map AST.InterpolatedString
  | PT.EChar(_, c) -> Ok(AST.CharLiteral c)
  | PT.EVariable(_, name) -> Ok(AST.Var name)
  | PT.EArg(_, i) ->
    if i >= 0 && i < ctx.Params.Length then Ok(AST.Var ctx.Params[i])
    else err "arg" $"EArg index {i} out of range"
  | PT.EInfix(_, op, l, r) ->
    bridgeInfix op
    |> Result.bind (fun o ->
      recurse l
      |> Result.bind (fun bl -> recurse r |> Result.map (fun br -> AST.BinOp(o, bl, br))))
  | PT.EIf(_, cond, thenE, Some elseE) ->
    recurse cond
    |> Result.bind (fun c ->
      recurse thenE
      |> Result.bind (fun t -> recurse elseE |> Result.map (fun el -> AST.If(c, t, el))))
  // `if cond then e` (no else) is exactly `if cond then e else ()`. Verified against the
  // interpreter rather than assumed: `let f (b: Bool) : Unit = if b then printLine "x"`
  // returns unit for BOTH b=true (branch runs) and b=false (branch skipped). The
  // then-branch is Unit-typed or the fn wouldn't typecheck, so an implicit unit else is
  // type-correct as well as semantically right.
  | PT.EIf(_, cond, thenE, None) ->
    recurse cond
    |> Result.bind (fun c -> recurse thenE |> Result.map (fun t -> AST.If(c, t, AST.UnitLiteral)))
  | PT.ELet(_, pat, value, body) ->
    recurse value
    |> Result.bind (fun v -> recurse body |> Result.bind (fun b -> bindPattern pat v b))
  // Records (type args, if any, are inferred by the compiler's monomorphizer)
  | PT.ERecord(_, nr, _, fields) ->
    typeHash nr
    |> Result.bind (fun h ->
      // Hoisting costs TYPE PROPAGATION: a record-field position pushes the field's
      // declared type into the expression, a Let does not, so a hoisted field infers
      // independently and its fresh tvar no longer unifies. Measured, not guessed —
      // hoisting every field regressed Stachu.Parser.many ("expected
      // T.830ffbde<List<a>>, got T.830ffbde<List<t>>"), and hoisting every CALL field
      // still regressed Cli.Packages.Deprecate.parseArgs ("expected (String, List<t>),
      // got (String, List<String>)"). Same tvar-vs-unification trap this branch keeps
      // hitting.
      //
      // So apply the workaround ONLY to the shape that actually miscompiles: a record
      // whose def has BOTH a List field and a Bytes field (task #26 — such a record
      // SIGSEGVs when the list is non-empty and the blob is >=2 bytes). Every other
      // record is left exactly as it was, which is why this costs 0 regressions.
      let needsHoist =
        match Map.tryFind (nameForType h) ctx.TypeDefs with
        | Some(AST.RecordDef(_, _, defFields)) ->
          let isL = defFields |> List.exists (fun (_, t) -> match t with AST.TList _ -> true | _ -> false)
          let isB = defFields |> List.exists (fun (_, t) -> match t with AST.TBytes -> true | _ -> false)
          isL && isB
        | _ -> false
      fields
      |> List.mapi (fun i (fname, fexpr) ->
        recurse fexpr
        |> Result.map (fun be ->
          // Only a CALL can clobber (it allocates and burns registers); literals are
          // harmless inline and keep the type context they need.
          match be with
          | (AST.Call _ | AST.TypeApp _ | AST.Apply _) when needsHoist -> (fname, Some $"__rf{i}_", be)
          | _ -> (fname, None, be)))
      |> allOk
      |> Result.map (fun fs ->
        // Bind call-shaped fields to a Let, then build the record from vars, rather than
        // inlining them.
        //
        // A RecordLiteral holding a CALL field next to a LIST field MISCOMPILES to a
        // SIGSEGV. Proven minimal (task #26): Http.Request { headers: List<String*String>;
        // body: Blob } -> headers=[] is fine at any blob size, a 1-byte blob is fine with
        // headers, but a NON-EMPTY list + a >=2-BYTE blob segfaults. It's size, not
        // content: "xy" crashes, "x" doesn't. Two heap objects built inline in one record
        // literal, and one clobbers the other; hoisting each into a Let fixes it, which is
        // what proved the diagnosis.
        //
        // This is a WORKAROUND, not the fix — the codegen bug is still there for anyone
        // else emitting a RecordLiteral. But it's the same discipline the decoders needed
        // (keep calls out of inline positions), it costs nothing, and it makes real Dark
        // code that builds such a record stop crashing.
        let rec2 =
          AST.RecordLiteral(
            nameForType h,
            fs
            |> List.map (fun (n, vn, e) ->
              match vn with
              | Some v -> (n, AST.Var v)
              | None -> (n, e)))
        fs
        |> List.rev
        |> List.fold
          (fun acc (_, vn, e) ->
            match vn with
            | Some v -> AST.Let(v, e, acc)
            | None -> acc)
          rec2))
  | PT.ERecordFieldAccess(_, record, fieldName) ->
    recurse record |> Result.map (fun r -> AST.RecordAccess(r, fieldName))
  | PT.ERecordUpdate(_, record, updates) ->
    recurse record
    |> Result.bind (fun r ->
      updates
      |> NEList.toList
      |> List.map (fun (fname, fexpr) -> recurse fexpr |> Result.map (fun be -> (fname, be)))
      |> allOk
      |> Result.map (fun ups -> AST.RecordUpdate(r, ups)))
  // Enum construction
  | PT.EEnum(_, nr, _, caseName, fields) ->
    typeHash nr
    |> Result.bind (fun h ->
      fields
      |> List.map recurse
      |> allOk
      |> Result.map (fun fs -> AST.Constructor(compilerTypeName h, caseName, tuplePayload fs)))
  // Cross-fn calls: a direct call to a package fn lowers to AST.Call, or
  // AST.TypeApp when the call site carries type args (the compiler monomorphizes
  // it). The callee itself is bridged separately (the builtin walks the graph).
  | PT.EApply(_, PT.EFnName(_, nr), typeArgs, args) ->
    match nr.resolved with
    | Error _ -> err "call" "unresolved fn name"
    | Ok resolved ->
      match resolved.name with
      | PT.FQFnName.Builtin b ->
        // Pure builtins route to the compiler's native stdlib (dispatch row 1).
        match Map.tryFind b.name builtinToStdlib with
        | Some stdlibFn ->
          args
          |> NEList.toList
          |> List.map recurse
          |> allOk
          |> Result.map (fun bas -> AST.Call(stdlibFn, AST.NonEmptyList.fromList bas))
        | None ->
          // Effectful builtins route through the host RPC seam (Stdlib.hostRpc):
          // request = "name\narg0\narg1…" (all args String), result per the
          // builtin's return type.
          match Map.tryFind b.name ctx.Effectful with
          | Some(argWires, wireRet) ->
            args
            |> NEList.toList
            |> List.map recurse
            |> allOk
            |> Result.bind (fun bas ->
              if List.length bas <> List.length argWires then
                err "builtin" $"{b.name}: arg count mismatch"
              else
                // stringify each arg per its wire type
                let marshaledR =
                  List.map2
                    (fun w a ->
                      match w with
                      | WAString -> Ok a
                      | WAInt -> Ok(AST.Call("Stdlib.Int64.toString", AST.NonEmptyList.singleton a))
                      | WABool -> Ok(AST.Call("Stdlib.Bool.toString", AST.NonEmptyList.singleton a))
                      // "hi:lo" bit halves, same encoding and same reasons as
                      // marshalTypedSeen's TFloat64 (exactness, and dodging the signed
                      // UInt64.toString bug). This is the ARG direction, where the old
                      // lossy rendering meant a Float param handed to a real builtin
                      // through the seam arrived rounded to 12 significant digits (and,
                      // for magnitudes past Float.toInt's range, as garbage).
                      | WAFloat ->
                        let bits = AST.Call("Stdlib.Float.toBits", AST.NonEmptyList.singleton a)
                        let u64 e = AST.Call("Stdlib.UInt64.toString", AST.NonEmptyList.singleton e)
                        let b = AST.Var "__rpc_fb"
                        let hi = AST.BinOp(AST.Shr, b, AST.UInt64Literal 32UL)
                        // lo by subtraction, not a 0xFFFFFFFF mask -- see marshalTypedSeen.
                        let lo = AST.BinOp(AST.Sub, b, AST.BinOp(AST.Shl, hi, AST.UInt64Literal 32UL))
                        Ok(
                          AST.Let(
                            "__rpc_fb",
                            bits,
                            AST.BinOp(
                              AST.StringConcat,
                              AST.BinOp(AST.StringConcat, u64 hi, AST.StringLiteral ":"),
                              u64 lo)))
                      | WAUnit -> Ok(AST.Let("__rpc_unit", a, AST.StringLiteral "unit"))
                      // A container/custom type: encode it exactly as the daemon's
                      // wireToDvalTyped decodes. If it CAN'T be encoded (a recursive
                      // type like Json needs a recursive serializer fn, which doesn't
                      // exist yet), hard-fail the call. Falling back to the raw value
                      // silently emits wrong code — it sent a Json where the seam
                      // expects a String.
                      | WATyped t -> marshalTyped ctx.TypeDefs 0 t a)
                    argWires
                    bas
                match allOk marshaledR with
                | Error e -> err "builtin" $"{b.name}: arg {e}"
                | Ok marshaled ->
                // Each arg is ESCAPED before being joined with "\n". The daemon splits the
                // request on "\n" to recover args, so an arg that CONTAINS a newline used
                // to become several args -- and dispatchBuiltin truncates to the param
                // count, so the extra lines were dropped on the floor. Every multi-line
                // arg was therefore silently mutilated:
                //   AltJson.format(Json.Bool true) -> wire "Bool\ntrue" -> the daemon saw
                //   just "Bool" -> "ERR:exn:Invalid Json". Only nullary cases (one line)
                //   worked.
                // It fails loudly for enums only by luck; a List arg ("a\nb\nc") truncates
                // to "a" and decodes as a VALID one-element list -- silently wrong, which
                // is the Directory.list bug over again in the arg direction. A String arg
                // containing a newline had the same flaw.
                let request =
                  (AST.StringLiteral b.name, marshaled)
                  ||> List.fold (fun acc arg ->
                    AST.BinOp(
                      AST.StringConcat,
                      AST.BinOp(AST.StringConcat, acc, AST.StringLiteral "\n"),
                      AST.Call("Stdlib.hostRpcEscape", AST.NonEmptyList.singleton arg)))
                let call = AST.Call("Stdlib.hostRpc", AST.NonEmptyList.singleton request)
                // A return type the decoder can't handle is a BLOCKER, reported like any
                // other, not a silently-wrong value. This is what lets buildEffectfulMap
                // stop pre-gating returns on isMarshalableType: that map is built from the
                // runtime and has no type env, whereas the failure is precise here.
                unmarshalReturn ctx.TypeDefs wireRet call
                |> Result.mapError (fun e -> $"unsupported-builtin: {b.name}: {e}"))
          | None -> err "builtin" $"{b.name}_v{b.version}"
      | PT.FQFnName.Package(PT.Hash h) ->
        let bridgedArgs = args |> NEList.toList |> List.map recurse |> allOk
        let bridgedTypeArgs = typeArgs |> List.map (bridgeType Map.empty)
        // NB: type args reference only TVar/prims here; a custom type in a type
        // arg would need the type env (rare) — falls through as an error then.
        match allOk bridgedTypeArgs, bridgedArgs with
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Ok tas, Ok bas ->
          // A stdlib fn with a REALLY-equivalent native impl calls the native fn
          // directly (its Dark def isn't bridged). Emit a bare Call so the compiler
          // infers the native fn's type args (dotted name -> module-scoped).
          let callName = routeResolved resolved |> Option.defaultValue (nameFor h)
          if List.isEmpty tas || Option.isSome (routeResolved resolved) then
            Ok(AST.Call(callName, AST.NonEmptyList.fromList bas))
          else
            Ok(AST.TypeApp(callName, tas, AST.NonEmptyList.fromList bas))
  // Self-recursion: `self(args)` calls the current fn by its compiled name.
  | PT.EApply(_, PT.ESelf _, _, args) ->
    args
    |> NEList.toList
    |> List.map recurse
    |> allOk
    |> Result.map (fun bas -> AST.Call(ctx.Self, AST.NonEmptyList.fromList bas))
  // A referenced package constant is inlined (its bridged body, from ctx.Values).
  | PT.EValue(_, nr) ->
    match nr.resolved with
    | Error _ -> err "value" "unresolved value name"
    | Ok resolved ->
      match resolved.name with
      | PT.FQValueName.Builtin b -> err "value-builtin" b.name
      | PT.FQValueName.Package(PT.Hash h) ->
        if Set.contains h ctx.Values then
          // Call the shared nullary fn rather than splicing the value's body in here.
          // The Unit arg is how the compiler spells a nullary call.
          Ok(AST.Call(valueFnName h, AST.NonEmptyList.singleton AST.UnitLiteral))
        else
          // Report why the value itself couldn't be bridged, not just that it's absent.
          match Map.tryFind h ctx.ValueErrors with
          | Some inner -> err "value" $"package value did not bridge: {inner}"
          | None -> err "value" "package value not resolved"
  | PT.ESelf _ -> err "expr" "bare ESelf (self as a value)"
  // Higher-order application: apply a fn value (variable/lambda/expr) to args.
  | PT.EApply(_, funcExpr, typeArgs, args) ->
    if not (List.isEmpty typeArgs) then
      err "generics" "type args on higher-order apply"
    else
      recurse funcExpr
      |> Result.bind (fun f ->
        args
        |> NEList.toList
        |> List.map recurse
        |> allOk
        |> Result.map (fun bas -> AST.Apply(f, AST.NonEmptyList.fromList bas)))
  | PT.EMatch(_, arg, cases) ->
    recurse arg
    |> Result.bind (fun scrut ->
      cases
      |> List.map (bridgeCase ctx)
      |> allOk
      |> Result.map (fun cs -> AST.Match(scrut, cs)))
  | PT.EList(_, elems) -> elems |> List.map recurse |> allOk |> Result.map AST.ListLiteral
  // The compiler AST has no dict literal, so build one with Stdlib.Dict.fromList over
  // a list of (key, value) tuples: the list literal pins k=String and v, so the call
  // needs no explicit type args. An EMPTY dict literal pins nothing, and inference
  // would have to come from context we don't have here, so it hard-fails cleanly
  // rather than emitting a Dict<String, ?> the compiler can't monomorphize.
  // An empty dict literal pins no types, so emit a bare nullary Dict.empty() and let
  // the compiler infer k/v from context (the Unit arg is how it spells a nullary
  // call). If context doesn't pin them it fails there instead — still better than
  // hard-failing here, since most empty dicts ARE used somewhere that pins them.
  | PT.EDict(_, []) ->
    Ok(AST.Call("Stdlib.Dict.empty", AST.NonEmptyList.singleton AST.UnitLiteral))
  | PT.EDict(_, pairs) ->
    pairs
    |> List.map (fun (k, v) ->
      recurse v |> Result.map (fun v' -> AST.TupleLiteral [ AST.StringLiteral k; v' ]))
    |> allOk
    |> Result.map (fun entries ->
      AST.Call("Stdlib.Dict.fromList", AST.NonEmptyList.singleton (AST.ListLiteral entries)))
  | PT.ETuple(_, a, b, rest) ->
    (a :: b :: rest) |> List.map recurse |> allOk |> Result.map AST.TupleLiteral
  | PT.EPipe(_, lhs, parts) ->
    recurse lhs
    |> Result.bind (fun start ->
      (Ok start, parts)
      ||> List.fold (fun accR part ->
        accR |> Result.bind (fun acc -> bridgePipePart ctx acc part)))
  // Lambda: untyped Dark params get a fresh TVar (unique per node id) that the
  // compiler's inference/monomorphizer resolves from the call context. A
  // non-variable param (e.g. `fun (a, b) -> …`) binds a fresh name and the body
  // destructures it via bindPattern.
  | PT.ELambda(lamId, pats, body) ->
    let named =
      pats
      |> NEList.toList
      |> List.mapi (fun i p ->
        match p with
        | PT.LPVariable(_, name) -> (name, None)
        | _ -> ($"__lamarg_{lamId}_{i}", Some p))
    let compilerParams =
      named |> List.map (fun (nm, _) -> (nm, AST.TVar $"__lamt_{lamId}_{nm}"))
    recurse body
    |> Result.bind (fun b ->
      // wrap the body with a destructuring bind for each non-variable param
      (Ok b, named)
      ||> List.fold (fun accR (nm, patOpt) ->
        match patOpt with
        | None -> accR
        | Some pat -> accR |> Result.bind (fun acc -> bindPattern pat (AST.Var nm) acc))
      |> Result.map (fun wb -> AST.Lambda(AST.NonEmptyList.fromList compilerParams, wb)))
  | PT.EStatement(_, first, next) ->
    // `first; next` — evaluate first, discard, then next.
    recurse first
    |> Result.bind (fun f -> recurse next |> Result.map (fun n -> AST.Let("__dark_stmt", f, n)))
  // A bare function reference (passed as a value, e.g. `List.map(xs, someFn)`)
  // lowers to a captureless Closure — the compiler's first-class function value.
  // A package fn references its bridged name; a pure builtin references the native
  // stdlib fn it routes to. Effectful builtins have no direct function value (they
  // need the hostRpc wrapper), so a bare reference to one hard-fails.
  | PT.EFnName(_, nr) ->
    match nr.resolved with
    | Error _ -> err "fnref" "unresolved fn name"
    | Ok resolved ->
      match resolved.name with
      // FuncRef, not a captureless Closure: the typechecker treats a Closure's FIRST
      // param as the captured environment and drops it (`TFunction(restParams, ret)`),
      // which is wrong for a plain top-level fn — it produced "Type mismatch in
      // closure fn.<hash>". FuncRef types as the fn's full signature.
      // A stdlib fn we route natively has no bridged def to point at, so its function
      // VALUE must name the native fn too (referencedPackageFns skips routed fns, so
      // a ref to fn.<hash> here would dangle).
      | PT.FQFnName.Package(PT.Hash h) ->
        match routeResolved resolved with
        | Some nativeName -> Ok(AST.FuncRef nativeName)
        | None -> Ok(AST.FuncRef(nameFor h))
      | PT.FQFnName.Builtin b ->
        match Map.tryFind b.name builtinToStdlib with
        | Some stdlibFn -> Ok(AST.FuncRef stdlibFn)
        | None -> err "fnref" $"effectful builtin as value: {b.name}"
  | _ -> err "expr" (e.GetType().Name)

/// Lower one match case. A PT case has a single pattern (alternatives live in
/// MPOr); the compiler groups alternatives in MatchCase.Patterns.
and bridgeCase
  (ctx : BridgeCtx)
  (c : PT.MatchCase)
  : Result<AST.MatchCase, string> =
  let patterns =
    match c.pat with
    | PT.MPOr(_, alts) -> alts |> NEList.toList |> List.map bridgePattern |> allOk
    | p -> bridgePattern p |> Result.map (fun x -> [ x ])
  patterns
  |> Result.bind (fun pats ->
    let guard =
      match c.whenCondition with
      | None -> Ok None
      | Some g -> bridgeExpr ctx g |> Result.map Some
    guard
    |> Result.bind (fun gd ->
      bridgeExpr ctx c.rhs
      |> Result.map (fun body ->
        ({ Patterns = AST.NonEmptyList.fromList pats
           Guard = gd
           Body = body }
        : AST.MatchCase))))

/// Desugar one pipe stage: the accumulated value `piped` becomes the stage's
/// first input. `x |> f a` -> f(x, a); `x |> (+) a` -> x + a; `x |> Some` ->
/// Some x; `x |> v a` -> v(x, a) (v is a fn value). Lambda stages need lambdas.
and bridgePipePart
  (ctx : BridgeCtx)
  (piped : AST.Expr)
  (part : PT.PipeExpr)
  : Result<AST.Expr, string> =
  let recurse = bridgeExpr ctx
  match part with
  | PT.EPipeInfix(_, op, rhs) ->
    bridgeInfix op
    |> Result.bind (fun o -> recurse rhs |> Result.map (fun r -> AST.BinOp(o, piped, r)))
  | PT.EPipeFnCall(_, nr, typeArgs, args) ->
    match nr.resolved with
    | Error _ -> err "call" "unresolved fn name (pipe)"
    | Ok resolved ->
      match resolved.name with
      | PT.FQFnName.Builtin b ->
        match Map.tryFind b.name builtinToStdlib with
        | Some stdlibFn ->
          args
          |> List.map recurse
          |> allOk
          |> Result.map (fun bas ->
            AST.Call(stdlibFn, AST.NonEmptyList.fromList (piped :: bas)))
        | None -> err "builtin" $"{b.name}_v{b.version}"
      | PT.FQFnName.Package(PT.Hash h) ->
        let bargs = args |> List.map recurse |> allOk
        let tas = typeArgs |> List.map (bridgeType Map.empty) |> allOk
        match tas, bargs with
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Ok tl, Ok bas ->
          let full = piped :: bas
          let callName = routeResolved resolved |> Option.defaultValue (nameFor h)
          if List.isEmpty tl || Option.isSome (routeResolved resolved) then
            Ok(AST.Call(callName, AST.NonEmptyList.fromList full))
          else Ok(AST.TypeApp(callName, tl, AST.NonEmptyList.fromList full))
  | PT.EPipeEnum(_, nr, caseName, fields) ->
    typeHash nr
    |> Result.bind (fun h ->
      fields
      |> List.map recurse
      |> allOk
      |> Result.map (fun fs ->
        AST.Constructor(nameForType h, caseName, tuplePayload (piped :: fs))))
  | PT.EPipeVariable(_, varName, args) ->
    args
    |> List.map recurse
    |> allOk
    |> Result.map (fun bas ->
      AST.Apply(AST.Var varName, AST.NonEmptyList.fromList (piped :: bas)))
  // `x |> fun p -> body` beta-reduces to binding p = x in body.
  | PT.EPipeLambda(_, pats, body) ->
    match NEList.toList pats with
    | [ single ] -> recurse body |> Result.bind (fun b -> bindPattern single piped b)
    | _ -> err "pipe" "multi-param pipe lambda"

// ---------------------------------------------------------------------------
// Type definitions
// ---------------------------------------------------------------------------

/// Lower a package type (non-generic record/enum) to a compiler TypeDef under
/// the given compiler-side name.
let bridgeTypeDef
  (types : Map<string, TypeEntry>)
  (name : string)
  (pt : PT.PackageType.PackageType)
  : Result<AST.TypeDef, string> =
  let d = pt.declaration
  let tps = d.typeParams
  match d.definition with
  | PT.TypeDeclaration.Alias _ -> err "type" "type alias"
  | PT.TypeDeclaration.Record fields ->
    fields
    |> NEList.toList
    |> List.map (fun (f : PT.TypeDeclaration.RecordField) ->
      bridgeType types f.typ |> Result.map (fun t -> (f.name, t)))
    |> allOk
    |> Result.map (fun fs -> AST.RecordDef(name, tps, fs))
  | PT.TypeDeclaration.Enum cases ->
    cases
    |> NEList.toList
    |> List.map (fun (c : PT.TypeDeclaration.EnumCase) ->
      c.fields
      |> List.map (fun (ef : PT.TypeDeclaration.EnumField) -> bridgeType types ef.typ)
      |> allOk
      |> Result.map (fun fieldTypes ->
        let payload =
          match fieldTypes with
          | [] -> None
          | [ t ] -> Some t
          | ts -> Some(AST.TTuple ts)
        ({ Name = c.name; Payload = payload } : AST.Variant)))
    |> allOk
    |> Result.map (fun variants -> AST.SumTypeDef(name, tps, variants))

// ---------------------------------------------------------------------------
// Functions
// ---------------------------------------------------------------------------

/// Every type variable appearing in a compiler type. Darklang signatures carry
/// free type vars (e.g. `'v`) that PT's `typeParams` list doesn't always
/// enumerate; the compiler FunctionDef must declare all of them or the reachability
/// harness can't monomorphize (no zero literal for an undeclared TVar).
let rec freeTVars (t : AST.Type) : Set<string> =
  match t with
  | AST.TVar v -> Set.singleton v
  | AST.TRecord(_, args)
  | AST.TSum(_, args) -> args |> List.map freeTVars |> Set.unionMany
  | AST.TList inner -> freeTVars inner
  | AST.TTuple ts -> ts |> List.map freeTVars |> Set.unionMany
  | AST.TDict(k, v) -> Set.union (freeTVars k) (freeTVars v)
  | AST.TFunction(args, ret) ->
    Set.union (args |> List.map freeTVars |> Set.unionMany) (freeTVars ret)
  | _ -> Set.empty

/// Lower a package function to a compiler FunctionDef under the given
/// compiler-side name. Params keep their Dark names (positional EArg refs and
/// any by-name refs both resolve). Generics are not yet supported.
let bridgeFn
  (types : Map<string, TypeEntry>)
  (typeDefs : Map<string, AST.TypeDef>)
  (values : Set<string>)
  (valueErrors : Map<string, string>)
  (effectful : Map<string, WireArg list * WireRet>)
  (compiledName : string)
  (fn : PT.PackageFn.PackageFn)
  : Result<AST.FunctionDef, string> =
  let paramList = NEList.toList fn.parameters
  let paramNames = paramList |> List.map (fun p -> p.name) |> Array.ofList
  let ctx =
    { Params = paramNames
      Self = compiledName
      Values = values
      ValueErrors = valueErrors
      Effectful = effectful
      TypeDefs = typeDefs }
  let bridgedParams =
    paramList
    |> List.map (fun p -> bridgeType types p.typ |> Result.map (fun t -> (p.name, t)))
    |> allOk
  bridgedParams
  |> Result.bind (fun ps ->
    bridgeType types fn.returnType
    |> Result.bind (fun retType ->
      bridgeExpr ctx fn.body
      |> Result.map (fun body ->
        // Declare every type var the signature actually uses — declared params
        // first (preserving order), then any free tvars PT didn't enumerate.
        let sigTVars =
          (ps |> List.map (snd >> freeTVars) |> Set.unionMany)
          |> Set.union (freeTVars retType)
        let declared = fn.typeParams
        let extra =
          sigTVars |> Set.toList |> List.filter (fun v -> not (List.contains v declared))
        ({ Name = compiledName
           TypeParams = declared @ extra
           Params = AST.NonEmptyList.fromList ps
           ReturnType = retType
           Body = body } : AST.FunctionDef))))

// ---------------------------------------------------------------------------
// Reference collectors (for the transitive fetch closures in the builtin)
// ---------------------------------------------------------------------------

/// The package-fn hashes a body calls directly (over the supported subset).
let rec referencedPackageFns (e : PT.Expr) : List<string> =
  let r = referencedPackageFns
  match e with
  | PT.EInfix(_, _, l, rhs) -> r l @ r rhs
  | PT.EIf(_, c, t, Some el) -> r c @ r t @ r el
  | PT.EIf(_, c, t, None) -> r c @ r t
  | PT.ELet(_, _, v, body) -> r v @ r body
  | PT.ERecord(_, _, _, fields) -> fields |> List.collect (snd >> r)
  | PT.ERecordFieldAccess(_, record, _) -> r record
  | PT.EEnum(_, _, _, _, fields) -> fields |> List.collect r
  | PT.EMatch(_, arg, cases) ->
    r arg
    @ (cases
       |> List.collect (fun c ->
         r c.rhs @ (match c.whenCondition with Some g -> r g | None -> [])))
  | PT.EApply(_, PT.EFnName(_, nr), _, args) ->
    let here =
      match nr.resolved with
      // A routed stdlib fn is served by the compiler's native impl, so its Dark
      // definition is never bridged — don't pull it into the compile closure.
      | Ok resolved when Option.isSome (routeResolved resolved) -> []
      | Ok resolved ->
        match resolved.name with
        | PT.FQFnName.Package(PT.Hash h) -> [ h ]
        | PT.FQFnName.Builtin _ -> []
      | Error _ -> []
    here @ (args |> NEList.toList |> List.collect r)
  // A fn used as a VALUE (passed to a higher-order fn, not called here) still needs
  // its definition compiled in: the bridge lowers it to a captureless Closure over
  // the bridged name. Only the CALL-position case above existed, so a bare fn-ref fell
  // through to `| _ -> []`, its def was never fetched, and the Closure dangled —
  // "Undefined variable: fn.<hash>" (45 fns). Unlike package VALUES, fns are emitted
  // once as FunctionDefs and shared by name, so pulling these in costs nothing.
  | PT.EFnName(_, nr) ->
    match nr.resolved with
    | Ok resolved when Option.isSome (routeResolved resolved) -> []
    | Ok resolved ->
      match resolved.name with
      | PT.FQFnName.Package(PT.Hash h) -> [ h ]
      | PT.FQFnName.Builtin _ -> []
    | Error _ -> []
  | PT.EString(_, segs) ->
    segs |> List.collect (fun s -> match s with PT.StringInterpolation e -> r e | PT.StringText _ -> [])
  | PT.ERecordUpdate(_, record, ups) ->
    r record @ (ups |> NEList.toList |> List.collect (snd >> r))
  | PT.EPipe(_, lhs, parts) ->
    r lhs
    @ (parts
       |> List.collect (fun p ->
         match p with
         | PT.EPipeFnCall(_, nr, _, args) ->
           (match nr.resolved with
            | Ok res when Option.isSome (routeResolved res) -> []
            | Ok res ->
              match res.name with
              | PT.FQFnName.Package(PT.Hash h) -> [ h ]
              | PT.FQFnName.Builtin _ -> []
            | Error _ -> [])
           @ (args |> List.collect r)
         | PT.EPipeInfix(_, _, e) -> r e
         | PT.EPipeEnum(_, _, _, fields) -> fields |> List.collect r
         | PT.EPipeVariable(_, _, args) -> args |> List.collect r
         | PT.EPipeLambda(_, _, body) -> r body))
  | PT.EList(_, elems) -> elems |> List.collect r
  | PT.EDict(_, pairs) -> pairs |> List.collect (snd >> r)
  | PT.ETuple(_, a, b, rest) -> (a :: b :: rest) |> List.collect r
  | PT.ELambda(_, _, body) -> r body
  | PT.EStatement(_, first, next) -> r first @ r next
  | PT.EApply(_, f, _, args) -> r f @ (args |> NEList.toList |> List.collect r)
  | _ -> []

/// The custom-type hashes a TypeReference mentions.
let rec typeRefsInType (t : PT.TypeReference) : List<string> =
  let r = typeRefsInType
  match t with
  | PT.TCustomType(nr, args) -> typeHashOpt nr @ List.collect r args
  | PT.TList inner -> r inner
  | PT.TTuple(a, b, rest) -> r a @ r b @ List.collect r rest
  | PT.TDict inner -> r inner
  | PT.TFn(args, ret) -> (NEList.toList args |> List.collect r) @ r ret
  | _ -> []

/// The custom-type hashes a body mentions (record/enum constructions).
let rec typeRefsInExpr (e : PT.Expr) : List<string> =
  let r = typeRefsInExpr
  match e with
  | PT.ERecord(_, nr, _, fields) -> typeHashOpt nr @ (fields |> List.collect (snd >> r))
  | PT.EEnum(_, nr, _, _, fields) -> typeHashOpt nr @ (fields |> List.collect r)
  | PT.ERecordFieldAccess(_, record, _) -> r record
  | PT.EInfix(_, _, l, rhs) -> r l @ r rhs
  | PT.EIf(_, c, t, Some el) -> r c @ r t @ r el
  | PT.EIf(_, c, t, None) -> r c @ r t
  | PT.ELet(_, _, v, body) -> r v @ r body
  | PT.EMatch(_, arg, cases) ->
    r arg
    @ (cases
       |> List.collect (fun c ->
         r c.rhs @ (match c.whenCondition with Some g -> r g | None -> [])))
  | PT.EString(_, segs) ->
    segs |> List.collect (fun s -> match s with PT.StringInterpolation e -> r e | PT.StringText _ -> [])
  | PT.ERecordUpdate(_, record, ups) ->
    r record @ (ups |> NEList.toList |> List.collect (snd >> r))
  | PT.EPipe(_, lhs, parts) ->
    r lhs
    @ (parts
       |> List.collect (fun p ->
         match p with
         | PT.EPipeFnCall(_, nr, _, args) ->
           (match nr.resolved with
            | Ok res when Option.isSome (routeResolved res) -> []
            | Ok res ->
              match res.name with
              | PT.FQFnName.Package(PT.Hash h) -> [ h ]
              | PT.FQFnName.Builtin _ -> []
            | Error _ -> [])
           @ (args |> List.collect r)
         | PT.EPipeInfix(_, _, e) -> r e
         | PT.EPipeEnum(_, _, _, fields) -> fields |> List.collect r
         | PT.EPipeVariable(_, _, args) -> args |> List.collect r
         | PT.EPipeLambda(_, _, body) -> r body))
  | PT.EList(_, elems) -> elems |> List.collect r
  | PT.EDict(_, pairs) -> pairs |> List.collect (snd >> r)
  | PT.ETuple(_, a, b, rest) -> (a :: b :: rest) |> List.collect r
  | PT.ELambda(_, _, body) -> r body
  | PT.EStatement(_, first, next) -> r first @ r next
  | PT.EApply(_, f, _, args) -> r f @ (args |> NEList.toList |> List.collect r)
  | _ -> []

/// The custom-type hashes a whole package fn mentions (signature + body).
let typeRefsInFn (fn : PT.PackageFn.PackageFn) : List<string> =
  (fn.parameters |> NEList.toList |> List.collect (fun p -> typeRefsInType p.typ))
  @ typeRefsInType fn.returnType
  @ typeRefsInExpr fn.body

/// The custom-type hashes a type definition mentions (field/case types).
let typeRefsInTypeDef (pt : PT.PackageType.PackageType) : List<string> =
  match pt.declaration.definition with
  | PT.TypeDeclaration.Alias t -> typeRefsInType t
  | PT.TypeDeclaration.Record fields ->
    fields |> NEList.toList |> List.collect (fun f -> typeRefsInType f.typ)
  | PT.TypeDeclaration.Enum cases ->
    cases
    |> NEList.toList
    |> List.collect (fun c -> c.fields |> List.collect (fun ef -> typeRefsInType ef.typ))

/// The package-value hashes an expression references (for the value closure).
let rec valueRefsInExpr (e : PT.Expr) : List<string> =
  let r = valueRefsInExpr
  match e with
  // Values are SHARED nullary fns now (see valueFnName), not inlined, so walking
  // pipes here is finally safe. It is also required: referencedPackageFns walks
  // pipes and this must agree, or a package value used inside a pipe is never
  // seeded, never fetched, and every fn touching it fails "package value not
  // resolved" (~307 fns — the #1 blocker). While values were inlined, adding this
  // compounded value chains and hit 55GB on a 50-fn batch.
  | PT.EPipe(_, lhs, parts) ->
    r lhs
    @ (parts
       |> List.collect (fun p ->
         match p with
         | PT.EPipeFnCall(_, _, _, args) -> args |> List.collect r
         | PT.EPipeInfix(_, _, e) -> r e
         | PT.EPipeEnum(_, _, _, fields) -> fields |> List.collect r
         | PT.EPipeVariable(_, _, args) -> args |> List.collect r
         | PT.EPipeLambda(_, _, body) -> r body))
  | PT.EValue(_, nr) ->
    match nr.resolved with
    | Ok resolved ->
      match resolved.name with
      | PT.FQValueName.Package(PT.Hash h) -> [ h ]
      | PT.FQValueName.Builtin _ -> []
    | Error _ -> []
  | PT.EInfix(_, _, l, rhs) -> r l @ r rhs
  | PT.EIf(_, c, t, Some el) -> r c @ r t @ r el
  | PT.EIf(_, c, t, None) -> r c @ r t
  | PT.ELet(_, _, v, body) -> r v @ r body
  | PT.EList(_, elems) -> elems |> List.collect r
  | PT.EDict(_, pairs) -> pairs |> List.collect (snd >> r)
  | PT.ETuple(_, a, b, rest) -> (a :: b :: rest) |> List.collect r
  | PT.ERecord(_, _, _, fields) -> fields |> List.collect (snd >> r)
  | PT.ERecordFieldAccess(_, record, _) -> r record
  | PT.ERecordUpdate(_, record, ups) ->
    r record @ (ups |> NEList.toList |> List.collect (snd >> r))
  | PT.EEnum(_, _, _, _, fields) -> fields |> List.collect r
  | PT.EMatch(_, arg, cases) ->
    r arg @ (cases |> List.collect (fun c -> r c.rhs))
  | PT.EString(_, segs) ->
    segs |> List.collect (fun s -> match s with PT.StringInterpolation e -> r e | PT.StringText _ -> [])
  | PT.ELambda(_, _, body) -> r body
  | PT.EStatement(_, first, next) -> r first @ r next
  | PT.EApply(_, f, _, args) -> r f @ (args |> NEList.toList |> List.collect r)
  | _ -> []
