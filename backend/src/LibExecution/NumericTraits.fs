/// The stdlib traits the arithmetic and comparison operators are.
///
/// `a + b` is `Stdlib.Add.add a b`: the lowering emits the trait method, the checker
/// asks for an `Add` impl of the operand type, and the query compiler pushes it down
/// as its SQL operator. The trait hashes come from
/// `PackageRefs`, so a tree whose refs are not generated yet (CI before the first
/// reload) lowers to the polymorphic builtin.
module LibExecution.NumericTraits

open Prelude
open ProgramTypes

module Traits = PackageRefs.Trait.Stdlib.Traits

/// The trait and method an operator is. Every operator is one, bitwise included: "integer-only"
/// is the set of types with a `BitwiseAnd` implementation. `==` is `Equal.equals`, with a structural
/// fallback for a type that has no implementation, so every value stays comparable and the
/// builtin types are never overridden.
let ofInfix (op : InfixFnName) : Option<string * string> =
  let some (hash : string) (methodName : string) =
    if hash = "" then None else Some(hash, methodName)
  match op with
  | ArithmeticPlus -> some (Traits.add ()) "add"
  | ArithmeticMinus -> some (Traits.subtract ()) "subtract"
  | ArithmeticMultiply -> some (Traits.multiply ()) "multiply"
  | ArithmeticDivide -> some (Traits.divide ()) "divide"
  | ArithmeticModulo -> some (Traits.modulo ()) "modulo"
  | ArithmeticPower -> some (Traits.power ()) "power"
  | ComparisonLessThan -> some (Traits.compare ()) "lessThan"
  | ComparisonLessThanOrEqual -> some (Traits.compare ()) "lessThanOrEqualTo"
  | ComparisonGreaterThan -> some (Traits.compare ()) "greaterThan"
  | ComparisonGreaterThanOrEqual -> some (Traits.compare ()) "greaterThanOrEqualTo"
  // `==` is `Equal.equals`; `!=` is `not (Equal.equals a b)`, lowered as two calls.
  | ComparisonEquals -> some (Traits.equal ()) "equals"
  | ComparisonNotEquals -> None
  | BitwiseAnd -> some (Traits.bitwiseAnd ()) "bitwiseAnd"
  | BitwiseOr -> some (Traits.bitwiseOr ()) "bitwiseOr"
  | BitwiseXor -> some (Traits.bitwiseXor ()) "bitwiseXor"
  | ShiftLeft -> some (Traits.shiftLeft ()) "shiftLeft"
  | ShiftRight -> some (Traits.shiftRight ()) "shiftRight"
  // `++` is not in the language; the case exists to decode ops stored before it went, and
  // lowers to the string-append builtin.
  | StringConcat -> None

/// Unary minus on a non-literal (`-x`): `Negate.negate`, or None while the refs are not
/// generated. The parser stores `Builtin.negate` in the PT; this is the lowering-time swap.
let ofNegate () : Option<string * string> =
  let hash = Traits.negate ()
  if hash = "" then None else Some(hash, "negate")

/// `~x`, the same way: the parser stores `Builtin.bitwiseNot` and this is the swap.
let ofBitwiseNot () : Option<string * string> =
  let hash = Traits.bitwiseNot ()
  if hash = "" then None else Some(hash, "bitwiseNot")

/// Every operator that is a trait method, with its trait and method, under the
/// refs as they are now.
let private all () : List<InfixFnName * (string * string)> =
  [ ArithmeticPlus
    ArithmeticMinus
    ArithmeticMultiply
    ArithmeticDivide
    ArithmeticModulo
    ArithmeticPower
    ComparisonLessThan
    ComparisonLessThanOrEqual
    ComparisonGreaterThan
    ComparisonGreaterThanOrEqual
    ComparisonEquals
    BitwiseAnd
    BitwiseOr
    BitwiseXor
    ShiftLeft
    ShiftRight ]
  |> List.choose (fun op -> ofInfix op |> Option.map (fun t -> (op, t)))

/// The operator a trait method is, when it is one. Generation-checked because the
/// hashes move with the stdlib; the table is published whole and never written
/// after, so readers on other threads only ever see a finished one.
///
/// A reference tuple rather than a struct one, so the generation and the table it
/// belongs to are published in ONE store. As a struct it took two, and a reader
/// between them could pair the new generation with the old table and answer from it.
let mutable private cache
  : int *
    System.Collections.Generic.Dictionary<struct (string * string), InfixFnName> =
  (-1, System.Collections.Generic.Dictionary())

let tryInfix (traitHash : string) (methodName : string) : Option<InfixFnName> =
  let gen = PackageRefs.currentGeneration ()
  let (tableGen, table) = cache
  let table =
    if gen = tableGen then
      table
    else
      let fresh = System.Collections.Generic.Dictionary()
      for (op, (hash, m)) in all () do
        fresh[struct (hash, m)] <- op
      cache <- (gen, fresh)
      fresh
  match table.TryGetValue(struct (traitHash, methodName)) with
  | true, op -> Some op
  | false, _ -> None

/// The hashes of the operator traits, for loading their impls into a checker
/// environment: every `+` needs `Add`'s impls visible, and no item names `Add`.
let traitHashes () : List<FQTypeName.Package> =
  Traits.all () |> List.filter (fun h -> h <> "") |> List.map Hash
