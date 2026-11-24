/// The stdlib traits the arithmetic and comparison operators are.
///
/// `a + b` is `Stdlib.Add.add a b`: the lowering emits the trait method, the checker
/// asks for an `Add` impl of the operand type. The trait hashes come from
/// `PackageRefs`, so a tree whose refs are not generated yet (CI before the first
/// reload) lowers to the polymorphic builtin.
module LibExecution.NumericTraits

open Prelude
open ProgramTypes

module Traits = PackageRefs.Trait.Stdlib.Traits

/// The trait and method an operator is. Every operator that is one, bitwise included:
/// "integer-only" is the set of types with a `BitwiseAnd` implementation.
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
  // `==` and `!=` are NOT traits: equality is structural, so both lower to their builtins.
  | ComparisonEquals -> None
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
/// generated.
let ofNegate () : Option<string * string> =
  let hash = Traits.negate ()
  if hash = "" then None else Some(hash, "negate")

/// `~x`, the same way: `BitwiseNot.bitwiseNot`.
let ofBitwiseNot () : Option<string * string> =
  let hash = Traits.bitwiseNot ()
  if hash = "" then None else Some(hash, "bitwiseNot")

/// Any of the three: what `pmOperatorTrait` answers for the at-rest checker.
let ofOperator (op : Operator) : Option<string * string> =
  match op with
  | OpInfix name -> ofInfix name
  | OpNegate -> ofNegate ()
  | OpBitwiseNot -> ofBitwiseNot ()
