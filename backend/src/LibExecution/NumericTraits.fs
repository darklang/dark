/// The stdlib traits the arithmetic and comparison operators are.
///
/// `a + b` is `Stdlib.Add.add a b`: the lowering emits the trait method, the checker
/// asks for an `Add` impl of the operand type, and the query compiler pushes it down
/// as the SQL operator the old polymorphic builtin carried. The trait hashes come from
/// `PackageRefs`, so a tree whose refs are not generated yet (CI before the first
/// reload) lowers to the polymorphic builtin exactly as before.
module LibExecution.NumericTraits

open Prelude
open ProgramTypes

module Traits = PackageRefs.Trait.Stdlib.Traits

/// The trait and method an operator is, when it is one. Bitwise operators, `++`,
/// `==` and `!=` stay on their builtins: bitwise is integer-only by nature, and
/// equality is structural over every type.
let ofInfix (op : InfixFnName) : Option<string * string> =
  let some (hash : string) (methodName : string) =
    if hash = "" then None else Some(hash, methodName)
  match op with
  | ArithmeticPlus -> some (Traits.add ()) "add"
  | ArithmeticMinus -> some (Traits.sub ()) "subtract"
  | ArithmeticMultiply -> some (Traits.mul ()) "multiply"
  | ArithmeticDivide -> some (Traits.div ()) "divide"
  | ArithmeticModulo -> some (Traits.mod' ()) "modulo"
  | ArithmeticPower -> some (Traits.pow ()) "power"
  | ComparisonLessThan -> some (Traits.ord ()) "lessThan"
  | ComparisonLessThanOrEqual -> some (Traits.ord ()) "lessThanOrEqualTo"
  | ComparisonGreaterThan -> some (Traits.ord ()) "greaterThan"
  | ComparisonGreaterThanOrEqual -> some (Traits.ord ()) "greaterThanOrEqualTo"
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ComparisonEquals
  | ComparisonNotEquals
  | StringConcat -> None

/// Unary minus on a non-literal (`-x`): `Neg.negate`, or None while the refs are
/// not generated. The parser stores `Builtin.negate` in the PT, so this is a
/// lowering-time swap and no hash moves.
let ofNegate () : Option<string * string> =
  let hash = Traits.neg ()
  if hash = "" then None else Some(hash, "negate")

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
    ComparisonGreaterThanOrEqual ]
  |> List.choose (fun op -> ofInfix op |> Option.map (fun t -> (op, t)))

/// The operator a trait method is, when it is one. Generation-checked because the
/// hashes move with the stdlib; the table is published whole and never written
/// after, so readers on other threads only ever see a finished one.
let mutable private cache
  : struct (int *
    System.Collections.Generic.Dictionary<struct (string * string), InfixFnName>) =
  struct (-1, System.Collections.Generic.Dictionary())

let tryInfix (traitHash : string) (methodName : string) : Option<InfixFnName> =
  let gen = PackageRefs.currentGeneration ()
  let struct (tableGen, table) = cache
  let table =
    if gen = tableGen then
      table
    else
      let fresh = System.Collections.Generic.Dictionary()
      for (op, (hash, m)) in all () do
        fresh[struct (hash, m)] <- op
      cache <- struct (gen, fresh)
      fresh
  match table.TryGetValue(struct (traitHash, methodName)) with
  | true, op -> Some op
  | false, _ -> None

/// The hashes of the operator traits, for loading their impls into a checker
/// environment: every `+` needs `Add`'s impls visible, and no item names `Add`.
let traitHashes () : List<FQTypeName.Package> =
  Traits.all () |> List.filter (fun h -> h <> "") |> List.map Hash
