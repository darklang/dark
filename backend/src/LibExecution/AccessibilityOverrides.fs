/// Builtin → "should be FromLocation, exactly 1 wrapper" registry.
///
/// Every BuiltInFn defaults to `accessibility = Any`. Names listed
/// here are upgraded to `FromLocation _` at `Builtin.combine` time.
/// The `Tests.Builtin.fromLocationBuiltinsAreSinglyReferenced` test
/// then enforces the "exactly 1 textual reference in packages/"
/// invariant.
///
/// To grow this list:
///   1. Run `scripts/linting/builtin-wrap-audit` for the 1-wrapper-each
///      candidates.
///   2. Append the obvious names below.
///   3. Run the test suite — anything that fails is either genuinely
///      multi-wrapped or audit-miscounted; investigate.
///
/// See `notes/wrap-up/accessibility-tightening-plan.md` for the full
/// tightening process + rationale.
module LibExecution.AccessibilityOverrides

/// Hash payload for FromLocation. The current test only inspects the
/// discriminator, so this is just a marker. A future richer test
/// could resolve it via the package manager.
let placeholderHash : RuntimeTypes.FQFnName.Package =
  RuntimeTypes.FQFnName.package "TIGHTENED-PLACEHOLDER"


/// Builtins whose accessibility upgrades from `Any` to `FromLocation`.
/// Starter set — all clearly-canonical Stdlib wrappers. See the
/// accessibility-tightening plan for the path to ~515.
let registry : Set<string> =
  Set.ofList
    [ // Bool
      "boolNot"

      // Dict
      "dictGet"
      "dictKeys"
      "dictMember"
      "dictRemove"
      "dictSet"
      "dictSize"
      "dictValues"

      // List
      "listLength"
      "listHead"

      // String
      "stringLength"
      "stringReverse"
      "stringSlice"
      "stringTrim"
      "stringTrimStart"
      "stringTrimEnd" ]
// Note: `stringIndexOf` looked canonical but it's called 3× from within
// stdlib/string.dark itself (different helper fns reusing the lookup).
// Either refactor those helpers to call the wrapper, or leave Any.
// Skipping for now.


let apply (fn : RuntimeTypes.BuiltInFn) : RuntimeTypes.BuiltInFn =
  if Set.contains fn.name.name registry then
    { fn with accessibility = RuntimeTypes.FromLocation placeholderHash }
  else
    fn
