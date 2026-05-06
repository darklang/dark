/// Builtin → "should be FromLocation, exactly 1 wrapper" registry.
///
/// Every BuiltInFn defaults to `accessibility = Any`. Names listed
/// here are upgraded to `FromLocation <PackageLocation>` at `Builtin.combine`
/// time. The `Tests.Builtin.fromLocationBuiltinsAreSinglyReferenced`
/// test then enforces the "exactly 1 textual reference in packages/"
/// invariant.
///
/// Locations are name-based (owner / modules / fn name) rather than
/// hash-based — the hash can shift across edits, but the canonical
/// wrapper's NAME is stable, and that's the assertion the registry
/// is making.
///
/// To grow this list:
///   1. Run `scripts/linting/builtin-wrap-audit` for the 1-wrapper-each
///      candidates.
///   2. Append the obvious names below with their wrapper Locations.
///   3. Run the test suite — anything that fails is either genuinely
///      multi-wrapped or audit-miscounted; investigate.
///
/// See `notes/wrap-up/accessibility-tightening-plan.md` for the full
/// tightening process + rationale.
module LibExecution.AccessibilityOverrides

open RuntimeTypes


/// Helper: build a PackageLocation under the canonical Darklang stdlib path.
let private stdlib (modules : List<string>) (name : string) : PackageLocation =
  { owner = "Darklang"
    modules = "Stdlib" :: modules
    name = name }


/// Builtin name → wrapper PackageLocation. Starter set; clearly-canonical
/// Stdlib wrappers. Path forward: append entries.
let registry : Map<string, PackageLocation> =
  Map.ofList
    [ // Bool
      "boolNot", stdlib [ "Bool" ] "not"

      // Dict
      "dictGet", stdlib [ "Dict" ] "get"
      "dictKeys", stdlib [ "Dict" ] "keys"
      "dictMember", stdlib [ "Dict" ] "member"
      "dictRemove", stdlib [ "Dict" ] "remove"
      "dictSet", stdlib [ "Dict" ] "set"
      "dictSize", stdlib [ "Dict" ] "size"
      "dictValues", stdlib [ "Dict" ] "values"

      // List
      "listLength", stdlib [ "List" ] "length"
      "listHead", stdlib [ "List" ] "head"

      // String
      "stringLength", stdlib [ "String" ] "length"
      "stringReverse", stdlib [ "String" ] "reverse"
      "stringSlice", stdlib [ "String" ] "slice"
      "stringTrim", stdlib [ "String" ] "trim"
      "stringTrimStart", stdlib [ "String" ] "trimStart"
      "stringTrimEnd", stdlib [ "String" ] "trimEnd" ]
// Note: `stringIndexOf` looked canonical but it's called 3× from within
// stdlib/string.dark itself (other helper fns reuse the lookup).
// Either refactor those helpers to call the wrapper, or leave Any.
// Skipping for now.


let apply (fn : BuiltInFn) : BuiltInFn =
  match Map.tryFind fn.name.name registry with
  | Some loc -> { fn with accessibility = FromLocation loc }
  | None -> fn
