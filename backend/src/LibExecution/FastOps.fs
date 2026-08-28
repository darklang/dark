/// Operators the interpreter answers itself, without the builtin's record or the call machinery
/// around it. Two `Int`s, two `String`s, or two `List`s -- every other pair, including every other
/// numeric type and every mixed one, goes the ordinary way and gets the ordinary error.
///
/// Not a compiler change: the `Apply` still happens, so a real opcode emitted by `PT2RT` would win
/// more again. This is the part that needed no new instruction.
///
/// Only pure `noCaps` builtins belong here: this path skips the capability check and both type
/// checks, which is right for an operator whose operand types the caller has just matched, and wrong
/// for anything guarding a side effect. Every entry must also produce exactly what its builtin
/// produces -- restating it, or calling the same helper it calls. Where that is not possible, or
/// where the builtin would raise on input this cannot check, the entry declines and the ordinary
/// path runs.
module LibExecution.FastOps

open Prelude
open RuntimeTypes
module VT = ValueType

let add = 0
let subtract = 1
let lessThan = 2
let lessThanOrEqualTo = 3
let greaterThan = 4
let greaterThanOrEqualTo = 5
let equals = 6
let notEquals = 7
/// String tags start here. `eval` handles the `Int` ones and declines these; `evalStr` does the
/// reverse. So the tag alone says which operand types an operator wants, and neither function
/// needs a range check -- `equals` and `notEquals` are simply handled by both.
let strAppend = 8
let listAppend = 9
/// One-argument tags. Same table: `eval1` handles these and the two-argument functions decline
/// them, so nothing needs to know an operator's arity before looking it up.
let intToString = 10
let boolNot = 11
let listLength = 12
/// `Dict` tags. Two of the busiest builtins in a view build, and the only entries here whose
/// operands are of different types.
let dictGet = 13
let dictSet = 14
let strRepeat = 15
let listMember = 16
let dictSetStrict = 17
let strIsEmpty = 18
let listIsEmpty = 19

/// The operator itself, given a tag from `byName` and two `Int`s.
let eval (tag : int) (a : DarkInt) (b : DarkInt) : Dval voption =
  if tag = add then
    ValueSome(Dval.dint (DarkInt.add a b))
  elif tag = subtract then
    ValueSome(Dval.dint (DarkInt.subtract a b))
  elif tag = lessThan then
    ValueSome(Dval.bool (DarkInt.compare a b < 0))
  elif tag = lessThanOrEqualTo then
    ValueSome(Dval.bool (DarkInt.compare a b <= 0))
  elif tag = greaterThan then
    ValueSome(Dval.bool (DarkInt.compare a b > 0))
  elif tag = greaterThanOrEqualTo then
    ValueSome(Dval.bool (DarkInt.compare a b >= 0))
  // Structurally, which is what `equals` does for this case: `DarkInt` is `Finite` whenever the
  // value fits an int64, so equal values have equal representations.
  elif tag = equals then
    ValueSome(Dval.bool (a = b))
  elif tag = notEquals then
    ValueSome(Dval.bool (a <> b))
  else
    ValueNone

/// The operator itself, for two `String`s. `stringAppend` reaches this because `++` is a thin
/// wrapper the elision already resolves to its builtin, so it arrives here with the same shape an
/// `Int` operator does -- and paid the full builtin path to concatenate two strings.
let evalStr (tag : int) (a : string) (b : string) : Dval voption =
  if tag = strAppend then
    // Exactly what the builtin computes, `normalize` included: two normalized strings can join
    // into one that is not, so this is not safe to drop.
    ValueSome(DString(String.normalize (a + b)))
  elif tag = equals then
    ValueSome(
      Dval.bool (System.String.Equals(a, b, System.StringComparison.Ordinal))
    )
  elif tag = notEquals then
    ValueSome(
      Dval.bool (not (System.String.Equals(a, b, System.StringComparison.Ordinal)))
    )
  else
    ValueNone


/// The operator itself, for two `List`s. `push` and `pushBack` are defined in terms of `List.append`,
/// so it carries far more traffic than its name suggests.
///
/// Declines when the element types don't merge, rather than reporting the error itself: the slow
/// path falls back to `DvalCreator.list` for a precise element-level message, and duplicating that
/// here would be reimplementing the builtin rather than short-circuiting it.
let evalList
  (tag : int)
  (vt1 : ValueType)
  (l1 : List<Dval>)
  (vt2 : ValueType)
  (l2 : List<Dval>)
  : Dval voption =
  if tag = listAppend then
    match ValueType.merge vt1 vt2 with
    | Ok merged -> ValueSome(DList(merged, List.append l1 l2))
    | Error() -> ValueNone
  else
    ValueNone


/// The operator itself, for one argument.
///
/// Only operators whose implementation can be restated exactly belong here -- these three are a
/// `not`, a length and a stringify. `cliTerminalStyledWidth` is busier than any of them and still
/// does not qualify: restating it would mean copying an escape-sequence walker into the interpreter,
/// which is duplicating a builtin rather than short-circuiting one.
let eval1 (tag : int) (arg : Dval) : Dval voption =
  if tag = intToString then
    match arg with
    | DInt i ->
      // The builtin goes through `Dval.asBigInt`, which allocates a `BigInteger` to stringify a
      // value that usually fits an int64. Same output either way.
      match i with
      | DarkInt.Finite n -> ValueSome(DString(string n))
      | DarkInt.Infinite b -> ValueSome(DString(string b))
    | _ -> ValueNone
  elif tag = boolNot then
    match arg with
    | DBool b -> ValueSome(Dval.bool (not b))
    | _ -> ValueNone
  elif tag = listLength then
    match arg with
    | DList(_, l) -> ValueSome(Dval.int (bigint l.Length))
    | _ -> ValueNone
  elif tag = strIsEmpty then
    match arg with
    | DString s -> ValueSome(Dval.bool (s.Length = 0))
    | _ -> ValueNone
  elif tag = listIsEmpty then
    match arg with
    | DList(_, l) -> ValueSome(Dval.bool (List.isEmpty l))
    | _ -> ValueNone
  else
    ValueNone


/// `List.member`: a list and a value of its element type.
///
/// Calls the same `Dval.equals` the builtin calls -- which is why that function moved into
/// `LibExecution` from `Builtins.Pure.Libs.NoModule`: structural equality of two Dvals belongs
/// beside `Dval`, and the interpreter could not reach it where it was.
///
/// Keeps `==`'s behaviour on mismatched element types, which is to raise rather than answer false,
/// by declining to the slow path when a merge fails.
let evalListMember (tag : int) (items : List<Dval>) (value : Dval) : Dval voption =
  if tag = listMember then
    let vtValue = Dval.toValueType value

    let rec search (rest : List<Dval>) : Dval voption =
      match rest with
      | [] -> ValueSome(Dval.bool false)
      | elem :: tail ->
        match ValueType.merge (Dval.toValueType elem) vtValue with
        | Error() -> ValueNone
        | Ok _ ->
          if Dval.equals elem value then ValueSome(Dval.bool true) else search tail

    search items
  else
    ValueNone


/// `String.repeat`: a `String` and an `Int` count, almost always padding a row out to a column.
///
/// Calls the same `String.repeat` the builtin now calls, so the two cannot drift. Declines a count
/// that does not fit an `int32`, where the builtin raises `OutOfRange` through `intToInt32`:
/// reproducing that error here would be restating the builtin's failure behaviour as well as its
/// success, and the slow path already gets it right.
let evalStrInt (tag : int) (s : string) (n : DarkInt) : Dval voption =
  if tag = strRepeat then
    match n with
    | DarkInt.Finite i when i >= -1L && i <= 2147483647L ->
      ValueSome(DString(String.repeat s (int i)))
    | _ -> ValueNone
  else
    ValueNone


/// `Dict.get`: a `Dict` and a `String` key. With `set` below, one of the two busiest builtins a
/// view build makes.
///
/// Calls the same `DvalCreator.option` the builtin does rather than restating enum construction.
/// That helper is in `LibExecution`, so this is short-circuiting the call machinery around a
/// builtin, not duplicating the builtin -- which is the line every entry in this table has to stay
/// on the right side of.
let evalDictGet
  (threadID : ThreadID)
  (tag : int)
  (o : Map<string, Dval>)
  (k : string)
  : Dval voption =
  if tag = dictGet then
    ValueSome(Map.find k o |> TypeChecker.DvalCreator.option threadID VT.unknownTODO)
  else
    ValueNone


/// `Dict.setOverridingDuplicates`: a `Dict`, a `String` key and a value. The only three-argument
/// operator here.
let evalDictSet
  (threadID : ThreadID)
  (tag : int)
  (vt : ValueType)
  (o : Map<string, Dval>)
  (k : string)
  (v : Dval)
  : Dval voption =
  if tag = dictSet || tag = dictSetStrict then
    // `Dict.set` raises on a key already present and `setOverridingDuplicates` does not, so the
    // strict one declines to the slow path when the key is there and lets the builtin raise.
    if tag = dictSetStrict && Map.containsKey k o then
      ValueNone
    else
      // Declines when the value would not merge, rather than letting `dictAddEntry` raise. The slow
      // path's *parameter* check catches a mismatched value first and reports it differently, and an
      // error message that depends on whether tracing happens to be on is worse than anything this
      // path is worth. Same reason `evalList` declines on a failed merge.
      match VT.merge vt (Dval.toValueType v) with
      | Ok _ ->
        let struct (typ, map) =
          TypeChecker.DvalCreator.dictAddEntry
            threadID
            vt
            o
            k
            v
            TypeChecker.ReplaceValue
        ValueSome(DDict(typ, map))
      | Error() -> ValueNone
  else
    ValueNone


/// Looked up by name once per call rather than matched as a string: `FQFnName.Builtin` is a small
/// record and this is a single probe of a table with ten entries in it.
///
/// Only pure `noCaps` builtins belong here. This path runs the operator without the builtin's
/// record, so the capability check, the argument type check and the result type check are all
/// skipped -- fine for operators whose operand types the match above has just established, and
/// wrong for anything that guards a side effect.
let byName : Dictionary<FQFnName.Builtin, int> =
  let d = Dictionary<FQFnName.Builtin, int>()
  let put (name : string) (tag : int) = d[{ name = name; version = 0 }] <- tag
  put "add" add
  put "subtract" subtract
  put "lessThan" lessThan
  put "lessThanOrEqualTo" lessThanOrEqualTo
  put "greaterThan" greaterThan
  put "greaterThanOrEqualTo" greaterThanOrEqualTo
  put "equals" equals
  put "notEquals" notEquals
  put "stringAppend" strAppend
  put "listAppend" listAppend
  put "intToString" intToString
  put "boolNot" boolNot
  put "listLength" listLength
  put "dictGet" dictGet
  put "dictSetOverridingDuplicates" dictSet
  put "dictSet" dictSetStrict
  put "stringRepeat" strRepeat
  put "listMember" listMember
  put "stringIsEmpty" strIsEmpty
  put "listIsEmpty" listIsEmpty
  d
