/// Operators the interpreter answers itself, without the builtin's record or the call machinery
/// around it. Two values of one builtin numeric type, two `String`s, or two `List`s -- every mixed
/// pair goes the ordinary way and gets the ordinary error.
///
/// Not a compiler change: the `Apply` still happens, so a real opcode emitted by `PT2RT` would win
/// more again. This is the part that needed no new instruction.
///
/// Only pure `noCaps` builtins belong here: this path skips the capability check and both type
/// checks, which is right for an operator whose operand types the caller has just matched, and wrong
/// for anything guarding a side effect. Every entry must also produce exactly what its builtin
/// produces -- by calling the same helper it calls, as every numeric operator calls `NumOps`, or
/// failing that by restating a one-line F# operator. Where the builtin would raise, the entry
/// declines and the ordinary path runs.
module LibExecution.FastOps

open Prelude
open RuntimeTypes
module VT = ValueType

let add = NumOps.add
let subtract = NumOps.subtract
let lessThan = NumOps.lessThan
let lessThanOrEqualTo = NumOps.lessThanOrEqualTo
let greaterThan = NumOps.greaterThan
let greaterThanOrEqualTo = NumOps.greaterThanOrEqualTo
let equals = 6
let notEquals = 7
/// String tags start here. `eval` and `eval64` handle the integer ones and decline these;
/// `evalStr` does the reverse. So the tag alone says which operand types an operator wants, and
/// no function needs a range check -- `equals` and `notEquals` are simply handled by all of them.
let strAppend = 8
let listConcat = 9
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
let listContains = 16
let strIsEmpty = 18
let listIsEmpty = 19
/// `Multiply.multiply`; only reached as a trait method. This and the rest of the numeric
/// operators' tags are `NumOps`' own numbers for them.
let multiply = NumOps.multiply
/// `Negate.negate`, one argument; `evalUnary` handles it.
let negate = NumOps.negate
/// The bitwise operators. `NumOps` answers them for the integers and `BigInt`.
/// Being in the table is also what tells the interpreter an operator is an operator: it is how
/// `1L & 2y` says "Cannot perform numeric operation on Int64 and Int8" rather than
/// failing inside the implementation's parameter check.
let bitwiseAnd = NumOps.bitwiseAnd
let bitwiseOr = NumOps.bitwiseOr
let bitwiseXor = NumOps.bitwiseXor
let bitwiseNot = NumOps.bitwiseNot
let shiftLeft = NumOps.shiftLeft
let shiftRight = NumOps.shiftRight
/// `List.prepend`: a list and a value of its element type.
let listPrepend = 28
/// `/`, `%` and `**`. Not in `traitTag`, because they can fail; `quickTraitTag` has them, and
/// `NumOps` answers the inputs where they cannot. The highest tags, so the drain can send them
/// straight to `evalNumbers`.
let divide = NumOps.divide
let modulo = NumOps.modulo
let power = NumOps.power

/// A `NumOps` answer, for a table that declines whatever it cannot answer: an error is the
/// impl's to raise.
let inline private ofOutcome (outcome : NumOps.Outcome) : Dval voption =
  match outcome with
  | NumOps.Value v -> ValueSome v
  | NumOps.Raises _
  | NumOps.Unsupported -> ValueNone

/// A binary operator on two numbers of one builtin numeric type, from `NumOps`; anything else,
/// and whatever raises, declines.
let evalNumbers (tag : int) (a : Dval) (b : Dval) : Dval voption =
  NumOps.tryBinary tag a b

/// `-x` and `~x` on a builtin numeric, from `NumOps`; anything else declines.
let evalUnary (tag : int) (a : Dval) : Dval voption = ofOutcome (NumOps.unary tag a)

/// The operator itself, given a tag from `byName` and two `BigInt`s: `NumOps`' answer, and
/// `==` and `!=`.
let eval (tag : int) (a : DarkBigInt) (b : DarkBigInt) : Dval voption =
  // Structurally, which is what `equals` does for this case: `DarkBigInt` is `Finite` whenever the
  // value fits an int64, so equal values have equal representations.
  if tag = equals then ValueSome(Dval.bool (a = b))
  elif tag = notEquals then ValueSome(Dval.bool (a <> b))
  else ofOutcome (NumOps.bigIntOp tag a b)

/// The operator itself, given a tag from `byName` and two `Int64`s: `NumOps`' answer, and
/// `==` and `!=`.
let eval64 (tag : int) (a : int64) (b : int64) : Dval voption =
  if tag = equals then ValueSome(Dval.bool (a = b))
  elif tag = notEquals then ValueSome(Dval.bool (a <> b))
  else NumOps.int64Op ValueSome (fun _ -> ValueNone) ValueNone tag a b

/// The operator itself, for two `String`s. `stringAppend` reaches this because `"a" + "b"` is
/// the `Add` impl for String, which names `Stdlib.String.append`, a thin wrapper the elision
/// already resolves to its builtin; so it arrives here with the same shape an integer operator
/// does, rather than paying the full builtin path to concatenate two strings.
let evalStr (tag : int) (a : DarkString) (b : DarkString) : Dval voption =
  if tag = strAppend then
    ValueSome(DStr(DarkString.append a b))
  elif tag = equals then
    ValueSome(
      Dval.bool (
        System.String.Equals(a.Value, b.Value, System.StringComparison.Ordinal)
      )
    )
  elif tag = notEquals then
    ValueSome(
      Dval.bool (
        not (System.String.Equals(a.Value, b.Value, System.StringComparison.Ordinal))
      )
    )
  else
    ValueNone


/// `List.concat`, and the `@` operator that desugars to it, for two `List`s.
///
/// The builtin's own `DvalCreator.tryConcat`. Declines when the element types don't merge: the
/// builtin then names the element at fault.
let evalList
  (tag : int)
  (vt1 : ValueType)
  (l1 : DvalList)
  (vt2 : ValueType)
  (l2 : DvalList)
  : Dval voption =
  if tag = listConcat then
    TypeChecker.DvalCreator.tryConcat vt1 l1 vt2 l2
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
    | DInt64 n -> ValueSome(DString(Dval.int64ToString n))
    | DBigInt i -> ValueSome(DString(DarkBigInt.toString i))
    | _ -> ValueNone
  elif tag = boolNot then
    match arg with
    | DBool b -> ValueSome(Dval.bool (not b))
    | _ -> ValueNone
  elif tag = listLength then
    match arg with
    | DList(_, l) -> ValueSome(Dval.dint64 (int64 (DvalList.length l)))
    | _ -> ValueNone
  elif tag = strIsEmpty then
    match arg with
    | DString s -> ValueSome(Dval.bool (s.Length = 0))
    | _ -> ValueNone
  elif tag = listIsEmpty then
    match arg with
    | DList(_, l) -> ValueSome(Dval.bool (DvalList.isEmpty l))
    | _ -> ValueNone
  else
    ValueNone


/// `List.contains`: a list and a value of its element type.
///
/// The builtin's own `Dval.listContains`. Declines where it would raise, on an element whose type
/// does not merge with the value's.
let evalListContains (tag : int) (items : DvalList) (value : Dval) : Dval voption =
  if tag = listContains then
    match Dval.listContains items value with
    | ValueSome found -> ValueSome(Dval.bool found)
    | ValueNone -> ValueNone
  else
    ValueNone


/// `List.prepend`: <param value> on the front of a list whose element type is <param vt>.
///
/// The builtin's own `DvalCreator.tryPrepend`, without the general call path (argument array,
/// parameter checks, result check). Every list a fold builds comes through here once per element.
/// Declines when the types do not merge, so the builtin reports it.
let evalListPrepend
  (vt : ValueType)
  (items : DvalList)
  (value : Dval)
  : Dval voption =
  TypeChecker.DvalCreator.tryPrepend vt items value


/// `String.repeat`: a `String` and an `Int64` count, almost always padding a row out to a column.
///
/// Calls the same `String.repeat` the builtin calls, so the two cannot drift. Declines a count
/// that does not fit an `int32`, where the builtin raises `OutOfRange`: reproducing that error
/// here would be restating the builtin's failure behaviour as well as its success, and the slow
/// path already gets it right.
let evalStrInt (tag : int) (s : string) (n : int64) : Dval voption =
  if tag = strRepeat && n >= -1L && n <= 2147483647L then
    ValueSome(DString(String.repeat s (int n)))
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
  (o : DictMap)
  (k : Dval)
  : Dval voption =
  if tag = dictGet && Dval.isUsableDictKey k then
    ValueSome(
      DictMap.tryFind k o |> TypeChecker.DvalCreator.option threadID VT.unknownTODO
    )
  else
    ValueNone


/// `Dict.set`: the builtin's own `DvalCreator.tryDictAddEntry`. Declines where the builtin would
/// raise, so its error is the same with tracing on or off.
let evalDictSet
  (tag : int)
  (keyType : ValueType)
  (valueType : ValueType)
  (o : DictMap)
  (k : Dval)
  (v : Dval)
  : Dval voption =
  if tag = dictSet then
    TypeChecker.DvalCreator.tryDictAddEntry keyType valueType o k v
  else
    ValueNone


/// The operators on two values of one builtin numeric type, or two `String`s, for the trait method
/// the operator lowers to, so the operator never pays impl selection. Numbers take `NumOps`, which
/// is what their impls compute too, and String `+` is `String.append`, as its builtin is. Every
/// mixed pair declines, and so does whatever would raise: the impl runs and raises its own error.
let evalNumeric (tag : int) (a : Dval) (b : Dval) : Dval voption =
  match NumOps.tryBinary tag a b with
  | ValueSome _ as answer -> answer
  | ValueNone ->
    // Nested matches, not `match a, b with`: the pair is an allocation per operator, which is the
    // cost this table exists to avoid.
    if tag = add then
      match a with
      | DStr x ->
        (match b with
         | DStr y -> ValueSome(DStr(DarkString.append x y))
         | _ -> ValueNone)
      | _ -> ValueNone
    else
      ValueNone


/// The tag for a trait method, when the trait is one of the stdlib operator traits.
/// Rebuilt when the package refs reload, since the hashes move with the stdlib.
///
/// A reference tuple, not a struct one, so the generation and its table publish in ONE
/// store: as a struct a reader between the two stores could pair the new generation with
/// the old table. Same shape and same reason as `NumericTraits.cache`.
let mutable private traitTags : int * Dictionary<struct (string * string), int> =
  (-1, Dictionary())

/// The default comparer hashes all 64 characters of the trait's hash with Marvin, once per
/// operator. The method names of one trait differ in length, so the length is enough beside it.
let private traitKeyComparer =
  { new System.Collections.Generic.IEqualityComparer<struct (string * string)> with
      member _.Equals(struct (h1, m1), struct (h2, m2)) =
        System.String.Equals(h1, h2) && System.String.Equals(m1, m2)
      member _.GetHashCode(struct (h, m)) = contentHashCode h ^^^ m.Length }

let traitTag (traitHash : string) (methodName : string) : int voption =
  let gen = PackageRefs.currentGeneration ()
  let (tableGen, table) = traitTags
  let table =
    if gen = tableGen then
      table
    else
      let fresh = Dictionary<struct (string * string), int>(traitKeyComparer)
      let put (hash : string) (methodName : string) (tag : int) =
        if hash <> "" then fresh[struct (hash, methodName)] <- tag
      put (PackageRefs.Trait.Stdlib.Traits.add ()) "add" add
      put (PackageRefs.Trait.Stdlib.Traits.subtract ()) "subtract" subtract
      put (PackageRefs.Trait.Stdlib.Traits.multiply ()) "multiply" multiply
      put (PackageRefs.Trait.Stdlib.Traits.negate ()) "negate" negate
      put (PackageRefs.Trait.Stdlib.Traits.bitwiseAnd ()) "bitwiseAnd" bitwiseAnd
      put (PackageRefs.Trait.Stdlib.Traits.bitwiseOr ()) "bitwiseOr" bitwiseOr
      put (PackageRefs.Trait.Stdlib.Traits.bitwiseXor ()) "bitwiseXor" bitwiseXor
      put (PackageRefs.Trait.Stdlib.Traits.bitwiseNot ()) "bitwiseNot" bitwiseNot
      put (PackageRefs.Trait.Stdlib.Traits.shiftLeft ()) "shiftLeft" shiftLeft
      put (PackageRefs.Trait.Stdlib.Traits.shiftRight ()) "shiftRight" shiftRight
      put (PackageRefs.Trait.Stdlib.Traits.compare ()) "lessThan" lessThan
      put
        (PackageRefs.Trait.Stdlib.Traits.compare ())
        "lessThanOrEqualTo"
        lessThanOrEqualTo
      put (PackageRefs.Trait.Stdlib.Traits.compare ()) "greaterThan" greaterThan
      put
        (PackageRefs.Trait.Stdlib.Traits.compare ())
        "greaterThanOrEqualTo"
        greaterThanOrEqualTo
      traitTags <- (gen, fresh)
      fresh
  // Out-param, not the tuple form, which allocates per lookup (docs/perf/history.md):
  // this runs once per trait-method Apply.
  let mutable tag = 0
  if table.TryGetValue(struct (traitHash, methodName), &tag) then
    ValueSome tag
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
  put "listConcat" listConcat
  put "bigIntToString" intToString
  put "int64ToString" intToString
  put "boolNot" boolNot
  put "listLength" listLength
  put "dictGet" dictGet
  put "dictSet" dictSet
  put "stringRepeat" strRepeat
  put "listContains" listContains
  put "stringIsEmpty" strIsEmpty
  put "listIsEmpty" listIsEmpty
  put "listPrepend" listPrepend
  d


/// `traitTag`, and `/`, `%` and `**` too. For `Interpreter.quicken`, which looks a call up once
/// per function, not once per call. `traitTag` itself leaves them out because it decides more than
/// whether `FastOps` can answer: an operator in it reports a mixed pair of operands up front, and
/// `/`, `%` and `**` report theirs from the impl.
let quickTraitTag (traitHash : string) (methodName : string) : int voption =
  match traitTag traitHash methodName with
  | ValueSome tag -> ValueSome tag
  | ValueNone ->
    if
      methodName = "divide" && traitHash = PackageRefs.Trait.Stdlib.Traits.divide ()
    then
      ValueSome divide
    elif
      methodName = "modulo" && traitHash = PackageRefs.Trait.Stdlib.Traits.modulo ()
    then
      ValueSome modulo
    elif
      methodName = "power" && traitHash = PackageRefs.Trait.Stdlib.Traits.power ()
    then
      ValueSome power
    else
      ValueNone
