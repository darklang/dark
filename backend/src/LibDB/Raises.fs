/// Whether a package fn can stop with a runtime error, and through which calls.
///
/// `Raise` flows through the call graph like any effect, but a few stdlib fns test for every
/// case where what they call raises. `view` and `effects` leave those out.
/// A `/` or `%` whose literal divisor rules out its raising is left out too.
module LibDB.Raises

open Prelude
open Fumble
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module Effects = LibExecution.Effects
module Requirements = LibExecution.CallGraph.Requirements

/// Stdlib fns that cannot raise, though they call something that declares `Raise`. The walk
/// cannot see why, so each kind is listed with its reason.
///
/// The numeric impls, for their own type: the polymorphic builtin they call declares `Raise` for
/// another (`Builtin.add` raises past Decimal's range). As `LibExecution.NumOps` has it, the
/// fixed-width integers wrap, BigInt grows, and Float and Float32 follow IEEE.
///
/// The checked functions, which test their arguments for every case where what they call raises,
/// and return an `Error` instead.
///
/// The clock: when its syscall fails, the runtime's own clock answers, and a reading is always in
/// `DateTime`'s range.
///
/// The date fields, such as `DateTime.month`: each divides by a nonzero constant, and what it
/// unwraps is always in the field's range.
let cannotRaise : Set<string> =
  let guarded =
    [ "Int64.checkedDivide"
      "Int64.checkedRemainder"
      "Int64.checkedMod"
      "BigInt.checkedShiftLeft"
      "BigInt.checkedShiftRight"
      "BigInt.checkedDivide"
      "BigInt.checkedRemainder"
      "BigInt.checkedMod"
      "String.checkedRepeat"
      "List.checkedRepeat"
      "List.checkedRange"
      "DateTime.parse"
      "DateTime.__now"
      "DateTime.__monotonicMilliseconds"
      "DateTime.month"
      "DateTime.day"
      "DateTime.weekday"
      "DateTime.hour"
      "DateTime.minute"
      "DateTime.second"
      "DateTime.millisecond" ]
    |> List.map (fun f -> $"Darklang.Stdlib.{f}")
  let ints =
    [ "Int8"
      "Int16"
      "Int32"
      "Int64"
      "Int128"
      "UInt8"
      "UInt16"
      "UInt32"
      "UInt64"
      "UInt128" ]
  let impls (types : List<string>) (ops : List<string * string>) =
    types
    |> List.collect (fun t ->
      ops
      |> List.map (fun (traitName, op) -> $"Darklang.Stdlib.{t}.{traitName}.{op}"))
  let wrapping =
    [ ("Add", "add")
      ("Subtract", "subtract")
      ("Multiply", "multiply")
      ("Negate", "negate") ]
  Set.ofList (
    impls
      ints
      (wrapping @ [ ("ShiftLeft", "shiftLeft"); ("ShiftRight", "shiftRight") ])
    @ impls [ "BigInt" ] wrapping
    @ impls
        [ "Float"; "Float32" ]
        (wrapping @ [ ("Divide", "divide"); ("Power", "power") ])
    @ guarded
  )

/// An integer literal's type and sign.
let private intLiteral (e : PT.Expr) : Option<string * int> =
  let inline signOf n = compare n LanguagePrimitives.GenericZero
  match e with
  | PT.EInt8(_, n) -> Some("Int8", signOf n)
  | PT.EUInt8(_, n) -> Some("UInt8", signOf n)
  | PT.EInt16(_, n) -> Some("Int16", signOf n)
  | PT.EUInt16(_, n) -> Some("UInt16", signOf n)
  | PT.EInt32(_, n) -> Some("Int32", signOf n)
  | PT.EUInt32(_, n) -> Some("UInt32", signOf n)
  | PT.EInt64(_, n) -> Some("Int64", signOf n)
  | PT.EUInt64(_, n) -> Some("UInt64", signOf n)
  | PT.EInt128(_, n) -> Some("Int128", signOf n)
  | PT.EUInt128(_, n) -> Some("UInt128", signOf n)
  | PT.EBigInt(_, n) -> Some("BigInt", signOf n)
  | _ -> None

/// Whether a literal divisor rules out every case where `impl` raises. On the integers and
/// BigInt, `/` raises on 0 alone (`MIN / -1` wraps) and `%` on 0 and below (`LibExecution.NumOps`).
let private divisorRulesOut (impl : string) (divisor : PT.Expr) : bool =
  match intLiteral divisor with
  | Some(t, sign) ->
    (impl = $"Darklang.Stdlib.{t}.Divide.divide" && sign <> 0)
    || (impl = $"Darklang.Stdlib.{t}.Modulo.modulo" && sign > 0)
  | None -> false

/// The impl an operator calls, when its literal divisor rules out its raising.
let private safeOperator
  (nameOf : PT.Hash -> string)
  (infix : PT.Infix)
  (divisor : PT.Expr)
  (implFn : PT.FQFnName.ImplChoice)
  : Option<PT.Hash> =
  match infix, implFn with
  | PT.InfixFnCall op, PT.FQFnName.Chosen impl when
    Option.isSome (LibExecution.NumericTraits.ofInfix op)
    && divisorRulesOut (nameOf impl.name) divisor
    ->
    Some impl.name
  | _ -> None

/// The fns `expr` calls where the literal arguments rule out the callee raising, once per call.
let rec private safeCalls
  (nameOf : PT.Hash -> string)
  (expr : PT.Expr)
  : List<PT.Hash> =
  let own =
    match expr with
    | PT.EInfix(_, infix, _, rhs, implFn) ->
      Option.toList (safeOperator nameOf infix rhs implFn)
    | PT.EPipe(_, _, parts) ->
      parts
      |> List.choose (function
        | PT.EPipeInfix(_, infix, rhs, implFn) ->
          safeOperator nameOf infix rhs implFn
        | _ -> None)
    | _ -> []
  own
  @ List.collect (safeCalls nameOf) (LibExecution.ProgramTypesAst.subExprs expr)

/// What `fn` calls that can raise: its calls, less the ones `safeCalls` rules out.
let calls
  (nameOf : PT.Hash -> string)
  (fn : PT.PackageFn.PackageFn)
  (analysis : LibExecution.CallGraph.Analysis)
  : List<PT.FQFnName.FQFnName> =
  safeCalls nameOf fn.body
  |> List.fold
    (fun names safe ->
      match List.tryFindIndex ((=) (PT.FQFnName.Package safe)) names with
      | Some i -> List.removeAt i names
      | None -> names)
    analysis.names

/// A hash with several names keeps the first in order.
let private namesOf query : Ply<Map<PT.Hash, string>> =
  uply {
    let! rows =
      query
      |> Sql.executeAsync (fun read ->
        (read.string "item_hash",
         read.string "owner",
         read.string "modules",
         read.string "name"))
    return
      rows
      |> List.map (fun (h, o, m, n) ->
        (PT.Hash h, (if m = "" then $"{o}.{n}" else $"{o}.{m}.{n}")))
      |> List.groupBy fst
      |> Map.map (fun named -> named |> List.map snd |> List.min)
  }

/// The live name of each fn in <hashes>.
let fnNames (hashes : List<PT.Hash>) : Ply<Map<PT.Hash, string>> =
  Sql.query
    "SELECT item_hash, owner, modules, name FROM locations
     WHERE live = 1 AND item_type = 'fn'
       AND item_hash IN (SELECT value FROM json_each(@hashes))"
  |> Sql.parameters
    [ "hashes", Sql.string (jsonStrings (hashes |> List.map (fun (PT.Hash h) -> h))) ]
  |> namesOf

/// Every live fn's name, for a pass over the whole store.
let allFnNames () : Ply<Map<PT.Hash, string>> =
  Sql.query
    "SELECT item_hash, owner, modules, name FROM locations
     WHERE live = 1 AND item_hash <> '' AND item_type = 'fn'"
  |> namesOf

/// The calls from `root` to a builtin that declares `Raise`, shortest first, avoiding the fns in
/// `cannotRaise`: each fn's name, then the builtin. `None` when nothing it reaches can raise.
let path
  (callEffectsFor : string * int -> Option<Set<Effects.Effect>>)
  (nameOf : PT.Hash -> string)
  (closure : Requirements.Closure)
  (root : PT.Hash)
  : Option<List<string>> =
  let raisingBuiltin (names : List<PT.FQFnName.FQFnName>) =
    names
    |> List.tryPick (function
      | PT.FQFnName.Builtin b when
        callEffectsFor (b.name, b.version)
        |> Option.exists (Set.contains Effects.Effect.Raise)
        ->
        Some $"Builtin.{b.name}"
      | _ -> None)
  let callees (names : List<PT.FQFnName.FQFnName>) =
    names
    |> List.choose (function
      | PT.FQFnName.Package p when not (Set.contains (nameOf p) cannotRaise) ->
        Some p
      | _ -> None)
  let rec search (visited : Set<PT.Hash>) (frontier : List<PT.Hash * List<string>>) =
    match frontier with
    | [] -> None
    | _ ->
      let found =
        frontier
        |> List.tryPick (fun (h, path) ->
          Map.tryFind h closure
          |> Option.bind (fun (fn, analysis) ->
            raisingBuiltin (calls nameOf fn analysis))
          |> Option.map (fun b -> path @ [ b ]))
      match found with
      | Some p -> Some p
      | None ->
        let next =
          frontier
          |> List.collect (fun (h, path) ->
            match Map.tryFind h closure with
            | Some(fn, analysis) ->
              callees (calls nameOf fn analysis)
              |> List.map (fun c -> c, path @ [ nameOf c ])
            | None -> [])
          |> List.distinctBy fst
          |> List.filter (fun (h, _) -> not (Set.contains h visited))
        search (next |> List.map fst |> Set.ofList |> Set.union visited) next
  if Set.contains (nameOf root) cannotRaise then
    None
  else
    search (Set.singleton root) [ root, [] ]
