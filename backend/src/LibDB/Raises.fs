/// Whether a package fn can stop with a runtime error, and through which calls.
///
/// `Raise` flows through the call graph like any effect, but a few stdlib fns test for every
/// case where what they call raises. `view` and `effects` leave those out.
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
let cannotRaise : Set<string> =
  let guarded =
    [ "Int64.checkedDivide"
      "Int64.checkedRemainder"
      "Int64.checkedMod"
      "BigInt.checkedDivide"
      "BigInt.checkedRemainder"
      "BigInt.checkedMod"
      "String.checkedRepeat"
      "List.checkedRepeat"
      "List.checkedRange" ]
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

/// Each live fn's name. A hash with several names keeps the first in order.
let fnNames () : Ply<Map<PT.Hash, string>> =
  uply {
    let! rows =
      Sql.query
        "SELECT item_hash, owner, modules, name FROM locations
         WHERE live = 1 AND item_hash <> '' AND item_type = 'fn'"
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

/// The calls from `root` to a builtin that declares `Raise`, shortest first, avoiding the fns in
/// `cannotRaise`: each fn's name, then the builtin. `None` when nothing it reaches can raise.
let path
  (callEffectsFor : string * int -> Option<Set<Effects.Effect>>)
  (nameOf : PT.Hash -> string)
  (closure : Requirements.Closure)
  (root : PT.Hash)
  : Option<List<string>> =
  let raisingBuiltin (calls : LibExecution.CallGraph.Analysis) =
    calls.names
    |> List.tryPick (function
      | PT.FQFnName.Builtin b when
        callEffectsFor (b.name, b.version)
        |> Option.exists (Set.contains Effects.Effect.Raise)
        ->
        Some $"Builtin.{b.name}"
      | _ -> None)
  let callees (calls : LibExecution.CallGraph.Analysis) =
    calls.names
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
          |> Option.bind (fun (_, calls) -> raisingBuiltin calls)
          |> Option.map (fun b -> path @ [ b ]))
      match found with
      | Some p -> Some p
      | None ->
        let next =
          frontier
          |> List.collect (fun (h, path) ->
            match Map.tryFind h closure with
            | Some(_, calls) ->
              callees calls |> List.map (fun c -> c, path @ [ nameOf c ])
            | None -> [])
          |> List.distinctBy fst
          |> List.filter (fun (h, _) -> not (Set.contains h visited))
        search (next |> List.map fst |> Set.ofList |> Set.union visited) next
  if Set.contains (nameOf root) cannotRaise then
    None
  else
    search (Set.singleton root) [ root, [] ]
