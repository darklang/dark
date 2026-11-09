module Builtins.Pure.Libs.Dict

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module TypeChecker = LibExecution.TypeChecker

module VT = LibExecution.ValueType
module RTE = RuntimeError
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module Spread = LibExecution.Spread

let varK = TVariable "k"
let varA = TVariable "a"
let varB = TVariable "b"


let private entryPair (dv : Dval) : Dval * Dval =
  match dv with
  | DTuple(k, value, []) -> (k, value)
  | dv -> Exception.raiseInternal "Not pair tuples in fromEntries" [ "dval", dv ]

/// Spreads `fn` over `entries` from `start` on (`LibExecution.Spread`), as `List.map` is: a
/// chunk is `listMap2shortest` over its keys and its values, which applies `fn` to each pair as
/// `applyEach` does. Writes the answers of the chunks that finished into `answers` and answers
/// where `applyEach` carries on: the first entry of the first chunk that did not. A chunk checks
/// a callback type an enclosing fn declared (`Stdlib.Dict.filter`'s `Bool`) as a serial run does,
/// so an answer of the wrong type fails its chunk and is raised here.
let private spreadRest
  (state : ExecutionState)
  (vm : VMState)
  (app : Applicable)
  (keyType : ValueType)
  (valueType : ValueType)
  (entries : (struct (Dval * Dval))[])
  (answers : Dval[])
  (start : int)
  (perElement : int64)
  : Ply<int> =
  let keyAt j =
    let struct (k, _) = entries[j]
    k
  let valueAt j =
    let struct (_, v) = entries[j]
    v
  uply {
    let! outcome =
      Spread.run
        state
        vm
        "listMap2shortest"
        (fun offset chunk ->
          let first = start + offset
          let keys = List.init (List.length chunk) (fun n -> keyAt (first + n))
          [ DList(keyType, Items(keys)); DList(valueType, Items(chunk)) ])
        app
        perElement
        (List.init (entries.Length - start) (fun n -> valueAt (start + n)))
    outcome.results |> List.iteri (fun n answer -> answers[start + n] <- answer)
    return start + List.length outcome.results
  }

/// Calls `fn` with each entry's key and value, in key order, and hands `finish` its
/// answers. `filtering` (`dictFilter`) waits for an answer that is a read in flight first.
///
/// Once the rest looks worth it, it is spread across cores (`spreadRest`), and whatever the spread
/// did not finish carries on here. `finish` sees every answer either way, so a check it makes
/// (`dictFilter`'s Bool) is made where it always was.
let private applyEach
  (state : ExecutionState)
  (vm : VMState)
  (app : Applicable)
  (filtering : bool)
  (keyType : ValueType)
  (valueType : ValueType)
  (entries : (struct (Dval * Dval))[])
  (finish : Dval[] -> Dval)
  : Ply<Dval> =
  let answers = Array.zeroCreate entries.Length
  let mutable i = 0
  let probe = if entries.Length > 1 then Spread.probeCostly state vm app else null
  let rec next (answer : Dval) : Ply<Dval> =
    if filtering then Interpreter.withValue vm answer store else store answer
  and store (answer : Dval) : Ply<Dval> =
    answers[i] <- answer
    i <- i + 1
    if i = entries.Length then
      Ply(finish answers)
    elif not (isNull probe) && probe.AskLeft(entries.Length - i) then
      probe.Stop()
      uply {
        let! carryOn =
          spreadRest
            state
            vm
            app
            keyType
            valueType
            entries
            answers
            i
            probe.PerElement
        i <- carryOn
        if i = entries.Length then
          return finish answers
        else
          let struct (k, v) = entries[i]
          return! Interpreter.requestApply vm app k [ v ] next
      }
    else
      let struct (k, v) = entries[i]
      Interpreter.requestApply vm app k [ v ] next
  if entries.Length = 0 then
    Ply(finish answers)
  else
    let struct (k, v) = entries[0]
    Interpreter.requestApply vm app k [ v ] next

let fns () : List<BuiltInFn> =
  [ { name = fn "dictSize" 0
      typeParams = []
      parameters = [ Param.moved "dict" (TDict(varK, varA)) "" ]
      returnType = TInt64
      description = "Returns the number of entries in <param dict>"
      fn =
        (function
        | _, _, _, [| DDict(_, _, o) |] -> Ply(Dval.dint64 (int64 (DictMap.count o)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictKeys" 0
      typeParams = []
      parameters = [ Param.moved "dict" (TDict(varK, varA)) "" ]
      returnType = TList varK
      description = "Returns <param dict>'s keys in a <type List>, in key order"
      fn =
        (function
        | _, _, _, [| DDict(keyType, _, o) |] ->
          DList(keyType, Items(DictMap.keys o)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictValues" 0
      typeParams = []
      parameters = [ Param.moved "dict" (TDict(varK, varA)) "" ]
      returnType = (TList varA)
      description =
        "Returns <param dict>'s values in a <type List>, ordered by their keys"
      fn =
        (function
        | _, _, _, [| DDict(_, valueType, o) |] ->
          // See `dictKeys`: a direct fold rather than a lazy sequence and its enumerator.
          DList(valueType, Items(DictMap.values o)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictToEntries" 0
      typeParams = []
      parameters = [ Param.moved "dict" (TDict(varK, varA)) "" ]
      returnType = TList(TTuple(varK, varA, []))
      description =
        "Returns <param dict>'s entries as a list of {{(key, value)}} tuples, "
        + "in key order. This function is the opposite of <fn Dict.fromEntries>"
      fn =
        (function
        | _, _, _, [| DDict(keyType, valueType, o) |] ->
          DictMap.foldBack (fun k v acc -> DTuple(k, v, []) :: acc) o []
          |> fun pairs -> DList(VT.tuple keyType valueType [], Items(pairs))
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictFromEntries" 0
      typeParams = [ "k"; "a" ]
      parameters = [ Param.make "entries" (TList(TTuple(varK, varA, []))) "" ]
      returnType = TDict(varK, varA)
      description =
        "Returns a <type dict> with <param entries>. Each value in <param "
        + "entries> must be a {{(key, value)}} tuple.\n\nIf <param entries> "
        + "contains duplicate <var key>s, the last entry with that key is used "
        + "(use <fn Dict.fromEntriesUnique> to refuse duplicates).\n\nThis "
        + "function is the opposite of <fn Dict.toEntries>."
      fn =
        (function
        | _, _, _, [| DList(_, Items([])) |] ->
          DDict(VT.unknown, VT.unknown, DictMap.empty) |> Ply

        | _,
          vm,
          _,
          [| DList(ValueType.Known(KTTuple(keyType, valueType, [])), Items(l)) |] ->
          let struct (keyType, valueType, map) =
            TypeChecker.DvalCreator.dictEntries
              vm.threadID
              keyType
              valueType
              (List.map entryPair l)
          DDict(keyType, valueType, map) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictMap" 0
      typeParams = []
      parameters =
        [ Param.moved "dict" (TDict(varK, varA)) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varK varA, varB))
            ""
            [ "key"; "value" ] ]
      returnType = TDict(varK, varB)
      description =
        "Calls <param fn> on every entry in <param dict>, returning a dict of the same "
        + "keys with what it returned"
      fn =
        (function
        | state, vm, [], [| DDict(kt, vt, o); DApplicable app |] ->
          let entries = o.Entries
          applyEach state vm app false kt vt entries (fun answers ->
            List.init entries.Length (fun i ->
              let struct (k, _) = entries[i]
              (k, answers[i]))
            |> TypeChecker.DvalCreator.dict vm.threadID kt VT.unknown)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictFilter" 0
      typeParams = []
      parameters =
        [ Param.moved "dict" (TDict(varK, varA)) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varK varA, TBool))
            ""
            [ "key"; "value" ] ]
      returnType = TDict(varK, varA)
      description =
        "Calls <param fn> on every entry in <param dict>, returning the entries for "
        + "which it returned true"
      fn =
        (function
        | state, vm, [], [| DDict(kt, vt, o); DApplicable app |] ->
          let entries = o.Entries
          applyEach state vm app true kt vt entries (fun answers ->
            let kept =
              entries
              |> Array.indexed
              |> Array.filter (fun (i, _) ->
                match answers[i] with
                | DBool keep -> keep
                | other ->
                  RuntimeError.Bools.ConditionRequiresBool(
                    Dval.toValueType other,
                    other
                  )
                  |> RuntimeError.Bool
                  |> raiseRTE vm.threadID)
              |> Array.map (fun (_, struct (k, v)) -> (k, v))
            DDict(kt, vt, DictMap.ofList (List.ofArray kept)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictGet" 0
      typeParams = []
      parameters =
        [ Param.moved "dict" (TDict(varK, varA)) ""; Param.make "key" varK "" ]
      returnType = TypeReference.option varA
      description =
        "If the <param dict> contains <param key>, returns the corresponding "
        + "value, wrapped in an <type Option>: {{Some value}}. Otherwise, returns "
        + "{{None}}."
      fn =
        (function
        | _, vm, _, [| DDict(_, _, o); key |] ->
          LibExecution.RuntimeTypes.Dval.assertUsableDictKey key
          DictMap.tryFind key o
          |> TypeChecker.DvalCreator.option vm.threadID VT.unknownTODO
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictContainsKey" 0
      typeParams = []
      parameters =
        [ Param.moved "dict" (TDict(varK, varA)) ""; Param.make "key" varK "" ]
      returnType = TBool
      description =
        "Returns {{true}} if the <param dict> contains an entry with <param "
        + "key>, and {{false}} otherwise"
      fn =
        (function
        | _, _, _, [| DDict(_, _, o); key |] ->
          LibExecution.RuntimeTypes.Dval.assertUsableDictKey key
          Ply(DBool(DictMap.containsKey key o))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictMerge" 0
      typeParams = []
      parameters =
        [ Param.moved "left" (TDict(varK, varA)) ""
          Param.moved "right" (TDict(varK, varA)) "" ]
      returnType = TDict(varK, varA)
      description =
        "Returns a combined dictionary with both dictionaries' entries. If the "
        + "same key exists in both <param left> and <param right>, it will have "
        + "the value from <param right>."
      fn =
        (function
        | _, _vm, _, [| DDict(kt1, vt1, intoMap); DDict(kt2, vt2, fromMap) |] ->
          match VT.merge kt1 kt2, VT.merge vt1 vt2 with
          | Ok mergedKeyType, Ok mergedValueType ->
            DDict(mergedKeyType, mergedValueType, DictMap.union intoMap fromMap)
            |> Ply
          | _ ->
            Exception.raiseInternal
              "Builtin.dictMerge input dicts somehow bypassed fn-arg type-checking"
              [ ("kt1", kt1); ("kt2", kt2); ("vt1", vt1); ("vt2", vt2) ]
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictSet" 0
      typeParams = [ "k"; "a" ]
      parameters =
        [ Param.moved "dict" (TDict(varK, varA)) ""
          Param.make "key" varK ""
          Param.moved "value" varA "" ]
      returnType = TDict(varK, varA)
      description =
        "Returns a copy of <param dict> with the <param key> set to <param "
        + "value>. If the key already exists in the Dict, the previous value is "
        + "overwritten."
      fn =
        (function
        | _, vm, _, [| DDict(kt, vt, o); k; v |] ->
          let struct (kt, vt, map) =
            TypeChecker.DvalCreator.dictAddEntry vm.threadID kt vt o k v
          DDict(kt, vt, map) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "dictRemove" 0
      typeParams = []
      parameters =
        [ Param.moved "dict" (TDict(varK, varA)) ""; Param.make "key" varK "" ]
      returnType = TDict(varK, varA)
      description =
        "If the <param dict> contains <param key>, returns a copy of <param dict> with <param key> and its associated value removed. Otherwise, returns <param dict> unchanged."
      fn =
        (function
        | _, _, _, [| DDict(kt, vt, o); k |] ->
          LibExecution.RuntimeTypes.Dval.assertUsableDictKey k
          DDict(kt, vt, DictMap.remove k o) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
