module Builtins.Pure.Libs.Dict

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module TypeChecker = LibExecution.TypeChecker

module VT = LibExecution.ValueType
module RTE = RuntimeError
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter

let varK = TVariable "k"
let varA = TVariable "a"


let rec private addAllEntries
  (threadID : ThreadID)
  (keyType : ValueType)
  (valueType : ValueType)
  (acc : DictMap)
  (remaining : List<Dval>)
  : struct (ValueType * ValueType * DictMap) =
  match remaining with
  | [] -> struct (keyType, valueType, acc)
  | DTuple(k, value, []) :: rest ->
    let struct (keyType, valueType, acc) =
      TypeChecker.DvalCreator.dictAddEntry threadID keyType valueType acc k value
    addAllEntries threadID keyType valueType acc rest
  | dv :: _ ->
    Exception.raiseInternal "Not pair tuples in fromEntries" [ "dval", dv ]

let fns () : List<BuiltInFn> =
  [ { name = fn "dictSize" 0
      typeParams = []
      parameters = [ Param.moved "dict" (TDict(varK, varA)) "" ]
      returnType = TInt
      description = "Returns the number of entries in <param dict>"
      fn =
        (function
        | _, _, _, [| DDict(_, _, o) |] -> Ply(Dval.int (bigint (Map.count o)))
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
          // `Map.foldBack` walks the tree directly, in `Map.keys`' ascending order. A lazy `Seq`
          // chain would cost an enumerator and a closure per stage.
          DList(
            keyType,
            Map.foldBack (fun (k : DictKey) _ acc -> k.Dval :: acc) o []
          )
          |> Ply
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
          DList(valueType, Map.foldBack (fun _ v acc -> v :: acc) o []) |> Ply
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
          let f (k : DictKey) v acc = DTuple(k.Dval, v, []) :: acc
          Map.foldBack f o []
          |> fun pairs -> DList(VT.tuple keyType valueType [], pairs)
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
        | _, _, _, [| DList(_, []) |] ->
          DDict(VT.unknown, VT.unknown, Map.empty) |> Ply

        | _, vm, _, [| DList(ValueType.Known(KTTuple(keyType, valueType, [])), l) |] ->
          let struct (keyType, valueType, map) =
            addAllEntries vm.threadID keyType valueType Map.empty l
          DDict(keyType, valueType, map) |> Ply
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
          Map.find (DictKey key) o
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
          Ply(DBool(Map.containsKey (DictKey key) o))
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
            let f accMap k v = Map.add k v accMap
            let mergedMap = Map.fold f intoMap fromMap
            DDict(mergedKeyType, mergedValueType, mergedMap) |> Ply
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
          DDict(kt, vt, Map.remove (DictKey k) o) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
