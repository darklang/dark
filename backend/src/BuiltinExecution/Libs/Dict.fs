module BuiltinExecution.Libs.Dict

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module TypeChecker = LibExecution.TypeChecker

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module PackageIDs = LibExecution.PackageIDs

let varA = TVariable "a"
let varB = TVariable "b"


let fns : List<BuiltInFn> =
  [ { name = fn "dictSize" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TInt64
      description = "Returns the number of entries in <param dict>"
      fn =
        (function
        | _, _, _, [ DDict(_vtTODO, o) ] -> Ply(DInt64(int64 (Map.count o)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictKeys" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TList TString
      description =
        "Returns <param dict>'s keys in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, _, _, [ DDict(_, o) ] ->
          // CLEANUP follow up here if/when `key` type is dynamic (not just String)
          o |> Map.keys |> Seq.map DString |> Seq.toList |> Dval.list KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictValues" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList varA)
      description =
        "Returns <param dict>'s values in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, _, _, [ DDict(valueType, o) ] ->
          o |> Map.values |> Seq.toList |> (fun vs -> DList(valueType, vs) |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictToList" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TList(TTuple(TString, varA, []))
      description =
        "Returns <param dict>'s entries as a list of {{(key, value)}} tuples, in an arbitrary order.
        This function is the opposite of <fn Dict.fromList>"
      fn =
        (function
        | _, _, _, [ DDict(valueType, o) ] ->
          Map.toList o
          |> List.map (fun (k, v) -> DTuple(DString k, v, []))
          |> fun pairs -> DList(VT.tuple VT.string valueType [], pairs)
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictFromListOverwritingDuplicates" 0
      typeParams = []
      parameters = [ Param.make "entries" (TList(TTuple(TString, varA, []))) "" ]
      returnType = TDict varB
      description =
        "Returns a <type dict> with <param entries>. Each value in <param entries>
          must be a {{(key, value)}} tuple, where <var key> is a <type String>.

          If <param entries> contains duplicate <var key>s, the last entry with that
          key will be used in the resulting dictionary (use <fn Dict.fromList> if you
          want to enforce unique keys).

          This function is the opposite of <fn Dict.toList>."
      fn =
        (function
        | _, _, _, [ DList(_, []) ] -> DDict(VT.unknown, Map.empty) |> Ply

        | _, vm, _, [ DList(ValueType.Known(KTTuple(_keyType, valueType, [])), l) ] ->
          let f (accType, accMap) dv =
            match dv with
            | DTuple(DString k, value, []) ->
              TypeChecker.DvalCreator.dictAddEntry
                vm.threadID
                accType
                accMap
                (k, value)
                TypeChecker.ReplaceValue
            | _ ->
              Exception.raiseInternal
                "Not string tuples in fromListOverwritingDuplicates"
                [ "dval", dv ]

          let (typ, map) = List.fold f (valueType, Map.empty) l
          DDict(typ, map) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictFromList" 0
      typeParams = []
      parameters = [ Param.make "entries" (TList(TTuple(TString, varB, []))) "" ]
      returnType = TypeReference.option (TDict varB)
      description =
        "Each value in <param entries> must be a {{(key, value)}} tuple, where <var
         key> is a <type String>.

         If <param entries> contains no duplicate keys, returns {{Some <var dict>}}
         where <var dict> has <param entries>.

         Otherwise, returns {{None}} (use <fn Dict.fromListOverwritingDuplicates>
         if you want to overwrite duplicate keys)."
      fn =
        let dictType = VT.unknownTODO
        let optType = VT.dict dictType
        (function
        | _, vmState, _, [ DList(_vtTODO, l) ] ->
          let f acc dv =
            match acc, dv with
            | None, _ -> None
            | Some acc, DTuple(DString k, _, _) when Map.containsKey k acc -> None
            | Some acc, DTuple(DString k, value, []) -> Some(Map.add k value acc)
            | Some _, DTuple(_, _, [])
            | Some _, _ ->
              Exception.raiseInternal "Not string tuples in fromList" [ "dval", dv ]

          let result = List.fold f (Some Map.empty) l

          match result with
          | Some entries ->
            DDict(dictType, entries)
            |> TypeChecker.DvalCreator.optionSome vmState.threadID optType
            |> Ply
          | None -> TypeChecker.DvalCreator.optionNone optType |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictGet" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TypeReference.option varA
      description =
        "If the <param dict> contains <param key>, returns the corresponding value,
         wrapped in an <type Option>: {{Some value}}. Otherwise, returns {{None}}."
      fn =
        (function
        | _, vm, _, [ DDict(_vtTODO, o); DString s ] ->
          Map.find s o
          |> TypeChecker.DvalCreator.option vm.threadID VT.unknownTODO
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictMember" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TBool
      description =
        "Returns {{true}} if the <param dict> contains an entry with <param key>, and
         {{false}} otherwise"
      fn =
        (function
        | _, _, _, [ DDict(_, o); DString s ] -> Ply(DBool(Map.containsKey s o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictMerge" 0
      typeParams = []
      parameters =
        [ Param.make "left" (TDict varA) ""; Param.make "right" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Returns a combined dictionary with both dictionaries' entries.
        If the same key exists in both <param left> and <param right>,
          it will have the value from <param right>."
      fn =
        (function
        | _, vm, _, [ DDict(_vtTODO1, l); DDict(_vtTODO2, r) ] ->
          Map.mergeFavoringRight l r
          // CLEANUP: performance
          |> Map.toList
          |> TypeChecker.DvalCreator.dict vm.threadID VT.unknownTODO
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictSet" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict(TVariable "a")) ""
          Param.make "key" TString ""
          Param.make "val" varA "" ]
      returnType = (TDict(TVariable "a"))
      description =
        "Returns a copy of <param dict> with the <param key> set to <param val>.
        If the key already exists in the Dict, an exception is raised."
      fn =
        (function
        | _, vm, _, [ DDict(vt, o); DString k; v ] ->
          let (typ, map) =
            TypeChecker.DvalCreator.dictAddEntry
              vm.threadID
              vt
              o
              (k, v)
              TypeChecker.ThrowIfDuplicate
          DDict(typ, map) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictSetOverridingDuplicates" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict(TVariable "a")) ""
          Param.make "key" TString ""
          Param.make "val" varA "" ]
      returnType = (TDict(TVariable "a"))
      description =
        "Returns a copy of <param dict> with the <param key> set to <param val>.
        If the key already exists in the Dict, the previous value is overwritten."
      fn =
        (function
        | _, vm, _, [ DDict(vt, o); DString k; v ] ->
          let (typ, map) =
            TypeChecker.DvalCreator.dictAddEntry
              vm.threadID
              vt
              o
              (k, v)
              TypeChecker.ReplaceValue
          DDict(typ, map) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dictRemove" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TDict varA
      description =
        "If the <param dict> contains <param key>, returns a copy of <param dict> with <param key> and its associated value removed. Otherwise, returns <param dict> unchanged."
      fn =
        (function
        | _, _, _, [ DDict(vt, o); DString k ] -> DDict(vt, Map.remove k o) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
