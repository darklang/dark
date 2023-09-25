module BuiltinExecution.Libs.Dict

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module TypeChecker = LibExecution.TypeChecker

module VT = ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter

let varA = TVariable "a"
let varB = TVariable "b"

let modules = [ "Dict" ]
let fn = fn modules
let constant = constant modules

let types : List<BuiltInType> = []

let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "size" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TInt
      description = "Returns the number of entries in <param dict>"
      fn =
        (function
        | _, _, [ DDict(_vtTODO, o) ] -> Ply(DInt(int64 (Map.count o)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "keys" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList TString)
      description =
        "Returns <param dict>'s keys in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, _, [ DDict(_vtTODO, o) ] ->
          o
          |> Map.keys
          |> Seq.map (fun k -> DString k)
          |> Seq.toList
          |> fun l -> Dval.list (ValueType.Known KTString) l
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "values" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList varA)
      description =
        "Returns <param dict>'s values in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, _, [ DDict(_vtTODO, o) ] ->
          o
          |> Map.values
          |> Seq.toList
          |> (fun l -> Dval.list VT.unknownTODO l |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toList" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList(TTuple(varA, varB, [])))
      description =
        "Returns <param dict>'s entries as a list of {{(key, value)}} tuples, in an arbitrary order. This function is the opposite of <fn Dict.fromList>"
      fn =
        (function
        | _, _, [ DDict(_vtTODO, o) ] ->
          Map.toList o
          |> List.map (fun (k, v) -> DTuple(DString k, v, []))
          |> Dval.list (
            ValueType.Known(KTTuple(ValueType.Known KTString, VT.unknownTODO, []))
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }



    { name = fn "fromListOverwritingDuplicates" 0
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
        | _, _, [ DList(_, l) ] ->
          let f acc dv =
            match dv with
            | DTuple(DString k, value, []) -> Map.add k value acc
            | _ ->
              Exception.raiseInternal
                "Not string tuples in fromListOverwritingDuplicates"
                [ "dval", dv ]
          let result = l |> List.fold f Map.empty |> Map.toList
          Ply(Dval.dict VT.unknownTODO result)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromList" 0
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
        | _, _, [ DList(_vtTODO, l) ] ->
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
          | Some map ->
            map |> Map.toList |> Dval.dict dictType |> Dval.optionSome optType
          | None -> Dval.optionNone optType
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "get" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TypeReference.option varA
      description =
        "If the <param dict> contains <param key>, returns the corresponding value,
         wrapped in an <type Option>: {{Some value}}. Otherwise, returns {{None}}."
      fn =
        (function
        | _, _, [ DDict(_vtTODO, o); DString s ] ->
          Map.find s o |> Dval.option VT.unknownTODO
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "member" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TBool
      description =
        "Returns {{true}} if the <param dict> contains an entry with <param key>, and
         {{false}} otherwise"
      fn =
        (function
        | _, _, [ DDict(_, o); DString s ] -> Ply(DBool(Map.containsKey s o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "map" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.ofList TString [ varA ], varB))
            ""
            [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Returns a new dictionary that contains the same keys as the original <param
         dict> with values that have been transformed by {{fn}}, which operates on
         each key-value pair.

         Consider <fn Dict.filterMap> if you also want to drop some of the entries."
      fn =
        (function
        | state, [], [ DDict(_vtTODO, o); DFnVal b ] ->
          uply {
            let mapped = Map.mapWithIndex (fun i v -> (i, v)) o

            let! result =
              Ply.Map.mapSequentially
                (fun (key, dv) ->
                  let args = NEList.ofList (DString key) [ dv ]
                  Interpreter.applyFnVal state 0UL b [] args)
                mapped

            return Dval.dict VT.unknownTODO (Map.toList result)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "iter" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.ofList TString [ varA ], TUnit))
            ""
            [ "key"; "value" ] ]
      returnType = TUnit
      description =
        "Evaluates {{fn key value}} on every entry in <param dict>. Returns {{()}}."
      fn =
        (function
        | state, _, [ DDict(_, o); DFnVal b ] ->
          uply {
            do!
              Map.toList o
              |> Ply.List.iterSequentially (fun (key, dv) ->
                uply {
                  let args = NEList.ofList (DString key) [ dv ]
                  match! Interpreter.applyFnVal state 0UL b [] args with
                  | DUnit -> return ()
                  | dv ->
                    return!
                      TypeChecker.raiseFnValResultNotExpectedType
                        SourceNone
                        dv
                        TUnit
                })
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "filter" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton TString varA, TBool))
            ""
            [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Evaluates {{fn key value}} on every entry in <param dict>. Returns a <type
         dict> that contains only the entries of <param dict> for which <param fn>
         returned {{true}}."
      fn =
        (function
        | state, _, [ DDict(_vtTODO, o); DFnVal b ] ->
          uply {
            let f (key : string) (data : Dval) : Ply<bool> =
              uply {
                let args = NEList.ofList (DString key) [ data ]
                match! Interpreter.applyFnVal state 0UL b [] args with
                | DBool v -> return v
                | v ->
                  return!
                    TypeChecker.raiseFnValResultNotExpectedType SourceNone v TBool
              }
            let! result = Ply.Map.filterSequentially f o
            return Dval.dictFromMap VT.unknownTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "filterMap" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.ofList TString [ varA ], TypeReference.option varB))
            ""
            [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Calls <param fn> on every entry in <param dict>, returning a <type dict> that drops some entries (filter) and transforms others (map).
          If {{fn key value}} returns {{None}}, does not add <var key> or <var value> to the new dictionary, dropping the entry.
          If {{fn key value}} returns {{Some newValue}}, adds the entry <var key>: <var newValue> to the new dictionary.
          This function combines <fn Dict.filter> and <fn Dict.map>."
      fn =
        (function
        | state, _, [ DDict(_vtTODO, o); DFnVal b ] ->
          uply {
            let f (key : string) (data : Dval) : Ply<Option<Dval>> =
              uply {
                let args = NEList.ofList (DString key) [ data ]
                let! result = Interpreter.applyFnVal state 0UL b [] args

                match result with
                | DEnum(FQName.Package { owner = "Darklang"
                                         modules = [ "Stdlib"; "Option" ]
                                         name = TypeName.TypeName "Option"
                                         version = 0 },
                        _,
                        _typeArgsDEnumTODO,
                        "Some",
                        [ o ]) -> return Some o
                | DEnum(FQName.Package { owner = "Darklang"
                                         modules = [ "Stdlib"; "Option" ]
                                         name = TypeName.TypeName "Option"
                                         version = 0 },
                        _,
                        _typeArgsDEnumTODO,
                        "None",
                        []) -> return None
                | v ->
                  let expectedType = TypeReference.option varB
                  return!
                    TypeChecker.raiseFnValResultNotExpectedType
                      SourceNone
                      v
                      expectedType
              }

            let! result = Ply.Map.filterMapSequentially f o
            return Dval.dictFromMap VT.unknownTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "isEmpty" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TBool
      description = "Returns {{true}} if the <param dict> contains no entries"
      fn =
        (function
        | _, _, [ DDict(_, dict) ] -> Ply(DBool(Map.isEmpty dict))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "merge" 0
      typeParams = []
      parameters =
        [ Param.make "left" (TDict varA) ""; Param.make "right" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Returns a combined dictionary with both dictionaries' entries. If the same key exists in both <param left> and <param right>, it will have the value from <param right>."
      fn =
        (function
        | _, _, [ DDict(_vtTODO1, l); DDict(_vtTODO2, r) ] ->
          Map.mergeFavoringRight l r |> Map.toList |> Dval.dict VT.unknownTODO |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "set" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict(TVariable "a")) ""
          Param.make "key" TString ""
          Param.make "val" varA "" ]
      returnType = (TDict(TVariable "a"))
      description =
        "Returns a copy of <param dict> with the <param key> set to <param val>"
      fn =
        (function
        | _, _, [ DDict(vt, o); DString k; v ] ->
          Map.add k v o |> Dval.dictFromMap vt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "remove" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TDict varA
      description =
        "If the <param dict> contains <param key>, returns a copy of <param dict> with <param key> and its associated value removed. Otherwise, returns <param dict> unchanged."
      fn =
        (function
        | _, _, [ DDict(vt, o); DString k ] ->
          Map.remove k o |> Dval.dictFromMap vt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
