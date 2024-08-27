module BuiltinExecution.Libs.Dict

open System.Threading.Tasks
open FSharp.Control.Tasks

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
      returnType = (TList(TTuple(varA, varB, [])))
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



    // { name = fn "dictFromListOverwritingDuplicates" 0
    //   typeParams = []
    //   parameters = [ Param.make "entries" (TList(TTuple(TString, varA, []))) "" ]
    //   returnType = TDict varB
    //   description =
    //     "Returns a <type dict> with <param entries>. Each value in <param entries>
    //       must be a {{(key, value)}} tuple, where <var key> is a <type String>.

    //       If <param entries> contains duplicate <var key>s, the last entry with that
    //       key will be used in the resulting dictionary (use <fn Dict.fromList> if you
    //       want to enforce unique keys).

    //       This function is the opposite of <fn Dict.toList>."
    //   fn =
    //     (function
    //     | _, _, _, [ DList(_, l) ] ->
    //       let f acc dv =
    //         match dv with
    //         | DTuple(DString k, value, []) -> Map.add k value acc
    //         | _ ->
    //           Exception.raiseInternal
    //             "Not string tuples in fromListOverwritingDuplicates"
    //             [ "dval", dv ]

    //       List.fold f Map.empty l
    //       |> TypeChecker.DvalCreator.dictFromMap VT.unknownTODO
    //       |> Ply
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


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
            |> TypeChecker.DvalCreator.optionSome vmState.callStack optType
            |> Ply
          | None -> TypeChecker.DvalCreator.optionNone optType |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // { name = fn "dictGet" 0
    //   typeParams = []
    //   parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
    //   returnType = TypeReference.option varA
    //   description =
    //     "If the <param dict> contains <param key>, returns the corresponding value,
    //      wrapped in an <type Option>: {{Some value}}. Otherwise, returns {{None}}."
    //   fn =
    //     (function
    //     | state, _, [ DDict(_vtTODO, o); DString s ] ->
    //       Map.find s o
    //       |> TypeChecker.DvalCreator.option vmState.callStack VT.unknownTODO
    //       |> Ply
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


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


    // { name = fn "dictMap" 0
    //   typeParams = []
    //   parameters =
    //     [ Param.make "dict" (TDict varA) ""
    //       Param.makeWithArgs
    //         "fn"
    //         (TFn(NEList.ofList TString [ varA ], varB))
    //         ""
    //         [ "key"; "value" ] ]
    //   returnType = TDict varB
    //   description =
    //     "Returns a new dictionary that contains the same keys as the original <param
    //      dict> with values that have been transformed by {{fn}}, which operates on
    //      each key-value pair.

    //      Consider <fn Dict.filterMap> if you also want to drop some of the entries."
    //   fn =
    //     (function
    //     | state, [], [ DDict(_vtTODO, o); DFnVal b ] ->
    //       uply {
    //         let mapped = Map.mapWithIndex (fun i v -> (i, v)) o

    //         let! result =
    //           Ply.Map.mapSequentially
    //             (fun (key, dv) ->
    //               let args = NEList.ofList (DString key) [ dv ]
    //               Interpreter.applyFnVal state b [] args)
    //             mapped

    //         return TypeChecker.DvalCreator.dictFromMap VT.unknownTODO result
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    // { name = fn "dictIter" 0
    //   typeParams = []
    //   parameters =
    //     [ Param.make "dict" (TDict varA) ""
    //       Param.makeWithArgs
    //         "fn"
    //         (TFn(NEList.ofList TString [ varA ], TUnit))
    //         ""
    //         [ "key"; "value" ] ]
    //   returnType = TUnit
    //   description =
    //     "Evaluates {{fn key value}} on every entry in <param dict>. Returns {{()}}."
    //   fn =
    //     (function
    //     | state, _, [ DDict(_, o); DFnVal b ] ->
    //       uply {
    //         do!
    //           Map.toList o
    //           |> Ply.List.iterSequentially (fun (key, dv) ->
    //             uply {
    //               let args = NEList.ofList (DString key) [ dv ]
    //               match! Interpreter.applyFnVal state b [] args with
    //               | DUnit -> return ()
    //               | dv ->
    //                 return!
    //                   TypeChecker.raiseFnValResultNotExpectedType
    //                     vmState.callStack
    //                     dv
    //                     TUnit
    //             })
    //         return DUnit
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    // { name = fn "dictFilter" 0
    //   typeParams = []
    //   parameters =
    //     [ Param.make "dict" (TDict varA) ""
    //       Param.makeWithArgs
    //         "fn"
    //         (TFn(NEList.doubleton TString varA, TBool))
    //         ""
    //         [ "key"; "value" ] ]
    //   returnType = TDict varB
    //   description =
    //     "Evaluates {{fn key value}} on every entry in <param dict>. Returns a <type
    //      dict> that contains only the entries of <param dict> for which <param fn>
    //      returned {{true}}."
    //   fn =
    //     (function
    //     | state, _, [ DDict(_vtTODO, o); DFnVal b ] ->
    //       uply {
    //         let f (key : string) (data : Dval) : Ply<bool> =
    //           uply {
    //             let args = NEList.ofList (DString key) [ data ]
    //             match! Interpreter.applyFnVal state b [] args with
    //             | DBool v -> return v
    //             | v ->
    //               return!
    //                 TypeChecker.raiseFnValResultNotExpectedType
    //                   vmState.callStack
    //                   v
    //                   TBool
    //           }
    //         let! result = Ply.Map.filterSequentially f o
    //         return TypeChecker.DvalCreator.dictFromMap VT.unknownTODO result
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    // { name = fn "dictFilterMap" 0
    //   typeParams = []
    //   parameters =
    //     [ Param.make "dict" (TDict varA) ""
    //       Param.makeWithArgs
    //         "fn"
    //         (TFn(NEList.ofList TString [ varA ], TypeReference.option varB))
    //         ""
    //         [ "key"; "value" ] ]
    //   returnType = TDict varB
    //   description =
    //     "Calls <param fn> on every entry in <param dict>, returning a <type dict> that drops some entries (filter) and transforms others (map).
    //       If {{fn key value}} returns {{None}}, does not add <var key> or <var value> to the new dictionary, dropping the entry.
    //       If {{fn key value}} returns {{Some newValue}}, adds the entry <var key>: <var newValue> to the new dictionary.
    //       This function combines <fn Dict.filter> and <fn Dict.map>."
    //   fn =
    //     (function
    //     | state, _, [ DDict(_vtTODO, o); DFnVal b ] ->
    //       uply {
    //         let f (key : string) (data : Dval) : Ply<Option<Dval>> =
    //           uply {
    //             let args = NEList.ofList (DString key) [ data ]
    //             let! result = Interpreter.applyFnVal state b [] args

    //             match result with
    //             | DEnum(FQTypeName.Package id, _, _typeArgsDEnumTODO, "Some", [ o ]) when
    //               id = PackageIDs.Type.Stdlib.option
    //               ->
    //               return Some o

    //             | DEnum(FQTypeName.Package id, _, _typeArgsDEnumTODO, "None", []) when
    //               id = PackageIDs.Type.Stdlib.option
    //               ->
    //               return None

    //             | v ->
    //               let expectedType = TypeReference.option varB
    //               return!
    //                 TypeChecker.raiseFnValResultNotExpectedType
    //                   vmState.callStack
    //                   v
    //                   expectedType
    //           }

    //         let! result = Ply.Map.filterMapSequentially f o
    //         return TypeChecker.DvalCreator.dictFromMap VT.unknownTODO result
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    { name = fn "dictIsEmpty" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TBool
      description = "Returns {{true}} if the <param dict> contains no entries"
      fn =
        (function
        | _, _, _, [ DDict(_, dict) ] -> Ply(DBool(Map.isEmpty dict))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // { name = fn "dictMerge" 0
    //   typeParams = []
    //   parameters =
    //     [ Param.make "left" (TDict varA) ""; Param.make "right" (TDict varA) "" ]
    //   returnType = TDict varA
    //   description =
    //     "Returns a combined dictionary with both dictionaries' entries.
    //     If the same key exists in both <param left> and <param right>,
    //       it will have the value from <param right>."
    //   fn =
    //     (function
    //     | _, _, _, [ DDict(_vtTODO1, l); DDict(_vtTODO2, r) ] ->
    //       Map.mergeFavoringRight l r
    //       |> TypeChecker.DvalCreator.dictFromMap VT.unknownTODO
    //       |> Ply
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    { name = fn "dictSet" 0
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
        | _, _, _, [ DDict(vt, o); DString k; v ] -> DDict(vt, Map.add k v o) |> Ply
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
