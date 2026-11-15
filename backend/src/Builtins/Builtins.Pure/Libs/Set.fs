/// The few set operations that need the set's insides. Everything else in
/// `Stdlib.Set` is written in Dark over these.
module Builtins.Pure.Libs.Set

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module TypeChecker = LibExecution.TypeChecker

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval

let varA = TVariable "a"


let fns () : List<BuiltInFn> =
  [ { name = fn "setFromList" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TSet varA
      description =
        "Returns a <type Set> of the elements of <param list>. A repeated element is "
        + "kept once."
      fn =
        (function
        | _, vm, _, [| DList(vt, Items items) |] ->
          TypeChecker.DvalCreator.set vm.threadID vt items |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "setToList" 0
      typeParams = []
      parameters = [ Param.make "set" (TSet varA) "" ]
      returnType = TList varA
      description = "Returns the elements of <param set> in a <type List>, in order"
      fn =
        (function
        | _, _, _, [| DSet(vt, elements) |] ->
          DList(vt, Items(DictMap.keys elements)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "setSize" 0
      typeParams = []
      parameters = [ Param.make "set" (TSet varA) "" ]
      returnType = TInt64
      description = "Returns the number of elements in <param set>"
      fn =
        (function
        | _, _, _, [| DSet(_, elements) |] ->
          Ply(Dval.dint64 (int64 (DictMap.count elements)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "setContains" 0
      typeParams = []
      parameters = [ Param.make "set" (TSet varA) ""; Param.make "element" varA "" ]
      returnType = TBool
      description = "Returns {{true}} if <param element> is in <param set>"
      fn =
        (function
        | _, _, _, [| DSet(_, elements); element |] ->
          LibExecution.RuntimeTypes.Dval.assertUsableSetElement element
          Ply(DBool(DictMap.containsKey element elements))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "setAdd" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "set" (TSet varA) ""; Param.make "element" varA "" ]
      returnType = TSet varA
      description =
        "Returns <param set> with <param element> added. If it is already there, the "
        + "set is returned unchanged."
      fn =
        (function
        | _, vm, _, [| DSet(vt, elements); element |] ->
          TypeChecker.DvalCreator.setAdd vm.threadID vt elements element |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "setRemove" 0
      typeParams = []
      parameters = [ Param.make "set" (TSet varA) ""; Param.make "element" varA "" ]
      returnType = TSet varA
      description =
        "Returns <param set> without <param element>. If it is not there, the set is "
        + "returned unchanged."
      fn =
        (function
        | _, _, _, [| DSet(vt, elements); element |] ->
          LibExecution.RuntimeTypes.Dval.assertUsableSetElement element
          DSet(vt, DictMap.remove element elements) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "setUnion" 0
      typeParams = []
      parameters =
        [ Param.make "left" (TSet varA) ""; Param.make "right" (TSet varA) "" ]
      returnType = TSet varA
      description = "Returns the elements in either <param left> or <param right>"
      fn =
        (function
        | _, _, _, [| DSet(vt1, left); DSet(vt2, right) |] ->
          match VT.merge vt1 vt2 with
          | Ok vt -> DSet(vt, DictMap.union left right) |> Ply
          | Error() ->
            Exception.raiseInternal
              "Builtin.setUnion input sets somehow bypassed fn-arg type-checking"
              [ ("vt1", vt1); ("vt2", vt2) ]
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
