/// (deprecated) StdLib functions for accessing and manipulating user datastores
module BackendOnlyStdLib.LibDB

open Prelude
open LibExecution.RuntimeTypes

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let removedFunction = LibExecution.Errors.removedFunction
let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
// CLEANUP: remove the obj type and use varA instead
let obj = TDict varA
let dbType = TDB varA

let fns : List<BuiltInFn> =
  [ { name = fn "DB" "insert" 0
      parameters = [ Param.make "val" obj ""; Param.make "table" dbType "" ]
      returnType = obj
      description = "Insert `val` into `table`"
      fn =
        function
        | state, [ v; table ] -> removedFunction state "DB::insert"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "delete" 0
      parameters = [ Param.make "value" obj ""; Param.make "table" dbType "" ]
      returnType = TNull
      description = "Delete `value` from `table`"
      fn =
        function
        | state, [ v; table ] -> removedFunction state "DB::delete"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "deleteAll" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = TNull
      description = "Delete everything from `table`"
      fn =
        function
        | state, [ table ] -> removedFunction state "DB::deleteAll"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "update" 0
      parameters = [ Param.make "value" obj ""; Param.make "table" dbType "" ]
      returnType = TNull
      description = "Update `table` value which has the same ID as `value`"
      fn =
        function
        | state, [ v; table ] -> removedFunction state "DB::update"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "fetchBy" 0
      parameters =
        [ Param.make "value" varA ""
          Param.make "field" TStr ""
          Param.make "table" dbType "" ]
      returnType = TList varA
      description = "Fetch all the values in `table` whose `field` is `value`"
      fn =
        function
        | state, [ v; field; table ] -> removedFunction state "DB::fetchBy"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "fetchOneBy" 0
      parameters =
        [ Param.make "value" varA ""
          Param.make "field" TStr ""
          Param.make "table" dbType "" ]
      returnType = varA
      description = "Fetch exactly one value in `table` whose `field` is `value`"
      fn =
        function
        | state, [ v; field; table ] -> removedFunction state "DB::fetchOneBy"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "fetchByMany" 0
      parameters = [ Param.make "spec" obj ""; Param.make "table" dbType "" ]
      returnType = TList varA
      description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has"
      fn =
        function
        | state, [ spec; table ] -> removedFunction state "DB::fetchByMany"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "fetchOneByMany" 0
      parameters = [ Param.make "spec" obj ""; Param.make "table" dbType "" ]
      returnType = varA
      description =
        "Fetch exactly one value from `table`, which have the same fields and values that `spec` has"
      fn =
        function
        | state, [ spec; table ] -> removedFunction state "DB::fetchOneByMany"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "fetchAll" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = TList varA
      description = "Fetch all the values in `table`"
      fn =
        function
        | state, [ table ] -> removedFunction state "DB::fetchAll"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "keys" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = TList varA
      description = "Fetch all the keys in `table`"
      fn =
        function
        | state, [ table ] -> removedFunction state "DB::keys"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") }


    { name = fn "DB" "schema" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = obj
      description = "Fetch all the values in `table`"
      fn =
        function
        | state, [ table ] -> removedFunction state "DB::schema"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = DeprecatedBecause("Old DB functions have been removed") } ]
