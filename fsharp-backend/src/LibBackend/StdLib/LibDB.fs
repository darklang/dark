module LibBackend.StdLib.LibDB

open Prelude
open LibExecution.RuntimeTypes

let fn = FQFnName.stdlibName

let err (str : string) = Value(Dval.errStr str)

let removedFunction = LibExecution.Errors.removedFunction

let varA = TVariable "a"
let dbType = TDB varA

let fns : List<BuiltInFn> =
  [ { name = fn "DB" "insert" 0
      parameters = [ Param.make "val" varA ""; Param.make "table" dbType "" ]
      returnType = varA
      description = "Insert `val` into `table`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "delete" 0
      parameters = [ Param.make "value" varA ""; Param.make "table" dbType "" ]
      returnType = TNull
      description = "Delete `value` from `table`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "deleteAll" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = TNull
      description = "Delete everything from `table`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "update" 0
      parameters = [ Param.make "value" varA ""; Param.make "table" dbType "" ]
      returnType = TNull
      description = "Update `table` value which has the same ID as `value`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "fetchBy" 0
      parameters =
        [ Param.make "value" TAny ""
          Param.make "field" TStr ""
          Param.make "table" dbType "" ]
      returnType = TList TAny
      description = "Fetch all the values in `table` whose `field` is `value`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "fetchOneBy" 0
      parameters =
        [ Param.make "value" TAny ""
          Param.make "field" TStr ""
          Param.make "table" dbType "" ]
      returnType = TAny
      description = "Fetch exactly one value in `table` whose `field` is `value`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "fetchByMany" 0
      parameters = [ Param.make "spec" varA ""; Param.make "table" dbType "" ]
      returnType = TList TAny
      description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "fetchOneByMany" 0
      parameters = [ Param.make "spec" varA ""; Param.make "table" dbType "" ]
      returnType = TAny
      description =
        "Fetch exactly one value from `table`, which have the same fields and values that `spec` has"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "fetchAll" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = TList TAny
      description = "Fetch all the values in `table`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "keys" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = TList TAny
      description = "Fetch all the keys in `table`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "schema" 0
      parameters = [ Param.make "table" dbType "" ]
      returnType = varA
      description = "Fetch all the values in `table`"
      fn = InProcess removedFunction
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) } ]
