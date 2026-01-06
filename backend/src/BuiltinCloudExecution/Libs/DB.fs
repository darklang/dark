/// Builtin functions for accessing and manipulating user datastores
module BuiltinCloudExecution.Libs.DB

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module Builtin = LibExecution.Builtin
module PT = LibExecution.ProgramTypes

module UserDB = LibCloud.UserDB
module Db = LibDB.Db
module SqlCompiler = LibCloud.SqlCompiler
module PackageIDs = LibExecution.PackageIDs

/// Create a SqlSpec lookup from ExecutionState
/// Looks up the SqlSpec for both builtin and package functions
let createSqlSpecLookup
  (exeState : ExecutionState)
  : LibExecution.RTQueryCompiler.SqlSpecLookup =

  fun (fnName : FQFnName.FQFnName) ->
    match fnName with
    | FQFnName.Builtin builtinName ->
      // Look up in builtin functions
      match Map.tryFind builtinName exeState.fns.builtIn with
      | Some fn -> Some fn.sqlSpec
      | None -> None
    | FQFnName.Package _ ->
      // Package functions don't have SqlSpec - they delegate to builtins
      // Return None and let the inliner handle it
      None


/// Create a function body lookup for inlining package functions
let createFunctionBodyLookup
  (exeState : ExecutionState)
  : LibExecution.RTQueryCompiler.FunctionBodyLookup =

  fun (pkgId : FQFnName.Package) ->
    uply {
      match! exeState.fns.package pkgId with
      | Some fn -> return Some fn.body
      | None -> return None
    }


/// Create a partial evaluator for executing non-DB-dependent expressions at compile time.
/// This allows user-defined functions like `rossDOB()` to be called and their results
/// used as literal values in SQL queries.
let createPartialEvaluator
  (exeState : ExecutionState)
  (_vm : VMState)
  : LibExecution.RTQueryCompiler.PartialEvaluator =

  fun
      (fnName : FQFnName.FQFnName)
      (typeArgs : List<TypeReference>)
      (args : List<Dval>) ->
    uply {
      // Create instructions to call the function:
      // reg 0: the function reference
      // reg 1..n: the arguments
      // reg n+1: the result
      let fnReg = 0
      let resultReg = List.length args + 1
      let argRegs = [ 1 .. List.length args ]

      // Build instruction list
      let instructions = ResizeArray<Instruction>()

      // Load the function reference
      let appFn =
        { name = fnName
          typeArgs = typeArgs
          argsSoFar = []
          typeSymbolTable = Map.empty }
      instructions.Add(LoadVal(fnReg, DApplicable(AppNamedFn appFn)))

      // Load the arguments
      args |> List.iteri (fun i arg -> instructions.Add(LoadVal(i + 1, arg)))

      // Apply the function
      let argRegNEList =
        match argRegs with
        | [] -> NEList.singleton fnReg // No args - just pass the fn reg as placeholder
        | first :: rest -> NEList.ofList first rest
      if List.isEmpty args then
        // Zero-arg function - we need to apply with a unit arg to trigger execution
        instructions.Add(LoadVal(1, DUnit))
        instructions.Add(Apply(resultReg, fnReg, typeArgs, NEList.singleton 1))
      else
        instructions.Add(Apply(resultReg, fnReg, typeArgs, argRegNEList))

      let instrs : Instructions =
        { registerCount = resultReg + 1
          instructions = instructions |> Seq.toList
          resultIn = resultReg }

      // Create a new VMState to execute these instructions
      let miniVm = VMState.createWithoutTLID instrs

      // Execute and return the result
      let! result = LibExecution.Interpreter.execute exeState miniVm
      return result
    }


let tvar v = TVariable v

let dbType v = TDB(tvar v)

let valParam v = Param.make "val" (tvar v) ""
let keyParam = Param.make "key" TString ""
let keysParam = Param.make "keys" (TList TString) ""

let tableParam v = Param.make "table" (dbType v) ""

/// A function param that goes from `TVariable v` to `TBool`, to be used as a filter
let queryFilterParam v =
  Param.makeWithArgs
    "filter"
    (TFn(NEList.singleton (TVariable v), TBool))
    ""
    [ "value" ]


/// Collect all LoadValue instructions from lambda instructions and resolve them
let resolveLoadValues
  (exeState : ExecutionState)
  (lambdaImpl : LambdaImpl)
  : Ply.Ply<Map<FQValueName.FQValueName, Dval>> =
  uply {
    // Collect all value references from LoadValue instructions
    let valueRefs =
      lambdaImpl.instructions.instructions
      |> List.choose (function
        | Instruction.LoadValue(_, valueName) -> Some valueName
        | _ -> None)
      |> List.distinct

    // Resolve each value
    let! resolved =
      valueRefs
      |> Ply.List.mapSequentially (fun valueName ->
        uply {
          match valueName with
          | FQValueName.Builtin builtinName ->
            // Builtin values - look up in builtIn values
            match Map.tryFind builtinName exeState.values.builtIn with
            | Some v -> return Some(valueName, v.body)
            | None -> return None
          | FQValueName.Package pkgId ->
            let! pkg = exeState.values.package pkgId
            match pkg with
            | Some v -> return Some(valueName, v.body)
            | None -> return None
        })

    return resolved |> List.choose (fun x -> x) |> Map.ofList
  }


/// Look up a lambda from the VM cache by its exprId
let lookupLambdaImpl (vm : VMState) (exprId : id) : LambdaImpl =
  match
    Map.tryFind
      (vm.callFrames[vm.currentFrameID].executionPoint, exprId)
      vm.lambdaInstrCache
  with
  | Some impl -> impl
  | None ->
    match Map.tryFind (Source, exprId) vm.lambdaInstrCache with
    | Some impl -> impl
    | None ->
      Exception.raiseInternal "Lambda not found in cache" [ "exprId", exprId ]


/// Compile a lambda to SQL for use in DB queries
/// Raises RuntimeErrorException on error
let compileQueryLambda
  (exeState : ExecutionState)
  (vm : VMState)
  (appLambda : ApplicableLambda)
  : Ply.Ply<LibExecution.RTQueryCompiler.CompiledQuery> =
  uply {
    let lambdaImpl = lookupLambdaImpl vm appLambda.exprId
    let! resolvedValues = resolveLoadValues exeState lambdaImpl

    match
      LibExecution.RTQueryCompiler.compileLambda
        (createSqlSpecLookup exeState)
        (Some(createPartialEvaluator exeState vm))
        (Some(createFunctionBodyLookup exeState))
        lambdaImpl
        appLambda.closedRegisters
        resolvedValues
    with
    | Error err ->
      let fullMessage = SqlCompiler.errorTemplate + err
      return raiseUntargetedRTE (RuntimeError.Error.SqlCompiler fullMessage)
    | Ok compiled -> return compiled
  }


// let handleUnexpectedExceptionDuringQuery
//   (exeState : ExecutionState)
//   (dbname : string)
//   (query : LambdaImpl)
//   (e : System.Exception)
//   : Dval =
//   match e with
//   | RuntimeErrorException _ -> Exception.reraise e
//   | e ->
//     exeState.reportException
//       exeState
//       [ "dbName", dbname; "lambda", query; "db", exeState.program.dbs[dbname] ]
//       e
//     LibCloud.SqlCompiler.error "An error occurred while querying the Datastore"

let fns : List<BuiltInFn> =
  [ { name = fn "dbSet" 0
      typeParams = []
      parameters = [ valParam "a"; keyParam; tableParam "a" ]
      returnType = tvar "a"
      description =
        "Upsert <param val> into <param table>, accessible by <param key>"
      fn =
        (function
        | exeState, vm, _, [ value; DString key; DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let! id = UserDB.set exeState vm.threadID true db key value

            match id with
            | Ok _id -> return value
            | Error rte -> return raiseUntargetedRTE rte
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbGet" 0
      typeParams = []
      parameters = [ keyParam; tableParam "a" ]
      returnType = TypeReference.option (tvar "a")
      description = "Finds a value in <param table> by <param key>"
      fn =
        (function
        | exeState, vm, _, [ DString key; DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! result = UserDB.getOption exeState vm.threadID db key
            return TypeChecker.DvalCreator.option vm.threadID VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbGetMany" 0
      typeParams = []
      parameters = [ keysParam; tableParam "a" ]
      returnType = TypeReference.option (TList(tvar "a"))
      description =
        "Finds many values in <param table> by <param keys>. If all <param keys> are found, returns Some a list of [values], otherwise returns None (to ignore missing keys, use DB.etExisting)"
      fn =
        let valueType = VT.unknownDbTODO
        let optType = KTList valueType
        (function
        | exeState, vm, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let tst = Map.empty // TODO idk if this is reasonable

            let! items =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getMany exeState vm.threadID tst db

            if List.length items = List.length keys then
              return
                items
                |> TypeChecker.DvalCreator.list vm.threadID valueType
                |> Dval.optionSome optType
            else
              return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbGetExisting" 0
      typeParams = []
      parameters = [ keysParam; tableParam "a" ]
      returnType = TList(tvar "a")
      description =
        "Finds many values in <param table> by <param keys> (ignoring any missing items), returning a {{ [value] }} list of values"
      fn =
        (function
        | exeState, vm, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let tst = Map.empty // TODO idk if this is reasonable

            let! result =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getMany exeState vm.threadID tst db
            return
              result |> TypeChecker.DvalCreator.list vm.threadID VT.unknownDbTODO
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbGetManyWithKeys" 0
      typeParams = []
      parameters = [ keysParam; tableParam "a" ]
      returnType = TDict(tvar "a")
      description =
        "Finds many values in <param table> by <param keys>, returning a {{ {key:{value}, key2: {value2} } }} object of keys and values"
      fn =
        (function
        | exeState, vm, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let tst = Map.empty // TODO idk if this is reasonable

            let! result =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getManyWithKeys exeState vm.threadID tst db
            return TypeChecker.DvalCreator.dict vm.threadID VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbDelete" 0
      typeParams = []
      parameters = [ keyParam; tableParam "a" ]
      returnType = TUnit
      description = "Delete <param key> from <param table>"
      fn =
        (function
        | exeState, _, _, [ DString key; DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            do! UserDB.delete exeState db key
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbDeleteAll" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TUnit
      description = "Delete everything from <param table>"
      fn =
        (function
        | exeState, _, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            do! UserDB.deleteAll exeState db
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbGetAll" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TList(tvar "a")
      description = "Fetch all the values in <param table>"
      fn =
        (function
        | exeState, vm, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let tst = Map.empty // TODO idk if this is reasonable
            let! results = UserDB.getAll exeState vm.threadID tst db
            return
              results
              |> List.map snd
              |> TypeChecker.DvalCreator.list vm.threadID VT.unknownDbTODO
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbGetAllWithKeys" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TDict(tvar "a")
      description =
        "Fetch all the values in <param table>. Returns an object with key: value. ie. {key : value, key2: value2}"
      fn =
        (function
        | exeState, vm, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let tst = Map.empty // TODO idk if this is reasonable
            let! result = UserDB.getAll exeState vm.threadID tst db
            return TypeChecker.DvalCreator.dict vm.threadID VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbCount" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TInt64
      description = "Return the number of items stored in <param table>"
      fn =
        (function
        | exeState, _, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! (count : int) = UserDB.count exeState db
            return count |> int64 |> DInt64
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbGenerateKey" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns a random key suitable for use as a DB key"
      fn =
        (function
        | _, _, _, [ DUnit ] -> System.Guid.NewGuid() |> string |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbKeys" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TList TString
      description =
        "Fetch all the keys of entries in <param table>. Returns an list with strings"
      fn =
        (function
        | exeState, _, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! results = UserDB.getAllKeys exeState db
            return results |> List.map DString |> Dval.list KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbQuery" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TList(tvar "a")
      description =
        "Fetch all the values from <param table> for which filter returns true.
        Note that this does not check every value in <param table>, but rather is optimized to find data with indexes.
        Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState vm appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                DBQueryAll
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbQueryWithKey" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TDict(tvar "a")
      description =
        "Fetch all the values from <param table> for which filter returns true, returning {key : value} as a dict."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState vm appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                DBQueryWithKey
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbQueryOne" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TypeReference.option (tvar "a")
      description =
        "Fetch exactly one value from <param table> for which filter returns true. Returns Some if exactly one found, None otherwise."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState vm appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                DBQueryOne
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dbQueryCount" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TInt64
      description =
        "Return the number of items from <param table> for which filter returns true."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState vm appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                DBQueryCount
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = Builtin.make [] fns
