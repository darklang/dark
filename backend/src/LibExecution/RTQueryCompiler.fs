/// Compiles RT Instructions (from lambdas) to SQL at runtime.
///
/// TODO: Ideally this would operate at PT level rather than RT level,
/// but we need RT to access lambda bodies when passed as variables rather than inline.
module LibExecution.RTQueryCompiler

open Prelude
module RT = RuntimeTypes

/// Error message prefix shown to users when SQL compilation fails
let errorTemplate =
  "You're using our new experimental Datastore query compiler. It compiles your lambdas into optimized (and partially indexed) Datastore queries, which should be reasonably fast.\n\nUnfortunately, we hit a snag while compiling your lambda. We only support a subset of Darklang's functionality, but will be expanding it in the future.\n\nSome Darklang code is not supported in DB::query lambdas for now, and some of it won't be supported because it's an odd thing to do in a datastore query. If you think your operation should be supported, let us know in #general in Discord.\n\n  Error: "

/// Symbolic representation of what a register contains
type SymbolicValue =
  /// The lambda parameter (the DB row)
  | DBRow
  /// A field access on the DB row, e.g., p.name -> DBField ["name"]
  | DBField of path : List<string>
  /// A literal value that can be used as SQL parameter
  | Literal of RT.Dval
  /// Boolean AND
  | BoolAnd of SymbolicValue * SymbolicValue
  /// Boolean OR
  | BoolOr of SymbolicValue * SymbolicValue
  /// Boolean NOT
  | BoolNot of SymbolicValue
  /// A function call we can compile to SQL (stores the SqlSpec directly)
  | SqlFnCall of sqlSpec : RT.SqlSpec * args : List<SymbolicValue>
  /// Unknown - can't compile to SQL
  | Unknown of reason : string

/// Check if a symbolic value depends on the DB row (i.e., cannot be evaluated at compile time)
let rec dependsOnDBRow (sv : SymbolicValue) : bool =
  match sv with
  | DBRow -> true
  | DBField _ -> true
  | Literal _ -> false
  | BoolAnd(left, right) -> dependsOnDBRow left || dependsOnDBRow right
  | BoolOr(left, right) -> dependsOnDBRow left || dependsOnDBRow right
  | BoolNot inner -> dependsOnDBRow inner
  | SqlFnCall(_, args) -> args |> List.exists dependsOnDBRow
  | Unknown _ -> false // Unknown values are typically from failed lookups, not DB-dependent


/// Result of SQL compilation
type CompiledQuery =
  {
    sql : string
    /// Parameter values to bind (indexed as @p1, @p2, etc.)
    paramValues : List<RT.Dval>
  }

/// Error when compilation fails
type CompileError = string


// -------------------------
// Helper functions for ExecutionState lookups
// -------------------------

/// Look up the SqlSpec for a function
let getSqlSpec
  (exeState : RT.ExecutionState)
  (fnName : RT.FQFnName.FQFnName)
  : Option<RT.SqlSpec> =
  match fnName with
  | RT.FQFnName.Builtin builtinName ->
    Map.tryFind builtinName exeState.fns.builtIn |> Option.map _.sqlSpec
  | RT.FQFnName.Package _ ->
    // Package functions don't have SqlSpec - they delegate to builtins
    None

/// Look up a package function's body for inlining
let getFnBody
  (exeState : RT.ExecutionState)
  (pkgId : RT.FQFnName.Package)
  : Ply.Ply<Option<RT.Instructions>> =
  uply {
    let! fn = exeState.fns.package pkgId
    return Option.map (fun (f : RT.PackageFn.PackageFn) -> f.body) fn
  }

/// Partially evaluate a function call at compile time
/// Used when all arguments are known literals and we can compute the result
let partialEvaluate
  (exeState : RT.ExecutionState)
  (fnName : RT.FQFnName.FQFnName)
  (typeArgs : List<RT.TypeReference>)
  (args : List<RT.Dval>)
  : Ply.Ply<RT.Dval> =
  uply {
    // Build instructions to call the function
    let fnReg = 0
    let argRegs = [ 1 .. List.length args ]
    let resultReg = List.length args + 1

    let instructions = ResizeArray<RT.Instruction>()

    // Load the function reference
    let appFn : RT.ApplicableNamedFn =
      { name = fnName
        typeSymbolTable = Map.empty
        typeArgs = typeArgs
        argsSoFar = [] }
    instructions.Add(RT.LoadVal(fnReg, RT.DApplicable(RT.AppNamedFn appFn)))

    // Load the arguments
    args |> List.iteri (fun i arg -> instructions.Add(RT.LoadVal(i + 1, arg)))

    // Apply the function
    if List.isEmpty args then
      // Zero-arg function - apply with unit to trigger execution
      instructions.Add(RT.LoadVal(1, RT.DUnit))
      instructions.Add(RT.Apply(resultReg, fnReg, typeArgs, NEList.singleton 1))
    else
      let argRegNEList =
        match argRegs with
        | first :: rest -> NEList.ofList first rest
        | [] -> NEList.singleton fnReg
      instructions.Add(RT.Apply(resultReg, fnReg, typeArgs, argRegNEList))

    let instrs : RT.Instructions =
      { registerCount = resultReg + 1
        instructions = instructions |> Seq.toList
        resultIn = resultReg }

    // Execute
    let miniVm = RT.VMState.createWithoutTLID instrs
    return! Interpreter.execute exeState miniVm
  }


/// Compile a function call using its SqlSpec
let compileSqlSpec
  (sqlSpec : RT.SqlSpec)
  (argsSql : List<string>)
  : Result<string, CompileError> =
  let joinArgs args = String.concat ", " args
  match sqlSpec, argsSql with
  | RT.SqlBinOp op, [ a; b ] -> Ok $"({a} {op} {b})"
  | RT.SqlUnaryOp op, [ a ] -> Ok $"({op} {a})"
  | RT.SqlFunction fn, args ->
    let argsStr = joinArgs args
    Ok $"{fn}({argsStr})"
  | RT.SqlFunctionWithPrefixArgs(fn, prefix), args ->
    let argsStr = joinArgs (prefix @ args)
    Ok $"{fn}({argsStr})"
  | RT.SqlFunctionWithSuffixArgs(fn, suffix), args ->
    let argsStr = joinArgs (args @ suffix)
    Ok $"{fn}({argsStr})"
  | RT.SqlCallback2 cb, [ a; b ] -> Ok(cb a b)
  | RT.NotYetImplemented, _ -> Error "Function not yet implemented for SQL"
  | RT.NotQueryable, _ -> Error "Function is not queryable in SQL"
  | RT.QueryFunction, _ -> Error "Query functions cannot be used inside queries"
  | _, _ -> Error "Argument count mismatch for SQL function"


/// Try to extract a Dval from a Literal symbolic value
let tryExtractDval (sv : SymbolicValue) : Option<RT.Dval> =
  match sv with
  | Literal dv -> Some dv
  | _ -> None


/// State for symbolic execution
type SymbolicState =
  {
    registers : Map<int, SymbolicValue>
    /// Parameter values collected during compilation
    queryParams : List<RT.Dval>
    /// Current parameter counter
    paramCounter : int
  }

  static member empty = { registers = Map.empty; queryParams = []; paramCounter = 0 }

  member this.withReg(reg : int, value : SymbolicValue) =
    { this with registers = Map.add reg value this.registers }

  member this.getReg(reg : int) : SymbolicValue =
    Map.tryFind reg this.registers
    |> Option.defaultValue (Unknown $"Register {reg} not found")

  member this.addParam(dv : RT.Dval) : string * SymbolicState =
    let paramName = $"@p{this.paramCounter + 1}"
    let newState =
      { this with
          queryParams = this.queryParams @ [ dv ]
          paramCounter = this.paramCounter + 1 }
    (paramName, newState)


/// Convert a symbolic value to SQL
let rec symbolicToSql
  (state : SymbolicState)
  (sv : SymbolicValue)
  : Result<string * SymbolicState, CompileError> =
  match sv with
  | DBRow -> Error "Cannot use the entire row directly; access its fields"

  | DBField path ->
    // Strip any non-alphanumeric characters to prevent SQL injection via field names
    let safePath =
      path
      |> List.map (fun s ->
        System.Text.RegularExpressions.Regex.Replace(s, "[^a-zA-Z0-9_]", ""))
    let jsonPath = "$." + (safePath |> String.concat ".")
    Ok($"json_extract(data, '{jsonPath}')", state)

  | Literal dv ->
    match dv with
    | RT.DBool true -> Ok("1", state)
    | RT.DBool false -> Ok("0", state)
    | RT.DInt64 n -> Ok(string n, state)
    | RT.DFloat f -> Ok(string f, state)
    | RT.DString _ ->
      // Use a parameter to prevent SQL injection
      let (paramName, newState) = state.addParam dv
      Ok(paramName, newState)
    | RT.DChar c ->
      let (paramName, newState) = state.addParam (RT.DString c)
      Ok(paramName, newState)
    | RT.DDateTime _ ->
      let (paramName, newState) = state.addParam dv
      Ok(paramName, newState)
    | RT.DUuid _ ->
      let (paramName, newState) = state.addParam dv
      Ok(paramName, newState)
    | RT.DEnum _
    | RT.DRecord _ ->
      // For enums and records, wrap with json() for proper comparison with json_extract
      let (paramName, newState) = state.addParam dv
      Ok($"json({paramName})", newState)
    | other ->
      // For other types, try to use as parameter
      let (paramName, newState) = state.addParam other
      Ok(paramName, newState)

  | BoolAnd(left, right) ->
    match symbolicToSql state left with
    | Error e -> Error e
    | Ok(leftSql, state1) ->
      match symbolicToSql state1 right with
      | Error e -> Error e
      | Ok(rightSql, state2) -> Ok($"({leftSql} AND {rightSql})", state2)

  | BoolOr(left, right) ->
    match symbolicToSql state left with
    | Error e -> Error e
    | Ok(leftSql, state1) ->
      match symbolicToSql state1 right with
      | Error e -> Error e
      | Ok(rightSql, state2) -> Ok($"({leftSql} OR {rightSql})", state2)

  | BoolNot inner ->
    match symbolicToSql state inner with
    | Error e -> Error e
    | Ok(innerSql, newState) -> Ok($"(NOT {innerSql})", newState)

  | SqlFnCall(sqlSpec, args) ->
    let rec compileArgs st acc remaining =
      match remaining with
      | [] -> Ok(List.rev acc, st)
      | arg :: rest ->
        match symbolicToSql st arg with
        | Error e -> Error e
        | Ok(argSql, newSt) -> compileArgs newSt (argSql :: acc) rest

    match compileArgs state [] args with
    | Error e -> Error e
    | Ok(argsSql, finalState) ->
      match compileSqlSpec sqlSpec argsSql with
      | Ok sql -> Ok(sql, finalState)
      | Error e -> Error e

  | Unknown reason -> Error $"Cannot compile to SQL: {reason}"




/// Recursively inline and execute a function's body
let rec inlineAndExecute
  (exeState : RT.ExecutionState)
  (resolvedValues : Map<RT.FQValueName.FQValueName, RT.Dval>)
  (maxInlineDepth : int)
  (state : SymbolicState)
  (fnId : RT.FQFnName.Package)
  (args : List<SymbolicValue>)
  : Result<SymbolicValue, CompileError> =

  if maxInlineDepth <= 0 then
    Error "Max inline depth exceeded"
  else
    // Look up the function body synchronously
    let fnBodyOpt = getFnBody exeState fnId |> Ply.toTask |> _.Result
    match fnBodyOpt with
    | None -> Error $"Function not found for inlining: {fnId}"
    | Some fnBody ->
      // Create a new symbolic state with args bound to parameter registers
      let initialState =
        args
        |> List.mapi (fun i arg -> (i, arg))
        |> List.fold
          (fun s (reg, v) -> { s with registers = Map.add reg v s.registers })
          state

      // Execute the function body instructions
      let rec executeAll st instrs =
        match instrs with
        | [] -> Ok st
        | instr :: rest ->
          match
            executeInstruction exeState maxInlineDepth resolvedValues st 0 instr
          with
          | Error e -> Error e
          | Ok newState -> executeAll newState rest

      match executeAll initialState fnBody.instructions with
      | Error e -> Error e
      | Ok finalState ->
        // Return the result from the result register
        Ok(finalState.getReg fnBody.resultIn)


/// Symbolically execute a single instruction
and executeInstruction
  (exeState : RT.ExecutionState)
  (maxInlineDepth : int)
  (resolvedValues : Map<RT.FQValueName.FQValueName, RT.Dval>)
  (state : SymbolicState)
  (_lambdaParamReg : int)
  (instr : RT.Instruction)
  : Result<SymbolicState, CompileError> =

  match instr with
  | RT.LoadVal(reg, dv) -> Ok(state.withReg (reg, Literal dv))

  | RT.CopyVal(copyTo, copyFrom) -> Ok(state.withReg (copyTo, state.getReg copyFrom))

  | RT.GetRecordField(targetReg, recordReg, fieldName) ->
    let recordVal = state.getReg recordReg
    match recordVal with
    | DBRow -> Ok(state.withReg (targetReg, DBField [ fieldName ]))
    | DBField path -> Ok(state.withReg (targetReg, DBField(path @ [ fieldName ])))
    | Literal(RT.DRecord(_, _, _, fields)) ->
      // Field access on a literal record - extract the field value
      match Map.tryFind fieldName fields with
      | Some fieldVal -> Ok(state.withReg (targetReg, Literal fieldVal))
      | None ->
        Ok(
          state.withReg (targetReg, Unknown $"Field {fieldName} not found in record")
        )
    | Literal _ ->
      Ok(state.withReg (targetReg, Unknown "Field access on non-record literal"))
    | _ -> Ok(state.withReg (targetReg, Unknown $"Field access on {recordVal}"))

  | RT.And(createTo, lhsReg, rhsReg) ->
    let lhs = state.getReg lhsReg
    let rhs = state.getReg rhsReg
    Ok(state.withReg (createTo, BoolAnd(lhs, rhs)))

  | RT.Or(createTo, lhsReg, rhsReg) ->
    let lhs = state.getReg lhsReg
    let rhs = state.getReg rhsReg
    Ok(state.withReg (createTo, BoolOr(lhs, rhs)))

  | RT.Apply(createTo, thingToApplyReg, typeArgs, argRegs) ->
    // The thing to apply should be a function reference
    match state.getReg thingToApplyReg with
    | Literal(RT.DApplicable(RT.AppNamedFn { name = fnName })) ->
      let args = argRegs |> NEList.toList |> List.map state.getReg

      // Look up the SqlSpec for the function
      match getSqlSpec exeState fnName with
      | Some sqlSpec when sqlSpec.isQueryable () ->
        // Function has a queryable SqlSpec - use it
        Ok(state.withReg (createTo, SqlFnCall(sqlSpec, args)))
      | _ ->
        // Not a SQL function - try partial evaluation or inlining
        let argDvals = args |> List.choose tryExtractDval
        if List.length argDvals = List.length args then
          // All args are Literals - partial evaluate
          let result =
            partialEvaluate exeState fnName typeArgs argDvals
            |> Ply.toTask
            |> _.Result
          Ok(state.withReg (createTo, Literal result))
        else
          // Not all args are Literals - try to inline package functions
          match fnName with
          | RT.FQFnName.Package pkgId ->
            match
              inlineAndExecute
                exeState
                resolvedValues
                (maxInlineDepth - 1)
                state
                pkgId
                args
            with
            | Ok resultSv -> Ok(state.withReg (createTo, resultSv))
            | Error _ ->
              Ok(
                state.withReg (createTo, Unknown $"Cannot inline function: {fnName}")
              )
          | RT.FQFnName.Builtin { name = n } ->
            Ok(
              state.withReg (createTo, Unknown $"Unsupported builtin function: {n}")
            )

    | other ->
      Ok(state.withReg (createTo, Unknown $"Apply on non-function: {other}"))

  | RT.CheckLetPatternAndExtractVars(valueReg, pat) ->
    // Handle let bindings - extract variables from the pattern
    let rec extractPattern
      (st : SymbolicState)
      (srcVal : SymbolicValue)
      (p : RT.LetPattern)
      =
      match p with
      | RT.LPVariable extractTo -> st.withReg (extractTo, srcVal)
      | RT.LPWildcard -> st
      | RT.LPUnit -> st
      | RT.LPTuple(first, second, rest) ->
        // For tuples, extract elements if srcVal is a literal tuple
        match srcVal with
        | Literal(RT.DTuple(v1, v2, vrest)) ->
          let allPatterns = first :: second :: rest
          let allValues = v1 :: v2 :: vrest
          List.zip allPatterns allValues
          |> List.fold (fun s (pat, v) -> extractPattern s (Literal v) pat) st
        | _ ->
          // Can't extract from non-literal tuple
          st
    let srcVal = state.getReg valueReg
    Ok(extractPattern state srcVal pat)

  // Instructions we can skip for SQL compilation
  | RT.JumpBy _ -> Ok state
  | RT.JumpByIfFalse _ -> Ok state

  // Instructions we can't handle
  | RT.CreateLambda _ -> Error "Nested lambdas not supported in SQL queries"
  | RT.CreateList(createTo, itemRegs) ->
    // Allow list creation when all items are literals (for partial evaluation)
    let rec collectLiterals acc regs =
      match regs with
      | [] -> Ok(List.rev acc)
      | reg :: rest ->
        match state.getReg reg with
        | Literal dv -> collectLiterals (dv :: acc) rest
        | _ ->
          Error "List elements must be literal values for SQL partial evaluation"
    match collectLiterals [] itemRegs with
    | Error e -> Error e
    | Ok items ->
      let listDval = RT.DList(RT.ValueType.Unknown, items)
      Ok(state.withReg (createTo, Literal listDval))
  | RT.CreateDict _ -> Error "Dict creation not supported in SQL queries"
  | RT.CreateRecord(createTo, typeName, _typeArgs, fields) ->
    // Build a record Dval from the field values (must all be literals)
    let rec buildFields acc remaining =
      match remaining with
      | [] -> Ok(Map.ofList (List.rev acc))
      | (fieldName, reg) :: rest ->
        match state.getReg reg with
        | Literal dv -> buildFields ((fieldName, dv) :: acc) rest
        | _ ->
          Error
            $"Record field {fieldName} must be a literal value for SQL comparison"
    match buildFields [] fields with
    | Error e -> Error e
    | Ok fieldMap ->
      // Use empty type args for SQL parameter serialization
      let recordDval = RT.DRecord(typeName, typeName, [], fieldMap)
      Ok(state.withReg (createTo, Literal recordDval))

  | RT.CreateEnum(createTo, typeName, _typeArgs, caseName, fieldRegs) ->
    // Build an enum Dval from the field values (must all be literals)
    let rec buildFields acc remaining =
      match remaining with
      | [] -> Ok(List.rev acc)
      | reg :: rest ->
        match state.getReg reg with
        | Literal dv -> buildFields (dv :: acc) rest
        | _ -> Error "Enum field must be a literal value for SQL comparison"
    match buildFields [] fieldRegs with
    | Error e -> Error e
    | Ok fields ->
      // Use empty type args for SQL parameter serialization
      let enumDval = RT.DEnum(typeName, typeName, [], caseName, fields)
      Ok(state.withReg (createTo, Literal enumDval))

  | RT.CreateTuple(createTo, firstReg, secondReg, restRegs) ->
    // Build a tuple Dval from the element values (must all be literals)
    let rec collectLiterals acc regs =
      match regs with
      | [] -> Ok(List.rev acc)
      | reg :: rest ->
        match state.getReg reg with
        | Literal dv -> collectLiterals (dv :: acc) rest
        | _ -> Error "Tuple element must be a literal value"
    match collectLiterals [] (firstReg :: secondReg :: restRegs) with
    | Error e -> Error e
    | Ok(first :: second :: rest) ->
      let tupleDval = RT.DTuple(first, second, rest)
      Ok(state.withReg (createTo, Literal tupleDval))
    | Ok _ -> Error "Tuple must have at least 2 elements"
  | RT.CreateString(createTo, segments) ->
    // Handle string creation/interpolation
    // Try to evaluate all segments to string literals
    let rec processSegments acc segs =
      match segs with
      | [] -> Ok(List.rev acc)
      | RT.Text text :: rest -> processSegments (text :: acc) rest
      | RT.Interpolated reg :: rest ->
        match state.getReg reg with
        | Literal(RT.DString s) -> processSegments (s :: acc) rest
        | Literal _dv ->
          // Non-string literal in interpolation - convert to string representation
          Error "String interpolation requires string values"
        | _ -> Error "String interpolation"

    match processSegments [] segments with
    | Ok parts ->
      // All segments are resolved literals - concatenate to a single string
      let concatenated = String.concat "" parts
      Ok(state.withReg (createTo, Literal(RT.DString concatenated)))
    | Error e -> Error $"Cannot compile to SQL: {e}"

  | RT.LoadValue(targetReg, valueName) ->
    // Look up the package value
    match Map.tryFind valueName resolvedValues with
    | Some dv -> Ok(state.withReg (targetReg, Literal dv))
    | None -> Error $"Value {valueName} not resolved for SQL compilation"
  | RT.CloneRecordWithUpdates _ ->
    Error "Record updates not supported in SQL queries"
  | RT.CheckMatchPatternAndExtractVars _ ->
    Error "Match expressions not supported in SQL queries"
  | RT.MatchUnmatched _ -> Error "Match expressions not supported in SQL queries"
  | RT.RaiseNRE _ -> Error "Name resolution errors not supported in SQL queries"
  | RT.VarNotFound(_, varName) -> Error $"This variable is not defined: {varName}"
  | RT.CheckIfFirstExprIsUnit _ -> Ok state


/// Compile a lambda's instructions to SQL
let compileLambda
  (exeState : RT.ExecutionState)
  (lambdaImpl : RT.LambdaImpl)
  (closedValues : List<RT.Register * RT.Dval>)
  (resolvedValues : Map<RT.FQValueName.FQValueName, RT.Dval>)
  : Result<CompiledQuery, CompileError> =

  // Max inline depth to prevent infinite recursion
  let maxInlineDepth = 10

  // Initialize state with closed-over values and the lambda parameter
  let lambdaParamReg =
    match lambdaImpl.patterns.head with
    | RT.LPVariable reg -> reg
    | RT.LPWildcard -> 0
    | RT.LPUnit -> 0
    | RT.LPTuple _ -> 0

  let initialState =
    // Start with DBRow in the lambda parameter register
    let state = SymbolicState.empty.withReg (lambdaParamReg, DBRow)
    // Add closed-over values as literals
    closedValues
    |> List.fold
      (fun (st : SymbolicState) (reg, dv) -> st.withReg (reg, Literal dv))
      state

  // Execute each instruction symbolically
  let rec executeAll state instrs =
    match instrs with
    | [] -> Ok state
    | instr :: rest ->
      match
        executeInstruction
          exeState
          maxInlineDepth
          resolvedValues
          state
          lambdaParamReg
          instr
      with
      | Error e -> Error e
      | Ok newState -> executeAll newState rest

  // Validate that a symbolic value represents a boolean expression
  let validateBoolResult (sv : SymbolicValue) : Result<unit, CompileError> =
    match sv with
    | Literal(RT.DBool _) -> Ok()
    | BoolAnd _ -> Ok()
    | BoolOr _ -> Ok()
    | BoolNot _ -> Ok()
    | SqlFnCall _ -> Ok() // Assume SQL functions return appropriate types
    | DBField _ -> Ok() // DBField could be a boolean field (e.g., p.human)
    | Literal(RT.DString s) ->
      Error $"Incorrect type in String \"{s}\", expected Bool, but got a String"
    | Literal(RT.DInt64 n) ->
      Error $"Incorrect type in Int64 {n}, expected Bool, but got an Int64"
    | Literal(RT.DFloat f) ->
      Error $"Incorrect type in Float {f}, expected Bool, but got a Float"
    | Literal other ->
      let typeName =
        match other with
        | RT.DUnit -> "Unit"
        | RT.DBool _ -> "Bool"
        | RT.DInt8 _ -> "Int8"
        | RT.DUInt8 _ -> "UInt8"
        | RT.DInt16 _ -> "Int16"
        | RT.DUInt16 _ -> "UInt16"
        | RT.DInt32 _ -> "Int32"
        | RT.DUInt32 _ -> "UInt32"
        | RT.DInt64 _ -> "Int64"
        | RT.DUInt64 _ -> "UInt64"
        | RT.DInt128 _ -> "Int128"
        | RT.DUInt128 _ -> "UInt128"
        | RT.DFloat _ -> "Float"
        | RT.DChar _ -> "Char"
        | RT.DString _ -> "String"
        | RT.DDateTime _ -> "DateTime"
        | RT.DUuid _ -> "Uuid"
        | RT.DList _ -> "List"
        | RT.DDict _ -> "Dict"
        | RT.DTuple _ -> "Tuple"
        | RT.DRecord _ -> "Record"
        | RT.DEnum _ -> "Enum"
        | RT.DApplicable _ -> "Function"
        | RT.DDB _ -> "DB"
      Error $"Incorrect type, expected Bool, but got {typeName}"
    | DBRow -> Error "Cannot use the entire row as a boolean result"
    | Unknown reason -> Error $"Cannot compile to SQL: {reason}"

  match executeAll initialState lambdaImpl.instructions.instructions with
  | Error e -> Error e
  | Ok finalState ->
    // Get the result from the result register
    let resultReg = lambdaImpl.instructions.resultIn
    let resultVal = finalState.getReg resultReg

    // Validate the result is a boolean expression
    match validateBoolResult resultVal with
    | Error e -> Error e
    | Ok() ->
      // Convert to SQL
      match symbolicToSql finalState resultVal with
      | Error e -> Error e
      | Ok(sql, stateWithParams) ->
        Ok { sql = sql; paramValues = stateWithParams.queryParams }
