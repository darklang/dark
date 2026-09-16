// 3_ANF_to_MIR.fs - MIR Transformation (Pass 3)
//
// Transforms ANF into MIR with Control Flow Graph (CFG).
//
// Algorithm:
// - Converts ANF expressions into MIR CFG with basic blocks
// - Maps ANF temporary variables to MIR virtual registers
// - Converts ANF If expressions into conditional branches with basic blocks
// - Each basic block has a label, instructions, and a terminator
//
// Example (with if):
//   if x then 10 else 20
//   →
//   entry:
//     branch x, then_block, else_block
//   then_block:
//     v0 <- 10
//     jump join_block
//   else_block:
//     v1 <- 20
//     jump join_block
//   join_block:
//     v2 <- phi(v0, v1)  // (simplified - actual implementation uses registers)
//     ret v2

module ANF_to_MIR

open ResultList

/// Helper to create VariantInfo record
let private mkVariantInfo (name: string) (tag: int) (payload: AST.Type option) : MIR.VariantInfo =
    { MIR.VariantInfo.Name = name; MIR.VariantInfo.Tag = tag; MIR.VariantInfo.Payload = payload }

/// Helper to create TypeVariants record
let private mkTypeVariants (typeParams: string list) (variants: MIR.VariantInfo list) : MIR.TypeVariants =
    { MIR.TypeVariants.TypeParams = typeParams; MIR.TypeVariants.Variants = variants }

/// Helper to create RecordField record
let private mkRecordField (name: string) (typ: AST.Type) : MIR.RecordField =
    { MIR.RecordField.Name = name; MIR.RecordField.Type = typ }

/// Build VariantRegistry from VariantLookup
/// VariantLookup: variantName -> (typeName, typeParams, tagIndex, payloadType)
/// VariantRegistry: typeName -> TypeVariants (with named record types)
let buildVariantRegistry (variantLookup: AST_to_ANF.VariantLookup) : MIR.VariantRegistry =
    variantLookup
    |> Map.toList
    |> List.map (fun (variantName, (typeName, typeParams, tagIndex, payloadType)) ->
        (typeName, typeParams, (variantName, tagIndex, payloadType)))
    |> List.groupBy (fun (typeName, _, _) -> typeName)
    |> List.map (fun (typeName, entries) ->
        // Get typeParams from first entry (all entries for same type have same params)
        let typeParams = entries |> List.head |> (fun (_, tp, _) -> tp)
        let variants =
            entries
            |> List.map (fun (_, _, (name, tag, payload)) -> mkVariantInfo name tag payload)
            |> List.sortBy (fun v -> v.Tag)
        (typeName, mkTypeVariants typeParams variants))
    |> Map.ofList

/// Build RecordRegistry from TypeReg
/// TypeReg: typeName -> (fieldName, fieldType) list
/// RecordRegistry: typeName -> RecordField list
let buildRecordRegistry (typeReg: Map<string, (string * AST.Type) list>) : MIR.RecordRegistry =
    typeReg
    |> Map.map (fun _typeName fields ->
        fields |> List.map (fun (name, typ) -> mkRecordField name typ))

/// Convert ANF.BinOp to MIR.BinOp
let convertBinOp (op: ANF.BinOp) : MIR.BinOp =
    match op with
    | ANF.Add -> MIR.Add
    | ANF.Sub -> MIR.Sub
    | ANF.Mul -> MIR.Mul
    | ANF.Div -> MIR.Div
    | ANF.Mod -> MIR.Mod
    | ANF.Shl -> MIR.Shl
    | ANF.Shr -> MIR.Shr
    | ANF.BitAnd -> MIR.BitAnd
    | ANF.BitOr -> MIR.BitOr
    | ANF.BitXor -> MIR.BitXor
    | ANF.Eq -> MIR.Eq
    | ANF.Neq -> MIR.Neq
    | ANF.Lt -> MIR.Lt
    | ANF.Gt -> MIR.Gt
    | ANF.Lte -> MIR.Lte
    | ANF.Gte -> MIR.Gte
    | ANF.And -> MIR.And
    | ANF.Or -> MIR.Or

/// Convert ANF.UnaryOp to MIR.UnaryOp
let convertUnaryOp (op: ANF.UnaryOp) : MIR.UnaryOp =
    match op with
    | ANF.Neg -> MIR.Neg
    | ANF.Not -> MIR.Not
    | ANF.BitNot -> MIR.BitNot

/// Precomputed descriptions for primitive ops (avoids formatting on hot path)
let private binOpDescription (op: ANF.BinOp) : string =
    match op with
    | ANF.Add -> "Prim Add"
    | ANF.Sub -> "Prim Sub"
    | ANF.Mul -> "Prim Mul"
    | ANF.Div -> "Prim Div"
    | ANF.Mod -> "Prim Mod"
    | ANF.Shl -> "Prim Shl"
    | ANF.Shr -> "Prim Shr"
    | ANF.BitAnd -> "Prim BitAnd"
    | ANF.BitOr -> "Prim BitOr"
    | ANF.BitXor -> "Prim BitXor"
    | ANF.Eq -> "Prim Eq"
    | ANF.Neq -> "Prim Neq"
    | ANF.Lt -> "Prim Lt"
    | ANF.Gt -> "Prim Gt"
    | ANF.Lte -> "Prim Lte"
    | ANF.Gte -> "Prim Gte"
    | ANF.And -> "Prim And"
    | ANF.Or -> "Prim Or"

/// Precomputed descriptions for unary ops (avoids formatting on hot path)
let private unaryOpDescription (op: ANF.UnaryOp) : string =
    match op with
    | ANF.Neg -> "UnaryPrim Neg"
    | ANF.Not -> "UnaryPrim Not"
    | ANF.BitNot -> "UnaryPrim BitNot"

/// Sequence a list of Results into a Result of list
let sequenceResults (results: Result<'a, string> list) : Result<'a list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | r :: rest ->
            match r with
            | Ok v -> loop (v :: acc) rest
            | Error e -> Error e
    loop [] results

/// Append a list of instructions (in order) to a reversed instruction list
let private appendInstrsRev (instrs: MIR.Instr list) (revInstrs: MIR.Instr list) : MIR.Instr list =
    (List.rev instrs) @ revInstrs

/// Build a dense type lookup array for TempIds up to maxId
let private buildTypeById (maxId: int) (typeMap: ANF.TypeMap) : AST.Type option array =
    if maxId < 0 then
        [||]
    else
        let arr = Array.create (maxId + 1) None
        typeMap
        |> Map.iter (fun (ANF.TempId id) typ ->
            if id >= 0 && id <= maxId then
                arr.[id] <- Some typ)
        arr

/// Map ANF TempId to MIR virtual register
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Find the maximum TempId in an atom (returns -1 if no TempId)
let maxTempIdInAtom (atom: ANF.Atom) : int =
    match atom with
    | ANF.Var (ANF.TempId id) -> id
    | _ -> -1

/// Find the maximum TempId in a CExpr
let maxTempIdInCExpr (cexpr: ANF.CExpr) : int =
    match cexpr with
    | ANF.Atom atom -> maxTempIdInAtom atom
    | ANF.TypedAtom (atom, _) -> maxTempIdInAtom atom
    | ANF.Prim (_, left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.UnaryPrim (_, atom) -> maxTempIdInAtom atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAtom thenVal) (maxTempIdInAtom elseVal))
    | ANF.Call (_, args)
    | ANF.BorrowedCall (_, args) ->
        args |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.TailCall (_, args) ->
        args |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.IndirectCall (func, args) ->
        max (maxTempIdInAtom func) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.IndirectTailCall (func, args) ->
        max (maxTempIdInAtom func) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.TupleAlloc atoms ->
        atoms |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.TupleGet (tuple, _) -> maxTempIdInAtom tuple
    | ANF.StringConcat (left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.RefCountInc (atom, _, _) -> maxTempIdInAtom atom
    | ANF.RefCountDec (atom, _, _) -> maxTempIdInAtom atom
    | ANF.Print (atom, _) -> maxTempIdInAtom atom
    | ANF.RuntimeError _ -> -1
    | ANF.ClosureAlloc (_, captures) ->
        captures |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.ClosureCall (closure, args) ->
        max (maxTempIdInAtom closure) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.ClosureTailCall (closure, args) ->
        max (maxTempIdInAtom closure) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.FileReadText path -> maxTempIdInAtom path
    | ANF.FileExists path -> maxTempIdInAtom path
    | ANF.FileWriteText (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)
    | ANF.FileAppendText (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)
    | ANF.FileDelete path -> maxTempIdInAtom path
    | ANF.FileSetExecutable path -> maxTempIdInAtom path
    | ANF.FileWriteFromPtr (path, ptr, length) -> max (maxTempIdInAtom path) (max (maxTempIdInAtom ptr) (maxTempIdInAtom length))
    | ANF.RawAlloc numBytes -> maxTempIdInAtom numBytes
    | ANF.RawFree ptr -> maxTempIdInAtom ptr
    | ANF.RawGet (ptr, offset, _) -> max (maxTempIdInAtom ptr) (maxTempIdInAtom offset)
    | ANF.RawGetByte (ptr, offset) -> max (maxTempIdInAtom ptr) (maxTempIdInAtom offset)
    | ANF.RawSet (ptr, offset, value, _) -> max (maxTempIdInAtom ptr) (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
    | ANF.RawSetByte (ptr, offset, value) -> max (maxTempIdInAtom ptr) (max (maxTempIdInAtom offset) (maxTempIdInAtom value))
    | ANF.FloatSqrt atom -> maxTempIdInAtom atom
    | ANF.FloatAbs atom -> maxTempIdInAtom atom
    | ANF.FloatNeg atom -> maxTempIdInAtom atom
    | ANF.Int64ToFloat atom -> maxTempIdInAtom atom
    | ANF.FloatToInt64 atom -> maxTempIdInAtom atom
    | ANF.FloatToBits atom -> maxTempIdInAtom atom
    | ANF.RefCountIncString str -> maxTempIdInAtom str
    | ANF.RefCountDecString str -> maxTempIdInAtom str
    | ANF.RandomInt64 -> -1  // No atoms, so no TempIds
    | ANF.DateNow -> -1      // No atoms, so no TempIds
    | ANF.FloatToString atom -> maxTempIdInAtom atom

/// Find the maximum TempId in an AExpr
let rec maxTempIdInAExpr (expr: ANF.AExpr) : int =
    match expr with
    | ANF.Let (ANF.TempId id, cexpr, body) ->
        max id (max (maxTempIdInCExpr cexpr) (maxTempIdInAExpr body))
    | ANF.Return atom -> maxTempIdInAtom atom
    | ANF.If (cond, thenBranch, elseBranch) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAExpr thenBranch) (maxTempIdInAExpr elseBranch))

/// Find the maximum TempId in a function
let maxTempIdInFunction (func: ANF.Function) : int =
    let paramMax =
        func.TypedParams
        |> List.map (fun tp -> let (ANF.TempId id) = tp.Id in id)
        |> List.fold max -1
    max paramMax (maxTempIdInAExpr func.Body)

/// Find the maximum TempId in an ANF program
let maxTempIdInProgram (program: ANF.Program) : int =
    let (ANF.Program (functions, mainExpr)) = program
    let funcMax =
        functions
        |> List.map maxTempIdInFunction
        |> List.fold max -1
    let mainMax = maxTempIdInAExpr mainExpr
    max funcMax mainMax

/// Helper to check if an atom is a float value
let isFloatAtom (floatRegs: Set<int>) (atom: ANF.Atom) : bool =
    match atom with
    | ANF.FloatLiteral _ -> true
    | ANF.Var (ANF.TempId id) -> Set.contains id floatRegs
    | _ -> false

/// Helper to check if a CExpr produces a float value
/// returnTypeReg: map from function name to return type (for checking Call results)
let cexprProducesFloat (floatRegs: Set<int>) (returnTypeReg: Map<string, AST.Type>) (cexpr: ANF.CExpr) : bool =
    match cexpr with
    | ANF.Prim (op, left, right) ->
        // Comparisons and boolean ops always produce Bool, not Float
        match op with
        | ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte
        | ANF.And | ANF.Or -> false
        // Arithmetic ops produce float if either operand is float
        | ANF.Add | ANF.Sub | ANF.Mul | ANF.Div | ANF.Mod
        | ANF.Shl | ANF.Shr | ANF.BitAnd | ANF.BitOr | ANF.BitXor ->
            isFloatAtom floatRegs left || isFloatAtom floatRegs right
    | ANF.FloatSqrt _ | ANF.FloatAbs _ | ANF.FloatNeg _ | ANF.Int64ToFloat _ -> true
    | ANF.Atom atom -> isFloatAtom floatRegs atom
    | ANF.IfValue (_, thenAtom, _) ->
        // IfValue produces a float if either branch produces a float
        // (then and else should have the same type, so we check then)
        isFloatAtom floatRegs thenAtom
    | ANF.Call (funcName, _)
    | ANF.BorrowedCall (funcName, _)
    | ANF.TailCall (funcName, _) ->
        // Check if the called function returns a float
        match Map.tryFind funcName returnTypeReg with
        | Some AST.TFloat64 -> true
        | _ -> false
    | ANF.IndirectCall _ | ANF.IndirectTailCall _ ->
        // Indirect calls - we don't know the return type, assume not float
        false
    | ANF.ClosureCall _ | ANF.ClosureTailCall _ ->
        // Closure calls - we don't know the return type, assume not float
        false
    | _ -> false

/// Collect direct callee names referenced in a CExpr (only direct calls affect return types)
let calleeNamesInCExpr (cexpr: ANF.CExpr) : Set<string> =
    match cexpr with
    | ANF.Call (funcName, _) -> Set.singleton funcName
    | ANF.BorrowedCall (funcName, _) -> Set.singleton funcName
    | ANF.TailCall (funcName, _) -> Set.singleton funcName
    | _ -> Set.empty

/// Collect direct callee names referenced in an AExpr
let rec calleeNamesInAExpr (expr: ANF.AExpr) : Set<string> =
    match expr with
    | ANF.Return _ -> Set.empty
    | ANF.Let (_, cexpr, rest) ->
        Set.union (calleeNamesInCExpr cexpr) (calleeNamesInAExpr rest)
    | ANF.If (_, thenBranch, elseBranch) ->
        Set.union (calleeNamesInAExpr thenBranch) (calleeNamesInAExpr elseBranch)

/// Analyze return statements in an ANF expression, tracking float temps
/// Returns the type of the expression's result
/// returnTypeReg: map from function name to return type (for checking Call results)
let rec getExprReturnType (floatRegs: Set<int>) (typeMap: ANF.TypeMap) (returnTypeReg: Map<string, AST.Type>) (expr: ANF.AExpr) : AST.Type =
    match expr with
    | ANF.Return atom ->
        match atom with
        | ANF.FloatLiteral _ -> AST.TFloat64
        | ANF.IntLiteral n -> ANF.sizedIntToType n  // Use actual type from SizedInt
        | ANF.BoolLiteral _ -> AST.TBool
        | ANF.StringLiteral _ -> AST.TString
        | ANF.UnitLiteral -> AST.TUnit
        | ANF.Var (ANF.TempId id) ->
            if Set.contains id floatRegs then AST.TFloat64
            else
                match Map.tryFind (ANF.TempId id) typeMap with
                | Some t -> t
                | None -> Crash.crash $"getExprReturnType: unknown type for TempId {id}"
        | ANF.FuncRef _ -> AST.TInt64
    | ANF.Let (ANF.TempId destId, cexpr, rest) ->
        // Update floatRegs if this binding produces a float
        let floatRegs' =
            if cexprProducesFloat floatRegs returnTypeReg cexpr then
                Set.add destId floatRegs
            else
                floatRegs
        getExprReturnType floatRegs' typeMap returnTypeReg rest
    | ANF.If (_, thenBranch, _) -> getExprReturnType floatRegs typeMap returnTypeReg thenBranch

/// Compute return type and maximum TempId in a single pass over an expression
let rec getReturnTypeAndMaxTempId
    (floatRegs: Set<int>)
    (typeMap: ANF.TypeMap)
    (returnTypeReg: Map<string, AST.Type>)
    (expr: ANF.AExpr)
    : int * AST.Type =
    match expr with
    | ANF.Return atom ->
        let maxId = maxTempIdInAtom atom
        let retType =
            match atom with
            | ANF.FloatLiteral _ -> AST.TFloat64
            | ANF.IntLiteral n -> ANF.sizedIntToType n
            | ANF.BoolLiteral _ -> AST.TBool
            | ANF.StringLiteral _ -> AST.TString
            | ANF.UnitLiteral -> AST.TUnit
            | ANF.Var (ANF.TempId id) ->
                if Set.contains id floatRegs then AST.TFloat64
                else
                    match Map.tryFind (ANF.TempId id) typeMap with
                    | Some t -> t
                    | None -> AST.TInt64
            | ANF.FuncRef _ -> AST.TInt64
        (maxId, retType)
    | ANF.Let (ANF.TempId destId, cexpr, rest) ->
        let maxInCExpr = maxTempIdInCExpr cexpr
        let floatRegs' =
            if cexprProducesFloat floatRegs returnTypeReg cexpr then
                Set.add destId floatRegs
            else
                floatRegs
        let (maxRest, retType) = getReturnTypeAndMaxTempId floatRegs' typeMap returnTypeReg rest
        (max destId (max maxInCExpr maxRest), retType)
    | ANF.If (cond, thenBranch, elseBranch) ->
        let maxCond = maxTempIdInAtom cond
        let (maxThen, thenType) = getReturnTypeAndMaxTempId floatRegs typeMap returnTypeReg thenBranch
        let (maxElse, _elseType) = getReturnTypeAndMaxTempId floatRegs typeMap returnTypeReg elseBranch
        (max maxCond (max maxThen maxElse), thenType)

/// Compute return type for an ANF function by analyzing return statements
/// Uses typeReg to determine which parameters are floats
/// Uses returnTypeReg to check return types of called functions
let computeReturnTypeWithReg (anfFunc: ANF.Function) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) (returnTypeReg: Map<string, AST.Type>) : AST.Type =
    // Get float parameter IDs for this function (types are now bundled in TypedParams)
    let floatParamIds =
        anfFunc.TypedParams
        |> List.filter (fun tp -> tp.Type = AST.TFloat64)
        |> List.map (fun tp -> let (ANF.TempId id) = tp.Id in id)
        |> Set.ofList
    getExprReturnType floatParamIds typeMap returnTypeReg anfFunc.Body

/// Build a map from function name to return type for all functions
/// Uses iterative fixpoint algorithm since functions may call each other
/// externalReturnTypes: return types for functions not in `functions` (e.g., specialized functions compiled elsewhere)
let buildReturnTypeReg
    (functions: ANF.Function list)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (externalReturnTypes: Map<string, AST.Type>)
    : Map<string, AST.Type> =
    // Worklist algorithm: only re-evaluate dependents when a return type changes
    let funcByName = functions |> List.map (fun f -> (f.Name, f)) |> Map.ofList
    let calleesByFunc =
        functions
        |> List.map (fun f -> (f.Name, calleeNamesInAExpr f.Body))
        |> Map.ofList
    let callersByFunc =
        calleesByFunc
        |> Map.fold (fun acc caller callees ->
            callees
            |> Set.fold (fun acc callee ->
                let existing = Map.tryFind callee acc |> Option.defaultValue []
                Map.add callee (caller :: existing) acc) acc) Map.empty

    let initialQueue = functions |> List.map (fun f -> f.Name)
    let initialQueued = Set.ofList initialQueue
    let maxSteps = 100 * max 1 functions.Length

    let rec loop
        (queue: string list)
        (queued: Set<string>)
        (currentReg: Map<string, AST.Type>)
        (stepsRemaining: int)
        : Map<string, AST.Type> =
        match queue with
        | [] -> currentReg
        | _ when stepsRemaining <= 0 -> currentReg
        | funcName :: rest ->
            let queued' = Set.remove funcName queued
            match Map.tryFind funcName funcByName with
            | None ->
                loop rest queued' currentReg (stepsRemaining - 1)
            | Some func ->
                let newType = computeReturnTypeWithReg func typeMap typeReg currentReg
                let changed =
                    match Map.tryFind funcName currentReg with
                    | Some existing when existing = newType -> false
                    | _ -> true
                let updatedReg =
                    if changed then Map.add funcName newType currentReg else currentReg
                if not changed then
                    loop rest queued' updatedReg (stepsRemaining - 1)
                else
                    let dependents = Map.tryFind funcName callersByFunc |> Option.defaultValue []
                    let (rest', queued'') =
                        dependents
                        |> List.fold (fun (q, s) dep ->
                            if Set.contains dep s then (q, s)
                            else (dep :: q, Set.add dep s)) (rest, queued')
                    loop rest' queued'' updatedReg (stepsRemaining - 1)

    loop initialQueue initialQueued externalReturnTypes maxSteps

/// Return type for monomorphized intrinsics not tracked in the return type registry
let tryGetIntrinsicReturnType (funcName: string) : AST.Type option =
    if funcName.StartsWith("__raw_get_") then Some AST.TInt64
    elif funcName.StartsWith("__raw_set_") then Some AST.TUnit
    elif funcName.StartsWith("__empty_dict_") then Some AST.TInt64
    elif funcName.StartsWith("__dict_is_null_") then Some AST.TBool
    elif funcName.StartsWith("__dict_get_tag_") then Some AST.TInt64
    elif funcName.StartsWith("__dict_to_rawptr_") then Some AST.TInt64
    elif funcName.StartsWith("__rawptr_to_dict_") then Some AST.TInt64
    elif funcName.StartsWith("__list_is_null_") then Some AST.TBool
    elif funcName.StartsWith("__list_get_tag_") then Some AST.TInt64
    elif funcName.StartsWith("__list_to_rawptr_") then Some AST.TInt64
    elif funcName.StartsWith("__rawptr_to_list_") then Some AST.TInt64
    else None

/// CFG builder state - includes lookups to avoid mutable module-level state
/// which would cause race conditions in parallel test execution
type CFGBuilder = {
    Blocks: Map<MIR.Label, MIR.BasicBlock>
    LabelGen: MIR.LabelGen
    RegGen: MIR.RegGen
    TypeById: AST.Type option array
    ExtraTypeMap: Map<ANF.TempId, AST.Type>
    TypeReg: Map<string, (string * AST.Type) list>
    ReturnTypeReg: Map<string, AST.Type>  // Function name -> return type
    FuncName: string  // For generating unique labels per function
    ParamRegs: MIR.VReg list  // Parameter VRegs for self-recursive tail call loop optimization
    FloatRegs: Set<int>  // VReg IDs that hold float values
    ClosureFuncs: Map<ANF.TempId, string>  // Closure temp -> function name for return type lookup
    // Coverage support
    EnableCoverage: bool
    ExprIdGen: ANF.ExprIdGen
    CoverageMapping: ANF.CoverageMapping
}

/// Lookup a TempId by raw integer id, checking extra types for newly created regs
let private tryFindTypeById (builder: CFGBuilder) (id: int) : AST.Type option =
    if id >= 0 && id < builder.TypeById.Length then
        match builder.TypeById.[id] with
        | Some t -> Some t
        | None -> Map.tryFind (ANF.TempId id) builder.ExtraTypeMap
    else
        Map.tryFind (ANF.TempId id) builder.ExtraTypeMap

/// Lookup a TempId, checking extra types for newly created regs
let private tryFindType (builder: CFGBuilder) (tempId: ANF.TempId) : AST.Type option =
    let (ANF.TempId id) = tempId
    tryFindTypeById builder id

/// Convert ANF Atom to MIR Operand using lookups from builder
/// Returns Error if float/string lookup fails (internal invariant violation)
let atomToOperand (builder: CFGBuilder) (atom: ANF.Atom) : Result<MIR.Operand, string> =
    match atom with
    | ANF.UnitLiteral -> Ok (MIR.Int64Const 0L)  // Unit is represented as 0
    | ANF.IntLiteral n -> Ok (MIR.Int64Const (ANF.sizedIntToInt64 n))
    | ANF.BoolLiteral b -> Ok (MIR.BoolConst b)
    | ANF.FloatLiteral f -> Ok (MIR.FloatSymbol f)
    | ANF.StringLiteral s -> Ok (MIR.StringSymbol s)
    | ANF.Var tempId -> Ok (MIR.Register (tempToVReg tempId))
    | ANF.FuncRef funcName -> Ok (MIR.FuncAddr funcName)

let private rcKindToMIR (kind: ANF.RcKind) : MIR.RcKind =
    match kind with
    | ANF.GenericHeap -> MIR.GenericHeap
    | ANF.TaggedList -> MIR.TaggedList

/// Get the type of an ANF Atom (for generating type-specific instructions)
let atomType (builder: CFGBuilder) (atom: ANF.Atom) : AST.Type =
    match atom with
    | ANF.UnitLiteral -> AST.TUnit
    | ANF.IntLiteral n -> ANF.sizedIntToType n  // Use the actual type from SizedInt
    | ANF.BoolLiteral _ -> AST.TBool
    | ANF.StringLiteral _ -> AST.TString
    | ANF.FloatLiteral _ -> AST.TFloat64
    | ANF.Var (ANF.TempId id) ->
        // Check if this VReg is known to hold a float
        let result =
            if Set.contains id builder.FloatRegs then AST.TFloat64
            else
                match tryFindTypeById builder id with
                | Some t -> t
                | None ->
                    // TypeMap is populated by RefCountInsertion pass with fallback to TInt64.
                    // If we reach here, a pass after RefCountInsertion created a TempId without tracking.
                    Crash.crash $"atomType: unknown type for TempId {id} - TempId created after RefCountInsertion?"
        result
    | ANF.FuncRef _ -> AST.TInt64  // Function addresses are pointer-sized

/// Get the operand type for a binary operation (checks both operands)
/// If either operand is float, the operation is float
let binOpType (builder: CFGBuilder) (leftAtom: ANF.Atom) (rightAtom: ANF.Atom) : AST.Type =
    let leftType = atomType builder leftAtom
    let rightType = atomType builder rightAtom
    match leftType, rightType with
    | AST.TFloat64, _ | _, AST.TFloat64 -> AST.TFloat64
    | _ -> leftType

/// Get the type of an MIR operand (for generating type-specific instructions)
let operandType (builder: CFGBuilder) (operand: MIR.Operand) : AST.Type =
    match operand with
    | MIR.Int64Const _ -> AST.TInt64
    | MIR.BoolConst _ -> AST.TBool
    | MIR.FloatSymbol _ -> AST.TFloat64
    | MIR.StringSymbol _ -> AST.TString
    | MIR.FuncAddr _ -> AST.TInt64  // Function addresses are pointer-sized
    | MIR.Register (MIR.VReg id) ->
        // Check if this VReg is known to hold a float or has a tracked type
        if Set.contains id builder.FloatRegs then AST.TFloat64
        else
            match tryFindTypeById builder id with
            | Some t -> t
            | None -> Crash.crash $"operandType: missing type for v{id}"

/// Generate description for a CExpr (for coverage mapping)
let cexprDescription (cexpr: ANF.CExpr) : string =
    match cexpr with
    | ANF.Atom _ -> "Atom"
    | ANF.TypedAtom _ -> "TypedAtom"
    | ANF.Prim (op, _, _) -> binOpDescription op
    | ANF.UnaryPrim (op, _) -> unaryOpDescription op
    | ANF.IfValue _ -> "IfValue"
    | ANF.Call (name, _) -> System.String.Concat("Call ", name)
    | ANF.BorrowedCall (name, _) -> System.String.Concat("BorrowedCall ", name)
    | ANF.TailCall (name, _) -> System.String.Concat("TailCall ", name)
    | ANF.IndirectCall _ -> "IndirectCall"
    | ANF.IndirectTailCall _ -> "IndirectTailCall"
    | ANF.ClosureAlloc (name, _) -> System.String.Concat("ClosureAlloc ", name)
    | ANF.ClosureCall _ -> "ClosureCall"
    | ANF.ClosureTailCall _ -> "ClosureTailCall"
    | ANF.TupleAlloc _ -> "TupleAlloc"
    | ANF.TupleGet _ -> "TupleGet"
    | ANF.StringConcat _ -> "StringConcat"
    | ANF.RefCountInc _ -> "RefCountInc"
    | ANF.RefCountDec _ -> "RefCountDec"
    | ANF.Print _ -> "Print"
    | ANF.RuntimeError _ -> "RuntimeError"
    | ANF.FileReadText _ -> "FileReadText"
    | ANF.FileExists _ -> "FileExists"
    | ANF.FileWriteText _ -> "FileWriteText"
    | ANF.FileAppendText _ -> "FileAppendText"
    | ANF.FileDelete _ -> "FileDelete"
    | ANF.FileSetExecutable _ -> "FileSetExecutable"
    | ANF.FileWriteFromPtr _ -> "FileWriteFromPtr"
    | ANF.FloatSqrt _ -> "FloatSqrt"
    | ANF.FloatAbs _ -> "FloatAbs"
    | ANF.FloatNeg _ -> "FloatNeg"
    | ANF.Int64ToFloat _ -> "Int64ToFloat"
    | ANF.FloatToInt64 _ -> "FloatToInt64"
    | ANF.FloatToBits _ -> "FloatToBits"
    | ANF.RawAlloc _ -> "RawAlloc"
    | ANF.RawFree _ -> "RawFree"
    | ANF.RawGet _ -> "RawGet"
    | ANF.RawGetByte _ -> "RawGetByte"
    | ANF.RawSet _ -> "RawSet"
    | ANF.RawSetByte _ -> "RawSetByte"
    | ANF.RefCountIncString _ -> "RefCountIncString"
    | ANF.RefCountDecString _ -> "RefCountDecString"
    | ANF.RandomInt64 -> "RandomInt64"
    | ANF.DateNow -> "DateNow"
    | ANF.FloatToString _ -> "FloatToString"

/// Generate coverage instrumentation for an expression
/// Returns: (CoverageHit instruction option, updated builder with new ExprId)
let withCoverage (builder: CFGBuilder) (cexpr: ANF.CExpr) : MIR.Instr list * CFGBuilder =
    if builder.EnableCoverage then
        let (exprId, exprIdGen') = ANF.freshExprId builder.ExprIdGen
        let description = System.String.Concat(builder.FuncName, ": ", cexprDescription cexpr)
        let mapping' = ANF.addCoverageEntry exprId description builder.CoverageMapping
        let builder' = { builder with ExprIdGen = exprIdGen'; CoverageMapping = mapping' }
        ([MIR.CoverageHit exprId], builder')
    else
        ([], builder)

/// Collect cleanup operations that must run before a self-tailcall loop jump.
/// Expected shape after TailCallDetection:
///   Let(callTmp, TailCall(...), Let(_, RefCountDec..., ... Return(callTmp)))
let rec collectSelfTailCallCleanup
    (builder: CFGBuilder)
    (callTempId: ANF.TempId)
    (expr: ANF.AExpr)
    : Result<MIR.Instr list, string> =
    match expr with
    | ANF.Return (ANF.Var tid) when tid = callTempId ->
        Ok []
    | ANF.Let (_, ANF.RefCountDec (ANF.Var tid, payloadSize, kind), rest) ->
        collectSelfTailCallCleanup builder callTempId rest
        |> Result.map (fun instrs -> MIR.RefCountDec (tempToVReg tid, payloadSize, rcKindToMIR kind) :: instrs)
    | ANF.Let (_, ANF.RefCountDec (_, _, _), _) ->
        Error "Internal error: RefCountDec in self-tailcall cleanup on non-variable"
    | ANF.Let (_, ANF.RefCountDecString strAtom, rest) ->
        atomToOperand builder strAtom
        |> Result.bind (fun strOp ->
            collectSelfTailCallCleanup builder callTempId rest
            |> Result.map (fun instrs -> MIR.RefCountDecString strOp :: instrs))
    | _ ->
        Error $"Internal error: unexpected expression after self tailcall in {builder.FuncName}"

/// If a self-tailcall cleanup decrements an argument value, increment that argument first
/// so ownership is transferred to the next loop iteration before cleanup runs.
let refCountIncForOverlappingArgs
    (argOperands: MIR.Operand list)
    (cleanupInstrs: MIR.Instr list)
    (existingInstrsRev: MIR.Instr list)
    : MIR.Instr list =
    let decInfos =
        cleanupInstrs
        |> List.choose (fun instr ->
            match instr with
            | MIR.RefCountDec (vreg, payloadSize, kind) -> Some (vreg, (payloadSize, kind))
            | _ -> None)
        |> Map.ofList

    let aliasMap =
        existingInstrsRev
        |> List.fold (fun map instr ->
            match instr with
            | MIR.Mov (dest, MIR.Register src, _) ->
                Map.add dest src map
            | _ ->
                map)
            Map.empty

    let rec findCleanupTargetAlias
        (vreg: MIR.VReg)
        (visited: Set<MIR.VReg>)
        : MIR.VReg option =
        if Set.contains vreg visited then
            None
        else if Map.containsKey vreg decInfos then
            Some vreg
        else
            match Map.tryFind vreg aliasMap with
            | Some next ->
                findCleanupTargetAlias next (Set.add vreg visited)
            | None ->
                None

    let (_, incsRev) =
        argOperands
        |> List.fold (fun (seen, incsRev) argOp ->
            match argOp with
            | MIR.Register vreg ->
                match findCleanupTargetAlias vreg Set.empty with
                | Some targetVReg when not (Set.contains targetVReg seen) ->
                    let (payloadSize, kind) = Map.find targetVReg decInfos
                    (Set.add targetVReg seen, MIR.RefCountInc (targetVReg, payloadSize, kind) :: incsRev)
                | _ ->
                    (seen, incsRev)
            | _ ->
                (seen, incsRev))
            (Set.empty, [])

    List.rev incsRev

/// Convert ANF expression to CFG
/// Returns: Result of (final value operand, CFG builder with all blocks)
let rec convertExpr
    (expr: ANF.AExpr)
    (currentLabel: MIR.Label)
    (currentInstrsRev: MIR.Instr list)
    (builder: CFGBuilder)
    : Result<MIR.Operand * CFGBuilder, string> =

    match expr with
    | ANF.Return atom ->
        // Return: end current block with Ret terminator
        atomToOperand builder atom
        |> Result.bind (fun operand ->
            let block = {
                MIR.Label = currentLabel
                MIR.Instrs = List.rev currentInstrsRev
                MIR.Terminator = MIR.Ret operand
            }
            let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks }
            Ok (operand, builder'))

    // Self-recursive tail call: emit arg capture + cleanup + param update + Jump to loop header
    // This must come before the general Let case to take precedence
    // Phi nodes carry type info, so this works for both int and float parameters.
    | ANF.Let (callTempId, ANF.TailCall (funcName, args), rest) when funcName = builder.FuncName ->
        collectSelfTailCallCleanup builder callTempId rest
        |> Result.bind (fun cleanupInstrs ->
            let argTypes = args |> List.map (atomType builder)
            args
            |> List.map (atomToOperand builder)
            |> sequenceResults
            |> Result.bind (fun argOperands ->
                let loopLabel = MIR.Label $"{funcName}_body"
                // To handle register swaps correctly (e.g., swapInt(b, a, n-1)),
                // we need temps only when an argument directly references a parameter.
                //
                // Example where temps are needed: args = [b, a] for params [a, b]
                //   - Arg 0 is param b (VReg 1), needs capture before a is overwritten
                //   - Arg 1 is param a (VReg 0), needs capture before b is overwritten
                //
                // Example where temps are NOT needed: args = [n-1, acc+n] for params [n, acc]
                //   - Arg 0 is a computed temp (VReg 10xxx), not a direct param reference
                //   - Arg 1 is a computed temp (VReg 10xxx), not a direct param reference
                //
                // We only need temps if ANY argument is a direct param reference AND
                // that param will be written to by another assignment.
                let paramSet = builder.ParamRegs |> Set.ofList
                let argReferencesParam (op: MIR.Operand) =
                    match op with
                    | MIR.Register vreg -> Set.contains vreg paramSet
                    | _ -> false
                let needsTemps = argOperands |> List.exists argReferencesParam

                let (captureInstrs, assignInstrs, regGen') =
                    if needsTemps then
                        // Use temps to avoid swap issues
                        let (tempRegs, rg) =
                            argOperands
                            |> List.fold (fun (temps, rg) _ ->
                                let (temp, rg') = MIR.freshReg rg
                                (temps @ [temp], rg')
                            ) ([], builder.RegGen)
                        // First capture all arg values into temps
                        let captures =
                            List.zip3 tempRegs argOperands argTypes
                            |> List.map (fun (temp, argOp, argType) ->
                                MIR.Mov (temp, argOp, Some argType))
                        // Then assign temps to params
                        let assigns =
                            List.zip3 builder.ParamRegs tempRegs argTypes
                            |> List.map (fun (paramReg, temp, argType) ->
                                MIR.Mov (paramReg, MIR.Register temp, Some argType))
                        (captures, assigns, rg)
                    else
                        // No temps needed - just assign directly
                        let assigns =
                            List.zip3 builder.ParamRegs argOperands argTypes
                            |> List.map (fun (paramReg, argOp, argType) ->
                                MIR.Mov (paramReg, argOp, Some argType))
                        ([], assigns, builder.RegGen)

                let overlapArgIncs = refCountIncForOverlappingArgs argOperands cleanupInstrs currentInstrsRev

                // Create block with accumulated instructions + arg capture + overlap incs + cleanup + param assignments + Jump
                let instrsRev =
                    currentInstrsRev
                    |> appendInstrsRev captureInstrs
                    |> appendInstrsRev overlapArgIncs
                    |> appendInstrsRev cleanupInstrs
                    |> appendInstrsRev assignInstrs
                let block = {
                    MIR.Label = currentLabel
                    MIR.Instrs = List.rev instrsRev
                    MIR.Terminator = MIR.Jump loopLabel
                }
                let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks; RegGen = regGen' }
                // Return dummy operand since this doesn't return
                Ok (MIR.Int64Const 0L, builder')))

    | ANF.Let (tempId, cexpr, rest) ->
        // Let binding: handle based on cexpr type
        let destReg = tempToVReg tempId
        let tupleGetAliasType =
            match cexpr, rest with
            | ANF.TupleGet _, ANF.Let (_, ANF.TypedAtom (ANF.Var sourceId, aliasType), _)
                when sourceId = tempId -> Some aliasType
            | _ -> None

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow blocks
            // 1. End current block with branch on condition
            // 2. Create then-block (assigns thenAtom to destReg, jumps to join)
            // 3. Create else-block (assigns elseAtom to destReg, jumps to join)
            // 4. Create join-block (continues with rest)

            // Add coverage instrumentation for the IfValue expression
            let (coverageInstrs, builderWithCoverage) = withCoverage builder cexpr

            atomToOperand builderWithCoverage condAtom
            |> Result.bind (fun condOp ->
                atomToOperand builderWithCoverage thenAtom
                |> Result.bind (fun thenOp ->
                    atomToOperand builderWithCoverage elseAtom
                    |> Result.bind (fun elseOp ->
                        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builderWithCoverage.FuncName builderWithCoverage.LabelGen
                        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builderWithCoverage.FuncName labelGen1
                        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builderWithCoverage.FuncName labelGen2

                        // Current block ends with branch (after coverage hit)
                        let instrsRev = appendInstrsRev coverageInstrs currentInstrsRev
                        let currentBlock = {
                            MIR.Label = currentLabel
                            MIR.Instrs = List.rev instrsRev
                            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
                        }

                        // Determine the type of the if result (then/else should have same type)
                        let resultType = atomType builderWithCoverage thenAtom

                        // Then block: assign thenAtom to destReg, jump to join
                        let thenBlock = {
                            MIR.Label = thenLabel
                            MIR.Instrs = [MIR.Mov (destReg, thenOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        // Else block: assign elseAtom to destReg, jump to join
                        let elseBlock = {
                            MIR.Label = elseLabel
                            MIR.Instrs = [MIR.Mov (destReg, elseOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        let builder' = {
                            builderWithCoverage with
                                Blocks = builderWithCoverage.Blocks
                                         |> Map.add currentLabel currentBlock
                                         |> Map.add thenLabel thenBlock
                                         |> Map.add elseLabel elseBlock
                                LabelGen = labelGen3
                        }

                        // Continue with rest in join block (no instructions yet)
                        convertExpr rest joinLabel [] builder')))

        | _ ->
            // Simple CExpr: add instruction(s) to current block, continue
            // Track if dest is float type for later builder update
            let destType = ref AST.TInt64
            let instrsResult =
                match cexpr with
                | ANF.Atom atom ->
                    let aType = atomType builder atom
                    destType := aType
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.TypedAtom (atom, aType) ->
                    // Use the explicit type annotation (for pattern matching with correct types)
                    destType := aType
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    let opType = binOpType builder leftAtom rightAtom
                    // Comparison and boolean ops produce Bool, not the operand type
                    let resultType =
                        match op with
                        | ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte
                        | ANF.And | ANF.Or -> AST.TBool
                        | _ -> opType
                    destType := resultType
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.BinOp (destReg, convertBinOp op, leftOp, rightOp, opType)]))
                | ANF.UnaryPrim (op, atom) ->
                    let atomTy = atomType builder atom
                    let resultType =
                        match op with
                        | ANF.Not -> AST.TBool
                        | _ -> atomTy
                    destType := resultType
                    atomToOperand builder atom
                    |> Result.map (fun operand ->
                        match op with
                        | ANF.Not ->
                            [MIR.UnaryOp (destReg, convertUnaryOp op, operand)]
                        | ANF.Neg ->
                            // Use typed subtraction so sized integers are truncated correctly downstream.
                            [MIR.BinOp (destReg, MIR.Sub, MIR.Int64Const 0L, operand, atomTy)]
                        | ANF.BitNot ->
                            // x XOR -1 is equivalent to bitwise-not and preserves integer width via operandType.
                            [MIR.BinOp (destReg, MIR.BitXor, operand, MIR.Int64Const -1L, atomTy)])
                | ANF.Call (funcName, args)
                | ANF.BorrowedCall (funcName, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match Map.tryFind funcName builder.ReturnTypeReg with
                        | Some t -> t
                        | None ->
                            match tryGetIntrinsicReturnType funcName with
                            | Some t -> t
                            | None -> Crash.crash $"ANF_to_MIR: Return type not found for function: {funcName}"
                    destType := returnType  // Track call result type for FloatRegs update
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.Call (destReg, funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectCall (func, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match atomType builder func with
                        | AST.TFunction (_, retType) -> retType
                        | other -> Crash.crash $"IndirectCall: Expected TFunction type for func, got {other}"
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectCall (destReg, funcOp, argOperands, argTypes, returnType)]))
                | ANF.ClosureAlloc (funcName, captures) ->
                    // Allocate closure: (func_addr, cap1, cap2, ...)
                    let numSlots = 1 + List.length captures  // func_ptr + captures
                    let sizeBytes = numSlots * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store function pointer at offset 0 (always int/pointer type)
                    let storeFuncInstr = MIR.HeapStore (destReg, 0, MIR.FuncAddr funcName, None)
                    // Store captured values at offsets 8, 16, ... tracking value type for floats
                    captures
                    |> List.mapi (fun i cap -> (i, cap))
                    |> List.map (fun (i, cap) ->
                        let capType = atomType builder cap
                        let valueType = if capType = AST.TFloat64 then Some AST.TFloat64 else None
                        atomToOperand builder cap
                        |> Result.map (fun op -> MIR.HeapStore (destReg, (i + 1) * 8, op, valueType)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeFuncInstr :: storeInstrs)
                | ANF.ClosureCall (closure, args) ->
                    // Call through closure: extract func_ptr, call with (closure, args...)
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        let fallback () =
                            match tryFindType builder tempId with
                            | Some (AST.TFunction (_, retType)) -> retType
                            | Some t -> t
                            | None -> Crash.crash $"ClosureCall: Return type not found for {tempId}"
                        match closure with
                        | ANF.Var closureId ->
                            match Map.tryFind closureId builder.ClosureFuncs with
                            | Some funcName ->
                                match Map.tryFind funcName builder.ReturnTypeReg with
                                | Some t -> t
                                | None -> fallback ()
                            | None -> fallback ()
                        | _ -> fallback ()
                    destType := returnType
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureCall (destReg, closureOp, argOperands, argTypes, returnType)]))
                | ANF.TailCall (funcName, args) ->
                    // Non-self-recursive tail call (self-recursive handled specially above)
                    // Emits TailCall instruction with full epilogue + branch
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match Map.tryFind funcName builder.ReturnTypeReg with
                        | Some t -> t
                        | None ->
                            match tryGetIntrinsicReturnType funcName with
                            | Some t -> t
                            | None -> Crash.crash $"ANF_to_MIR: Return type not found for function: {funcName}"
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.TailCall (funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectTailCall (func, args) ->
                    // Indirect tail call: no destination register
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match atomType builder func with
                        | AST.TFunction (_, retType) -> retType
                        | other -> Crash.crash $"IndirectTailCall: Expected TFunction type for func, got {other}"
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectTailCall (funcOp, argOperands, argTypes, returnType)]))
                | ANF.ClosureTailCall (closure, args) ->
                    // Closure tail call: no destination register
                    let argTypes = args |> List.map (atomType builder)
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureTailCall (closureOp, argOperands, argTypes)]))
                | ANF.TupleAlloc elems ->
                    // Allocate heap space: 8 bytes per element
                    let sizeBytes = List.length elems * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store each element at its offset, tracking value type for float handling
                    elems
                    |> List.mapi (fun i elem -> (i, elem))
                    |> List.map (fun (i, elem) ->
                        let elemType = atomType builder elem
                        let valueType = if elemType = AST.TFloat64 then Some AST.TFloat64 else None
                        atomToOperand builder elem
                        |> Result.map (fun op -> MIR.HeapStore (destReg, i * 8, op, valueType)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeInstrs)
                | ANF.TupleGet (tupleAtom, index) ->
                    // Tuple should always be a variable in ANF
                    match tupleAtom with
                    | ANF.Var tid ->
                        let tupleReg = tempToVReg tid
                        match tupleGetAliasType with
                        | Some AST.TFloat64 -> destType := AST.TFloat64
                        | _ -> ()
                        match tryFindType builder tempId with
                        | Some AST.TFloat64 -> destType := AST.TFloat64
                        | _ -> ()
                        // Look up the tuple's type to determine the element type at this index
                        // This is needed for Float elements to be properly tracked in FloatRegs
                        let tupleType = tryFindType builder tid
                        let _ =
                            match tupleType with
                            | Some (AST.TTuple elemTypes) when index < List.length elemTypes ->
                                let elemType = List.item index elemTypes
                                if elemType = AST.TFloat64 then destType := AST.TFloat64
                            | Some (AST.TList elemType) ->
                                // List is a Cons cell: (tag, head, tail) - index 1 is head
                                if index = 1 && elemType = AST.TFloat64 then destType := AST.TFloat64
                            | Some (AST.TFunction (_, AST.TList elemType)) ->
                                // Function returning list - extract list element type
                                // This happens when TypeMap contains TFunction instead of just the return type
                                if index = 1 && elemType = AST.TFloat64 then destType := AST.TFloat64
                            | Some (AST.TSum (_typeName, typeArgs)) ->
                                // Sum type: [tag:8][payload:8], index 1 is payload
                                // For single type arg sums like Option<Float>, payload is that type
                                match index, typeArgs with
                                | 1, [singleType] when singleType = AST.TFloat64 -> destType := AST.TFloat64
                                | _ -> ()
                            | _ -> ()
                        let loadType = if !destType = AST.TFloat64 then Some AST.TFloat64 else None
                        Ok [MIR.HeapLoad (destReg, tupleReg, index * 8, loadType)]
                    | _ ->
                        Error "Internal error: Tuple access on non-variable (ANF invariant violated)"
                | ANF.IfValue _ ->
                    // This case is handled above; reaching here indicates a bug
                    Error "Internal error: IfValue should have been handled in outer match"
                | ANF.RefCountInc (atom, payloadSize, kind) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountInc (tempToVReg tid, payloadSize, rcKindToMIR kind)]
                    | _ -> Error "Internal error: RefCountInc on non-variable"
                | ANF.RefCountDec (atom, payloadSize, kind) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountDec (tempToVReg tid, payloadSize, rcKindToMIR kind)]
                    | _ -> Error "Internal error: RefCountDec on non-variable"
                | ANF.Print (atom, valueType) ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Print (op, valueType)])
                | ANF.RuntimeError message ->
                    Ok [MIR.RuntimeError message]
                | ANF.StringConcat (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringConcat (destReg, leftOp, rightOp)]))
                | ANF.FileReadText pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileReadText (destReg, pathOp)])
                | ANF.FileExists pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileExists (destReg, pathOp)])
                | ANF.FileWriteText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileWriteText (destReg, pathOp, contentOp)]))
                | ANF.FileAppendText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileAppendText (destReg, pathOp, contentOp)]))
                | ANF.FileDelete pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileDelete (destReg, pathOp)])
                | ANF.FileSetExecutable pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileSetExecutable (destReg, pathOp)])
                | ANF.FileWriteFromPtr (pathAtom, ptrAtom, lengthAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder ptrAtom
                        |> Result.bind (fun ptrOp ->
                            atomToOperand builder lengthAtom
                            |> Result.map (fun lengthOp ->
                                [MIR.FileWriteFromPtr (destReg, pathOp, ptrOp, lengthOp)])))
                | ANF.RawAlloc numBytesAtom ->
                    atomToOperand builder numBytesAtom
                    |> Result.map (fun numBytesOp -> [MIR.RawAlloc (destReg, numBytesOp)])
                | ANF.RawFree ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.RawFree ptrOp])
                | ANF.RawGet (ptrAtom, offsetAtom, valueType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGet (destReg, ptrOp, offsetOp, valueType)]))
                | ANF.RawGetByte (ptrAtom, offsetAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGetByte (destReg, ptrOp, offsetOp)]))
                | ANF.RawSet (ptrAtom, offsetAtom, valueAtom, valueType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawSet (ptrOp, offsetOp, valueOp, valueType)])))
                | ANF.RawSetByte (ptrAtom, offsetAtom, valueAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawSetByte (ptrOp, offsetOp, valueOp)])))
                | ANF.FloatSqrt atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatSqrt (destReg, op)])
                | ANF.FloatAbs atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatAbs (destReg, op)])
                | ANF.FloatNeg atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatNeg (destReg, op)])
                | ANF.Int64ToFloat atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Int64ToFloat (destReg, op)])
                | ANF.FloatToInt64 atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToInt64 (destReg, op)])
                | ANF.FloatToBits atom ->
                    // FloatToBits copies float bits to UInt64 (produces integer, not float)
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToBits (destReg, op)])
                | ANF.RefCountIncString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountIncString strOp])
                | ANF.RefCountDecString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountDecString strOp])
                | ANF.RandomInt64 ->
                    Ok [MIR.RandomInt64 destReg]
                | ANF.DateNow ->
                    Ok [MIR.DateNow destReg]
                | ANF.FloatToString valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.FloatToString (destReg, valueOp)])

            match instrsResult with
            | Error err -> Error err
            | Ok instrs ->
                // Add coverage instrumentation if enabled
                let (coverageInstrs, builderWithCoverage) = withCoverage builder cexpr
                let builderWithClosure =
                    match cexpr with
                    | ANF.ClosureAlloc (funcName, _) ->
                        { builderWithCoverage with ClosureFuncs = Map.add tempId funcName builderWithCoverage.ClosureFuncs }
                    | _ -> builderWithCoverage
                let newInstrsRev = appendInstrsRev (coverageInstrs @ instrs) currentInstrsRev
                // Update FloatRegs if this dest is a float
                let (MIR.VReg destId) = destReg
                let builder' =
                    if !destType = AST.TFloat64 then
                        { builderWithClosure with FloatRegs = Set.add destId builderWithClosure.FloatRegs }
                    else
                        builderWithClosure
                convertExpr rest currentLabel newInstrsRev builder'

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression:
        // 1. End current block with Branch terminator
        // 2. Create then-block and else-block
        // 3. Create join-block where both branches meet
        // 4. Both branches put result in same register and jump to join

        atomToOperand builder condAtom
        |> Result.bind (fun condOp ->

        // Generate labels for then, else, and join blocks
        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

        // Create a register to hold the result from both branches
        let (resultReg, regGen1) = MIR.freshReg builder.RegGen

        // End current block with conditional branch
        let currentBlock = {
            MIR.Label = currentLabel
            MIR.Instrs = List.rev currentInstrsRev
            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
        }

        let builder1 = {
            builder with
                Blocks = Map.add currentLabel currentBlock builder.Blocks
                LabelGen = labelGen3
                RegGen = regGen1
        }

        // Convert then-branch: result goes into resultReg, then jump to join
        match convertExprToOperand thenBranch thenLabel [] builder1 with
        | Error err -> Error err
        | Ok (thenResult, thenJoinOpt, builder2) ->

        // Helper: check if a block is a self-recursive loop-back (jumps to _body label)
        // Such blocks should NOT be patched as they are terminal control flow
        let isLoopBackBlock (block: MIR.BasicBlock) =
            match block.Terminator with
            | MIR.Jump (MIR.Label label) when label.EndsWith("_body") -> true
            | _ -> false

        // If then-branch created blocks (nested if), patch its join block
        // Otherwise, create a simple block that moves result and jumps
        // EXCEPTION: If the block is a self-recursive loop-back, don't patch it
        let thenResultType = operandType builder2 thenResult
        let builder3 =
            match thenJoinOpt with
            | Some nestedJoinLabel ->
                // Patch the nested join block to jump to our join instead of returning
                match Map.tryFind nestedJoinLabel builder2.Blocks with
                | Some nestedJoinBlock when isLoopBackBlock nestedJoinBlock ->
                    // Self-recursive loop-back: don't patch, it's already terminal
                    builder2
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder2 with Blocks = Map.add nestedJoinLabel patchedBlock builder2.Blocks }
                | None -> builder2  // Should not happen
            | None ->
                // Simple expression - create block that moves result and jumps
                let thenBlock = {
                    MIR.Label = thenLabel
                    MIR.Instrs = [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

        // Convert else-branch: result goes into resultReg, then jump to join
        match convertExprToOperand elseBranch elseLabel [] builder3 with
        | Error err -> Error err
        | Ok (elseResult, elseJoinOpt, builder4) ->

        // Same logic for else-branch
        let elseResultType = operandType builder4 elseResult
        let builder5 =
            match elseJoinOpt with
            | Some nestedJoinLabel ->
                match Map.tryFind nestedJoinLabel builder4.Blocks with
                | Some nestedJoinBlock when isLoopBackBlock nestedJoinBlock ->
                    // Self-recursive loop-back: don't patch, it's already terminal
                    builder4
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder4 with Blocks = Map.add nestedJoinLabel patchedBlock builder4.Blocks }
                | None -> builder4  // Should not happen
            | None ->
                let elseBlock = {
                    MIR.Label = elseLabel
                    MIR.Instrs = [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

        // Create join block that returns the result
        let joinBlock = {
            MIR.Label = joinLabel
            MIR.Instrs = []
            MIR.Terminator = MIR.Ret (MIR.Register resultReg)
        }
        let builder6 = { builder5 with Blocks = Map.add joinLabel joinBlock builder5.Blocks }

        // Track the result type for nested ifs that return through this register
        let (MIR.VReg resultId) = resultReg
        let builder6WithType =
            { builder6 with ExtraTypeMap = Map.add (ANF.TempId resultId) thenResultType builder6.ExtraTypeMap }

        // Update FloatRegs if the result is a float
        let builder7 =
            if thenResultType = AST.TFloat64 || elseResultType = AST.TFloat64 then
                { builder6WithType with FloatRegs = Set.add resultId builder6WithType.FloatRegs }
            else
                builder6WithType

        // Return the result operand
        let resultOp = MIR.Register resultReg
        Ok (resultOp, builder7))

/// Helper: convert expression and extract final operand
/// Returns: Result of (operand, optional join label if blocks were created, builder)
/// - If join label is Some(label), the expression created blocks ending at that join block
/// - If join label is None, no blocks were created (simple expression)
and convertExprToOperand
    (expr: ANF.AExpr)
    (startLabel: MIR.Label)
    (startInstrsRev: MIR.Instr list)
    (builder: CFGBuilder)
    : Result<MIR.Operand * MIR.Label option * CFGBuilder, string> =

    match expr with
    | ANF.Return atom ->
        // If we have accumulated instructions from Let bindings, create a block
        // Otherwise just return the operand
        atomToOperand builder atom
        |> Result.bind (fun operand ->
            if List.isEmpty startInstrsRev then
                Ok (operand, None, builder)
            else
                // Create a block with accumulated instructions
                // Use temporary Ret terminator - caller will patch if needed
                let block = {
                    MIR.Label = startLabel
                    MIR.Instrs = List.rev startInstrsRev
                    MIR.Terminator = MIR.Ret operand
                }
                let builder' = { builder with Blocks = Map.add startLabel block builder.Blocks }
                Ok (operand, Some startLabel, builder'))

    // Self-recursive tail call: emit arg capture + cleanup + param update + Jump to loop header
    // This must come before the general Let case to take precedence
    // Phi nodes carry type info, so this works for both int and float parameters.
    | ANF.Let (callTempId, ANF.TailCall (funcName, args), rest) when funcName = builder.FuncName ->
        collectSelfTailCallCleanup builder callTempId rest
        |> Result.bind (fun cleanupInstrs ->
            let argTypes = args |> List.map (atomType builder)
            args
            |> List.map (atomToOperand builder)
            |> sequenceResults
            |> Result.bind (fun argOperands ->
                let loopLabel = MIR.Label $"{funcName}_body"
                // To handle register swaps correctly (e.g., swapInt(b, a, n-1)),
                // we need temps only when an argument directly references a parameter.
                // (See convertExpr for detailed explanation)
                let paramSet = builder.ParamRegs |> Set.ofList
                let argReferencesParam (op: MIR.Operand) =
                    match op with
                    | MIR.Register vreg -> Set.contains vreg paramSet
                    | _ -> false
                let needsTemps = argOperands |> List.exists argReferencesParam

                let (captureInstrs, assignInstrs, regGen') =
                    if needsTemps then
                        // Use temps to avoid swap issues
                        let (tempRegs, rg) =
                            argOperands
                            |> List.fold (fun (temps, rg) _ ->
                                let (temp, rg') = MIR.freshReg rg
                                (temps @ [temp], rg')
                            ) ([], builder.RegGen)
                        let captures =
                            List.zip3 tempRegs argOperands argTypes
                            |> List.map (fun (temp, argOp, argType) ->
                                MIR.Mov (temp, argOp, Some argType))
                        let assigns =
                            List.zip3 builder.ParamRegs tempRegs argTypes
                            |> List.map (fun (paramReg, temp, argType) ->
                                MIR.Mov (paramReg, MIR.Register temp, Some argType))
                        (captures, assigns, rg)
                    else
                        // No temps needed - just assign directly
                        let assigns =
                            List.zip3 builder.ParamRegs argOperands argTypes
                            |> List.map (fun (paramReg, argOp, argType) ->
                                MIR.Mov (paramReg, argOp, Some argType))
                        ([], assigns, builder.RegGen)

                let overlapArgIncs = refCountIncForOverlappingArgs argOperands cleanupInstrs startInstrsRev

                // Create block with accumulated instructions + arg capture + overlap incs + cleanup + param assignments + Jump
                let instrsRev =
                    startInstrsRev
                    |> appendInstrsRev captureInstrs
                    |> appendInstrsRev overlapArgIncs
                    |> appendInstrsRev cleanupInstrs
                    |> appendInstrsRev assignInstrs
                let block = {
                    MIR.Label = startLabel
                    MIR.Instrs = List.rev instrsRev
                    MIR.Terminator = MIR.Jump loopLabel
                }
                let builder' = { builder with Blocks = Map.add startLabel block builder.Blocks; RegGen = regGen' }
                // Return Some startLabel to tell the caller we created a block that's terminal
                // (jumps back to loop header). The caller should not try to patch this block.
                Ok (MIR.Int64Const 0L, Some startLabel, builder')))

    | ANF.Let (tempId, cexpr, rest) ->
        let destReg = tempToVReg tempId
        let tupleGetAliasType =
            match cexpr, rest with
            | ANF.TupleGet _, ANF.Let (_, ANF.TypedAtom (ANF.Var sourceId, aliasType), _)
                when sourceId = tempId -> Some aliasType
            | _ -> None

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow - similar to convertExpr version
            atomToOperand builder condAtom
            |> Result.bind (fun condOp ->
                atomToOperand builder thenAtom
                |> Result.bind (fun thenOp ->
                    atomToOperand builder elseAtom
                    |> Result.bind (fun elseOp ->
                        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
                        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
                        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

                        // Current block ends with branch
                        let startBlock = {
                            MIR.Label = startLabel
                            MIR.Instrs = List.rev startInstrsRev
                            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
                        }

                        // Determine the type of the if result (then/else should have same type)
                        let resultType = atomType builder thenAtom

                        // Then block: assign thenAtom to destReg, jump to join
                        let thenBlock = {
                            MIR.Label = thenLabel
                            MIR.Instrs = [MIR.Mov (destReg, thenOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        // Else block: assign elseAtom to destReg, jump to join
                        let elseBlock = {
                            MIR.Label = elseLabel
                            MIR.Instrs = [MIR.Mov (destReg, elseOp, Some resultType)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        let builder' = {
                            builder with
                                Blocks = builder.Blocks
                                         |> Map.add startLabel startBlock
                                         |> Map.add thenLabel thenBlock
                                         |> Map.add elseLabel elseBlock
                                LabelGen = labelGen3
                        }

                        // Continue with rest in join block (no instructions yet)
                        convertExprToOperand rest joinLabel [] builder')))

        | _ ->
            // Simple CExpr: create instruction(s) and accumulate
            // Track if dest is float type for later builder update
            let destType = ref AST.TInt64
            let instrsResult =
                match cexpr with
                | ANF.Atom atom ->
                    let aType = atomType builder atom
                    destType := aType
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.TypedAtom (atom, aType) ->
                    // Use the explicit type annotation (for pattern matching with correct types)
                    destType := aType
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op, Some aType)])
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    let opType = binOpType builder leftAtom rightAtom
                    // Comparison and boolean ops produce Bool, not the operand type
                    let resultType =
                        match op with
                        | ANF.Eq | ANF.Neq | ANF.Lt | ANF.Gt | ANF.Lte | ANF.Gte
                        | ANF.And | ANF.Or -> AST.TBool
                        | _ -> opType
                    destType := resultType
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.BinOp (destReg, convertBinOp op, leftOp, rightOp, opType)]))
                | ANF.UnaryPrim (op, atom) ->
                    let atomTy = atomType builder atom
                    let resultType =
                        match op with
                        | ANF.Not -> AST.TBool
                        | _ -> atomTy
                    destType := resultType
                    atomToOperand builder atom
                    |> Result.map (fun operand ->
                        match op with
                        | ANF.Not ->
                            [MIR.UnaryOp (destReg, convertUnaryOp op, operand)]
                        | ANF.Neg ->
                            // Use typed subtraction so sized integers are truncated correctly downstream.
                            [MIR.BinOp (destReg, MIR.Sub, MIR.Int64Const 0L, operand, atomTy)]
                        | ANF.BitNot ->
                            // x XOR -1 is equivalent to bitwise-not and preserves integer width via operandType.
                            [MIR.BinOp (destReg, MIR.BitXor, operand, MIR.Int64Const -1L, atomTy)])
                | ANF.Call (funcName, args)
                | ANF.BorrowedCall (funcName, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match Map.tryFind funcName builder.ReturnTypeReg with
                        | Some t -> t
                        | None ->
                            match tryGetIntrinsicReturnType funcName with
                            | Some t -> t
                            | None -> Crash.crash $"ANF_to_MIR: Return type not found for function: {funcName}"
                    destType := returnType  // Track call result type for FloatRegs update
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.Call (destReg, funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectCall (func, args) ->
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match atomType builder func with
                        | AST.TFunction (_, retType) -> retType
                        | other -> Crash.crash $"IndirectCall: Expected TFunction type for func, got {other}"
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectCall (destReg, funcOp, argOperands, argTypes, returnType)]))
                | ANF.ClosureAlloc (funcName, captures) ->
                    // Allocate closure: (func_addr, cap1, cap2, ...)
                    let numSlots = 1 + List.length captures  // func_ptr + captures
                    let sizeBytes = numSlots * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store function pointer at offset 0 (always int/pointer type)
                    let storeFuncInstr = MIR.HeapStore (destReg, 0, MIR.FuncAddr funcName, None)
                    // Store captured values at offsets 8, 16, ... tracking value type for floats
                    captures
                    |> List.mapi (fun i cap -> (i, cap))
                    |> List.map (fun (i, cap) ->
                        let capType = atomType builder cap
                        let valueType = if capType = AST.TFloat64 then Some AST.TFloat64 else None
                        atomToOperand builder cap
                        |> Result.map (fun op -> MIR.HeapStore (destReg, (i + 1) * 8, op, valueType)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeFuncInstr :: storeInstrs)
                | ANF.ClosureCall (closure, args) ->
                    // Call through closure: extract func_ptr, call with (closure, args...)
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        let fallback () =
                            match tryFindType builder tempId with
                            | Some (AST.TFunction (_, retType)) -> retType
                            | Some t -> t
                            | None -> Crash.crash $"ClosureCall: Return type not found for {tempId}"
                        match closure with
                        | ANF.Var closureId ->
                            match Map.tryFind closureId builder.ClosureFuncs with
                            | Some funcName ->
                                match Map.tryFind funcName builder.ReturnTypeReg with
                                | Some t -> t
                                | None -> fallback ()
                            | None -> fallback ()
                        | _ -> fallback ()
                    destType := returnType
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureCall (destReg, closureOp, argOperands, argTypes, returnType)]))
                | ANF.TailCall (funcName, args) ->
                    // Non-self-recursive tail call (self-recursive handled specially above)
                    // Emits TailCall instruction with full epilogue + branch
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match Map.tryFind funcName builder.ReturnTypeReg with
                        | Some t -> t
                        | None ->
                            match tryGetIntrinsicReturnType funcName with
                            | Some t -> t
                            | None -> Crash.crash $"ANF_to_MIR: Return type not found for function: {funcName}"
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.TailCall (funcName, argOperands, argTypes, returnType)])
                | ANF.IndirectTailCall (func, args) ->
                    // Indirect tail call: no destination register
                    let argTypes = args |> List.map (atomType builder)
                    let returnType =
                        match atomType builder func with
                        | AST.TFunction (_, retType) -> retType
                        | other -> Crash.crash $"IndirectTailCall: Expected TFunction type for func, got {other}"
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectTailCall (funcOp, argOperands, argTypes, returnType)]))
                | ANF.ClosureTailCall (closure, args) ->
                    // Closure tail call: no destination register
                    let argTypes = args |> List.map (atomType builder)
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureTailCall (closureOp, argOperands, argTypes)]))
                | ANF.TupleAlloc elems ->
                    // Allocate heap space: 8 bytes per element
                    let sizeBytes = List.length elems * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store each element at its offset, tracking value type for float handling
                    elems
                    |> List.mapi (fun i elem -> (i, elem))
                    |> List.map (fun (i, elem) ->
                        let elemType = atomType builder elem
                        let valueType = if elemType = AST.TFloat64 then Some AST.TFloat64 else None
                        atomToOperand builder elem
                        |> Result.map (fun op -> MIR.HeapStore (destReg, i * 8, op, valueType)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeInstrs)
                | ANF.TupleGet (tupleAtom, index) ->
                    // Tuple should always be a variable in ANF
                    match tupleAtom with
                    | ANF.Var tid ->
                        let tupleReg = tempToVReg tid
                        match tupleGetAliasType with
                        | Some AST.TFloat64 -> destType := AST.TFloat64
                        | _ -> ()
                        match tryFindType builder tempId with
                        | Some AST.TFloat64 -> destType := AST.TFloat64
                        | _ -> ()
                        // Look up the tuple's type to determine the element type at this index
                        // This is needed for Float elements to be properly tracked in FloatRegs
                        let tupleType = tryFindType builder tid
                        let _ =
                            match tupleType with
                            | Some (AST.TTuple elemTypes) when index < List.length elemTypes ->
                                let elemType = List.item index elemTypes
                                if elemType = AST.TFloat64 then destType := AST.TFloat64
                            | Some (AST.TList elemType) ->
                                // List is a Cons cell: (tag, head, tail) - index 1 is head
                                if index = 1 && elemType = AST.TFloat64 then destType := AST.TFloat64
                            | Some (AST.TFunction (_, AST.TList elemType)) ->
                                // Function returning list - extract list element type
                                // This happens when TypeMap contains TFunction instead of just the return type
                                if index = 1 && elemType = AST.TFloat64 then destType := AST.TFloat64
                            | Some (AST.TSum (_typeName, typeArgs)) ->
                                // Sum type: [tag:8][payload:8], index 1 is payload
                                // For single type arg sums like Option<Float>, payload is that type
                                match index, typeArgs with
                                | 1, [singleType] when singleType = AST.TFloat64 -> destType := AST.TFloat64
                                | _ -> ()
                            | _ -> ()
                        let loadType = if !destType = AST.TFloat64 then Some AST.TFloat64 else None
                        Ok [MIR.HeapLoad (destReg, tupleReg, index * 8, loadType)]
                    | _ ->
                        Error "Internal error: Tuple access on non-variable (ANF invariant violated)"
                | ANF.IfValue _ ->
                    // This case is handled above; reaching here indicates a bug
                    Error "Internal error: IfValue should have been handled in outer match"
                | ANF.RefCountInc (atom, payloadSize, kind) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountInc (tempToVReg tid, payloadSize, rcKindToMIR kind)]
                    | _ -> Error "Internal error: RefCountInc on non-variable"
                | ANF.RefCountDec (atom, payloadSize, kind) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountDec (tempToVReg tid, payloadSize, rcKindToMIR kind)]
                    | _ -> Error "Internal error: RefCountDec on non-variable"
                | ANF.Print (atom, valueType) ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Print (op, valueType)])
                | ANF.RuntimeError message ->
                    Ok [MIR.RuntimeError message]
                | ANF.StringConcat (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringConcat (destReg, leftOp, rightOp)]))
                | ANF.FileReadText pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileReadText (destReg, pathOp)])
                | ANF.FileExists pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileExists (destReg, pathOp)])
                | ANF.FileWriteText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileWriteText (destReg, pathOp, contentOp)]))
                | ANF.FileAppendText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileAppendText (destReg, pathOp, contentOp)]))
                | ANF.FileDelete pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileDelete (destReg, pathOp)])
                | ANF.FileSetExecutable pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileSetExecutable (destReg, pathOp)])
                | ANF.FileWriteFromPtr (pathAtom, ptrAtom, lengthAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder ptrAtom
                        |> Result.bind (fun ptrOp ->
                            atomToOperand builder lengthAtom
                            |> Result.map (fun lengthOp ->
                                [MIR.FileWriteFromPtr (destReg, pathOp, ptrOp, lengthOp)])))
                | ANF.RawAlloc numBytesAtom ->
                    atomToOperand builder numBytesAtom
                    |> Result.map (fun numBytesOp -> [MIR.RawAlloc (destReg, numBytesOp)])
                | ANF.RawFree ptrAtom ->
                    atomToOperand builder ptrAtom
                    |> Result.map (fun ptrOp -> [MIR.RawFree ptrOp])
                | ANF.RawGet (ptrAtom, offsetAtom, valueType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGet (destReg, ptrOp, offsetOp, valueType)]))
                | ANF.RawGetByte (ptrAtom, offsetAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.map (fun offsetOp ->
                            [MIR.RawGetByte (destReg, ptrOp, offsetOp)]))
                | ANF.RawSet (ptrAtom, offsetAtom, valueAtom, valueType) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawSet (ptrOp, offsetOp, valueOp, valueType)])))
                | ANF.RawSetByte (ptrAtom, offsetAtom, valueAtom) ->
                    atomToOperand builder ptrAtom
                    |> Result.bind (fun ptrOp ->
                        atomToOperand builder offsetAtom
                        |> Result.bind (fun offsetOp ->
                            atomToOperand builder valueAtom
                            |> Result.map (fun valueOp ->
                                [MIR.RawSetByte (ptrOp, offsetOp, valueOp)])))
                | ANF.FloatSqrt atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatSqrt (destReg, op)])
                | ANF.FloatAbs atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatAbs (destReg, op)])
                | ANF.FloatNeg atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatNeg (destReg, op)])
                | ANF.Int64ToFloat atom ->
                    destType := AST.TFloat64
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Int64ToFloat (destReg, op)])
                | ANF.FloatToInt64 atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToInt64 (destReg, op)])
                | ANF.FloatToBits atom ->
                    // FloatToBits copies float bits to UInt64 (produces integer, not float)
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.FloatToBits (destReg, op)])
                | ANF.RefCountIncString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountIncString strOp])
                | ANF.RefCountDecString strAtom ->
                    atomToOperand builder strAtom
                    |> Result.map (fun strOp -> [MIR.RefCountDecString strOp])
                | ANF.RandomInt64 ->
                    Ok [MIR.RandomInt64 destReg]
                | ANF.DateNow ->
                    Ok [MIR.DateNow destReg]
                | ANF.FloatToString valueAtom ->
                    atomToOperand builder valueAtom
                    |> Result.map (fun valueOp -> [MIR.FloatToString (destReg, valueOp)])

            // Let bindings accumulate instructions, pass through join label
            match instrsResult with
            | Error err -> Error err
            | Ok instrs ->
                let builderWithClosure =
                    match cexpr with
                    | ANF.ClosureAlloc (funcName, _) ->
                        { builder with ClosureFuncs = Map.add tempId funcName builder.ClosureFuncs }
                    | _ -> builder
                // Update FloatRegs if this dest is a float
                let (MIR.VReg destId) = destReg
                let builder' =
                    if !destType = AST.TFloat64 then
                        { builderWithClosure with FloatRegs = Set.add destId builderWithClosure.FloatRegs }
                    else
                        builderWithClosure
                let newInstrsRev = appendInstrsRev instrs startInstrsRev
                convertExprToOperand rest startLabel newInstrsRev builder'

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression: creates blocks with branch/jump/join structure
        atomToOperand builder condAtom
        |> Result.bind (fun condOp ->
            let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
            let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
            let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2
            let (resultReg, regGen1) = MIR.freshReg builder.RegGen

            let startBlock = {
                MIR.Label = startLabel
                MIR.Instrs = List.rev startInstrsRev
                MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
            }

            let builder1 = {
                builder with
                    Blocks = Map.add startLabel startBlock builder.Blocks
                    LabelGen = labelGen3
                    RegGen = regGen1
            }

            // Helper: check if a block is a self-recursive loop-back (jumps to _body label)
            // Such blocks should NOT be patched as they are terminal control flow
            let isLoopBackBlock (block: MIR.BasicBlock) =
                match block.Terminator with
                | MIR.Jump (MIR.Label label) when label.EndsWith("_body") -> true
                | _ -> false

            // Convert then-branch
            match convertExprToOperand thenBranch thenLabel [] builder1 with
            | Error err -> Error err
            | Ok (thenResult, thenJoinOpt, builder2) ->

            // If then-branch created blocks (nested if), patch its join block
            // Otherwise, create a simple block that moves result and jumps
            // EXCEPTION: If the block is a self-recursive loop-back, don't patch it
            let thenResultType = operandType builder2 thenResult
            let builder3 =
                match thenJoinOpt with
                | Some nestedJoinLabel ->
                    // Patch the nested join block to jump to our join instead of returning
                    match Map.tryFind nestedJoinLabel builder2.Blocks with
                    | Some nestedJoinBlock when isLoopBackBlock nestedJoinBlock ->
                        // Self-recursive loop-back: don't patch, it's already terminal
                        builder2
                    | Some nestedJoinBlock ->
                        let patchedBlock = {
                            nestedJoinBlock with
                                Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                                Terminator = MIR.Jump joinLabel
                        }
                        { builder2 with Blocks = Map.add nestedJoinLabel patchedBlock builder2.Blocks }
                    | None -> builder2  // Should not happen
                | None ->
                    // Simple expression - create block that moves result and jumps
                    let thenBlock = {
                        MIR.Label = thenLabel
                        MIR.Instrs = [MIR.Mov (resultReg, thenResult, Some thenResultType)]
                        MIR.Terminator = MIR.Jump joinLabel
                    }
                    { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

            // Convert else-branch
            match convertExprToOperand elseBranch elseLabel [] builder3 with
            | Error err -> Error err
            | Ok (elseResult, elseJoinOpt, builder4) ->

            // Same logic for else-branch
            let elseResultType = operandType builder4 elseResult
            let builder5 =
                match elseJoinOpt with
                | Some nestedJoinLabel ->
                    match Map.tryFind nestedJoinLabel builder4.Blocks with
                    | Some nestedJoinBlock when isLoopBackBlock nestedJoinBlock ->
                        // Self-recursive loop-back: don't patch, it's already terminal
                        builder4
                    | Some nestedJoinBlock ->
                        let patchedBlock = {
                            nestedJoinBlock with
                                Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                                Terminator = MIR.Jump joinLabel
                        }
                        { builder4 with Blocks = Map.add nestedJoinLabel patchedBlock builder4.Blocks }
                    | None -> builder4  // Should not happen
                | None ->
                    let elseBlock = {
                        MIR.Label = elseLabel
                        MIR.Instrs = [MIR.Mov (resultReg, elseResult, Some elseResultType)]
                        MIR.Terminator = MIR.Jump joinLabel
                    }
                    { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

            // Create join block
            let joinBlock = {
                MIR.Label = joinLabel
                MIR.Instrs = []
                MIR.Terminator = MIR.Ret (MIR.Register resultReg)
            }
            let builder6 = { builder5 with Blocks = Map.add joinLabel joinBlock builder5.Blocks }

            // Track the result type for nested ifs that return through this register
            let (MIR.VReg resultId) = resultReg
            let builder6WithType =
                { builder6 with ExtraTypeMap = Map.add (ANF.TempId resultId) thenResultType builder6.ExtraTypeMap }

            // Update FloatRegs if the result is a float
            let builder7 =
                if thenResultType = AST.TFloat64 || elseResultType = AST.TFloat64 then
                    { builder6WithType with FloatRegs = Set.add resultId builder6WithType.FloatRegs }
                else
                    builder6WithType

            // Return result register and our join label for potential patching by caller
            Ok (MIR.Register resultReg, Some joinLabel, builder7))

/// Convert an ANF function to a MIR function
/// Each function gets its own RegGen starting from (maxTempId + 1) for deterministic VReg assignment.
/// This ensures the same function always produces identical MIR regardless of compilation context.
let convertANFFunction
    (anfFunc: ANF.Function)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (returnTypeReg: Map<string, AST.Type>)
    (enableCoverage: bool)
    : Result<MIR.Function, string> =
    let convertCore () : Result<MIR.Function, string> =
        // Calculate RegGen for THIS function only
        // freshReg must generate VRegs that don't conflict with TempId-derived VRegs.
        // tempToVReg (TempId n) → VReg n, so freshReg must start past the max TempId used.
        let paramMax =
            anfFunc.TypedParams
            |> List.map (fun tp -> let (ANF.TempId id) = tp.Id in id)
            |> List.fold max -1

        // Initialize FloatRegs with float parameter IDs (types are now bundled in TypedParams)
        let floatParamIds =
            anfFunc.TypedParams
            |> List.filter (fun tp -> tp.Type = AST.TFloat64)
            |> List.map (fun tp -> let (ANF.TempId id) = tp.Id in id)
            |> Set.ofList

        // Convert ANF parameter TempIds to MIR VRegs
        // Must use tempToVReg to preserve the TempId values, not fresh VRegs,
        // because the body uses Var (TempId n) which converts to VReg n
        let paramVRegs = anfFunc.TypedParams |> List.map (fun tp -> tempToVReg tp.Id)

        // Get parameter types from TypedParams (types are now bundled)
        let paramTypes = anfFunc.TypedParams |> List.map (fun tp -> tp.Type)

        let (bodyMaxId, returnType) =
            getReturnTypeAndMaxTempId floatParamIds typeMap returnTypeReg anfFunc.Body
        let maxId = max paramMax bodyMaxId
        let regGen = MIR.RegGen (maxId + 1)
        let typeById = buildTypeById maxId typeMap

        // Create initial builder
        let initialBuilder = {
            RegGen = regGen
            LabelGen = MIR.initialLabelGen
            Blocks = Map.empty
            TypeById = typeById
            ExtraTypeMap = Map.empty
            TypeReg = typeReg
            ReturnTypeReg = returnTypeReg
            FuncName = anfFunc.Name
            ParamRegs = paramVRegs  // For self-recursive tail call loop optimization
            FloatRegs = floatParamIds
            ClosureFuncs = Map.empty
            EnableCoverage = enableCoverage
            ExprIdGen = ANF.initialExprIdGen
            CoverageMapping = ANF.emptyCoverageMapping
        }

        // Create entry label for CFG (internal to function body)
        let entryLabel = MIR.Label $"{anfFunc.Name}_body"

        // For self-recursive functions, we need a separate entry block that jumps to the body.
        // This allows the body to be a proper loop header with two predecessors:
        // 1. The entry block (first call with initial param values)
        // 2. The recursive block (back-edge with updated param values)
        // This structure enables SSA to insert phi nodes at the loop header.
        let trueEntryLabel = MIR.Label $"{anfFunc.Name}_entry"
        let entryBlock = {
            MIR.Label = trueEntryLabel
            MIR.Instrs = []  // Params are implicitly defined here by calling convention
            MIR.Terminator = MIR.Jump entryLabel
        }

        // Convert function body to CFG
        match convertExpr anfFunc.Body entryLabel [] initialBuilder with
        | Error err -> Error err
        | Ok (_, finalBuilder) ->

        // Add the entry block to the CFG
        let allBlocks = Map.add trueEntryLabel entryBlock finalBuilder.Blocks

        let cfg = {
            MIR.Entry = trueEntryLabel
            MIR.Blocks = allBlocks
        }

        // Create TypedMIRParams by zipping VRegs with types
        let typedMIRParams : MIR.TypedMIRParam list =
            List.zip paramVRegs paramTypes
            |> List.map (fun (reg, typ) -> { Reg = reg; Type = typ })

        let mirFunc = {
            MIR.Name = anfFunc.Name
            MIR.TypedParams = typedMIRParams
            MIR.ReturnType = returnType
            MIR.CFG = cfg
            MIR.FloatRegs = finalBuilder.FloatRegs
        }

        Ok mirFunc

    convertCore ()

/// Convert ANF program to MIR program
/// mainExprType: the type of the main expression (used for _start's return type)
/// variantLookup: mapping from variant names to type info (for enum printing)
/// typeReg: mapping from record type names to field info (for record printing, converted to RecordRegistry)
/// externalReturnTypes: return types for functions not in the program (e.g., specialized functions compiled elsewhere)
/// Each function gets its own RegGen for deterministic VReg assignment.
let toMIR
    (program: ANF.Program)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (mainExprType: AST.Type)
    (variantLookup: AST_to_ANF.VariantLookup)
    (typeRegForRecords: Map<string, (string * AST.Type) list>)
    (enableCoverage: bool)
    (externalReturnTypes: Map<string, AST.Type>)
    : Result<MIR.Program, string> =
    let (ANF.Program (functions, mainExpr)) = program

    // Build return type registry for all functions (needed for caller to know return type)
    let returnTypeReg = buildReturnTypeReg functions typeMap typeReg externalReturnTypes

    // Phase 2: Convert all functions to MIR
    // Each function gets its own RegGen starting from (maxTempId + 1) for deterministic compilation
    match
        mapResults
            (fun anfFunc -> convertANFFunction anfFunc typeMap typeReg returnTypeReg enableCoverage)
            functions
    with
    | Error err -> Error err
    | Ok mirFuncs ->

    // Convert main expression to a synthetic "_start" function
    // _start gets its own RegGen based on the main expression's TempIds
    let startMaxId = maxTempIdInAExpr mainExpr
    let startRegGen = MIR.RegGen (startMaxId + 1)
    let startTypeById = buildTypeById startMaxId typeMap
    let entryLabel = MIR.Label "_start_body"
    let initialBuilder = {
        RegGen = startRegGen
        LabelGen = MIR.initialLabelGen
        Blocks = Map.empty
        TypeById = startTypeById
        ExtraTypeMap = Map.empty
        TypeReg = typeReg
        ReturnTypeReg = returnTypeReg
        FuncName = "_start"
        ParamRegs = []  // _start has no params
        FloatRegs = Set.empty
        ClosureFuncs = Map.empty
        EnableCoverage = enableCoverage
        ExprIdGen = ANF.initialExprIdGen
        CoverageMapping = ANF.emptyCoverageMapping
    }
    match convertExpr mainExpr entryLabel [] initialBuilder with
    | Error err -> Error err
    | Ok (_, finalBuilder) ->
    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }
    // Use the passed mainExprType for _start's return type
    // This is needed for proper float handling in the Ret terminator
    let startFunc = {
        MIR.Name = "_start"
        MIR.TypedParams = []
        MIR.ReturnType = mainExprType
        MIR.CFG = cfg
        MIR.FloatRegs = finalBuilder.FloatRegs
    }
    let allFuncs = mirFuncs @ [startFunc]
    let variantRegistry = buildVariantRegistry variantLookup
    // Build recordRegistry from typeRegForRecords (converts tuples to RecordField records)
    let recordRegistry = buildRecordRegistry typeRegForRecords
    Ok (MIR.Program (allFuncs, variantRegistry, recordRegistry))

/// Convert ANF program to MIR (functions only, no _start)
/// Use for stdlib where there's no real main expression to convert.
/// Returns just the function list, variant registry, and record registry without wrapping in MIR.Program.
/// externalReturnTypes: return types for functions not in the program (e.g., specialized functions compiled elsewhere)
/// Each function gets its own RegGen for deterministic VReg assignment.
let toMIRFunctionsOnly
    (program: ANF.Program)
    (typeMap: ANF.TypeMap)
    (typeReg: Map<string, (string * AST.Type) list>)
    (variantLookup: AST_to_ANF.VariantLookup)
    (typeRegForRecords: Map<string, (string * AST.Type) list>)
    (enableCoverage: bool)
    (externalReturnTypes: Map<string, AST.Type>)
    : Result<MIR.Function list * MIR.VariantRegistry * MIR.RecordRegistry, string> =
    let (ANF.Program (functions, _mainExpr)) = program

    // Build return type registry for all functions (needed for caller to know return type)
    let returnTypeReg = buildReturnTypeReg functions typeMap typeReg externalReturnTypes

    // Phase 2: Convert all functions to MIR (skip main/_start)
    // Each function gets its own RegGen starting from (maxTempId + 1) for deterministic compilation
    match
        mapResults
            (fun anfFunc -> convertANFFunction anfFunc typeMap typeReg returnTypeReg enableCoverage)
            functions
    with
    | Error err -> Error err
    | Ok mirFuncs ->
        let variantRegistry = buildVariantRegistry variantLookup
        let recordRegistry = buildRecordRegistry typeRegForRecords
        Ok (mirFuncs, variantRegistry, recordRegistry)
