// 2.4_ANF_Inlining.fs - ANF Function Inlining Pass
//
// Inlines small, non-recursive functions at their call sites to eliminate
// function call overhead.
//
// Heuristics:
// - MaxFunctionSize: Only inline functions with <= N TempIds in body
// - MaxInlineDepth: Limit recursive inlining to prevent code explosion
// - Skip recursive functions (direct and mutual recursion via SCC detection)
// - Skip functions with closures (complex runtime behavior)
// - Skip tail calls (preserve TCO optimization)
//
// Mutual recursion detection uses Kosaraju's algorithm to find strongly
// connected components (SCCs) in the call graph. Any function in an SCC
// of size > 1, or that calls itself, is considered recursive.
//
// Literal arguments
// -----------------
// We inline calls even when arguments are literals by binding each literal to a
// fresh TempId before inlining. This preserves ANF shape and avoids re-evaluating
// literal expressions while allowing more helpers to inline.

module ANF_Inlining

open ANF

/// Inlining configuration
type InliningConfig = {
    /// Maximum function body size (in TempIds) to inline
    MaxFunctionSize: int
    /// Maximum depth of recursive inlining
    MaxInlineDepth: int
}

/// Default inlining configuration
let defaultConfig = {
    MaxFunctionSize = 20
    MaxInlineDepth = 3
}

/// Information about a function for inlining decisions
type FunctionInfo = {
    Func: Function
    Size: int            // Count of TempIds (Let bindings) in body
    IsRecursive: bool    // Calls itself directly
    HasClosures: bool    // Contains ClosureAlloc or ClosureCall
    CallsCount: int      // Number of call sites (for future heuristics)
}

// ============================================================================
// Phase 1: Analysis - Build function info map
// ============================================================================

/// Count TempIds (Let bindings) in an expression
let rec countTempIds (expr: AExpr) : int =
    match expr with
    | Let (_, _, body) -> 1 + countTempIds body
    | Return _ -> 0
    | If (_, thenBranch, elseBranch) ->
        countTempIds thenBranch + countTempIds elseBranch

/// Check if a CExpr contains closures
let cexprHasClosures (cexpr: CExpr) : bool =
    match cexpr with
    | ClosureAlloc _ | ClosureCall _ | ClosureTailCall _ -> true
    | _ -> false

/// Check if expression contains closures
let rec exprHasClosures (expr: AExpr) : bool =
    match expr with
    | Let (_, cexpr, body) ->
        cexprHasClosures cexpr || exprHasClosures body
    | Return _ -> false
    | If (_, thenBranch, elseBranch) ->
        exprHasClosures thenBranch || exprHasClosures elseBranch

/// Collect all function names called in a CExpr
let collectCallsInCExpr (cexpr: CExpr) : Set<string> =
    match cexpr with
    | Call (name, _) -> Set.singleton name
    | TailCall (name, _) -> Set.singleton name
    | _ -> Set.empty

/// Collect all function names called in an expression
let rec collectCalls (expr: AExpr) : Set<string> =
    match expr with
    | Let (_, cexpr, body) ->
        Set.union (collectCallsInCExpr cexpr) (collectCalls body)
    | Return _ -> Set.empty
    | If (_, thenBranch, elseBranch) ->
        Set.union (collectCalls thenBranch) (collectCalls elseBranch)

// ============================================================================
// Mutual Recursion Detection via SCC (Strongly Connected Components)
// Uses Kosaraju's algorithm to find SCCs in the call graph
// ============================================================================

/// Build a call graph from functions: Map<caller, Set<callees>>
let buildCallGraph (funcs: Function list) : Map<string, Set<string>> =
    funcs
    |> List.map (fun f -> (f.Name, collectCalls f.Body))
    |> Map.ofList

/// Build reverse call graph: Map<callee, Set<callers>>
let buildReverseCallGraph (callGraph: Map<string, Set<string>>) : Map<string, Set<string>> =
    callGraph
    |> Map.fold (fun acc caller callees ->
        callees
        |> Set.fold (fun acc' callee ->
            let existing = Map.tryFind callee acc' |> Option.defaultValue Set.empty
            Map.add callee (Set.add caller existing) acc'
        ) acc
    ) Map.empty

/// DFS to compute finish order (for Kosaraju's algorithm)
let rec dfsFinishOrder (graph: Map<string, Set<string>>) (node: string)
                       (visited: Set<string>) (order: string list)
    : Set<string> * string list =
    if Set.contains node visited then
        (visited, order)
    else
        let visited' = Set.add node visited
        let neighbors = Map.tryFind node graph |> Option.defaultValue Set.empty
        let (visited'', order') =
            neighbors
            |> Set.fold (fun (v, o) neighbor ->
                dfsFinishOrder graph neighbor v o
            ) (visited', order)
        (visited'', node :: order')

/// DFS to collect SCC members
let rec dfsCollectSCC (graph: Map<string, Set<string>>) (node: string)
                      (visited: Set<string>) (scc: Set<string>)
    : Set<string> * Set<string> =
    if Set.contains node visited then
        (visited, scc)
    else
        let visited' = Set.add node visited
        let scc' = Set.add node scc
        let neighbors = Map.tryFind node graph |> Option.defaultValue Set.empty
        neighbors
        |> Set.fold (fun (v, c) neighbor ->
            dfsCollectSCC graph neighbor v c
        ) (visited', scc')

/// Find all SCCs using Kosaraju's algorithm
/// Returns list of SCCs, where each SCC is a Set of function names
let findSCCs (funcs: Function list) : Set<string> list =
    let funcNames = funcs |> List.map (fun f -> f.Name) |> Set.ofList
    let callGraph = buildCallGraph funcs
    let reverseGraph = buildReverseCallGraph callGraph

    // Step 1: DFS on original graph to get finish order
    let (_, finishOrder) =
        funcNames
        |> Set.fold (fun (visited, order) name ->
            dfsFinishOrder callGraph name visited order
        ) (Set.empty, [])

    // Step 2: DFS on reverse graph in reverse finish order to find SCCs
    let (_, sccs) =
        finishOrder
        |> List.fold (fun (visited, components) name ->
            if Set.contains name visited then
                (visited, components)
            else
                let (visited', scc) = dfsCollectSCC reverseGraph name visited Set.empty
                (visited', scc :: components)
        ) (Set.empty, [])

    sccs

/// Find all functions involved in mutual recursion (in SCCs of size > 1)
/// or direct self-recursion (calls itself)
let findRecursiveFunctions (funcs: Function list) : Set<string> =
    let sccs = findSCCs funcs
    let callGraph = buildCallGraph funcs

    // Functions in SCCs of size > 1 (mutual recursion)
    let mutuallyRecursive =
        sccs
        |> List.filter (fun scc -> Set.count scc > 1)
        |> List.fold Set.union Set.empty

    // Functions that call themselves (direct recursion)
    let directlyRecursive =
        funcs
        |> List.filter (fun f ->
            let calls = Map.tryFind f.Name callGraph |> Option.defaultValue Set.empty
            Set.contains f.Name calls
        )
        |> List.map (fun f -> f.Name)
        |> Set.ofList

    Set.union mutuallyRecursive directlyRecursive

/// Build function info for a single function
let buildFunctionInfo (recursiveFuncs: Set<string>) (func: Function) : FunctionInfo =
    {
        Func = func
        Size = countTempIds func.Body
        IsRecursive = Set.contains func.Name recursiveFuncs
        HasClosures = exprHasClosures func.Body
        CallsCount = 0  // Will be updated later if needed
    }

/// Build function info map for all functions
let buildFunctionInfoMap (funcs: Function list) : Map<string, FunctionInfo> =
    // First, find all recursive functions (direct and mutual)
    let recursiveFuncs = findRecursiveFunctions funcs
    // Then build info for each function
    funcs
    |> List.map (fun f -> (f.Name, buildFunctionInfo recursiveFuncs f))
    |> Map.ofList

// ============================================================================
// Phase 2: TempId Renaming - Avoid variable conflicts when inlining
// ============================================================================

/// Rename an atom (substitute TempIds)
let renameAtom (mapping: Map<TempId, TempId>) (atom: Atom) : Atom =
    match atom with
    | Var tid ->
        match Map.tryFind tid mapping with
        | Some newTid -> Var newTid
        | None -> atom  // External reference, keep as-is
    | _ -> atom

/// Rename all TempIds in a CExpr
let renameCExpr (mapping: Map<TempId, TempId>) (cexpr: CExpr) : CExpr =
    let r = renameAtom mapping
    match cexpr with
    | Atom a -> Atom (r a)
    | TypedAtom (a, t) -> TypedAtom (r a, t)
    | Prim (op, left, right) -> Prim (op, r left, r right)
    | UnaryPrim (op, src) -> UnaryPrim (op, r src)
    | IfValue (cond, thenVal, elseVal) -> IfValue (r cond, r thenVal, r elseVal)
    | Call (name, args) -> Call (name, List.map r args)
    | BorrowedCall (name, args) -> BorrowedCall (name, List.map r args)
    | TailCall (name, args) -> TailCall (name, List.map r args)
    | IndirectCall (func, args) -> IndirectCall (r func, List.map r args)
    | IndirectTailCall (func, args) -> IndirectTailCall (r func, List.map r args)
    | ClosureAlloc (name, captures) -> ClosureAlloc (name, List.map r captures)
    | ClosureCall (closure, args) -> ClosureCall (r closure, List.map r args)
    | ClosureTailCall (closure, args) -> ClosureTailCall (r closure, List.map r args)
    | TupleAlloc elems -> TupleAlloc (List.map r elems)
    | TupleGet (tuple, idx) -> TupleGet (r tuple, idx)
    | StringConcat (left, right) -> StringConcat (r left, r right)
    | RefCountInc (a, size, kind) -> RefCountInc (r a, size, kind)
    | RefCountDec (a, size, kind) -> RefCountDec (r a, size, kind)
    | Print (a, t) -> Print (r a, t)
    | FileReadText path -> FileReadText (r path)
    | FileExists path -> FileExists (r path)
    | FileWriteText (path, content) -> FileWriteText (r path, r content)
    | FileAppendText (path, content) -> FileAppendText (r path, r content)
    | FileDelete path -> FileDelete (r path)
    | FileSetExecutable path -> FileSetExecutable (r path)
    | FileWriteFromPtr (path, ptr, len) -> FileWriteFromPtr (r path, r ptr, r len)
    | FloatSqrt a -> FloatSqrt (r a)
    | FloatAbs a -> FloatAbs (r a)
    | FloatNeg a -> FloatNeg (r a)
    | Int64ToFloat a -> Int64ToFloat (r a)
    | FloatToInt64 a -> FloatToInt64 (r a)
    | FloatToBits a -> FloatToBits (r a)
    | RawAlloc numBytes -> RawAlloc (r numBytes)
    | RawFree ptr -> RawFree (r ptr)
    | RawGet (ptr, offset, valueType) -> RawGet (r ptr, r offset, valueType)
    | RawGetByte (ptr, offset) -> RawGetByte (r ptr, r offset)
    | RawSet (ptr, offset, value, valueType) -> RawSet (r ptr, r offset, r value, valueType)
    | RawSetByte (ptr, offset, value) -> RawSetByte (r ptr, r offset, r value)
    | RefCountIncString a -> RefCountIncString (r a)
    | RefCountDecString a -> RefCountDecString (r a)
    | RandomInt64 -> RandomInt64
    | DateNow -> DateNow
    | FloatToString a -> FloatToString (r a)
    | RuntimeError message -> RuntimeError message

/// Rename all TempIds in an expression, allocating fresh TempIds
let rec renameExpr (mapping: Map<TempId, TempId>) (varGen: VarGen) (expr: AExpr)
    : AExpr * VarGen =
    match expr with
    | Let (tid, cexpr, body) ->
        // Allocate fresh TempId for this binding
        let (newTid, varGen') = freshVar varGen
        let mapping' = Map.add tid newTid mapping
        // Rename the CExpr (uses old mapping for references)
        let cexpr' = renameCExpr mapping cexpr
        // Rename the body (uses new mapping including this binding)
        let (body', varGen'') = renameExpr mapping' varGen' body
        (Let (newTid, cexpr', body'), varGen'')
    | Return atom ->
        (Return (renameAtom mapping atom), varGen)
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen') = renameExpr mapping varGen thenBranch
        let (elseBranch', varGen'') = renameExpr mapping varGen' elseBranch
        (If (renameAtom mapping cond, thenBranch', elseBranch'), varGen'')

// ============================================================================
// Phase 3: Inlining - Substitute function calls with bodies
// ============================================================================

/// Check if a function should be inlined
let shouldInline (info: FunctionInfo) (config: InliningConfig) (depth: int) : bool =
    info.Size <= config.MaxFunctionSize
    && not info.IsRecursive
    && not info.HasClosures
    && depth < config.MaxInlineDepth

/// Substitute Return with a continuation expression
/// This replaces `Return atom` with a binding and continues with the rest
let rec substituteReturn (resultTid: TempId) (continuation: AExpr) (expr: AExpr) : AExpr =
    match expr with
    | Return atom ->
        // Replace return with a binding to resultTid, then continue
        Let (resultTid, Atom atom, continuation)
    | Let (tid, cexpr, body) ->
        Let (tid, cexpr, substituteReturn resultTid continuation body)
    | If (cond, thenBranch, elseBranch) ->
        If (cond,
            substituteReturn resultTid continuation thenBranch,
            substituteReturn resultTid continuation elseBranch)

/// Bind literal arguments to fresh TempIds and build parameter mapping
let bindLiteralArgs
    (parameters: TypedParam list)
    (args: Atom list)
    (varGen: VarGen)
    : Map<TempId, TempId> * (TempId * Atom) list * VarGen =
    let rec loop
        (paramList: TypedParam list)
        (args: Atom list)
        (mapping: Map<TempId, TempId>)
        (bindings: (TempId * Atom) list)
        (varGen: VarGen)
        : Map<TempId, TempId> * (TempId * Atom) list * VarGen =
        match paramList, args with
        | [], [] -> (mapping, List.rev bindings, varGen)
        | param :: restParams, arg :: restArgs ->
            match arg with
            | Var tid ->
                loop restParams restArgs (Map.add param.Id tid mapping) bindings varGen
            | _ ->
                let (litTid, varGen') = freshVar varGen
                loop restParams restArgs (Map.add param.Id litTid mapping) ((litTid, arg) :: bindings) varGen'
        | _ ->
            Crash.crash "ANF_Inlining: argument count mismatch when inlining"

    loop parameters args Map.empty [] varGen

/// Inline a function call
/// Returns the inlined expression and updated VarGen
let inlineCall (info: FunctionInfo) (args: Atom list) (resultTid: TempId)
               (continuation: AExpr) (varGen: VarGen)
    : AExpr * VarGen =
    // Step 1: Bind literal args and build parameter -> TempId mapping
    let (paramMapping, literalBindings, varGen') =
        bindLiteralArgs info.Func.TypedParams args varGen

    // Step 2: Rename all TempIds in the function body to fresh ones
    let (renamedBody, varGen'') = renameExpr paramMapping varGen' info.Func.Body

    // Step 3: Insert literal bindings in front of the body
    let bodyWithLiteralBindings =
        List.foldBack (fun (tid, atom) acc -> Let (tid, Atom atom, acc)) literalBindings renamedBody

    // Step 4: Substitute Return with continuation
    let inlinedExpr = substituteReturn resultTid continuation bodyWithLiteralBindings

    (inlinedExpr, varGen'')

/// Recursively inline calls in an expression
let rec inlineInExpr (funcs: Map<string, FunctionInfo>) (config: InliningConfig)
                     (depth: int) (varGen: VarGen) (expr: AExpr)
    : AExpr * VarGen * bool =  // Returns (expr, varGen, changed)
    match expr with
    | Let (tid, Call (funcName, args), body) ->
        // Check if this is a regular call (not tail call) to a user function
        match Map.tryFind funcName funcs with
        | Some info when shouldInline info config depth ->
            // Inline the call
            let (inlinedExpr, varGen') = inlineCall info args tid body varGen
            // Recursively inline in the result (with increased depth)
            let (result, varGen'', _) = inlineInExpr funcs config (depth + 1) varGen' inlinedExpr
            (result, varGen'', true)
        | _ ->
            // Don't inline - continue processing body
            let (body', varGen', changed) = inlineInExpr funcs config depth varGen body
            (Let (tid, Call (funcName, args), body'), varGen', changed)

    | Let (tid, cexpr, body) ->
        // Not a call, just process the body
        let (body', varGen', changed) = inlineInExpr funcs config depth varGen body
        (Let (tid, cexpr, body'), varGen', changed)

    | Return atom ->
        (Return atom, varGen, false)

    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen', changed1) = inlineInExpr funcs config depth varGen thenBranch
        let (elseBranch', varGen'', changed2) = inlineInExpr funcs config depth varGen' elseBranch
        (If (cond, thenBranch', elseBranch'), varGen'', changed1 || changed2)

/// Inline in a function body
let inlineInFunction (funcs: Map<string, FunctionInfo>) (config: InliningConfig)
                     (varGen: VarGen) (func: Function)
    : Function * VarGen * bool =
    let (body', varGen', changed) = inlineInExpr funcs config 0 varGen func.Body
    ({ func with Body = body' }, varGen', changed)

/// Find the maximum TempId used in an expression
let rec maxTempId (expr: AExpr) : int =
    match expr with
    | Let (TempId n, _, body) -> max n (maxTempId body)
    | Return (Var (TempId n)) -> n
    | Return _ -> 0
    | If (Var (TempId n), thenBranch, elseBranch) ->
        max n (max (maxTempId thenBranch) (maxTempId elseBranch))
    | If (_, thenBranch, elseBranch) ->
        max (maxTempId thenBranch) (maxTempId elseBranch)

/// Find the maximum TempId in a function
let maxTempIdInFunction (func: Function) : int =
    let paramMax = func.TypedParams |> List.map (fun p -> let (TempId n) = p.Id in n) |> List.fold max 0
    max paramMax (maxTempId func.Body)

/// Find the maximum TempId in a program
let maxTempIdInProgram (Program (funcs, main)) : int =
    let funcMax = funcs |> List.map maxTempIdInFunction |> List.fold max 0
    max funcMax (maxTempId main)

// ============================================================================
// Phase 4: Main entry point
// ============================================================================

/// Inline functions in a program
let inlineProgram (config: InliningConfig) (program: Program) : Program =
    let (Program (funcs, main)) = program

    // Build function info map (only for user functions, not stdlib)
    let funcInfoMap = buildFunctionInfoMap funcs

    // Find starting VarGen value (must be higher than any existing TempId)
    let startVarGen = VarGen (maxTempIdInProgram program + 1)

    // Inline in each function (single pass for now)
    let (funcs', varGen', _) =
        funcs
        |> List.fold (fun (accFuncs, varGen, anyChanged) func ->
            let (func', varGen', changed) = inlineInFunction funcInfoMap config varGen func
            (func' :: accFuncs, varGen', anyChanged || changed)
        ) ([], startVarGen, false)

    // Inline in main expression
    let (main', _, _) = inlineInExpr funcInfoMap config 0 varGen' main

    Program (List.rev funcs', main')

/// Inline functions with default configuration
let inlineProgramDefault (program: Program) : Program =
    inlineProgram defaultConfig program
