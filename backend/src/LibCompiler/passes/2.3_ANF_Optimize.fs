// 2.3_ANF_Optimize.fs - ANF Optimization Pass
//
// Performs optimizations on ANF before reference counting:
// - Constant folding: evaluate constant expressions at compile time
// - Constant propagation: replace variable uses with constant definitions
// - Copy propagation: eliminate trivial bindings
// - Dead code elimination: remove unused bindings
// - Strength reduction: replace pow2 mul/div/mod with shifts/bitwise ops
//
// These optimizations run in a loop until no more changes occur.

module ANF_Optimize

open ANF

/// Environment mapping TempIds to their constant values (for propagation)
type ConstEnv = Map<TempId, Atom>

/// Optimization toggles for ANF optimization passes
type OptimizeOptions = {
    EnableConstFolding: bool
    EnableConstProp: bool
    EnableCopyProp: bool
    EnableDCE: bool
    EnableStrengthReduction: bool
}

let defaultOptimizeOptions = {
    EnableConstFolding = true
    EnableConstProp = true
    EnableCopyProp = true
    EnableDCE = true
    EnableStrengthReduction = true
}

/// Check if n is a power of 2, and if so return its log2
/// Returns None if n is not a power of 2 or is <= 0
let tryLog2 (n: int64) : int64 option =
    if n <= 0L || (n &&& (n - 1L)) <> 0L then None
    else
        let rec countBits acc x =
            if x = 1L then acc
            else countBits (acc + 1L) (x >>> 1)
        Some (countBits 0L n)

/// Euclidean modulo: result has the sign of the divisor
let euclideanMod (a: int64) (b: int64) : int64 =
    let remainder = a % b
    if remainder = 0L then 0L
    elif (remainder > 0L && b < 0L) || (remainder < 0L && b > 0L) then remainder + b
    else remainder

/// Fold a binary operation on constants
/// Only folds Int64 for now - other integer types need proper overflow handling at runtime
let foldBinOp (op: BinOp) (left: Atom) (right: Atom) : CExpr option =
    match op, left, right with
    // Int64 arithmetic (unchecked - overflow wraps)
    | Add, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a + b))))
    | Sub, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a - b))))
    | Mul, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a * b))))
    | Div, IntLiteral (Int64 a), IntLiteral (Int64 b) when b <> 0L && not (a = System.Int64.MinValue && b = -1L) -> Some (Atom (IntLiteral (Int64 (a / b))))
    // Skip folding INT64_MIN / -1 - F# throws but runtime handles it (returns INT64_MIN)
    | Div, IntLiteral (Int64 _), IntLiteral (Int64 _) -> None
    | Mod, IntLiteral (Int64 a), IntLiteral (Int64 b) when b > 0L -> Some (Atom (IntLiteral (Int64 (euclideanMod a b))))

    // Float arithmetic
    | Add, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a + b)))
    | Sub, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a - b)))
    | Mul, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a * b)))
    | Div, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a / b)))

    // Int64 comparisons
    | Eq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a = b)))
    | Neq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <> b)))
    | Lt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a < b)))
    | Gt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a > b)))
    | Lte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <= b)))
    | Gte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a >= b)))

    // Boolean comparisons
    | Eq, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a = b)))
    | Neq, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a <> b)))

    // Boolean operations
    | And, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a && b)))
    | Or, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a || b)))

    // String comparisons
    | Eq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a = b)))
    | Neq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a <> b)))

    // Algebraic identities (strength reduction) - Int64
    | Add, IntLiteral (Int64 0L), x -> Some (Atom x)
    | Add, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Sub, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Mul, IntLiteral (Int64 1L), x -> Some (Atom x)
    | Mul, x, IntLiteral (Int64 1L) -> Some (Atom x)
    | Mul, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
    | Mul, _, IntLiteral (Int64 0L) -> Some (Atom (IntLiteral (Int64 0L)))
    | Div, x, IntLiteral (Int64 1L) -> Some (Atom x)

    // Algebraic identities - Float
    // Note: We skip 0.0 * x -> 0.0 because 0.0 * inf = NaN, 0.0 * NaN = NaN
    | Add, FloatLiteral 0.0, x -> Some (Atom x)
    | Add, x, FloatLiteral 0.0 -> Some (Atom x)
    | Sub, x, FloatLiteral 0.0 -> Some (Atom x)
    | Mul, FloatLiteral 1.0, x -> Some (Atom x)
    | Mul, x, FloatLiteral 1.0 -> Some (Atom x)
    | Div, x, FloatLiteral 1.0 -> Some (Atom x)

    // Self-subtraction: x - x -> 0 (only for Int64, not Float due to NaN)
    | Sub, Var a, Var b when a = b -> Some (Atom (IntLiteral (Int64 0L)))

    // Short-circuit boolean
    | And, BoolLiteral false, _ -> Some (Atom (BoolLiteral false))
    | And, _, BoolLiteral false -> Some (Atom (BoolLiteral false))
    | And, BoolLiteral true, x -> Some (Atom x)
    | And, x, BoolLiteral true -> Some (Atom x)
    | Or, BoolLiteral true, _ -> Some (Atom (BoolLiteral true))
    | Or, _, BoolLiteral true -> Some (Atom (BoolLiteral true))
    | Or, BoolLiteral false, x -> Some (Atom x)
    | Or, x, BoolLiteral false -> Some (Atom x)

    | _ -> None

let tryStrengthReduce (op: BinOp) (left: Atom) (right: Atom) : CExpr option =
    match op, left, right with
    | Mul, x, IntLiteral (Int64 n) ->
        match tryLog2 n with
        | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
        | None -> None
    | Mul, IntLiteral (Int64 n), x ->
        match tryLog2 n with
        | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
        | None -> None
    | Mod, x, IntLiteral (Int64 n) when n > 0L ->
        // For positive power-of-two divisors, Euclidean remainder equals x & (n - 1)
        match tryLog2 n with
        | Some _ -> Some (Prim (BitAnd, x, IntLiteral (Int64 (n - 1L))))
        | None -> None
    | Div, x, IntLiteral (Int64 n) when n > 0L ->
        match tryLog2 n with
        | Some shift -> Some (Prim (Shr, x, IntLiteral (Int64 shift)))
        | None -> None
    // Float strength reduction: 2.0 * x -> x + x
    | Mul, FloatLiteral 2.0, x -> Some (Prim (Add, x, x))
    | Mul, x, FloatLiteral 2.0 -> Some (Prim (Add, x, x))
    // Float division by power of 2 -> multiplication by reciprocal
    // These reciprocals are exactly representable in IEEE 754
    | Div, x, FloatLiteral 2.0 -> Some (Prim (Mul, x, FloatLiteral 0.5))
    | Div, x, FloatLiteral 4.0 -> Some (Prim (Mul, x, FloatLiteral 0.25))
    | Div, x, FloatLiteral 8.0 -> Some (Prim (Mul, x, FloatLiteral 0.125))
    | Div, x, FloatLiteral 16.0 -> Some (Prim (Mul, x, FloatLiteral 0.0625))
    | Div, x, FloatLiteral 32.0 -> Some (Prim (Mul, x, FloatLiteral 0.03125))
    | Div, x, FloatLiteral 64.0 -> Some (Prim (Mul, x, FloatLiteral 0.015625))
    | Div, x, FloatLiteral 128.0 -> Some (Prim (Mul, x, FloatLiteral 0.0078125))
    | Div, x, FloatLiteral 256.0 -> Some (Prim (Mul, x, FloatLiteral 0.00390625))
    | _ -> None

/// Fold a unary operation on constants
let foldUnaryOp (op: UnaryOp) (src: Atom) : CExpr option =
    match op, src with
    // Int64 negation (unchecked - INT64_MIN wraps to itself)
    | Neg, IntLiteral (Int64 n) -> Some (Atom (IntLiteral (Int64 (-n))))
    | Neg, FloatLiteral f -> Some (Atom (FloatLiteral (-f)))
    | Not, BoolLiteral b -> Some (Atom (BoolLiteral (not b)))
    // Bitwise NOT: flip all bits
    | BitNot, IntLiteral (Int64 n) -> Some (Atom (IntLiteral (Int64 (~~~n))))
    | _ -> None

/// Check if a CExpr has side effects
let hasSideEffects (cexpr: CExpr) : bool =
    match cexpr with
    | Atom _ -> false
    | TypedAtom _ -> false
    | Prim _ -> false
    | UnaryPrim _ -> false
    | IfValue _ -> false
    | TupleAlloc _ -> false
    | TupleGet _ -> false
    // These have side effects
    | Call _ -> true
    | BorrowedCall _ -> true
    | TailCall _ -> true
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true
    | StringConcat _ -> true  // Allocates memory
    | RefCountInc _ -> true
    | RefCountDec _ -> true
    | Print _ -> true
    | FileReadText _ -> true
    | FileExists _ -> true
    | FileWriteText _ -> true
    | FileAppendText _ -> true
    | FileDelete _ -> true
    | FileSetExecutable _ -> true
    | FileWriteFromPtr _ -> true  // File I/O
    | RawAlloc _ -> true  // Allocates memory
    | RawFree _ -> true   // Frees memory
    | RawGet _ -> false   // Pure memory read
    | RawGetByte _ -> false  // Pure memory read (byte)
    | RawSet _ -> true    // Memory mutation
    | RawSetByte _ -> true  // Memory mutation (byte)
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | Int64ToFloat _ -> false // Pure conversion
    | FloatToInt64 _ -> false // Pure conversion
    | FloatToBits _ -> false // Pure conversion
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RandomInt64 -> true   // Reads from OS random source
    | DateNow -> true       // Reads current time (syscall)
    | FloatToString _ -> false  // Pure conversion (but allocates - maybe should be true?)
    | RuntimeError _ -> true

/// Collect all TempIds used in an atom
let collectAtomUses (atom: Atom) : Set<TempId> =
    match atom with
    | Var tid -> Set.singleton tid
    | _ -> Set.empty

/// Collect all TempIds used in a CExpr
let collectCExprUses (cexpr: CExpr) : Set<TempId> =
    match cexpr with
    | Atom a -> collectAtomUses a
    | TypedAtom (a, _) -> collectAtomUses a
    | Prim (_, left, right) -> Set.union (collectAtomUses left) (collectAtomUses right)
    | UnaryPrim (_, src) -> collectAtomUses src
    | IfValue (cond, thenVal, elseVal) ->
        Set.unionMany [collectAtomUses cond; collectAtomUses thenVal; collectAtomUses elseVal]
    | Call (_, args) -> args |> List.map collectAtomUses |> Set.unionMany
    | BorrowedCall (_, args) -> args |> List.map collectAtomUses |> Set.unionMany
    | TailCall (_, args) -> args |> List.map collectAtomUses |> Set.unionMany
    | IndirectCall (func, args) ->
        Set.unionMany ((collectAtomUses func) :: (args |> List.map collectAtomUses))
    | IndirectTailCall (func, args) ->
        Set.unionMany ((collectAtomUses func) :: (args |> List.map collectAtomUses))
    | ClosureAlloc (_, captures) -> captures |> List.map collectAtomUses |> Set.unionMany
    | ClosureCall (closure, args) ->
        Set.unionMany ((collectAtomUses closure) :: (args |> List.map collectAtomUses))
    | ClosureTailCall (closure, args) ->
        Set.unionMany ((collectAtomUses closure) :: (args |> List.map collectAtomUses))
    | TupleAlloc elems -> elems |> List.map collectAtomUses |> Set.unionMany
    | TupleGet (tuple, _) -> collectAtomUses tuple
    | StringConcat (left, right) -> Set.union (collectAtomUses left) (collectAtomUses right)
    | RefCountInc (atom, _, _) -> collectAtomUses atom
    | RefCountDec (atom, _, _) -> collectAtomUses atom
    | Print (atom, _) -> collectAtomUses atom
    | FileReadText path -> collectAtomUses path
    | FileExists path -> collectAtomUses path
    | FileWriteText (path, content) -> Set.union (collectAtomUses path) (collectAtomUses content)
    | FileAppendText (path, content) -> Set.union (collectAtomUses path) (collectAtomUses content)
    | FileDelete path -> collectAtomUses path
    | FileSetExecutable path -> collectAtomUses path
    | FileWriteFromPtr (path, ptr, length) -> Set.unionMany [collectAtomUses path; collectAtomUses ptr; collectAtomUses length]
    | RawAlloc numBytes -> collectAtomUses numBytes
    | RawFree ptr -> collectAtomUses ptr
    | RawGet (ptr, byteOffset, _) -> Set.union (collectAtomUses ptr) (collectAtomUses byteOffset)
    | RawGetByte (ptr, byteOffset) -> Set.union (collectAtomUses ptr) (collectAtomUses byteOffset)
    | RawSet (ptr, byteOffset, value, _) -> Set.unionMany [collectAtomUses ptr; collectAtomUses byteOffset; collectAtomUses value]
    | RawSetByte (ptr, byteOffset, value) -> Set.unionMany [collectAtomUses ptr; collectAtomUses byteOffset; collectAtomUses value]
    | FloatSqrt atom -> collectAtomUses atom
    | FloatAbs atom -> collectAtomUses atom
    | FloatNeg atom -> collectAtomUses atom
    | Int64ToFloat atom -> collectAtomUses atom
    | FloatToInt64 atom -> collectAtomUses atom
    | FloatToBits atom -> collectAtomUses atom
    | RefCountIncString str -> collectAtomUses str
    | RefCountDecString str -> collectAtomUses str
    | RandomInt64 -> Set.empty  // No atoms
    | DateNow -> Set.empty      // No atoms
    | FloatToString atom -> collectAtomUses atom
    | RuntimeError _ -> Set.empty

/// Substitute atom in another atom
let substAtom (env: Map<TempId, Atom>) (atom: Atom) : Atom =
    match atom with
    | Var tid -> Map.tryFind tid env |> Option.defaultValue atom
    | _ -> atom

/// Substitute atoms in CExpr
let substCExpr (env: Map<TempId, Atom>) (cexpr: CExpr) : CExpr =
    let s = substAtom env
    match cexpr with
    | Atom a -> Atom (s a)
    | TypedAtom (a, t) -> TypedAtom (s a, t)
    | Prim (op, left, right) -> Prim (op, s left, s right)
    | UnaryPrim (op, src) -> UnaryPrim (op, s src)
    | IfValue (cond, thenVal, elseVal) -> IfValue (s cond, s thenVal, s elseVal)
    | Call (name, args) -> Call (name, List.map s args)
    | BorrowedCall (name, args) -> BorrowedCall (name, List.map s args)
    | TailCall (name, args) -> TailCall (name, List.map s args)
    | IndirectCall (func, args) -> IndirectCall (s func, List.map s args)
    | IndirectTailCall (func, args) -> IndirectTailCall (s func, List.map s args)
    | ClosureAlloc (name, captures) -> ClosureAlloc (name, List.map s captures)
    | ClosureCall (closure, args) -> ClosureCall (s closure, List.map s args)
    | ClosureTailCall (closure, args) -> ClosureTailCall (s closure, List.map s args)
    | TupleAlloc elems -> TupleAlloc (List.map s elems)
    | TupleGet (tuple, idx) -> TupleGet (s tuple, idx)
    | StringConcat (left, right) -> StringConcat (s left, s right)
    | RefCountInc (atom, size, kind) -> RefCountInc (s atom, size, kind)
    | RefCountDec (atom, size, kind) -> RefCountDec (s atom, size, kind)
    | Print (atom, t) -> Print (s atom, t)
    | FileReadText path -> FileReadText (s path)
    | FileExists path -> FileExists (s path)
    | FileWriteText (path, content) -> FileWriteText (s path, s content)
    | FileAppendText (path, content) -> FileAppendText (s path, s content)
    | FileDelete path -> FileDelete (s path)
    | FileSetExecutable path -> FileSetExecutable (s path)
    | FileWriteFromPtr (path, ptr, length) -> FileWriteFromPtr (s path, s ptr, s length)
    | RawAlloc numBytes -> RawAlloc (s numBytes)
    | RawFree ptr -> RawFree (s ptr)
    | RawGet (ptr, byteOffset, valueType) -> RawGet (s ptr, s byteOffset, valueType)
    | RawGetByte (ptr, byteOffset) -> RawGetByte (s ptr, s byteOffset)
    | RawSet (ptr, byteOffset, value, valueType) -> RawSet (s ptr, s byteOffset, s value, valueType)
    | RawSetByte (ptr, byteOffset, value) -> RawSetByte (s ptr, s byteOffset, s value)
    | FloatSqrt atom -> FloatSqrt (s atom)
    | FloatAbs atom -> FloatAbs (s atom)
    | FloatNeg atom -> FloatNeg (s atom)
    | Int64ToFloat atom -> Int64ToFloat (s atom)
    | FloatToInt64 atom -> FloatToInt64 (s atom)
    | FloatToBits atom -> FloatToBits (s atom)
    | RefCountIncString str -> RefCountIncString (s str)
    | RefCountDecString str -> RefCountDecString (s str)
    | RandomInt64 -> RandomInt64
    | DateNow -> DateNow
    | FloatToString atom -> FloatToString (s atom)
    | RuntimeError message -> RuntimeError message

/// Optimize a CExpr with constant folding
let optimizeCExpr (options: OptimizeOptions) (env: ConstEnv) (cexpr: CExpr) : CExpr * bool =
    // First, substitute known constants
    let cexpr' = substCExpr env cexpr

    let tryConstFold () =
        if options.EnableConstFolding then
            match cexpr' with
            | Prim (op, left, right) ->
                match foldBinOp op left right with
                | Some folded -> Some folded
                | None -> None
            | UnaryPrim (op, src) -> foldUnaryOp op src
            | IfValue (BoolLiteral true, thenVal, _) -> Some (Atom thenVal)
            | IfValue (BoolLiteral false, _, elseVal) -> Some (Atom elseVal)
            | _ -> None
        else
            None

    match tryConstFold () with
    | Some folded -> (folded, true)
    | None ->
        if options.EnableStrengthReduction then
            match cexpr' with
            | Prim (op, left, right) ->
                match tryStrengthReduce op left right with
                | Some reduced -> (reduced, true)
                | None -> (cexpr', cexpr' <> cexpr)
            | _ -> (cexpr', cexpr' <> cexpr)
        else
            (cexpr', cexpr' <> cexpr)

type OptimizeAExprResult = {
    Expr: AExpr
    Changed: bool
    Uses: Set<TempId>
}

/// Optimize an AExpr, returning optimized expression, change flag, and used TempIds
let rec private optimizeAExprWithUses (options: OptimizeOptions) (env: ConstEnv) (aexpr: AExpr) : OptimizeAExprResult =
    match aexpr with
    | Return atom ->
        let atom' = substAtom env atom
        {
            Expr = Return atom'
            Changed = atom' <> atom
            Uses = collectAtomUses atom'
        }

    | Let (tid, cexpr, body) ->
        // Optimize the CExpr
        let (cexpr', cexprChanged) = optimizeCExpr options env cexpr

        // Check for copy propagation: if cexpr is just an Atom, substitute it
        let (env', skipBinding) =
            match cexpr' with
            | Atom a when options.EnableCopyProp && not (hasSideEffects cexpr') ->
                // Copy propagation: don't emit binding, just substitute
                (Map.add tid a env, true)
            | Atom (IntLiteral _ | BoolLiteral _ | FloatLiteral _ | StringLiteral _ | UnitLiteral as constAtom)
                when options.EnableConstProp ->
                // Constant propagation
                (Map.add tid constAtom env, false)
            | _ ->
                (env, false)

        // Optimize the body
        let bodyResult = optimizeAExprWithUses options env' body

        // Dead code elimination: if tid is not used in body and cexpr has no side effects
        let usesInBody = bodyResult.Uses
        let isDead = options.EnableDCE && not (Set.contains tid usesInBody) && not (hasSideEffects cexpr')
        let usesInBodyWithoutTid = Set.remove tid usesInBody

        if skipBinding then
            // Copy propagation: skip this binding entirely
            {
                Expr = bodyResult.Expr
                Changed = true
                Uses = usesInBodyWithoutTid
            }
        elif isDead then
            // Dead code elimination
            {
                Expr = bodyResult.Expr
                Changed = true
                Uses = usesInBodyWithoutTid
            }
        else
            let usesInCExpr = collectCExprUses cexpr'
            let uses = Set.union usesInCExpr usesInBodyWithoutTid
            {
                Expr = Let (tid, cexpr', bodyResult.Expr)
                Changed = cexprChanged || bodyResult.Changed
                Uses = uses
            }

    | If (cond, thenBranch, elseBranch) ->
        let cond' = substAtom env cond

        // Fold constant conditions
        match cond' with
        | BoolLiteral true when options.EnableConstFolding ->
            let thenResult = optimizeAExprWithUses options env thenBranch
            {
                Expr = thenResult.Expr
                Changed = true
                Uses = thenResult.Uses
            }
        | BoolLiteral false when options.EnableConstFolding ->
            let elseResult = optimizeAExprWithUses options env elseBranch
            {
                Expr = elseResult.Expr
                Changed = true
                Uses = elseResult.Uses
            }
        | _ ->
            let thenResult = optimizeAExprWithUses options env thenBranch
            let elseResult = optimizeAExprWithUses options env elseBranch
            let uses = Set.unionMany [collectAtomUses cond'; thenResult.Uses; elseResult.Uses]
            {
                Expr = If (cond', thenResult.Expr, elseResult.Expr)
                Changed = cond' <> cond || thenResult.Changed || elseResult.Changed
                Uses = uses
            }

/// Optimize an AExpr
let optimizeAExpr (options: OptimizeOptions) (env: ConstEnv) (aexpr: AExpr) : AExpr * bool =
    let result = optimizeAExprWithUses options env aexpr
    (result.Expr, result.Changed)

/// Optimize a function
let optimizeFunction (options: OptimizeOptions) (func: Function) : Function * bool =
    // Initialize env with function parameters (they're not constants)
    let env = Map.empty
    let (body', changed) = optimizeAExpr options env func.Body
    ({ func with Body = body' }, changed)

/// Optimize until fixed point
let rec optimizeToFixedPoint (options: OptimizeOptions) (func: Function) (maxIterations: int) : Function =
    if maxIterations <= 0 then func
    else
        let (func', changed) = optimizeFunction options func
        if changed then
            optimizeToFixedPoint options func' (maxIterations - 1)
        else
            func'

/// Optimize a program with explicit options
let optimizeProgramWithOptions (options: OptimizeOptions) (program: Program) : Program =
    let (Program (functions, mainExpr)) = program

    // Optimize all functions
    let functions' = functions |> List.map (fun f -> optimizeToFixedPoint options f 10)

    // Optimize main expression
    let mainFunc = { Name = "__main__"
                     TypedParams = []
                     ReturnType = AST.TUnit
                     ReturnOwnership = OwnedReturn
                     Body = mainExpr }
    let mainOptimized = optimizeToFixedPoint options mainFunc 10

    Program (functions', mainOptimized.Body)

/// Optimize a program with default options
let optimizeProgram (program: Program) : Program =
    optimizeProgramWithOptions defaultOptimizeOptions program

let optimizeConstFolding (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = true
            EnableConstProp = false
            EnableCopyProp = false
            EnableDCE = false
            EnableStrengthReduction = false }
        program

let optimizeCopyProp (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableConstProp = false
            EnableCopyProp = true
            EnableDCE = false
            EnableStrengthReduction = false }
        program

let optimizeDCE (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableConstProp = false
            EnableCopyProp = false
            EnableDCE = true
            EnableStrengthReduction = false }
        program
