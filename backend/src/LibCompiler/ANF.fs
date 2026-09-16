// ANF.fs - A-Normal Form Intermediate Representation
//
// Defines the ANF (A-Normal Form) data structures.
//
// ANF is an intermediate representation where:
// - All intermediate computations are named with temporary variables
// - All operands to operations are simple (variables or literals, called "atoms")
// - Evaluation order is completely explicit through let-bindings
//
// This representation simplifies subsequent compiler passes by eliminating
// nested expressions.
//
// Example ANF for "2 + 3 * 4":
//   let tmp0 = 3
//   let tmp1 = 4
//   let tmp2 = tmp0 * tmp1
//   let tmp3 = 2
//   let tmp4 = tmp3 + tmp2
//   return tmp4

module ANF

/// Unique identifier for temporary variables
type TempId = TempId of int

/// Parameter with type information bundled together (makes invalid states unrepresentable)
type TypedParam = { Id: TempId; Type: AST.Type }

/// Integer value with explicit size - invalid states unrepresentable
/// Following "make invalid states unrepresentable" principle
type SizedInt =
    | Int8 of sbyte
    | Int16 of int16
    | Int32 of int32
    | Int64 of int64
    | UInt8 of byte
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64

/// Extract int64 value from SizedInt (for codegen)
/// Note: UInt64 values > Int64.MaxValue will be converted incorrectly
let sizedIntToInt64 (si: SizedInt) : int64 =
    match si with
    | Int8 n -> int64 n
    | Int16 n -> int64 n
    | Int32 n -> int64 n
    | Int64 n -> n
    | UInt8 n -> int64 n
    | UInt16 n -> int64 n
    | UInt32 n -> int64 n
    | UInt64 n -> int64 n  // May lose high bit for values > Int64.MaxValue

/// Get the AST.Type corresponding to a SizedInt
let sizedIntToType (si: SizedInt) : AST.Type =
    match si with
    | Int8 _ -> AST.TInt8
    | Int16 _ -> AST.TInt16
    | Int32 _ -> AST.TInt32
    | Int64 _ -> AST.TInt64
    | UInt8 _ -> AST.TUInt8
    | UInt16 _ -> AST.TUInt16
    | UInt32 _ -> AST.TUInt32
    | UInt64 _ -> AST.TUInt64

/// Atomic expressions (cannot be decomposed further)
type Atom =
    | UnitLiteral            // Unit value: ()
    | IntLiteral of SizedInt // Integer with explicit size
    | BoolLiteral of bool
    | StringLiteral of string
    | FloatLiteral of float
    | Var of TempId
    | FuncRef of string  // Reference to a function by name (for higher-order functions)

/// Binary operations on atoms
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    // Bitwise
    | Shl     // << (left shift)
    | Shr     // >> (right shift)
    | BitAnd  // & (bitwise and)
    | BitOr   // | (bitwise or)
    | BitXor  // ^ (bitwise xor)
    // Comparisons
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte
    // Boolean
    | And
    | Or

/// Unary operations on atoms
type UnaryOp =
    | Neg
    | Not
    | BitNot  // Bitwise NOT: ~~~expr

/// Reference-count operation kind
type RcKind =
    | GenericHeap
    | TaggedList

/// Function return ownership convention
type ReturnOwnership =
    | OwnedReturn
    | BorrowedReturn

/// Complex expressions (produce values)
type CExpr =
    | Atom of Atom
    | TypedAtom of Atom * AST.Type  // Atom with explicit type (for pattern matching where inferred types would be wrong)
    | Prim of BinOp * Atom * Atom
    | UnaryPrim of UnaryOp * Atom
    | IfValue of cond:Atom * thenValue:Atom * elseValue:Atom  // If-expression that produces a value
    | Call of funcName:string * args:Atom list  // Function call (direct: BL instruction)
    | BorrowedCall of funcName:string * args:Atom list  // Function call that returns a borrowed/aliased value
    | TailCall of funcName:string * args:Atom list  // Tail call (direct: B instruction, no return)
    | IndirectCall of func:Atom * args:Atom list  // Call through function variable (BLR instruction)
    | IndirectTailCall of func:Atom * args:Atom list  // Tail call through function variable (BR instruction)
    | ClosureAlloc of funcName:string * captures:Atom list  // Allocate closure: (func_addr, cap1, cap2, ...)
    | ClosureCall of closure:Atom * args:Atom list  // Call through closure, passing closure as hidden first arg
    | ClosureTailCall of closure:Atom * args:Atom list  // Tail call through closure (BR instruction)
    | TupleAlloc of Atom list                   // Create tuple: (a, b, c)
    | TupleGet of tuple:Atom * index:int        // Get tuple element: t.0
    // String operations (heap-allocating)
    | StringConcat of left:Atom * right:Atom    // Concatenate strings: s1 ++ s2
    // Reference counting operations
    | RefCountInc of Atom * payloadSize:int * kind:RcKind    // Increment ref count of heap value
    | RefCountDec of Atom * payloadSize:int * kind:RcKind    // Decrement ref count, free if zero
    // Output operations (for main expression result)
    | Print of Atom * AST.Type                 // Print value with type-appropriate formatting
    | RuntimeError of message:string           // Print runtime error to stderr and exit with code 1
    // File I/O intrinsics (generate syscalls)
    | FileReadText of path:Atom               // Read file, returns Result<String, String>
    | FileExists of path:Atom                 // Check if file exists, returns Bool
    | FileWriteText of path:Atom * content:Atom  // Write file, returns Result<Unit, String>
    | FileAppendText of path:Atom * content:Atom // Append to file, returns Result<Unit, String>
    | FileDelete of path:Atom                     // Delete file, returns Result<Unit, String>
    | FileSetExecutable of path:Atom             // Set executable bit, returns Result<Unit, String>
    | FileWriteFromPtr of path:Atom * ptr:Atom * length:Atom  // Write raw bytes from pointer to file
    // Float intrinsics
    | FloatSqrt of Atom                       // Square root: sqrt(x)
    | FloatAbs of Atom                        // Absolute value: |x|
    | FloatNeg of Atom                        // Negate: -x
    | Int64ToFloat of Atom                    // Convert Int64 to Float64
    | FloatToInt64 of Atom                    // Convert Float64 to Int64 (truncate)
    | FloatToBits of Atom                     // Copy Float64 bits to UInt64
    // Raw memory intrinsics (internal, for HAMT implementation)
    | RawAlloc of numBytes:Atom               // Allocate raw bytes (no header), returns RawPtr
    | RawFree of ptr:Atom                     // Manually free raw memory
    | RawGet of ptr:Atom * byteOffset:Atom * valueType:AST.Type option  // Read 8 bytes at offset, valueType for float
    | RawGetByte of ptr:Atom * byteOffset:Atom  // Read 1 byte at offset, returns Int64 (zero-extended)
    | RawSet of ptr:Atom * byteOffset:Atom * value:Atom * valueType:AST.Type option  // Write 8 bytes at offset, valueType for float
    | RawSetByte of ptr:Atom * byteOffset:Atom * value:Atom  // Write 1 byte at offset
    // String reference counting (at dynamic offset)
    | RefCountIncString of Atom               // Increment string ref count
    | RefCountDecString of Atom               // Decrement string ref count, free if zero
    // Random intrinsics
    | RandomInt64                             // Get 8 random bytes as Int64
    // Date intrinsics
    | DateNow                                 // Get current Unix epoch seconds as Int64
    // Float to String conversion
    | FloatToString of Atom                   // Convert Float to heap String

/// ANF expressions with explicit sequencing
type AExpr =
    | Let of TempId * CExpr * AExpr
    | Return of Atom
    | If of cond:Atom * thenBranch:AExpr * elseBranch:AExpr

/// ANF function definition
type Function = {
    Name: string
    TypedParams: TypedParam list  // Parameter IDs with their types bundled
    ReturnType: AST.Type
    ReturnOwnership: ReturnOwnership
    Body: AExpr
}

/// ANF program (functions and main expression)
type Program = Program of functions:Function list * main:AExpr

/// Fresh variable generator (functional style)
type VarGen = VarGen of int

/// Generate a fresh temporary variable
let freshVar (VarGen n) : TempId * VarGen =
    (TempId n, VarGen (n + 1))

/// Initial variable generator
let initialVarGen = VarGen 0

/// Type map for tracking TempId -> Type mappings
/// Used by reference counting pass to determine which values are heap-allocated
type TypeMap = Map<TempId, AST.Type>

/// Program with type information for reference counting
type TypedProgram = {
    Program: Program
    TypeMap: TypeMap
}

/// Check if a type requires reference counting (heap-allocated)
let isHeapType (t: AST.Type) : bool =
    match t with
    | AST.TTuple _ -> true
    | AST.TRecord _ -> true
    | AST.TList _ -> true
    | AST.TSum _ -> true  // Conservative: sum types with payloads are heap-allocated
    | AST.TDict _ -> true  // Dict root pointer is heap-allocated
    | _ -> false

/// Calculate payload size in bytes for a heap-allocated type
let payloadSize (t: AST.Type) (typeReg: Map<string, (string * AST.Type) list>) : int =
    match t with
    | AST.TTuple ts -> List.length ts * 8
    | AST.TRecord (name, _) ->
        match Map.tryFind name typeReg with
        | Some fields -> List.length fields * 8
        | None -> Crash.crash $"payloadSize: Record type '{name}' not found in typeReg"
    | AST.TList _ -> 24  // [tag, head, tail] - same size for all element types
    | AST.TSum _ -> 16  // [tag, payload]
    | AST.TDict _ -> 8  // Root pointer only (HAMT structure is variable-sized raw memory)
    | _ -> 0  // Non-heap types

/// Determine reference-count dispatch kind for a heap type
let rcKind (t: AST.Type) : RcKind =
    match t with
    | AST.TList (AST.TFunction _) -> GenericHeap
    | AST.TList _ -> TaggedList
    | _ -> GenericHeap

// ============================================================================
// Coverage Types
// ============================================================================

/// Unique expression ID for coverage tracking
type ExprId = int

/// Expression ID generator (functional style, like VarGen)
type ExprIdGen = ExprIdGen of int

/// Generate a fresh expression ID
let freshExprId (ExprIdGen n) : ExprId * ExprIdGen =
    (n, ExprIdGen (n + 1))

/// Initial expression ID generator
let initialExprIdGen = ExprIdGen 0

/// Coverage mapping: tracks expression descriptions for reporting
type CoverageMapping = {
    /// ExprId -> description string (e.g., "List.map: Call filter")
    Descriptions: Map<int, string>
    /// Total number of expressions tracked
    TotalExpressions: int
}

/// Empty coverage mapping
let emptyCoverageMapping : CoverageMapping = {
    Descriptions = Map.empty
    TotalExpressions = 0
}

/// Add an expression to the coverage mapping
let addCoverageEntry (exprId: ExprId) (description: string) (mapping: CoverageMapping) : CoverageMapping =
    { mapping with
        Descriptions = Map.add exprId description mapping.Descriptions
        TotalExpressions = max mapping.TotalExpressions (exprId + 1) }
