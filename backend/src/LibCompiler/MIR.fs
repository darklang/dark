// MIR.fs - Mid-level Intermediate Representation
//
// Defines the MIR (Mid-level IR) data structures.
//
// MIR is a platform-independent three-address code representation where:
// - Each instruction has at most two operands and one destination
// - Virtual registers are used (infinite supply)
// - Instructions are organized into basic blocks
//
// Example MIR:
//   v0 <- 2
//   v1 <- 3
//   v2 <- v0 + v1
//   ret v2

module MIR

/// Virtual register (infinite supply)
type VReg = VReg of int

/// Parameter with register and type bundled (makes invalid states unrepresentable)
type TypedMIRParam = { Reg: VReg; Type: AST.Type }

/// Operands
type Operand =
    | Int64Const of int64
    | BoolConst of bool
    | FloatSymbol of float
    | StringSymbol of string
    | Register of VReg
    | FuncAddr of string  // Address of a function (for higher-order functions)

/// Binary operations
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

/// Unary operations
type UnaryOp =
    | Neg
    | Not
    | BitNot  // Bitwise NOT: ~~~expr

/// Reference-count operation kind
type RcKind =
    | GenericHeap
    | TaggedList

/// Basic block label (defined early for use in Phi nodes)
type Label = Label of string

/// Instructions (non-control-flow)
type Instr =
    | Mov of dest:VReg * src:Operand * valueType:AST.Type option  // valueType for float/int distinction
    | BinOp of dest:VReg * op:BinOp * left:Operand * right:Operand * operandType:AST.Type
    | UnaryOp of dest:VReg * op:UnaryOp * src:Operand
    | Call of dest:VReg * funcName:string * args:Operand list * argTypes:AST.Type list * returnType:AST.Type  // Direct function call (BL instruction)
    | TailCall of funcName:string * args:Operand list * argTypes:AST.Type list * returnType:AST.Type  // Tail call (B instruction, no return)
    | IndirectCall of dest:VReg * func:Operand * args:Operand list * argTypes:AST.Type list * returnType:AST.Type  // Call through function pointer (BLR instruction)
    | IndirectTailCall of func:Operand * args:Operand list * argTypes:AST.Type list * returnType:AST.Type  // Indirect tail call (BR instruction)
    | ClosureAlloc of dest:VReg * funcName:string * captures:Operand list  // Allocate closure: (func_addr, caps...)
    | ClosureCall of dest:VReg * closure:Operand * args:Operand list * argTypes:AST.Type list * returnType:AST.Type  // Call through closure with hidden first arg
    | ClosureTailCall of closure:Operand * args:Operand list * argTypes:AST.Type list  // Tail call through closure (BR instruction)
    // Heap operations for tuples and other compound types
    | HeapAlloc of dest:VReg * sizeBytes:int       // Allocate heap memory
    | HeapStore of addr:VReg * offset:int * src:Operand * valueType:AST.Type option  // Store at heap[addr+offset], valueType for float/int
    | HeapLoad of dest:VReg * addr:VReg * offset:int * valueType:AST.Type option  // Load from heap[addr+offset]
    // String operations
    | StringConcat of dest:VReg * left:Operand * right:Operand  // Concatenate strings
    // Reference counting operations
    | RefCountInc of addr:VReg * payloadSize:int * kind:RcKind   // Increment ref count at [addr + payloadSize]
    | RefCountDec of addr:VReg * payloadSize:int * kind:RcKind   // Decrement ref count, free if zero
    // Output operations (for main expression result printing)
    | Print of src:Operand * valueType:AST.Type    // Print value with type-appropriate formatting
    | RuntimeError of message:string               // Print runtime error to stderr and exit with code 1
    // File I/O intrinsics (generate syscalls)
    | FileReadText of dest:VReg * path:Operand    // Read file, returns Result<String, String>
    | FileExists of dest:VReg * path:Operand      // Check if file exists, returns Bool
    | FileWriteText of dest:VReg * path:Operand * content:Operand   // Write file, returns Result<Unit, String>
    | FileAppendText of dest:VReg * path:Operand * content:Operand  // Append to file, returns Result<Unit, String>
    | FileDelete of dest:VReg * path:Operand      // Delete file, returns Result<Unit, String>
    | FileSetExecutable of dest:VReg * path:Operand  // Set executable bit, returns Result<Unit, String>
    | FileWriteFromPtr of dest:VReg * path:Operand * ptr:Operand * length:Operand  // Write raw bytes to file
    // Float intrinsics
    | FloatSqrt of dest:VReg * src:Operand        // Square root: sqrt(x)
    | FloatAbs of dest:VReg * src:Operand         // Absolute value: |x|
    | FloatNeg of dest:VReg * src:Operand         // Negate: -x
    | Int64ToFloat of dest:VReg * src:Operand     // Convert Int64 to Float64
    | FloatToInt64 of dest:VReg * src:Operand    // Convert Float64 to Int64 (truncate)
    | FloatToBits of dest:VReg * src:Operand     // Copy Float64 bits to UInt64
    // Raw memory intrinsics (internal, for HAMT implementation)
    | RawAlloc of dest:VReg * numBytes:Operand    // Allocate raw bytes (no header), returns RawPtr
    | RawFree of ptr:Operand                      // Manually free raw memory
    | RawGet of dest:VReg * ptr:Operand * byteOffset:Operand * valueType:AST.Type option  // Read 8 bytes at offset, valueType for float
    | RawGetByte of dest:VReg * ptr:Operand * byteOffset:Operand  // Read 1 byte at offset (zero-extended)
    | RawSet of ptr:Operand * byteOffset:Operand * value:Operand * valueType:AST.Type option  // Write 8 bytes at offset, valueType for float
    | RawSetByte of ptr:Operand * byteOffset:Operand * value:Operand  // Write 1 byte at offset
    // String reference counting (at dynamic offset)
    | RefCountIncString of str:Operand             // Increment string ref count (at [str + 8 + len])
    | RefCountDecString of str:Operand             // Decrement string ref count, free if zero
    // Random intrinsics
    | RandomInt64 of dest:VReg                     // Get 8 random bytes as Int64
    // Date intrinsics
    | DateNow of dest:VReg                         // Get current Unix epoch seconds as Int64
    // Float to String conversion
    | FloatToString of dest:VReg * value:Operand   // Convert Float to heap String
    // SSA phi node - merges values from different predecessor blocks
    // Phi nodes must appear at the beginning of a basic block, before other instructions
    // valueType distinguishes between integer (X registers) and float (D registers) phi nodes
    | Phi of dest:VReg * sources:(Operand * Label) list * valueType:AST.Type option
    // Coverage instrumentation - records that expression was executed
    | CoverageHit of exprId:int

/// Terminator instructions (control flow)
type Terminator =
    | Ret of Operand                                      // Return from function
    | Branch of cond:Operand * trueLabel:Label * falseLabel:Label  // Conditional branch
    | Jump of Label                                        // Unconditional jump

/// Basic block with label, instructions, and terminator
type BasicBlock = {
    Label: Label
    Instrs: Instr list
    Terminator: Terminator
}

/// Control Flow Graph
type CFG = {
    Entry: Label
    Blocks: Map<Label, BasicBlock>
}

/// MIR function with CFG
type Function = {
    Name: string
    TypedParams: TypedMIRParam list  // Parameters with types bundled
    ReturnType: AST.Type             // Return type (for distinguishing int vs float returns)
    CFG: CFG
    FloatRegs: Set<int>              // VReg IDs that hold float values (for SSA phi nodes)
}

/// Info about a single variant in a sum type (makes structure explicit)
type VariantInfo = {
    Name: string
    Tag: int
    Payload: AST.Type option
}

/// All variants for a sum type, with type parameters
type TypeVariants = {
    TypeParams: string list
    Variants: VariantInfo list
}

/// Maps type name -> variant information
type VariantRegistry = Map<string, TypeVariants>

/// Info about a single record field (makes structure explicit)
type RecordField = {
    Name: string
    Type: AST.Type
}

/// Maps type name -> list of fields
type RecordRegistry = Map<string, RecordField list>

/// MIR program (list of functions plus type/record registries)
type Program = Program of functions:Function list * variants:VariantRegistry * records:RecordRegistry

/// Fresh register generator
type RegGen = RegGen of int

/// Generate a fresh virtual register
let freshReg (RegGen n) : VReg * RegGen =
    (VReg n, RegGen (n + 1))

/// Fresh label generator
type LabelGen = LabelGen of int

/// Generate a fresh label with optional function prefix (for uniqueness across functions)
let freshLabelWithPrefix (prefix: string) (LabelGen n) : Label * LabelGen =
    (Label $"{prefix}_L{n}", LabelGen (n + 1))

/// Initial label generator
let initialLabelGen = LabelGen 0
