// LIR.fs - Symbolic Low-level Intermediate Representation
//
// Defines the LIR (Low-level IR) data structures with symbolic literals.
// Strings and floats are stored by value so pools are resolved late in emission.

module LIR

/// ARM64 general-purpose registers
/// X16/X17 are IP0/IP1 scratch registers, X27/X28 are reserved for runtime state.
type PhysReg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17
    | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27
    | X29
    | X30
    | SP

/// ARM64 floating-point registers
/// D0-D7 are caller-saved, D8-D15 are callee-saved.
type PhysFPReg =
    | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15

/// Register or virtual register (before allocation)
type Reg =
    | Physical of PhysReg
    | Virtual of int

/// Floating-point register or virtual FP register (before allocation)
type FReg =
    | FPhysical of PhysFPReg
    | FVirtual of int

/// Parameter with register and type bundled (makes invalid states unrepresentable)
type TypedLIRParam = { Reg: Reg; Type: AST.Type }

/// Operands (symbolic string/float references)
type Operand =
    | Imm of int64
    | FloatImm of float
    | Reg of Reg
    | StackSlot of int
    | StringSymbol of string
    | FloatSymbol of float
    | FuncAddr of string

/// Comparison conditions (for CSET)
type Condition =
    | EQ
    | NE
    | LT
    | GT
    | LE
    | GE
    // Unsigned ordered comparisons (for UInt* operands). x64: setb/seta/setbe/setae.
    | ULT
    | UGT
    | ULE
    | UGE

/// Reference-count operation kind
type RcKind =
    | GenericHeap
    | TaggedList

/// Basic block label (wrapper type for type safety)
type Label = Label of string

/// Instructions (symbolic)
type Instr =
    | Mov of dest:Reg * src:Operand
    | Phi of dest:Reg * sources:(Operand * Label) list * valueType:AST.Type option
    | Store of stackSlot:int * src:Reg
    | Add of dest:Reg * left:Reg * right:Operand
    | Sub of dest:Reg * left:Reg * right:Operand
    | Mul of dest:Reg * left:Reg * right:Reg
    | Sdiv of dest:Reg * left:Reg * right:Reg
    | Udiv of dest:Reg * left:Reg * right:Reg
    | Msub of dest:Reg * mulLeft:Reg * mulRight:Reg * sub:Reg
    | Madd of dest:Reg * mulLeft:Reg * mulRight:Reg * add:Reg
    | Cmp of left:Reg * right:Operand
    | Cset of dest:Reg * cond:Condition
    | And of dest:Reg * left:Reg * right:Reg
    | And_imm of dest:Reg * src:Reg * imm:int64
    | Orr of dest:Reg * left:Reg * right:Reg
    | Eor of dest:Reg * left:Reg * right:Reg
    | Lsl of dest:Reg * src:Reg * shift:Reg
    | Lsr of dest:Reg * src:Reg * shift:Reg
    | Lsl_imm of dest:Reg * src:Reg * shift:int
    | Lsr_imm of dest:Reg * src:Reg * shift:int
    | Mvn of dest:Reg * src:Reg
    | Sxtb of dest:Reg * src:Reg
    | Sxth of dest:Reg * src:Reg
    | Sxtw of dest:Reg * src:Reg
    | Uxtb of dest:Reg * src:Reg
    | Uxth of dest:Reg * src:Reg
    | Uxtw of dest:Reg * src:Reg
    | Call of dest:Reg * funcName:string * args:Operand list
    | TailCall of funcName:string * args:Operand list
    | IndirectCall of dest:Reg * func:Reg * args:Operand list
    | IndirectTailCall of func:Reg * args:Operand list
    | ClosureAlloc of dest:Reg * funcName:string * captures:Operand list
    | ClosureCall of dest:Reg * closure:Reg * args:Operand list
    | ClosureTailCall of closure:Reg * args:Operand list
    | SaveRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list
    | RestoreRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list
    | ArgMoves of (PhysReg * Operand) list
    | TailArgMoves of (PhysReg * Operand) list
    | FArgMoves of (PhysFPReg * FReg) list
    | PrintInt64 of Reg
    | PrintBool of Reg
    | PrintInt64NoNewline of Reg
    | PrintBoolNoNewline of Reg
    | PrintFloat of FReg
    | PrintFloatNoNewline of FReg
    | PrintString of string
    | RuntimeError of string
    | PrintHeapStringNoNewline of Reg
    | PrintChars of byte list
    | PrintBytes of Reg
    | PrintList of listPtr:Reg * elemType:AST.Type
    | PrintSum of sumPtr:Reg * variants:(string * int * AST.Type option) list
    | PrintRecord of recordPtr:Reg * typeName:string * fields:(string * AST.Type) list
    | Exit
    | FPhi of dest:FReg * sources:(FReg * Label) list
    | FMov of dest:FReg * src:FReg
    | FLoad of dest:FReg * floatValue:float
    | FAdd of dest:FReg * left:FReg * right:FReg
    | FSub of dest:FReg * left:FReg * right:FReg
    | FMul of dest:FReg * left:FReg * right:FReg
    | FDiv of dest:FReg * left:FReg * right:FReg
    | FNeg of dest:FReg * src:FReg
    | FAbs of dest:FReg * src:FReg
    | FSqrt of dest:FReg * src:FReg
    | FCmp of left:FReg * right:FReg
    | Int64ToFloat of dest:FReg * src:Reg
    | FloatToInt64 of dest:Reg * src:FReg
    | FloatToBits of dest:Reg * src:FReg
    | GpToFp of dest:FReg * src:Reg
    | FpToGp of dest:Reg * src:FReg
    | HeapAlloc of dest:Reg * sizeBytes:int
    | HeapStore of addr:Reg * offset:int * src:Operand * valueType:AST.Type option
    | HeapLoad of dest:Reg * addr:Reg * offset:int
    | RefCountInc of addr:Reg * payloadSize:int * kind:RcKind
    | RefCountDec of addr:Reg * payloadSize:int * kind:RcKind
    | StringConcat of dest:Reg * left:Operand * right:Operand
    | PrintHeapString of Reg
    | LoadFuncAddr of dest:Reg * funcName:string
    | FileReadText of dest:Reg * path:Operand
    | FileExists of dest:Reg * path:Operand
    | FileWriteText of dest:Reg * path:Operand * content:Operand
    | FileAppendText of dest:Reg * path:Operand * content:Operand
    | FileDelete of dest:Reg * path:Operand
    | FileSetExecutable of dest:Reg * path:Operand
    | FileWriteFromPtr of dest:Reg * path:Operand * ptr:Reg * length:Reg
    | RawAlloc of dest:Reg * numBytes:Reg
    | RawFree of ptr:Reg
    | RawGet of dest:Reg * ptr:Reg * byteOffset:Reg
    | RawGetByte of dest:Reg * ptr:Reg * byteOffset:Reg
    | RawSet of ptr:Reg * byteOffset:Reg * value:Reg * valueType:AST.Type option
    | RawSetByte of ptr:Reg * byteOffset:Reg * value:Reg
    | RefCountIncString of str:Operand
    | RefCountDecString of str:Operand
    | RandomInt64 of dest:Reg
    | DateNow of dest:Reg
    | FloatToString of dest:Reg * value:FReg
    | CoverageHit of exprId:int

/// Terminators
type Terminator =
    | Ret
    | Branch of cond:Reg * trueLabel:Label * falseLabel:Label
    | BranchZero of cond:Reg * zeroLabel:Label * nonZeroLabel:Label
    | BranchBitZero of reg:Reg * bit:int * zeroLabel:Label * nonZeroLabel:Label
    | BranchBitNonZero of reg:Reg * bit:int * nonZeroLabel:Label * zeroLabel:Label
    | CondBranch of cond:Condition * trueLabel:Label * falseLabel:Label
    | Jump of Label

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

/// Function with CFG
type Function = {
    Name: string
    TypedParams: TypedLIRParam list
    CFG: CFG
    StackSize: int
    UsedCalleeSaved: PhysReg list
}

/// LIR program (symbolic literals, no pools)
type Program = Program of functions:Function list

/// Count the number of CoverageHit instructions in a program
let countCoverageHits (Program functions) : int =
    functions
    |> List.collect (fun f ->
        f.CFG.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) -> block.Instrs))
    |> List.filter (function CoverageHit _ -> true | _ -> false)
    |> List.length
