// ARM64.fs - ARM64 Instruction Types
//
// Defines ARM64 instruction and register types.
//
// ARM64 is a RISC architecture with fixed 32-bit instruction width.
// These types represent ARM64 assembly instructions that will be encoded
// to machine code by the ARM64_Encoding pass.
//
// Supported instructions:
// - MOVZ/MOVK: Load immediate values (16-bit chunks)
// - ADD/SUB: Arithmetic (immediate and register forms)
// - MUL/SDIV: Multiplication and signed division
// - MOV: Register-to-register move
// - STP/LDP: Store/Load pair (for stack frames)
// - STR/LDR: Store/Load register (for stack slots)
// - BL: Branch with link (function calls)
// - RET: Return from function
//
// Example instructions:
//   MOVZ X0, #42, LSL #0
//   ADD X1, X0, #5
//   RET

module ARM64

/// ARM64 general-purpose registers
type Reg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17  // X16/X17 are IP0/IP1 scratch registers
    | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27  // Callee-saved
    | X28  // Reserved for heap pointer
    | X29 | X30 | SP

/// ARM64 floating-point registers (D0-D31 for double precision)
/// D0-D7: Argument/result registers (caller-saved)
/// D8-D15: Callee-saved registers
/// D16-D31: Additional caller-saved registers
type FReg =
    | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15
    | D16  // Used as temp for FArgMoves cycle breaking
    | D17  // Used as temp for binary ops (right operand)
    | D18  // Used as temp for binary ops (left operand)
    | D19 | D20 | D21 | D22 | D23 | D24 | D25 | D26  // Used for float call arg temps
    | D27 | D28 | D29 | D30 | D31  // Additional SSA temp registers

/// Comparison conditions (for CSET)
type Condition =
    | EQ   // Equal (Z set)
    | NE   // Not equal (Z clear)
    | LT   // Less than (signed)
    | GT   // Greater than (signed)
    | LE   // Less than or equal (signed)
    | GE   // Greater than or equal (signed)

/// ARM64 instruction types
type Instr =
    | MOVZ of dest:Reg * imm:uint16 * shift:int  // Move with zero
    | MOVN of dest:Reg * imm:uint16 * shift:int  // Move with NOT (for negative constants)
    | MOVK of dest:Reg * imm:uint16 * shift:int  // Move with keep
    | ADD_imm of dest:Reg * src:Reg * imm:uint16
    | ADD_reg of dest:Reg * src1:Reg * src2:Reg
    | ADD_shifted of dest:Reg * src1:Reg * src2:Reg * shift:int  // ADD with shifted register: dest = src1 + (src2 << shift)
    | SUB_imm of dest:Reg * src:Reg * imm:uint16
    | SUB_imm12 of dest:Reg * src:Reg * imm:uint16  // SUB with shift=12, value = imm * 4096
    | SUB_reg of dest:Reg * src1:Reg * src2:Reg
    | SUB_shifted of dest:Reg * src1:Reg * src2:Reg * shift:int  // SUB with shifted register: dest = src1 - (src2 << shift)
    | SUBS_imm of dest:Reg * src:Reg * imm:uint16  // SUB and set flags (for fused SUB+CMP)
    | MUL of dest:Reg * src1:Reg * src2:Reg
    | SDIV of dest:Reg * src1:Reg * src2:Reg
    | UDIV of dest:Reg * src1:Reg * src2:Reg  // Unsigned division (for positive integers)
    | MSUB of dest:Reg * src1:Reg * src2:Reg * src3:Reg  // Multiply-subtract: dest = src3 - src1 * src2 (for modulo)
    | MADD of dest:Reg * src1:Reg * src2:Reg * src3:Reg  // Multiply-add: dest = src3 + src1 * src2
    | CMP_imm of src:Reg * imm:uint16  // Compare with immediate (sets condition flags)
    | CMP_reg of src1:Reg * src2:Reg  // Compare registers (sets condition flags)
    | CSET of dest:Reg * cond:Condition  // Set register to 1 if condition, 0 otherwise
    | AND_reg of dest:Reg * src1:Reg * src2:Reg  // Bitwise AND
    | AND_imm of dest:Reg * src:Reg * imm:uint64  // Bitwise AND with immediate (bitmask)
    | ORR_reg of dest:Reg * src1:Reg * src2:Reg  // Bitwise OR
    | EOR_reg of dest:Reg * src1:Reg * src2:Reg  // Bitwise XOR (exclusive or)
    | LSL_reg of dest:Reg * src:Reg * shift:Reg  // Logical shift left by register
    | LSR_reg of dest:Reg * src:Reg * shift:Reg  // Logical shift right by register
    | LSL_imm of dest:Reg * src:Reg * shift:int  // Logical shift left by immediate (0-63)
    | LSR_imm of dest:Reg * src:Reg * shift:int  // Logical shift right by immediate (0-63)
    | MVN of dest:Reg * src:Reg  // Bitwise NOT
    | MOV_reg of dest:Reg * src:Reg
    | STRB of src:Reg * addr:Reg * offset:int  // Store byte [addr + offset] = src (lower 8 bits)
    | LDRB of dest:Reg * baseAddr:Reg * index:Reg  // Load byte with register offset: dest = [baseAddr + index]
    | LDRB_imm of dest:Reg * baseAddr:Reg * offset:int  // Load byte with immediate offset: dest = [baseAddr + offset]
    | STRB_reg of src:Reg * addr:Reg  // Store byte: [addr] = src (lower 8 bits)
    // Stack operations (for function calls and stack frames)
    | STP of reg1:Reg * reg2:Reg * addr:Reg * offset:int16  // Store pair: [addr + offset] = reg1, [addr + offset + 8] = reg2
    | STP_pre of reg1:Reg * reg2:Reg * addr:Reg * offset:int16  // Store pair with pre-index: addr += offset, then store
    | LDP of reg1:Reg * reg2:Reg * addr:Reg * offset:int16  // Load pair: reg1 = [addr + offset], reg2 = [addr + offset + 8]
    | LDP_post of reg1:Reg * reg2:Reg * addr:Reg * offset:int16  // Load pair with post-index: load, then addr += offset
    | STR of src:Reg * addr:Reg * offset:int16  // Store register (unsigned offset): [addr + offset] = src (64-bit)
    | LDR of dest:Reg * addr:Reg * offset:int16  // Load register (unsigned offset): dest = [addr + offset] (64-bit)
    | STUR of src:Reg * addr:Reg * offset:int16  // Store register (signed offset, -256 to +255): [addr + offset] = src (64-bit)
    | LDUR of dest:Reg * addr:Reg * offset:int16  // Load register (signed offset, -256 to +255): dest = [addr + offset] (64-bit)
    | BL of label:string  // Branch with link: call function at label (sets X30/LR to return address)
    | BLR of reg:Reg  // Branch with link to register: call function at address in reg (indirect call)
    | BR of reg:Reg  // Branch to register without link: tail call to address in reg (no return)
    // Label-based branches (for compiler-generated code with CFG)
    | CBZ of reg:Reg * label:string  // Compare and branch if zero (label will be resolved)
    | CBNZ of reg:Reg * label:string  // Compare and branch if not zero
    | B_label of label:string  // Unconditional branch to label
    | B_cond_label of cond:Condition * label:string  // Conditional branch to label
    // Offset-based branches (for handcrafted runtime code with known offsets)
    | CBZ_offset of reg:Reg * offset:int  // CBZ with immediate offset
    | CBNZ_offset of reg:Reg * offset:int  // CBNZ with immediate offset
    | TBZ of reg:Reg * bit:int * offset:int  // Test bit and branch if zero
    | TBNZ of reg:Reg * bit:int * offset:int  // Test bit and branch if not zero
    | TBZ_label of reg:Reg * bit:int * label:string  // Test bit and branch if zero (label will be resolved)
    | TBNZ_label of reg:Reg * bit:int * label:string  // Test bit and branch if not zero (label will be resolved)
    | B of offset:int  // Unconditional branch with immediate offset
    | B_cond of cond:Condition * offset:int  // Conditional branch with immediate offset
    | NEG of dest:Reg * src:Reg  // Negate: dest = 0 - src
    | RET
    | SVC of imm:uint16  // Supervisor call (syscall)
    | Label of string  // Pseudo-instruction: marks a label position
    // PC-relative addressing for .rodata access
    | ADRP of dest:Reg * label:string  // Address page: dest = PC-relative page address of label
    | ADD_label of dest:Reg * src:Reg * label:string  // Add label offset: dest = src + page offset of label
    | ADR of dest:Reg * label:string  // PC-relative address: dest = address of label (±1MB range)
    // Floating-point instructions
    | LDR_fp of dest:FReg * addr:Reg * offset:int16  // Load double from [addr + offset]
    | STR_fp of src:FReg * addr:Reg * offset:int16   // Store double to [addr + offset]
    | STP_fp of freg1:FReg * freg2:FReg * addr:Reg * offset:int16  // Store FP pair: [addr + offset] = freg1, [addr + offset + 8] = freg2
    | LDP_fp of freg1:FReg * freg2:FReg * addr:Reg * offset:int16  // Load FP pair: freg1 = [addr + offset], freg2 = [addr + offset + 8]
    | FADD of dest:FReg * src1:FReg * src2:FReg      // FP add: dest = src1 + src2
    | FSUB of dest:FReg * src1:FReg * src2:FReg      // FP sub: dest = src1 - src2
    | FMUL of dest:FReg * src1:FReg * src2:FReg      // FP mul: dest = src1 * src2
    | FDIV of dest:FReg * src1:FReg * src2:FReg      // FP div: dest = src1 / src2
    | FNEG of dest:FReg * src:FReg                   // FP negate: dest = -src
    | FABS of dest:FReg * src:FReg                   // FP absolute value: dest = |src|
    | FSQRT of dest:FReg * src:FReg                  // FP square root: dest = sqrt(src)
    | FCMP of src1:FReg * src2:FReg                  // FP compare (sets condition flags)
    | FMOV_reg of dest:FReg * src:FReg               // FP move between registers
    | FMOV_to_gp of dest:Reg * src:FReg              // FP to GP register (bit-for-bit)
    | FMOV_from_gp of dest:FReg * src:Reg            // GP to FP register (bit-for-bit)
    | SCVTF of dest:FReg * src:Reg                   // Signed int to FP: dest = (double)src
    | FCVTZS of dest:Reg * src:FReg                  // FP to signed int (truncate): dest = (int64)src
    // Sign/zero extension instructions (for integer overflow truncation)
    | SXTB of dest:Reg * src:Reg                     // Sign-extend byte: dest = sign_extend(src[7:0])
    | SXTH of dest:Reg * src:Reg                     // Sign-extend halfword: dest = sign_extend(src[15:0])
    | SXTW of dest:Reg * src:Reg                     // Sign-extend word: dest = sign_extend(src[31:0])
    | UXTB of dest:Reg * src:Reg                     // Zero-extend byte: dest = zero_extend(src[7:0])
    | UXTH of dest:Reg * src:Reg                     // Zero-extend halfword: dest = zero_extend(src[15:0])
    | UXTW of dest:Reg * src:Reg                     // Zero-extend word: dest = zero_extend(src[31:0])

/// Machine code (32-bit instruction)
type MachineCode = uint32

/// ARM64-specific syscall invocation details (layered on top of Platform.SyscallNumbers).
/// Platform.fs intentionally has no ARM64 dependency, so these are defined here.
type SyscallConfig = {
    Numbers: Platform.SyscallNumbers
    SvcImmediate: uint16   // SVC instruction immediate value
    SyscallRegister: Reg   // Register to hold syscall number (X16 macOS, X8 Linux)
}

/// Build the ARM64-specific syscall config for the given OS.
let syscallConfigFor (os: Platform.OS) : SyscallConfig =
    match os with
    | Platform.MacOS ->
        { Numbers = Platform.syscallNumbersFor Platform.MacOS Platform.ARM64
          SvcImmediate = 0x80us
          SyscallRegister = X16 }
    | Platform.Linux ->
        { Numbers = Platform.syscallNumbersFor Platform.Linux Platform.ARM64
          SvcImmediate = 0us
          SyscallRegister = X8 }
