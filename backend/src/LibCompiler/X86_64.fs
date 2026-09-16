// X86_64.fs - x86-64 Instruction Types
//
// Defines x86-64 instruction and register types.
//
// x86-64 is a CISC architecture with variable-length instructions (1-15 bytes).
// These types represent x86-64 assembly instructions that will be encoded
// to machine code by the x86_64 encoding pass.
//
// Register conventions (System V AMD64 ABI - Linux):
// - RDI, RSI, RDX, RCX, R8, R9: Integer argument registers
// - XMM0-XMM7: Floating-point argument registers
// - RAX: Return value
// - RBX, RBP, R12-R15: Callee-saved
// - RSP: Stack pointer
//
// Syscall conventions (Linux):
// - RAX: Syscall number
// - RDI, RSI, RDX, R10, R8, R9: Syscall arguments
// - Invoked via SYSCALL instruction

module X86_64

/// x86-64 general-purpose registers (64-bit)
type Reg =
    | RAX | RBX | RCX | RDX
    | RSI | RDI | RBP | RSP
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15

/// x86-64 SSE/AVX floating-point registers (128-bit, used as 64-bit double)
type FReg =
    | XMM0  | XMM1  | XMM2  | XMM3
    | XMM4  | XMM5  | XMM6  | XMM7
    | XMM8  | XMM9  | XMM10 | XMM11
    | XMM12 | XMM13 | XMM14 | XMM15

/// Comparison conditions (for SETcc/Jcc)
type Condition =
    | EQ   // Equal (ZF=1)
    | NE   // Not equal (ZF=0)
    | LT   // Less than (signed: SF!=OF)
    | GT   // Greater than (signed: ZF=0 and SF=OF)
    | LE   // Less than or equal (signed: ZF=1 or SF!=OF)
    | GE   // Greater than or equal (signed: SF=OF)
    // Unsigned/float conditions (for use after UCOMISD):
    | B    // Below (CF=1) — float less than
    | A    // Above (CF=0 and ZF=0) — float greater than
    | BE   // Below or equal (CF=1 or ZF=1) — float less or equal
    | AE   // Above or equal (CF=0) — float greater or equal
    | P    // Parity set (PF=1) — unordered (NaN)
    | NP   // Parity not set (PF=0) — ordered (not NaN)

/// Operand size for instructions that need explicit sizing
type Size =
    | Byte   // 8-bit
    | Word   // 16-bit
    | DWord  // 32-bit
    | QWord  // 64-bit

/// x86-64 instruction types
type Instr =
    // Data movement
    | MOV_imm of dest:Reg * imm:int64                    // MOV reg, imm64 (movabs for 64-bit)
    | MOV_imm32 of dest:Reg * imm:int32                  // MOV reg, imm32 (sign-extended to 64-bit)
    | MOV_reg of dest:Reg * src:Reg                       // MOV reg, reg
    | MOV_load of dest:Reg * baseAddr:Reg * offset:int32  // MOV reg, [base + offset]
    | MOV_store of baseAddr:Reg * offset:int32 * src:Reg  // MOV [base + offset], reg
    | MOV_reg32 of dest:Reg * src:Reg                      // MOV r32, r32 (zero-extends to 64-bit)
    | MOVZX_byte of dest:Reg * src:Reg                    // MOVZX reg, reg8 (zero-extend byte)
    | MOVZX_word of dest:Reg * src:Reg                    // MOVZX reg, reg16 (zero-extend word)
    | MOVSX_byte of dest:Reg * src:Reg                    // MOVSX reg, reg8 (sign-extend byte)
    | MOVSX_word of dest:Reg * src:Reg                    // MOVSX reg, reg16 (sign-extend word)
    | MOVSXD of dest:Reg * src:Reg                        // MOVSXD reg64, reg32 (sign-extend dword)
    | LEA of dest:Reg * baseAddr:Reg * offset:int32       // LEA reg, [base + offset]
    | LEA_rip of dest:Reg * label:string                  // LEA reg, [RIP + label]
    // Stack operations
    | PUSH of Reg
    | POP of Reg
    // Arithmetic
    | ADD_imm of dest:Reg * imm:int32
    | ADD_reg of dest:Reg * src:Reg
    | SUB_imm of dest:Reg * imm:int32
    | SUB_reg of dest:Reg * src:Reg
    | IMUL_reg of dest:Reg * src:Reg                      // Signed multiply: dest = dest * src
    | IMUL_imm of dest:Reg * src:Reg * imm:int32          // Signed multiply: dest = src * imm
    | IDIV of src:Reg                                     // Signed divide: RDX:RAX / src → RAX=quot, RDX=rem
    | DIV of src:Reg                                      // Unsigned divide: RDX:RAX / src → RAX=quot, RDX=rem
    | NEG of dest:Reg                                     // Negate: dest = -dest
    | NOT of dest:Reg                                     // Bitwise NOT: dest = ~dest
    | CQO                                                 // Sign-extend RAX into RDX:RAX (before IDIV)
    | XOR_reg of dest:Reg * src:Reg                       // XOR for zeroing or bitwise xor
    // Comparison and conditional
    | CMP_imm of src:Reg * imm:int32
    | CMP_reg of src1:Reg * src2:Reg
    | TEST_reg of src1:Reg * src2:Reg                     // TEST reg, reg (AND without storing, sets flags)
    | SETcc of cond:Condition * dest:Reg                  // Set byte to 0/1 based on condition
    // Bitwise
    | AND_imm of dest:Reg * imm:int32
    | AND_reg of dest:Reg * src:Reg
    | OR_reg of dest:Reg * src:Reg
    | SHL_imm of dest:Reg * shift:int                     // Shift left by immediate
    | SHR_imm of dest:Reg * shift:int                     // Logical shift right by immediate
    | SHL_cl of dest:Reg                                  // Shift left by CL register
    | SHR_cl of dest:Reg                                  // Logical shift right by CL register
    // Byte-level memory
    | MOV_store_byte of baseAddr:Reg * offset:int32 * src:Reg  // MOV [base + offset], src8
    | MOV_load_byte of dest:Reg * baseAddr:Reg * offset:int32  // MOVZX dest, byte [base + offset]
    // Control flow
    | CALL of label:string                                // CALL rel32
    | CALL_reg of reg:Reg                                 // CALL reg (indirect)
    | JMP of label:string                                 // JMP rel32
    | JMP_reg of reg:Reg                                  // JMP reg (indirect, for tail calls)
    | Jcc of cond:Condition * label:string                // Conditional jump to label
    | RET
    | SYSCALL                                             // Linux syscall
    | Label of string                                     // Pseudo-instruction: marks a label position
    // Floating-point (SSE2)
    | MOVSD_load of dest:FReg * baseAddr:Reg * offset:int32   // Load double from [base + offset]
    | MOVSD_store of baseAddr:Reg * offset:int32 * src:FReg   // Store double to [base + offset]
    | MOVSD_reg of dest:FReg * src:FReg                       // Move between XMM registers
    | ADDSD of dest:FReg * src:FReg                           // Add double
    | SUBSD of dest:FReg * src:FReg                           // Subtract double
    | MULSD of dest:FReg * src:FReg                           // Multiply double
    | DIVSD of dest:FReg * src:FReg                           // Divide double
    | XORPD of dest:FReg * src:FReg                           // XOR packed double (for negation/zeroing)
    | SQRTSD of dest:FReg * src:FReg                          // Square root double
    | UCOMISD of src1:FReg * src2:FReg                        // Compare doubles (sets flags)
    | CVTSI2SD of dest:FReg * src:Reg                         // Convert int64 to double
    | CVTTSD2SI of dest:Reg * src:FReg                        // Convert double to int64 (truncate)
    | MOVQ_to_gp of dest:Reg * src:FReg                       // Move 64 bits from XMM to GP
    | MOVQ_from_gp of dest:FReg * src:Reg                     // Move 64 bits from GP to XMM

/// Machine code (variable-length byte sequence for one instruction)
type MachineCode = byte array
