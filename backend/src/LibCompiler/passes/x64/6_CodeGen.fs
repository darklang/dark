// 6_CodeGen.fs - Code Generation (Pass 6, x64 backend)
//
// Transforms LIR into x86-64 instructions.
//
// Maps LIR physical registers to x86-64 registers:
//   X0→RAX, X1→RDI, X2→RSI, X3→RCX, X4→R8, X5→R9,
//   X6→R10, X7→RDX, X8-X17→R11 (shared scratch),
//   X19→RBX, X20→R12, X21→R13, X22→R14 (heap), X23→R15 (freelist).
//
// Key traits of the x86-64 instruction set:
//   - CISC: most instructions modify destination in-place (dest = dest OP src),
//     so three-operand LIR ops need MOV + OP sequences or operand swaps.
//   - Integer division uses the fixed (RDX:RAX) register pair.
//   - SYSCALL with syscall number in RAX.
//   - No link register; CALL pushes return address on the stack.
//   - 6 integer argument registers (System V AMD64 ABI).

module CodeGen_X86_64

let private syscalls = Platform.linuxX86_64SyscallNumbers

/// Map LIR.PhysReg to x86-64 register
let lirRegToX86 (reg: LIR.PhysReg) : X86_64.Reg =
    match reg with
    | LIR.X0  -> X86_64.RAX   // Return value
    | LIR.X1  -> X86_64.RDI   // Arg 1
    | LIR.X2  -> X86_64.RSI   // Arg 2
    | LIR.X3  -> X86_64.RCX   // Arg 3 (NOT RDX — RDX is reserved for IDIV)
    | LIR.X4  -> X86_64.R8    // Arg 4
    | LIR.X5  -> X86_64.R9    // Arg 5
    | LIR.X6  -> X86_64.R10   // Arg 6 / caller-saved
    | LIR.X7  -> X86_64.RDX   // Caller-saved (only used when IDIV isn't active)
    | LIR.X8  -> X86_64.R11   // Scratch
    | LIR.X9  -> X86_64.R11   // Scratch (shared)
    | LIR.X10 -> X86_64.R11
    | LIR.X11 -> X86_64.R11
    | LIR.X12 -> X86_64.R11
    | LIR.X13 -> X86_64.R11
    | LIR.X14 -> X86_64.R11
    | LIR.X15 -> X86_64.R11
    | LIR.X16 -> X86_64.R11
    | LIR.X17 -> X86_64.R11
    | LIR.X19 -> X86_64.RBX   // Callee-saved 1
    | LIR.X20 -> X86_64.R12   // Callee-saved 2
    | LIR.X21 -> X86_64.R13   // Callee-saved 3
    | LIR.X22 -> X86_64.R14   // Reserved: heap bump pointer
    | LIR.X23 -> X86_64.R15   // Reserved: free list base
    | LIR.X24 -> X86_64.R15   // Overflow (shouldn't be allocated on x86_64)
    | LIR.X25 -> X86_64.R15
    | LIR.X26 -> X86_64.R15
    | LIR.X27 -> X86_64.RBP   // Reserved (free list / heap)
    | LIR.X29 -> X86_64.RBP   // Frame pointer
    | LIR.X30 -> X86_64.RAX   // Link register (not applicable on x86_64)
    | LIR.SP  -> X86_64.RSP

/// Map LIR.FReg to x86-64 XMM register
let lirFRegToX86 (freg: LIR.PhysFPReg) : X86_64.FReg =
    match freg with
    | LIR.D0  -> X86_64.XMM0  | LIR.D1  -> X86_64.XMM1
    | LIR.D2  -> X86_64.XMM2  | LIR.D3  -> X86_64.XMM3
    | LIR.D4  -> X86_64.XMM4  | LIR.D5  -> X86_64.XMM5
    | LIR.D6  -> X86_64.XMM6  | LIR.D7  -> X86_64.XMM7
    | LIR.D8  -> X86_64.XMM8  | LIR.D9  -> X86_64.XMM9
    | LIR.D10 -> X86_64.XMM10 | LIR.D11 -> X86_64.XMM11
    | LIR.D12 -> X86_64.XMM12 | LIR.D13 -> X86_64.XMM13
    | LIR.D14 -> X86_64.XMM14 | LIR.D15 -> X86_64.XMM15

/// Resolve a LIR.FReg to x86-64 XMM register.
/// FVirtual 2000 is used as a temp for parallel float move resolution.
let private resolveFreg (freg: LIR.FReg) : Result<X86_64.FReg, string> =
    match freg with
    | LIR.FPhysical fp -> Ok (lirFRegToX86 fp)
    | LIR.FVirtual 2000 -> Ok X86_64.XMM15
    | LIR.FVirtual id -> Error $"Unresolved virtual float register f{id} in x86-64 codegen"

/// Resolve a LIR.Reg (Physical or Virtual) to x86-64 register.
let resolveReg (reg: LIR.Reg) : Result<X86_64.Reg, string> =
    match reg with
    | LIR.Physical phys -> Ok (lirRegToX86 phys)
    | LIR.Virtual id -> Error $"Unresolved virtual register v{id} in x86-64 codegen"

/// Load a 64-bit immediate into a register.
let private loadImm64 (dest: X86_64.Reg) (value: int64) : X86_64.Instr list =
    if value = 0L then
        [X86_64.XOR_reg (dest, dest)]
    elif value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue then
        [X86_64.MOV_imm32 (dest, int32 value)]
    else
        [X86_64.MOV_imm (dest, value)]

/// Scratch register for temporaries in codegen
let private scratch = X86_64.R11

/// Heap bump pointer register (codegen-internal, reserved; not allocatable).
let private heapPtr = X86_64.R14

/// Free list base register (codegen-internal, reserved; not allocatable).
let private freeListBase = X86_64.R15

/// Size of free list heads area (32 size classes × 8 bytes = 256 bytes)
let private freeListSize = 256

/// Max payload size class for free list reuse (freeListSize - 8)
let private maxFreeListPayload = freeListSize - 8

/// Emit inline 8-byte-at-a-time copy of a UTF-8 byte array to heap memory.
/// Stores bytes starting at [destReg + 8] (after the 8-byte length prefix).
let private emitStringByteCopy (destReg: X86_64.Reg) (strBytes: byte array) : X86_64.Instr list =
    let len = strBytes.Length
    if len = 0 then []
    else
        let chunks = (len + 7) / 8
        [0 .. chunks - 1]
        |> List.collect (fun i ->
            let offset = 8 + i * 8
            let chunkLen = min 8 (len - i * 8)
            let value =
                [0 .. chunkLen - 1]
                |> List.fold (fun acc j ->
                    let byteIdx = i * 8 + j
                    if byteIdx < strBytes.Length then
                        acc ||| (int64 strBytes.[byteIdx] <<< (j * 8))
                    else acc) 0L
            loadImm64 scratch value
            @ [X86_64.MOV_store (destReg, int32 offset, scratch)])

/// Allocate a heap string from a literal value: [length:8][data:N][padding][refcount:8].
/// Bump-allocates from heapPtr, stores length, copies bytes, sets refcount=1.
/// Returns instructions that leave destReg pointing to the new string.
let private emitStringLiteral (destReg: X86_64.Reg) (value: string) : X86_64.Instr list =
    let strBytes = System.Text.Encoding.UTF8.GetBytes(value)
    let len = strBytes.Length
    let totalSize = ((len + 16) + 7) &&& (~~~7)
    let alloc = [X86_64.MOV_reg (destReg, heapPtr); X86_64.ADD_imm (heapPtr, int32 totalSize)]
    let storeLen = loadImm64 scratch (int64 len) @ [X86_64.MOV_store (destReg, 0, scratch)]
    let copyBytes = emitStringByteCopy destReg strBytes
    let rcOffset = 8 + ((len + 7) &&& (~~~7))
    let storeRefCount = loadImm64 scratch 1L @ [X86_64.MOV_store (destReg, int32 rcOffset, scratch)]
    alloc @ storeLen @ copyBytes @ storeRefCount

/// Allocate a heap string without refcount (for file-op path buffers).
/// Layout: [length:8][data:N]. Returns instructions with destReg = string ptr.
let private emitStringLiteralNoRefCount (destReg: X86_64.Reg) (value: string) : X86_64.Instr list =
    let strBytes = System.Text.Encoding.UTF8.GetBytes(value)
    let len = strBytes.Length
    let totalSize = ((len + 16) + 7) &&& (~~~7)
    let alloc = [X86_64.MOV_reg (destReg, heapPtr); X86_64.ADD_imm (heapPtr, int32 totalSize)]
    let storeLen = loadImm64 scratch (int64 len) @ [X86_64.MOV_store (destReg, 0, scratch)]
    let copyBytes = emitStringByteCopy destReg strBytes
    alloc @ storeLen @ copyBytes

/// Heap size for mmap (512 MB)
let private heapMmapSizeBytes = 512L * 1024L * 1024L

/// Generate x86-64 write(fd, buf, len) syscall
let private genWriteSyscall : X86_64.Instr list =
    loadImm64 X86_64.RAX (int64 syscalls.Write) @ [X86_64.SYSCALL]

/// Generate x86-64 exit(code) syscall.
/// Exit code must already be in RDI.
let private genExitSyscall : X86_64.Instr list =
    loadImm64 X86_64.RAX (int64 syscalls.Exit) @ [X86_64.SYSCALL]

/// Label for shared OOM handler (set per-program, not per-function)
let mutable private oomHandlerLabel = "__heap_oom"

/// Generate a jump to the shared OOM handler
let private genOomJump () : X86_64.Instr list =
    [X86_64.JMP oomHandlerLabel]

/// Generate the shared OOM handler code (placed once at end of program)
let private genOomHandler () : X86_64.Instr list =
    let msg = "Out of heap memory\n"
    let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
    let len = bytes.Length
    let padded = ((len + 7) / 8) * 8
    let paddedBytes = (bytes |> Array.toList) @ List.replicate (padded - len) 0uy
    let pushInstrs =
        paddedBytes
        |> List.chunkBySize 8
        |> List.rev
        |> List.collect (fun chunk ->
            let value = chunk |> List.mapi (fun i b -> int64 b <<< (i * 8)) |> List.fold (|||) 0L
            loadImm64 scratch value @ [X86_64.PUSH scratch])
    [X86_64.Label oomHandlerLabel]
    @ pushInstrs
    @ [X86_64.MOV_imm32 (X86_64.RDI, 2)]  // fd = stderr
    @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]
    @ loadImm64 X86_64.RDX (int64 len)
    @ genWriteSyscall
    @ [X86_64.ADD_imm (X86_64.RSP, int32 padded)]
    @ loadImm64 X86_64.RDI 1L
    @ genExitSyscall

/// Mutable counter for generating unique labels within a compilation
let mutable private labelCounter = 0
let private freshLabel (prefix: string) : string =
    labelCounter <- labelCounter + 1
    $"__{prefix}_{labelCounter}"

/// Generate x86-64 instructions to print a signed 64-bit integer to stdout.
/// Value is in the given register. Includes newline. Does NOT exit.
///
/// Algorithm: itoa by repeated division by 10, writing digits backwards
/// into a stack buffer, then write(1, buf, len).
let private genPrintInt64 (srcReg: X86_64.Reg) (addNewline: bool) : X86_64.Instr list =
    let loopLabel = freshLabel "itoa_loop"
    let doneLabel = freshLabel "itoa_done"
    let zeroLabel = freshLabel "itoa_zero"
    let negLabel = freshLabel "itoa_neg"
    let writeLabel = freshLabel "itoa_write"
    let skipMinusLabel = freshLabel "itoa_skipminus"

    [
        // Save value, allocate 32-byte buffer on stack
        X86_64.SUB_imm (X86_64.RSP, 32)
        // RCX = write pointer (end of buffer, work backwards)
        X86_64.LEA (X86_64.RCX, X86_64.RSP, 31)
    ]
    @ (if addNewline then [
        // Store newline at end
        X86_64.MOV_imm32 (scratch, 10)
        X86_64.MOV_store_byte (X86_64.RCX, 0, scratch)
        X86_64.SUB_imm (X86_64.RCX, 1)
    ] else [])
    @ [
        // R8 = value to print; R9 = negative flag
        X86_64.MOV_reg (X86_64.R8, srcReg)
        X86_64.XOR_reg (X86_64.R9, X86_64.R9)  // R9 = 0 (positive)

        // Check if negative
        X86_64.TEST_reg (X86_64.R8, X86_64.R8)
        X86_64.Jcc (X86_64.LT, negLabel)

        // Check if zero
        X86_64.TEST_reg (X86_64.R8, X86_64.R8)
        X86_64.Jcc (X86_64.EQ, zeroLabel)

        // convert_loop: extract digits by dividing by 10
        X86_64.Label loopLabel
        X86_64.MOV_reg (X86_64.RAX, X86_64.R8)  // RAX = value
        X86_64.XOR_reg (X86_64.RDX, X86_64.RDX) // Clear RDX for unsigned div
        // But we made it positive, so use unsigned division
        X86_64.MOV_imm32 (X86_64.RSI, 10)
        X86_64.DIV X86_64.RSI  // RAX = quotient, RDX = remainder
        X86_64.ADD_imm (X86_64.RDX, 48)  // Convert remainder to ASCII
        X86_64.MOV_store_byte (X86_64.RCX, 0, X86_64.RDX)  // Store digit
        X86_64.SUB_imm (X86_64.RCX, 1)  // Move pointer back
        X86_64.MOV_reg (X86_64.R8, X86_64.RAX)  // value = quotient
        X86_64.TEST_reg (X86_64.R8, X86_64.R8)
        X86_64.Jcc (X86_64.NE, loopLabel)  // Loop if not zero

        // store_minus_if_needed
        X86_64.TEST_reg (X86_64.R9, X86_64.R9)
        X86_64.Jcc (X86_64.EQ, skipMinusLabel)
        X86_64.MOV_imm32 (scratch, 45)  // '-' = 45
        X86_64.MOV_store_byte (X86_64.RCX, 0, scratch)
        X86_64.SUB_imm (X86_64.RCX, 1)
        X86_64.Label skipMinusLabel

        // write_output
        X86_64.JMP writeLabel

        // print_zero: special case
        X86_64.Label zeroLabel
        X86_64.MOV_imm32 (scratch, 48)  // '0' = 48
        X86_64.MOV_store_byte (X86_64.RCX, 0, scratch)
        X86_64.SUB_imm (X86_64.RCX, 1)
        X86_64.JMP writeLabel

        // handle_negative: negate and set flag
        X86_64.Label negLabel
        X86_64.NEG X86_64.R8
        X86_64.MOV_imm32 (X86_64.R9, 1)  // negative flag
        X86_64.TEST_reg (X86_64.R8, X86_64.R8)
        X86_64.Jcc (X86_64.EQ, zeroLabel)
        X86_64.JMP loopLabel

        // write_output
        X86_64.Label writeLabel
        X86_64.ADD_imm (X86_64.RCX, 1)  // RCX was one past first char
        // length = (RSP + 32) - RCX
        X86_64.LEA (X86_64.RDX, X86_64.RSP, 32)
        X86_64.SUB_reg (X86_64.RDX, X86_64.RCX)  // RDX = length
        X86_64.MOV_imm32 (X86_64.RDI, 1)  // fd = stdout
        X86_64.MOV_reg (X86_64.RSI, X86_64.RCX)  // buf
    ]
    @ genWriteSyscall
    @ [
        X86_64.ADD_imm (X86_64.RSP, 32)  // Deallocate buffer
        X86_64.Label doneLabel
    ]

/// Generate PrintInt64 + exit(0)
let private genPrintInt64AndExit (srcReg: X86_64.Reg) : X86_64.Instr list =
    genPrintInt64 srcReg true
    @ loadImm64 X86_64.RDI 0L
    @ genExitSyscall

/// Generate heap initialization via mmap (only for _start).
let private genHeapInit () : X86_64.Instr list =
    let failLabel = freshLabel "mmap_fail"
    let okLabel = freshLabel "mmap_ok"
    // mmap(NULL, 512MB, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
    // x86_64 Linux: rax=9, rdi=addr, rsi=length, rdx=prot, r10=flags, r8=fd, r9=offset
    loadImm64 X86_64.RDI 0L
    @ loadImm64 X86_64.RSI heapMmapSizeBytes
    @ loadImm64 X86_64.RDX 3L                         // PROT_READ | PROT_WRITE
    @ loadImm64 X86_64.R10 0x22L                       // MAP_PRIVATE | MAP_ANONYMOUS
    @ [X86_64.MOV_imm32 (X86_64.R8, -1)]              // fd = -1
    @ loadImm64 X86_64.R9 0L                           // offset = 0
    @ loadImm64 X86_64.RAX (int64 syscalls.Mmap)
    @ [X86_64.SYSCALL
       X86_64.CMP_imm (X86_64.RAX, -1)
       X86_64.Jcc (X86_64.NE, okLabel)
       X86_64.Label failLabel]
    @ loadImm64 X86_64.RDI 1L
    @ genExitSyscall
    @ [X86_64.Label okLabel
       X86_64.MOV_reg (freeListBase, X86_64.RAX)
       X86_64.LEA (heapPtr, freeListBase, int32 freeListSize)]

/// Generate PrintBool + exit(0)
let private genPrintBoolAndExit (srcReg: X86_64.Reg) : X86_64.Instr list =
    let trueLabel = freshLabel "bool_true"
    let writeLabel = freshLabel "bool_write"
    [
        X86_64.TEST_reg (srcReg, srcReg)
        X86_64.Jcc (X86_64.NE, trueLabel)
        // false\n = 6 bytes
        X86_64.SUB_imm (X86_64.RSP, 8)
    ]
    @ loadImm64 scratch 0x0A65736C6166L  // "false\n" in little-endian (6 bytes)
    @ [
        X86_64.MOV_store (X86_64.RSP, 0, scratch)
        X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
        X86_64.MOV_imm32 (X86_64.RDX, 6)
        X86_64.JMP writeLabel

        X86_64.Label trueLabel
        X86_64.SUB_imm (X86_64.RSP, 8)
    ]
    @ loadImm64 scratch 0x0A65757274L  // "true\n" in little-endian (5 bytes)
    @ [
        X86_64.MOV_store (X86_64.RSP, 0, scratch)
        X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
        X86_64.MOV_imm32 (X86_64.RDX, 5)

        X86_64.Label writeLabel
        X86_64.MOV_imm32 (X86_64.RDI, 1)  // fd = stdout
    ]
    @ genWriteSyscall
    @ [X86_64.ADD_imm (X86_64.RSP, 8)]
    @ loadImm64 X86_64.RDI 0L
    @ genExitSyscall

// ============================================================================
// LIR Instruction Translation
// ============================================================================

let private alignedStackSize (stackSlots: int) (numCalleeSaved: int) : int =
    let returnAddr = 8
    let pushes = numCalleeSaved * 8
    let total = returnAddr + pushes + stackSlots  // StackSize is already in bytes from regalloc
    let aligned = ((total + 15) / 16) * 16
    aligned - returnAddr - pushes

let private genPrologue (stackSize: int) (usedCalleeSaved: LIR.PhysReg list) : X86_64.Instr list =
    // Push RBP and set up frame pointer for stack slot access.
    // Stack slots use [RBP - offset] which is stable across SaveRegs PUSHes.
    let setupFP = [X86_64.PUSH X86_64.RBP; X86_64.MOV_reg (X86_64.RBP, X86_64.RSP)]
    let saves = usedCalleeSaved |> List.map (fun reg -> X86_64.PUSH (lirRegToX86 reg))
    let alignedSize = alignedStackSize stackSize (List.length usedCalleeSaved + 1)  // +1 for RBP push
    let stackAlloc =
        if alignedSize > 0 then [X86_64.SUB_imm (X86_64.RSP, int32 alignedSize)]
        else []
    setupFP @ saves @ stackAlloc

let private genEpilogue (stackSize: int) (usedCalleeSaved: LIR.PhysReg list) : X86_64.Instr list =
    let alignedSize = alignedStackSize stackSize (List.length usedCalleeSaved + 1)
    let stackDealloc =
        if alignedSize > 0 then [X86_64.ADD_imm (X86_64.RSP, int32 alignedSize)]
        else []
    let restores = usedCalleeSaved |> List.rev |> List.map (fun reg -> X86_64.POP (lirRegToX86 reg))
    let restoreFP = [X86_64.POP X86_64.RBP]
    stackDealloc @ restores @ restoreFP

/// Function context for instructions that need stack frame info (TailCall, etc.)
type private FuncCtx = {
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
    EnableLeakCheck: bool
}

// ============================================================================
// Leak Counter (data label _leak_count in ELF data section)
// ============================================================================

/// Increment the leak counter (called on every heap allocation)
let private genLeakCounterInc (ctx: FuncCtx) : X86_64.Instr list =
    if not ctx.EnableLeakCheck then []
    else
        // LEA R11, [RIP + _leak_count]; INC [R11]
        [X86_64.PUSH scratch
         X86_64.PUSH X86_64.RCX
         X86_64.LEA_rip (scratch, "_leak_count")
         X86_64.MOV_load (X86_64.RCX, scratch, 0)
         X86_64.ADD_imm (X86_64.RCX, 1)
         X86_64.MOV_store (scratch, 0, X86_64.RCX)
         X86_64.POP X86_64.RCX
         X86_64.POP scratch]

/// Decrement the leak counter (called when refcount hits zero and block is freed)
let private genLeakCounterDec (ctx: FuncCtx) : X86_64.Instr list =
    if not ctx.EnableLeakCheck then []
    else
        [X86_64.PUSH scratch
         X86_64.PUSH X86_64.RCX
         X86_64.LEA_rip (scratch, "_leak_count")
         X86_64.MOV_load (X86_64.RCX, scratch, 0)
         X86_64.SUB_imm (X86_64.RCX, 1)
         X86_64.MOV_store (scratch, 0, X86_64.RCX)
         X86_64.POP X86_64.RCX
         X86_64.POP scratch]

/// Reverse bytes at [RSP..RSP+RCX-1] in place
let private genReverseBytes () : X86_64.Instr list =
    let loopLabel = freshLabel "rev_loop"
    let doneLabel = freshLabel "rev_done"
    [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)
     X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
     X86_64.ADD_reg (X86_64.RSI, X86_64.RCX)
     X86_64.SUB_imm (X86_64.RSI, 1)
     X86_64.Label loopLabel
     X86_64.CMP_reg (X86_64.RDI, X86_64.RSI)
     X86_64.Jcc (X86_64.GE, doneLabel)
     X86_64.MOV_load_byte (X86_64.RAX, X86_64.RDI, 0)
     X86_64.MOV_load_byte (X86_64.RDX, X86_64.RSI, 0)
     X86_64.MOV_store_byte (X86_64.RDI, 0, X86_64.RDX)
     X86_64.MOV_store_byte (X86_64.RSI, 0, X86_64.RAX)
     X86_64.ADD_imm (X86_64.RDI, 1)
     X86_64.SUB_imm (X86_64.RSI, 1)
     X86_64.JMP loopLabel
     X86_64.Label doneLabel]

/// Print an int64 value to stderr followed by newline
let private genPrintInt64ToStderr (srcReg: X86_64.Reg) : X86_64.Instr list =
    let loopLabel = freshLabel "print_i64_stderr_loop"
    let writeLabel = freshLabel "print_i64_stderr_write"
    [X86_64.SUB_imm (X86_64.RSP, 24)
     X86_64.MOV_imm32 (X86_64.RCX, 0)
     X86_64.TEST_reg (srcReg, srcReg)
     X86_64.Jcc (X86_64.NE, loopLabel)
     X86_64.MOV_imm32 (scratch, 48)
     X86_64.MOV_store_byte (X86_64.RSP, 0, scratch)
     X86_64.MOV_imm32 (X86_64.RCX, 1)
     X86_64.JMP writeLabel
     X86_64.Label loopLabel
     X86_64.TEST_reg (srcReg, srcReg)
     X86_64.Jcc (X86_64.EQ, writeLabel)]
    @ loadImm64 scratch 10L
    @ [X86_64.PUSH X86_64.RDX
       X86_64.MOV_reg (X86_64.RAX, srcReg)
       X86_64.XOR_reg (X86_64.RDX, X86_64.RDX)
       X86_64.IDIV scratch
       X86_64.ADD_imm (X86_64.RDX, 48)
       X86_64.MOV_reg (scratch, X86_64.RSP)
       X86_64.ADD_imm (scratch, 8)
       X86_64.ADD_reg (scratch, X86_64.RCX)
       X86_64.MOV_store_byte (scratch, 0, X86_64.RDX)
       X86_64.ADD_imm (X86_64.RCX, 1)
       X86_64.MOV_reg (srcReg, X86_64.RAX)
       X86_64.POP X86_64.RDX
       X86_64.JMP loopLabel
       X86_64.Label writeLabel]
    @ genReverseBytes ()
    @ [X86_64.MOV_imm32 (X86_64.RDI, 2)
       X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
       X86_64.MOV_reg (scratch, X86_64.RSP)
       X86_64.ADD_reg (scratch, X86_64.RCX)
       X86_64.MOV_imm32 (X86_64.RAX, 10)
       X86_64.MOV_store_byte (scratch, 0, X86_64.RAX)
       X86_64.LEA (X86_64.RDX, X86_64.RCX, 1)]
    @ loadImm64 X86_64.RAX (int64 syscalls.Write)
    @ [X86_64.SYSCALL
       X86_64.ADD_imm (X86_64.RSP, 24)]

/// Generate leak check report at exit: if _leak_count > 0, print "leaks: N\n" to stderr
let private genLeakCheckReport () : X86_64.Instr list =
    let noLeaksLabel = freshLabel "no_leaks"
    [X86_64.LEA_rip (scratch, "_leak_count")
     X86_64.MOV_load (X86_64.RAX, scratch, 0)
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, noLeaksLabel)
     X86_64.PUSH X86_64.RAX
     X86_64.SUB_imm (X86_64.RSP, 8)]
    @ loadImm64 scratch 0x203A736B61656CL   // "leaks: " little-endian
    @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
       X86_64.MOV_imm32 (X86_64.RDI, 2)
       X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
       X86_64.MOV_imm32 (X86_64.RDX, 7)]
    @ loadImm64 X86_64.RAX (int64 syscalls.Write)
    @ [X86_64.SYSCALL
       X86_64.ADD_imm (X86_64.RSP, 8)
       X86_64.POP X86_64.RAX]
    @ genPrintInt64ToStderr X86_64.RAX
    @ [X86_64.Label noLeaksLabel]

// ============================================================================
// Reference Counting Helpers
// ============================================================================

/// Generic RefCountDec: decrement refcount at [addr + payloadSize].
/// If zero, free block to free list and optionally decrement leak counter.
/// Uses PUSH/POP to preserve both RCX and RDX as temps, keeping addr safe.
let private genRefCountDecGeneric (ctx: FuncCtx) (addrReg: X86_64.Reg) (payloadSize: int) : X86_64.Instr list =
    let skipLabel = freshLabel "rc_dec_skip"
    let noFreeLabel = freshLabel "rc_dec_nofree"
    let leakDec = genLeakCounterDec ctx
    // Use RDX as temp for addr (saved/restored), RCX as temp for refcount
    // This avoids conflicts when addrReg is RCX or R11
    [X86_64.TEST_reg (addrReg, addrReg)
     X86_64.Jcc (X86_64.EQ, skipLabel)
     X86_64.PUSH X86_64.RDX
     X86_64.PUSH X86_64.RCX
     X86_64.MOV_reg (X86_64.RDX, addrReg)            // RDX = addr (safe copy)
     // Load refcount, decrement, store back
     X86_64.MOV_load (X86_64.RCX, X86_64.RDX, payloadSize)
     X86_64.SUB_imm (X86_64.RCX, 1)
     X86_64.MOV_store (X86_64.RDX, payloadSize, X86_64.RCX)
     X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
     X86_64.Jcc (X86_64.NE, noFreeLabel)]
    // Refcount hit zero — free to free list (only for valid payload sizes)
    @ (if payloadSize >= 0 && payloadSize < freeListSize then
        [X86_64.MOV_load (X86_64.RCX, freeListBase, payloadSize)
         X86_64.MOV_store (X86_64.RDX, 0, X86_64.RCX)
         X86_64.MOV_store (freeListBase, payloadSize, X86_64.RDX)]
       else [])
    @ leakDec
    @ [X86_64.Label noFreeLabel
       X86_64.POP X86_64.RCX
       X86_64.POP X86_64.RDX
       X86_64.Label skipLabel]

/// Generic RefCountInc: increment refcount at [addr + payloadSize].
let private genRefCountIncGeneric (addrReg: X86_64.Reg) (payloadSize: int) : X86_64.Instr list =
    let skipLabel = freshLabel "rc_inc_skip"
    [X86_64.TEST_reg (addrReg, addrReg)
     X86_64.Jcc (X86_64.EQ, skipLabel)
     X86_64.PUSH X86_64.RDX
     X86_64.MOV_reg (X86_64.RDX, addrReg)
     X86_64.MOV_load (X86_64.RDX, X86_64.RDX, payloadSize)
     X86_64.ADD_imm (X86_64.RDX, 1)
     X86_64.MOV_store (addrReg, payloadSize, X86_64.RDX)
     X86_64.POP X86_64.RDX
     X86_64.Label skipLabel]

// ============================================================================
// TaggedList RefCountDec Helper (FingerTree recursive DFS)
// ============================================================================

/// Label for the shared list refcount dec helper function
let private listRefCountDecHelperLabel = "__dark_list_rc_dec_helper"

/// Generate the TaggedList RefCountDec helper function.
/// Called via CALL with the tagged list pointer in RAX.
/// Uses iterative DFS with the machine stack as a work stack.
/// Clobbers all caller-saved registers. Caller must save/restore.
///
/// Register usage inside the helper:
///   RAX = current node (tagged pointer, iterative work item)
///   RCX = pending work count (number of items pushed on stack)
///   RDX = tag bits
///   RDI = untagged node address
///   RSI = payload size
///   R8  = refcount address, then child pointer for addChild
///   R9  = refcount value, old free list head, bounds check temp
///   R10 = prefix/suffix count
///   R14 = heap pointer (upper bound, read-only)
///   R15 = free list base (read-only)
let private generateListRefCountDecHelper (enableLeakCheck: bool) : X86_64.Instr list =
    let label name = $"__dark_list_rc_dec_{name}"

    let leakDec =
        if enableLeakCheck then
            [X86_64.PUSH scratch
             X86_64.PUSH X86_64.RCX
             X86_64.LEA_rip (scratch, "_leak_count")
             X86_64.MOV_load (X86_64.RCX, scratch, 0)
             X86_64.SUB_imm (X86_64.RCX, 1)
             X86_64.MOV_store (scratch, 0, X86_64.RCX)
             X86_64.POP X86_64.RCX
             X86_64.POP scratch]
        else []

    /// Inline helper: process child pointer in R8.
    /// If R8 is a valid tagged list pointer:
    ///   - If RAX == 0: set RAX = R8 (use as next work item)
    ///   - Else: PUSH R8 and increment RCX (add to work stack)
    let addChild (suffix: string) : X86_64.Instr list =
        let doneLabel = label $"child_done_{suffix}"
        let pushLabel = label $"child_push_{suffix}"
        [// Skip null
         X86_64.TEST_reg (X86_64.R8, X86_64.R8)
         X86_64.Jcc (X86_64.EQ, doneLabel)
         // Check tag is in [1,5]
         X86_64.MOV_reg (X86_64.R9, X86_64.R8)
         X86_64.AND_imm (X86_64.R9, 7)
         X86_64.TEST_reg (X86_64.R9, X86_64.R9)
         X86_64.Jcc (X86_64.EQ, doneLabel)
         X86_64.CMP_imm (X86_64.R9, 5)
         X86_64.Jcc (X86_64.GT, doneLabel)
         // Bounds check untagged address
         X86_64.MOV_reg (X86_64.R9, X86_64.R8)
         X86_64.AND_imm (X86_64.R9, -8)
         X86_64.CMP_reg (X86_64.R9, freeListBase)
         X86_64.Jcc (X86_64.B, doneLabel)
         X86_64.CMP_reg (X86_64.R9, heapPtr)
         X86_64.Jcc (X86_64.AE, doneLabel)
         // Valid child: add to work
         X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
         X86_64.Jcc (X86_64.NE, pushLabel)
         X86_64.MOV_reg (X86_64.RAX, X86_64.R8)
         X86_64.JMP doneLabel
         X86_64.Label pushLabel
         X86_64.PUSH X86_64.R8
         X86_64.ADD_imm (X86_64.RCX, 1)
         X86_64.Label doneLabel]

    let loopCheck = label "loop_check"
    let popOrRet = label "pop_or_ret"
    let helperRet = label "ret"
    let size8 = label "size_8"
    let size24 = label "size_24"
    let size32 = label "size_32"
    let size96 = label "size_96"
    let haveSize = label "have_size"
    let collectSingle = label "collect_single"
    let collectDeep = label "collect_deep"
    let collectNode2 = label "collect_node2"
    let collectNode3 = label "collect_node3"
    let collectLeaf = label "collect_leaf"
    let afterPrefix = label "after_prefix"
    let afterSuffix = label "after_suffix"
    let freeNode = label "free_node"

    [X86_64.Label listRefCountDecHelperLabel
     // RAX = tagged list pointer, init pending count
     X86_64.XOR_reg (X86_64.RCX, X86_64.RCX)  // pending = 0
     X86_64.JMP loopCheck

     X86_64.Label loopCheck
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, popOrRet)
     // Extract tag
     X86_64.MOV_reg (X86_64.RDX, X86_64.RAX)
     X86_64.AND_imm (X86_64.RDX, 7)
     X86_64.TEST_reg (X86_64.RDX, X86_64.RDX)
     X86_64.Jcc (X86_64.EQ, popOrRet)
     // Untag: RDI = RAX & ~7
     X86_64.MOV_reg (X86_64.RDI, X86_64.RAX)
     X86_64.AND_imm (X86_64.RDI, -8)
     // Bounds check
     X86_64.CMP_reg (X86_64.RDI, freeListBase)
     X86_64.Jcc (X86_64.B, popOrRet)
     X86_64.CMP_reg (X86_64.RDI, heapPtr)
     X86_64.Jcc (X86_64.AE, popOrRet)

     // Resolve payload size from tag
     X86_64.CMP_imm (X86_64.RDX, 1)
     X86_64.Jcc (X86_64.EQ, size8)       // SINGLE → 8
     X86_64.CMP_imm (X86_64.RDX, 2)
     X86_64.Jcc (X86_64.EQ, size96)      // DEEP → 96
     X86_64.CMP_imm (X86_64.RDX, 3)
     X86_64.Jcc (X86_64.EQ, size24)      // NODE2 → 24
     X86_64.CMP_imm (X86_64.RDX, 4)
     X86_64.Jcc (X86_64.EQ, size32)      // NODE3 → 32
     X86_64.CMP_imm (X86_64.RDX, 5)
     X86_64.Jcc (X86_64.EQ, size8)       // LEAF → 8
     X86_64.JMP popOrRet

     X86_64.Label size8
     X86_64.MOV_imm32 (X86_64.RSI, 8)
     X86_64.JMP haveSize
     X86_64.Label size24
     X86_64.MOV_imm32 (X86_64.RSI, 24)
     X86_64.JMP haveSize
     X86_64.Label size32
     X86_64.MOV_imm32 (X86_64.RSI, 32)
     X86_64.JMP haveSize
     X86_64.Label size96
     X86_64.MOV_imm32 (X86_64.RSI, 96)

     // RSI = payload size, RDI = untagged address, RDX = tag
     X86_64.Label haveSize
     // Refcount at [RDI + RSI]
     X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
     X86_64.ADD_reg (X86_64.R8, X86_64.RSI)      // R8 = &refcount
     X86_64.MOV_load (X86_64.R9, X86_64.R8, 0)   // R9 = refcount
     X86_64.SUB_imm (X86_64.R9, 1)
     X86_64.MOV_store (X86_64.R8, 0, X86_64.R9)  // store decremented
     X86_64.TEST_reg (X86_64.R9, X86_64.R9)
     X86_64.Jcc (X86_64.NE, popOrRet)             // refcount > 0, done

     // Refcount zero: collect children then free
     X86_64.CMP_imm (X86_64.RDX, 1)
     X86_64.Jcc (X86_64.EQ, collectSingle)
     X86_64.CMP_imm (X86_64.RDX, 2)
     X86_64.Jcc (X86_64.EQ, collectDeep)
     X86_64.CMP_imm (X86_64.RDX, 3)
     X86_64.Jcc (X86_64.EQ, collectNode2)
     X86_64.CMP_imm (X86_64.RDX, 4)
     X86_64.Jcc (X86_64.EQ, collectNode3)
     X86_64.CMP_imm (X86_64.RDX, 5)
     X86_64.Jcc (X86_64.EQ, collectLeaf)
     X86_64.JMP freeNode]

    // --- SINGLE (tag 1): one child at offset 0 ---
    @ [X86_64.Label collectSingle
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0)]
    @ addChild "single_0"
    @ [X86_64.JMP freeNode]

    // --- NODE2 (tag 3): two children at offsets 0, 8 ---
    @ [X86_64.Label collectNode2
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0)]
    @ addChild "node2_0"
    @ [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 8)]
    @ addChild "node2_1"
    @ [X86_64.JMP freeNode]

    // --- NODE3 (tag 4): three children at offsets 0, 8, 16 ---
    @ [X86_64.Label collectNode3
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0)]
    @ addChild "node3_0"
    @ [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 8)]
    @ addChild "node3_1"
    @ [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 16)]
    @ addChild "node3_2"
    @ [X86_64.JMP freeNode]

    // --- DEEP (tag 2): prefix[0..3], middle, suffix[0..3] ---
    @ [X86_64.Label collectDeep
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)
       // Prefix: count at offset 8, children at offsets 16,24,32,40
       X86_64.MOV_load (X86_64.R10, X86_64.RDI, 8)     // prefix_count
       X86_64.TEST_reg (X86_64.R10, X86_64.R10)
       X86_64.Jcc (X86_64.LE, afterPrefix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 16)]
    @ addChild "deep_p0"
    @ [X86_64.CMP_imm (X86_64.R10, 1)
       X86_64.Jcc (X86_64.LE, afterPrefix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 24)]
    @ addChild "deep_p1"
    @ [X86_64.CMP_imm (X86_64.R10, 2)
       X86_64.Jcc (X86_64.LE, afterPrefix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 32)]
    @ addChild "deep_p2"
    @ [X86_64.CMP_imm (X86_64.R10, 3)
       X86_64.Jcc (X86_64.LE, afterPrefix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 40)]
    @ addChild "deep_p3"
    @ [X86_64.Label afterPrefix
       // Middle tree at offset 48
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 48)]
    @ addChild "deep_middle"
    @ [// Suffix: count at offset 56, children at offsets 64,72,80,88
       X86_64.MOV_load (X86_64.R10, X86_64.RDI, 56)     // suffix_count
       X86_64.TEST_reg (X86_64.R10, X86_64.R10)
       X86_64.Jcc (X86_64.LE, afterSuffix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 64)]
    @ addChild "deep_s0"
    @ [X86_64.CMP_imm (X86_64.R10, 1)
       X86_64.Jcc (X86_64.LE, afterSuffix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 72)]
    @ addChild "deep_s1"
    @ [X86_64.CMP_imm (X86_64.R10, 2)
       X86_64.Jcc (X86_64.LE, afterSuffix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 80)]
    @ addChild "deep_s2"
    @ [X86_64.CMP_imm (X86_64.R10, 3)
       X86_64.Jcc (X86_64.LE, afterSuffix)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 88)]
    @ addChild "deep_s3"
    @ [X86_64.Label afterSuffix
       X86_64.JMP freeNode]

    // --- LEAF (tag 5): no children ---
    @ [X86_64.Label collectLeaf
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)]

    // --- Free node to free list by payload size class ---
    @ [X86_64.Label freeNode
       // freeList[RSI] = node; node.next = old_head
       X86_64.MOV_reg (X86_64.R8, freeListBase)
       X86_64.ADD_reg (X86_64.R8, X86_64.RSI)             // R8 = &freeList[payload_class]
       X86_64.MOV_load (X86_64.R9, X86_64.R8, 0)          // R9 = old head
       X86_64.MOV_store (X86_64.RDI, 0, X86_64.R9)        // node.next = old head
       X86_64.MOV_store (X86_64.R8, 0, X86_64.RDI)]       // freeList[class] = node
    @ leakDec
    @ [X86_64.JMP loopCheck]

    // --- Pop from work stack or return ---
    @ [X86_64.Label popOrRet
       X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
       X86_64.Jcc (X86_64.EQ, helperRet)
       X86_64.POP X86_64.RAX
       X86_64.SUB_imm (X86_64.RCX, 1)
       X86_64.JMP loopCheck

       X86_64.Label helperRet
       X86_64.RET]

// ============================================================================
// TaggedList RefCountInc Helper (increment root node refcount only)
// ============================================================================

/// Label for the shared list refcount inc helper function
let private listRefCountIncHelperLabel = "__dark_list_rc_inc_helper"

/// Generate the TaggedList RefCountInc helper function.
/// Called via CALL with the tagged list pointer in RAX.
/// Just increments the refcount of the root node (no recursion).
/// Clobbers RCX, RDX, RDI. Caller must save/restore.
let private generateListRefCountIncHelper () : X86_64.Instr list =
    let label name = $"__dark_list_rc_inc_{name}"
    let helperRet = label "ret"
    let size24 = label "size_24"
    let size32 = label "size_32"
    let size96 = label "size_96"
    let haveSize = label "have_size"

    [X86_64.Label listRefCountIncHelperLabel
     // RAX = tagged list pointer (or 0)
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, helperRet)
     // Extract tag
     X86_64.MOV_reg (X86_64.RCX, X86_64.RAX)
     X86_64.AND_imm (X86_64.RCX, 7)
     X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
     X86_64.Jcc (X86_64.EQ, helperRet)
     X86_64.CMP_imm (X86_64.RCX, 5)
     X86_64.Jcc (X86_64.GT, helperRet)
     // Untag: RDI = RAX & ~7
     X86_64.MOV_reg (X86_64.RDI, X86_64.RAX)
     X86_64.AND_imm (X86_64.RDI, -8)
     // Resolve payload size from tag
     X86_64.CMP_imm (X86_64.RCX, 2)
     X86_64.Jcc (X86_64.EQ, size96)
     X86_64.CMP_imm (X86_64.RCX, 3)
     X86_64.Jcc (X86_64.EQ, size24)
     X86_64.CMP_imm (X86_64.RCX, 4)
     X86_64.Jcc (X86_64.EQ, size32)
     // Tags 1 (SINGLE) and 5 (LEAF): payload = 8
     X86_64.MOV_imm32 (X86_64.RDX, 8)
     X86_64.JMP haveSize
     X86_64.Label size24
     X86_64.MOV_imm32 (X86_64.RDX, 24)
     X86_64.JMP haveSize
     X86_64.Label size32
     X86_64.MOV_imm32 (X86_64.RDX, 32)
     X86_64.JMP haveSize
     X86_64.Label size96
     X86_64.MOV_imm32 (X86_64.RDX, 96)
     // RDX = payload size, RDI = untagged address
     X86_64.Label haveSize
     X86_64.ADD_reg (X86_64.RDI, X86_64.RDX)       // RDI = &refcount
     X86_64.MOV_load (X86_64.RDX, X86_64.RDI, 0)   // RDX = refcount
     X86_64.ADD_imm (X86_64.RDX, 1)
     X86_64.MOV_store (X86_64.RDI, 0, X86_64.RDX)  // store incremented
     X86_64.Label helperRet
     X86_64.RET]

/// Adjust a stack slot offset to account for callee-saved registers pushed after RBP.
/// LIR stack slots are byte offsets from FP (e.g., -8, -16), but callee-saved pushes
/// occupy [RBP-8] through [RBP-N*8], so spill slots must be shifted past them.
let private adjustStackOffset (ctx: FuncCtx) (offset: int) : int =
    offset - (List.length ctx.UsedCalleeSaved * 8)

/// Track whether the last comparison was FCmp (float) for correct condition codes.
/// UCOMISD sets CF/ZF differently from CMP which sets SF/OF/ZF.
let mutable private lastCompWasFloat = false

/// Translate a single LIR instruction to x86-64 instructions
let private translateInstr (ctx: FuncCtx) (instr: LIR.Instr) : Result<X86_64.Instr list, string> =
    match instr with

    | LIR.Mov (dest, src) ->
        resolveReg dest
        |> Result.bind (fun destReg ->
            match src with
            | LIR.Imm value ->
                Ok (loadImm64 destReg value)
            | LIR.Reg srcReg ->
                resolveReg srcReg
                |> Result.map (fun srcX86 ->
                    if destReg = srcX86 then []
                    else [X86_64.MOV_reg (destReg, srcX86)])
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (destReg, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value ->
                Ok (emitStringLiteral destReg value)
            | LIR.FuncAddr funcName ->
                Ok [X86_64.LEA_rip (destReg, funcName)]
            | LIR.FloatImm value | LIR.FloatSymbol value ->
                // Store float bits in GP register
                let bits = System.BitConverter.DoubleToInt64Bits(value)
                Ok (loadImm64 destReg bits))

    | LIR.Store (stackSlot, src) ->
        // Stack slots are byte offsets from FP, adjusted past callee-saved pushes
        resolveReg src
        |> Result.map (fun srcReg ->
            [X86_64.MOV_store (X86_64.RBP, int32 (adjustStackOffset ctx stackSlot), srcReg)])

    | LIR.Add (dest, left, right) ->
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue ->
                    // x86_64: dest = left + imm32
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    Ok (setup @ [X86_64.ADD_imm (destReg, int32 value)])
                | LIR.Imm value ->
                    if destReg = scratch then
                        // dest is R11: can't use scratch for imm. Use PUSH/POP RCX.
                        Ok ([X86_64.PUSH X86_64.RCX]
                            @ loadImm64 X86_64.RCX value
                            @ [X86_64.ADD_reg (destReg, X86_64.RCX)
                               X86_64.POP X86_64.RCX])
                    else
                        let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                        Ok (setup @ loadImm64 scratch value @ [X86_64.ADD_reg (destReg, scratch)])
                | LIR.Reg rightReg ->
                    resolveReg rightReg
                    |> Result.map (fun rightX86 ->
                        if destReg = rightX86 && destReg <> leftReg then
                            // dest is right operand: ADD is commutative, so just swap
                            [X86_64.ADD_reg (destReg, leftReg)]
                        else
                            let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                            setup @ [X86_64.ADD_reg (destReg, rightX86)])
                | LIR.StackSlot offset ->
                    let adjOff = int32 (adjustStackOffset ctx offset)
                    if destReg = scratch && leftReg = scratch then
                        // Both dest and left are R11: use PUSH/POP to avoid clobbering
                        Ok ([X86_64.PUSH X86_64.RCX
                             X86_64.MOV_load (X86_64.RCX, X86_64.RBP, adjOff)
                             X86_64.ADD_reg (destReg, X86_64.RCX)
                             X86_64.POP X86_64.RCX])
                    else
                        let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                        Ok (setup @ [X86_64.MOV_load (scratch, X86_64.RBP, adjOff); X86_64.ADD_reg (destReg, scratch)])
                | _ -> Error $"Unsupported Add right operand: {right}"))

    | LIR.Sub (dest, left, right) ->
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue ->
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    Ok (setup @ [X86_64.SUB_imm (destReg, int32 value)])
                | LIR.Imm value ->
                    if destReg = scratch then
                        Ok ([X86_64.PUSH X86_64.RCX]
                            @ loadImm64 X86_64.RCX value
                            @ [X86_64.SUB_reg (destReg, X86_64.RCX)
                               X86_64.POP X86_64.RCX])
                    else
                        let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                        Ok (setup @ loadImm64 scratch value @ [X86_64.SUB_reg (destReg, scratch)])
                | LIR.Reg rightReg ->
                    resolveReg rightReg
                    |> Result.map (fun rightX86 ->
                        if destReg = rightX86 && destReg <> leftReg then
                            if destReg = scratch then
                                // dest=right=R11, left is different: use PUSH/POP
                                [X86_64.PUSH X86_64.RCX
                                 X86_64.MOV_reg (X86_64.RCX, leftReg)
                                 X86_64.SUB_reg (X86_64.RCX, rightX86)
                                 X86_64.MOV_reg (destReg, X86_64.RCX)
                                 X86_64.POP X86_64.RCX]
                            else
                                [X86_64.MOV_reg (scratch, leftReg)
                                 X86_64.SUB_reg (scratch, rightX86)
                                 X86_64.MOV_reg (destReg, scratch)]
                        else
                            let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                            setup @ [X86_64.SUB_reg (destReg, rightX86)])
                | LIR.StackSlot offset ->
                    let adjOff = int32 (adjustStackOffset ctx offset)
                    if destReg = scratch && leftReg = scratch then
                        Ok ([X86_64.PUSH X86_64.RCX
                             X86_64.MOV_load (X86_64.RCX, X86_64.RBP, adjOff)
                             X86_64.SUB_reg (destReg, X86_64.RCX)
                             X86_64.POP X86_64.RCX])
                    else
                        let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                        Ok (setup @ [X86_64.MOV_load (scratch, X86_64.RBP, adjOff); X86_64.SUB_reg (destReg, scratch)])
                | _ -> Error $"Unsupported Sub right operand: {right}"))

    | LIR.Mul (dest, left, right) ->
        // x86_64 IMUL r64, r/m64 — dest = dest * src
        // Must handle case where dest == right (would clobber right when setting up left)
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg left
            |> Result.bind (fun leftReg ->
                resolveReg right
                |> Result.map (fun rightReg ->
                    if destReg = rightReg && destReg <> leftReg then
                        if destReg = scratch then
                            // dest=right=R11, left different: MUL is commutative, swap
                            [X86_64.IMUL_reg (destReg, leftReg)]
                        else
                            [X86_64.MOV_reg (scratch, leftReg)
                             X86_64.IMUL_reg (scratch, rightReg)
                             X86_64.MOV_reg (destReg, scratch)]
                    else
                        let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                        setup @ [X86_64.IMUL_reg (destReg, rightReg)])))

    | LIR.Sdiv (dest, left, right) ->
        // IDIV: RDX:RAX / src → RAX=quotient, RDX=remainder.
        // Clobbers both RAX and RDX. Save/restore RDX using the red zone
        // (below RSP) to avoid changing RSP.
        // Special case: INT64_MIN / -1 traps with #DE (SIGFPE). LIR.Sdiv
        // is defined to wrap to INT64_MIN, so detect the case and bypass.
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg left
            |> Result.bind (fun leftReg ->
                resolveReg right
                |> Result.map (fun rightReg ->
                    let overflowLabel = freshLabel "idiv_overflow"
                    let doneLabel = freshLabel "idiv_done"
                    let divisor =
                        if rightReg = X86_64.RAX || rightReg = X86_64.RDX then scratch
                        else rightReg
                    let saveDivisor =
                        if rightReg = X86_64.RAX || rightReg = X86_64.RDX then
                            [X86_64.MOV_reg (scratch, rightReg)]
                        else []
                    let moveLeft =
                        if leftReg <> X86_64.RAX then [X86_64.MOV_reg (X86_64.RAX, leftReg)]
                        else []
                    // Check for INT64_MIN / -1 overflow
                    saveDivisor
                    @ moveLeft
                    @ [X86_64.CMP_imm (divisor, -1)]
                    @ [X86_64.Jcc (X86_64.NE, doneLabel)]
                    // divisor is -1, check if dividend is INT64_MIN
                    @ loadImm64 scratch System.Int64.MinValue
                    @ [X86_64.CMP_reg (X86_64.RAX, scratch)]
                    @ [X86_64.Jcc (X86_64.EQ, overflowLabel)]
                    // Normal IDIV path
                    @ [X86_64.Label doneLabel]
                    // Restore divisor if it was moved to scratch for the CMP
                    @ (if rightReg = X86_64.RAX || rightReg = X86_64.RDX then
                           [X86_64.MOV_reg (scratch, divisor)]  // re-setup (was clobbered by INT64_MIN load)
                       else [])
                    @ (if (rightReg = X86_64.RAX || rightReg = X86_64.RDX) then
                           [X86_64.MOV_reg (scratch, rightReg)]
                       else [])
                    @ (if leftReg <> X86_64.RAX then [X86_64.MOV_reg (X86_64.RAX, leftReg)] else [])
                    @ [X86_64.MOV_store (X86_64.RSP, -8, X86_64.RDX)]
                    @ [X86_64.CQO; X86_64.IDIV divisor]
                    @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else [])
                    @ [X86_64.MOV_load (X86_64.RDX, X86_64.RSP, -8)
                       X86_64.JMP (overflowLabel + "_end")]
                    // Overflow path: return INT64_MIN
                    @ [X86_64.Label overflowLabel]
                    @ loadImm64 destReg System.Int64.MinValue
                    @ [X86_64.Label (overflowLabel + "_end")])))

    | LIR.Udiv (dest, left, right) ->
        // DIV: RDX:RAX / src → RAX=quotient, RDX=remainder. Unsigned, so zero-extend
        // RAX into RDX via `xor rdx,rdx` (not CQO), and use DIV (not IDIV). No
        // INT64_MIN/-1 overflow case for unsigned. Preserve RDX via the red zone,
        // mirroring the Sdiv setup.
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg left
            |> Result.bind (fun leftReg ->
                resolveReg right
                |> Result.map (fun rightReg ->
                    let divisor =
                        if rightReg = X86_64.RAX || rightReg = X86_64.RDX then scratch
                        else rightReg
                    let saveDivisor =
                        if rightReg = X86_64.RAX || rightReg = X86_64.RDX then
                            [X86_64.MOV_reg (scratch, rightReg)]
                        else []
                    let moveLeft =
                        if leftReg <> X86_64.RAX then [X86_64.MOV_reg (X86_64.RAX, leftReg)]
                        else []
                    saveDivisor
                    @ moveLeft
                    @ [X86_64.MOV_store (X86_64.RSP, -8, X86_64.RDX)]
                    @ [X86_64.XOR_reg (X86_64.RDX, X86_64.RDX); X86_64.DIV divisor]
                    @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else [])
                    @ [X86_64.MOV_load (X86_64.RDX, X86_64.RSP, -8)])))

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        // dest = sub - mulLeft * mulRight
        // No fused instruction on x86_64: IMUL tmp, mulLeft, mulRight; MOV dest, sub; SUB dest, tmp
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg mulLeft
            |> Result.bind (fun mlReg ->
                resolveReg mulRight
                |> Result.bind (fun mrReg ->
                    resolveReg sub
                    |> Result.map (fun subReg ->
                        [X86_64.MOV_reg (scratch, mlReg); X86_64.IMUL_reg (scratch, mrReg)]
                        @ (if destReg <> subReg then [X86_64.MOV_reg (destReg, subReg)] else [])
                        @ [X86_64.SUB_reg (destReg, scratch)]))))

    | LIR.Cmp (left, right) ->
        lastCompWasFloat <- false
        resolveReg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue ->
                Ok [X86_64.CMP_imm (leftReg, int32 value)]
            | LIR.Imm value ->
                if leftReg = scratch then
                    Ok ([X86_64.PUSH X86_64.RCX]
                        @ loadImm64 X86_64.RCX value
                        @ [X86_64.CMP_reg (leftReg, X86_64.RCX)
                           X86_64.POP X86_64.RCX])
                else
                    Ok (loadImm64 scratch value @ [X86_64.CMP_reg (leftReg, scratch)])
            | LIR.Reg rightReg ->
                resolveReg rightReg
                |> Result.map (fun rightX86 ->
                    if leftReg = scratch && rightX86 = scratch then
                        // Both are R11 - always equal, just emit CMP R11, R11
                        [X86_64.CMP_reg (scratch, scratch)]
                    else
                        [X86_64.CMP_reg (leftReg, rightX86)])
            | LIR.StackSlot offset ->
                let adjOff = int32 (adjustStackOffset ctx offset)
                if leftReg = scratch then
                    Ok ([X86_64.PUSH X86_64.RCX
                         X86_64.MOV_load (X86_64.RCX, X86_64.RBP, adjOff)
                         X86_64.CMP_reg (leftReg, X86_64.RCX)
                         X86_64.POP X86_64.RCX])
                else
                    Ok [X86_64.MOV_load (scratch, X86_64.RBP, adjOff); X86_64.CMP_reg (leftReg, scratch)]
            | _ -> Error $"Unsupported Cmp right operand: {right}")

    | LIR.Cset (dest, cond) ->
        resolveReg dest
        |> Result.map (fun destReg ->
            if lastCompWasFloat then
                match cond with
                | LIR.EQ ->
                    // Float EQ: ordered AND equal (ZF=1 AND PF=0)
                    // SETE + SETNP, then AND
                    [X86_64.SETcc (X86_64.EQ, destReg)
                     X86_64.MOVZX_byte (destReg, destReg)
                     X86_64.SETcc (X86_64.NP, scratch)
                     X86_64.MOVZX_byte (scratch, scratch)
                     X86_64.AND_reg (destReg, scratch)]
                | LIR.NE ->
                    // Float NE: unordered OR not equal (ZF=0 OR PF=1)
                    // SETNE + SETP, then OR
                    [X86_64.SETcc (X86_64.NE, destReg)
                     X86_64.MOVZX_byte (destReg, destReg)
                     X86_64.SETcc (X86_64.P, scratch)
                     X86_64.MOVZX_byte (scratch, scratch)
                     X86_64.OR_reg (destReg, scratch)]
                | LIR.LT | LIR.ULT -> [X86_64.SETcc (X86_64.B, destReg); X86_64.MOVZX_byte (destReg, destReg)]
                | LIR.GT | LIR.UGT -> [X86_64.SETcc (X86_64.A, destReg); X86_64.MOVZX_byte (destReg, destReg)]
                | LIR.LE | LIR.ULE -> [X86_64.SETcc (X86_64.BE, destReg); X86_64.MOVZX_byte (destReg, destReg)]
                | LIR.GE | LIR.UGE -> [X86_64.SETcc (X86_64.AE, destReg); X86_64.MOVZX_byte (destReg, destReg)]
            else
                let x86Cond =
                    match cond with
                    | LIR.EQ -> X86_64.EQ | LIR.NE -> X86_64.NE
                    | LIR.LT -> X86_64.LT | LIR.GT -> X86_64.GT
                    | LIR.LE -> X86_64.LE | LIR.GE -> X86_64.GE
                    // Unsigned: below/above (setb/seta/setbe/setae).
                    | LIR.ULT -> X86_64.B | LIR.UGT -> X86_64.A
                    | LIR.ULE -> X86_64.BE | LIR.UGE -> X86_64.AE
                [X86_64.SETcc (x86Cond, destReg); X86_64.MOVZX_byte (destReg, destReg)])

    | LIR.And (dest, left, right) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg left |> Result.bind (fun l -> resolveReg right |> Result.map (fun r ->
            if d = r && d <> l then
                [X86_64.AND_reg (d, l)]  // AND is commutative
            else
                (if d <> l then [X86_64.MOV_reg (d, l)] else []) @ [X86_64.AND_reg (d, r)])))

    | LIR.And_imm (dest, src, imm) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            (if d <> s then [X86_64.MOV_reg (d, s)] else [])
            @ [X86_64.AND_imm (d, int32 imm)]))

    | LIR.Orr (dest, left, right) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg left |> Result.bind (fun l -> resolveReg right |> Result.map (fun r ->
            if d = r && d <> l then [X86_64.OR_reg (d, l)]  // OR is commutative
            else (if d <> l then [X86_64.MOV_reg (d, l)] else []) @ [X86_64.OR_reg (d, r)])))

    | LIR.Eor (dest, left, right) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg left |> Result.bind (fun l -> resolveReg right |> Result.map (fun r ->
            if d = r && d <> l then [X86_64.XOR_reg (d, l)]  // XOR is commutative
            else (if d <> l then [X86_64.MOV_reg (d, l)] else []) @ [X86_64.XOR_reg (d, r)])))

    | LIR.Lsl_imm (dest, src, shift) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.SHL_imm (d, shift)]))

    | LIR.Lsr_imm (dest, src, shift) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.SHR_imm (d, shift)]))

    | LIR.Mvn (dest, src) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.NOT d]))

    | LIR.Sxtb (dest, src) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            [X86_64.MOVSX_byte (d, s)]))

    | LIR.Sxth (dest, src) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            [X86_64.MOVSX_word (d, s)]))

    | LIR.Sxtw (dest, src) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            [X86_64.MOVSXD (d, s)]))

    | LIR.Uxtb (dest, src) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            [X86_64.MOVZX_byte (d, s)]))

    | LIR.Exit ->
        // RDI should already contain the exit code
        Ok genExitSyscall

    | LIR.PrintChars bytes ->
        // Write literal bytes to stdout: write(1, buf, len)
        // Push bytes onto stack, write from RSP, then pop
        let len = List.length bytes
        let padded = ((len + 7) / 8) * 8  // 8-byte aligned
        let paddedBytes = bytes @ List.replicate (padded - len) 0uy
        // Push bytes in reverse 8-byte chunks
        let pushInstrs =
            paddedBytes
            |> List.chunkBySize 8
            |> List.rev
            |> List.collect (fun chunk ->
                let value =
                    chunk |> List.mapi (fun i b -> int64 b <<< (i * 8)) |> List.fold (|||) 0L
                loadImm64 scratch value @ [X86_64.PUSH scratch])
        let writeInstrs =
            [X86_64.MOV_imm32 (X86_64.RDI, 1)]  // fd = stdout
            @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]  // buf = stack
            @ loadImm64 X86_64.RDX (int64 len)  // len
            @ genWriteSyscall
            @ [X86_64.ADD_imm (X86_64.RSP, int32 padded)]  // pop
        Ok (pushInstrs @ writeInstrs)

    | LIR.PrintInt64 reg ->
        resolveReg reg
        |> Result.map (fun srcReg -> genPrintInt64 srcReg true)

    | LIR.PrintInt64NoNewline reg ->
        resolveReg reg
        |> Result.map (fun srcReg -> genPrintInt64 srcReg false)

    | LIR.PrintBool reg ->
        resolveReg reg
        |> Result.map (fun srcReg ->
            // Print "true\n" or "false\n" without exiting (Ret handles exit)
            let trueLabel = freshLabel "bool_true"
            let writeLabel = freshLabel "bool_write"
            [X86_64.TEST_reg (srcReg, srcReg)
             X86_64.Jcc (X86_64.NE, trueLabel)
             X86_64.SUB_imm (X86_64.RSP, 8)]
            @ loadImm64 scratch 0x0A65736C6166L  // "false\n"
            @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
               X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
               X86_64.MOV_imm32 (X86_64.RDX, 6)
               X86_64.JMP writeLabel
               X86_64.Label trueLabel
               X86_64.SUB_imm (X86_64.RSP, 8)]
            @ loadImm64 scratch 0x0A65757274L  // "true\n"
            @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
               X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
               X86_64.MOV_imm32 (X86_64.RDX, 5)
               X86_64.Label writeLabel
               X86_64.MOV_imm32 (X86_64.RDI, 1)]
            @ genWriteSyscall
            @ [X86_64.ADD_imm (X86_64.RSP, 8)])

    | LIR.PrintBoolNoNewline reg ->
        resolveReg reg
        |> Result.map (fun srcReg ->
            // TODO: implement bool printing without exit
            [])

    | LIR.PrintHeapString reg ->
        // Heap string format: [length:8][data:N][refcount:8]
        // Print data + newline (exit handled by subsequent Ret → epilogue)
        resolveReg reg
        |> Result.map (fun srcReg ->
            [X86_64.MOV_load (X86_64.RDX, srcReg, 0)
             X86_64.LEA (X86_64.RSI, srcReg, 8)
             X86_64.MOV_imm32 (X86_64.RDI, 1)]
            @ genWriteSyscall
            @ [X86_64.SUB_imm (X86_64.RSP, 8)]
            @ loadImm64 scratch 10L
            @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
               X86_64.MOV_imm32 (X86_64.RDI, 1)
               X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
               X86_64.MOV_imm32 (X86_64.RDX, 1)]
            @ genWriteSyscall
            @ [X86_64.ADD_imm (X86_64.RSP, 8)])

    | LIR.PrintHeapStringNoNewline reg ->
        resolveReg reg
        |> Result.map (fun srcReg ->
            [X86_64.MOV_load (X86_64.RDX, srcReg, 0)
             X86_64.LEA (X86_64.RSI, srcReg, 8)
             X86_64.MOV_imm32 (X86_64.RDI, 1)]
            @ genWriteSyscall)

    | LIR.PrintString str ->
        // Write a literal string to stdout and exit(0)
        let bytes = System.Text.Encoding.UTF8.GetBytes(str + "\n")
        let len = bytes.Length
        let padded = ((len + 7) / 8) * 8
        let paddedBytes = (bytes |> Array.toList) @ List.replicate (padded - len) 0uy
        let pushInstrs =
            paddedBytes
            |> List.chunkBySize 8
            |> List.rev
            |> List.collect (fun chunk ->
                let value = chunk |> List.mapi (fun i b -> int64 b <<< (i * 8)) |> List.fold (|||) 0L
                loadImm64 scratch value @ [X86_64.PUSH scratch])
        Ok (
            pushInstrs
            @ [X86_64.MOV_imm32 (X86_64.RDI, 1)]  // fd = stdout
            @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]
            @ loadImm64 X86_64.RDX (int64 len)
            @ genWriteSyscall
            @ [X86_64.ADD_imm (X86_64.RSP, int32 padded)]
            @ loadImm64 X86_64.RDI 0L
            @ genExitSyscall
        )

    | LIR.RuntimeError msg ->
        // Write error message to stderr (fd=2) and exit(1)
        let bytes = System.Text.Encoding.UTF8.GetBytes(msg + "\n")
        let len = bytes.Length
        let padded = ((len + 7) / 8) * 8
        let paddedBytes = (bytes |> Array.toList) @ List.replicate (padded - len) 0uy
        let pushInstrs =
            paddedBytes
            |> List.chunkBySize 8
            |> List.rev
            |> List.collect (fun chunk ->
                let value = chunk |> List.mapi (fun i b -> int64 b <<< (i * 8)) |> List.fold (|||) 0L
                loadImm64 scratch value @ [X86_64.PUSH scratch])
        Ok (
            pushInstrs
            @ [X86_64.MOV_imm32 (X86_64.RDI, 2)]  // fd = stderr
            @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]
            @ loadImm64 X86_64.RDX (int64 len)
            @ genWriteSyscall
            @ [X86_64.ADD_imm (X86_64.RSP, int32 padded)]
            @ loadImm64 X86_64.RDI 1L
            @ genExitSyscall
        )

    | LIR.SaveRegs (intRegs, floatRegs) ->
        // Save caller-saved registers that are live across a call.
        // PUSH each in order — first pushed is deepest on the stack.
        if List.isEmpty intRegs && List.isEmpty floatRegs then
            Ok []
        else
            let intSaves =
                intRegs |> List.map (fun reg -> X86_64.PUSH (lirRegToX86 reg))
            let floatSaves =
                floatRegs |> List.collect (fun freg ->
                    let xmm = lirFRegToX86 freg
                    // SUB RSP, 8; MOVSD [RSP], xmm
                    [X86_64.SUB_imm (X86_64.RSP, 8)
                     X86_64.MOVSD_store (X86_64.RSP, 0, xmm)])
            // Track save area size for RestoreRegs/ArgMoves
            Ok (intSaves @ floatSaves)

    | LIR.RestoreRegs (intRegs, floatRegs) ->
        if List.isEmpty intRegs && List.isEmpty floatRegs then
            Ok []
        else
            // Restore in reverse order of saves
            let floatRestores =
                floatRegs |> List.rev |> List.collect (fun freg ->
                    let xmm = lirFRegToX86 freg
                    [X86_64.MOVSD_load (xmm, X86_64.RSP, 0)
                     X86_64.ADD_imm (X86_64.RSP, 8)])
            let intRestores =
                intRegs |> List.rev |> List.map (fun reg -> X86_64.POP (lirRegToX86 reg))
            Ok (floatRestores @ intRestores)

    | LIR.ArgMoves moves ->
        // Parallel move resolution for function arguments.
        // Must handle case where a source register is also a destination of another
        // move (e.g., X1 <- X21; X4 <- X1 — second move must read ORIGINAL X1).
        //
        // Strategy: save all source registers that will be clobbered to scratch stack,
        // then perform all moves using saved values where needed.
        let destRegs = moves |> List.choose (fun (d, _) -> Some d) |> Set.ofList
        let generateMove (destPhys: LIR.PhysReg, srcOp: LIR.Operand) : Result<X86_64.Instr list, string> =
            let destX86 = lirRegToX86 destPhys
            match srcOp with
            | LIR.Imm value ->
                Ok (loadImm64 destX86 value)
            | LIR.Reg (LIR.Physical srcPhys) ->
                if srcPhys = destPhys then Ok []
                else
                    // If source will be clobbered by an earlier move, we need the
                    // saved value. For simplicity, we use a two-pass approach:
                    // all moves from Reg sources that ARE destinations get saved first.
                    Ok [X86_64.MOV_reg (destX86, lirRegToX86 srcPhys)]
            | LIR.Reg (LIR.Virtual _) ->
                Error "Virtual register in ArgMoves"
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (destX86, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value ->
                Ok (emitStringLiteral destX86 value)
            | LIR.FloatSymbol value ->
                let bits = System.BitConverter.DoubleToInt64Bits(value)
                Ok (loadImm64 destX86 bits)  // Store float bits in GP register (for passing as arg)
            | LIR.FuncAddr funcName ->
                Ok [X86_64.LEA_rip (destX86, funcName)]
            | _ -> Error $"Unsupported ArgMoves operand: {srcOp}"
        // Two-pass approach to handle parallel move conflicts:
        // 1. Find source registers that are also destinations (will be clobbered)
        // 2. Save those to the red zone before any moves
        // 3. Do all moves, using red zone values for clobbered sources
        let destRegSet = moves |> List.map fst |> Set.ofList
        let clobberedSources =
            moves
            |> List.choose (fun (_, srcOp) ->
                match srcOp with
                | LIR.Reg (LIR.Physical srcPhys) ->
                    if Set.contains srcPhys destRegSet then Some srcPhys
                    else None
                | _ -> None)
            |> List.distinct

        // Save clobbered sources to red zone (below RSP, no RSP adjustment)
        // Use offsets -16, -24, -32, etc. (-8 is used by IDIV)
        let saveInstrs =
            clobberedSources
            |> List.mapi (fun i reg ->
                let offset = -16 - (i * 8)
                X86_64.MOV_store (X86_64.RSP, int32 offset, lirRegToX86 reg))

        // Build a map from clobbered source to red zone offset
        let clobberedOffsets =
            clobberedSources
            |> List.mapi (fun i reg -> (reg, -16 - (i * 8)))
            |> Map.ofList

        // Generate moves, using red zone for clobbered sources
        let generateMoveWithSave (destPhys: LIR.PhysReg, srcOp: LIR.Operand) : Result<X86_64.Instr list, string> =
            let destX86 = lirRegToX86 destPhys
            match srcOp with
            | LIR.Reg (LIR.Physical srcPhys) when Map.containsKey srcPhys clobberedOffsets ->
                if srcPhys = destPhys then Ok []
                else
                    let offset = clobberedOffsets.[srcPhys]
                    Ok [X86_64.MOV_load (destX86, X86_64.RSP, int32 offset)]
            | _ -> generateMove (destPhys, srcOp)

        let rec genMoves acc remaining =
            match remaining with
            | [] -> Ok (List.rev acc |> List.concat)
            | m :: rest ->
                match generateMoveWithSave m with
                | Error e -> Error e
                | Ok instrs -> genMoves (instrs :: acc) rest
        genMoves [] moves
        |> Result.map (fun moveInstrs -> saveInstrs @ moveInstrs)

    | LIR.TailArgMoves moves ->
        // Same parallel move resolution as ArgMoves
        let destRegSet = moves |> List.map fst |> Set.ofList
        let clobberedSources =
            moves |> List.choose (fun (_, srcOp) ->
                match srcOp with
                | LIR.Reg (LIR.Physical srcPhys) when Set.contains srcPhys destRegSet -> Some srcPhys
                | _ -> None)
            |> List.distinct
        let saveInstrs =
            clobberedSources |> List.mapi (fun i reg ->
                X86_64.MOV_store (X86_64.RSP, int32 (-16 - i * 8), lirRegToX86 reg))
        let clobberedOffsets =
            clobberedSources |> List.mapi (fun i reg -> (reg, -16 - i * 8)) |> Map.ofList
        let generateMove (destPhys: LIR.PhysReg, srcOp: LIR.Operand) : Result<X86_64.Instr list, string> =
            let destX86 = lirRegToX86 destPhys
            match srcOp with
            | LIR.Imm value -> Ok (loadImm64 destX86 value)
            | LIR.Reg (LIR.Physical srcPhys) when Map.containsKey srcPhys clobberedOffsets ->
                if srcPhys = destPhys then Ok []
                else Ok [X86_64.MOV_load (destX86, X86_64.RSP, int32 clobberedOffsets.[srcPhys])]
            | LIR.Reg (LIR.Physical srcPhys) ->
                if srcPhys = destPhys then Ok []
                else Ok [X86_64.MOV_reg (destX86, lirRegToX86 srcPhys)]
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (destX86, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value ->
                Ok (emitStringLiteral destX86 value)
            | LIR.FloatSymbol value ->
                let bits = System.BitConverter.DoubleToInt64Bits(value)
                Ok (loadImm64 destX86 bits)
            | LIR.FuncAddr funcName ->
                Ok [X86_64.LEA_rip (destX86, funcName)]
            | _ -> Error $"Unsupported TailArgMoves operand: {srcOp}"
        let rec genMoves acc remaining =
            match remaining with
            | [] -> Ok (List.rev acc |> List.concat)
            | m :: rest ->
                match generateMove m with
                | Error e -> Error e
                | Ok instrs -> genMoves (instrs :: acc) rest
        genMoves [] moves
        |> Result.map (fun moveInstrs -> saveInstrs @ moveInstrs)

    | LIR.Call (dest, funcName, _args) ->
        // Arguments are already in place from ArgMoves
        resolveReg dest
        |> Result.map (fun destReg ->
            [X86_64.CALL funcName]
            @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else []))

    | LIR.TailCall (funcName, _args) ->
        // Restore stack frame before jumping (epilogue without RET)
        Ok (genEpilogue ctx.StackSize ctx.UsedCalleeSaved @ [X86_64.JMP funcName])

    | LIR.IndirectCall (dest, func, _args) ->
        resolveReg func
        |> Result.bind (fun funcReg ->
            resolveReg dest
            |> Result.map (fun destReg ->
                [X86_64.CALL_reg funcReg]
                @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else [])))

    | LIR.IndirectTailCall (func, _args) ->
        resolveReg func
        |> Result.map (fun funcReg ->
            genEpilogue ctx.StackSize ctx.UsedCalleeSaved
            @ [X86_64.JMP_reg funcReg])

    | LIR.LoadFuncAddr (dest, funcName) ->
        resolveReg dest
        |> Result.map (fun destReg -> [X86_64.LEA_rip (destReg, funcName)])

    | LIR.FArgMoves moves ->
        // Move float arguments into XMM registers. These are PARALLEL moves: a naive
        // sequential lowering corrupts cyclic shuffles — e.g. swapping the two args of a
        // 2-arg float call, FArgMoves(D0<-D1, D1<-D0), as `movsd D0,D1; movsd D1,D0`
        // leaves BOTH = the old D1 (was bug #30: multiply(x, sqrt 4) computed x*x).
        // Resolve the moves and break cycles through the reserved XMM15 scratch.
        let resolved =
            moves |> List.map (fun (destPhys, srcFreg) ->
                match srcFreg with
                | LIR.FPhysical srcPhys -> Ok (lirFRegToX86 destPhys, lirFRegToX86 srcPhys)
                | LIR.FVirtual id -> Error $"Unresolved virtual float register f{id} in FArgMoves")
            |> List.fold (fun acc r ->
                match acc, r with
                | Ok xs, Ok x -> Ok (x :: xs)
                | Error e, _ -> Error e
                | _, Error e -> Error e) (Ok [])
            |> Result.map List.rev
        resolved
        |> Result.map (fun xmmMoves ->
            let actions = ParallelMoves.resolve xmmMoves (fun (src: X86_64.FReg) -> Some src)
            actions |> List.collect (fun action ->
                match action with
                | ParallelMoves.SaveToTemp src -> [X86_64.MOVSD_reg (X86_64.XMM15, src)]
                | ParallelMoves.Move (dest, src) -> if dest = src then [] else [X86_64.MOVSD_reg (dest, src)]
                | ParallelMoves.MoveFromTemp dest -> [X86_64.MOVSD_reg (dest, X86_64.XMM15)]))

    | LIR.Phi (dest, _, _) ->
        // Phi nodes should be eliminated before codegen (SSA destruction)
        // If we see one, it's a no-op — the parallel moves handle it
        Ok []

    | LIR.FPhi (_, _) ->
        Ok []

    | LIR.HeapAlloc (dest, sizeBytes) ->
        // Bump allocator with free list reuse + bounds check
        let okLabel = freshLabel "heap_ok"
        resolveReg dest
        |> Result.map (fun destReg ->
            // Check free list for this size class (if valid)
            let freeListAlloc =
                if sizeBytes >= 0 && sizeBytes < freeListSize then
                    let bumpLabel = freshLabel "heap_bump"
                    let freeListDoneLabel = freshLabel "heap_fl_done"
                    [X86_64.PUSH X86_64.RCX
                     X86_64.MOV_load (X86_64.RCX, freeListBase, sizeBytes)
                     X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
                     X86_64.Jcc (X86_64.EQ, bumpLabel)
                     // Free list hit: dest = block, update head to next
                     X86_64.MOV_reg (destReg, X86_64.RCX)
                     X86_64.MOV_load (X86_64.RCX, X86_64.RCX, 0)     // next ptr
                     X86_64.MOV_store (freeListBase, sizeBytes, X86_64.RCX)
                     X86_64.POP X86_64.RCX
                     X86_64.JMP freeListDoneLabel
                     X86_64.Label bumpLabel
                     X86_64.POP X86_64.RCX], [X86_64.Label freeListDoneLabel]
                else [], []
            let (freeListPre, freeListPost) = freeListAlloc
            freeListPre
            // Bump allocator path
            @ [X86_64.MOV_reg (destReg, heapPtr)
               X86_64.ADD_imm (heapPtr, int32 sizeBytes)]
            // Bounds check
            @ (if destReg = scratch then
                [X86_64.PUSH X86_64.RAX
                 X86_64.MOV_reg (X86_64.RAX, heapPtr)
                 X86_64.SUB_reg (X86_64.RAX, freeListBase)
                 X86_64.CMP_imm (X86_64.RAX, int32 heapMmapSizeBytes)
                 X86_64.POP X86_64.RAX
                 X86_64.Jcc (X86_64.LE, okLabel)]
               else
                [X86_64.MOV_reg (scratch, heapPtr)
                 X86_64.SUB_reg (scratch, freeListBase)
                 X86_64.CMP_imm (scratch, int32 heapMmapSizeBytes)
                 X86_64.Jcc (X86_64.LE, okLabel)])
            @ genOomJump ()
            @ [X86_64.Label okLabel]
            @ freeListPost)

    | LIR.HeapStore (addr, offset, src, _) ->
        resolveReg addr
        |> Result.bind (fun addrReg ->
            match src with
            | LIR.Imm value ->
                if addrReg = scratch then
                    // Address is R11 - can't use scratch for the immediate value
                    Ok ([X86_64.PUSH X86_64.RCX]
                        @ loadImm64 X86_64.RCX value
                        @ [X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                           X86_64.POP X86_64.RCX])
                else
                    Ok (loadImm64 scratch value @ [X86_64.MOV_store (addrReg, int32 offset, scratch)])
            | LIR.Reg srcReg ->
                resolveReg srcReg
                |> Result.map (fun srcX86 ->
                    if addrReg = scratch && srcX86 = scratch then
                        // Both addr and src are R11 - store R11 at [R11 + offset]
                        [X86_64.MOV_store (scratch, int32 offset, scratch)]
                    else if addrReg = scratch then
                        [X86_64.MOV_store (scratch, int32 offset, srcX86)]
                    else
                        [X86_64.MOV_store (addrReg, int32 offset, srcX86)])
            | LIR.FuncAddr funcName ->
                if addrReg = scratch then
                    // Address is R11 - use RCX to hold the function address
                    Ok [X86_64.PUSH X86_64.RCX
                        X86_64.LEA_rip (X86_64.RCX, funcName)
                        X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                        X86_64.POP X86_64.RCX]
                else
                    Ok [X86_64.LEA_rip (scratch, funcName)
                        X86_64.MOV_store (addrReg, int32 offset, scratch)]
            | LIR.FloatSymbol value ->
                // Store float bits as 8-byte integer value at the heap offset
                let bits = System.BitConverter.DoubleToInt64Bits(value)
                if addrReg = scratch then
                    // Address is R11 - can't use scratch for the immediate value
                    Ok ([X86_64.PUSH X86_64.RCX]
                        @ loadImm64 X86_64.RCX bits
                        @ [X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                           X86_64.POP X86_64.RCX])
                else
                    Ok (loadImm64 scratch bits @ [X86_64.MOV_store (addrReg, int32 offset, scratch)])
            | LIR.StringSymbol value ->
                // Create heap string from literal, store pointer
                let strBytes = System.Text.Encoding.UTF8.GetBytes(value)
                let len = strBytes.Length
                let totalSize = ((len + 16) + 7) &&& (~~~7)
                let alloc = [X86_64.MOV_reg (scratch, heapPtr); X86_64.ADD_imm (heapPtr, int32 totalSize)]
                let storeLen = loadImm64 X86_64.RCX (int64 len) @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)]
                let copyBytes =
                    let chunks = (len + 7) / 8
                    [0 .. chunks - 1] |> List.collect (fun i ->
                        let off = 8 + i * 8
                        let chunkLen = min 8 (len - i * 8)
                        let v = [0..chunkLen-1] |> List.fold (fun acc j ->
                            let bi = i * 8 + j
                            if bi < strBytes.Length then acc ||| (int64 strBytes.[bi] <<< (j * 8)) else acc) 0L
                        loadImm64 X86_64.RCX v @ [X86_64.MOV_store (scratch, int32 off, X86_64.RCX)])
                let storeRC =
                    let rcOff = 8 + ((len + 7) &&& (~~~7))
                    loadImm64 X86_64.RCX 1L @ [X86_64.MOV_store (scratch, int32 rcOff, X86_64.RCX)]
                // The string build clobbers scratch (R11 — holds the new pointer) AND
                // uses RCX as its length/byte/refcount temp. RCX = X3 is an ALLOCATABLE
                // register (callerSaved), so it may hold a live value (e.g. a sibling
                // heap pointer being stored into an adjacent field). It MUST be preserved,
                // or that live pointer is corrupted and later dereferenced -> SIGSEGV.
                // (This was bug #26: `("a", Some 1)` stored the string at offset 0 first,
                // clobbering RCX, then stored the Some-pointer at offset 8 from the now-garbage RCX.)
                if addrReg = X86_64.RCX then
                    // Address lives in RCX; its live value IS the target address.
                    // Save it, rebuild (clobbering RCX), restore it as the address, store.
                    Ok ([X86_64.PUSH X86_64.RCX]
                        @ alloc @ storeLen @ copyBytes @ storeRC
                        @ [X86_64.POP X86_64.RCX
                           X86_64.MOV_store (X86_64.RCX, int32 offset, scratch)])
                elif addrReg = scratch then
                    // Address lives in R11, clobbered by alloc. Save the (possibly live) RCX
                    // AND the address; rebuild; borrow RCX to hold the address for the store;
                    // then restore RCX's live value.
                    Ok ([X86_64.PUSH X86_64.RCX; X86_64.PUSH scratch]
                        @ alloc @ storeLen @ copyBytes @ storeRC
                        @ [X86_64.POP X86_64.RCX
                           X86_64.MOV_store (X86_64.RCX, int32 offset, scratch)
                           X86_64.POP X86_64.RCX])
                else
                    // Address is in some other register (untouched by the build).
                    // Preserve RCX around the string construction.
                    Ok ([X86_64.PUSH X86_64.RCX]
                        @ alloc @ storeLen @ copyBytes @ storeRC
                        @ [X86_64.MOV_store (addrReg, int32 offset, scratch)
                           X86_64.POP X86_64.RCX])
            | LIR.StackSlot stackOffset ->
                let adjOff = adjustStackOffset ctx stackOffset
                if addrReg = scratch then
                    Ok ([X86_64.PUSH X86_64.RCX
                         X86_64.MOV_load (X86_64.RCX, X86_64.RBP, int32 adjOff)
                         X86_64.MOV_store (addrReg, int32 offset, X86_64.RCX)
                         X86_64.POP X86_64.RCX])
                else
                    Ok [X86_64.MOV_load (scratch, X86_64.RBP, int32 adjOff)
                        X86_64.MOV_store (addrReg, int32 offset, scratch)]
            | _ -> Error $"Unsupported HeapStore source: {src}")

    | LIR.HeapLoad (dest, addr, offset) ->
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg addr
            |> Result.map (fun addrReg ->
                [X86_64.MOV_load (destReg, addrReg, int32 offset)]))

    // --- Floating-point operations ---

    | LIR.FMov (dest, src) ->
        // Handle both physical and virtual FP registers.
        // FVirtual 2000 is used as a temp for parallel float move resolution.
        let resolveF (freg: LIR.FReg) : Result<X86_64.FReg, string> =
            match freg with
            | LIR.FPhysical fp -> Ok (lirFRegToX86 fp)
            | LIR.FVirtual 2000 -> Ok X86_64.XMM15  // Reserved parallel-move temp
            | LIR.FVirtual id -> Error $"Unresolved virtual float register f{id} in x86-64 codegen"
        resolveF dest
        |> Result.bind (fun d ->
            resolveF src
            |> Result.map (fun s ->
                if d = s then [] else [X86_64.MOVSD_reg (d, s)]))

    | LIR.FLoad (dest, value) ->
        match dest with
        | LIR.FPhysical dp ->
            let d = lirFRegToX86 dp
            // Load float immediate via GP register
            let bits = System.BitConverter.DoubleToInt64Bits(value)
            Ok (loadImm64 scratch bits @ [X86_64.MOVQ_from_gp (d, scratch)])
        | _ -> Error "FLoad with virtual FP register"

    | LIR.FAdd (dest, left, right) ->
        match dest, left, right with
        | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
            let d = lirFRegToX86 dp
            let l = lirFRegToX86 lp
            let r = lirFRegToX86 rp
            if d = r && d <> l then
                Ok [X86_64.ADDSD (d, l)]  // commutative: swap operands
            else
                let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
                Ok (setup @ [X86_64.ADDSD (d, r)])
        | _ -> Error "FAdd with virtual FP register"

    | LIR.FSub (dest, left, right) ->
        match dest, left, right with
        | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
            let d = lirFRegToX86 dp
            let l = lirFRegToX86 lp
            let r = lirFRegToX86 rp
            if d = r && d <> l then
                // NOT commutative: use XMM15 as temp
                Ok [X86_64.MOVSD_reg (X86_64.XMM15, l)
                    X86_64.SUBSD (X86_64.XMM15, r)
                    X86_64.MOVSD_reg (d, X86_64.XMM15)]
            else
                let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
                Ok (setup @ [X86_64.SUBSD (d, r)])
        | _ -> Error "FSub with virtual FP register"

    | LIR.FMul (dest, left, right) ->
        match dest, left, right with
        | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
            let d = lirFRegToX86 dp
            let l = lirFRegToX86 lp
            let r = lirFRegToX86 rp
            if d = r && d <> l then
                Ok [X86_64.MULSD (d, l)]  // commutative: swap operands
            else
                let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
                Ok (setup @ [X86_64.MULSD (d, r)])
        | _ -> Error "FMul with virtual FP register"

    | LIR.FDiv (dest, left, right) ->
        match dest, left, right with
        | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
            let d = lirFRegToX86 dp
            let l = lirFRegToX86 lp
            let r = lirFRegToX86 rp
            if d = r && d <> l then
                // NOT commutative: use XMM15 as temp
                Ok [X86_64.MOVSD_reg (X86_64.XMM15, l)
                    X86_64.DIVSD (X86_64.XMM15, r)
                    X86_64.MOVSD_reg (d, X86_64.XMM15)]
            else
                let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
                Ok (setup @ [X86_64.DIVSD (d, r)])
        | _ -> Error "FDiv with virtual FP register"

    | LIR.FNeg (dest, src) ->
        match dest, src with
        | LIR.FPhysical dp, LIR.FPhysical sp ->
            let d = lirFRegToX86 dp
            let s = lirFRegToX86 sp
            // Negate by XOR with sign bit mask
            // Load 0x8000000000000000 into scratch, move to XMM, XOR
            Ok (loadImm64 scratch (System.Int64.MinValue)
                @ [X86_64.MOVQ_from_gp (X86_64.XMM15, scratch)
                   X86_64.MOVSD_reg (d, s)
                   X86_64.XORPD (d, X86_64.XMM15)])
        | _ -> Error "FNeg with virtual FP register"

    | LIR.FAbs (dest, src) ->
        match dest, src with
        | LIR.FPhysical dp, LIR.FPhysical sp ->
            let d = lirFRegToX86 dp
            let s = lirFRegToX86 sp
            // Abs: clear sign bit using ANDPD with 0x7FFFFFFFFFFFFFFF mask.
            // We don't have ANDPD in our ISA, but we can use the GP trick:
            // 1. Move float to GP register
            // 2. AND with 0x7FFFFFFFFFFFFFFF
            // 3. Move back to float register
            // Move float bits to GP, AND with mask to clear sign bit, move back
            Ok ([X86_64.MOVQ_to_gp (scratch, s)]
                @ loadImm64 X86_64.RCX 0x7FFFFFFFFFFFFFFFL
                @ [X86_64.AND_reg (scratch, X86_64.RCX)
                   X86_64.MOVQ_from_gp (d, scratch)])
        | _ -> Error "FAbs with virtual FP register"

    | LIR.FSqrt (dest, src) ->
        match dest, src with
        | LIR.FPhysical dp, LIR.FPhysical sp ->
            Ok [X86_64.SQRTSD (lirFRegToX86 dp, lirFRegToX86 sp)]
        | _ -> Error "FSqrt with virtual FP register"

    | LIR.FCmp (left, right) ->
        lastCompWasFloat <- true
        match left, right with
        | LIR.FPhysical lp, LIR.FPhysical rp ->
            Ok [X86_64.UCOMISD (lirFRegToX86 lp, lirFRegToX86 rp)]
        | _ -> Error "FCmp with virtual FP register"

    | LIR.Int64ToFloat (dest, src) ->
        match dest with
        | LIR.FPhysical dp ->
            resolveReg src
            |> Result.map (fun srcReg -> [X86_64.CVTSI2SD (lirFRegToX86 dp, srcReg)])
        | _ -> Error "Int64ToFloat with virtual FP register"

    | LIR.FloatToInt64 (dest, src) ->
        match src with
        | LIR.FPhysical sp ->
            resolveReg dest
            |> Result.map (fun destReg -> [X86_64.CVTTSD2SI (destReg, lirFRegToX86 sp)])
        | _ -> Error "FloatToInt64 with virtual FP register"

    | LIR.GpToFp (dest, src) ->
        match dest with
        | LIR.FPhysical dp ->
            resolveReg src
            |> Result.map (fun srcReg -> [X86_64.MOVQ_from_gp (lirFRegToX86 dp, srcReg)])
        | _ -> Error "GpToFp with virtual FP register"

    | LIR.FpToGp (dest, src) ->
        match src with
        | LIR.FPhysical sp ->
            resolveReg dest
            |> Result.map (fun destReg -> [X86_64.MOVQ_to_gp (destReg, lirFRegToX86 sp)])
        | _ -> Error "FpToGp with virtual FP register"

    | LIR.FloatToBits (dest, src) ->
        match src with
        | LIR.FPhysical sp ->
            resolveReg dest
            |> Result.map (fun destReg -> [X86_64.MOVQ_to_gp (destReg, lirFRegToX86 sp)])
        | _ -> Error "FloatToBits with virtual FP register"

    | LIR.RefCountInc (addr, payloadSize, kind) ->
        resolveReg addr
        |> Result.map (fun addrReg ->
            match kind with
            | LIR.TaggedList ->
                // No-op until RefCountDec is enabled (see notes there).
                []
            | _ ->
                // Generic refcount inc — no-op for now (needs more testing)
                [])

    | LIR.RefCountDec (addr, payloadSize, kind) ->
        resolveReg addr
        |> Result.map (fun addrReg ->
            match kind with
            | LIR.TaggedList ->
                // TaggedList RefCountDec: calls the recursive FingerTree DFS helper.
                // NOTE: Currently disabled. When enabled, this frees nodes whose refcounts
                // reach 0. But this REQUIRES the RawSet ownership increment (above) to be
                // active, otherwise child nodes are freed while still referenced by parents.
                // Enable both together once the 37-test regression is fixed.
                []
                // When ready to enable:
                // let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10; scratch]
                // let saves = saveRegs |> List.map X86_64.PUSH
                // let restores = saveRegs |> List.rev |> List.map X86_64.POP
                // saves @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL listRefCountDecHelperLabel] @ restores
            | _ ->
                // Generic refcount dec — no-op for now (payloadSize mismatch risk)
                [])

    | LIR.RefCountIncString str ->
        match str with
        | LIR.StringSymbol _ -> Ok []  // Literal string - no refcount
        | LIR.Reg reg ->
            resolveReg reg
            |> Result.map (fun addrReg ->
                // Heap string: [length:8][data:N][padding:P][refcount:8]
                // refcount offset = 8 + ((length + 7) & ~7)
                // Sentinel value INT64_MAX means literal (read-only)
                let skipLabel = freshLabel "rcinc_str_skip"
                let literalLabel = freshLabel "rcinc_str_lit"
                [X86_64.PUSH X86_64.RCX
                 X86_64.PUSH X86_64.RDX
                 // Compute refcount address
                 X86_64.MOV_load (X86_64.RCX, addrReg, 0)    // RCX = length
                 X86_64.ADD_imm (X86_64.RCX, 7)               // RCX = length + 7
                 X86_64.AND_imm (X86_64.RCX, -8)              // RCX = aligned(length)
                 X86_64.ADD_imm (X86_64.RCX, 8)               // RCX = 8 + aligned(length)
                 X86_64.ADD_reg (X86_64.RCX, addrReg)         // RCX = addr + refcount offset
                 // Load refcount, check sentinel
                 X86_64.MOV_load (X86_64.RDX, X86_64.RCX, 0) // RDX = refcount
                ]
                @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL        // scratch = INT64_MAX
                @ [X86_64.CMP_reg (X86_64.RDX, scratch)
                   X86_64.Jcc (X86_64.EQ, literalLabel)        // skip if literal
                   X86_64.ADD_imm (X86_64.RDX, 1)
                   X86_64.MOV_store (X86_64.RCX, 0, X86_64.RDX)
                   X86_64.Label literalLabel
                   X86_64.POP X86_64.RDX
                   X86_64.POP X86_64.RCX
                   X86_64.Label skipLabel])
        | _ -> Error "RefCountIncString requires StringSymbol or Reg operand"

    | LIR.RefCountDecString str ->
        match str with
        | LIR.StringSymbol _ -> Ok []  // Literal string - no refcount
        | LIR.Reg reg ->
            resolveReg reg
            |> Result.map (fun addrReg ->
                let skipLabel = freshLabel "rcdec_str_skip"
                let literalLabel = freshLabel "rcdec_str_lit"
                let noFreeLabel = freshLabel "rcdec_str_nofree"
                let leakDec = genLeakCounterDec ctx
                [X86_64.PUSH X86_64.RCX
                 X86_64.PUSH X86_64.RDX
                 // Compute refcount address
                 X86_64.MOV_load (X86_64.RCX, addrReg, 0)    // RCX = length
                 X86_64.ADD_imm (X86_64.RCX, 7)
                 X86_64.AND_imm (X86_64.RCX, -8)
                 X86_64.ADD_imm (X86_64.RCX, 8)
                 X86_64.ADD_reg (X86_64.RCX, addrReg)         // RCX = refcount addr
                 X86_64.MOV_load (X86_64.RDX, X86_64.RCX, 0) // RDX = refcount
                ]
                @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
                @ [X86_64.CMP_reg (X86_64.RDX, scratch)
                   X86_64.Jcc (X86_64.EQ, literalLabel)
                   X86_64.SUB_imm (X86_64.RDX, 1)
                   X86_64.MOV_store (X86_64.RCX, 0, X86_64.RDX)
                   X86_64.TEST_reg (X86_64.RDX, X86_64.RDX)
                   X86_64.Jcc (X86_64.NE, noFreeLabel)]
                // String refcount hit zero - decrement leak counter
                @ leakDec
                @ [X86_64.Label noFreeLabel
                   X86_64.Label literalLabel
                   X86_64.POP X86_64.RDX
                   X86_64.POP X86_64.RCX
                   X86_64.Label skipLabel])
        | _ -> Error "RefCountDecString requires StringSymbol or Reg operand"

    | LIR.StringConcat (dest, left, right) ->
        // String concat: dest = left ++ right
        // Heap string: [length:8][data:N][refcount:8]
        // Strategy: load both strings' info, allocate result, copy bytes with loops.
        // Register plan (no PUSH/POP in loops):
        //   RDI = left data ptr, RSI = left len
        //   R8  = right data ptr, R9 = right len
        //   R10 = loop counter, R11(scratch) = temp byte
        //   destReg = result ptr, RCX = dest write ptr
        //
        // IMPORTANT: This operation clobbers RDI, RSI, RCX, R8, R9, R10.
        // Save/restore all caller-saved registers except those used as operands,
        // since the register allocator doesn't model these clobbers.
        resolveReg dest
        |> Result.bind (fun destReg ->
            // Clobbered registers (RDI=X1, RSI=X2, RCX=X3, R8=X4, R9=X5, R10=X6)
            // Save all except the dest reg (caller may still need operand regs after this)
            let clobbered = [X86_64.RDI; X86_64.RSI; X86_64.RCX; X86_64.R8; X86_64.R9; X86_64.R10]
            let toSave = clobbered |> List.filter (fun r -> r <> destReg)
            let saveInstrs = toSave |> List.map (fun r -> X86_64.PUSH r)
            let restoreInstrs = toSave |> List.rev |> List.map (fun r -> X86_64.POP r)

            let loadInfo (op: LIR.Operand) (addrDest: X86_64.Reg) (lenDest: X86_64.Reg) : Result<X86_64.Instr list, string> =
                match op with
                | LIR.Reg reg ->
                    resolveReg reg
                    |> Result.map (fun srcReg ->
                        if srcReg = lenDest then
                            // srcReg == lenDest: LEA first so MOV_load doesn't clobber pointer
                            [X86_64.LEA (addrDest, srcReg, 8)
                             X86_64.MOV_load (lenDest, srcReg, 0)]
                        elif srcReg = addrDest then
                            // srcReg == addrDest: save pointer in scratch before LEA clobbers it
                            [X86_64.MOV_reg (scratch, srcReg)
                             X86_64.MOV_load (lenDest, srcReg, 0)
                             X86_64.LEA (addrDest, scratch, 8)]
                        else
                            [X86_64.MOV_load (lenDest, srcReg, 0)
                             X86_64.LEA (addrDest, srcReg, 8)])
                | LIR.StringSymbol value ->
                    let len = System.Text.Encoding.UTF8.GetByteCount(value)
                    let instrs = emitStringLiteralNoRefCount addrDest value
                    let setResults = loadImm64 lenDest (int64 len) @ [X86_64.LEA (addrDest, addrDest, 8)]
                    Ok (instrs @ setResults)
                | _ -> Ok (loadImm64 lenDest 0L @ loadImm64 addrDest 0L)

            let copy1 = freshLabel "strcat_c1"
            let done1 = freshLabel "strcat_d1"
            let copy2 = freshLabel "strcat_c2"
            let done2 = freshLabel "strcat_d2"

            // Load RIGHT first (if Reg, no allocation needed), then LEFT
            // (which might allocate for StringSymbol). This avoids clobbering
            // the right source register during left's heap allocation.
            //
            // BUG FIX: If left is in R8 or R9, loading right will clobber left's
            // register. Save left to scratch (R11) first, then load from scratch.
            let leftConflictReg =
                match left with
                | LIR.Reg reg ->
                    match resolveReg reg with
                    | Ok r when r = X86_64.R8 || r = X86_64.R9 -> Some r
                    | _ -> None
                | _ -> None

            loadInfo right X86_64.R8 X86_64.R9
            |> Result.bind (fun rightInstrs ->
                // Save right info before loading left (left might clobber R8/R9)
                let saveRight = [X86_64.PUSH X86_64.R8; X86_64.PUSH X86_64.R9]

                let preserveLeft =
                    match leftConflictReg with
                    | Some r -> [X86_64.MOV_reg (scratch, r)]
                    | None -> []

                let loadLeft =
                    match leftConflictReg with
                    | Some _ ->
                        // Left was saved in scratch before right clobbered its register
                        Ok [X86_64.MOV_load (X86_64.RSI, scratch, 0)
                            X86_64.LEA (X86_64.RDI, scratch, 8)]
                    | None ->
                        loadInfo left X86_64.RDI X86_64.RSI

                loadLeft
                |> Result.map (fun leftInstrs ->
                    saveInstrs
                    @ preserveLeft
                    @ rightInstrs @ saveRight @ leftInstrs
                    // Restore right info
                    @ [X86_64.POP X86_64.R9; X86_64.POP X86_64.R8]

                    // Total length in RCX
                    @ [X86_64.MOV_reg (X86_64.RCX, X86_64.RSI)
                       X86_64.ADD_reg (X86_64.RCX, X86_64.R9)]

                    // Allocate: use RBX to hold result ptr (callee-saved, safe across loops)
                    // Save RBX first
                    @ [X86_64.PUSH X86_64.RBX]
                    @ [X86_64.MOV_reg (X86_64.RBX, heapPtr)
                       X86_64.MOV_reg (X86_64.R10, X86_64.RCX)
                       X86_64.ADD_imm (X86_64.R10, 23)
                       X86_64.AND_imm (X86_64.R10, -8)
                       X86_64.ADD_reg (heapPtr, X86_64.R10)]

                    // Store total length at [RBX]
                    @ [X86_64.MOV_store (X86_64.RBX, 0, X86_64.RCX)]

                    // Copy left bytes: RBX[8+i] = left[i]
                    @ loadImm64 X86_64.R10 0L
                    @ [X86_64.Label copy1
                       X86_64.CMP_reg (X86_64.R10, X86_64.RSI)
                       X86_64.Jcc (X86_64.GE, done1)
                       X86_64.MOV_reg (scratch, X86_64.RDI)
                       X86_64.ADD_reg (scratch, X86_64.R10)
                       X86_64.MOV_load_byte (scratch, scratch, 0)
                       X86_64.LEA (X86_64.RCX, X86_64.RBX, 8)
                       X86_64.ADD_reg (X86_64.RCX, X86_64.R10)
                       X86_64.MOV_store_byte (X86_64.RCX, 0, scratch)
                       X86_64.ADD_imm (X86_64.R10, 1)
                       X86_64.JMP copy1
                       X86_64.Label done1]

                    // Copy right bytes: RBX[8+leftLen+i] = right[i]
                    @ [X86_64.LEA (X86_64.RCX, X86_64.RBX, 8)
                       X86_64.ADD_reg (X86_64.RCX, X86_64.RSI)]
                    @ loadImm64 X86_64.R10 0L
                    @ [X86_64.Label copy2
                       X86_64.CMP_reg (X86_64.R10, X86_64.R9)
                       X86_64.Jcc (X86_64.GE, done2)
                       X86_64.MOV_reg (scratch, X86_64.R8)
                       X86_64.ADD_reg (scratch, X86_64.R10)
                       X86_64.MOV_load_byte (scratch, scratch, 0)
                       X86_64.MOV_reg (X86_64.RDI, X86_64.RCX)
                       X86_64.ADD_reg (X86_64.RDI, X86_64.R10)
                       X86_64.MOV_store_byte (X86_64.RDI, 0, scratch)
                       X86_64.ADD_imm (X86_64.R10, 1)
                       X86_64.JMP copy2
                       X86_64.Label done2]

                    // Store refcount = 1
                    @ [X86_64.MOV_load (X86_64.RCX, X86_64.RBX, 0)
                       X86_64.ADD_imm (X86_64.RCX, 8 + 7)
                       X86_64.AND_imm (X86_64.RCX, -8)
                       X86_64.ADD_reg (X86_64.RCX, X86_64.RBX)]
                    @ loadImm64 scratch 1L
                    @ [X86_64.MOV_store (X86_64.RCX, 0, scratch)]
                    // Leak counter increment for string allocation
                    @ genLeakCounterInc ctx
                    // Move result to destReg, restore RBX
                    // If destReg IS RBX, we need to save result elsewhere first
                    @ (if destReg = X86_64.RBX then
                           // Result is already in RBX. Pop saved RBX to scratch, keep result.
                           [X86_64.ADD_imm (X86_64.RSP, 8)]  // discard saved RBX
                       else
                           [X86_64.MOV_reg (destReg, X86_64.RBX)
                            X86_64.POP X86_64.RBX])
                    @ restoreInstrs)))

    | LIR.CoverageHit _ ->
        Ok []  // Coverage instrumentation not supported on x86_64 yet

    | LIR.Lsl (dest, src, shift) ->
        // SHL by register: shift amount must be in CL (lower byte of RCX)
        // Save/restore RCX if it's not the shift operand or dest (clobber not modeled by regalloc)
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.bind (fun s ->
            resolveReg shift |> Result.map (fun shReg ->
                let needSaveRCX = shReg <> X86_64.RCX && d <> X86_64.RCX
                let save = if needSaveRCX then [X86_64.PUSH X86_64.RCX] else []
                let restore = if needSaveRCX then [X86_64.POP X86_64.RCX] else []
                if d = shReg && d <> s then
                    // dest == shift register: moving src to dest would clobber shift.
                    // Use scratch to save src, then move shift to RCX, then put src in dest.
                    // This handles the case where s = RCX (which MOV RCX,shReg would clobber).
                    save
                    @ [X86_64.MOV_reg (scratch, s)]
                    @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                    @ [X86_64.MOV_reg (d, scratch)]
                    @ [X86_64.SHL_cl d]
                    @ restore
                elif d = X86_64.RCX && shReg <> X86_64.RCX then
                    // dest is RCX: MOV d,s then MOV RCX,shReg would clobber src in d.
                    // Use scratch to hold value, shift there, move result back.
                    [X86_64.MOV_reg (scratch, s)
                     X86_64.MOV_reg (X86_64.RCX, shReg)
                     X86_64.SHL_cl scratch
                     X86_64.MOV_reg (X86_64.RCX, scratch)]
                else
                    save
                    @ (if d <> s then [X86_64.MOV_reg (d, s)] else [])
                    @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                    @ [X86_64.SHL_cl d]
                    @ restore)))

    | LIR.Lsr (dest, src, shift) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.bind (fun s ->
            resolveReg shift |> Result.map (fun shReg ->
                let needSaveRCX = shReg <> X86_64.RCX && d <> X86_64.RCX
                let save = if needSaveRCX then [X86_64.PUSH X86_64.RCX] else []
                let restore = if needSaveRCX then [X86_64.POP X86_64.RCX] else []
                if d = shReg && d <> s then
                    // dest == shift: save src via scratch to avoid clobbering when s=RCX
                    save
                    @ [X86_64.MOV_reg (scratch, s)]
                    @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                    @ [X86_64.MOV_reg (d, scratch)]
                    @ [X86_64.SHR_cl d]
                    @ restore
                elif d = X86_64.RCX && shReg <> X86_64.RCX then
                    // dest is RCX: use scratch to avoid clobbering
                    [X86_64.MOV_reg (scratch, s)
                     X86_64.MOV_reg (X86_64.RCX, shReg)
                     X86_64.SHR_cl scratch
                     X86_64.MOV_reg (X86_64.RCX, scratch)]
                else
                    save
                    @ (if d <> s then [X86_64.MOV_reg (d, s)] else [])
                    @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                    @ [X86_64.SHR_cl d]
                    @ restore)))

    | LIR.Uxth (dest, src) ->
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            [X86_64.MOVZX_word (d, s)]))

    | LIR.Uxtw (dest, src) ->
        // 32-bit MOV zero-extends to 64-bit on x86_64
        resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
            [X86_64.MOV_reg32 (d, s)]))

    | LIR.ClosureAlloc (dest, funcName, captures) ->
        // Allocate closure on heap: [func_ptr, cap1, cap2, ...][refcount]
        resolveReg dest
        |> Result.bind (fun destReg ->
            let numSlots = 1 + List.length captures
            let sizeBytes = numSlots * 8
            let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)  // + refcount, aligned
            let alloc = [
                X86_64.MOV_reg (destReg, heapPtr)
                X86_64.ADD_imm (heapPtr, int32 totalSize)
            ]
            // Store refcount = 1
            let storeRC =
                loadImm64 scratch 1L
                @ [X86_64.MOV_store (destReg, int32 sizeBytes, scratch)]
            // Store function address at offset 0
            let storeFunc = [
                X86_64.LEA_rip (scratch, funcName)
                X86_64.MOV_store (destReg, 0, scratch)
            ]
            // Store captures
            let storeCaptures =
                captures
                |> List.mapi (fun i cap -> (i, cap))
                |> List.collect (fun (i, cap) ->
                    let offset = (i + 1) * 8
                    match cap with
                    | LIR.Imm value ->
                        loadImm64 scratch value
                        @ [X86_64.MOV_store (destReg, int32 offset, scratch)]
                    | LIR.Reg reg ->
                        match resolveReg reg with
                        | Ok srcReg -> [X86_64.MOV_store (destReg, int32 offset, srcReg)]
                        | Error _ -> []
                    | LIR.StackSlot stackOffset ->
                        let adjOff = adjustStackOffset ctx stackOffset
                        [X86_64.MOV_load (scratch, X86_64.RBP, int32 adjOff)
                         X86_64.MOV_store (destReg, int32 offset, scratch)]
                    | _ -> [])
            Ok (alloc @ storeRC @ storeFunc @ storeCaptures))

    | LIR.ClosureCall (dest, closure, _args) ->
        // The closure register contains the function pointer
        // (LIR does HeapLoad to extract func_ptr before ClosureCall)
        resolveReg closure
        |> Result.bind (fun closureReg ->
            resolveReg dest
            |> Result.map (fun destReg ->
                // Move to R10 if in scratch (R11) to avoid conflicts
                let callReg = if closureReg = scratch then X86_64.R10 else closureReg
                let setup = if callReg <> closureReg then [X86_64.MOV_reg (callReg, closureReg)] else []
                setup
                @ [X86_64.CALL_reg callReg]
                @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else [])))

    | LIR.ClosureTailCall (closure, _args) ->
        resolveReg closure
        |> Result.map (fun closureReg ->
            let callReg = if closureReg = scratch then X86_64.R10 else closureReg
            let setup = if callReg <> closureReg then [X86_64.MOV_reg (callReg, closureReg)] else []
            setup
            @ genEpilogue ctx.StackSize ctx.UsedCalleeSaved
            @ [X86_64.JMP_reg callReg])

    | LIR.RawAlloc (dest, numBytes) ->
        let okLabel = freshLabel "rawalloc_ok"
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg numBytes
            |> Result.map (fun sizeReg ->
                // --- Free list reuse ---
                // Before bump-allocating, check if the free list has a block of the right size class.
                // aligned_size = (numBytes + 7) & ~7; payload_class = aligned_size - 8
                // If freeList[payload_class] is non-null, pop from free list and skip bump alloc.
                let bumpLabel = freshLabel "rawalloc_bump"
                let doneLabel = freshLabel "rawalloc_done"
                // Pick two temp registers that don't conflict with destReg or sizeReg
                let candidates = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI]
                let available = candidates |> List.filter (fun r -> r <> destReg && r <> sizeReg)
                let temp1 = available.[0]  // holds aligned_size → payload_class → head → next
                let temp2 = available.[1]  // holds &freeList[payload_class]
                let freeListCheck =
                    [X86_64.PUSH temp1
                     X86_64.PUSH temp2
                     // Compute aligned size
                     X86_64.MOV_reg (temp1, sizeReg)
                     X86_64.ADD_imm (temp1, 7)
                     X86_64.AND_imm (temp1, -8)         // temp1 = aligned_size
                     // Need at least 16 bytes (8 payload + 8 refcount) for free list
                     X86_64.CMP_imm (temp1, 8)
                     X86_64.Jcc (X86_64.LE, bumpLabel)
                     X86_64.SUB_imm (temp1, 8)           // temp1 = payload_class
                     X86_64.CMP_imm (temp1, maxFreeListPayload)
                     X86_64.Jcc (X86_64.GT, bumpLabel)
                     // Compute free list slot address: &freeList[payload_class]
                     X86_64.MOV_reg (temp2, freeListBase)
                     X86_64.ADD_reg (temp2, temp1)       // temp2 = &freeList[payload_class]
                     // Load free list head
                     X86_64.MOV_load (temp1, temp2, 0)   // temp1 = head
                     X86_64.TEST_reg (temp1, temp1)
                     X86_64.Jcc (X86_64.EQ, bumpLabel)
                     // Pop from free list: dest = head, freeList[class] = head->next
                     X86_64.MOV_reg (destReg, temp1)     // dest = free block
                     X86_64.MOV_load (temp1, temp1, 0)   // temp1 = next ptr
                     X86_64.MOV_store (temp2, 0, temp1)  // update head
                     X86_64.POP temp2
                     X86_64.POP temp1
                     X86_64.JMP doneLabel
                     X86_64.Label bumpLabel
                     X86_64.POP temp2
                     X86_64.POP temp1]
                // --- Bump allocation (existing) ---
                let allocInstrs =
                    if destReg = sizeReg then
                        [X86_64.MOV_reg (scratch, sizeReg)
                         X86_64.MOV_reg (destReg, heapPtr)
                         X86_64.ADD_reg (heapPtr, scratch)
                         X86_64.ADD_imm (heapPtr, 7)
                         X86_64.AND_imm (heapPtr, -8)]
                    else
                        [X86_64.MOV_reg (destReg, heapPtr)
                         X86_64.ADD_reg (heapPtr, sizeReg)
                         X86_64.ADD_imm (heapPtr, 7)
                         X86_64.AND_imm (heapPtr, -8)]
                // Bounds check: heapPtr - freeListBase <= heapMmapSize
                // Use RAX temp if dest or size uses scratch (R11)
                let useScratch = destReg <> scratch && sizeReg <> scratch
                let boundsCheck =
                    if useScratch then
                        [X86_64.MOV_reg (scratch, heapPtr)
                         X86_64.SUB_reg (scratch, freeListBase)
                         X86_64.CMP_imm (scratch, int32 heapMmapSizeBytes)
                         X86_64.Jcc (X86_64.LE, okLabel)]
                    else
                        [X86_64.PUSH X86_64.RAX
                         X86_64.MOV_reg (X86_64.RAX, heapPtr)
                         X86_64.SUB_reg (X86_64.RAX, freeListBase)
                         X86_64.CMP_imm (X86_64.RAX, int32 heapMmapSizeBytes)
                         X86_64.POP X86_64.RAX
                         X86_64.Jcc (X86_64.LE, okLabel)]
                    @ genOomJump ()
                    @ [X86_64.Label okLabel]
                // NOTE: genLeakCounterInc should be added here when RefCountDec is enabled,
                // so that alloc/free counts balance for the leak checker.
                freeListCheck @ allocInstrs @ boundsCheck @ [X86_64.Label doneLabel]))

    | LIR.RawFree _ ->
        Ok []  // No-op (no free in bump allocator)

    | LIR.RawGet (dest, ptr, byteOffset) ->
        resolveReg dest |> Result.bind (fun d ->
            resolveReg ptr |> Result.bind (fun p ->
                resolveReg byteOffset |> Result.map (fun o ->
                    if o = scratch && p <> scratch then
                        // Offset is R11: MOV scratch,p would clobber offset.
                        // Swap: compute p + o by loading o first, adding p.
                        [X86_64.ADD_reg (scratch, p)
                         X86_64.MOV_load (d, scratch, 0)]
                    elif p = scratch && o <> scratch then
                        // Ptr is R11: MOV is no-op, just add offset
                        [X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_load (d, scratch, 0)]
                    elif p = scratch && o = scratch then
                        // Both are R11 (same virtual reg): scratch = scratch + scratch
                        [X86_64.ADD_reg (scratch, scratch)
                         X86_64.MOV_load (d, scratch, 0)]
                    else
                        [X86_64.MOV_reg (scratch, p)
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_load (d, scratch, 0)])))

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        resolveReg dest |> Result.bind (fun d ->
            resolveReg ptr |> Result.bind (fun p ->
                resolveReg byteOffset |> Result.map (fun o ->
                    if o = scratch && p <> scratch then
                        [X86_64.ADD_reg (scratch, p)
                         X86_64.MOV_load_byte (d, scratch, 0)]
                    elif p = scratch then
                        [X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_load_byte (d, scratch, 0)]
                    else
                        [X86_64.MOV_reg (scratch, p)
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_load_byte (d, scratch, 0)])))

    | LIR.RawSet (ptr, byteOffset, value, valueType) ->
        resolveReg ptr |> Result.bind (fun p ->
            resolveReg byteOffset |> Result.bind (fun o ->
                resolveReg value |> Result.map (fun v ->
                    // Ownership increment: when storing a tagged list pointer into a node,
                    // increment the stored value's refcount (the parent now owns that edge).
                    // Without this, RefCountDec for child nodes will free them even though
                    // the parent still references them.
                    // NOTE: Currently disabled — enabling it causes 37 test regressions
                    // (crypto SIGSEGVs, tco-refcounting leak counter mismatch). See
                    // docs/x64-refcounting.md.
                    let ownershipInc : X86_64.Instr list =
                        ignore valueType
                        []

                    let storeInstrs =
                        if v = scratch || o = scratch then
                            // An operand is R11 (scratch) which we need for address computation.
                            // Use RCX as an extra temp (save/restore if needed).
                            let tempReg = X86_64.RCX
                            if p = tempReg then
                                // ptr is RCX: can't use RCX as temp without saving ptr first.
                                // Use two pushes: save ptr, save value, compute address, store.
                                [X86_64.PUSH tempReg             // save ptr (RCX)
                                 X86_64.PUSH scratch              // save value/offset (R11)
                                 // Stack: [R11] [RCX] ...
                                 // Compute address: R11 = ptr + offset
                                 X86_64.MOV_load (scratch, X86_64.RSP, 8) // R11 = saved ptr (RCX)
                                 X86_64.ADD_reg (scratch, o)      // R11 = ptr + offset
                                 // Get value
                                 X86_64.MOV_load (tempReg, X86_64.RSP, 0) // RCX = saved R11 (value)
                                 X86_64.MOV_store (scratch, 0, tempReg) // [addr] = value
                                 X86_64.POP scratch               // restore R11
                                 X86_64.POP tempReg]              // restore RCX
                            else
                                [X86_64.PUSH tempReg
                                 X86_64.MOV_reg (tempReg, v)   // save value in temp
                                 X86_64.MOV_reg (scratch, p)
                                 X86_64.ADD_reg (scratch, o)
                                 X86_64.MOV_store (scratch, 0, tempReg)
                                 X86_64.POP tempReg]
                        else
                            [X86_64.MOV_reg (scratch, p)
                             X86_64.ADD_reg (scratch, o)
                             X86_64.MOV_store (scratch, 0, v)]

                    ownershipInc @ storeInstrs)))

    | LIR.RawSetByte (ptr, byteOffset, value) ->
        resolveReg ptr |> Result.bind (fun p ->
            resolveReg byteOffset |> Result.bind (fun o ->
                resolveReg value |> Result.map (fun v ->
                    if v = scratch || o = scratch then
                        let tempReg = X86_64.RCX
                        if p = tempReg then
                            // ptr is RCX: save both before clobbering
                            [X86_64.PUSH tempReg
                             X86_64.PUSH scratch
                             X86_64.MOV_load (scratch, X86_64.RSP, 8)  // R11 = saved ptr
                             X86_64.ADD_reg (scratch, o)
                             X86_64.MOV_load (tempReg, X86_64.RSP, 0)  // RCX = saved value/offset
                             X86_64.MOV_store_byte (scratch, 0, tempReg)
                             X86_64.POP scratch
                             X86_64.POP tempReg]
                        else
                            [X86_64.PUSH tempReg
                             X86_64.MOV_reg (tempReg, v)
                             X86_64.MOV_reg (scratch, p)
                             X86_64.ADD_reg (scratch, o)
                             X86_64.MOV_store_byte (scratch, 0, tempReg)
                             X86_64.POP tempReg]
                    else
                        [X86_64.MOV_reg (scratch, p)
                         X86_64.ADD_reg (scratch, o)
                         X86_64.MOV_store_byte (scratch, 0, v)])))

    | LIR.RandomInt64 dest ->
        // getrandom(buf, 8, 0) syscall
        resolveReg dest
        |> Result.map (fun destReg ->
            [X86_64.SUB_imm (X86_64.RSP, 8)]
            @ [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]  // buf
            @ loadImm64 X86_64.RSI 8L                      // len = 8
            @ loadImm64 X86_64.RDX 0L                      // flags = 0
            @ loadImm64 X86_64.RAX (int64 syscalls.Getrandom)
            @ [X86_64.SYSCALL
               X86_64.MOV_load (destReg, X86_64.RSP, 0)
               X86_64.ADD_imm (X86_64.RSP, 8)])

    | LIR.DateNow dest ->
        // clock_gettime(CLOCK_REALTIME=0, &ts) → ts.tv_sec * 1000000 + ts.tv_nsec / 1000
        resolveReg dest
        |> Result.map (fun destReg ->
            [X86_64.SUB_imm (X86_64.RSP, 16)]  // timespec: tv_sec(8) + tv_nsec(8)
            @ loadImm64 X86_64.RDI 0L           // CLOCK_REALTIME
            @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]
            @ loadImm64 X86_64.RAX (int64 syscalls.Gettimeofday)
            @ [X86_64.SYSCALL
               X86_64.MOV_load (destReg, X86_64.RSP, 0)  // tv_sec
               X86_64.ADD_imm (X86_64.RSP, 16)])

    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        // dest = add + mulLeft * mulRight
        resolveReg dest |> Result.bind (fun d ->
            resolveReg mulLeft |> Result.bind (fun ml ->
                resolveReg mulRight |> Result.bind (fun mr ->
                    resolveReg add |> Result.map (fun addReg ->
                        [X86_64.MOV_reg (scratch, ml); X86_64.IMUL_reg (scratch, mr)]
                        @ (if d <> addReg then [X86_64.MOV_reg (d, addReg)] else [])
                        @ [X86_64.ADD_reg (d, scratch)]))))

    | LIR.PrintFloat freg ->
        // Call Stdlib.Float.toString(D0), print result as heap string
        match freg with
        | LIR.FPhysical fp ->
            let xmm = lirFRegToX86 fp
            Ok ((if xmm <> X86_64.XMM0 then [X86_64.MOVSD_reg (X86_64.XMM0, xmm)] else [])
                @ [X86_64.CALL "Stdlib.Float.toString"]
                @ [X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0)
                   X86_64.LEA (X86_64.RSI, X86_64.RAX, 8)
                   X86_64.MOV_imm32 (X86_64.RDI, 1)]
                @ genWriteSyscall
                @ [X86_64.SUB_imm (X86_64.RSP, 8)]
                @ loadImm64 scratch 10L
                @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
                   X86_64.MOV_imm32 (X86_64.RDI, 1)
                   X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
                   X86_64.MOV_imm32 (X86_64.RDX, 1)]
                @ genWriteSyscall
                @ [X86_64.ADD_imm (X86_64.RSP, 8)])
        | _ -> Error "PrintFloat with virtual FP register"

    | LIR.PrintFloatNoNewline freg ->
        // Call Stdlib.Float.toString(D0), print result without newline
        match freg with
        | LIR.FPhysical fp ->
            let xmm = lirFRegToX86 fp
            Ok ((if xmm <> X86_64.XMM0 then [X86_64.MOVSD_reg (X86_64.XMM0, xmm)] else [])
                @ [X86_64.CALL "Stdlib.Float.toString"]
                @ [X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0)
                   X86_64.LEA (X86_64.RSI, X86_64.RAX, 8)
                   X86_64.MOV_imm32 (X86_64.RDI, 1)]
                @ genWriteSyscall)
        | _ -> Error "PrintFloatNoNewline with virtual FP register"

    | LIR.FloatToString (dest, src) ->
        // Call Stdlib.Float.toString(D0)
        match src with
        | LIR.FPhysical fp ->
            let xmm = lirFRegToX86 fp
            resolveReg dest
            |> Result.map (fun destReg ->
                (if xmm <> X86_64.XMM0 then [X86_64.MOVSD_reg (X86_64.XMM0, xmm)] else [])
                @ [X86_64.CALL "Stdlib.Float.toString"]
                @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else []))
        | _ -> Error "FloatToString with virtual FP register"

    | LIR.PrintList (listPtr, _elemType) ->
        // TODO: implement list printing
        resolveReg listPtr
        |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)

    | LIR.PrintSum (sumPtr, _variants) ->
        resolveReg sumPtr
        |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)

    | LIR.PrintRecord (recordPtr, _typeName, _fields) ->
        resolveReg recordPtr
        |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)

    | LIR.PrintBytes reg ->
        resolveReg reg
        |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)

    | LIR.FileReadText (dest, path) ->
        // File read: open → fstat → alloc → read → close → Result
        resolveReg dest
        |> Result.bind (fun destReg ->
            let resolvePathToR10 =
                match path with
                | LIR.Reg reg ->
                    resolveReg reg |> Result.map (fun srcReg ->
                        if srcReg = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, srcReg)])
                | LIR.StackSlot offset ->
                    Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
                | LIR.StringSymbol value ->
                    Ok (emitStringLiteralNoRefCount X86_64.R10 value)
                | _ -> Ok (loadImm64 X86_64.R10 0L)
            let copyLabel = freshLabel "fr_copy"
            let doneLabel = freshLabel "fr_done"
            let errorLabel = freshLabel "fr_err"
            let cleanupLabel = freshLabel "fr_clean"
            let openSyscall = int64 syscalls.Open
            let fstatSyscall = int64 syscalls.Fstat
            let readSyscall = int64 syscalls.Read
            let closeSyscall = int64 syscalls.Close
            resolvePathToR10 |> Result.map (fun pathSetup ->
                // Save registers that syscalls will clobber
                let saves = [X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX
                             X86_64.PUSH X86_64.R10; X86_64.PUSH X86_64.R8; X86_64.PUSH X86_64.R9]
                let restores = [X86_64.POP X86_64.R9; X86_64.POP X86_64.R8; X86_64.POP X86_64.R10
                                X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI; X86_64.POP X86_64.RDI]
                pathSetup @ saves
                // Allocate stack: 4096 bytes for path (PATH_MAX) + 144 bytes for stat buf = 4240
                @ [X86_64.SUB_imm (X86_64.RSP, 4240)]
                // Copy heap string to null-terminated C string on stack
                // R10 = heap string ptr, [R10] = length, R10+8 = data
                @ [X86_64.MOV_load (X86_64.RCX, X86_64.R10, 0)    // RCX = length
                   X86_64.LEA (X86_64.RSI, X86_64.R10, 8)          // RSI = data ptr
                   X86_64.LEA (X86_64.RDI, X86_64.RSP, 144)]       // RDI = stack buf (after stat buf)
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.Label copyLabel
                   X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                   X86_64.Jcc (X86_64.GE, doneLabel)
                   X86_64.MOV_reg (scratch, X86_64.RSI)
                   X86_64.ADD_reg (scratch, X86_64.R10)
                   X86_64.MOV_load_byte (scratch, scratch, 0)
                   X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
                   X86_64.ADD_reg (X86_64.R8, X86_64.R10)
                   X86_64.MOV_store_byte (X86_64.R8, 0, scratch)
                   X86_64.ADD_imm (X86_64.R10, 1)
                   X86_64.JMP copyLabel
                   X86_64.Label doneLabel]
                // Null-terminate
                @ [X86_64.MOV_reg (scratch, X86_64.RDI)
                   X86_64.ADD_reg (scratch, X86_64.RCX)]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.MOV_store_byte (scratch, 0, X86_64.R10)]
                // open(path, O_RDONLY=0, 0) → fd
                @ [X86_64.LEA (X86_64.RDI, X86_64.RSP, 144)]   // path on stack
                @ loadImm64 X86_64.RSI 0L                        // O_RDONLY
                @ loadImm64 X86_64.RDX 0L                        // mode
                @ loadImm64 X86_64.RAX openSyscall
                @ [X86_64.SYSCALL]
                // Check if open failed (RAX < 0)
                @ [X86_64.CMP_imm (X86_64.RAX, 0)
                   X86_64.Jcc (X86_64.LT, errorLabel)]
                // Save fd in R8
                @ [X86_64.MOV_reg (X86_64.R8, X86_64.RAX)]
                // fstat(fd, stat_buf) → get file size
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]      // fd
                @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]      // stat buf at RSP
                @ loadImm64 X86_64.RAX fstatSyscall
                @ [X86_64.SYSCALL]
                // File size is at offset 48 in stat struct (x86_64 Linux)
                @ [X86_64.MOV_load (X86_64.R9, X86_64.RSP, 48)]  // R9 = file size
                // Allocate heap string: [len:8][data:N][refcount:8]
                @ [X86_64.MOV_reg (X86_64.R10, heapPtr)]          // R10 = string ptr
                @ [X86_64.MOV_reg (scratch, X86_64.R9)
                   X86_64.ADD_imm (scratch, 24)                    // size + 24
                   X86_64.ADD_reg (heapPtr, scratch)
                   X86_64.ADD_imm (heapPtr, 7)
                   X86_64.AND_imm (heapPtr, -8)]                  // align
                // Store length
                @ [X86_64.MOV_store (X86_64.R10, 0, X86_64.R9)]
                // read(fd, buf, count)
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]        // fd
                @ [X86_64.LEA (X86_64.RSI, X86_64.R10, 8)]        // buf = string data
                @ [X86_64.MOV_reg (X86_64.RDX, X86_64.R9)]        // count = file size
                @ loadImm64 X86_64.RAX readSyscall
                @ [X86_64.SYSCALL]
                // Store refcount after data: [R10 + 8 + size]
                @ [X86_64.MOV_reg (scratch, X86_64.R10)
                   X86_64.ADD_imm (scratch, 8)
                   X86_64.ADD_reg (scratch, X86_64.R9)]
                @ loadImm64 X86_64.RCX 1L
                @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)]
                // close(fd)
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]
                @ loadImm64 X86_64.RAX closeSyscall
                @ [X86_64.SYSCALL]
                // Allocate Result Ok: [tag=0:8][payload=string_ptr:8][refcount=1:8]
                @ [X86_64.MOV_reg (scratch, heapPtr)
                   X86_64.ADD_imm (heapPtr, 24)]
                @ loadImm64 X86_64.RCX 0L
                @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)       // tag = 0 (Ok)
                   X86_64.MOV_store (scratch, 8, X86_64.R10)]      // payload = string ptr
                @ loadImm64 X86_64.RCX 1L
                @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)      // refcount = 1
                   X86_64.MOV_reg (X86_64.RAX, scratch)
                   X86_64.JMP cleanupLabel]
                // === Error path ===
                @ [X86_64.Label errorLabel]
                // Allocate error string "Error": [len=5:8]["Error":8][refcount=1:8]
                @ [X86_64.MOV_reg (X86_64.R10, heapPtr)
                   X86_64.ADD_imm (heapPtr, 24)]
                @ loadImm64 scratch 5L
                @ [X86_64.MOV_store (X86_64.R10, 0, scratch)]       // length = 5
                @ loadImm64 scratch 0x726F727245L                    // "Error" in little-endian
                @ [X86_64.MOV_store (X86_64.R10, 8, scratch)]       // data
                @ loadImm64 scratch 1L
                @ [X86_64.MOV_store (X86_64.R10, 16, scratch)]      // refcount
                // Allocate Result Error: [tag=1:8][payload=error_str:8][refcount=1:8]
                @ [X86_64.MOV_reg (scratch, heapPtr)
                   X86_64.ADD_imm (heapPtr, 24)]
                @ loadImm64 X86_64.RCX 1L
                @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)        // tag = 1 (Error)
                   X86_64.MOV_store (scratch, 8, X86_64.R10)]       // payload = error string
                @ loadImm64 X86_64.RCX 1L
                @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)       // refcount
                   X86_64.MOV_reg (X86_64.RAX, scratch)]
                // === Cleanup ===
                @ [X86_64.Label cleanupLabel
                   X86_64.ADD_imm (X86_64.RSP, 4240)]
                @ restores
                @ [X86_64.MOV_reg (destReg, X86_64.RAX)]))

    | LIR.FileWriteText (dest, path, content) | LIR.FileAppendText (dest, path, content) ->
        // File write/append: open → write → close → Result
        let isAppend = match instr with LIR.FileAppendText _ -> true | _ -> false
        resolveReg dest
        |> Result.bind (fun destReg ->
            let resolvePathToR10 =
                match path with
                | LIR.Reg reg ->
                    resolveReg reg |> Result.map (fun srcReg ->
                        if srcReg = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, srcReg)])
                | LIR.StackSlot offset ->
                    Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
                | LIR.StringSymbol value ->
                    Ok (emitStringLiteralNoRefCount X86_64.R10 value)
                | _ -> Ok (loadImm64 X86_64.R10 0L)
            let resolveContentToR9 =
                match content with
                | LIR.Reg reg ->
                    resolveReg reg |> Result.map (fun srcReg ->
                        if srcReg = X86_64.R9 then [] else [X86_64.MOV_reg (X86_64.R9, srcReg)])
                | LIR.StackSlot offset ->
                    Ok [X86_64.MOV_load (X86_64.R9, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
                | LIR.StringSymbol value ->
                    Ok (emitStringLiteralNoRefCount X86_64.R9 value)
                | _ -> Ok (loadImm64 X86_64.R9 0L)
            let copyLabel = freshLabel "fw_copy"
            let doneLabel = freshLabel "fw_done"
            let errorLabel = freshLabel "fw_err"
            let cleanupLabel = freshLabel "fw_clean"
            let openSyscall = int64 syscalls.Open
            let writeSyscall = int64 syscalls.Write
            let closeSyscall = int64 syscalls.Close
            // O_WRONLY|O_CREAT|O_TRUNC = 577 for write, O_WRONLY|O_CREAT|O_APPEND = 1089 for append
            let openFlags = if isAppend then 1089L else 577L
            resolvePathToR10 |> Result.bind (fun pathSetup ->
                resolveContentToR9 |> Result.map (fun contentSetup ->
                    let saves = [X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX
                                 X86_64.PUSH X86_64.R10; X86_64.PUSH X86_64.R8; X86_64.PUSH X86_64.R9]
                    let restores = [X86_64.POP X86_64.R9; X86_64.POP X86_64.R8; X86_64.POP X86_64.R10
                                    X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI; X86_64.POP X86_64.RDI]
                    pathSetup @ contentSetup @ saves
                    // Allocate 4096 bytes on stack for path (PATH_MAX)
                    @ [X86_64.SUB_imm (X86_64.RSP, 4096)]
                    // Copy path to null-terminated stack buffer
                    @ [X86_64.MOV_load (X86_64.RCX, X86_64.R10, 0)
                       X86_64.LEA (X86_64.RSI, X86_64.R10, 8)
                       X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
                    @ loadImm64 X86_64.R10 0L
                    @ [X86_64.Label copyLabel
                       X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                       X86_64.Jcc (X86_64.GE, doneLabel)
                       X86_64.MOV_reg (scratch, X86_64.RSI)
                       X86_64.ADD_reg (scratch, X86_64.R10)
                       X86_64.MOV_load_byte (scratch, scratch, 0)
                       X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
                       X86_64.ADD_reg (X86_64.R8, X86_64.R10)
                       X86_64.MOV_store_byte (X86_64.R8, 0, scratch)
                       X86_64.ADD_imm (X86_64.R10, 1)
                       X86_64.JMP copyLabel
                       X86_64.Label doneLabel]
                    // Null-terminate
                    @ [X86_64.MOV_reg (scratch, X86_64.RDI)
                       X86_64.ADD_reg (scratch, X86_64.RCX)]
                    @ loadImm64 X86_64.R10 0L
                    @ [X86_64.MOV_store_byte (scratch, 0, X86_64.R10)]
                    // open(path, flags, mode=0666)
                    @ [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
                    @ loadImm64 X86_64.RSI openFlags
                    @ loadImm64 X86_64.RDX 0o666L
                    @ loadImm64 X86_64.RAX openSyscall
                    @ [X86_64.SYSCALL]
                    @ [X86_64.CMP_imm (X86_64.RAX, 0)
                       X86_64.Jcc (X86_64.LT, errorLabel)]
                    // Save fd in R8
                    @ [X86_64.MOV_reg (X86_64.R8, X86_64.RAX)]
                    // write(fd, content_data, content_len)
                    // R9 = content heap string (saved by PUSH above, load from stack)
                    // R9 was pushed at position 5 (index from top after SUB): need to recalculate
                    // After pushes (6 * 8 = 48) + SUB 4096 = 4144 bytes below original RSP
                    // R9 was the last push, so at [RSP + 4096 + 0] = [RSP + 4096]
                    @ [X86_64.MOV_load (X86_64.R9, X86_64.RSP, 4096)]  // reload R9 (content)
                    @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]        // fd
                    @ [X86_64.LEA (X86_64.RSI, X86_64.R9, 8)]         // content data
                    @ [X86_64.MOV_load (X86_64.RDX, X86_64.R9, 0)]    // content length
                    @ loadImm64 X86_64.RAX writeSyscall
                    @ [X86_64.SYSCALL]
                    // close(fd)
                    @ [X86_64.MOV_reg (X86_64.RDI, X86_64.R8)]
                    @ loadImm64 X86_64.RAX closeSyscall
                    @ [X86_64.SYSCALL]
                    // Allocate Result Ok: [tag=0:8][payload=0:8][refcount=1:8]
                    @ [X86_64.MOV_reg (scratch, heapPtr)
                       X86_64.ADD_imm (heapPtr, 24)]
                    @ loadImm64 X86_64.RCX 0L
                    @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)       // tag = 0 (Ok)
                       X86_64.MOV_store (scratch, 8, X86_64.RCX)]      // payload = 0 (Unit)
                    @ loadImm64 X86_64.RCX 1L
                    @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)      // refcount
                       X86_64.MOV_reg (X86_64.RAX, scratch)
                       X86_64.JMP cleanupLabel]
                    // === Error path ===
                    @ [X86_64.Label errorLabel]
                    @ [X86_64.MOV_reg (X86_64.R10, heapPtr)
                       X86_64.ADD_imm (heapPtr, 24)]
                    @ loadImm64 scratch 5L
                    @ [X86_64.MOV_store (X86_64.R10, 0, scratch)]
                    @ loadImm64 scratch 0x726F727245L                    // "Error"
                    @ [X86_64.MOV_store (X86_64.R10, 8, scratch)]
                    @ loadImm64 scratch 1L
                    @ [X86_64.MOV_store (X86_64.R10, 16, scratch)]
                    @ [X86_64.MOV_reg (scratch, heapPtr)
                       X86_64.ADD_imm (heapPtr, 24)]
                    @ loadImm64 X86_64.RCX 1L
                    @ [X86_64.MOV_store (scratch, 0, X86_64.RCX)
                       X86_64.MOV_store (scratch, 8, X86_64.R10)]
                    @ loadImm64 X86_64.RCX 1L
                    @ [X86_64.MOV_store (scratch, 16, X86_64.RCX)
                       X86_64.MOV_reg (X86_64.RAX, scratch)]
                    // === Cleanup ===
                    @ [X86_64.Label cleanupLabel
                       X86_64.ADD_imm (X86_64.RSP, 4096)]
                    @ restores
                    @ [X86_64.MOV_reg (destReg, X86_64.RAX)])))

    | LIR.FileExists (dest, path) ->
        resolveReg dest
        |> Result.bind (fun destReg ->
            let resolvePathToR10 =
                match path with
                | LIR.Reg reg ->
                    resolveReg reg |> Result.map (fun srcReg ->
                        if srcReg = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, srcReg)])
                | LIR.StackSlot offset ->
                    Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
                | LIR.StringSymbol value ->
                    Ok (emitStringLiteralNoRefCount X86_64.R10 value)
                | _ -> Ok (loadImm64 X86_64.R10 0L)
            let copyLabel = freshLabel "fe_copy"
            let doneLabel = freshLabel "fe_done"
            resolvePathToR10 |> Result.map (fun pathSetup ->
                let accessSyscall = int64 syscalls.Access
                // Save clobbered registers (access syscall uses RDI, RSI, RAX + copy uses RCX, R10)
                let saves = [X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX; X86_64.PUSH X86_64.R10]
                let restores = [X86_64.POP X86_64.R10; X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI; X86_64.POP X86_64.RDI]
                pathSetup @ saves
                // Allocate 4096 bytes on stack for null-terminated path (PATH_MAX)
                @ [X86_64.SUB_imm (X86_64.RSP, 4096)]
                // R10 = heap string ptr. [R10] = length, R10+8 = data
                // RSI = string data addr, RDI = stack buf, RCX = length, R11 = counter
                @ [X86_64.MOV_load (X86_64.RCX, X86_64.R10, 0)   // RCX = length
                   X86_64.LEA (X86_64.RSI, X86_64.R10, 8)         // RSI = data ptr
                   X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]       // RDI = stack buf
                // Copy loop using R11 (scratch) as counter
                @ loadImm64 X86_64.R10 0L  // R10 = counter (reuse R10 since string ptr no longer needed)
                @ [X86_64.Label copyLabel
                   X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                   X86_64.Jcc (X86_64.GE, doneLabel)
                   // scratch = [RSI + R10]
                   X86_64.MOV_reg (scratch, X86_64.RSI)
                   X86_64.ADD_reg (scratch, X86_64.R10)
                   X86_64.MOV_load_byte (scratch, scratch, 0)
                   // [RDI + R10] = byte
                   X86_64.PUSH X86_64.R8
                   X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
                   X86_64.ADD_reg (X86_64.R8, X86_64.R10)
                   X86_64.MOV_store_byte (X86_64.R8, 0, scratch)
                   X86_64.POP X86_64.R8
                   X86_64.ADD_imm (X86_64.R10, 1)
                   X86_64.JMP copyLabel
                   X86_64.Label doneLabel]
                // Null-terminate: [RDI + RCX] = 0
                @ [X86_64.MOV_reg (scratch, X86_64.RDI)
                   X86_64.ADD_reg (scratch, X86_64.RCX)]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.MOV_store_byte (scratch, 0, X86_64.R10)]
                // syscall: access(path=RSP, mode=F_OK=0)
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
                @ loadImm64 X86_64.RSI 0L
                @ loadImm64 X86_64.RAX accessSyscall
                @ [X86_64.SYSCALL
                   // RAX = 0 if exists, negative otherwise
                   // Convert to boolean in R10 (safe temp, will be popped later but unused)
                   X86_64.CMP_imm (X86_64.RAX, 0)
                   X86_64.SETcc (X86_64.EQ, X86_64.RAX)
                   X86_64.MOVZX_byte (X86_64.RAX, X86_64.RAX)
                   X86_64.ADD_imm (X86_64.RSP, 4096)]
                @ restores
                // Move result to destReg after restoring saved registers
                @ [X86_64.MOV_reg (destReg, X86_64.RAX)]))

    | LIR.FileDelete (dest, path) ->
        // Real unlink(path) syscall (was a no-op stub). Copies the heap string to
        // a null-terminated stack buffer (same as FileExists) then unlinks it.
        resolveReg dest
        |> Result.bind (fun destReg ->
            let resolvePathToR10 =
                match path with
                | LIR.Reg reg ->
                    resolveReg reg |> Result.map (fun srcReg ->
                        if srcReg = X86_64.R10 then [] else [X86_64.MOV_reg (X86_64.R10, srcReg)])
                | LIR.StackSlot offset ->
                    Ok [X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
                | LIR.StringSymbol value ->
                    Ok (emitStringLiteralNoRefCount X86_64.R10 value)
                | _ -> Ok (loadImm64 X86_64.R10 0L)
            let copyLabel = freshLabel "fd_copy"
            let doneLabel = freshLabel "fd_done"
            resolvePathToR10 |> Result.map (fun pathSetup ->
                let unlinkSyscall = int64 syscalls.Unlink
                let saves = [X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX; X86_64.PUSH X86_64.R10]
                let restores = [X86_64.POP X86_64.R10; X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI; X86_64.POP X86_64.RDI]
                pathSetup @ saves
                @ [X86_64.SUB_imm (X86_64.RSP, 4096)]
                @ [X86_64.MOV_load (X86_64.RCX, X86_64.R10, 0)
                   X86_64.LEA (X86_64.RSI, X86_64.R10, 8)
                   X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.Label copyLabel
                   X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                   X86_64.Jcc (X86_64.GE, doneLabel)
                   X86_64.MOV_reg (scratch, X86_64.RSI)
                   X86_64.ADD_reg (scratch, X86_64.R10)
                   X86_64.MOV_load_byte (scratch, scratch, 0)
                   X86_64.PUSH X86_64.R8
                   X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
                   X86_64.ADD_reg (X86_64.R8, X86_64.R10)
                   X86_64.MOV_store_byte (X86_64.R8, 0, scratch)
                   X86_64.POP X86_64.R8
                   X86_64.ADD_imm (X86_64.R10, 1)
                   X86_64.JMP copyLabel
                   X86_64.Label doneLabel]
                @ [X86_64.MOV_reg (scratch, X86_64.RDI)
                   X86_64.ADD_reg (scratch, X86_64.RCX)]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.MOV_store_byte (scratch, 0, X86_64.R10)]
                // syscall: unlink(path=RSP)
                @ [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]
                @ loadImm64 X86_64.RAX unlinkSyscall
                @ [X86_64.SYSCALL
                   X86_64.ADD_imm (X86_64.RSP, 4096)]
                @ restores
                @ loadImm64 destReg 0L))

    | LIR.FileSetExecutable (dest, _) ->
        resolveReg dest
        |> Result.map (fun destReg -> loadImm64 destReg 0L)

    | LIR.FileWriteFromPtr (dest, _, _, _) ->
        resolveReg dest
        |> Result.map (fun destReg -> loadImm64 destReg 0L)

    // All LIR instruction variants are handled above

/// Calculate aligned stack allocation size.
/// After CALL pushes 8-byte return address, each PUSH adds 8 bytes.
/// We need total to be 16-byte aligned for System V ABI compliance.
/// Translate a LIR terminator to x86-64 instructions
let private translateTerminator (epilogueLabel: string) (term: LIR.Terminator) : Result<X86_64.Instr list, string> =
    match term with
    | LIR.Ret ->
        // Jump to shared epilogue at end of function
        Ok [X86_64.JMP epilogueLabel]
    | LIR.Jump (LIR.Label target) ->
        Ok [X86_64.JMP target]
    | LIR.Branch (cond, LIR.Label trueLabel, LIR.Label falseLabel) ->
        resolveReg cond
        |> Result.map (fun condReg ->
            [X86_64.TEST_reg (condReg, condReg)
             X86_64.Jcc (X86_64.NE, trueLabel)
             X86_64.JMP falseLabel])
    | LIR.BranchZero (cond, LIR.Label zeroLabel, LIR.Label nonZeroLabel) ->
        resolveReg cond
        |> Result.map (fun condReg ->
            [X86_64.TEST_reg (condReg, condReg)
             X86_64.Jcc (X86_64.EQ, zeroLabel)
             X86_64.JMP nonZeroLabel])
    | LIR.CondBranch (cond, LIR.Label trueLabel, LIR.Label falseLabel) ->
        let x86Cond =
            if lastCompWasFloat then
                match cond with
                | LIR.EQ -> X86_64.EQ | LIR.NE -> X86_64.NE
                | LIR.LT | LIR.ULT -> X86_64.B  | LIR.GT | LIR.UGT -> X86_64.A
                | LIR.LE | LIR.ULE -> X86_64.BE | LIR.GE | LIR.UGE -> X86_64.AE
            else
                match cond with
                | LIR.EQ -> X86_64.EQ | LIR.NE -> X86_64.NE
                | LIR.LT -> X86_64.LT | LIR.GT -> X86_64.GT
                | LIR.LE -> X86_64.LE | LIR.GE -> X86_64.GE
                // Unsigned: below/above.
                | LIR.ULT -> X86_64.B | LIR.UGT -> X86_64.A
                | LIR.ULE -> X86_64.BE | LIR.UGE -> X86_64.AE
        Ok [X86_64.Jcc (x86Cond, trueLabel)
            X86_64.JMP falseLabel]
    | LIR.BranchBitZero (reg, bit, LIR.Label zeroLabel, LIR.Label nonZeroLabel) ->
        resolveReg reg
        |> Result.map (fun regX86 ->
            // TEST reg with bit mask, branch on zero flag
            let mask = 1L <<< bit
            if mask >= int64 System.Int32.MinValue && mask <= int64 System.Int32.MaxValue then
                [X86_64.TEST_reg (regX86, regX86)  // Actually need to test specific bit
                 // Use AND with immediate to test the bit
                ] |> ignore
                loadImm64 scratch mask
                @ [X86_64.AND_reg (scratch, regX86)
                   X86_64.Jcc (X86_64.EQ, zeroLabel)
                   X86_64.JMP nonZeroLabel]
            else
                loadImm64 scratch mask
                @ [X86_64.AND_reg (scratch, regX86)
                   X86_64.Jcc (X86_64.EQ, zeroLabel)
                   X86_64.JMP nonZeroLabel])

    | LIR.BranchBitNonZero (reg, bit, LIR.Label nonZeroLabel, LIR.Label zeroLabel) ->
        resolveReg reg
        |> Result.map (fun regX86 ->
            let mask = 1L <<< bit
            loadImm64 scratch mask
            @ [X86_64.AND_reg (scratch, regX86)
               X86_64.Jcc (X86_64.NE, nonZeroLabel)
               X86_64.JMP zeroLabel])

/// Translate a LIR basic block to x86-64 instructions
let private translateBlock (ctx: FuncCtx) (epilogueLabel: string) (block: LIR.BasicBlock) : Result<X86_64.Instr list, string> =
    let (LIR.Label labelName) = block.Label
    let labelInstr = [X86_64.Label labelName]

    let rec translateInstrs acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | instr :: rest ->
            match translateInstr ctx instr with
            | Error e -> Error e
            | Ok instrs -> translateInstrs (instrs :: acc) rest

    match translateInstrs [] block.Instrs with
    | Error e -> Error e
    | Ok bodyInstrs ->
        translateTerminator epilogueLabel block.Terminator
        |> Result.map (fun termInstrs ->
            labelInstr @ bodyInstrs @ termInstrs)

/// Translate a LIR function to x86-64 instructions
let translateFunction (enableLeakCheck: bool) (func: LIR.Function) : Result<X86_64.Instr list, string> =
    let epilogueLabel = "_epilogue_" + func.Name
    let prologue = genPrologue func.StackSize func.UsedCalleeSaved

    // Float parameter setup: the register allocator inserts FMov instructions
    // at the start of the entry block (e.g., "D1 <- FMov(D0)"). These are
    // handled by the FMov case in translateInstr. No extra codegen needed.

    // Translate all blocks in order (entry first)
    let entryBlock = Map.find func.CFG.Entry func.CFG.Blocks
    let otherBlocks =
        func.CFG.Blocks
        |> Map.toList
        |> List.filter (fun (label, _) -> label <> func.CFG.Entry)
        |> List.map snd

    let allBlocks = entryBlock :: otherBlocks

    let rec translateBlocks acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | block :: rest ->
            let ctx : FuncCtx = { StackSize = func.StackSize; UsedCalleeSaved = func.UsedCalleeSaved; EnableLeakCheck = enableLeakCheck }
            match translateBlock ctx epilogueLabel block with
            | Error e -> Error e
            | Ok instrs -> translateBlocks (instrs :: acc) rest

    match translateBlocks [] allBlocks with
    | Error e -> Error e
    | Ok blockInstrs ->
        // Heap initialization for _start only
        let heapInit =
            if func.Name = "_start" then genHeapInit ()
            else []

        let funcLabel = [X86_64.Label func.Name]
        // Generate leak check report for _start exit
        let leakReport =
            if func.Name = "_start" && enableLeakCheck then
                genLeakCheckReport ()
            else []

        let epilogue =
            [X86_64.Label epilogueLabel]
            @ genEpilogue func.StackSize func.UsedCalleeSaved
            @ (if func.Name = "_start" then
                   // _start: report leaks then exit(0)
                   leakReport @ loadImm64 X86_64.RDI 0L @ genExitSyscall
               else
                   [X86_64.RET])
        Ok (funcLabel @ prologue @ heapInit @ blockInstrs @ epilogue)

/// Translate a complete LIR program to x86-64 instructions
let translateProgram (LIR.Program functions) (enableLeakCheck: bool) : Result<X86_64.Instr list, string> =
    let rec translateFuncs acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | func :: rest ->
            match translateFunction enableLeakCheck func with
            | Error e -> Error e
            | Ok instrs -> translateFuncs (instrs :: acc) rest

    // Check if any function uses TaggedList RefCountDec or RefCountInc
    let needsListRcDecHelper =
        functions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.RefCountDec (_, _, LIR.TaggedList) -> true
                    | _ -> false)))

    let needsListRcIncHelper =
        functions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.RefCountInc (_, _, LIR.TaggedList) -> true
                    | LIR.RawSet (_, _, _, Some (AST.TList _)) -> true
                    | _ -> false)))

    translateFuncs [] functions
    |> Result.map (fun allInstrs ->
        let listIncHelper =
            if needsListRcIncHelper then generateListRefCountIncHelper ()
            else []
        let listDecHelper =
            if needsListRcDecHelper then generateListRefCountDecHelper enableLeakCheck
            else []
        allInstrs @ listIncHelper @ listDecHelper @ genOomHandler ())
