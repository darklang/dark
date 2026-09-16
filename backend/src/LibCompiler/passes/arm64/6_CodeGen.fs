// 6_CodeGen.fs - Code Generation (Pass 6)
//
// Transforms LIR into ARM64 instructions.
//
// Code generation algorithm:
// - Maps LIR physical registers to ARM64 registers
// - Selects ARM64 instruction forms (immediate vs register operands)
// - Generates MOVZ instructions for loading immediate values
// - Handles 12-bit immediate constraints for ADD/SUB
//
// Assumes register allocation completed (no virtual registers remain)
//
// Example:
//   X0 <- Mov(Imm 42); X1 <- Add(X0, Imm 5)
//   →
//   MOVZ X0, #42, LSL #0; ADD X1, X0, #5

module CodeGen

/// Code generation options
type CodeGenOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
    /// Enable coverage instrumentation
    EnableCoverage: bool
    /// Number of coverage expressions (determines buffer size)
    CoverageExprCount: int
    /// Enable leak checking instrumentation
    EnableLeakCheck: bool
}

/// Default code generation options
let defaultOptions : CodeGenOptions = {
    DisableFreeList = false
    EnableCoverage = false
    CoverageExprCount = 0
    EnableLeakCheck = false
}

/// Code generation context (passed through to instruction conversion)
type CodeGenContext = {
    Options: CodeGenOptions
    // Function context for tail call epilogue generation
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
    HeapOverflowLabel: string
}

let leakCounterLabel = "_leak_count"
let heapOutOfMemoryMessage = "Out of heap memory"
let private heapMmapSizeBytes = 512L * 1024L * 1024L
let private heapMmapSizeMovzImm16 = 0x2000us  // 512MB == 0x20000000
let private heapOverflowLabelPrefix = "__heap_oom_"
let private listRefCountIncHelperLabel = "__dark_list_refcount_inc_helper"
let private listRefCountDecHelperLabel = "__dark_list_refcount_dec_helper"

let private dataLabel (name: string) : ARM64Symbolic.LabelRef =
    ARM64Symbolic.DataLabel (ARM64Symbolic.Named name)

let private stringDataLabel (value: string) : ARM64Symbolic.LabelRef =
    ARM64Symbolic.DataLabel (ARM64Symbolic.StringLiteral value)

let private floatDataLabel (value: float) : ARM64Symbolic.LabelRef =
    ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral value)

let private codeLabel (name: string) : ARM64Symbolic.LabelRef =
    ARM64Symbolic.CodeLabel name

let private runtimeInstrs (instrs: ARM64.Instr list) : ARM64Symbolic.Instr list =
    ARM64Symbolic.ofARM64List instrs

let private utf8Len (value: string) : int =
    System.Text.Encoding.UTF8.GetByteCount value

let private generateHeapOverflowTrapBody () : ARM64Symbolic.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error msg -> Crash.crash $"Platform detection failed: {msg}"
    let syscalls = ARM64.syscallConfigFor os
    let messageBytes = System.Text.Encoding.UTF8.GetBytes(heapOutOfMemoryMessage) |> Array.toList
    runtimeInstrs (Runtime.generatePrintCharsToStderr messageBytes)
    @ [
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)  // exit code = 1
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
        ARM64Symbolic.SVC syscalls.SvcImmediate
    ]

let private generateHeapOverflowTrapBlock (label: string) : ARM64Symbolic.Instr list =
    ARM64Symbolic.Label label :: generateHeapOverflowTrapBody ()

let private withHeapBoundsCheck
    (overflowLabel: string)
    (nextPtrInstrs: ARM64Symbolic.Instr list)
    (allocInstrs: ARM64Symbolic.Instr list)
    : ARM64Symbolic.Instr list =
    [
        ARM64Symbolic.MOVZ (ARM64Symbolic.X11, heapMmapSizeMovzImm16, 16)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X11, ARM64Symbolic.X27, ARM64Symbolic.X11)
    ]
    @ nextPtrInstrs
    @ [
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X14, ARM64Symbolic.X11)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, overflowLabel)
    ]
    @ allocInstrs

let private checkedBumpAllocReg
    (overflowLabel: string)
    (destReg: ARM64Symbolic.Reg)
    (sizeReg: ARM64Symbolic.Reg)
    : ARM64Symbolic.Instr list =
    withHeapBoundsCheck
        overflowLabel
        [ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X28, sizeReg)]
        [
            ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, sizeReg)
        ]

let private generateListRefCountIncHelper () : ARM64Symbolic.Instr list =
    let label (name: string) : string = $"__dark_list_rc_inc_{name}"
    let size24 = label "size_24"
    let size32 = label "size_32"
    let size96 = label "size_96"
    let haveSize = label "have_size"
    let helperRet = label "ret"

    [
        ARM64Symbolic.Label listRefCountIncHelperLabel
        // X0 = tagged list pointer (or 0)
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, helperRet)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X1, ARM64Symbolic.X0, 7UL)
        ARM64Symbolic.CBZ (ARM64Symbolic.X1, helperRet)  // Untagged pointer => not a FingerTree node
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 5us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, helperRet)
        // Clear low tag bits via shifts (AND #~7 is not always encodable as a logical immediate).
        ARM64Symbolic.LSR_imm (ARM64Symbolic.X2, ARM64Symbolic.X0, 3)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X2, ARM64Symbolic.X2, 3)

        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size96)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size24)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 4us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size32)

        // tags 1 and 5 (SINGLE/LEAF)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 8us, 0)
        ARM64Symbolic.B_label haveSize

        ARM64Symbolic.Label size24
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 24us, 0)
        ARM64Symbolic.B_label haveSize

        ARM64Symbolic.Label size32
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 32us, 0)
        ARM64Symbolic.B_label haveSize

        ARM64Symbolic.Label size96
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 96us, 0)

        ARM64Symbolic.Label haveSize
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X2, ARM64Symbolic.X2, ARM64Symbolic.X3)
        ARM64Symbolic.LDR (ARM64Symbolic.X4, ARM64Symbolic.X2, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X4, ARM64Symbolic.X2, 0s)

        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]

let private generateListRefCountDecHelper (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    let label (name: string) : string = $"__dark_list_rc_dec_{name}"
    let leakDec =
        if ctx.Options.EnableLeakCheck then
            let labelRef = dataLabel leakCounterLabel
            [
                ARM64Symbolic.ADRP (ARM64Symbolic.X17, labelRef)
                ARM64Symbolic.ADD_label (ARM64Symbolic.X17, ARM64Symbolic.X17, labelRef)
                ARM64Symbolic.LDR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
            ]
        else
            []

    let addChild (suffix: string) : ARM64Symbolic.Instr list =
        let doneLabel = label $"child_done_{suffix}"
        let pushLabel = label $"child_push_{suffix}"
        [
            ARM64Symbolic.CBZ (ARM64Symbolic.X8, doneLabel)
            // Only traverse plausible tagged list pointers inside the managed heap range.
            ARM64Symbolic.AND_imm (ARM64Symbolic.X9, ARM64Symbolic.X8, 7UL)
            ARM64Symbolic.CBZ (ARM64Symbolic.X9, doneLabel)
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X9, 5us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, doneLabel)
            ARM64Symbolic.LSR_imm (ARM64Symbolic.X10, ARM64Symbolic.X8, 3)
            ARM64Symbolic.LSL_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 3)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X10, ARM64Symbolic.X27)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, doneLabel)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X10, ARM64Symbolic.X28)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, doneLabel)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X0, pushLabel)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X8)
            ARM64Symbolic.B_label doneLabel
            ARM64Symbolic.Label pushLabel
            ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ARM64Symbolic.STR (ARM64Symbolic.X8, ARM64Symbolic.SP, 0s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
            ARM64Symbolic.Label doneLabel
        ]

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

    [
        ARM64Symbolic.Label listRefCountDecHelperLabel
        // X0 = current tagged list pointer to process, X1 = number of pending stack entries.
        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 0us, 0)
        ARM64Symbolic.B_label loopCheck

        ARM64Symbolic.Label loopCheck
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, popOrRet)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X2, ARM64Symbolic.X0, 7UL)
        ARM64Symbolic.CBZ (ARM64Symbolic.X2, popOrRet)
        // Clear low tag bits via shifts (AND #~7 is not always encodable as a logical immediate).
        ARM64Symbolic.LSR_imm (ARM64Symbolic.X3, ARM64Symbolic.X0, 3)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 3)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X3, ARM64Symbolic.X27)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, popOrRet)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X3, ARM64Symbolic.X28)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, popOrRet)

        // Resolve payload size from FingerTree node tag.
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size8)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size96)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size24)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 4us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size32)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 5us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size8)
        ARM64Symbolic.B_label popOrRet

        ARM64Symbolic.Label size8
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 8us, 0)
        ARM64Symbolic.B_label haveSize
        ARM64Symbolic.Label size24
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 24us, 0)
        ARM64Symbolic.B_label haveSize
        ARM64Symbolic.Label size32
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 32us, 0)
        ARM64Symbolic.B_label haveSize
        ARM64Symbolic.Label size96
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 96us, 0)

        ARM64Symbolic.Label haveSize
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X5, ARM64Symbolic.X3, ARM64Symbolic.X4)
        ARM64Symbolic.LDR (ARM64Symbolic.X6, ARM64Symbolic.X5, 0s)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X6, ARM64Symbolic.X5, 0s)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X6, popOrRet)

        // Refcount reached zero: collect child pointers for further decref work.
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectSingle)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectDeep)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectNode2)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 4us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectNode3)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 5us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectLeaf)
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectSingle
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
    ]
    @ addChild "single_0"
    @ [
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectNode2
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
    ]
    @ addChild "node2_0"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 8s)
    ]
    @ addChild "node2_1"
    @ [
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectNode3
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
    ]
    @ addChild "node3_0"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 8s)
    ]
    @ addChild "node3_1"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 16s)
    ]
    @ addChild "node3_2"
    @ [
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectDeep
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X7, ARM64Symbolic.X3, 8s)  // prefix_count
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 16s)
    ]
    @ addChild "deep_p0"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 24s)
    ]
    @ addChild "deep_p1"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 32s)
    ]
    @ addChild "deep_p2"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 40s)
    ]
    @ addChild "deep_p3"
    @ [
        ARM64Symbolic.Label afterPrefix
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 48s)  // middle tree
    ]
    @ addChild "deep_middle"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X7, ARM64Symbolic.X3, 56s)  // suffix_count
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 64s)
    ]
    @ addChild "deep_s0"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 72s)
    ]
    @ addChild "deep_s1"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 80s)
    ]
    @ addChild "deep_s2"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 88s)
    ]
    @ addChild "deep_s3"
    @ [
        ARM64Symbolic.Label afterSuffix
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectLeaf
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)

        ARM64Symbolic.Label freeNode
        // Recycle node memory by payload size class.
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X5, ARM64Symbolic.X27, ARM64Symbolic.X4)
        ARM64Symbolic.LDR (ARM64Symbolic.X6, ARM64Symbolic.X5, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X6, ARM64Symbolic.X3, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X3, ARM64Symbolic.X5, 0s)
    ]
    @ leakDec
    @ [
        ARM64Symbolic.B_label loopCheck

        ARM64Symbolic.Label popOrRet
        ARM64Symbolic.CBZ (ARM64Symbolic.X1, helperRet)
        ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.SP, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
        ARM64Symbolic.B_label loopCheck

        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]

let generateLeakCounterInc (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    if ctx.Options.EnableLeakCheck then
        let labelRef = dataLabel leakCounterLabel
        [
            ARM64Symbolic.ADRP (ARM64Symbolic.X17, labelRef)
            ARM64Symbolic.ADD_label (ARM64Symbolic.X17, ARM64Symbolic.X17, labelRef)
            ARM64Symbolic.LDR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
        ]
    else
        []

let generateLeakCounterDec (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    if ctx.Options.EnableLeakCheck then
        let labelRef = dataLabel leakCounterLabel
        [
            ARM64Symbolic.ADRP (ARM64Symbolic.X17, labelRef)
            ARM64Symbolic.ADD_label (ARM64Symbolic.X17, ARM64Symbolic.X17, labelRef)
            ARM64Symbolic.LDR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
        ]
    else
        []

let generateLeakCheckReport (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    if ctx.Options.EnableLeakCheck then
        let prefix = Runtime.generatePrintCharsToStderr [byte 'l'; byte 'e'; byte 'a'; byte 'k'; byte 's'; byte ':'; byte ' '] |> runtimeInstrs
        let printCount = Runtime.generatePrintInt64ToStderrNoExit () |> runtimeInstrs
        let skipOffset = List.length prefix + 1 + List.length printCount + 1
        let labelRef = dataLabel leakCounterLabel
        [
            ARM64Symbolic.ADRP (ARM64Symbolic.X17, labelRef)
            ARM64Symbolic.ADD_label (ARM64Symbolic.X17, ARM64Symbolic.X17, labelRef)
            ARM64Symbolic.LDR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
            ARM64Symbolic.CBZ_offset (ARM64Symbolic.X16, skipOffset)
        ]
        @ prefix
        @ [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X16)]
        @ printCount
    else
        []

/// Convert LIR.PhysReg to ARM64Symbolic.Reg
let lirPhysRegToARM64Reg (physReg: LIR.PhysReg) : ARM64Symbolic.Reg =
    match physReg with
    | LIR.X0 -> ARM64Symbolic.X0
    | LIR.X1 -> ARM64Symbolic.X1
    | LIR.X2 -> ARM64Symbolic.X2
    | LIR.X3 -> ARM64Symbolic.X3
    | LIR.X4 -> ARM64Symbolic.X4
    | LIR.X5 -> ARM64Symbolic.X5
    | LIR.X6 -> ARM64Symbolic.X6
    | LIR.X7 -> ARM64Symbolic.X7
    | LIR.X8 -> ARM64Symbolic.X8
    | LIR.X9 -> ARM64Symbolic.X9
    | LIR.X10 -> ARM64Symbolic.X10
    | LIR.X11 -> ARM64Symbolic.X11
    | LIR.X12 -> ARM64Symbolic.X12
    | LIR.X13 -> ARM64Symbolic.X13
    | LIR.X14 -> ARM64Symbolic.X14
    | LIR.X15 -> ARM64Symbolic.X15
    | LIR.X16 -> ARM64Symbolic.X16
    | LIR.X17 -> ARM64Symbolic.X17
    | LIR.X19 -> ARM64Symbolic.X19
    | LIR.X20 -> ARM64Symbolic.X20
    | LIR.X21 -> ARM64Symbolic.X21
    | LIR.X22 -> ARM64Symbolic.X22
    | LIR.X23 -> ARM64Symbolic.X23
    | LIR.X24 -> ARM64Symbolic.X24
    | LIR.X25 -> ARM64Symbolic.X25
    | LIR.X26 -> ARM64Symbolic.X26
    | LIR.X27 -> ARM64Symbolic.X27
    | LIR.X29 -> ARM64Symbolic.X29
    | LIR.X30 -> ARM64Symbolic.X30
    | LIR.SP -> ARM64Symbolic.SP

/// Convert LIR.PhysFPReg to ARM64Symbolic.FReg
let lirPhysFPRegToARM64FReg (physReg: LIR.PhysFPReg) : ARM64Symbolic.FReg =
    match physReg with
    | LIR.D0 -> ARM64Symbolic.D0
    | LIR.D1 -> ARM64Symbolic.D1
    | LIR.D2 -> ARM64Symbolic.D2
    | LIR.D3 -> ARM64Symbolic.D3
    | LIR.D4 -> ARM64Symbolic.D4
    | LIR.D5 -> ARM64Symbolic.D5
    | LIR.D6 -> ARM64Symbolic.D6
    | LIR.D7 -> ARM64Symbolic.D7
    | LIR.D8 -> ARM64Symbolic.D8
    | LIR.D9 -> ARM64Symbolic.D9
    | LIR.D10 -> ARM64Symbolic.D10
    | LIR.D11 -> ARM64Symbolic.D11
    | LIR.D12 -> ARM64Symbolic.D12
    | LIR.D13 -> ARM64Symbolic.D13
    | LIR.D14 -> ARM64Symbolic.D14
    | LIR.D15 -> ARM64Symbolic.D15

/// Convert LIR.FReg to ARM64Symbolic.FReg
/// For FVirtual, we use a two-tier allocation scheme to avoid collisions:
/// - FVirtual 1000 -> D18 (left operand temp for binary ops)
/// - FVirtual 1001 -> D17 (right operand temp for binary ops)
/// - FVirtual 3000-3007 -> D14-D15 (temps for float call args)
/// - FVirtual 0-7 -> D2-D9 (dedicated 1:1 mapping for parameters)
/// - FVirtual 8+ -> D10-D13 (4 temps with modulo, for SSA temps and locals)
///
/// The two-tier scheme ensures that parameter VRegs (0-7) never collide with
/// SSA-generated temps (which have high IDs like 12001). Parameters get D2-D9,
/// while temps get D10-D13 with modulo 4.
let lirFRegToARM64FReg (freg: LIR.FReg) : Result<ARM64Symbolic.FReg, string> =
    match freg with
    | LIR.FPhysical physReg -> Ok (lirPhysFPRegToARM64FReg physReg)
    // Special temp registers for specific purposes
    | LIR.FVirtual 1000 -> Ok ARM64Symbolic.D18  // Left temp for binary ops
    | LIR.FVirtual 1001 -> Ok ARM64Symbolic.D17  // Right temp for binary ops
    | LIR.FVirtual 2000 -> Ok ARM64Symbolic.D16  // Temp for FPhi cycle resolution
    | LIR.FVirtual n when n >= 3000 && n < 4000 ->
        // Temps for float call arguments - use D19-D26 (8 registers)
        // These must not collide with each other since up to 8 floats
        // can be loaded before FArgMoves. Using D19-D26 avoids collision
        // with argument regs D0-D7, parameter VRegs D2-D9, SSA temps D10-D13,
        // and binary op temps D17-D18.
        let tempIdx = (n - 3000) % 8
        match tempIdx with
        | 0 -> Ok ARM64Symbolic.D19
        | 1 -> Ok ARM64Symbolic.D20
        | 2 -> Ok ARM64Symbolic.D21
        | 3 -> Ok ARM64Symbolic.D22
        | 4 -> Ok ARM64Symbolic.D23
        | 5 -> Ok ARM64Symbolic.D24
        | 6 -> Ok ARM64Symbolic.D25
        | _ -> Ok ARM64Symbolic.D26
    | LIR.FVirtual n when n >= 0 && n <= 7 ->
        // Parameters (VRegs 0-7) get dedicated D2-D9 mapping
        // This prevents collisions with SSA-generated temps
        let physReg =
            match n with
            | 0 -> ARM64Symbolic.D2
            | 1 -> ARM64Symbolic.D3
            | 2 -> ARM64Symbolic.D4
            | 3 -> ARM64Symbolic.D5
            | 4 -> ARM64Symbolic.D6
            | 5 -> ARM64Symbolic.D7
            | 6 -> ARM64Symbolic.D8
            | _ -> ARM64Symbolic.D9
        Ok physReg
    | LIR.FVirtual n when n < 10000 ->
        // ANF-level VRegs (8-9999): function params and local bindings
        // These come from ANF TempIds which are sequential across functions.
        // Pool: D0, D1, D10-D15, D27-D31 (13 registers)
        // Using direct index: (n - 8) % 13
        let tempRegs = [| ARM64Symbolic.D0; ARM64Symbolic.D1; ARM64Symbolic.D10; ARM64Symbolic.D11; ARM64Symbolic.D12; ARM64Symbolic.D13; ARM64Symbolic.D14; ARM64Symbolic.D15;
                          ARM64Symbolic.D27; ARM64Symbolic.D28; ARM64Symbolic.D29; ARM64Symbolic.D30; ARM64Symbolic.D31 |]
        let regIdx = (n - 8) % tempRegs.Length
        Ok tempRegs.[regIdx]
    | LIR.FVirtual n ->
        // MIR intermediates (VRegs 10000+): computation temps from freshReg
        // Use same pool but with offset to reduce collisions with ANF-level VRegs
        // The offset of 7 ensures that if ANF VReg k and MIR VReg (10000+k) exist,
        // they map to different registers (since 7 and 13 are coprime)
        let tempRegs = [| ARM64Symbolic.D0; ARM64Symbolic.D1; ARM64Symbolic.D10; ARM64Symbolic.D11; ARM64Symbolic.D12; ARM64Symbolic.D13; ARM64Symbolic.D14; ARM64Symbolic.D15;
                          ARM64Symbolic.D27; ARM64Symbolic.D28; ARM64Symbolic.D29; ARM64Symbolic.D30; ARM64Symbolic.D31 |]
        let regIdx = ((n - 10000) + 7) % tempRegs.Length
        Ok tempRegs.[regIdx]

/// Convert LIR.Reg to ARM64Symbolic.Reg (assumes physical registers only)
let lirRegToARM64Reg (reg: LIR.Reg) : Result<ARM64Symbolic.Reg, string> =
    match reg with
    | LIR.Physical physReg -> Ok (lirPhysRegToARM64Reg physReg)
    | LIR.Virtual vreg -> Error $"Virtual register {vreg} should have been allocated"

/// Convert LIR.Reg (Virtual) to LIR.FReg (FVirtual) for float HeapStore
/// This is used when a float value is stored via HeapStore - the register
/// ID is shared between Virtual and FVirtual address spaces
let virtualToFVirtual (reg: LIR.Reg) : LIR.FReg =
    match reg with
    | LIR.Virtual n -> LIR.FVirtual n
    | LIR.Physical p -> LIR.FPhysical (
        // Map GP physical registers to FP physical registers for edge cases
        match p with
        | LIR.X0 -> LIR.D0 | LIR.X1 -> LIR.D1 | LIR.X2 -> LIR.D2 | LIR.X3 -> LIR.D3
        | LIR.X4 -> LIR.D4 | LIR.X5 -> LIR.D5 | LIR.X6 -> LIR.D6 | LIR.X7 -> LIR.D7
        | _ -> LIR.D15)

/// Generate ARM64 instructions to load an immediate into a register
let loadImmediate (dest: ARM64Symbolic.Reg) (value: int64) : ARM64Symbolic.Instr list =
    // Load 64-bit immediate using MOVZ/MOVN + MOVK sequence
    // For negative numbers, MOVN (move NOT) can be more efficient

    // Extract each 16-bit chunk
    let chunk0 = uint16 (value >>> 0) &&& 0xFFFFus
    let chunk1 = uint16 (value >>> 16) &&& 0xFFFFus
    let chunk2 = uint16 (value >>> 32) &&& 0xFFFFus
    let chunk3 = uint16 (value >>> 48) &&& 0xFFFFus

    // Count how many chunks are all-zeros vs all-ones
    let zeroCount =
        (if chunk0 = 0us then 1 else 0) +
        (if chunk1 = 0us then 1 else 0) +
        (if chunk2 = 0us then 1 else 0) +
        (if chunk3 = 0us then 1 else 0)
    let onesCount =
        (if chunk0 = 0xFFFFus then 1 else 0) +
        (if chunk1 = 0xFFFFus then 1 else 0) +
        (if chunk2 = 0xFFFFus then 1 else 0) +
        (if chunk3 = 0xFFFFus then 1 else 0)

    // Use MOVN if more chunks are 0xFFFF (inverted gives more zeros)
    if onesCount > zeroCount then
        // Use MOVN: start with first non-0xFFFF chunk, then MOVK for remaining non-0xFFFF chunks
        // MOVN Xd, #imm, LSL #shift sets Xd = NOT(imm << shift), filling rest with 1s
        // Find first chunk that is NOT 0xFFFF (so inverting gives a meaningful value)
        let chunks = [(chunk0, 0); (chunk1, 16); (chunk2, 32); (chunk3, 48)]
        let firstNonOnes = chunks |> List.tryFind (fun (c, _) -> c <> 0xFFFFus)
        match firstNonOnes with
        | Some (firstChunk, firstShift) ->
            // Start with MOVN using inverted first non-0xFFFF chunk
            let invFirstChunk = ~~~firstChunk
            [ARM64Symbolic.MOVN (dest, invFirstChunk, firstShift)]
            @ (if firstShift <> 0 && chunk0 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk0, 0)] else [])
            @ (if firstShift <> 16 && chunk1 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk1, 16)] else [])
            @ (if firstShift <> 32 && chunk2 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk2, 32)] else [])
            @ (if firstShift <> 48 && chunk3 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk3, 48)] else [])
        | None ->
            // All chunks are 0xFFFF, use MOVN #0 to get all 1s (-1)
            [ARM64Symbolic.MOVN (dest, 0us, 0)]
    else
        // Use MOVZ: find first non-zero chunk, then MOVK for remaining non-zero chunks
        // MOVZ Xd, #imm, LSL #shift sets Xd = imm << shift, zeros elsewhere
        let chunks = [(chunk0, 0); (chunk1, 16); (chunk2, 32); (chunk3, 48)]
        let firstNonZero = chunks |> List.tryFind (fun (c, _) -> c <> 0us)
        match firstNonZero with
        | Some (firstChunk, firstShift) ->
            // Start with MOVZ using first non-zero chunk
            [ARM64Symbolic.MOVZ (dest, firstChunk, firstShift)]
            @ (if firstShift <> 0 && chunk0 <> 0us then [ARM64Symbolic.MOVK (dest, chunk0, 0)] else [])
            @ (if firstShift <> 16 && chunk1 <> 0us then [ARM64Symbolic.MOVK (dest, chunk1, 16)] else [])
            @ (if firstShift <> 32 && chunk2 <> 0us then [ARM64Symbolic.MOVK (dest, chunk2, 32)] else [])
            @ (if firstShift <> 48 && chunk3 <> 0us then [ARM64Symbolic.MOVK (dest, chunk3, 48)] else [])
        | None ->
            // All chunks are zero, just use MOVZ #0
            [ARM64Symbolic.MOVZ (dest, 0us, 0)]

/// Generate ARM64 instructions to load a stack slot into a register
/// Stack slots are accessed relative to FP (X29)
/// Uses LDUR for small offsets (-256 to +255), computes address for larger offsets
let loadStackSlot (dest: ARM64Symbolic.Reg) (offset: int) : Result<ARM64Symbolic.Instr list, string> =
    if offset >= -256 && offset <= 255 then
        // Small offset: use LDUR directly
        Ok [ARM64Symbolic.LDUR (dest, ARM64Symbolic.X29, int16 offset)]
    elif offset < 0 && -offset <= 4095 then
        // Larger negative offset: compute address into X10, then load
        // X10 = X29 - (-offset), then LDR dest, [X10, #0]
        Ok [
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 (-offset))
            ARM64Symbolic.LDR (dest, ARM64Symbolic.X10, 0s)
        ]
    elif offset > 0 && offset <= 4095 then
        // Larger positive offset: compute address into X10, then load
        Ok [
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 offset)
            ARM64Symbolic.LDR (dest, ARM64Symbolic.X10, 0s)
        ]
    else
        Error $"Stack offset {offset} exceeds supported range (-4095 to +4095)"

/// Generate ARM64 instructions to store a register to a stack slot
/// Stack slots are accessed relative to FP (X29)
/// Uses STUR for small offsets (-256 to +255), computes address for larger offsets
let storeStackSlot (src: ARM64Symbolic.Reg) (offset: int) : Result<ARM64Symbolic.Instr list, string> =
    if offset >= -256 && offset <= 255 then
        // Small offset: use STUR directly
        Ok [ARM64Symbolic.STUR (src, ARM64Symbolic.X29, int16 offset)]
    elif offset < 0 && -offset <= 4095 then
        // Larger negative offset: compute address into X10, then store
        // X10 = X29 - (-offset), then STR src, [X10, #0]
        Ok [
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 (-offset))
            ARM64Symbolic.STR (src, ARM64Symbolic.X10, 0s)
        ]
    elif offset > 0 && offset <= 4095 then
        // Larger positive offset: compute address into X10, then store
        Ok [
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 offset)
            ARM64Symbolic.STR (src, ARM64Symbolic.X10, 0s)
        ]
    else
        Error $"Stack offset {offset} exceeds supported range (-4095 to +4095)"

/// Generate STP instructions to save callee-saved register pairs
/// Returns instructions and total bytes pushed
let generateCalleeSavedSaves (regs: LIR.PhysReg list) : ARM64Symbolic.Instr list * int =
    // Sort registers for consistent ordering and pair them
    let sorted = regs |> List.sortBy (fun r ->
        match r with
        | LIR.X19 -> 19 | LIR.X20 -> 20 | LIR.X21 -> 21 | LIR.X22 -> 22
        | LIR.X23 -> 23 | LIR.X24 -> 24 | LIR.X25 -> 25 | LIR.X26 -> 26
        | LIR.X27 -> 27 | _ -> 99)

    // Process in pairs. If odd number, pad with X27 (or just save single)
    let rec savePairs (remaining: LIR.PhysReg list) (offset: int) (acc: ARM64Symbolic.Instr list) =
        match remaining with
        | [] -> (List.rev acc, offset)
        | [single] ->
            // Single register: use STR instead of STP
            let instr = ARM64Symbolic.STR (lirPhysRegToARM64Reg single, ARM64Symbolic.SP, int16 offset)
            (List.rev (instr :: acc), offset + 8)
        | r1 :: r2 :: rest ->
            let instr = ARM64Symbolic.STP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, int16 offset)
            savePairs rest (offset + 16) (instr :: acc)

    if List.isEmpty sorted then
        ([], 0)
    else
        savePairs sorted 0 []

/// Generate LDP instructions to restore callee-saved register pairs
let generateCalleeSavedRestores (regs: LIR.PhysReg list) : ARM64Symbolic.Instr list =
    let sorted = regs |> List.sortBy (fun r ->
        match r with
        | LIR.X19 -> 19 | LIR.X20 -> 20 | LIR.X21 -> 21 | LIR.X22 -> 22
        | LIR.X23 -> 23 | LIR.X24 -> 24 | LIR.X25 -> 25 | LIR.X26 -> 26
        | LIR.X27 -> 27 | _ -> 99)

    let rec restorePairs (remaining: LIR.PhysReg list) (offset: int) (acc: ARM64Symbolic.Instr list) =
        match remaining with
        | [] -> List.rev acc
        | [single] ->
            let instr = ARM64Symbolic.LDR (lirPhysRegToARM64Reg single, ARM64Symbolic.SP, int16 offset)
            List.rev (instr :: acc)
        | r1 :: r2 :: rest ->
            let instr = ARM64Symbolic.LDP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, int16 offset)
            restorePairs rest (offset + 16) (instr :: acc)

    if List.isEmpty sorted then []
    else restorePairs sorted 0 []

/// Calculate stack space needed for callee-saved registers (16-byte aligned)
let calleeSavedStackSpace (regs: LIR.PhysReg list) : int =
    let count = List.length regs
    if count = 0 then 0
    else ((count * 8 + 15) / 16) * 16  // 16-byte aligned

/// Generate function prologue
/// Saves FP, LR, callee-saved registers, and allocates stack space
let generatePrologue (usedCalleeSaved: LIR.PhysReg list) (stackSize: int) : ARM64Symbolic.Instr list =
    // Prologue sequence:
    // 1. Save FP (X29) and LR (X30) with pre-indexed addressing (combines SUB and STP)
    // 2. Set FP = SP: MOV X29, SP
    // 3. Allocate stack space for spills and callee-saved registers
    // 4. Save callee-saved registers

    // Use pre-indexed STP to save FP/LR and decrement SP in one instruction
    let saveFpLr = [ARM64Symbolic.STP_pre (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)]
    let setFp = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X29, ARM64Symbolic.SP)]

    // Calculate total additional stack space needed
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let totalExtraStack = stackSize + calleeSavedSpace

    // Allocate all stack space at once (for spills + callee-saved)
    let allocStack =
        if totalExtraStack > 0 then
            [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
        else
            []

    // Save callee-saved registers at [SP]
    // (callee-saved are at the bottom of the frame, spill space is above them)
    let (saveCalleeSavedInstrs, _) = generateCalleeSavedSaves usedCalleeSaved

    saveFpLr @ setFp @ allocStack @ saveCalleeSavedInstrs

/// Generate function epilogue
/// Restores callee-saved registers, FP, LR, and returns
let generateEpilogue (usedCalleeSaved: LIR.PhysReg list) (stackSize: int) : ARM64Symbolic.Instr list =
    // Epilogue sequence (reverse of prologue):
    // 1. Restore callee-saved registers from [SP + stackSize]
    // 2. Deallocate stack space (spills + callee-saved) at once
    // 3. Restore FP and LR with post-indexed addressing (combines LDP and ADD)
    // 4. Return: RET

    // Restore callee-saved registers from [SP]
    // (callee-saved are at the bottom of the frame, spill space is above them)
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let restoreCalleeSavedInstrs = generateCalleeSavedRestores usedCalleeSaved

    // Deallocate all stack space at once
    let totalExtraStack = stackSize + calleeSavedSpace
    let deallocStack =
        if totalExtraStack > 0 then
            [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
        else
            []

    // Use post-indexed LDP to restore FP/LR and increment SP in one instruction
    let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
    let ret = [ARM64Symbolic.RET]

    restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ ret

/// Convert LIR instruction to ARM64 instructions
let convertInstr (ctx: CodeGenContext) (instr: LIR.Instr) : Result<ARM64Symbolic.Instr list, string> =
    let getListDisplayStringFunc (elemType: AST.Type) : string option =
        match elemType with
        | AST.TInt64 -> Some "Stdlib.List.toDisplayString_i64"
        | AST.TBool -> Some "Stdlib.List.toDisplayString_bool"
        | AST.TString -> Some "Stdlib.List.toDisplayString_str"
        | AST.TFloat64 -> Some "Stdlib.List.toDisplayString_f64"
        | _ -> None

    let generatePrintListInstrs (listReg: ARM64Symbolic.Reg) (elemType: AST.Type) (includeNewline: bool) : ARM64Symbolic.Instr list =
        let os =
            match Platform.detectOS () with
            | Ok platform -> platform
            | Error msg -> Crash.crash $"Platform detection failed: {msg}"
        let syscalls = ARM64.syscallConfigFor os

        // Generate element print code based on type (uses X0 for value)
        let elemPrintCode =
            match elemType with
            | AST.TInt64 -> runtimeInstrs (Runtime.generatePrintInt64NoNewline ())
            | AST.TBool -> runtimeInstrs (Runtime.generatePrintBoolNoNewline ())
            | AST.TFloat64 ->
                // Need to move from X0 to D0 for float
                [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (Runtime.generatePrintFloatNoNewline ())
            | AST.TString | AST.TChar ->
                // X0 has string address, load len/data and print
                [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 0s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 8us)] @
                runtimeInstrs (Runtime.generatePrintStringNoNewline ())
            | AST.TTuple elemTypes ->
                // Print tuple inside list: (elem1, elem2, ...)
                // Use X21 for tuple ptr (callee-saved), keep X19 for list ptr
                let moveTupleToX21 = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X21, ARM64Symbolic.X0)]

                // Print "("
                let printOpenParen = [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte '('), 0)
                    ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
                    ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
                    ARM64Symbolic.SVC syscalls.SvcImmediate
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ]

                // Print ", " helper
                let printTupleCommaSpace = [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ','), 0)
                    ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ' '), 0)
                    ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 1)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 2us, 0)
                    ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
                    ARM64Symbolic.SVC syscalls.SvcImmediate
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ]

                // Generate code for each tuple element (load from X21)
                let tupleElemInstrs =
                    elemTypes
                    |> List.mapi (fun i eType ->
                        let loadElem = [ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X21, int16 (i * 8))]
                        let printElem =
                            match eType with
                            | AST.TInt64 -> runtimeInstrs (Runtime.generatePrintInt64NoNewline ())
                            | AST.TBool -> runtimeInstrs (Runtime.generatePrintBoolNoNewline ())
                            | AST.TFloat64 ->
                                [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (Runtime.generatePrintFloatNoNewline ())
                            | AST.TString | AST.TChar ->
                                [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 0s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 8us)] @
                                runtimeInstrs (Runtime.generatePrintStringNoNewline ())
                            | _ -> runtimeInstrs (Runtime.generatePrintInt64NoNewline ())
                        let comma = if i < List.length elemTypes - 1 then printTupleCommaSpace else []
                        loadElem @ printElem @ comma
                    )
                    |> List.concat

                // Print ")"
                let printCloseParen = [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ')'), 0)
                    ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
                    ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
                    ARM64Symbolic.SVC syscalls.SvcImmediate
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ]

                moveTupleToX21 @ printOpenParen @ tupleElemInstrs @ printCloseParen
            | _ ->
                // For other types (nested lists, etc.), print as integer for now
                runtimeInstrs (Runtime.generatePrintInt64NoNewline ())

        let elemPrintLen = List.length elemPrintCode

        // Print "[" - 9 instructions
        let printOpenBracket = [
            ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte '['), 0);
            ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);          // fd = stdout
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);    // buffer
            ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0);         // len = 1
            ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
            ARM64Symbolic.SVC syscalls.SvcImmediate;
            ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
        ]

        // Setup: X19 = list pointer, X20 = 1 (first element flag)
        let setup = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, listReg); ARM64Symbolic.MOVZ (ARM64Symbolic.X20, 1us, 0)]

        // Print ", " - used inside loop when not first element
        let printCommaSpace = [
            ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ','), 0);
            ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ' '), 0);
            ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 1);
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
            ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 2us, 0);
            ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
            ARM64Symbolic.SVC syscalls.SvcImmediate;
            ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
        ]
        let commaLen = List.length printCommaSpace

        // Loop structure:
        // loop_start:
        //   CBZ X19, loop_end           // if list == nil, exit
        //   CBNZ X20, skip_comma        // if first, skip comma
        //   <print ", ">
        // skip_comma:
        //   MOV X20, 0                  // first = false
        //   LDR X0, [X19, #8]           // X0 = head
        //   <print element>
        //   LDR X19, [X19, #16]         // X19 = tail
        //   B loop_start
        // loop_end:
        //   <print "]">

        // Calculate branch offsets
        // loopBodyLen = instructions after CBZ = CBNZ(1) + comma(11) + skipComma(2) + element(N) + loopEnd(2)
        let loopBodyLen = 1 + commaLen + 2 + elemPrintLen + 2
        // CBZ skips to loop_end (after B), which is at index loopBodyLen+1 (since CBZ is at index 0)
        let cbzOffset = loopBodyLen + 1
        // CBNZ skips commaLen instructions to reach skipComma
        let skipCommaOffset = commaLen

        let loopStart = [ARM64Symbolic.CBZ_offset (ARM64Symbolic.X19, cbzOffset); ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X20, skipCommaOffset)]
        let skipComma = [ARM64Symbolic.MOVZ (ARM64Symbolic.X20, 0us, 0); ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, 8s)]
        // B is at index loopBodyLen, jump back to CBZ at index 0
        let loopEnd = [ARM64Symbolic.LDR (ARM64Symbolic.X19, ARM64Symbolic.X19, 16s); ARM64Symbolic.B (-loopBodyLen)]
        let loopCode = loopStart @ printCommaSpace @ skipComma @ elemPrintCode @ loopEnd

        let printCloseBracket =
            if includeNewline then
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ']'), 0);
                    ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte '\n'), 0);
                    ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 1);
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 2us, 0);
                    ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                    ARM64Symbolic.SVC syscalls.SvcImmediate;
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ]
            else
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us);
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 (byte ']'), 0);
                    ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, 0);
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0);
                    ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                    ARM64Symbolic.SVC syscalls.SvcImmediate;
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                ]

        setup @ printOpenBracket @ loopCode @ printCloseBracket

    match instr with
    | LIR.Phi _ ->
        // Phi nodes should be eliminated before code generation (by register allocation)
        Error "Phi nodes should be eliminated before code generation"

    | LIR.FPhi _ ->
        // Float phi nodes should be eliminated before code generation (by register allocation)
        Error "Float phi nodes should be eliminated before code generation"

    | LIR.Mov (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match src with
            | LIR.Imm value ->
                Ok (loadImmediate destReg value)
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg srcReg ->
                lirRegToARM64Reg srcReg
                |> Result.map (fun srcARM64 ->
                    // Skip self-moves (can happen after register allocation coalesces VRegs)
                    if destReg = srcARM64 then []
                    else [ARM64Symbolic.MOV_reg (destReg, srcARM64)])
            | LIR.StackSlot offset ->
                // Load from stack slot into destination register
                loadStackSlot destReg offset
            | LIR.StringSymbol value ->
                // Convert literal string to heap string format when storing in variable
                // This ensures all string variables have consistent heap layout:
                // [length:8][data:N][refcount:8]
                //
                // Algorithm:
                // 1. Get literal string address and length
                // 2. Allocate heap: length + 16 bytes
                // 3. Store length at [heap]
                // 4. Copy bytes from pool to [heap+8]
                // 5. Store refcount=1 at [heap+8+aligned(length)]
                // 6. dest = heap address
                //
                // IMPORTANT: Use X13 for loop counter, not X0!
                // If destReg is X0, using X0 as loop counter would clobber the result.
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                Ok ([
                        // Load literal string address into X9
                        // Literal format: [length:8][data:N] - skip length prefix to get data address
                        ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                        ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // X9 = data address (skip 8-byte length)
                        // Allocate heap space (bump allocator)
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)  // dest = current heap pointer
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump pointer
                        // Store length
                    ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                        ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 0s)  // [dest] = length
                        // Copy bytes: loop counter in X13 (NOT X0 - it might be destReg!)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0us, 0)  // X13 = 0
                    ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                        // Loop start (if X13 >= len, done)
                        ARM64Symbolic.CMP_reg (ARM64Symbolic.X13, ARM64Symbolic.X11)
                        ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)  // Skip 7 instructions to exit loop (to after B)
                        ARM64Symbolic.LDRB (ARM64Symbolic.X15, ARM64Symbolic.X9, ARM64Symbolic.X13)  // X15 = pool[X13]
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, destReg, 8us)  // X12 = dest + 8
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X13)  // X12 = dest + 8 + X13
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X15, ARM64Symbolic.X12)  // [X12] = byte
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)  // X13++
                        ARM64Symbolic.B (-7)  // Loop back to CMP
                        // Store refcount at aligned offset
                        // aligned(x) = ((x + 7) >> 3) << 3
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X10, 7us)        // X12 = len + 7
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 3us, 0)                   // X15 = 3
                        ARM64Symbolic.LSR_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X15)  // X12 = (len + 7) >> 3
                        ARM64Symbolic.LSL_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X15)  // X12 = aligned(len)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, destReg, 8us)          // X15 = dest + 8
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X15, ARM64Symbolic.X12)  // X12 = dest + 8 + aligned(len)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)  // [X12] = 1
                    ] @ generateLeakCounterInc ctx)
            | LIR.FloatSymbol _ ->
                Error "Cannot MOV float reference - use FLoad instruction"
            | LIR.FuncAddr funcName ->
                // Load function address using ADR instruction
                Ok [ARM64Symbolic.ADR (destReg, codeLabel funcName)])

    | LIR.Store (offset, src) ->
        // Store register to stack slot
        lirRegToARM64Reg src
        |> Result.bind (fun srcReg -> storeStackSlot srcReg offset)

    | LIR.Add (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= 0L && value < 4096L ->
                    // Can use immediate ADD
                    Ok [ARM64Symbolic.ADD_imm (destReg, leftReg, uint16 value)]
                | LIR.Imm value ->
                    // Need to load immediate into register first
                    let tempReg = ARM64Symbolic.X9  // Use X9 as temp
                    Ok (loadImmediate tempReg value @ [ARM64Symbolic.ADD_reg (destReg, leftReg, tempReg)])
                | LIR.FloatImm _ ->
                    Error "Float code generation not yet implemented"
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64Symbolic.ADD_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot offset ->
                    // Load stack slot into temp register, then add
                    let tempReg = ARM64Symbolic.X9
                    loadStackSlot tempReg offset
                    |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64Symbolic.ADD_reg (destReg, leftReg, tempReg)])
                | LIR.StringSymbol _ ->
                    Error "Cannot use string reference in arithmetic operation"
                | LIR.FloatSymbol _ ->
                    Error "Cannot use float reference in integer arithmetic"
                | LIR.FuncAddr _ ->
                    Error "Cannot use function address in arithmetic operation"))

    | LIR.Sub (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= 0L && value < 4096L ->
                    Ok [ARM64Symbolic.SUB_imm (destReg, leftReg, uint16 value)]
                | LIR.Imm value ->
                    let tempReg = ARM64Symbolic.X9
                    Ok (loadImmediate tempReg value @ [ARM64Symbolic.SUB_reg (destReg, leftReg, tempReg)])
                | LIR.FloatImm _ ->
                    Error "Float code generation not yet implemented"
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64Symbolic.SUB_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot offset ->
                    // Load stack slot into temp register, then subtract
                    let tempReg = ARM64Symbolic.X9
                    loadStackSlot tempReg offset
                    |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64Symbolic.SUB_reg (destReg, leftReg, tempReg)])
                | LIR.StringSymbol _ ->
                    Error "Cannot use string reference in arithmetic operation"
                | LIR.FloatSymbol _ ->
                    Error "Cannot use float reference in integer arithmetic"
                | LIR.FuncAddr _ ->
                    Error "Cannot use function address in arithmetic operation"))


    | LIR.Mul (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.MUL (destReg, leftReg, rightReg)])))

    | LIR.Sdiv (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.SDIV (destReg, leftReg, rightReg)])))

    | LIR.Udiv (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.UDIV (destReg, leftReg, rightReg)])))

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        // MSUB: dest = sub - mulLeft * mulRight
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg mulLeft
            |> Result.bind (fun mulLeftReg ->
                lirRegToARM64Reg mulRight
                |> Result.bind (fun mulRightReg ->
                    lirRegToARM64Reg sub
                    |> Result.map (fun subReg ->
                        [ARM64Symbolic.MSUB (destReg, mulLeftReg, mulRightReg, subReg)]))))

    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        // MADD: dest = add + mulLeft * mulRight
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg mulLeft
            |> Result.bind (fun mulLeftReg ->
                lirRegToARM64Reg mulRight
                |> Result.bind (fun mulRightReg ->
                    lirRegToARM64Reg add
                    |> Result.map (fun addReg ->
                        [ARM64Symbolic.MADD (destReg, mulLeftReg, mulRightReg, addReg)]))))

    | LIR.Cmp (left, right) ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= 0L && value < 4096L ->
                Ok [ARM64Symbolic.CMP_imm (leftReg, uint16 value)]
            | LIR.Imm value ->
                let tempReg = ARM64Symbolic.X9
                Ok (loadImmediate tempReg value @ [ARM64Symbolic.CMP_reg (leftReg, tempReg)])
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [ARM64Symbolic.CMP_reg (leftReg, rightARM64)])
            | LIR.StackSlot offset ->
                // Load stack slot into temp register, then compare
                let tempReg = ARM64Symbolic.X9
                loadStackSlot tempReg offset
                |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64Symbolic.CMP_reg (leftReg, tempReg)])
            | LIR.StringSymbol _ ->
                Error "Cannot compare string references directly"
            | LIR.FloatSymbol _ ->
                Error "Cannot compare float references directly - use FCmp"
            | LIR.FuncAddr _ ->
                Error "Cannot compare function addresses directly")

    | LIR.Cset (dest, cond) ->
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            let arm64Cond =
                match cond with
                | LIR.EQ -> ARM64Symbolic.EQ
                | LIR.NE -> ARM64Symbolic.NE
                | LIR.LT -> ARM64Symbolic.LT
                | LIR.GT -> ARM64Symbolic.GT
                | LIR.LE -> ARM64Symbolic.LE
                | LIR.GE -> ARM64Symbolic.GE
                // TODO(#22): ARM64.Condition has no unsigned codes (needs LO/HI/LS/HS).
                // ARM64 is not the compiled/tested backend; map to signed to stay building.
                // This is knowingly wrong for UInt* on ARM64 only — x64 is correct.
                | LIR.ULT -> ARM64Symbolic.LT
                | LIR.UGT -> ARM64Symbolic.GT
                | LIR.ULE -> ARM64Symbolic.LE
                | LIR.UGE -> ARM64Symbolic.GE
            [ARM64Symbolic.CSET (destReg, arm64Cond)])

    | LIR.And (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.AND_reg (destReg, leftReg, rightReg)])))

    | LIR.And_imm (dest, src, imm) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.AND_imm (destReg, srcReg, uint64 imm)]))

    | LIR.Orr (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.ORR_reg (destReg, leftReg, rightReg)])))

    | LIR.Eor (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.EOR_reg (destReg, leftReg, rightReg)])))

    | LIR.Lsl (dest, src, shift) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.bind (fun srcReg ->
                lirRegToARM64Reg shift
                |> Result.map (fun shiftReg -> [ARM64Symbolic.LSL_reg (destReg, srcReg, shiftReg)])))

    | LIR.Lsr (dest, src, shift) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.bind (fun srcReg ->
                lirRegToARM64Reg shift
                |> Result.map (fun shiftReg -> [ARM64Symbolic.LSR_reg (destReg, srcReg, shiftReg)])))

    | LIR.Lsl_imm (dest, src, shift) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.LSL_imm (destReg, srcReg, shift)]))

    | LIR.Lsr_imm (dest, src, shift) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.LSR_imm (destReg, srcReg, shift)]))

    | LIR.Mvn (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.MVN (destReg, srcReg)]))

    // Sign/zero extension for integer overflow truncation
    | LIR.Sxtb (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.SXTB (destReg, srcReg)]))

    | LIR.Sxth (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.SXTH (destReg, srcReg)]))

    | LIR.Sxtw (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.SXTW (destReg, srcReg)]))

    | LIR.Uxtb (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.UXTB (destReg, srcReg)]))

    | LIR.Uxth (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.UXTH (destReg, srcReg)]))

    | LIR.Uxtw (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.UXTW (destReg, srcReg)]))

    | LIR.PrintBool reg ->
        // Print booleans as "true" or "false" (no exit)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64Symbolic.X0 then
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (Runtime.generatePrintBoolNoExit ())
            else
                runtimeInstrs (Runtime.generatePrintBoolNoExit ()))

    | LIR.PrintChars chars ->
        // Print literal characters (for tuple/list delimiters like "(", ", ", ")")
        Ok (runtimeInstrs (Runtime.generatePrintChars chars))

    | LIR.PrintBytes reg ->
        // Print bytes as "<N bytes>\n"
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64Symbolic.X19 then
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, regARM64)] @ runtimeInstrs (Runtime.generatePrintBytes ())
            else
                runtimeInstrs (Runtime.generatePrintBytes ()))

    | LIR.PrintInt64NoNewline reg ->
        // Print integer without newline (for tuple elements)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64Symbolic.X0 then
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (Runtime.generatePrintInt64NoNewline ())
            else
                runtimeInstrs (Runtime.generatePrintInt64NoNewline ()))

    | LIR.PrintBoolNoNewline reg ->
        // Print boolean without newline (for tuple elements)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64Symbolic.X0 then
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (Runtime.generatePrintBoolNoNewline ())
            else
                runtimeInstrs (Runtime.generatePrintBoolNoNewline ()))

    | LIR.PrintFloatNoNewline freg ->
        // Print float without newline (for tuple/list elements)
        lirFRegToARM64FReg freg
        |> Result.map (fun fregARM64 ->
            if fregARM64 <> ARM64Symbolic.D0 then
                [ARM64Symbolic.FMOV_reg (ARM64Symbolic.D0, fregARM64)] @ runtimeInstrs (Runtime.generatePrintFloatNoNewline ())
            else
                runtimeInstrs (Runtime.generatePrintFloatNoNewline ()))

    | LIR.PrintHeapStringNoNewline reg ->
        // Print heap string without newline (for tuple/list elements)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            // Heap string layout: [len:8 bytes][data:N bytes]
            let loadInstrs = [ARM64Symbolic.LDR (ARM64Symbolic.X10, regARM64, 0s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, regARM64, 8us)]
            let loadAndPrint = loadInstrs @ runtimeInstrs (Runtime.generatePrintStringNoNewline ())
            if regARM64 <> ARM64Symbolic.X9 then
                loadAndPrint
            else
                // Need to save the original address first
                let saveReg = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, regARM64)]
                let loadFromSaved = [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X11, 0s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X11, 8us)]
                saveReg @ loadFromSaved @ runtimeInstrs (Runtime.generatePrintStringNoNewline ()))

    | LIR.PrintList (listPtr, elemType) ->
        // Print list as [elem1, elem2, ...]
        // List layout: Nil = 0, Cons = [tag=1, head, tail]
        // Uses X19 for list pointer (callee-saved), X20 for first flag
        lirRegToARM64Reg listPtr
        |> Result.map (fun listReg -> generatePrintListInstrs listReg elemType true)

    | LIR.PrintSum (sumPtr, variants) ->
        // Print sum type: variant name + optional payload + newline
        // Sum layout depends on whether ANY variant has a payload:
        // - If any payload: [tag, payload] on heap
        // - If all nullary: just the tag value (integer)
        lirRegToARM64Reg sumPtr
        |> Result.map (fun sumReg ->
            let os =
                match Platform.detectOS () with
                | Ok platform -> platform
                | Error msg -> Crash.crash $"Platform detection failed: {msg}"
            let syscalls = ARM64.syscallConfigFor os

            // Check if any variant has a payload
            let hasAnyPayload = variants |> List.exists (fun (_, _, payload) -> Option.isSome payload)

            // Helper: generate code to print a string literal
            let printLiteral (s: string) =
                let bytes = System.Text.Encoding.UTF8.GetBytes(s)
                if bytes.Length = 0 then []
                else
                    let alignedSize = max 16 ((bytes.Length + 15) &&& ~~~15)
                    [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)] @
                    (bytes |> Array.toList |> List.mapi (fun i b ->
                        [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 b, 0); ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, i)]
                    ) |> List.concat) @
                    [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                     ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                     ARM64Symbolic.MOVZ (ARM64Symbolic.X2, uint16 bytes.Length, 0);
                     ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                     ARM64Symbolic.SVC syscalls.SvcImmediate;
                     ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)]

            // Setup depends on representation
            let setup =
                if hasAnyPayload then
                    // Heap-allocated: X19 = sum pointer, load tag from [X19, 0] into X20
                    [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, sumReg); ARM64Symbolic.LDR (ARM64Symbolic.X20, ARM64Symbolic.X19, 0s)]
                else
                    // All nullary: X19 = sum pointer (for consistency), X20 = tag (the value itself)
                    [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, sumReg); ARM64Symbolic.MOV_reg (ARM64Symbolic.X20, sumReg)]

            // Generate code for each variant: compare tag, branch, print name, optionally print payload
            // Structure: for each variant, generate:
            //   CMP X20, #tag
            //   B.NE next_variant
            //   <print variant name>
            //   <if payload: print "(", print payload, print ")">
            //   B end
            // next_variant:
            //   ... (repeat)
            // end:
            //   <print "\n">

            // Pre-calculate code blocks for each variant
            let variantBlocks =
                variants |> List.map (fun (variantName, _tag, payloadType) ->
                    let printName = printLiteral variantName
                    let printPayload =
                        match payloadType with
                        | None -> []
                        | Some pType ->
                            let printOpen = printLiteral "("
                            let loadPayload = [ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, 8s)]  // Load payload from offset 8
                            let printPayloadValue =
                                match pType with
                                | AST.TInt64 -> runtimeInstrs (Runtime.generatePrintInt64NoNewline ())
                                | AST.TBool -> runtimeInstrs (Runtime.generatePrintBoolNoNewline ())
                                | AST.TFloat64 ->
                                    [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (Runtime.generatePrintFloatNoNewline ())
                                | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 ->
                                    [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 0s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 8us)] @
                                    runtimeInstrs (Runtime.generatePrintStringNoNewline ())
                                | AST.TList elemType ->
                                    match getListDisplayStringFunc elemType with
                                    | Some funcName ->
                                        let callToDisplay = [ARM64Symbolic.BL funcName]
                                        let printString =
                                            [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 0s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 8us)] @
                                            runtimeInstrs (Runtime.generatePrintStringNoNewline ())
                                        callToDisplay @ printString
                                    | None ->
                                        Crash.crash $"Unsupported list element type in sum variant: {elemType}"
                                | t -> Crash.crash $"Unsupported payload type in sum variant: {t}"
                            let printClose = printLiteral ")"
                            printOpen @ loadPayload @ printPayloadValue @ printClose
                    (printName, printPayload))

            // Calculate end label offset from each variant block
            // We'll build the code and calculate offsets manually

            // Print newline at end
            let printNewline = printLiteral "\n"
            let endBlockLen = List.length printNewline

            // Build variant blocks with branching
            // For each variant: CMP(1) + B.NE(1) + name + payload + B(1) to end
            let mutable codeBlocks : ARM64Symbolic.Instr list list = []
            let mutable cumulativeOffset = 0

            // First pass: calculate total length to know where "end" is
            let blockLengths =
                variants
                |> List.mapi (fun i (_, _tag, _) ->
                    let (printName, printPayload) = variantBlocks.[i]
                    2 + List.length printName + List.length printPayload + 1)  // CMP + B.NE + name + payload + B

            let totalVariantCodeLen = List.sum blockLengths
            let endOffset = totalVariantCodeLen + endBlockLen

            // Second pass: build actual code with correct offsets
            let mutable currentPos = 0
            let variantCode =
                variants
                |> List.mapi (fun i (_, tag, _) ->
                    let (printName, printPayload) = variantBlocks.[i]
                    let blockLen = 2 + List.length printName + List.length printPayload + 1
                    // B.NE is at position 1, next block CMP is at position blockLen
                    // So offset = blockLen - 1 (forward jump from B.NE to next CMP)
                    let nextBlockOffset = blockLen - 1
                    let endFromHere = totalVariantCodeLen - currentPos - blockLen + 1  // Jump to after all variant blocks
                    currentPos <- currentPos + blockLen

                    let cmpInstr = ARM64Symbolic.CMP_imm (ARM64Symbolic.X20, uint16 tag)
                    let branchNeInstr = ARM64Symbolic.B_cond (ARM64Symbolic.NE, nextBlockOffset)  // Skip this variant's code
                    let branchEndInstr = ARM64Symbolic.B endFromHere  // Jump to end (after all variant code)

                    [cmpInstr; branchNeInstr] @ printName @ printPayload @ [branchEndInstr])
                |> List.concat

            setup @ variantCode @ printNewline)

    | LIR.PrintRecord (recordPtr, typeName, fields) ->
        // Print record: TypeName { field1 = val1, field2 = val2, ... }\n
        // Record layout: [field0, field1, field2, ...] on heap (each 8 bytes)
        lirRegToARM64Reg recordPtr
        |> Result.map (fun recordReg ->
            let os =
                match Platform.detectOS () with
                | Ok platform -> platform
                | Error msg -> Crash.crash $"Platform detection failed: {msg}"
            let syscalls = ARM64.syscallConfigFor os

            // Helper: generate code to print a string literal
            let printLiteral (s: string) =
                let bytes = System.Text.Encoding.UTF8.GetBytes(s)
                if bytes.Length = 0 then []
                else
                    let alignedSize = max 16 ((bytes.Length + 15) &&& ~~~15)
                    [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)] @
                    (bytes |> Array.toList |> List.mapi (fun i b ->
                        [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, uint16 b, 0); ARM64Symbolic.STRB (ARM64Symbolic.X0, ARM64Symbolic.SP, i)]
                    ) |> List.concat) @
                    [ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0);
                     ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP);
                     ARM64Symbolic.MOVZ (ARM64Symbolic.X2, uint16 bytes.Length, 0);
                     ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0);
                     ARM64Symbolic.SVC syscalls.SvcImmediate;
                     ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 alignedSize)]

            // Save record pointer in callee-saved register X19
            let setup = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, recordReg)]

            // Print type name and opening brace
            let printHeader = printLiteral (typeName + " { ")

            // Print each field: "fieldName = value" with ", " separator between fields
            let printFields =
                fields
                |> List.mapi (fun i (fieldName, fieldType) ->
                    let printFieldName = printLiteral (fieldName + " = ")
                    let offset = int16 (i * 8)  // Each field is 8 bytes
                    let loadField = [ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, offset)]
                    let printValue =
                        match fieldType with
                        | AST.TInt64 -> runtimeInstrs (Runtime.generatePrintInt64NoNewline ())
                        | AST.TBool -> runtimeInstrs (Runtime.generatePrintBoolNoNewline ())
                        | AST.TFloat64 ->
                            [ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D0, ARM64Symbolic.X0)] @ runtimeInstrs (Runtime.generatePrintFloatNoNewline ())
                        | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 ->
                            // String is a pointer: load length, compute data ptr, print
                            [ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X0, 0s); ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X0, 8us)] @
                            runtimeInstrs (Runtime.generatePrintStringNoNewline ())
                        | t -> Crash.crash $"Unsupported field type in record: {t}"
                    let separator =
                        if i < List.length fields - 1 then printLiteral ", "
                        else []
                    printFieldName @ loadField @ printValue @ separator)
                |> List.concat

            // Print closing brace and newline
            let printFooter = printLiteral " }\n"

            setup @ printHeader @ printFields @ printFooter)

    | LIR.Call (dest, funcName, args) ->
        // Function call: arguments already moved to X0-X7 by preceding MOVs
        // Caller-save is handled by SaveRegs/RestoreRegs instructions
        Ok [ARM64Symbolic.BL funcName]

    | LIR.TailCall (funcName, args) ->
        // Tail call: restore stack frame, then branch (no link)
        // This is the same as the epilogue but with B instead of RET
        let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
        let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
        // Deallocate all stack at once
        let totalExtraStack = ctx.StackSize + calleeSavedSpace
        let deallocStack =
            if totalExtraStack > 0 then
                [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
            else
                []
        let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
        let branch = [ARM64Symbolic.B_label funcName]
        Ok (restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

    | LIR.IndirectCall (dest, func, args) ->
        // Indirect call: call through function pointer in register
        // Use BLR instruction instead of BL
        lirRegToARM64Reg func
        |> Result.map (fun funcReg -> [ARM64Symbolic.BLR funcReg])

    | LIR.IndirectTailCall (func, args) ->
        // Indirect tail call: restore stack frame, then branch to register
        lirRegToARM64Reg func
        |> Result.map (fun funcReg ->
            let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
            let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
            let totalExtraStack = ctx.StackSize + calleeSavedSpace
            let deallocStack =
                if totalExtraStack > 0 then
                    [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
                else
                    []
            let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
            let branch = [ARM64Symbolic.BR funcReg]
            restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

    | LIR.ClosureAlloc (dest, funcName, captures) ->
        // Allocate closure on heap: (func_ptr, cap1, cap2, ...)
        // Each slot is 8 bytes
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            let numSlots = 1 + List.length captures  // func_ptr + captures
            let sizeBytes = numSlots * 8
            // Total size includes 8 bytes for ref count, aligned to 8 bytes
            let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)

            // Allocate using bump allocator
            let allocInstrs = [
                ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)                      // dest = current heap pointer
                ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)                          // X15 = 1 (initial ref count)
                ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X28, int16 sizeBytes)       // store ref count after payload
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump pointer
            ]

            // Store function address at offset 0
            let storeFuncAddr = [
                ARM64Symbolic.ADR (ARM64Symbolic.X15, codeLabel funcName)               // X15 = function address
                ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, 0s)                      // [dest] = func_ptr
            ]

            // Store captures at subsequent offsets
            let storeCaptures =
                captures
                |> List.mapi (fun i cap -> (i, cap))
                |> List.collect (fun (i, cap) ->
                    let offset = (i + 1) * 8
                    match cap with
                    | LIR.Imm value ->
                        loadImmediate ARM64Symbolic.X15 value @
                        [ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, int16 offset)]
                    | LIR.Reg reg ->
                        match lirRegToARM64Reg reg with
                        | Ok srcReg ->
                            // Avoid storing dest into itself at offset
                            if srcReg = destReg then
                                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, srcReg); ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, int16 offset)]
                            else
                                [ARM64Symbolic.STR (srcReg, destReg, int16 offset)]
                        | Error msg -> Crash.crash $"ClosureAlloc: lirRegToARM64Reg failed: {msg}"
                    | LIR.FuncAddr fname ->
                        [ARM64Symbolic.ADR (ARM64Symbolic.X15, codeLabel fname); ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, int16 offset)]
                    | other -> Crash.crash $"ClosureAlloc: Unexpected capture operand type: {other}")

            Ok (allocInstrs @ generateLeakCounterInc ctx @ storeFuncAddr @ storeCaptures))

    | LIR.ClosureCall (dest, funcPtr, args) ->
        // Call through closure - MIR_to_LIR already set up:
        // - X9: function pointer (loaded from closure[0])
        // - X0: closure
        // - X1-X7: args
        // Just do the BLR
        lirRegToARM64Reg funcPtr
        |> Result.map (fun funcPtrReg ->
            [ARM64Symbolic.BLR funcPtrReg])

    | LIR.ClosureTailCall (funcPtr, args) ->
        // Closure tail call: restore stack frame, then branch to register
        lirRegToARM64Reg funcPtr
        |> Result.map (fun funcPtrReg ->
            let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
            let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
            let totalExtraStack = ctx.StackSize + calleeSavedSpace
            let deallocStack =
                if totalExtraStack > 0 then
                    [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
                else
                    []
            let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
            let branch = [ARM64Symbolic.BR funcPtrReg]
            restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

    | LIR.SaveRegs (intRegs, floatRegs) ->
        // Save only the caller-saved registers that are live across this call
        // We maintain fixed offsets for ArgMoves compatibility:
        // Layout: X1-X10 at SP+0..SP+72 (fixed), D0-D7 at SP+80..SP+136
        // If no registers need saving, emit nothing (no stack allocation)
        if List.isEmpty intRegs && List.isEmpty floatRegs then
            Ok []  // Nothing to save - no stack allocation needed
        else
            // Determine stack size - we need fixed layout for ArgMoves compatibility
            // when any int registers are saved
            let hasIntRegs = not (List.isEmpty intRegs)
            let hasFloatRegs = not (List.isEmpty floatRegs)
            let intSlotSize = if hasIntRegs then 80 else 0  // X1-X10 (10 regs * 8 bytes)
            let floatSlotSize = if hasFloatRegs then 64 else 0  // D0-D7 (8 regs * 8 bytes)
            let totalSize = intSlotSize + floatSlotSize

            let allocStack = [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalSize)]

            // Save int registers using STP pairs where possible
            // Pairs: (X1,X2)@0, (X3,X4)@16, (X5,X6)@32, (X7,X8)@48, (X9,X10)@64
            let intPairs = [
                (LIR.X1, LIR.X2, 0s)
                (LIR.X3, LIR.X4, 16s)
                (LIR.X5, LIR.X6, 32s)
                (LIR.X7, LIR.X8, 48s)
                (LIR.X9, LIR.X10, 64s)
            ]

            let intSaves : ARM64Symbolic.Instr list =
                intPairs |> List.collect (fun (r1, r2, offset) ->
                    let has1 = List.contains r1 intRegs
                    let has2 = List.contains r2 intRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use STP
                        [ARM64Symbolic.STP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset)]
                    | (true, false) ->
                        // Only first register - use STR
                        [ARM64Symbolic.STR (lirPhysRegToARM64Reg r1, ARM64Symbolic.SP, offset)]
                    | (false, true) ->
                        // Only second register - use STR
                        [ARM64Symbolic.STR (lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            // Save float registers using STP_fp pairs where possible
            // Pairs: (D0,D1)@0, (D2,D3)@16, (D4,D5)@32, (D6,D7)@48
            let baseFloatOffset = if hasIntRegs then 80s else 0s
            let floatPairs = [
                (LIR.D0, LIR.D1, 0s)
                (LIR.D2, LIR.D3, 16s)
                (LIR.D4, LIR.D5, 32s)
                (LIR.D6, LIR.D7, 48s)
            ]

            let floatSaves : ARM64Symbolic.Instr list =
                floatPairs |> List.collect (fun (f1, f2, offset) ->
                    let has1 = List.contains f1 floatRegs
                    let has2 = List.contains f2 floatRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use STP_fp
                        [ARM64Symbolic.STP_fp (lirPhysFPRegToARM64FReg f1, lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset)]
                    | (true, false) ->
                        // Only first register - use STR_fp
                        [ARM64Symbolic.STR_fp (lirPhysFPRegToARM64FReg f1, ARM64Symbolic.SP, baseFloatOffset + offset)]
                    | (false, true) ->
                        // Only second register - use STR_fp
                        [ARM64Symbolic.STR_fp (lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            Ok (allocStack @ intSaves @ floatSaves)

    | LIR.RestoreRegs (intRegs, floatRegs) ->
        // Restore only the caller-saved registers that are live across this call
        // Must match the layout from SaveRegs
        if List.isEmpty intRegs && List.isEmpty floatRegs then
            Ok []  // Nothing was saved - no stack deallocation needed
        else
            let hasIntRegs = not (List.isEmpty intRegs)
            let hasFloatRegs = not (List.isEmpty floatRegs)
            let intSlotSize = if hasIntRegs then 80 else 0
            let floatSlotSize = if hasFloatRegs then 64 else 0
            let totalSize = intSlotSize + floatSlotSize

            // Restore int registers using LDP pairs where possible
            // Pairs: (X1,X2)@0, (X3,X4)@16, (X5,X6)@32, (X7,X8)@48, (X9,X10)@64
            let intPairs = [
                (LIR.X1, LIR.X2, 0s)
                (LIR.X3, LIR.X4, 16s)
                (LIR.X5, LIR.X6, 32s)
                (LIR.X7, LIR.X8, 48s)
                (LIR.X9, LIR.X10, 64s)
            ]

            let intRestores : ARM64Symbolic.Instr list =
                intPairs |> List.collect (fun (r1, r2, offset) ->
                    let has1 = List.contains r1 intRegs
                    let has2 = List.contains r2 intRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use LDP
                        [ARM64Symbolic.LDP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset)]
                    | (true, false) ->
                        // Only first register - use LDR
                        [ARM64Symbolic.LDR (lirPhysRegToARM64Reg r1, ARM64Symbolic.SP, offset)]
                    | (false, true) ->
                        // Only second register - use LDR
                        [ARM64Symbolic.LDR (lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            // Restore float registers using LDP_fp pairs where possible
            // Pairs: (D0,D1)@0, (D2,D3)@16, (D4,D5)@32, (D6,D7)@48
            let baseFloatOffset = if hasIntRegs then 80s else 0s
            let floatPairs = [
                (LIR.D0, LIR.D1, 0s)
                (LIR.D2, LIR.D3, 16s)
                (LIR.D4, LIR.D5, 32s)
                (LIR.D6, LIR.D7, 48s)
            ]

            let floatRestores : ARM64Symbolic.Instr list =
                floatPairs |> List.collect (fun (f1, f2, offset) ->
                    let has1 = List.contains f1 floatRegs
                    let has2 = List.contains f2 floatRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use LDP_fp
                        [ARM64Symbolic.LDP_fp (lirPhysFPRegToARM64FReg f1, lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset)]
                    | (true, false) ->
                        // Only first register - use LDR_fp
                        [ARM64Symbolic.LDR_fp (lirPhysFPRegToARM64FReg f1, ARM64Symbolic.SP, baseFloatOffset + offset)]
                    | (false, true) ->
                        // Only second register - use LDR_fp
                        [ARM64Symbolic.LDR_fp (lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            let deallocStack = [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalSize)]

            Ok (intRestores @ floatRestores @ deallocStack)

    | LIR.ArgMoves moves ->
        // Parallel move resolution for function arguments
        // After SaveRegs, X1-X10 are saved at [SP+0..SP+72]
        // If source is in X1-X7 and could be clobbered, load from stack instead
        //
        // Stack layout after SaveRegs: X1@[SP+0], X2@[SP+8], ..., X10@[SP+72]
        let saveRegsOffset (reg: LIR.PhysReg) : int option =
            match reg with
            | LIR.X1 -> Some 0
            | LIR.X2 -> Some 8
            | LIR.X3 -> Some 16
            | LIR.X4 -> Some 24
            | LIR.X5 -> Some 32
            | LIR.X6 -> Some 40
            | LIR.X7 -> Some 48
            | LIR.X8 -> Some 56
            | LIR.X9 -> Some 64
            | LIR.X10 -> Some 72
            | _ -> None

        // Find which destination registers (X0-X7) will be written
        let destRegs = moves |> List.map fst |> Set.ofList

        // For each move, determine how to execute it safely
        let generateMove (destReg: LIR.PhysReg, srcOp: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
            let destARM64 = lirPhysRegToARM64Reg destReg
            match srcOp with
            | LIR.Imm value ->
                Ok (loadImmediate destARM64 value)
            | LIR.Reg (LIR.Physical srcPhysReg) ->
                // If source equals destination, it's a no-op
                if srcPhysReg = destReg then
                    Ok []
                else
                    // Check if source register will be clobbered by an earlier move
                    // A register is clobbered if it's a destination of a move to a LOWER index
                    // (since we process X0, X1, X2, ... in order)
                    let srcWillBeClobbered =
                        match srcPhysReg with
                        | LIR.X1 | LIR.X2 | LIR.X3 | LIR.X4 | LIR.X5 | LIR.X6 | LIR.X7 ->
                            Set.contains srcPhysReg destRegs
                        | _ -> false
                    if srcWillBeClobbered then
                        // Load from SaveRegs stack instead of live register
                        match saveRegsOffset srcPhysReg with
                        | Some offset ->
                            Ok [ARM64Symbolic.LDR (destARM64, ARM64Symbolic.SP, int16 offset)]
                        | None ->
                            Error $"ArgMoves: Source register {srcPhysReg} will be clobbered but has no SaveRegs offset"
                    else
                        let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
                        Ok [ARM64Symbolic.MOV_reg (destARM64, srcARM64)]
            | LIR.Reg (LIR.Virtual _) ->
                Error "Virtual register in ArgMoves - should have been allocated"
            | LIR.StackSlot offset ->
                loadStackSlot destARM64 offset
            | LIR.StringSymbol value ->
                // Convert literal string to heap format for function arguments
                // Functions expect heap strings: [length:8][data:N][refcount:8]
                // Literal data uses [length:8][data:N] - skip length to copy data
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                Ok ([
                    // Load literal string data address into X9 (skip 8-byte length prefix)
                    ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip length prefix
                    // Allocate heap space (bump allocator)
                    ARM64Symbolic.MOV_reg (destARM64, ARM64Symbolic.X28)  // dest = current heap pointer
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump pointer
                    // Store length
                ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                    ARM64Symbolic.STR (ARM64Symbolic.X10, destARM64, 0s)  // [dest] = length
                    // Copy bytes: loop counter in X13 (NOT X0-X7 - those are arg registers!)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0us, 0)  // X13 = 0
                ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                    // Loop start (if X13 >= len, done)
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X13, ARM64Symbolic.X11)
                    ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)  // Skip 7 instructions to exit loop
                    ARM64Symbolic.LDRB (ARM64Symbolic.X15, ARM64Symbolic.X9, ARM64Symbolic.X13)  // X15 = literal[X13]
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, destARM64, 8us)  // X12 = dest + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X13)  // X12 = dest + 8 + X13
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X15, ARM64Symbolic.X12)  // [X12] = byte
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)  // X13++
                    ARM64Symbolic.B (-7)  // Loop back to CMP
                    // Store refcount at aligned offset
                    // aligned(x) = ((x + 7) >> 3) << 3
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X10, 7us)        // X12 = len + 7
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 3us, 0)                   // X15 = 3
                    ARM64Symbolic.LSR_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X15)  // X12 = (len + 7) >> 3
                    ARM64Symbolic.LSL_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X15)  // X12 = aligned(len)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, destARM64, 8us)        // X15 = dest + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X15, ARM64Symbolic.X12)  // X12 = dest + 8 + aligned(len)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)  // [X12] = 1
                ] @ generateLeakCounterInc ctx)
            | LIR.FuncAddr funcName ->
                Ok [ARM64Symbolic.ADR (destARM64, codeLabel funcName)]
            | LIR.FloatImm _ | LIR.FloatSymbol _ ->
                Error "Float in ArgMoves not yet supported"

        // Generate all moves in order (X0, X1, X2, ...)
        let moveInstrs =
            moves
            |> List.sortBy (fun (destReg, _) ->
                match destReg with
                | LIR.X0 -> 0 | LIR.X1 -> 1 | LIR.X2 -> 2 | LIR.X3 -> 3
                | LIR.X4 -> 4 | LIR.X5 -> 5 | LIR.X6 -> 6 | LIR.X7 -> 7
                | _ -> 100)
            |> List.map generateMove
            |> List.fold (fun acc r ->
                match acc, r with
                | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
                | Error e, _ -> Error e
                | _, Error e -> Error e) (Ok [])

        moveInstrs

    | LIR.TailArgMoves moves ->
        // Parallel move resolution for TAIL CALL arguments
        // Unlike ArgMoves, there is NO SaveRegs, so we can't load from stack.
        // We use the shared ParallelMoves module with X16 as the temp register.

        // Helper to get source register if operand is a physical register
        let getSrcPhysReg (srcOp: LIR.Operand) : LIR.PhysReg option =
            match srcOp with
            | LIR.Reg (LIR.Physical srcPhysReg) -> Some srcPhysReg
            | _ -> None

        // Generate a single move instruction (for non-register sources)
        let generateMoveInstr (destReg: LIR.PhysReg, srcOp: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
            let destARM64 = lirPhysRegToARM64Reg destReg
            match srcOp with
            | LIR.Imm value ->
                Ok (loadImmediate destARM64 value)
            | LIR.Reg (LIR.Physical srcPhysReg) ->
                let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
                Ok [ARM64Symbolic.MOV_reg (destARM64, srcARM64)]
            | LIR.Reg (LIR.Virtual _) ->
                Error "Virtual register in TailArgMoves - should have been allocated"
            | LIR.StackSlot offset ->
                loadStackSlot destARM64 offset
            | LIR.FuncAddr funcName ->
                Ok [ARM64Symbolic.ADR (destARM64, codeLabel funcName)]
            | LIR.StringSymbol value ->
                // Convert literal string to heap format for tail call arguments
                // Same pattern as ArgMoves - functions expect heap strings
                // Literal data uses: [length:8][data:N] - skip length to copy data
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                Ok ([
                    // Load literal string data address into X9 (skip 8-byte length prefix)
                    ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip length prefix
                    // Allocate heap space (bump allocator)
                    ARM64Symbolic.MOV_reg (destARM64, ARM64Symbolic.X28)  // dest = current heap pointer
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump pointer
                    // Store length
                ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                    ARM64Symbolic.STR (ARM64Symbolic.X10, destARM64, 0s)  // [dest] = length
                    // Copy bytes: loop counter in X13
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0us, 0)  // X13 = 0
                ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                    // Loop start (if X13 >= len, done)
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X13, ARM64Symbolic.X11)
                    ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)  // Skip 7 instructions to exit loop
                    ARM64Symbolic.LDRB (ARM64Symbolic.X15, ARM64Symbolic.X9, ARM64Symbolic.X13)  // X15 = literal[X13]
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, destARM64, 8us)  // X12 = dest + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X13)  // X12 = dest + 8 + X13
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X15, ARM64Symbolic.X12)  // [X12] = byte
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)  // X13++
                    ARM64Symbolic.B (-7)  // Loop back to CMP
                    // Store refcount
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, destARM64, 8us)
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.X10)  // X12 = dest + 8 + len
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)  // [X12] = 1
                ] @ generateLeakCounterInc ctx)
            | LIR.FloatImm _ | LIR.FloatSymbol _ ->
                Error "Float in TailArgMoves not yet supported"

        // Use the shared parallel move resolution algorithm
        let actions = ParallelMoves.resolve moves getSrcPhysReg

        // Convert actions to ARM64 instructions
        let mutable allInstrs : ARM64Symbolic.Instr list = []
        let mutable error : string option = None

        for action in actions do
            if error.IsNone then
                match action with
                | ParallelMoves.SaveToTemp reg ->
                    // Save register to X16 (temp)
                    allInstrs <- allInstrs @ [ARM64Symbolic.MOV_reg (ARM64Symbolic.X16, lirPhysRegToARM64Reg reg)]
                | ParallelMoves.Move (dest, src) ->
                    match generateMoveInstr (dest, src) with
                    | Ok instrs -> allInstrs <- allInstrs @ instrs
                    | Error e -> error <- Some e
                | ParallelMoves.MoveFromTemp dest ->
                    // Move from X16 (temp) to destination
                    allInstrs <- allInstrs @ [ARM64Symbolic.MOV_reg (lirPhysRegToARM64Reg dest, ARM64Symbolic.X16)]

        match error with
        | Some e -> Error e
        | None -> Ok allInstrs

    | LIR.FArgMoves moves ->
        // Float argument moves - move float values to D0-D7
        // Uses parallel move resolution to handle register conflicts correctly

        // First, convert all source FRegs to ARM64 FRegs
        let resolvedMoves =
            moves
            |> List.map (fun (destPhysReg, srcFReg) ->
                let destARM64 = lirPhysFPRegToARM64FReg destPhysReg
                match lirFRegToARM64FReg srcFReg with
                | Ok srcARM64 -> Ok (destARM64, srcARM64)
                | Error e -> Error e)
            |> List.fold (fun acc r ->
                match acc, r with
                | Ok moves, Ok move -> Ok (move :: moves)
                | Error e, _ -> Error e
                | _, Error e -> Error e) (Ok [])
            |> Result.map List.rev

        match resolvedMoves with
        | Error e -> Error e
        | Ok armMoves ->
            // Use ParallelMoves.resolve to get the correct move order
            // We treat ARM64Symbolic.FReg as both dest and src type
            let getSrcReg (srcReg: ARM64Symbolic.FReg) : ARM64Symbolic.FReg option = Some srcReg
            let actions = ParallelMoves.resolve armMoves getSrcReg

            // Convert actions to ARM64 instructions
            // Use D16 as temp register for cycle breaking
            // D16-D31 are the upper half of the SIMD register file, not used elsewhere
            let mutable allInstrs : ARM64Symbolic.Instr list = []
            for action in actions do
                match action with
                | ParallelMoves.SaveToTemp srcReg ->
                    // Save to D16 (temp) - using upper SIMD register
                    allInstrs <- allInstrs @ [ARM64Symbolic.FMOV_reg (ARM64Symbolic.D16, srcReg)]
                | ParallelMoves.Move (dest, src) ->
                    if dest <> src then
                        allInstrs <- allInstrs @ [ARM64Symbolic.FMOV_reg (dest, src)]
                | ParallelMoves.MoveFromTemp dest ->
                    // Move from D16 (temp) to destination
                    allInstrs <- allInstrs @ [ARM64Symbolic.FMOV_reg (dest, ARM64Symbolic.D16)]

            Ok allInstrs

    | LIR.PrintInt64 reg ->
        // Value to print should be in X0 (no exit)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64Symbolic.X0 then
                // Move to X0 if not already there
                [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, regARM64)] @ runtimeInstrs (Runtime.generatePrintInt64NoExit ())
            else
                runtimeInstrs (Runtime.generatePrintInt64NoExit ()))

    | LIR.Exit ->
        // Exit program with code 0
        Ok (runtimeInstrs (Runtime.generateExit ()))

    | LIR.PrintFloat freg ->
        // Print float value from FP register
        // Value should be in D0 for generatePrintFloat
        lirFRegToARM64FReg freg
        |> Result.map (fun fregARM64 ->
            if fregARM64 <> ARM64Symbolic.D0 then
                // Move to D0 if not already there
                [ARM64Symbolic.FMOV_reg (ARM64Symbolic.D0, fregARM64)] @ runtimeInstrs (Runtime.generatePrintFloat ())
            else
                runtimeInstrs (Runtime.generatePrintFloat ()))

    | LIR.PrintString value ->
        // To print a string, we need:
        // 1. ADRP + ADD to load string address into X0
        // 2. Call Runtime.generatePrintString which handles write syscall
        let len = utf8Len value
        let labelRef = stringDataLabel value
        Ok ([
            ARM64Symbolic.ADRP (ARM64Symbolic.X0, labelRef)  // Load page address of string
            ARM64Symbolic.ADD_label (ARM64Symbolic.X0, ARM64Symbolic.X0, labelRef)  // Add page offset
        ] @ runtimeInstrs (Runtime.generatePrintString len))

    | LIR.RuntimeError message ->
        let os =
            match Platform.detectOS () with
            | Ok platform -> platform
            | Error msg -> Crash.crash $"Platform detection failed: {msg}"
        let syscalls = ARM64.syscallConfigFor os
        let messageBytes =
            System.Text.Encoding.UTF8.GetBytes(message)
            |> Array.toList
        Ok (
            runtimeInstrs (Runtime.generatePrintCharsToStderr messageBytes)
            @ [
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)  // exit code = 1
                ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
                ARM64Symbolic.SVC syscalls.SvcImmediate
            ]
        )

    // Floating-point instructions
    | LIR.FMov (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_reg (destReg, srcReg)]))

    | LIR.FLoad (dest, value) ->
        // Load float from literal pool into FP register
        lirFRegToARM64FReg dest
        |> Result.map (fun destReg ->
            let labelRef = floatDataLabel value
            [
                ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)           // Load page address of float
                ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)  // Add page offset
                ARM64Symbolic.LDR_fp (destReg, ARM64Symbolic.X9, 0s)        // Load float from [X9]
            ])

    | LIR.FAdd (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.FADD (destReg, leftReg, rightReg)])))

    | LIR.FSub (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.FSUB (destReg, leftReg, rightReg)])))

    | LIR.FMul (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.FMUL (destReg, leftReg, rightReg)])))

    | LIR.FDiv (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64Symbolic.FDIV (destReg, leftReg, rightReg)])))

    | LIR.FNeg (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FNEG (destReg, srcReg)]))

    | LIR.FAbs (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FABS (destReg, srcReg)]))

    | LIR.FSqrt (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FSQRT (destReg, srcReg)]))

    | LIR.FCmp (left, right) ->
        lirFRegToARM64FReg left
        |> Result.bind (fun leftReg ->
            lirFRegToARM64FReg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.FCMP (leftReg, rightReg)]))

    | LIR.Int64ToFloat (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.SCVTF (destReg, srcReg)]))

    | LIR.FloatToInt64 (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FCVTZS (destReg, srcReg)]))

    | LIR.GpToFp (dest, src) ->
        // Move bits from GP register to FP register (for floats loaded from heap)
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_from_gp (destReg, srcReg)]))

    | LIR.FpToGp (dest, src) ->
        // Move bits from FP register to GP register (for floats stored to list)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_to_gp (destReg, srcReg)]))

    | LIR.FloatToBits (dest, src) ->
        // Copy Float64 bits to UInt64 (uses FMOV to GP register)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_to_gp (destReg, srcReg)]))

    // Heap operations
    | LIR.HeapAlloc (dest, sizeBytes) ->
        // Heap allocator with free list support
        // X27 = free list heads base, X28 = bump allocator pointer
        //
        // Memory layout with reference counting:
        //   [payload: sizeBytes][refcount: 8 bytes]
        //
        // Algorithm:
        // 1. Check free list for this size class (sizeClassOffset = sizeBytes)
        // 2. If free list non-empty: pop from list, initialize refcount, return
        // 3. If empty: bump allocate from X28
        //
        // Code structure (10 instructions):
        //   LDR X15, [X27, sizeBytes]         ; Load free list head
        //   CBZ X15, +5                       ; If empty, skip to bump alloc (5 instrs)
        //   MOV dest, X15                     ; dest = freed block
        //   LDR X14, [X15, 0]                 ; Load next pointer from freed block
        //   STR X14, [X27, sizeBytes]         ; Update free list head
        //   MOVZ X14, 1                       ; X14 = 1 (initial ref count)
        //   STR X14, [dest, sizeBytes]        ; Store ref count
        //   B +5                              ; Skip bump allocator (5 instrs)
        //   ; Bump allocator:
        //   MOV dest, X28                     ; dest = current heap pointer
        //   MOVZ X15, 1                       ; X15 = 1 (initial ref count)
        //   STR X15, [X28, sizeBytes]         ; store ref count after payload
        //   ADD X28, X28, totalSize           ; bump pointer
        //   (continue)
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            // Total size includes 8 bytes for ref count, aligned to 8 bytes
            let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)
            if ctx.Options.DisableFreeList then
                // Bump allocator only (no free list reuse)
                (withHeapBoundsCheck
                    ctx.HeapOverflowLabel
                    [ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X28, uint16 totalSize)]
                    [
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)                  // dest = current heap pointer
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)                      // X15 = 1 (initial ref count)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X28, int16 sizeBytes)   // store ref count after payload
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize) // bump pointer
                    ])
                @ generateLeakCounterInc ctx
            else
                // Full allocator with free list support
                let popFreeList = [
                    ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X15)                  // dest = freed block
                    ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X15, 0s)        // Load next pointer
                    ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X27, int16 sizeBytes)   // Update free list head
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X14, 1us, 0)                       // X14 = 1 (initial ref count)
                    ARM64Symbolic.STR (ARM64Symbolic.X14, destReg, int16 sizeBytes)      // Store ref count
                ]

                let bumpAlloc =
                    withHeapBoundsCheck
                        ctx.HeapOverflowLabel
                        [ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X28, uint16 totalSize)]
                        [
                            ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)                  // dest = current heap pointer
                            ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)                      // X15 = 1 (initial ref count)
                            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X28, int16 sizeBytes)   // store ref count after payload
                            ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize) // bump pointer
                        ]

                [
                    ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X27, int16 sizeBytes)   // Load free list head
                    ARM64Symbolic.CBZ_offset (ARM64Symbolic.X15, popFreeList.Length + 2)         // If empty, jump past pop path + branch to bump alloc
                ]
                @ popFreeList
                // B uses a current-PC-relative instruction offset, so skipping N instructions needs N + 1.
                @ [ARM64Symbolic.B (bumpAlloc.Length + 1)]
                @ bumpAlloc
                @ generateLeakCounterInc ctx)

    | LIR.HeapStore (addr, offset, src, valueType) ->
        // Store value at addr + offset (offset is in bytes)
        lirRegToARM64Reg addr
        |> Result.bind (fun addrReg ->
            match src, valueType with
            | LIR.Imm value, _ ->
                // Load immediate into temp register, then store
                let tempReg = ARM64Symbolic.X9
                Ok (loadImmediate tempReg value @
                    [ARM64Symbolic.STR (tempReg, addrReg, int16 offset)])
            | LIR.Reg srcReg, Some AST.TFloat64 ->
                // Float value in register: interpret as FReg and use STR_fp
                // The srcReg ID is actually an FVirtual, convert to ARM64 FP register
                lirFRegToARM64FReg (virtualToFVirtual srcReg)
                |> Result.map (fun srcARM64FP ->
                    [ARM64Symbolic.STR_fp (srcARM64FP, addrReg, int16 offset)])
            | LIR.Reg srcReg, _ ->
                lirRegToARM64Reg srcReg
                |> Result.map (fun srcARM64 ->
                    // If src and addr are the same register, we have a problem
                    // due to register allocation bug. Use temp register as workaround.
                    if srcARM64 = addrReg then
                        // Save value to temp, use temp for store
                        let tempReg = ARM64Symbolic.X9
                        [ARM64Symbolic.MOV_reg (tempReg, srcARM64); ARM64Symbolic.STR (tempReg, addrReg, int16 offset)]
                    else
                        [ARM64Symbolic.STR (srcARM64, addrReg, int16 offset)])
            | LIR.StackSlot slotOffset, _ ->
                // Load from stack slot into temp, then store to heap
                let tempReg = ARM64Symbolic.X9
                loadStackSlot tempReg slotOffset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ [ARM64Symbolic.STR (tempReg, addrReg, int16 offset)])
            | LIR.FuncAddr funcName, _ ->
                // Load function address into temp, then store to heap
                let tempReg = ARM64Symbolic.X9
                Ok [ARM64Symbolic.ADR (tempReg, codeLabel funcName); ARM64Symbolic.STR (tempReg, addrReg, int16 offset)]
            | LIR.StringSymbol value, _ ->
                // Convert literal string to heap format when storing in tuples/data structures
                // Heap strings: [length:8][data:N][refcount:8]
                // Literal data: [length:8][data:N]
                // We must convert because tuple extraction expects heap format
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                Ok ([
                    // Load literal string address into X10
                    // Literal format [length:8][data:N] - we need the data pointer
                    ARM64Symbolic.ADRP (ARM64Symbolic.X10, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X10, ARM64Symbolic.X10, labelRef)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 8us)  // Skip 8-byte length prefix to point at data
                    // Allocate heap space (bump allocator), store address in X9
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X28)  // X9 = current heap pointer (result)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump pointer
                    // Store length (known at compile time)
                ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                    ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.X9, 0s)   // Store length at heap[0]
                    // Copy bytes: counter in X13, limit in X11
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0us, 0)  // X13 = 0
                    // Loop start (if X13 >= len, done)
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X13, ARM64Symbolic.X11)
                    ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)  // Skip 7 instructions to exit loop
                    ARM64Symbolic.LDRB (ARM64Symbolic.X15, ARM64Symbolic.X10, ARM64Symbolic.X13)  // X15 = literal[X13]
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X9, 8us)  // X14 = heap + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X13)  // X14 = heap + 8 + X13
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X15, ARM64Symbolic.X14)  // heap_data[X13] = byte
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)  // X13++
                    ARM64Symbolic.B (-7)  // Loop back to CMP
                    // Store refcount at aligned offset
                    // aligned(x) = ((x + 7) >> 3) << 3
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X11, 7us)        // X14 = len + 7
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 3us, 0)                   // X15 = 3
                    ARM64Symbolic.LSR_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X15)  // X14 = (len + 7) >> 3
                    ARM64Symbolic.LSL_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X15)  // X14 = aligned(len)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X9, 8us)         // X15 = heap + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.X14)  // X14 = heap + 8 + aligned(len)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)  // refcount = 1
                    // Store heap string address to tuple slot
                    ARM64Symbolic.STR (ARM64Symbolic.X9, addrReg, int16 offset)
                ] @ generateLeakCounterInc ctx)
            | LIR.FloatSymbol value, _ ->
                // Load float literal from pool into temp FP register, then store to heap
                let labelRef = floatDataLabel value
                Ok [
                    ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)              // Load page address
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef) // Add offset
                    ARM64Symbolic.LDR_fp (ARM64Symbolic.D15, ARM64Symbolic.X9, 0s)         // Load float into D15
                    ARM64Symbolic.STR_fp (ARM64Symbolic.D15, addrReg, int16 offset) // Store float to heap
                ]
            | _ -> Error "Unsupported operand type in HeapStore")

    | LIR.HeapLoad (dest, addr, offset) ->
        // Load value from addr + offset (offset is in bytes)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg addr
            |> Result.map (fun addrReg ->
                [ARM64Symbolic.LDR (destReg, addrReg, int16 offset)]))

    | LIR.RefCountInc (addr, payloadSize, kind) ->
        // Generic RC increment for heap values.
        // RcKind controls list-helper dispatch explicitly (no payload-size heuristics).
        lirRegToARM64Reg addr
        |> Result.map (fun addrReg ->
            let tupleIncPath = [
                ARM64Symbolic.LDR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
            ]

            match kind with
            | LIR.TaggedList ->
                let listIncCall = [
                    ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -64s)
                    ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                    ARM64Symbolic.BL listRefCountIncHelperLabel
                    ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 64s)
                ]
                let listCallLen = List.length listIncCall
                [
                    ARM64Symbolic.CBZ_offset (addrReg, listCallLen + 1)
                ]
                @ listIncCall
            | LIR.GenericHeap ->
                [
                    ARM64Symbolic.CBZ_offset (addrReg, 4)
                ] @ tupleIncPath)

    | LIR.RefCountDec (addr, payloadSize, kind) ->
        // Decrement ref count at [addr + payloadSize]
        // Skip if addr is null (e.g., empty list = 0)
        // When ref count hits 0, add block to free list for memory reuse
        //
        // Free list structure:
        // - X27 = base of free list heads (32 slots × 8 bytes = 256 bytes)
        // - Slot N contains head of free list for blocks of size (N+1)*8 bytes
        // - sizeClassOffset = payloadSize (for 8-aligned payloads)
        // - Freed blocks use first 8 bytes as next pointer
        //
        // Code structure (8 instructions, plus optional leak counter update):
        //   CBZ addr, +8                      ; If null, skip all 7 instructions
        //   LDR X15, [addr, payloadSize]      ; Load ref count
        //   SUB X15, X15, 1                   ; Decrement
        //   STR X15, [addr, payloadSize]      ; Store back
        //   CBNZ X15, +4                      ; If not zero, skip free list code (4 instrs)
        //   LDR X14, [X27, payloadSize]       ; Load current free list head
        //   STR X14, [addr, 0]                ; Store old head as next in freed block
        //   STR addr, [X27, payloadSize]      ; Update free list head to freed block
        //   (continue)
        lirRegToARM64Reg addr
        |> Result.map (fun addrReg ->
            let leakDec = generateLeakCounterDec ctx
            let tupleDecPath =
                let cbnzOffset = if List.isEmpty leakDec then 4 else 9
                [
                    ARM64Symbolic.LDR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
                    ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X15, cbnzOffset)
                    ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X27, int16 payloadSize)
                    ARM64Symbolic.STR (ARM64Symbolic.X14, addrReg, 0s)
                    ARM64Symbolic.STR (addrReg, ARM64Symbolic.X27, int16 payloadSize)
                ] @ leakDec

            match kind with
            | LIR.TaggedList ->
                let listDecCall = [
                    ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -80s)
                    ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                    ARM64Symbolic.BL listRefCountDecHelperLabel
                    ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 80s)
                ]
                let listCallLen = List.length listDecCall
                [
                    ARM64Symbolic.CBZ_offset (addrReg, listCallLen + 1)
                ]
                @ listDecCall
            | LIR.GenericHeap ->
                let cbzOffset = if List.isEmpty leakDec then 8 else 13
                [
                    ARM64Symbolic.CBZ_offset (addrReg, cbzOffset)
                ] @ tupleDecPath)

    | LIR.StringConcat (dest, left, right) ->
        // String concatenation:
        // Heap string layout: [length:8][data:N][refcount:8]
        // Literal string layout: [length:8][data:N]
        //
        // Register usage:
        // X9  = left data address (for literal: string address, for heap: addr+8)
        // X10 = left length
        // X11 = right data address
        // X12 = right length
        // X13 = total length
        // X14 = result pointer
        // X15 = temp for byte copy
        //
        // Algorithm:
        // 1. Load left address and length into X9, X10
        // 2. Load right address and length into X11, X12
        // 3. Calculate total length: X13 = X10 + X12
        // 4. Allocate: total + 16 bytes using bump allocator
        // 5. Store total length at [X14]
        // 6. Copy left bytes to [X14+8]
        // 7. Copy right bytes to [X14+8+len1]
        // 8. Store refcount=1 at [X14+8+total]
        // 9. Move result to dest

        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Helper: load operand address and length into registers
            let loadOperandInfo (operand: LIR.Operand) (addrReg: ARM64Symbolic.Reg) (lenReg: ARM64Symbolic.Reg) : Result<ARM64Symbolic.Instr list, string> =
                match operand with
                | LIR.StringSymbol value ->
                    // Literal string: address via ADRP+ADD, length from UTF-8 bytes
                    // Literal format: [length:8][data:N] - skip length prefix to get data address
                    let len = utf8Len value
                    let labelRef = stringDataLabel value
                    Ok ([
                        ARM64Symbolic.ADRP (addrReg, labelRef)
                        ARM64Symbolic.ADD_label (addrReg, addrReg, labelRef)
                        ARM64Symbolic.ADD_imm (addrReg, addrReg, 8us)    // Skip 8-byte length prefix
                    ] @ loadImmediate lenReg (int64 len))
                | LIR.Reg reg ->
                    // Heap string: address in reg, length at [reg], data at [reg+8]
                    lirRegToARM64Reg reg
                    |> Result.map (fun srcReg ->
                        [
                            ARM64Symbolic.LDR (lenReg, srcReg, 0s)           // len = [srcReg]
                            ARM64Symbolic.ADD_imm (addrReg, srcReg, 8us)     // addr = srcReg + 8 (data start)
                        ])
                | other -> Error $"StringConcat requires StringSymbol or Reg operand, got: {other}"

            // Load both operands
            loadOperandInfo left ARM64Symbolic.X9 ARM64Symbolic.X10
            |> Result.bind (fun leftInstrs ->
                loadOperandInfo right ARM64Symbolic.X11 ARM64Symbolic.X12
                |> Result.map (fun rightInstrs ->
                    // Calculate total length
                    let calcTotal = [ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X10, ARM64Symbolic.X12)]

                    // Allocate: totalLen + 16 bytes (8 for length, 8 for refcount)
                    // Using bump allocator (X28 = bump pointer)
                    let allocate = [
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X13, 16us)   // X14 = total + 16
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 7us)    // Align up
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0xFFF8us, 0)          // ~7 mask (lower bits)
                        ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 16)         // Bits 16-31
                        ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 32)         // Bits 32-47
                        ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 48)         // Bits 48-63
                        ARM64Symbolic.AND_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X15)  // X14 = aligned size
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X28)            // X14 = current heap ptr (result)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X13, 16us)      // X15 = total + 16
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 7us)       // Align
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0xFFF8us, 0)              // ~7 mask again (X15 was clobbered)
                        ARM64Symbolic.MOVK (ARM64Symbolic.X0, 0xFFFFus, 16)             // Bits 16-31
                        ARM64Symbolic.MOVK (ARM64Symbolic.X0, 0xFFFFus, 32)             // Bits 32-47
                        ARM64Symbolic.MOVK (ARM64Symbolic.X0, 0xFFFFus, 48)             // Bits 48-63
                        ARM64Symbolic.AND_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X0)
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X15) // Bump heap pointer
                    ]

                    // Store total length at [X14]
                    let storeLen = [ARM64Symbolic.STR (ARM64Symbolic.X13, ARM64Symbolic.X14, 0s)]

                    // Copy left bytes: loop copying X10 bytes from X9 to [X14+8]
                    // IMPORTANT: Don't use X0-X7 as temps - they may hold function arguments!
                    // Strategy: Use pointer-bumping loops instead of indexed addressing
                    // X15 = source pointer (starts at X9, bumped each iteration)
                    // X16 = dest pointer (starts at X14+8, bumped each iteration)
                    // X13 = remaining count (starts at X10, decremented, reused since we stored total already)
                    let copyLeft = [
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X9)              // 0: X15 = src ptr
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X14, 8us)        // 1: X16 = dest ptr (X14 + 8)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X13, ARM64Symbolic.X10)             // 2: X13 = remaining = len1
                        // Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 9)
                        ARM64Symbolic.CBZ_offset (ARM64Symbolic.X13, 7)                  // 3: Skip 7 instructions if done -> index 10 (past end)
                        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X8, ARM64Symbolic.X15, 0)          // 4: X8 = byte at [X15]
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X8, ARM64Symbolic.X16)             // 5: [X16] = byte
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // 6: X15++ (src ptr)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)        // 7: X16++ (dest ptr)
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)        // 8: X13-- (remaining)
                        ARM64Symbolic.B (-6)                                     // 9: Loop back to CBZ (index 3)
                    ]

                    // Copy right bytes: loop copying X12 bytes from X11 to [X14+8+X10]
                    // X15 = source pointer (starts at X11)
                    // X16 = dest pointer (starts at X14+8+X10, already in X16 from copyLeft end)
                    // X13 = remaining count (use X12)
                    // Note: X16 is already at X14+8+len1 after copyLeft loop ends!
                    let copyRight = [
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X11)             // 0: X15 = src ptr (right string)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X13, ARM64Symbolic.X12)             // 1: X13 = remaining = len2
                        // Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 8)
                        ARM64Symbolic.CBZ_offset (ARM64Symbolic.X13, 7)                  // 2: Skip 7 instructions if done -> index 9 (past end)
                        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X8, ARM64Symbolic.X15, 0)          // 3: X8 = byte at [X15]
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X8, ARM64Symbolic.X16)             // 4: [X16] = byte
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // 5: X15++ (src ptr)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)        // 6: X16++ (dest ptr)
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)        // 7: X13-- (remaining)
                        ARM64Symbolic.B (-6)                                     // 8: Loop back to CBZ (index 2)
                    ]

                    // Recompute total length since we clobbered X13
                    let recomputeTotal = [
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X10, ARM64Symbolic.X12)  // X13 = len1 + len2
                    ]

                    // Store refcount=1 at [X14+8+aligned(total)]
                    // where aligned(x) = ((x + 7) >> 3) << 3
                    let storeRefcount = [
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X13, 7us)        // X15 = total + 7
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X16, 3us, 0)                   // X16 = 3 (shift amount)
                        ARM64Symbolic.LSR_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X16)  // X15 = (total + 7) >> 3
                        ARM64Symbolic.LSL_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X16)  // X15 = aligned(total)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X14, 8us)        // X16 = dest + 8
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ARM64Symbolic.X16, ARM64Symbolic.X15)  // X15 = dest + 8 + aligned(total)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X16, 1us, 0)                   // X16 = 1
                        ARM64Symbolic.STR (ARM64Symbolic.X16, ARM64Symbolic.X15, 0s)             // [X15] = 1
                    ]

                    // Move result to dest
                    let moveResult = [ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X14)]

                    leftInstrs @ rightInstrs @ calcTotal @ allocate @ storeLen @ copyLeft @ copyRight @ recomputeTotal @ storeRefcount @ moveResult @ generateLeakCounterInc ctx
                )))

    | LIR.PrintHeapString reg ->
        // Print heap string: layout is [len:8][data:N]
        // Note: The syscall clobbers X0, X1, X2, X8. If the input register is one
        // of these, we save it to X9 before and restore after so subsequent code
        // can still use it.
        // 1. Save input to X9
        // 2. Load length from [X9] into X2
        // 3. Compute data pointer (X9 + 8) into X1
        // 4. Set X0 = 1 (stdout)
        // 5. write syscall
        // 6. Restore input register if it was clobbered
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            let isClobbered = regARM64 = ARM64Symbolic.X0 || regARM64 = ARM64Symbolic.X1 || regARM64 = ARM64Symbolic.X2 || regARM64 = ARM64Symbolic.X8
            let restoreInstrs = if isClobbered then [ARM64Symbolic.MOV_reg (regARM64, ARM64Symbolic.X9)] else []
            [
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, regARM64)           // X9 = input (save in case regARM64 is X0/X1/X2)
                ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X9, 0s)           // X2 = length
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X9, 8us)      // X1 = data pointer (X9 + 8)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)                // X0 = stdout fd
            ] @ runtimeInstrs (Runtime.generateWriteSyscall ()) @ restoreInstrs)

    | LIR.LoadFuncAddr (dest, funcName) ->
        // Load the address of a function into the destination register using ADR
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            [ARM64Symbolic.ADR (destReg, codeLabel funcName)])

    | LIR.FileReadText (dest, path) ->
        // File reading: generates syscall sequence to read file contents
        // Returns Result<String, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    runtimeInstrs (Runtime.generateFileReadText destReg pathARM64))
            | LIR.StringSymbol value ->
                // Literal string - convert to heap format first (same as FileExists)
                // Literal format: [length:8][data:N] - skip length to copy data
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)
                Ok ([
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X28)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)
                    ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip 8-byte length prefix
                ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                    ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X15, 0s)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
                ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X11)
                    ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)
                    ARM64Symbolic.LDRB (ARM64Symbolic.X12, ARM64Symbolic.X9, ARM64Symbolic.X0)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X0)
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)
                    ARM64Symbolic.B (-7)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X10)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
                ] @ generateLeakCounterInc ctx @ runtimeInstrs (Runtime.generateFileReadText destReg ARM64Symbolic.X15))
            | LIR.StackSlot offset ->
                loadStackSlot ARM64Symbolic.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ runtimeInstrs (Runtime.generateFileReadText destReg ARM64Symbolic.X15))
            | _ -> Error "FileReadText requires string operand")

    | LIR.FileExists (dest, path) ->
        // File exists check: generates syscall sequence to check file accessibility
        // Uses access/faccessat syscall to check if path exists
        // Path can be either a Reg (heap string pointer) or StringSymbol (literal string)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    runtimeInstrs (Runtime.generateFileExists destReg pathARM64))
            | LIR.StringSymbol value ->
                // Literal string - convert to heap format first, then call FileExists
                // Literal format: [length:8][data:N] - skip length to copy data
                // Heap format: [length:8][data:N][refcount:8]
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                // Use X15 as temp to hold heap string pointer
                Ok ([
                    // Allocate heap space for converted string
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X28)  // X15 = heap pointer
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump allocator
                    // Load literal string data address into X9 (skip 8-byte length prefix)
                    ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip length prefix
                    // Store length at [X15]
                ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                    ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X15, 0s)  // [X15] = length
                    // Copy bytes from literal string to heap
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)  // X0 = loop counter
                ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                    // Copy loop
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X11)
                    ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)  // Exit if counter >= len
                    ARM64Symbolic.LDRB (ARM64Symbolic.X12, ARM64Symbolic.X9, ARM64Symbolic.X0)  // X12 = literal[X0]
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)  // X13 = X15 + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X0)  // X13 = X15 + 8 + X0
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)  // [X13] = byte
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)  // X0++
                    ARM64Symbolic.B (-7)  // Loop back
                    // Store refcount = 1 at [X15 + 8 + aligned(len)]
                    // aligned(x) = ((x + 7) >> 3) << 3
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X10, 7us)        // X13 = len + 7
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 3us, 0)                   // X12 = 3
                    ARM64Symbolic.LSR_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = (len + 7) >> 3
                    ARM64Symbolic.LSL_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = aligned(len)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X15, 8us)        // X12 = X15 + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X12, ARM64Symbolic.X13)  // X13 = X15 + 8 + aligned(len)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)  // [X13] = 1
                ] @ generateLeakCounterInc ctx @ runtimeInstrs (Runtime.generateFileExists destReg ARM64Symbolic.X15))
            | LIR.StackSlot offset ->
                // Load heap string from stack slot
                loadStackSlot ARM64Symbolic.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ runtimeInstrs (Runtime.generateFileExists destReg ARM64Symbolic.X15))
            | _ -> Error "FileExists requires string operand")

    | LIR.FileWriteText (dest, path, content) ->
        // File write: writes content string to file at path
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Helper to get operand into a register
            let getOperandReg operand tempReg =
                match operand with
                | LIR.Reg reg ->
                    lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
                | LIR.StringSymbol value ->
                    // Literal string - convert to heap format
                    // Literal format: [length:8][data:N] - skip length to copy data
                    let len = utf8Len value
                    let labelRef = stringDataLabel value
                    let totalSize = ((len + 16) + 7) &&& (~~~7)
                    Ok ([
                        ARM64Symbolic.MOV_reg (tempReg, ARM64Symbolic.X28)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)
                        ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                        ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip 8-byte length prefix
                    ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                        ARM64Symbolic.STR (ARM64Symbolic.X10, tempReg, 0s)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
                    ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                        ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X11)
                        ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)
                        ARM64Symbolic.LDRB (ARM64Symbolic.X12, ARM64Symbolic.X9, ARM64Symbolic.X0)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, tempReg, 8us)
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X0)
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)
                        ARM64Symbolic.B (-7)  // Jump back to CMP
                        // Store refcount = 1 at [tempReg + 8 + aligned(len)]
                        // aligned(x) = ((x + 7) >> 3) << 3
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X10, 7us)        // X13 = len + 7
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 3us, 0)                   // X12 = 3
                        ARM64Symbolic.LSR_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = (len + 7) >> 3
                        ARM64Symbolic.LSL_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = aligned(len)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, tempReg, 8us)          // X12 = tempReg + 8
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X12, ARM64Symbolic.X13)  // X13 = tempReg + 8 + aligned(len)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
                        ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
                    ] @ generateLeakCounterInc ctx, tempReg)
                | LIR.StackSlot offset ->
                    loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
                | _ -> Error "FileWriteText requires string operands"

            getOperandReg path ARM64Symbolic.X15
            |> Result.bind (fun (pathInstrs, pathReg) ->
                getOperandReg content ARM64Symbolic.X14
                |> Result.map (fun (contentInstrs, contentReg) ->
                    pathInstrs @ contentInstrs @ runtimeInstrs (Runtime.generateFileWriteText destReg pathReg contentReg false))))

    | LIR.FileAppendText (dest, path, content) ->
        // File append: appends content string to file at path
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Same helper as FileWriteText
            // Pool format: [length:8][data:N] - skip length to copy data
            let getOperandReg operand tempReg =
                match operand with
                | LIR.Reg reg ->
                    lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
                | LIR.StringSymbol value ->
                    let len = utf8Len value
                    let labelRef = stringDataLabel value
                    let totalSize = ((len + 16) + 7) &&& (~~~7)
                    Ok ([
                        ARM64Symbolic.MOV_reg (tempReg, ARM64Symbolic.X28)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)
                        ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                        ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip 8-byte length prefix
                    ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                        ARM64Symbolic.STR (ARM64Symbolic.X10, tempReg, 0s)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
                    ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                        ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X11)
                        ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)
                        ARM64Symbolic.LDRB (ARM64Symbolic.X12, ARM64Symbolic.X9, ARM64Symbolic.X0)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, tempReg, 8us)
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X0)
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)
                        ARM64Symbolic.B (-7)  // Jump back to CMP
                        // Store refcount = 1 at [tempReg + 8 + aligned(len)]
                        // aligned(x) = ((x + 7) >> 3) << 3
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X10, 7us)        // X13 = len + 7
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 3us, 0)                   // X12 = 3
                        ARM64Symbolic.LSR_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = (len + 7) >> 3
                        ARM64Symbolic.LSL_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = aligned(len)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, tempReg, 8us)          // X12 = tempReg + 8
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X12, ARM64Symbolic.X13)  // X13 = tempReg + 8 + aligned(len)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
                        ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
                    ] @ generateLeakCounterInc ctx, tempReg)
                | LIR.StackSlot offset ->
                    loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
                | _ -> Error "FileAppendText requires string operands"

            getOperandReg path ARM64Symbolic.X15
            |> Result.bind (fun (pathInstrs, pathReg) ->
                getOperandReg content ARM64Symbolic.X14
                |> Result.map (fun (contentInstrs, contentReg) ->
                    pathInstrs @ contentInstrs @ runtimeInstrs (Runtime.generateFileWriteText destReg pathReg contentReg true))))

    | LIR.FileDelete (dest, path) ->
        // File delete: deletes file at path
        // Uses unlink syscall to remove file
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    runtimeInstrs (Runtime.generateFileDelete destReg pathARM64))
            | LIR.StringSymbol value ->
                // Literal string - convert to heap format first, then call FileDelete
                // Literal format: [length:8][data:N] - skip length to copy data
                // Heap format: [length:8][data:N][refcount:8]
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                // Use X15 as temp to hold heap string pointer
                Ok ([
                    // Allocate heap space for converted string
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X28)  // X15 = heap pointer
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump allocator
                    // Load literal string data address into X9 (skip 8-byte length prefix)
                    ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip length prefix
                    // Store length at [X15]
                ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                    ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X15, 0s)  // [X15] = length
                    // Copy bytes from literal string to heap
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)  // X0 = loop counter
                ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                    // Copy loop
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X11)
                    ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)  // Exit if counter >= len
                    ARM64Symbolic.LDRB (ARM64Symbolic.X12, ARM64Symbolic.X9, ARM64Symbolic.X0)  // X12 = literal[X0]
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)  // X13 = X15 + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X0)  // X13 = X15 + 8 + X0
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)  // [X13] = byte
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)  // X0++
                    ARM64Symbolic.B (-7)  // Loop back
                    // Store refcount = 1 at [X15 + 8 + aligned(len)]
                    // aligned(x) = ((x + 7) >> 3) << 3
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X10, 7us)        // X13 = len + 7
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 3us, 0)                   // X12 = 3
                    ARM64Symbolic.LSR_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = (len + 7) >> 3
                    ARM64Symbolic.LSL_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)  // X13 = aligned(len)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X15, 8us)        // X12 = X15 + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X12, ARM64Symbolic.X13)  // X13 = X15 + 8 + aligned(len)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)  // [X13] = 1
                ] @ generateLeakCounterInc ctx @ runtimeInstrs (Runtime.generateFileDelete destReg ARM64Symbolic.X15))
            | LIR.StackSlot offset ->
                // Load heap string from stack slot
                loadStackSlot ARM64Symbolic.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ runtimeInstrs (Runtime.generateFileDelete destReg ARM64Symbolic.X15))
            | _ -> Error "FileDelete requires string operand")

    | LIR.FileSetExecutable (dest, path) ->
        // File set executable: sets executable bit on file at path
        // Uses chmod syscall with executable permission
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    runtimeInstrs (Runtime.generateFileSetExecutable destReg pathARM64))
            | LIR.StringSymbol value ->
                // Literal string - convert to heap format first
                // Literal format: [length:8][data:N] - skip length to copy data
                let len = utf8Len value
                let labelRef = stringDataLabel value
                let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                Ok ([
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X28)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)
                    ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip 8-byte length prefix
                ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                    ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X15, 0s)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
                ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X11)
                    ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)
                    ARM64Symbolic.LDRB (ARM64Symbolic.X12, ARM64Symbolic.X9, ARM64Symbolic.X0)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X0)
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)
                    ARM64Symbolic.B (-7)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X10)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
                ] @ generateLeakCounterInc ctx @ runtimeInstrs (Runtime.generateFileSetExecutable destReg ARM64Symbolic.X15))
            | LIR.StackSlot offset ->
                loadStackSlot ARM64Symbolic.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ runtimeInstrs (Runtime.generateFileSetExecutable destReg ARM64Symbolic.X15))
            | _ -> Error "FileSetExecutable requires string operand")

    | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
        // Write raw bytes from ptr to file at path
        // Returns 1 on success, 0 on failure
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg ptr
            |> Result.bind (fun ptrARM64 ->
                lirRegToARM64Reg length
                |> Result.bind (fun lengthARM64 ->
                    match path with
                    | LIR.Reg pathReg ->
                        // Already a heap string pointer
                        lirRegToARM64Reg pathReg
                        |> Result.map (fun pathARM64 ->
                            runtimeInstrs (Runtime.generateFileWriteFromPtr destReg pathARM64 ptrARM64 lengthARM64))
                    | LIR.StringSymbol value ->
                        // Literal string - convert to heap format first
                        // Literal format: [length:8][data:N] - skip length to copy data
                        let len = utf8Len value
                        let labelRef = stringDataLabel value
                        let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                        Ok ([
                            ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X28)
                            ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)
                            ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                            ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                            ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)  // Skip 8-byte length prefix
                        ] @ loadImmediate ARM64Symbolic.X10 (int64 len) @ [
                            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X15, 0s)
                            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
                        ] @ loadImmediate ARM64Symbolic.X11 (int64 len) @ [
                            ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X11)
                            ARM64Symbolic.B_cond (ARM64Symbolic.GE, 7)
                            ARM64Symbolic.LDRB (ARM64Symbolic.X12, ARM64Symbolic.X9, ARM64Symbolic.X0)
                            ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)
                            ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X0)
                            ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)
                            ARM64Symbolic.ADD_imm (ARM64Symbolic.X0, ARM64Symbolic.X0, 1us)
                            ARM64Symbolic.B (-7)
                            ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us)
                            ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X10)
                            ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
                            ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
                        ] @ generateLeakCounterInc ctx @ runtimeInstrs (Runtime.generateFileWriteFromPtr destReg ARM64Symbolic.X15 ptrARM64 lengthARM64))
                    | LIR.StackSlot offset ->
                        loadStackSlot ARM64Symbolic.X15 offset
                        |> Result.map (fun loadInstrs ->
                            loadInstrs @ runtimeInstrs (Runtime.generateFileWriteFromPtr destReg ARM64Symbolic.X15 ptrARM64 lengthARM64))
                    | _ -> Error "FileWriteFromPtr requires string path operand")))

    | LIR.RawAlloc (dest, numBytes) ->
        // Raw allocation: free-list reuse for small aligned size classes, else bump allocation.
        // This path is used by FingerTree nodes, so reusing freed raw blocks is required to avoid OOM.
        // numBytes is already in a physical register (from MIR_to_LIR)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg numBytes
            |> Result.map (fun numBytesReg ->
                let alignSizeInstrs = [
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, numBytesReg, 7us)        // X15 = numBytes + 7
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X14, 3us, 0)                     // X14 = 3 (shift amount)
                    ARM64Symbolic.LSR_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X14)  // X15 = (numBytes + 7) >> 3
                    ARM64Symbolic.LSL_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X14)  // X15 = aligned size
                ]

                if ctx.Options.DisableFreeList then
                    alignSizeInstrs
                    @ checkedBumpAllocReg ctx.HeapOverflowLabel destReg ARM64Symbolic.X15
                    @ generateLeakCounterInc ctx
                else
                    let popFreeList = [
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X14)             // dest = free-list head block
                        ARM64Symbolic.LDR (ARM64Symbolic.X13, ARM64Symbolic.X14, 0s)   // X13 = next block
                        ARM64Symbolic.STR (ARM64Symbolic.X13, ARM64Symbolic.X12, 0s)   // update free-list head
                    ]

                    let bumpAlloc = checkedBumpAllocReg ctx.HeapOverflowLabel destReg ARM64Symbolic.X15

                    alignSizeInstrs
                    @ [
                        ARM64Symbolic.CMP_imm (ARM64Symbolic.X15, 8us)                  // Need at least payload + refcount to have a payload class
                        ARM64Symbolic.B_cond (ARM64Symbolic.LE, popFreeList.Length + 8) // skip free-list lookup for undersized allocations
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X13, ARM64Symbolic.X15, 8us) // X13 = payload size class (total size minus refcount word)
                        ARM64Symbolic.CMP_imm (ARM64Symbolic.X13, 248us)                // free-list has slots for payload classes up to 248 bytes
                        ARM64Symbolic.B_cond (ARM64Symbolic.GT, popFreeList.Length + 5) // skip free-list lookup when class is out of range
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X27, ARM64Symbolic.X13) // X12 = &free_list[payload_class]
                        ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)    // X14 = free-list head
                        ARM64Symbolic.CBZ_offset (ARM64Symbolic.X14, popFreeList.Length + 2) // if empty, jump to bump path
                    ]
                    @ popFreeList
                    // B uses a current-PC-relative instruction offset, so skipping N instructions needs N + 1.
                    @ [ARM64Symbolic.B (bumpAlloc.Length + 1)]
                    @ bumpAlloc
                    @ generateLeakCounterInc ctx))

    | LIR.RawFree ptr ->
        // Raw free: no-op for now (bump allocator doesn't support free)
        // In future: could add to a raw memory free list
        Ok []

    | LIR.RawGet (dest, ptr, byteOffset) ->
        // Load 8 bytes from ptr + byteOffset
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg ptr
            |> Result.bind (fun ptrReg ->
                lirRegToARM64Reg byteOffset
                |> Result.map (fun offsetReg ->
                    [
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ptrReg, offsetReg)   // X15 = ptr + offset
                        ARM64Symbolic.LDR (destReg, ARM64Symbolic.X15, 0s)             // dest = [X15]
                    ])))

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        // Load 1 byte from ptr + byteOffset (zero-extended to 64 bits)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg ptr
            |> Result.bind (fun ptrReg ->
                lirRegToARM64Reg byteOffset
                |> Result.map (fun offsetReg ->
                    [
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ptrReg, offsetReg)   // X15 = ptr + offset
                        ARM64Symbolic.LDRB_imm (destReg, ARM64Symbolic.X15, 0)         // dest = [X15] (byte, zero-extended)
                    ])))

    | LIR.RawSet (ptr, byteOffset, value, valueType) ->
        // Store 8 bytes at ptr + byteOffset.
        // If the stored value is RC-managed, increment ownership because the parent now owns that edge.
        lirRegToARM64Reg ptr
        |> Result.bind (fun ptrReg ->
            lirRegToARM64Reg byteOffset
            |> Result.bind (fun offsetReg ->
                lirRegToARM64Reg value
                |> Result.map (fun valueReg ->
                    let tempReg =
                        if ptrReg = ARM64Symbolic.X15 || offsetReg = ARM64Symbolic.X15 || valueReg = ARM64Symbolic.X15 then
                            ARM64Symbolic.X14
                        else
                            ARM64Symbolic.X15
                    let storeValue = [
                        ARM64Symbolic.ADD_reg (tempReg, ptrReg, offsetReg)   // temp = ptr + offset
                        ARM64Symbolic.STR (valueReg, tempReg, 0s)            // [temp] = value
                    ]

                    let ownershipInc =
                        match valueType with
                        | Some (AST.TList _) ->
                            [
                                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -64s)
                                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, valueReg)
                                ARM64Symbolic.BL listRefCountIncHelperLabel
                                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 64s)
                            ]
                        | Some typ ->
                            let isRcManagedHeapType =
                                match typ with
                                | AST.TDict _ -> false
                                | _ -> ANF.isHeapType typ
                            if isRcManagedHeapType then
                                let rcOffsetOpt =
                                    match typ with
                                    | AST.TTuple elemTypes -> Some (int16 (List.length elemTypes * 8))
                                    | AST.TSum _ -> Some 16s
                                    | AST.TList _ -> Some 24s
                                    | AST.TDict _ -> Some 8s
                                    | _ -> None
                                match rcOffsetOpt with
                                | Some rcOffset ->
                                    let rcReg =
                                        if valueReg = ARM64Symbolic.X15 then ARM64Symbolic.X14 else ARM64Symbolic.X15
                                    [
                                        ARM64Symbolic.CBZ_offset (valueReg, 4)
                                        ARM64Symbolic.LDR (rcReg, valueReg, rcOffset)
                                        ARM64Symbolic.ADD_imm (rcReg, rcReg, 1us)
                                        ARM64Symbolic.STR (rcReg, valueReg, rcOffset)
                                    ]
                                | None ->
                                    []
                            else
                                []
                        | _ -> []

                    storeValue @ ownershipInc)))

    | LIR.RawSetByte (ptr, byteOffset, value) ->
        // Store 1 byte at ptr + byteOffset
        // IMPORTANT: If any input reg is X15, use X14 as temp instead
        lirRegToARM64Reg ptr
        |> Result.bind (fun ptrReg ->
            lirRegToARM64Reg byteOffset
            |> Result.bind (fun offsetReg ->
                lirRegToARM64Reg value
                |> Result.map (fun valueReg ->
                    let tempReg =
                        if ptrReg = ARM64Symbolic.X15 || offsetReg = ARM64Symbolic.X15 || valueReg = ARM64Symbolic.X15 then
                            ARM64Symbolic.X14
                        else
                            ARM64Symbolic.X15
                    [
                        ARM64Symbolic.ADD_reg (tempReg, ptrReg, offsetReg)   // temp = ptr + offset
                        ARM64Symbolic.STRB_reg (valueReg, tempReg)           // [temp] = value (byte)
                    ])))

    | LIR.RefCountIncString str ->
        // Increment refcount for a heap string
        // Heap string layout: [length:8][data:N][padding:P][refcount:8] where P aligns to 8
        // Literal strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
        match str with
        | LIR.StringSymbol _ ->
            // Literal string - no refcount, no-op
            Ok []
        | LIR.Reg reg ->
            // Heap or literal string - refcount is at [addr + 8 + aligned(length)]
            lirRegToARM64Reg reg
            |> Result.map (fun addrReg ->
                [
                    // Save address to X12 in case addrReg is X13/X14/X15 which we clobber
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, addrReg)               // X12 = string address
                    ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)             // X15 = length
                    // Align length: X15 = ((X15 + 7) >> 3) << 3
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 7us)        // X15 = length + 7
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 3us, 0)                   // X13 = 3 (shift amount)
                    ARM64Symbolic.LSR_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X13)  // X15 = (length + 7) >> 3
                    ARM64Symbolic.LSL_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X13)  // X15 = aligned(length)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X12, 8us)        // X14 = addr + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X15)  // X14 = addr + 8 + aligned(length) (refcount addr)
                    ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)             // X15 = refcount
                    // Load sentinel value 0x7FFFFFFFFFFFFFFF (INT64_MAX) into X13
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)             // Compare with sentinel
                    ARM64Symbolic.B_cond (ARM64Symbolic.EQ, 3)                       // If literal string, skip to end
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // X15++
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)             // store back
                ])
        | _ -> Error "RefCountIncString requires StringSymbol or Reg operand"

    | LIR.RefCountDecString str ->
        // Decrement refcount for a heap string
        // Heap string layout: [length:8][data:N][padding:P][refcount:8] where P aligns to 8
        // Literal strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
        match str with
        | LIR.StringSymbol _ ->
            // Literal string - no refcount, no-op
            Ok []
        | LIR.Reg reg ->
            // Heap or literal string - refcount is at [addr + 8 + aligned(length)]
            lirRegToARM64Reg reg
            |> Result.map (fun addrReg ->
                let leakDec = generateLeakCounterDec ctx
                let bcondOffset = if List.isEmpty leakDec then 3 else 9
                let refcountUpdate =
                    if List.isEmpty leakDec then
                        [
                            ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // X15--
                            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)             // store back
                        ]
                    else
                        [
                            ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // X15--
                            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)             // store back
                            // If refcount hits 0, update leak counter (string freeing not implemented yet)
                            ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X15, 6)                 // If not zero, skip leak counter
                        ] @ leakDec
                [
                    // Save address to X12 in case addrReg is X13/X14/X15 which we clobber
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, addrReg)               // X12 = string address
                    ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)             // X15 = length
                    // Align length: X15 = ((X15 + 7) >> 3) << 3
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 7us)        // X15 = length + 7
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 3us, 0)                   // X13 = 3 (shift amount)
                    ARM64Symbolic.LSR_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X13)  // X15 = (length + 7) >> 3
                    ARM64Symbolic.LSL_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X13)  // X15 = aligned(length)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X12, 8us)        // X14 = addr + 8
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X15)  // X14 = addr + 8 + aligned(length) (refcount addr)
                    ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)             // X15 = refcount
                    // Load sentinel value 0x7FFFFFFFFFFFFFFF (INT64_MAX) into X13
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
                    ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)             // Compare with sentinel
                    ARM64Symbolic.B_cond (ARM64Symbolic.EQ, bcondOffset)             // If literal string, skip to end
                ] @ refcountUpdate)
        | _ -> Error "RefCountDecString requires StringSymbol or Reg operand"

    | LIR.RandomInt64 dest ->
        // Generate random 8 bytes as Int64
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            runtimeInstrs (Runtime.generateRandomInt64 destReg))

    | LIR.DateNow dest ->
        // Generate current Unix epoch seconds as Int64
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            runtimeInstrs (Runtime.generateDateNow destReg))

    | LIR.FloatToString (dest, value) ->
        // Convert float in FP register to heap string
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg value
            |> Result.map (fun valueReg ->
                runtimeInstrs (Runtime.generateFloatToString destReg valueReg) @ generateLeakCounterInc ctx))

    | LIR.CoverageHit exprId ->
        // Increment coverage counter at _coverage_data[exprId * 8]
        // Uses PC-relative addressing (ADRP+ADD) to get BSS buffer address
        // Uses X9 and X10 as scratch registers
        let offset = exprId * 8
        Ok ([
            // Get address of coverage buffer using PC-relative addressing
            ARM64Symbolic.ADRP (ARM64Symbolic.X9, dataLabel "_coverage_data")
            ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, dataLabel "_coverage_data")
        ] @
        // Add offset for this expression's counter
        (if offset = 0 then
            []
        elif offset < 4096 then
            [ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, uint16 offset)]
        else
            loadImmediate ARM64Symbolic.X10 (int64 offset) @ [ARM64Symbolic.ADD_reg (ARM64Symbolic.X9, ARM64Symbolic.X9, ARM64Symbolic.X10)]) @
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)        // X10 = coverage_buffer[exprId]
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)  // X10++
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)        // coverage_buffer[exprId] = X10
        ])

/// Convert LIR terminator to ARM64 instructions
/// epilogueLabel: the label to jump to for function return (handles stack cleanup)
let convertTerminator (epilogueLabel: string) (terminator: LIR.Terminator) : Result<ARM64Symbolic.Instr list, string> =
    match terminator with
    | LIR.Ret ->
        // Jump to function epilogue (handles stack cleanup and RET)
        Ok [ARM64Symbolic.B_label epilogueLabel]

    | LIR.Branch (condReg, trueLabel, falseLabel) ->
        // Branch if register is non-zero (true), otherwise fall through to else
        // Use CBNZ (compare and branch if not zero) to true label
        // Then unconditional branch to false label
        lirRegToARM64Reg condReg
        |> Result.map (fun arm64Reg ->
            let (LIR.Label trueLbl) = trueLabel
            let (LIR.Label falseLbl) = falseLabel
            [
                ARM64Symbolic.CBNZ (arm64Reg, trueLbl)  // If true, jump to then branch
                ARM64Symbolic.B_label falseLbl           // Otherwise jump to else branch
            ])

    | LIR.BranchZero (condReg, zeroLabel, nonZeroLabel) ->
        // Branch if register is zero, otherwise fall through to non-zero case
        // Use CBZ (compare and branch if zero) to zero label
        // Then unconditional branch to non-zero label
        lirRegToARM64Reg condReg
        |> Result.map (fun arm64Reg ->
            let (LIR.Label zeroLbl) = zeroLabel
            let (LIR.Label nonZeroLbl) = nonZeroLabel
            [
                ARM64Symbolic.CBZ (arm64Reg, zeroLbl)    // If zero, jump to zero branch
                ARM64Symbolic.B_label nonZeroLbl          // Otherwise jump to non-zero branch
            ])

    | LIR.BranchBitZero (condReg, bit, zeroLabel, nonZeroLabel) ->
        // Branch if specified bit is zero, otherwise fall through to non-zero case
        // Use TBZ (test bit and branch if zero) to zero label
        // Then unconditional branch to non-zero label
        lirRegToARM64Reg condReg
        |> Result.map (fun arm64Reg ->
            let (LIR.Label zeroLbl) = zeroLabel
            let (LIR.Label nonZeroLbl) = nonZeroLabel
            [
                ARM64Symbolic.TBZ_label (arm64Reg, bit, zeroLbl)  // If bit is zero, jump to zero branch
                ARM64Symbolic.B_label nonZeroLbl                   // Otherwise jump to non-zero branch
            ])

    | LIR.BranchBitNonZero (condReg, bit, nonZeroLabel, zeroLabel) ->
        // Branch if specified bit is non-zero, otherwise fall through to zero case
        // Use TBNZ (test bit and branch if not zero) to non-zero label
        // Then unconditional branch to zero label
        lirRegToARM64Reg condReg
        |> Result.map (fun arm64Reg ->
            let (LIR.Label nonZeroLbl) = nonZeroLabel
            let (LIR.Label zeroLbl) = zeroLabel
            [
                ARM64Symbolic.TBNZ_label (arm64Reg, bit, nonZeroLbl)  // If bit is not zero, jump to non-zero branch
                ARM64Symbolic.B_label zeroLbl                          // Otherwise jump to zero branch
            ])

    | LIR.Jump label ->
        let (LIR.Label lbl) = label
        Ok [ARM64Symbolic.B_label lbl]

    | LIR.CondBranch (cond, trueLabel, falseLabel) ->
        // Branch based on condition flags (set by previous CMP)
        // Use B.cond to true label, then unconditional branch to false label
        let (LIR.Label trueLbl) = trueLabel
        let (LIR.Label falseLbl) = falseLabel
        let arm64Cond =
            match cond with
            | LIR.EQ -> ARM64Symbolic.EQ
            | LIR.NE -> ARM64Symbolic.NE
            | LIR.LT -> ARM64Symbolic.LT
            | LIR.GT -> ARM64Symbolic.GT
            | LIR.LE -> ARM64Symbolic.LE
            | LIR.GE -> ARM64Symbolic.GE
            // TODO(#22): unsigned mapped to signed on the untested ARM64 backend (needs LO/HI/LS/HS).
            | LIR.ULT -> ARM64Symbolic.LT
            | LIR.UGT -> ARM64Symbolic.GT
            | LIR.ULE -> ARM64Symbolic.LE
            | LIR.UGE -> ARM64Symbolic.GE
        Ok [
            ARM64Symbolic.B_cond_label (arm64Cond, trueLbl)  // If condition, jump to true branch
            ARM64Symbolic.B_label falseLbl                   // Otherwise jump to false branch
        ]

/// Convert LIR basic block to ARM64 instructions (with label)
/// epilogueLabel: passed through to terminator for Ret handling
let convertBlock (ctx: CodeGenContext) (epilogueLabel: string) (block: LIR.BasicBlock) : Result<ARM64Symbolic.Instr list, string> =
    // Emit label for this block
    let (LIR.Label lbl) = block.Label
    let labelInstr = ARM64Symbolic.Label lbl

    ResultList.mapResults (convertInstr ctx) block.Instrs
    |> Result.bind (fun instrLists ->
        convertTerminator epilogueLabel block.Terminator
        |> Result.map (fun termInstrs ->
            let instrs = List.concat instrLists
            labelInstr :: (instrs @ termInstrs)))

/// Convert LIR CFG to ARM64 instructions
/// epilogueLabel: passed through to blocks for Ret handling
let convertCFG (ctx: CodeGenContext) (epilogueLabel: string) (cfg: LIR.CFG) : Result<ARM64Symbolic.Instr list, string> =
    // Get blocks in a deterministic order (entry first, then sorted by label)
    let entryBlock =
        match Map.tryFind cfg.Entry cfg.Blocks with
        | Some block -> [block]
        | None -> []

    let otherBlocks =
        cfg.Blocks
        |> Map.toList
        |> List.filter (fun (label, _) -> label <> cfg.Entry)
        |> List.sortBy fst
        |> List.map snd

    let allBlocks = entryBlock @ otherBlocks

    ResultList.mapResults (convertBlock ctx epilogueLabel) allBlocks
    |> Result.map List.concat

/// Generate heap initialization code for _start function
/// Uses mmap to allocate 512MB of heap space and initializes X27/X28
///
/// Memory layout:
///   X27 -> [free list heads: 256 bytes (32 entries × 8 bytes)]
///   X28 -> [heap allocation area: mapped heap after free list heads]
///
/// Free list heads are indexed by (totalSize / 8), where totalSize includes
/// the 8-byte ref count. Size class 0 and 1 are unused (too small).
/// Size class 2 = 16 bytes, class 3 = 24 bytes, etc.
///
/// X27 is the base for free list heads (constant after init)
/// X28 is the bump pointer for new allocations
let generateHeapInit () : ARM64Symbolic.Instr list =
    let freeListSize = 256
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error msg -> Crash.crash $"Platform detection failed: {msg}"
    let syscalls = ARM64.syscallConfigFor os
    let mmapFlags =
        match os with
        | Platform.MacOS -> 0x1002us  // MAP_PRIVATE | MAP_ANON
        | Platform.Linux -> 0x22us    // MAP_PRIVATE | MAP_ANONYMOUS
    let heapSizeForMmap = loadImmediate ARM64Symbolic.X1 heapMmapSizeBytes
    [
        // mmap(NULL, 512MB, PROT_READ|PROT_WRITE, flags, -1, 0)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)              // addr = NULL
    ]
    @ heapSizeForMmap
    @ [
        ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 3us, 0)              // PROT_READ | PROT_WRITE
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, mmapFlags, 0)        // flags
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 0us, 0)              // X4 = 0
        ARM64Symbolic.MVN (ARM64Symbolic.X4, ARM64Symbolic.X4)             // X4 = ~0 = -1 (fd)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 0us, 0)              // offset = 0
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Mmap, 0)
        ARM64Symbolic.SVC syscalls.SvcImmediate
        // Check for mmap failure (returns -1 on error)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0us, 0)             // X15 = 0
        ARM64Symbolic.MVN (ARM64Symbolic.X15, ARM64Symbolic.X15)           // X15 = -1
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X15)        // Compare X0 with -1
        ARM64Symbolic.B_cond (ARM64Symbolic.NE, 3)                 // Skip exit if not error (+3 instructions)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)              // exit code = 1
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
        ARM64Symbolic.SVC syscalls.SvcImmediate
        // X0 now contains mmap result (valid address)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X27, ARM64Symbolic.X0)        // X27 = free list heads base
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X27, uint16 freeListSize)  // X28 = heap start
        // No need to zero free list - MAP_ANONYMOUS provides zeroed pages
    ]

/// Convert LIR function to ARM64 instructions with prologue and epilogue
let convertFunction (ctx: CodeGenContext) (func: LIR.Function) : Result<ARM64Symbolic.Instr list, string> =
    // Generate epilogue label for this function (passed to convertCFG for Ret terminators)
    let epilogueLabel = "_epilogue_" + func.Name
    let overflowLabel = heapOverflowLabelPrefix + func.Name
    let needsHeapOverflowTrap =
        func.CFG.Blocks
        |> Map.toList
        |> List.exists (fun (_, block) ->
            block.Instrs
            |> List.exists (function
                | LIR.HeapAlloc _ -> true
                | LIR.RawAlloc _ -> true
                | _ -> false))

    // Create function-specific context with stack info for tail call epilogue generation
    let funcCtx = {
        ctx with
            StackSize = func.StackSize
            UsedCalleeSaved = func.UsedCalleeSaved
            HeapOverflowLabel = overflowLabel
    }

    // Convert CFG to ARM64 instructions
    match convertCFG funcCtx epilogueLabel func.CFG with
    | Error err -> Error err
    | Ok cfgInstrs ->
        // Generate prologue (save FP/LR, allocate stack)
        let prologue = generatePrologue func.UsedCalleeSaved func.StackSize

        // Generate heap initialization for _start only
        let heapInit =
            if func.Name = "_start" then generateHeapInit ()
            else []

        // Note: Coverage buffer is in BSS section (zero-initialized by OS)
        // No runtime initialization needed - CoverageHit uses ADRP+ADD to access it

        // Generate parameter setup: move X0-X7/D0-D7 to allocated parameter registers
        // This must come AFTER the prologue but BEFORE the function body
        // Strategy: Save all source registers to temp regs first to avoid clobbering
        let argRegs = [ARM64Symbolic.X0; ARM64Symbolic.X1; ARM64Symbolic.X2; ARM64Symbolic.X3; ARM64Symbolic.X4; ARM64Symbolic.X5; ARM64Symbolic.X6; ARM64Symbolic.X7]
        let tempRegs = [ARM64Symbolic.X9; ARM64Symbolic.X10; ARM64Symbolic.X11; ARM64Symbolic.X12; ARM64Symbolic.X13; ARM64Symbolic.X14; ARM64Symbolic.X15]

        // AAPCS64: int and float use SEPARATE register counters
        let paramsWithTypes = func.TypedParams |> List.map (fun tp -> (tp.Reg, tp.Type))

        // Collect integer parameters with their calling convention index
        let intParamsWithIdx =
            paramsWithTypes
            |> List.indexed
            |> List.fold (fun (intIdx, acc) (_, (param, typ)) ->
                if typ = AST.TFloat64 then
                    (intIdx, acc)  // Skip float params
                else
                    (intIdx + 1, (param, intIdx) :: acc)
            ) (0, [])
            |> snd
            |> List.rev

        // Step 1a: Save integer calling convention registers to temps
        let saveIntToTemps =
            intParamsWithIdx
            |> List.map (fun (_, intIdx) ->
                let argReg = List.item intIdx argRegs
                let tempReg = List.item intIdx tempRegs
                ARM64Symbolic.MOV_reg (tempReg, argReg))

        // Note: Float parameter setup is NOT done here - it's handled by RegisterAllocation
        // which inserts FMov instructions at the start of the CFG entry block.
        // Doing it here would corrupt D0/D1 before those CFG instructions run.

        // Step 2a: Move integers from temps to allocated parameter registers
        let moveIntFromTemps =
            intParamsWithIdx
            |> List.map (fun (paramReg, intIdx) ->
                let tempReg = List.item intIdx tempRegs
                match lirRegToARM64Reg paramReg with
                | Ok paramArm64 ->
                    if paramArm64 = tempReg then
                        []  // Already in the right place
                    else
                        [ARM64Symbolic.MOV_reg (paramArm64, tempReg)]
                | Error msg -> Crash.crash $"ParamSetup: lirRegToARM64Reg failed: {msg}")
            |> List.concat

        let paramSetup = saveIntToTemps @ moveIntFromTemps

        // Shared cold path for allocation overflow in this function.
        let heapOverflowTrap =
            if needsHeapOverflowTrap then
                generateHeapOverflowTrapBlock overflowLabel
            else
                []

        // Generate epilogue (deallocate stack, restore FP/LR, return or exit)
        let epilogueLabelInstr = [ARM64Symbolic.Label ("_epilogue_" + func.Name)]
        let epilogue =
            if func.Name = "_start" then
                // For _start, flush coverage (if enabled) then exit instead of return
                let coverageFlush =
                    if ctx.Options.EnableCoverage then
                        runtimeInstrs (Runtime.generateCoverageFlush ctx.Options.CoverageExprCount)
                    else []
                let leakCheckReport = generateLeakCheckReport ctx
                generateEpilogue func.UsedCalleeSaved func.StackSize
                |> List.filter (function ARM64Symbolic.RET -> false | _ -> true)  // Remove RET
                |> fun instrs -> instrs @ coverageFlush @ leakCheckReport @ runtimeInstrs (Runtime.generateExit ())
            else
                generateEpilogue func.UsedCalleeSaved func.StackSize

        // Add function entry label (for BL to branch to)
        let functionEntryLabel = [ARM64Symbolic.Label func.Name]

        // Combine: function label + prologue + heap init + param setup + CFG body + epilogue label + epilogue
        // All Ret terminators jump to the epilogue label
        Ok (functionEntryLabel @ prologue @ heapInit @ paramSetup @ cfgInstrs @ heapOverflowTrap @ epilogueLabelInstr @ epilogue)

/// Peephole optimization pass
/// Patterns:
/// 1. SUB_imm + CMP #0 → SUBS (fuse subtract and compare)
/// 2. MOV Xn, Xn → remove (redundant self-move)
/// 3. FMOV Dn, Dn → remove (redundant FP self-move)
/// 4. ADD Xn, Xn, #0 → remove (add zero)
/// 5. SUB Xn, Xn, #0 → remove (subtract zero)
/// 6. B_label X + Label X → remove branch (branch to next instruction)
/// 7. CMP #0 + B.EQ → CBZ (compare zero and branch equal)
/// 8. CMP #0 + B.NE → CBNZ (compare zero and branch not equal)
/// 9. AND Xn, Xn, Xn → MOV (AND with self is identity)
/// 10. ORR Xn, Xn, Xn → MOV (OR with self is identity)
let peepholeOptimize (instrs: ARM64Symbolic.Instr list) : ARM64Symbolic.Instr list =
    let rec optimize acc remaining =
        match remaining with
        | [] -> List.rev acc
        // Fuse SUB + CMP #0 into SUBS
        | ARM64Symbolic.SUB_imm (dest, src, imm) :: ARM64Symbolic.CMP_imm (cmpReg, 0us) :: rest when dest = cmpReg ->
            optimize (ARM64Symbolic.SUBS_imm (dest, src, imm) :: acc) rest
        // Fuse CMP #0 + B.EQ into CBZ
        | ARM64Symbolic.CMP_imm (reg, 0us) :: ARM64Symbolic.B_cond_label (ARM64.EQ, label) :: rest ->
            optimize (ARM64Symbolic.CBZ (reg, label) :: acc) rest
        // Fuse CMP #0 + B.NE into CBNZ
        | ARM64Symbolic.CMP_imm (reg, 0us) :: ARM64Symbolic.B_cond_label (ARM64.NE, label) :: rest ->
            optimize (ARM64Symbolic.CBNZ (reg, label) :: acc) rest
        // Remove redundant self-move (integer)
        | ARM64Symbolic.MOV_reg (dest, src) :: rest when dest = src ->
            optimize acc rest
        // Remove redundant self-move (FP)
        | ARM64Symbolic.FMOV_reg (dest, src) :: rest when dest = src ->
            optimize acc rest
        // Remove add zero
        | ARM64Symbolic.ADD_imm (dest, src, 0us) :: rest when dest = src ->
            optimize acc rest
        // Remove subtract zero
        | ARM64Symbolic.SUB_imm (dest, src, 0us) :: rest when dest = src ->
            optimize acc rest
        // AND with self is identity - simplify to MOV if dest differs from operand
        | ARM64Symbolic.AND_reg (dest, src1, src2) :: rest when src1 = src2 ->
            if dest = src1 then
                optimize acc rest  // dest = src AND src = src, remove entirely
            else
                optimize (ARM64Symbolic.MOV_reg (dest, src1) :: acc) rest
        // OR with self is identity - simplify to MOV if dest differs from operand
        | ARM64Symbolic.ORR_reg (dest, src1, src2) :: rest when src1 = src2 ->
            if dest = src1 then
                optimize acc rest  // dest = src OR src = src, remove entirely
            else
                optimize (ARM64Symbolic.MOV_reg (dest, src1) :: acc) rest
        // Remove branch to next instruction
        | ARM64Symbolic.B_label target :: ARM64Symbolic.Label lbl :: rest when target = lbl ->
            optimize (ARM64Symbolic.Label lbl :: acc) rest
        // Fuse LSL_imm + ADD_reg into ADD_shifted: dest = src1 + (src2 << shift)
        // Pattern: LSL_imm temp, x, shift; ADD_reg dest, x, temp → ADD_shifted dest, x, x, shift
        | ARM64Symbolic.LSL_imm (lslDest, lslSrc, shift) :: ARM64Symbolic.ADD_reg (addDest, addSrc1, addSrc2) :: rest
            when lslDest = addSrc2 && lslSrc = addSrc1 ->
            optimize (ARM64Symbolic.ADD_shifted (addDest, addSrc1, lslSrc, shift) :: acc) rest
        // Fuse LSL_imm + ADD_reg (commutative): ADD_reg dest, temp, x → ADD_shifted dest, x, x, shift
        | ARM64Symbolic.LSL_imm (lslDest, lslSrc, shift) :: ARM64Symbolic.ADD_reg (addDest, addSrc1, addSrc2) :: rest
            when lslDest = addSrc1 && lslSrc = addSrc2 ->
            optimize (ARM64Symbolic.ADD_shifted (addDest, addSrc2, lslSrc, shift) :: acc) rest
        // Fuse LSL_imm + SUB_reg into SUB_shifted: dest = shifted - src
        // Pattern: LSL_imm temp, x, shift; SUB_reg dest, temp, x → SUB_shifted dest, temp, x, 0 then adjust
        // Actually for n = 2^k - 1: x * n = (x << k) - x, so SUB dest, shifted, x
        // We need: SUB_shifted dest, (x << shift), x, 0 but that's not quite right...
        // For x * 7 = (x << 3) - x: LSL temp, x, 3; SUB dest, temp, x
        // This becomes: dest = temp - x = (x << 3) - x
        // ARM64 SUB_shifted is: dest = src1 - (src2 << shift)
        // So we need: dest = (x << 3) - x which is dest = (x << 3) - (x << 0)
        // That's not directly expressible with SUB_shifted... but we can use:
        // SUB dest, temp, x where temp = x << 3, which is two instructions
        // Actually let's skip SUB fusion for now since it doesn't map cleanly to SUB_shifted
        | instr :: rest ->
            optimize (instr :: acc) rest
    optimize [] instrs

/// Convert LIR program to ARM64 instructions with options
let generateARM64WithOptions (options: CodeGenOptions) (program: LIR.Program) : Result<ARM64Symbolic.Instr list, string> =
    let (LIR.Program functions) = program

    // Create code generation context with options
    // StackSize and UsedCalleeSaved are set per-function in convertFunction
    let ctx = {
        Options = options
        StackSize = 0
        UsedCalleeSaved = []
        HeapOverflowLabel = ""
    }

    // Ensure _start is first (entry point)
    let sortedFunctions =
        match List.tryFind (fun (f: LIR.Function) -> f.Name = "_start") functions with
        | Some startFunc ->
            let otherFuncs = List.filter (fun (f: LIR.Function) -> f.Name <> "_start") functions
            startFunc :: otherFuncs
        | None -> functions  // No _start, keep original order

    let needsListRcDecHelper =
        sortedFunctions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.RefCountDec (_, _, LIR.TaggedList) -> true
                    | _ -> false)))

    let needsListRcIncHelper =
        sortedFunctions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.RefCountInc (_, _, LIR.TaggedList) -> true
                    | LIR.RawSet (_, _, _, Some (AST.TList _)) -> true
                    | _ -> false)))

    ResultList.mapResults (convertFunction ctx) sortedFunctions
    |> Result.map (fun instrLists ->
        let allFunctionInstrs = instrLists |> List.concat
        let listRcHelpers =
            (if needsListRcIncHelper then generateListRefCountIncHelper () else [])
            @ (if needsListRcDecHelper then generateListRefCountDecHelper ctx else [])
        (allFunctionInstrs @ listRcHelpers) |> peepholeOptimize)

/// Convert LIR program to ARM64 instructions (uses default options)
let generateARM64 (program: LIR.Program) : Result<ARM64Symbolic.Instr list, string> =
    generateARM64WithOptions defaultOptions program
