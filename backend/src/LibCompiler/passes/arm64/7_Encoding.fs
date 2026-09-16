// 7_ARM64_Encoding.fs - ARM64 Machine Code Encoding (Pass 7)
//
// Encodes ARM64 instructions to 32-bit machine code per ARMv8 specification.
//
// Encoding algorithm:
// - Two-pass encoding for label-based branches:
//   Pass 1: Compute label positions (byte offsets)
//   Pass 2: Encode with computed branch offsets
// - Encodes registers as 5-bit fields
// - Packs immediates into instruction-specific bit positions
// - Combines opcode bits, operand fields into 32-bit words
// - Each instruction has unique bit layout defined by ARMv8
//
// Example:
//   MOVZ X0, #42, LSL #0  →  0xD2800540
//   ADD X1, X0, #5        →  0x91001401
//   RET                   →  0xD65F03C0

module ARM64_Encoding

/// Encode general-purpose register to 5-bit value
let encodeReg (reg: ARM64.Reg) : uint32 =
    match reg with
    | ARM64.X0 -> 0u | ARM64.X1 -> 1u | ARM64.X2 -> 2u | ARM64.X3 -> 3u
    | ARM64.X4 -> 4u | ARM64.X5 -> 5u | ARM64.X6 -> 6u | ARM64.X7 -> 7u
    | ARM64.X8 -> 8u | ARM64.X9 -> 9u | ARM64.X10 -> 10u | ARM64.X11 -> 11u
    | ARM64.X12 -> 12u | ARM64.X13 -> 13u | ARM64.X14 -> 14u | ARM64.X15 -> 15u
    | ARM64.X16 -> 16u | ARM64.X17 -> 17u
    | ARM64.X19 -> 19u | ARM64.X20 -> 20u | ARM64.X21 -> 21u | ARM64.X22 -> 22u
    | ARM64.X23 -> 23u | ARM64.X24 -> 24u | ARM64.X25 -> 25u | ARM64.X26 -> 26u
    | ARM64.X27 -> 27u | ARM64.X28 -> 28u
    | ARM64.X29 -> 29u | ARM64.X30 -> 30u | ARM64.SP -> 31u

/// Encode floating-point register to 5-bit value
let encodeFReg (reg: ARM64.FReg) : uint32 =
    match reg with
    | ARM64.D0 -> 0u | ARM64.D1 -> 1u | ARM64.D2 -> 2u | ARM64.D3 -> 3u
    | ARM64.D4 -> 4u | ARM64.D5 -> 5u | ARM64.D6 -> 6u | ARM64.D7 -> 7u
    | ARM64.D8 -> 8u | ARM64.D9 -> 9u | ARM64.D10 -> 10u | ARM64.D11 -> 11u
    | ARM64.D12 -> 12u | ARM64.D13 -> 13u | ARM64.D14 -> 14u | ARM64.D15 -> 15u
    | ARM64.D16 -> 16u | ARM64.D17 -> 17u | ARM64.D18 -> 18u
    | ARM64.D19 -> 19u | ARM64.D20 -> 20u | ARM64.D21 -> 21u | ARM64.D22 -> 22u
    | ARM64.D23 -> 23u | ARM64.D24 -> 24u | ARM64.D25 -> 25u | ARM64.D26 -> 26u
    | ARM64.D27 -> 27u | ARM64.D28 -> 28u | ARM64.D29 -> 29u | ARM64.D30 -> 30u | ARM64.D31 -> 31u

/// Encode ARM64 instruction to 32-bit machine code
let encode (instr: ARM64.Instr) : ARM64.MachineCode list =
    match instr with
    | ARM64.MOVZ (dest, imm, shift) ->
        // MOVZ encoding: sf=1 opc=10 100101 hw imm16 Rd
        // Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
        let sf = 1u <<< 31
        let opc = 2u <<< 29
        let opcode = 0b100101u <<< 23
        let hw = (uint32 shift / 16u) <<< 21
        let imm16 = (uint32 imm) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| opcode ||| hw ||| imm16 ||| rd]

    | ARM64.MOVN (dest, imm, shift) ->
        // MOVN encoding: sf=1 opc=00 100101 hw imm16 Rd
        // Sets Rd = NOT(imm16 << shift), useful for negative constants
        // Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
        let sf = 1u <<< 31
        let opc = 0u <<< 29  // MOVN has opc=00
        let opcode = 0b100101u <<< 23
        let hw = (uint32 shift / 16u) <<< 21
        let imm16 = (uint32 imm) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| opcode ||| hw ||| imm16 ||| rd]

    | ARM64.MOVK (dest, imm, shift) ->
        // MOVK encoding: sf=1 opc=11 100101 hw imm16 Rd
        // Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
        let sf = 1u <<< 31
        let opc = 3u <<< 29
        let opcode = 0b100101u <<< 23
        let hw = (uint32 shift / 16u) <<< 21
        let imm16 = (uint32 imm) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| opcode ||| hw ||| imm16 ||| rd]

    | ARM64.ADD_imm (dest, src, imm) ->
        // ADD immediate: sf=1 0 0 10001 shift(2) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b10001u <<< 24
        let shift = 0u <<< 22  // No shift
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.ADD_reg (dest, src1, src2) ->
        // ADD register: sf=1 0 0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b01011u <<< 24
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| rn ||| rd]

    | ARM64.ADD_shifted (dest, src1, src2, shiftAmt) ->
        // ADD shifted register: sf=1 0 0 01011 shift=00(LSL) 0 Rm(5) imm6(6) Rn(5) Rd(5)
        // dest = src1 + (src2 << shiftAmt)
        let sf = 1u <<< 31
        let op = 0b01011u <<< 24
        let rm = (encodeReg src2) <<< 16
        let imm6 = (uint32 shiftAmt) <<< 10  // shift amount in bits 15-10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| imm6 ||| rn ||| rd]

    | ARM64.SUB_imm (dest, src, imm) ->
        // SUB immediate: sf=1 op=1 S=0 10001 shift(2) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31          // 64-bit operation
        let op = 1u <<< 30          // SUB (vs ADD which has op=0)
        let s = 0u <<< 29           // Don't set flags
        let opcode = 0b10001u <<< 24 // Fixed opcode bits
        let shift = 0u <<< 22       // No shift
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.SUB_imm12 (dest, src, imm) ->
        // SUB immediate with shift=12: actual value = imm * 4096
        // sf=1 op=1 S=0 10001 shift=01(12-bit shift) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31          // 64-bit operation
        let op = 1u <<< 30          // SUB (vs ADD which has op=0)
        let s = 0u <<< 29           // Don't set flags
        let opcode = 0b10001u <<< 24 // Fixed opcode bits
        let shift = 1u <<< 22       // shift=1 means LSL #12
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.SUB_reg (dest, src1, src2) ->
        // SUB register: sf=1 op=1 S=0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        // Bits: sf(31) op(30) S(29) 01011(28-24) shift(23-22) 0(21) Rm(20-16) imm6(15-10) Rn(9-5) Rd(4-0)
        let sf = 1u <<< 31  // 64-bit
        let op = 1u <<< 30  // Subtract (not add)
        let s = 0u <<< 29   // Don't set flags (use SUB not SUBS)
        let opcode = 0b01011u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| rm ||| rn ||| rd]

    | ARM64.SUB_shifted (dest, src1, src2, shiftAmt) ->
        // SUB shifted register: sf=1 op=1 S=0 01011 shift=00(LSL) 0 Rm(5) imm6(6) Rn(5) Rd(5)
        // dest = src1 - (src2 << shiftAmt)
        let sf = 1u <<< 31  // 64-bit
        let op = 1u <<< 30  // Subtract (not add)
        let s = 0u <<< 29   // Don't set flags
        let opcode = 0b01011u <<< 24
        let rm = (encodeReg src2) <<< 16
        let imm6 = (uint32 shiftAmt) <<< 10  // shift amount in bits 15-10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| rm ||| imm6 ||| rn ||| rd]

    | ARM64.SUBS_imm (dest, src, imm) ->
        // SUBS immediate: like SUB but sets condition flags
        // sf=1 op=1 S=1 10001 shift=00 imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31          // 64-bit operation
        let op = 1u <<< 30          // SUB (vs ADD which has op=0)
        let s = 1u <<< 29           // Set flags (this is SUBS, not SUB)
        let opcode = 0b10001u <<< 24 // Fixed opcode bits
        let shift = 0u <<< 22       // No shift on immediate
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.MUL (dest, src1, src2) ->
        // MADD: sf=1 0 0 11011 000 Rm(5) 0 Ra=11111 Rn(5) Rd(5)
        // Using Ra=XZR(31) for pure multiply
        let sf = 1u <<< 31
        let op = 0b11011000u <<< 21
        let rm = (encodeReg src2) <<< 16
        let ra = 31u <<< 10  // XZR
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| ra ||| rn ||| rd]

    | ARM64.SDIV (dest, src1, src2) ->
        // SDIV: sf=1 0 0 11010110 Rm(5) 000011 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b11010110u <<< 21
        let rm = (encodeReg src2) <<< 16
        let fixedBits = 0b000011u <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | ARM64.UDIV (dest, src1, src2) ->
        // UDIV: sf=1 0 0 11010110 Rm(5) 000010 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b11010110u <<< 21
        let rm = (encodeReg src2) <<< 16
        let fixedBits = 0b000010u <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | ARM64.MSUB (dest, src1, src2, src3) ->
        // MSUB: sf=1 0 0 11011 000 Rm(5) 1 Ra(5) Rn(5) Rd(5)
        // Computes: Rd = Ra - (Rn * Rm)
        let sf = 1u <<< 31
        let op = 0b11011000u <<< 21
        let rm = (encodeReg src2) <<< 16
        let flagBit = 1u <<< 15  // Distinguishes MSUB from MADD
        let ra = (encodeReg src3) <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| flagBit ||| ra ||| rn ||| rd]

    | ARM64.MADD (dest, src1, src2, src3) ->
        // MADD: sf=1 0 0 11011 000 Rm(5) 0 Ra(5) Rn(5) Rd(5)
        // Computes: Rd = Ra + (Rn * Rm)
        let sf = 1u <<< 31
        let op = 0b11011000u <<< 21
        let rm = (encodeReg src2) <<< 16
        // flagBit = 0 for MADD (vs 1 for MSUB)
        let ra = (encodeReg src3) <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| ra ||| rn ||| rd]

    | ARM64.MOV_reg (dest, src) ->
        // Special case: MOV Xd, SP cannot use ORR (register 31 = XZR in ORR context)
        // Use ADD Xd, SP, #0 instead
        if src = ARM64.SP then
            // ADD immediate: sf=1 0 0 10001 shift(2) imm12(12) Rn(5) Rd(5)
            let sf = 1u <<< 31
            let op = 0b10001u <<< 24
            let shift = 0u <<< 22  // No shift
            let imm12 = 0u <<< 10  // Zero immediate
            let rn = 31u <<< 5     // SP
            let rd = encodeReg dest
            [sf ||| op ||| shift ||| imm12 ||| rn ||| rd]
        else
            // MOV is ORR with XZR: sf=1 01 01010 00 0 Rm(5) 000000 Rn=11111 Rd(5)
            // Bit 31: sf=1, Bit 30: 0, Bit 29: 1 (ORR not AND), Bits 28-21: 01010000
            let sf = 1u <<< 31
            let opc = 1u <<< 29  // Critical: bit 29 distinguishes ORR (1) from AND (0)
            let op = 0b01010000u <<< 21
            let rm = (encodeReg src) <<< 16
            let rn = 31u <<< 5  // XZR
            let rd = encodeReg dest
            [sf ||| opc ||| op ||| rm ||| rn ||| rd]

    | ARM64.STRB (src, addr, offset) ->
        // STRB immediate unsigned offset: 00 111 001 00 imm12 Rn Rt
        // Size=00 (byte), opc=00, bit24=1 for unsigned offset mode
        let size = 0u <<< 30  // Byte operation
        let vOpc = 0b11100100u <<< 22  // Fixed bits for STRB unsigned offset (bit 24 = 1)
        let imm12 = (uint32 offset &&& 0xFFFu) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg src
        [size ||| vOpc ||| imm12 ||| rn ||| rt]

    | ARM64.LDRB (dest, baseReg, indexReg) ->
        // LDRB (register offset): 00 111 000 01 1 Rm option S 10 Rn Rt
        // Size=00 (byte), V=0, opc=01 (load unsigned), register offset mode
        // option=011 (LSL), S=0 (no shift)
        let size = 0u <<< 30  // Byte operation (bits 31-30 = 00)
        let bits29to21 = 0b111000011u <<< 21  // 111 0 00 01 1 at bits 29-21
        let rm = (encodeReg indexReg) <<< 16
        let option = 0b011u <<< 13  // LSL extend
        let s = 0u <<< 12  // No shift
        let fixed2 = 0b10u <<< 10
        let rn = (encodeReg baseReg) <<< 5
        let rt = encodeReg dest
        [size ||| bits29to21 ||| rm ||| option ||| s ||| fixed2 ||| rn ||| rt]

    | ARM64.LDRB_imm (dest, baseReg, offset) ->
        // LDRB (unsigned offset): 00 111 001 01 imm12 Rn Rt
        // Size=00 (byte), V=0, opc=01 (load unsigned), unsigned offset mode
        let size = 0u <<< 30  // Byte operation
        let vOpc = 0b11100101u <<< 22  // Fixed bits for LDRB unsigned offset (opc=01)
        let imm12 = (uint32 offset &&& 0xFFFu) <<< 10
        let rn = (encodeReg baseReg) <<< 5
        let rt = encodeReg dest
        [size ||| vOpc ||| imm12 ||| rn ||| rt]

    | ARM64.STRB_reg (src, addr) ->
        // STRB (register): store byte to address in register
        // Use immediate offset 0: 00 111 001 00 000000000000 Rn Rt
        let size = 0u <<< 30  // Byte operation
        let vOpc = 0b11100100u <<< 22  // Fixed bits for STRB unsigned offset
        let imm12 = 0u <<< 10  // offset = 0
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg src
        [size ||| vOpc ||| imm12 ||| rn ||| rt]

    // Label-based branches - resolved via two-pass encoding (see encodeWithLabels)
    // These return empty here because they are only used during label resolution
    | ARM64.CBZ (reg, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.CBNZ (reg, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.B_label label ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.B_cond_label (cond, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.Label label ->
        // Pseudo-instruction: marks a label position (no machine code generated)
        []

    | ARM64.ADRP (dest, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.ADR (dest, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.ADD_label (dest, src, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    // Offset-based branches (for handcrafted runtime code with known offsets)
    | ARM64.CBZ_offset (reg, offset) ->
        // CBZ: sf 011010 0 imm19 Rt
        // Compare and Branch on Zero
        let sf = 1u <<< 31  // 64-bit register
        let op = 0b011010u <<< 25
        let flag = 0u <<< 24  // CBZ (vs CBNZ which has 1)
        // Offset is in instructions (4-byte units), sign-extended, stored as imm19
        let imm19 = ((uint32 offset) &&& 0x7FFFFu) <<< 5
        let rt = encodeReg reg
        [sf ||| op ||| flag ||| imm19 ||| rt]

    | ARM64.CBNZ_offset (reg, offset) ->
        // CBNZ: sf 011010 1 imm19 Rt
        // Compare and Branch on Non-Zero
        let sf = 1u <<< 31  // 64-bit register
        let op = 0b011010u <<< 25
        let flag = 1u <<< 24  // CBNZ (vs CBZ which has 0)
        // Offset is in instructions (4-byte units), sign-extended, stored as imm19
        let imm19 = ((uint32 offset) &&& 0x7FFFFu) <<< 5
        let rt = encodeReg reg
        [sf ||| op ||| flag ||| imm19 ||| rt]

    | ARM64.TBZ (reg, bit, offset) ->
        // TBZ: b5 011011 0 b40 imm14 Rt
        // Test bit and Branch if Zero
        // b5 = bit[5], b40 = bit[4:0]
        let b5 = (uint32 bit >>> 5) <<< 31
        let op = 0b011011u <<< 25
        let flag = 0u <<< 24  // TBZ (vs TBNZ which has 1)
        let b40 = (uint32 bit &&& 0x1Fu) <<< 19
        let imm14 = ((uint32 offset) &&& 0x3FFFu) <<< 5
        let rt = encodeReg reg
        [b5 ||| op ||| flag ||| b40 ||| imm14 ||| rt]

    | ARM64.TBNZ (reg, bit, offset) ->
        // TBNZ: b5 011011 1 b40 imm14 Rt
        // Test bit and Branch if Not Zero
        // b5 = bit[5], b40 = bit[4:0]
        let b5 = (uint32 bit >>> 5) <<< 31
        let op = 0b011011u <<< 25
        let flag = 1u <<< 24  // TBNZ (vs TBZ which has 0)
        let b40 = (uint32 bit &&& 0x1Fu) <<< 19
        let imm14 = ((uint32 offset) &&& 0x3FFFu) <<< 5
        let rt = encodeReg reg
        [b5 ||| op ||| flag ||| b40 ||| imm14 ||| rt]

    | ARM64.TBZ_label _ | ARM64.TBNZ_label _ ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.B offset ->
        // B: 000101 imm26
        // Unconditional branch
        // Offset is in instructions (4-byte units), sign-extended
        let op = 0b000101u <<< 26
        let imm26 = (uint32 offset) &&& 0x3FFFFFFu
        [op ||| imm26]

    | ARM64.B_cond (cond, offset) ->
        // B.cond: 01010100 imm19 0 cond
        // Conditional branch based on condition flags
        let op = 0b01010100u <<< 24
        let imm19 = ((uint32 offset) &&& 0x7FFFFu) <<< 5
        let condBits =
            match cond with
            | ARM64.EQ -> 0b0000u  // Equal (Z set)
            | ARM64.NE -> 0b0001u  // Not equal (Z clear)
            | ARM64.LT -> 0b1011u  // Less than (signed)
            | ARM64.GT -> 0b1100u  // Greater than (signed)
            | ARM64.LE -> 0b1101u  // Less than or equal (signed)
            | ARM64.GE -> 0b1010u  // Greater than or equal (signed)
        [op ||| imm19 ||| condBits]

    | ARM64.CMP_imm (src, imm) ->
        // CMP immediate is SUBS XZR, Rn, #imm (SUB with set flags, dest=XZR)
        // Encoding: sf=1 op=1 S=1 10001 shift(2) imm12(12) Rn(5) Rd=11111
        let sf = 1u <<< 31          // 64-bit operation
        let op = 1u <<< 30          // SUB (vs ADD)
        let s = 1u <<< 29           // Set flags (critical for CMP)
        let opcode = 0b10001u <<< 24
        let shift = 0u <<< 22       // No shift
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = 31u                // XZR (discard result, only flags matter)
        [sf ||| op ||| s ||| opcode ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.CMP_reg (src1, src2) ->
        // CMP register is SUBS XZR, Rn, Rm (SUB with set flags, dest=XZR)
        // Encoding: sf=1 op=1 S=1 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd=11111
        let sf = 1u <<< 31  // 64-bit
        let op = 1u <<< 30  // SUB
        let s = 1u <<< 29   // Set flags (critical for CMP)
        let opcode = 0b01011u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = 31u        // XZR (discard result)
        [sf ||| op ||| s ||| opcode ||| shift ||| rm ||| rn ||| rd]

    | ARM64.CSET (dest, cond) ->
        // CSET Rd, cond is CSINC Rd, XZR, XZR, invert(cond)
        // Encoding: sf=1 op=0 S=0 11010100 Rm=11111 cond(4) 01 Rn=11111 Rd(5)
        let sf = 1u <<< 31
        let op = 0u <<< 30
        let s = 0u <<< 29
        let opcode = 0b11010100u <<< 21
        let rm = 31u <<< 16  // XZR
        // Invert condition for CSINC
        let condCode =
            match cond with
            | ARM64.EQ -> 0b0001u  // Inverted from NE
            | ARM64.NE -> 0b0000u  // Inverted from EQ
            | ARM64.LT -> 0b1010u  // Inverted from GE
            | ARM64.GT -> 0b1101u  // Inverted from LE
            | ARM64.LE -> 0b1100u  // Inverted from GT
            | ARM64.GE -> 0b1011u  // Inverted from LT
        let condBits = condCode <<< 12
        let fixedBits = 0b01u <<< 10  // CSINC vs CSEL
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| rm ||| condBits ||| fixedBits ||| rn ||| rd]

    | ARM64.AND_reg (dest, src1, src2) ->
        // AND register: sf=1 opc=00 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 0u <<< 29  // AND (vs ORR which has opc=01)
        let op = 0b01010u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| shift ||| rm ||| rn ||| rd]

    | ARM64.AND_imm (dest, src, imm) ->
        // AND immediate: sf=1 opc=00 100100 N(1) immr(6) imms(6) Rn(5) Rd(5)
        // For 64-bit (sf=1, N=1), logical immediate encoding:
        // - immr = rotation amount
        // - imms = ones run length - 1
        // For power-of-2 minus 1 masks (0x1, 0x3, 0x7, etc.): immr=0, imms=popcount-1
        let sf = 1u <<< 31
        let opc = 0u <<< 29  // AND
        let op = 0b100100u <<< 23
        let n = 1u <<< 22  // N=1 for 64-bit element size
        // Calculate imms from the mask (assumes power-of-2 minus 1)
        let popcount = System.Numerics.BitOperations.PopCount(imm)
        let immr = 0u <<< 16
        let imms = (uint32 (popcount - 1)) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.ORR_reg (dest, src1, src2) ->
        // ORR register: sf=1 opc=01 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 1u <<< 29  // ORR
        let op = 0b01010u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| shift ||| rm ||| rn ||| rd]

    | ARM64.EOR_reg (dest, src1, src2) ->
        // EOR register: sf=1 opc=10 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 2u <<< 29  // EOR (vs AND=00, ORR=01)
        let op = 0b01010u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| shift ||| rm ||| rn ||| rd]

    | ARM64.LSL_reg (dest, src, shift) ->
        // LSLV (variable shift left): sf=1 0 0 11010110 Rm(5) 001000 Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let op = 0b11010110u <<< 21
        let rm = (encodeReg shift) <<< 16
        let fixedBits = 0b001000u <<< 10  // LSLV opcode
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | ARM64.LSR_reg (dest, src, shift) ->
        // LSRV (variable shift right): sf=1 0 0 11010110 Rm(5) 001001 Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let op = 0b11010110u <<< 21
        let rm = (encodeReg shift) <<< 16
        let fixedBits = 0b001001u <<< 10  // LSRV opcode
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | ARM64.LSL_imm (dest, src, shift) ->
        // LSL Rd, Rn, #shift is alias for UBFM Rd, Rn, #(64-shift), #(63-shift)
        // UBFM: sf=1 opc=10 100110 N=1 immr(6) imms(6) Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 2u <<< 29  // UBFM
        let op = 0b100110u <<< 23
        let n = 1u <<< 22  // N=1 for 64-bit
        let immr = (uint32 ((64 - shift) &&& 63)) <<< 16
        let imms = (uint32 ((63 - shift) &&& 63)) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.LSR_imm (dest, src, shift) ->
        // LSR Rd, Rn, #shift is alias for UBFM Rd, Rn, #shift, #63
        // UBFM: sf=1 opc=10 100110 N=1 immr(6) imms(6) Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 2u <<< 29  // UBFM
        let op = 0b100110u <<< 23
        let n = 1u <<< 22  // N=1 for 64-bit
        let immr = (uint32 (shift &&& 63)) <<< 16
        let imms = 63u <<< 10  // imms = 63 for LSR
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.MVN (dest, src) ->
        // MVN is ORN Rd, XZR, Rm (OR NOT with Rn=XZR)
        // Encoding: sf=1 opc=01 01010 shift=00 1 Rm(5) imm6=000000 Rn=11111 Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 1u <<< 29  // ORR-family
        let op = 0b01010u <<< 24
        let shift = 0u <<< 22  // No shift
        let n = 1u <<< 21  // NOT bit (distinguishes ORN from ORR)
        let rm = (encodeReg src) <<< 16
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| shift ||| n ||| rm ||| rn ||| rd]

    | ARM64.NEG (dest, src) ->
        // NEG: SUB dest, XZR, src
        // Encoding: sf=1 op=1 S=0 01011 shift=00 0 Rm(src) imm6=000000 Rn=11111(XZR) Rd(dest)
        let sf = 1u <<< 31  // 64-bit
        let op = 1u <<< 30  // Subtract
        let s = 0u <<< 29   // Don't set flags
        let opcode = 0b01011u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src) <<< 16
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| rm ||| rn ||| rd]

    // Stack operations
    | ARM64.STP (reg1, reg2, addr, offset) ->
        // STP (Store Pair) - signed offset addressing
        // Encoding: opc(2) 101 V(1) mode(2) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
        // For 64-bit registers: opc=10 (bits 31-30)
        // V=0 (integer), mode=10 (signed offset), L=0 (store)
        // Bits 29-22 = 1010 010 0 = 0b10100100
        let opc = 2u <<< 30  // 64-bit
        let fixedBits = 0b10100100u <<< 22  // STP: 101 0 010 0 (mode=signed offset, L=0)
        // Convert byte offset to 8-byte units and extract 7 bits
        let imm7 = ((uint32 (int offset / 8)) &&& 0x7Fu) <<< 15
        let rt2 = (encodeReg reg2) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg reg1
        [opc ||| fixedBits ||| imm7 ||| rt2 ||| rn ||| rt]

    | ARM64.LDP (reg1, reg2, addr, offset) ->
        // LDP (Load Pair) - signed offset addressing
        // Encoding: opc(2) 101 V(1) mode(2) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
        // V=0 (integer), mode=10 (signed offset), L=1 (load)
        // Bits 29-22 = 1010 010 1 = 0b10100101
        let opc = 2u <<< 30  // 64-bit
        let fixedBits = 0b10100101u <<< 22  // LDP: 101 0 010 1 (mode=signed offset, L=1)
        let imm7 = ((uint32 (int offset / 8)) &&& 0x7Fu) <<< 15
        let rt2 = (encodeReg reg2) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg reg1
        [opc ||| fixedBits ||| imm7 ||| rt2 ||| rn ||| rt]

    | ARM64.STP_pre (reg1, reg2, addr, offset) ->
        // STP (Store Pair) - pre-indexed addressing: addr += offset, then store
        // Encoding: opc(2) 101 V(1) mode(3) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
        // V=0 (integer), mode=011 (pre-indexed), L=0 (store)
        // Bits 29-22 = 1010 011 0 = 0b10100110
        let opc = 2u <<< 30  // 64-bit
        let fixedBits = 0b10100110u <<< 22  // STP pre-indexed: 101 0 011 0
        let imm7 = ((uint32 (int offset / 8)) &&& 0x7Fu) <<< 15
        let rt2 = (encodeReg reg2) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg reg1
        [opc ||| fixedBits ||| imm7 ||| rt2 ||| rn ||| rt]

    | ARM64.LDP_post (reg1, reg2, addr, offset) ->
        // LDP (Load Pair) - post-indexed addressing: load, then addr += offset
        // Encoding: opc(2) 101 V(1) mode(2) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
        // V=0 (integer), mode=01 (post-indexed), L=1 (load)
        // Bits 29-22 = 1010 001 1 = 0b10100011
        let opc = 2u <<< 30  // 64-bit
        let fixedBits = 0b10100011u <<< 22  // LDP post-indexed: 101 0 001 1
        let imm7 = ((uint32 (int offset / 8)) &&& 0x7Fu) <<< 15
        let rt2 = (encodeReg reg2) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg reg1
        [opc ||| fixedBits ||| imm7 ||| rt2 ||| rn ||| rt]

    | ARM64.STR (src, addr, offset) ->
        // STR (Store Register) - unsigned offset addressing
        // Encoding: size=11 111 001 00 imm12 Rn Rt
        // For 64-bit: size=11 (bits 31-30)
        // imm12 is unsigned offset in units of 8 bytes (bits 21-10)
        let size = 3u <<< 30  // 64-bit (11)
        let fixedBits = 0b11100100u <<< 22  // STR unsigned offset mode
        // Convert byte offset to 8-byte units and extract 12 bits
        let imm12 = ((uint32 (int offset / 8)) &&& 0xFFFu) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg src
        [size ||| fixedBits ||| imm12 ||| rn ||| rt]

    | ARM64.LDR (dest, addr, offset) ->
        // LDR (Load Register) - unsigned offset addressing
        // Encoding: size=11 111 001 01 imm12 Rn Rt
        // Same as STR but bit 22 = 1 for load
        let size = 3u <<< 30  // 64-bit (11)
        let fixedBits = 0b11100101u <<< 22  // LDR unsigned offset mode (bit 22=1)
        let imm12 = ((uint32 (int offset / 8)) &&& 0xFFFu) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg dest
        [size ||| fixedBits ||| imm12 ||| rn ||| rt]

    | ARM64.STUR (src, addr, offset) ->
        // STUR (Store Register Unscaled) - signed offset addressing
        // Encoding: size=11 111 000 00 0 imm9 00 Rn Rt
        // For 64-bit: size=11 (bits 31-30)
        // Fixed bits 29-21: 111000000 (unscaled store)
        // imm9 is signed offset in bytes (bits 20-12)
        let size = 3u <<< 30  // 64-bit (11)
        let fixedBits = 0b111000000u <<< 21  // STUR: bits 29-21 = 111000000
        // Extract 9-bit signed offset (sign-extend to 32-bit, then mask)
        let imm9 = ((uint32 offset) &&& 0x1FFu) <<< 12
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg src
        [size ||| fixedBits ||| imm9 ||| rn ||| rt]

    | ARM64.LDUR (dest, addr, offset) ->
        // LDUR (Load Register Unscaled) - signed offset addressing
        // Encoding: size=11 111 000 01 0 imm9 00 Rn Rt
        // Same as STUR but bit 22 = 1 for load
        // Fixed bits 29-21: 111000010 (unscaled load, bit 22=1)
        let size = 3u <<< 30  // 64-bit (11)
        let fixedBits = 0b111000010u <<< 21  // LDUR: bits 29-21 = 111000010 (bit 22=1)
        let imm9 = ((uint32 offset) &&& 0x1FFu) <<< 12
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg dest
        [size ||| fixedBits ||| imm9 ||| rn ||| rt]

    | ARM64.BL label ->
        // BL is handled in encodeWithLabels (label-based branch)
        // Resolved via two-pass encoding like other label-based branches
        []

    | ARM64.BLR reg ->
        // BLR: Branch with Link to Register
        // Encoding: 1101011 0 0 01 11111 0000 0 0 Rn 00000
        // 0xD63F0000 | (Rn << 5)
        let rn = encodeReg reg
        [0xD63F0000u ||| (rn <<< 5)]

    | ARM64.BR reg ->
        // BR: Branch to Register (no link, for tail calls)
        // Encoding: 1101011 0 0 00 11111 0000 0 0 Rn 00000
        // 0xD61F0000 | (Rn << 5)
        let rn = encodeReg reg
        [0xD61F0000u ||| (rn <<< 5)]

    | ARM64.RET ->
        // RET: 1101011 0 0 10 11111 0000 0 0 Rn=11110 00000
        // Default RET uses X30 (link register)
        [0xD65F03C0u]

    | ARM64.SVC imm ->
        // SVC: 11010100 000 imm16 000 01
        // Bits: 11010100000(31-21) imm16(20-5) 00001(4-0)
        let imm16 = uint32 imm
        [0xD4000001u ||| (imm16 <<< 5)]

    // Floating-point instructions
    | ARM64.LDR_fp (dest, addr, offset) ->
        // LDR (SIMD&FP) - unsigned offset addressing for double
        // Encoding: size=11 111 101 01 imm12 Rn Rt
        // size=11 for 64-bit (double), V=1 (FP), opc=01 (load)
        let size = 3u <<< 30  // 64-bit (double)
        let fixedBits = 0b11110101u <<< 22  // LDR FP unsigned offset mode
        // imm12 is unsigned offset in units of 8 bytes (like integer LDR 64-bit)
        let imm12 = ((uint32 (int offset / 8)) &&& 0xFFFu) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeFReg dest
        [size ||| fixedBits ||| imm12 ||| rn ||| rt]

    | ARM64.STR_fp (src, addr, offset) ->
        // STR (SIMD&FP) - unsigned offset addressing for double
        // Encoding: size=11 111 101 00 imm12 Rn Rt
        // size=11 for 64-bit (double), V=1 (FP), opc=00 (store)
        let size = 3u <<< 30  // 64-bit (double)
        let fixedBits = 0b11110100u <<< 22  // STR FP unsigned offset mode
        let imm12 = ((uint32 (int offset / 8)) &&& 0xFFFu) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeFReg src
        [size ||| fixedBits ||| imm12 ||| rn ||| rt]

    | ARM64.STP_fp (freg1, freg2, addr, offset) ->
        // STP (SIMD&FP) - signed offset addressing for double
        // Encoding: opc(2) 101 V(1) mode(2) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
        // opc=01 for 64-bit (D registers), V=1 (FP), mode=010 (signed offset), L=0 (store)
        // Bits 29-22 = 101 1 010 0 = 0b10110100
        let opc = 1u <<< 30  // 01 for 64-bit FP
        let fixedBits = 0b10110100u <<< 22  // STP FP: 101 1 010 0
        let imm7 = ((uint32 (int offset / 8)) &&& 0x7Fu) <<< 15
        let rt2 = (encodeFReg freg2) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeFReg freg1
        [opc ||| fixedBits ||| imm7 ||| rt2 ||| rn ||| rt]

    | ARM64.LDP_fp (freg1, freg2, addr, offset) ->
        // LDP (SIMD&FP) - signed offset addressing for double
        // Encoding: opc(2) 101 V(1) mode(2) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
        // opc=01 for 64-bit (D registers), V=1 (FP), mode=010 (signed offset), L=1 (load)
        // Bits 29-22 = 101 1 010 1 = 0b10110101
        let opc = 1u <<< 30  // 01 for 64-bit FP
        let fixedBits = 0b10110101u <<< 22  // LDP FP: 101 1 010 1
        let imm7 = ((uint32 (int offset / 8)) &&& 0x7Fu) <<< 15
        let rt2 = (encodeFReg freg2) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeFReg freg1
        [opc ||| fixedBits ||| imm7 ||| rt2 ||| rn ||| rt]

    | ARM64.FADD (dest, src1, src2) ->
        // FADD (scalar, double): 0001 1110 01 1 Rm 0010 10 Rn Rd
        // ftype=01 (double), opcode=0010 (add)
        let fixedBits = 0b00011110011u <<< 21
        let rm = (encodeFReg src2) <<< 16
        let opcode = 0b001010u <<< 10  // FADD
        let rn = (encodeFReg src1) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FSUB (dest, src1, src2) ->
        // FSUB (scalar, double): 0001 1110 01 1 Rm 0011 10 Rn Rd
        let fixedBits = 0b00011110011u <<< 21
        let rm = (encodeFReg src2) <<< 16
        let opcode = 0b001110u <<< 10  // FSUB
        let rn = (encodeFReg src1) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FMUL (dest, src1, src2) ->
        // FMUL (scalar, double): 0001 1110 01 1 Rm 0000 10 Rn Rd
        let fixedBits = 0b00011110011u <<< 21
        let rm = (encodeFReg src2) <<< 16
        let opcode = 0b000010u <<< 10  // FMUL
        let rn = (encodeFReg src1) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FDIV (dest, src1, src2) ->
        // FDIV (scalar, double): 0001 1110 01 1 Rm 0001 10 Rn Rd
        let fixedBits = 0b00011110011u <<< 21
        let rm = (encodeFReg src2) <<< 16
        let opcode = 0b000110u <<< 10  // FDIV
        let rn = (encodeFReg src1) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FNEG (dest, src) ->
        // FNEG (scalar, double): 0x1E614000 for D0, D0
        // Encoding: 0001 1110 0110 0001 0100 0000 Rn Rd
        let fixedBits = 0b00011110011u <<< 21
        let rm = 1u <<< 16  // bit 16 set for FNEG
        let opcode = 0b010000u <<< 10  // FNEG opcode (16)
        let rn = (encodeFReg src) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FABS (dest, src) ->
        // FABS (scalar, double): 0x1E60C000 for D0, D0
        // Encoding: 0001 1110 0110 0000 1100 0000 Rn Rd
        let fixedBits = 0b00011110011u <<< 21
        let rm = 0u <<< 16  // bit 16 clear for FABS
        let opcode = 0b110000u <<< 10  // FABS opcode (48)
        let rn = (encodeFReg src) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FSQRT (dest, src) ->
        // FSQRT (scalar, double): 0x1E61C000 for D0, D0
        // Encoding: 0001 1110 0110 0001 1100 0000 Rn Rd
        let fixedBits = 0b00011110011u <<< 21
        let rm = 1u <<< 16  // bit 16 set for FSQRT
        let opcode = 0b110000u <<< 10  // FSQRT opcode (48)
        let rn = (encodeFReg src) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FCMP (src1, src2) ->
        // FCMP (scalar, double): 0001 1110 01 1 Rm 00 1000 Rn 00 000
        // Encoding: 0001 1110 01 1 Rm 00 1000 Rn 00 opc=000
        let fixedBits = 0b00011110011u <<< 21
        let rm = (encodeFReg src2) <<< 16
        let opcode = 0b001000u <<< 10  // FCMP
        let rn = (encodeFReg src1) <<< 5
        let opc = 0b00000u  // Compare (not with zero)
        [fixedBits ||| rm ||| opcode ||| rn ||| opc]

    | ARM64.FMOV_reg (dest, src) ->
        // FMOV (register, double): 0001 1110 01 1 00000 010000 Rn Rd
        let fixedBits = 0b00011110011u <<< 21
        let rm = 0u <<< 16  // Unused
        let opcode = 0b010000u <<< 10  // FMOV
        let rn = (encodeFReg src) <<< 5
        let rd = encodeFReg dest
        [fixedBits ||| rm ||| opcode ||| rn ||| rd]

    | ARM64.FMOV_to_gp (dest, src) ->
        // FMOV (scalar to GP, double): 1001 1110 01 1 00110 000000 Vn Rd
        // sf=1, ftype=01 (double), rmode=00, opcode=110
        // Move 64-bit FP register to GP register (bit-for-bit)
        let sf = 1u <<< 31
        let fixedBits = 0b0011110011u <<< 21
        let opcode1 = 0b00110u <<< 16  // FMOV to general
        let opcode2 = 0b000000u <<< 10
        let rn = (encodeFReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| fixedBits ||| opcode1 ||| opcode2 ||| rn ||| rd]

    | ARM64.FMOV_from_gp (dest, src) ->
        // FMOV (general to scalar, double): 1001 1110 01 1 00111 000000 Rn Vd
        // sf=1, ftype=01 (double), rmode=00, opcode=111
        // Move GP register to 64-bit FP register (bit-for-bit)
        let sf = 1u <<< 31
        let fixedBits = 0b0011110011u <<< 21
        let opcode1 = 0b00111u <<< 16  // FMOV from general
        let opcode2 = 0b000000u <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeFReg dest
        [sf ||| fixedBits ||| opcode1 ||| opcode2 ||| rn ||| rd]

    | ARM64.SCVTF (dest, src) ->
        // SCVTF (scalar, integer to FP, double): 1001 1110 01 1 00010 000000 Rn Rd
        // sf=1 (64-bit int), ftype=01 (double), rmode=00, opcode=010
        let sf = 1u <<< 31
        let fixedBits = 0b0011110011u <<< 21
        let opcode1 = 0b00010u <<< 16  // SCVTF
        let opcode2 = 0b000000u <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeFReg dest
        [sf ||| fixedBits ||| opcode1 ||| opcode2 ||| rn ||| rd]

    | ARM64.FCVTZS (dest, src) ->
        // FCVTZS (scalar, FP to integer, double): 1001 1110 01 1 11000 000000 Rn Rd
        // sf=1 (64-bit int), ftype=01 (double), rmode=11 (toward zero), opcode=000
        let sf = 1u <<< 31
        let fixedBits = 0b0011110011u <<< 21
        let opcode1 = 0b11000u <<< 16  // FCVTZS
        let opcode2 = 0b000000u <<< 10
        let rn = (encodeFReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| fixedBits ||| opcode1 ||| opcode2 ||| rn ||| rd]

    // Sign/zero extension instructions (for integer overflow truncation)
    // These are encoded using SBFM/UBFM (Signed/Unsigned Bitfield Move)
    | ARM64.SXTB (dest, src) ->
        // SXTB: SBFM Xd, Xn, #0, #7 (sign-extend byte to 64-bit)
        // Encoding: sf=1 opc=00 100110 N=1 immr=0 imms=7 Rn Rd
        let sf = 1u <<< 31
        let opc = 0u <<< 29  // SBFM (signed)
        let fixedBits = 0b100110u <<< 23
        let n = 1u <<< 22  // N=1 for 64-bit
        let immr = 0u <<< 16  // rotate by 0
        let imms = 7u <<< 10  // extract bits 0-7 (byte)
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| fixedBits ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.SXTH (dest, src) ->
        // SXTH: SBFM Xd, Xn, #0, #15 (sign-extend halfword to 64-bit)
        let sf = 1u <<< 31
        let opc = 0u <<< 29  // SBFM
        let fixedBits = 0b100110u <<< 23
        let n = 1u <<< 22
        let immr = 0u <<< 16
        let imms = 15u <<< 10  // extract bits 0-15 (halfword)
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| fixedBits ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.SXTW (dest, src) ->
        // SXTW: SBFM Xd, Xn, #0, #31 (sign-extend word to 64-bit)
        let sf = 1u <<< 31
        let opc = 0u <<< 29  // SBFM
        let fixedBits = 0b100110u <<< 23
        let n = 1u <<< 22
        let immr = 0u <<< 16
        let imms = 31u <<< 10  // extract bits 0-31 (word)
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| fixedBits ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.UXTB (dest, src) ->
        // UXTB: UBFM Xd, Xn, #0, #7 (zero-extend byte to 64-bit)
        // Encoding: sf=1 opc=10 100110 N=1 immr=0 imms=7 Rn Rd
        let sf = 1u <<< 31
        let opc = 2u <<< 29  // UBFM (unsigned)
        let fixedBits = 0b100110u <<< 23
        let n = 1u <<< 22
        let immr = 0u <<< 16
        let imms = 7u <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| fixedBits ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.UXTH (dest, src) ->
        // UXTH: UBFM Xd, Xn, #0, #15 (zero-extend halfword to 64-bit)
        let sf = 1u <<< 31
        let opc = 2u <<< 29  // UBFM
        let fixedBits = 0b100110u <<< 23
        let n = 1u <<< 22
        let immr = 0u <<< 16
        let imms = 15u <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| fixedBits ||| n ||| immr ||| imms ||| rn ||| rd]

    | ARM64.UXTW (dest, src) ->
        // UXTW: UBFM Xd, Xn, #0, #31 (zero-extend word to 64-bit)
        let sf = 1u <<< 31
        let opc = 2u <<< 29  // UBFM
        let fixedBits = 0b100110u <<< 23
        let n = 1u <<< 22
        let immr = 0u <<< 16
        let imms = 31u <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| fixedBits ||| n ||| immr ||| imms ||| rn ||| rd]

/// Two-Pass Encoding for Label Resolution

/// Pass 1: Compute byte offset for each label
/// Returns map from label name to byte offset
/// Note: Labels mark positions but don't generate code, so we track actual code offset
let computeLabelPositions (instructions: ARM64.Instr list) : Map<string, int> =
    let rec loop instrs offset labelMap =
        match instrs with
        | [] -> labelMap
        | instr :: rest ->
            match instr with
            | ARM64.Label name ->
                // Record this label's position, don't increment offset (pseudo-instruction)
                loop rest offset (Map.add name offset labelMap)
            | _ ->
                // All ARM64 instructions are fixed 4 bytes
                loop rest (offset + 4) labelMap
    loop instructions 0 Map.empty

/// Pass 1 (symbolic): Compute byte offset for each label
let computeSymbolicLabelPositions (instructions: ARM64Symbolic.Instr list) : Map<string, int> =
    let rec loop instrs offset labelMap =
        match instrs with
        | [] -> labelMap
        | instr :: rest ->
            match instr with
            | ARM64Symbolic.Label name ->
                loop rest offset (Map.add name offset labelMap)
            | _ ->
                loop rest (offset + 4) labelMap
    loop instructions 0 Map.empty

/// Compute the size of code in bytes from symbolic instructions
let getSymbolicCodeSize (instructions: ARM64Symbolic.Instr list) : int =
    instructions
    |> List.sumBy (fun instr ->
        match instr with
        | ARM64Symbolic.Label _ -> 0
        | _ -> 4)

let private resolveLabelRefWithPools
    (stringPool: LiteralPool.StringPool)
    (floatPool: LiteralPool.FloatPool)
    (labelRef: ARM64Symbolic.LabelRef)
    : string =
    match labelRef with
    | ARM64Symbolic.CodeLabel name -> name
    | ARM64Symbolic.DataLabel dataRef ->
        match dataRef with
        | ARM64Symbolic.Named name -> name
        | ARM64Symbolic.StringLiteral value ->
            match Map.tryFind value stringPool.StringToId with
            | Some idx -> "str_" + string idx
            | None -> Crash.crash $"String literal label missing from pool: {value}"
        | ARM64Symbolic.FloatLiteral value ->
            let bits = System.BitConverter.DoubleToInt64Bits value
            match Map.tryFind bits floatPool.FloatBitsToId with
            | Some idx -> "_float" + string idx
            | None -> Crash.crash $"Float literal label missing from pool: {value}"

let private resolveSymbolicInstr
    (stringPool: LiteralPool.StringPool)
    (floatPool: LiteralPool.FloatPool)
    (instr: ARM64Symbolic.Instr)
    : ARM64.Instr =
    match instr with
    | ARM64Symbolic.MOVZ (dest, imm, shift) -> ARM64.MOVZ (dest, imm, shift)
    | ARM64Symbolic.MOVN (dest, imm, shift) -> ARM64.MOVN (dest, imm, shift)
    | ARM64Symbolic.MOVK (dest, imm, shift) -> ARM64.MOVK (dest, imm, shift)
    | ARM64Symbolic.ADD_imm (dest, src, imm) -> ARM64.ADD_imm (dest, src, imm)
    | ARM64Symbolic.ADD_reg (dest, src1, src2) -> ARM64.ADD_reg (dest, src1, src2)
    | ARM64Symbolic.ADD_shifted (dest, src1, src2, shift) -> ARM64.ADD_shifted (dest, src1, src2, shift)
    | ARM64Symbolic.SUB_imm (dest, src, imm) -> ARM64.SUB_imm (dest, src, imm)
    | ARM64Symbolic.SUB_imm12 (dest, src, imm) -> ARM64.SUB_imm12 (dest, src, imm)
    | ARM64Symbolic.SUB_reg (dest, src1, src2) -> ARM64.SUB_reg (dest, src1, src2)
    | ARM64Symbolic.SUB_shifted (dest, src1, src2, shift) -> ARM64.SUB_shifted (dest, src1, src2, shift)
    | ARM64Symbolic.SUBS_imm (dest, src, imm) -> ARM64.SUBS_imm (dest, src, imm)
    | ARM64Symbolic.MUL (dest, src1, src2) -> ARM64.MUL (dest, src1, src2)
    | ARM64Symbolic.SDIV (dest, src1, src2) -> ARM64.SDIV (dest, src1, src2)
    | ARM64Symbolic.UDIV (dest, src1, src2) -> ARM64.UDIV (dest, src1, src2)
    | ARM64Symbolic.MSUB (dest, src1, src2, src3) -> ARM64.MSUB (dest, src1, src2, src3)
    | ARM64Symbolic.MADD (dest, src1, src2, src3) -> ARM64.MADD (dest, src1, src2, src3)
    | ARM64Symbolic.CMP_imm (src, imm) -> ARM64.CMP_imm (src, imm)
    | ARM64Symbolic.CMP_reg (src1, src2) -> ARM64.CMP_reg (src1, src2)
    | ARM64Symbolic.CSET (dest, cond) -> ARM64.CSET (dest, cond)
    | ARM64Symbolic.AND_reg (dest, src1, src2) -> ARM64.AND_reg (dest, src1, src2)
    | ARM64Symbolic.AND_imm (dest, src, imm) -> ARM64.AND_imm (dest, src, imm)
    | ARM64Symbolic.ORR_reg (dest, src1, src2) -> ARM64.ORR_reg (dest, src1, src2)
    | ARM64Symbolic.EOR_reg (dest, src1, src2) -> ARM64.EOR_reg (dest, src1, src2)
    | ARM64Symbolic.LSL_reg (dest, src, shift) -> ARM64.LSL_reg (dest, src, shift)
    | ARM64Symbolic.LSR_reg (dest, src, shift) -> ARM64.LSR_reg (dest, src, shift)
    | ARM64Symbolic.LSL_imm (dest, src, shift) -> ARM64.LSL_imm (dest, src, shift)
    | ARM64Symbolic.LSR_imm (dest, src, shift) -> ARM64.LSR_imm (dest, src, shift)
    | ARM64Symbolic.MVN (dest, src) -> ARM64.MVN (dest, src)
    | ARM64Symbolic.MOV_reg (dest, src) -> ARM64.MOV_reg (dest, src)
    | ARM64Symbolic.STRB (src, addr, offset) -> ARM64.STRB (src, addr, offset)
    | ARM64Symbolic.LDRB (dest, baseAddr, index) -> ARM64.LDRB (dest, baseAddr, index)
    | ARM64Symbolic.LDRB_imm (dest, baseAddr, offset) -> ARM64.LDRB_imm (dest, baseAddr, offset)
    | ARM64Symbolic.STRB_reg (src, addr) -> ARM64.STRB_reg (src, addr)
    | ARM64Symbolic.STP (reg1, reg2, addr, offset) -> ARM64.STP (reg1, reg2, addr, offset)
    | ARM64Symbolic.STP_pre (reg1, reg2, addr, offset) -> ARM64.STP_pre (reg1, reg2, addr, offset)
    | ARM64Symbolic.LDP (reg1, reg2, addr, offset) -> ARM64.LDP (reg1, reg2, addr, offset)
    | ARM64Symbolic.LDP_post (reg1, reg2, addr, offset) -> ARM64.LDP_post (reg1, reg2, addr, offset)
    | ARM64Symbolic.STR (src, addr, offset) -> ARM64.STR (src, addr, offset)
    | ARM64Symbolic.LDR (dest, addr, offset) -> ARM64.LDR (dest, addr, offset)
    | ARM64Symbolic.STUR (src, addr, offset) -> ARM64.STUR (src, addr, offset)
    | ARM64Symbolic.LDUR (dest, addr, offset) -> ARM64.LDUR (dest, addr, offset)
    | ARM64Symbolic.BL label -> ARM64.BL label
    | ARM64Symbolic.BLR reg -> ARM64.BLR reg
    | ARM64Symbolic.BR reg -> ARM64.BR reg
    | ARM64Symbolic.CBZ (reg, label) -> ARM64.CBZ (reg, label)
    | ARM64Symbolic.CBNZ (reg, label) -> ARM64.CBNZ (reg, label)
    | ARM64Symbolic.B_label label -> ARM64.B_label label
    | ARM64Symbolic.B_cond_label (cond, label) -> ARM64.B_cond_label (cond, label)
    | ARM64Symbolic.CBZ_offset (reg, offset) -> ARM64.CBZ_offset (reg, offset)
    | ARM64Symbolic.CBNZ_offset (reg, offset) -> ARM64.CBNZ_offset (reg, offset)
    | ARM64Symbolic.TBZ (reg, bit, offset) -> ARM64.TBZ (reg, bit, offset)
    | ARM64Symbolic.TBNZ (reg, bit, offset) -> ARM64.TBNZ (reg, bit, offset)
    | ARM64Symbolic.TBZ_label (reg, bit, label) -> ARM64.TBZ_label (reg, bit, label)
    | ARM64Symbolic.TBNZ_label (reg, bit, label) -> ARM64.TBNZ_label (reg, bit, label)
    | ARM64Symbolic.B offset -> ARM64.B offset
    | ARM64Symbolic.B_cond (cond, offset) -> ARM64.B_cond (cond, offset)
    | ARM64Symbolic.NEG (dest, src) -> ARM64.NEG (dest, src)
    | ARM64Symbolic.RET -> ARM64.RET
    | ARM64Symbolic.SVC imm -> ARM64.SVC imm
    | ARM64Symbolic.Label name -> ARM64.Label name
    | ARM64Symbolic.ADRP (dest, labelRef) ->
        let label = resolveLabelRefWithPools stringPool floatPool labelRef
        ARM64.ADRP (dest, label)
    | ARM64Symbolic.ADD_label (dest, src, labelRef) ->
        let label = resolveLabelRefWithPools stringPool floatPool labelRef
        ARM64.ADD_label (dest, src, label)
    | ARM64Symbolic.ADR (dest, labelRef) ->
        let label = resolveLabelRefWithPools stringPool floatPool labelRef
        ARM64.ADR (dest, label)
    | ARM64Symbolic.LDR_fp (dest, addr, offset) -> ARM64.LDR_fp (dest, addr, offset)
    | ARM64Symbolic.STR_fp (src, addr, offset) -> ARM64.STR_fp (src, addr, offset)
    | ARM64Symbolic.STP_fp (freg1, freg2, addr, offset) -> ARM64.STP_fp (freg1, freg2, addr, offset)
    | ARM64Symbolic.LDP_fp (freg1, freg2, addr, offset) -> ARM64.LDP_fp (freg1, freg2, addr, offset)
    | ARM64Symbolic.FADD (dest, src1, src2) -> ARM64.FADD (dest, src1, src2)
    | ARM64Symbolic.FSUB (dest, src1, src2) -> ARM64.FSUB (dest, src1, src2)
    | ARM64Symbolic.FMUL (dest, src1, src2) -> ARM64.FMUL (dest, src1, src2)
    | ARM64Symbolic.FDIV (dest, src1, src2) -> ARM64.FDIV (dest, src1, src2)
    | ARM64Symbolic.FNEG (dest, src) -> ARM64.FNEG (dest, src)
    | ARM64Symbolic.FABS (dest, src) -> ARM64.FABS (dest, src)
    | ARM64Symbolic.FSQRT (dest, src) -> ARM64.FSQRT (dest, src)
    | ARM64Symbolic.FCMP (src1, src2) -> ARM64.FCMP (src1, src2)
    | ARM64Symbolic.FMOV_reg (dest, src) -> ARM64.FMOV_reg (dest, src)
    | ARM64Symbolic.FMOV_to_gp (dest, src) -> ARM64.FMOV_to_gp (dest, src)
    | ARM64Symbolic.FMOV_from_gp (dest, src) -> ARM64.FMOV_from_gp (dest, src)
    | ARM64Symbolic.SCVTF (dest, src) -> ARM64.SCVTF (dest, src)
    | ARM64Symbolic.FCVTZS (dest, src) -> ARM64.FCVTZS (dest, src)
    | ARM64Symbolic.SXTB (dest, src) -> ARM64.SXTB (dest, src)
    | ARM64Symbolic.SXTH (dest, src) -> ARM64.SXTH (dest, src)
    | ARM64Symbolic.SXTW (dest, src) -> ARM64.SXTW (dest, src)
    | ARM64Symbolic.UXTB (dest, src) -> ARM64.UXTB (dest, src)
    | ARM64Symbolic.UXTH (dest, src) -> ARM64.UXTH (dest, src)
    | ARM64Symbolic.UXTW (dest, src) -> ARM64.UXTW (dest, src)

let private tryFindDataLabel
    (label: string)
    (stringLabels: Map<string, int>)
    (floatLabels: Map<string, int>)
    (dataLabels: Map<string, int>)
    : int option =
    if label.StartsWith("str_") then
        Map.tryFind label stringLabels
    elif label.StartsWith("_float") then
        Map.tryFind label floatLabels
    else
        Map.tryFind label dataLabels

/// Encode an instruction with label resolution
/// currentOffset: byte offset of current instruction
let encodeWithLabels
    (instr: ARM64.Instr)
    (currentOffset: int)
    (codeLabels: Map<string, int>)
    (stringLabels: Map<string, int>)
    (floatLabels: Map<string, int>)
    (dataLabels: Map<string, int>)
    : ARM64.MachineCode list =
    match instr with
    // Label-based branches - resolve to offsets
    | ARM64.CBZ (reg, label) ->
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            // Compute relative offset in instructions (divide by 4)
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as CBZ with immediate offset
            let sf = 1u <<< 31
            let op = 0b011010u <<< 25
            let flag = 0u <<< 24  // CBZ
            let imm19 = ((uint32 instrOffset) &&& 0x7FFFFu) <<< 5
            let rt = encodeReg reg
            [sf ||| op ||| flag ||| imm19 ||| rt]
        | None ->
            Crash.crash $"CBZ: Label '{label}' not found in labelMap"

    | ARM64.CBNZ (reg, label) ->
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as CBNZ with immediate offset
            let sf = 1u <<< 31
            let op = 0b011010u <<< 25
            let flag = 1u <<< 24  // CBNZ
            let imm19 = ((uint32 instrOffset) &&& 0x7FFFFu) <<< 5
            let rt = encodeReg reg
            [sf ||| op ||| flag ||| imm19 ||| rt]
        | None ->
            Crash.crash $"CBNZ: Label '{label}' not found in labelMap"

    | ARM64.TBZ_label (reg, bit, label) ->
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as TBZ with immediate offset
            let b5 = (uint32 bit >>> 5) <<< 31
            let op = 0b011011u <<< 25
            let flag = 0u <<< 24  // TBZ
            let b40 = (uint32 bit &&& 0x1Fu) <<< 19
            let imm14 = ((uint32 instrOffset) &&& 0x3FFFu) <<< 5
            let rt = encodeReg reg
            [b5 ||| op ||| flag ||| b40 ||| imm14 ||| rt]
        | None ->
            Crash.crash $"TBZ: Label '{label}' not found in labelMap"

    | ARM64.TBNZ_label (reg, bit, label) ->
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as TBNZ with immediate offset
            let b5 = (uint32 bit >>> 5) <<< 31
            let op = 0b011011u <<< 25
            let flag = 1u <<< 24  // TBNZ
            let b40 = (uint32 bit &&& 0x1Fu) <<< 19
            let imm14 = ((uint32 instrOffset) &&& 0x3FFFu) <<< 5
            let rt = encodeReg reg
            [b5 ||| op ||| flag ||| b40 ||| imm14 ||| rt]
        | None ->
            Crash.crash $"TBNZ: Label '{label}' not found in labelMap"

    | ARM64.B_label label ->
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as B with immediate offset
            let op = 0b000101u <<< 26
            let imm26 = (uint32 instrOffset) &&& 0x3FFFFFFu
            [op ||| imm26]
        | None ->
            Crash.crash $"B: Label '{label}' not found in labelMap"

    | ARM64.B_cond_label (cond, label) ->
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // B.cond: 01010100 imm19 0 cond
            let op = 0b01010100u <<< 24
            let imm19 = ((uint32 instrOffset) &&& 0x7FFFFu) <<< 5
            let condBits =
                match cond with
                | ARM64.EQ -> 0b0000u
                | ARM64.NE -> 0b0001u
                | ARM64.LT -> 0b1011u
                | ARM64.GT -> 0b1100u
                | ARM64.LE -> 0b1101u
                | ARM64.GE -> 0b1010u
            [op ||| imm19 ||| condBits]
        | None ->
            Crash.crash $"B.cond: Label '{label}' not found in labelMap"

    | ARM64.BL label ->
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // BL encoding: 1 00101 imm26
            // Bit 31 = 1 (distinguishes BL from B which has bit 31 = 0)
            let op = 0b100101u <<< 26  // BL opcode (bit 31=1, bits 30-26=00101)
            let imm26 = (uint32 instrOffset) &&& 0x3FFFFFFu
            [op ||| imm26]
        | None ->
            Crash.crash $"BL: Label '{label}' not found in labelMap"

    | ARM64.Label _ ->
        // Pseudo-instruction: no machine code
        []

    | ARM64.ADRP (dest, label) ->
        // ADRP: form PC-relative address to 4KB page
        // Encoding: 1 immlo(2) 10000 immhi(19) Rd(5)
        // The label should point to data in .rodata section
        match tryFindDataLabel label stringLabels floatLabels dataLabels with
        | Some targetOffset ->
            // Compute page-relative offset (4KB pages)
            // ADRP uses the page containing PC, so we compute:
            // page_offset = ((target & ~0xFFF) - (pc & ~0xFFF)) >> 12
            let pcPage = currentOffset &&& (~~~0xFFF)
            let targetPage = targetOffset &&& (~~~0xFFF)
            let pageOffset = (targetPage - pcPage) / 4096
            // Encode
            let rd = encodeReg dest
            let immlo = ((uint32 pageOffset) &&& 0b11u) <<< 29
            let immhi = ((uint32 pageOffset >>> 2) &&& 0x7FFFFu) <<< 5
            let op = 1u <<< 31  // ADRP (bit 31=1)
            let opcode = 0b10000u <<< 24
            [op ||| immlo ||| opcode ||| immhi ||| rd]
        | None ->
            Crash.crash $"ADRP: Label '{label}' not found in labelMap"

    | ARM64.ADR (dest, label) ->
        // ADR: form PC-relative address
        // Encoding: 0 immlo(2) 10000 immhi(19) Rd(5)
        // immlo is bits 0-1, immhi is bits 2-20 of the 21-bit signed offset
        match Map.tryFind label codeLabels with
        | Some targetOffset ->
            // Compute byte offset from current PC to label
            let byteOffset = targetOffset - currentOffset
            // ADR has a ±1MB range (21-bit signed immediate)
            // Encode
            let rd = encodeReg dest
            let immlo = ((uint32 byteOffset) &&& 0b11u) <<< 29
            let immhi = ((uint32 byteOffset >>> 2) &&& 0x7FFFFu) <<< 5
            let op = 0u <<< 31  // ADR (bit 31=0, vs ADRP which has bit 31=1)
            let opcode = 0b10000u <<< 24
            [op ||| immlo ||| opcode ||| immhi ||| rd]
        | None ->
            Crash.crash $"ADR: Label '{label}' not found in labelMap"

    | ARM64.ADD_label (dest, src, label) ->
        // ADD with label offset (page offset portion)
        // Used with ADRP to get full address
        // This adds the lower 12 bits of the address (page offset)
        match tryFindDataLabel label stringLabels floatLabels dataLabels with
        | Some targetOffset ->
            // Get the 12-bit page offset
            let pageOffset = targetOffset &&& 0xFFF
            // Encode as ADD immediate
            let sf = 1u <<< 31  // 64-bit
            let op = 0b00100010u <<< 23  // ADD immediate opcode
            let shift = 0u <<< 22  // No shift
            let imm12 = ((uint32 pageOffset) &&& 0xFFFu) <<< 10
            let rn = encodeReg src <<< 5
            let rd = encodeReg dest
            [sf ||| op ||| shift ||| imm12 ||| rn ||| rd]
        | None ->
            Crash.crash $"ADD_label: Label '{label}' not found in labelMap"

    // All other instructions: use single-pass encoding
    | _ ->
        encode instr

/// Compute the size of code in bytes (for placing string data after code)
let getCodeSize (instructions: ARM64.Instr list) : int =
    instructions
    |> List.sumBy (fun instr ->
        match instr with
        | ARM64.Label _ -> 0  // Labels don't generate code
        | _ -> 4)  // All ARM64 instructions are 4 bytes

/// Compute float label positions given code file offset, code size, and float pool
/// Returns map from "_floatN" to byte offset (relative to segment/file start)
/// Floats are stored as 8-byte IEEE 754 doubles, aligned to 8 bytes
let computeFloatLabels (codeFileOffset: int) (codeSize: int) (floatPool: LiteralPool.FloatPool) : Map<string, int> =
    if floatPool.Floats.IsEmpty then
        Map.empty
    else
        // Sort by index to ensure consistent ordering
        let sortedFloats =
            floatPool.Floats
            |> Map.toList
            |> List.sortBy fst

        // Build label map with offsets using fold
        // Floats start after headers + code, 8-byte aligned
        let startOffset = codeFileOffset + codeSize
        let alignedStart = (startOffset + 7) &&& (~~~7)

        sortedFloats
        |> List.fold (fun (offset, labelMap) (idx, _floatVal) ->
            let label = "_float" + string idx
            let newMap = Map.add label offset labelMap
            (offset + 8, newMap))  // Each double is 8 bytes
            (alignedStart, Map.empty)
        |> snd

/// Compute string label positions given code file offset, code size, and string pool
/// Returns map from "_strN" to byte offset (relative to segment/file start)
/// codeFileOffset: where code starts in the file/segment
/// Compute the size of the float pool in bytes
let getFloatPoolSize (floatPool: LiteralPool.FloatPool) : int =
    floatPool.Floats.Count * 8  // Each double is 8 bytes

let computeStringLabels (codeFileOffset: int) (codeSize: int) (floatPoolSize: int) (stringPool: LiteralPool.StringPool) : Map<string, int> =
    if stringPool.Strings.IsEmpty then
        Map.empty
    else
        // Sort by index to ensure consistent ordering
        let sortedStrings =
            stringPool.Strings
            |> Map.toList
            |> List.sortBy fst

        // Build label map with offsets using fold
        // Strings start after headers + code + floats
        // Float pool is 8-byte aligned, so account for alignment
        let floatStart = (codeFileOffset + codeSize + 7) &&& (~~~7)
        let startOffset = floatStart + floatPoolSize

        // Each string has format: [length:8][data:N][null:1]
        sortedStrings
        |> List.fold (fun (offset, labelMap) (idx, (_str, len)) ->
            let label = "str_" + string idx  // Match label format in CodeGen
            let newMap = Map.add label offset labelMap
            (offset + 8 + len + 1, newMap))  // 8 for length + data + 1 for null
            (startOffset, Map.empty)
        |> snd

/// Compute the size of the string pool in bytes
/// Each string has format: [length:8][data:N][null:1]
let getStringPoolSize (stringPool: LiteralPool.StringPool) : int =
    if stringPool.Strings.IsEmpty then 0
    else
        stringPool.Strings
        |> Map.toList
        |> List.sumBy (fun (_, (str, _)) ->
            8 + System.Text.Encoding.UTF8.GetBytes(str).Length + 1)  // 8 for length + data + 1 for null

/// Compute the platform-specific code file offset for encoding
let private computeCodeFileOffset
    (os: Platform.OS)
    (stringPool: LiteralPool.StringPool)
    (floatPool: LiteralPool.FloatPool)
    (enableLeakCheck: bool)
    : int =
    match os with
    | Platform.Linux ->
        // ELF: header (64) + 1 program header (56) = 120
        64 + 56
    | Platform.MacOS ->
        // Mach-O: header (32) + load commands + padding
        // Must match 8_Binary_Generation_MachO.fs calculation
        let headerSize = 32
        let pageZeroCommandSize = 72
        let hasData =
            not stringPool.Strings.IsEmpty
            || not floatPool.Floats.IsEmpty
            || enableLeakCheck
        let numTextSections = if hasData then 2 else 1
        let textSegmentCommandSize = 72 + (80 * numTextSections)
        let linkeditSegmentCommandSize = 72
        let dylinkerCommandSize = 32
        let dylibCommandSize = 56
        let symtabCommandSize = 24
        let dysymtabCommandSize = 80
        let uuidCommandSize = 24
        let buildVersionCommandSize = 24
        let mainCommandSize = 24
        let commandsSize =
            pageZeroCommandSize + textSegmentCommandSize + linkeditSegmentCommandSize + dylinkerCommandSize + dylibCommandSize + symtabCommandSize + dysymtabCommandSize + uuidCommandSize + buildVersionCommandSize + mainCommandSize
        (headerSize + commandsSize + 200 + 7) &&& (~~~7)

/// Compute leak counter label position
/// Returns map with "_leak_count" label after all other data, 8-byte aligned
let computeLeakCounterLabel (codeFileOffset: int) (codeSize: int) (floatPoolSize: int) (stringPoolSize: int) : Map<string, int> =
    let floatStart = (codeFileOffset + codeSize + 7) &&& (~~~7)
    let stringStart = floatStart + floatPoolSize
    let leakStart = (stringStart + stringPoolSize + 7) &&& (~~~7)
    Map.ofList [("_leak_count", leakStart)]

/// Encoding with string and float pool support
/// Computes label positions and encodes ADRP/ADD_label correctly
let encodeAllWithPools
    (instructions: ARM64.Instr list)
    (stringPool: LiteralPool.StringPool)
    (floatPool: LiteralPool.FloatPool)
    (os: Platform.OS)
    (enableLeakCheck: bool)
    : ARM64.MachineCode list =
    let codeFileOffset =
        computeCodeFileOffset os stringPool floatPool enableLeakCheck
    // Step 1: Compute code size
    let codeSize =
        getCodeSize instructions

    // Step 2: Compute float label positions (after headers + code, 8-byte aligned)
    let floatLabels =
        computeFloatLabels codeFileOffset codeSize floatPool
    let floatPoolSize =
        getFloatPoolSize floatPool

    // Step 3: Compute string label positions (after headers + code + floats)
    let stringLabels =
        computeStringLabels codeFileOffset codeSize floatPoolSize stringPool

    let stringPoolSize =
        getStringPoolSize stringPool
    let leakLabels =
        if enableLeakCheck then
            computeLeakCounterLabel codeFileOffset codeSize floatPoolSize stringPoolSize
        else
            Map.empty

    // Step 4: Compute code label positions (relative to code start, add file offset)
    let rawCodeLabels =
        computeLabelPositions instructions
    let codeLabelMap =
        rawCodeLabels |> Map.map (fun _ offset -> codeFileOffset + offset)

    let dataLabels = leakLabels

    // Step 5: Encode with label resolution (current offset includes file offset)
    let rec encodeLoop instrs offset acc =
        match instrs with
        | [] -> List.rev acc
        | instr :: rest ->
            let machineCode = encodeWithLabels instr offset codeLabelMap stringLabels floatLabels dataLabels
            match machineCode with
            | [] ->
                encodeLoop rest offset acc
            | [code] ->
                encodeLoop rest (offset + 4) (code :: acc)
            | _ ->
                let (nextAcc, wordCount) =
                    machineCode
                    |> List.fold (fun (acc', count) code -> (code :: acc', count + 1)) (acc, 0)
                encodeLoop rest (offset + (wordCount * 4)) nextAcc

    encodeLoop instructions codeFileOffset []

/// Encode symbolic instructions with string and float pool support
/// Resolves label refs on the fly to avoid allocating a concrete instruction list
let encodeSymbolicWithPools
    (instructions: ARM64Symbolic.Instr list)
    (stringPool: LiteralPool.StringPool)
    (floatPool: LiteralPool.FloatPool)
    (os: Platform.OS)
    (enableLeakCheck: bool)
    : ARM64.MachineCode list =
    let codeFileOffset =
        computeCodeFileOffset os stringPool floatPool enableLeakCheck
    // Step 1: Compute code size
    let codeSize =
        getSymbolicCodeSize instructions

    // Step 2: Compute float label positions (after headers + code, 8-byte aligned)
    let floatLabels =
        computeFloatLabels codeFileOffset codeSize floatPool
    let floatPoolSize =
        getFloatPoolSize floatPool

    // Step 3: Compute string label positions (after headers + code + floats)
    let stringLabels =
        computeStringLabels codeFileOffset codeSize floatPoolSize stringPool

    let stringPoolSize =
        getStringPoolSize stringPool
    let leakLabels =
        if enableLeakCheck then
            computeLeakCounterLabel codeFileOffset codeSize floatPoolSize stringPoolSize
        else
            Map.empty

    // Step 4: Compute code label positions (relative to code start, add file offset)
    let rawCodeLabels =
        computeSymbolicLabelPositions instructions
    let codeLabelMap =
        rawCodeLabels |> Map.map (fun _ offset -> codeFileOffset + offset)

    let dataLabels = leakLabels

    // Step 5: Encode with label resolution (current offset includes file offset)
    let rec encodeLoop instrs offset acc =
        match instrs with
        | [] -> List.rev acc
        | instr :: rest ->
            let resolvedInstr = resolveSymbolicInstr stringPool floatPool instr
            let machineCode = encodeWithLabels resolvedInstr offset codeLabelMap stringLabels floatLabels dataLabels
            match machineCode with
            | [] ->
                encodeLoop rest offset acc
            | [code] ->
                encodeLoop rest (offset + 4) (code :: acc)
            | _ ->
                let (nextAcc, wordCount) =
                    machineCode
                    |> List.fold (fun (acc', count) code -> (code :: acc', count + 1)) (acc, 0)
                encodeLoop rest (offset + (wordCount * 4)) nextAcc

    encodeLoop instructions codeFileOffset []
