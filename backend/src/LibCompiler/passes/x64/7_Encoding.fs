// 7_X86_64_Encoding.fs - x86-64 Instruction Encoding (Pass 7, x86_64 variant)
//
// Encodes x86-64 instructions to variable-length machine code bytes.
//
// x86-64 encoding format (variable length, 1-15 bytes):
//   [REX prefix] [Opcode] [ModR/M] [SIB] [Displacement] [Immediate]
//
// REX prefix (0x40-0x4F): Required when using 64-bit operands or R8-R15.
//   REX.W (bit 3): 64-bit operand size
//   REX.R (bit 2): Extension of ModR/M reg field (for R8-R15)
//   REX.X (bit 1): Extension of SIB index field
//   REX.B (bit 0): Extension of ModR/M r/m field or SIB base (for R8-R15)
//
// ModR/M byte: [mod:2][reg:3][r/m:3]
//   mod=11: register direct
//   mod=00: [r/m] (register indirect)
//   mod=01: [r/m + disp8]
//   mod=10: [r/m + disp32]
//
// See Intel SDM Vol 2, Chapter 2 for complete encoding reference.

module X86_64_Encoding

open X86_64

/// Get the 3-bit register encoding and whether REX.B/REX.R extension is needed
let private regEncoding (reg: Reg) : int * bool =
    match reg with
    | RAX -> (0, false) | RCX -> (1, false) | RDX -> (2, false) | RBX -> (3, false)
    | RSP -> (4, false) | RBP -> (5, false) | RSI -> (6, false) | RDI -> (7, false)
    | R8  -> (0, true)  | R9  -> (1, true)  | R10 -> (2, true)  | R11 -> (3, true)
    | R12 -> (4, true)  | R13 -> (5, true)  | R14 -> (6, true)  | R15 -> (7, true)

/// Get the 3-bit XMM register encoding and whether REX extension is needed
let private fregEncoding (reg: FReg) : int * bool =
    match reg with
    | XMM0  -> (0, false) | XMM1  -> (1, false) | XMM2  -> (2, false) | XMM3  -> (3, false)
    | XMM4  -> (4, false) | XMM5  -> (5, false) | XMM6  -> (6, false) | XMM7  -> (7, false)
    | XMM8  -> (0, true)  | XMM9  -> (1, true)  | XMM10 -> (2, true)  | XMM11 -> (3, true)
    | XMM12 -> (4, true)  | XMM13 -> (5, true)  | XMM14 -> (6, true)  | XMM15 -> (7, true)

/// Build a REX prefix byte. Returns empty array if no REX needed.
let private rex (w: bool) (r: bool) (x: bool) (b: bool) : byte array =
    if w || r || x || b then
        let mutable v = 0x40uy
        if w then v <- v ||| 0x08uy  // REX.W: 64-bit operand
        if r then v <- v ||| 0x04uy  // REX.R: reg field extension
        if x then v <- v ||| 0x02uy  // REX.X: SIB index extension
        if b then v <- v ||| 0x01uy  // REX.B: r/m field extension
        [| v |]
    else
        [||]

/// Build a ModR/M byte
let private modRM (modBits: int) (reg: int) (rm: int) : byte =
    byte ((modBits <<< 6) ||| (reg <<< 3) ||| rm)

/// Encode a 32-bit signed immediate as little-endian bytes
let private imm32Bytes (v: int32) : byte array =
    let u = uint32 v
    [| byte (u &&& 0xFFu); byte ((u >>> 8) &&& 0xFFu); byte ((u >>> 16) &&& 0xFFu); byte ((u >>> 24) &&& 0xFFu) |]

/// Encode an 8-bit signed immediate
let private imm8Bytes (v: int) : byte array =
    [| byte (v &&& 0xFF) |]

/// Encode a 64-bit immediate as little-endian bytes
let private imm64Bytes (v: int64) : byte array =
    let u = uint64 v
    [| byte (u &&& 0xFFUL); byte ((u >>> 8) &&& 0xFFUL); byte ((u >>> 16) &&& 0xFFUL); byte ((u >>> 24) &&& 0xFFUL)
       byte ((u >>> 32) &&& 0xFFUL); byte ((u >>> 40) &&& 0xFFUL); byte ((u >>> 48) &&& 0xFFUL); byte ((u >>> 56) &&& 0xFFUL) |]

/// Check if a value fits in a signed 8-bit immediate
let private fitsInt8 (v: int32) : bool =
    v >= -128 && v <= 127

/// Check if a value fits in a signed 32-bit immediate
let private fitsInt32 (v: int64) : bool =
    v >= int64 System.Int32.MinValue && v <= int64 System.Int32.MaxValue

/// Encode register-to-register operation with REX.W and a 2-byte opcode of form [opcode] [ModR/M]
let private encodeRegReg (opcode: byte) (dest: Reg) (src: Reg) : byte array =
    let (destEnc, destExt) = regEncoding dest
    let (srcEnc, srcExt) = regEncoding src
    // ModR/M: mod=11 (register direct), reg=src, r/m=dest
    Array.concat [| rex true srcExt false destExt; [| opcode; modRM 3 srcEnc destEnc |] |]

/// Encode [base + disp32] memory operand with ModR/M (and SIB if needed)
let private encodeMemOperand (regBits: int) (regExt: bool) (baseReg: Reg) (offset: int32) : byte array =
    let (baseEnc, baseExt) = regEncoding baseReg
    // RSP/R12 as base requires SIB byte; RBP/R13 with no offset requires disp8=0
    let needsSIB = (baseEnc = 4) // RSP or R12
    let modBits =
        if offset = 0 && baseEnc <> 5 then 0  // [base] (RBP/R13 can't use mod=00)
        elif fitsInt8 offset then 1             // [base + disp8]
        else 2                                  // [base + disp32]
    let rexBytes = rex true regExt false baseExt
    if needsSIB then
        let modrmByte = modRM modBits regBits 4  // r/m=4 means SIB follows
        let sibByte = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)  // scale=1, index=RSP(none), base
        let dispBytes =
            if modBits = 0 then [||]
            elif modBits = 1 then imm8Bytes (int offset)
            else imm32Bytes offset
        Array.concat [| rexBytes; [| modrmByte; sibByte |]; dispBytes |]
    else
        let modrmByte = modRM modBits regBits baseEnc
        let dispBytes =
            if modBits = 0 then [||]
            elif modBits = 1 then imm8Bytes (int offset)
            else imm32Bytes offset
        Array.concat [| rexBytes; [| modrmByte |]; dispBytes |]

/// Encode a condition code to its 4-bit value (for Jcc, SETcc)
let private condCode (cond: Condition) : byte =
    match cond with
    | EQ -> 0x04uy  // ZF=1
    | NE -> 0x05uy  // ZF=0
    | LT -> 0x0Cuy  // SF!=OF (signed)
    | GE -> 0x0Duy  // SF=OF (signed)
    | LE -> 0x0Euy  // ZF=1 or SF!=OF (signed)
    | GT -> 0x0Fuy  // ZF=0 and SF=OF (signed)
    | B  -> 0x02uy  // CF=1 (unsigned/float below)
    | AE -> 0x03uy  // CF=0 (unsigned/float above or equal)
    | BE -> 0x06uy  // CF=1 or ZF=1 (unsigned/float below or equal)
    | A  -> 0x07uy  // CF=0 and ZF=0 (unsigned/float above)
    | P  -> 0x0Auy  // PF=1 (parity/unordered - NaN)
    | NP -> 0x0Buy  // PF=0 (no parity/ordered - not NaN)

/// Encode a single x86-64 instruction to bytes
let encodeInstruction (instr: Instr) : byte array =
    match instr with

    // --- Data movement ---

    | MOV_imm (dest, imm) ->
        // REX.W + B8+rd io (MOV r64, imm64) — "movabs"
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xB8uy + byte destEnc |]; imm64Bytes imm |]

    | MOV_imm32 (dest, imm) ->
        // REX.W + C7 /0 id (MOV r/m64, imm32) — sign-extended
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xC7uy; modRM 3 0 destEnc |]; imm32Bytes imm |]

    | MOV_reg (dest, src) ->
        // REX.W + 89 /r (MOV r/m64, r64)
        encodeRegReg 0x89uy dest src

    | MOV_reg32 (dest, src) ->
        // 89 /r (MOV r/m32, r32) — no REX.W, 32-bit write zero-extends to 64-bit
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        let needsRex = destExt || srcExt
        let rexByte = if needsRex then [| 0x40uy ||| (if srcExt then 0x04uy else 0x00uy) ||| (if destExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| rexByte; [| 0x89uy; modRM 3 srcEnc destEnc |] |]

    | MOV_load (dest, baseAddr, offset) ->
        // REX.W + 8B /r (MOV r64, r/m64)
        let (destEnc, destExt) = regEncoding dest
        let (baseEnc, baseExt) = regEncoding baseAddr
        let rexByte = rex true destExt false baseExt
        let needsSIB = (baseEnc = 4)
        let modBits =
            if offset = 0 && baseEnc <> 5 then 0
            elif fitsInt8 offset then 1
            else 2
        let modrm = modRM modBits destEnc baseEnc
        if needsSIB then
            let sib = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x8Buy; modrm; sib |]; disp |]
        else
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x8Buy; modrm |]; disp |]

    | MOV_store (baseAddr, offset, src) ->
        // REX.W + 89 /r (MOV r/m64, r64)
        let (srcEnc, srcExt) = regEncoding src
        let (baseEnc, baseExt) = regEncoding baseAddr
        let rexByte = rex true srcExt false baseExt
        let needsSIB = (baseEnc = 4)
        let modBits =
            if offset = 0 && baseEnc <> 5 then 0
            elif fitsInt8 offset then 1
            else 2
        let modrm = modRM modBits srcEnc baseEnc
        if needsSIB then
            let sib = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x89uy; modrm; sib |]; disp |]
        else
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x89uy; modrm |]; disp |]

    | LEA (dest, baseAddr, offset) ->
        // REX.W + 8D /r (LEA r64, m)
        let (destEnc, destExt) = regEncoding dest
        let (baseEnc, baseExt) = regEncoding baseAddr
        let rexByte = rex true destExt false baseExt
        let needsSIB = (baseEnc = 4)
        let modBits =
            if offset = 0 && baseEnc <> 5 then 0
            elif fitsInt8 offset then 1
            else 2
        let modrm = modRM modBits destEnc baseEnc
        if needsSIB then
            let sib = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x8Duy; modrm; sib |]; disp |]
        else
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x8Duy; modrm |]; disp |]

    | PUSH reg ->
        // 50+rd (PUSH r64) — REX.B if R8-R15
        let (enc, ext) = regEncoding reg
        if ext then [| 0x41uy; 0x50uy + byte enc |]
        else [| 0x50uy + byte enc |]

    | POP reg ->
        // 58+rd (POP r64) — REX.B if R8-R15
        let (enc, ext) = regEncoding reg
        if ext then [| 0x41uy; 0x58uy + byte enc |]
        else [| 0x58uy + byte enc |]

    // --- Arithmetic ---

    | ADD_imm (dest, imm) ->
        let (destEnc, destExt) = regEncoding dest
        if fitsInt8 imm then
            // REX.W + 83 /0 ib (ADD r/m64, imm8)
            Array.concat [| rex true false false destExt; [| 0x83uy; modRM 3 0 destEnc |]; imm8Bytes (int imm) |]
        else
            // REX.W + 81 /0 id (ADD r/m64, imm32)
            Array.concat [| rex true false false destExt; [| 0x81uy; modRM 3 0 destEnc |]; imm32Bytes imm |]

    | ADD_reg (dest, src) ->
        // REX.W + 01 /r (ADD r/m64, r64)
        encodeRegReg 0x01uy dest src

    | SUB_imm (dest, imm) ->
        let (destEnc, destExt) = regEncoding dest
        if fitsInt8 imm then
            // REX.W + 83 /5 ib (SUB r/m64, imm8)
            Array.concat [| rex true false false destExt; [| 0x83uy; modRM 3 5 destEnc |]; imm8Bytes (int imm) |]
        else
            // REX.W + 81 /5 id (SUB r/m64, imm32)
            Array.concat [| rex true false false destExt; [| 0x81uy; modRM 3 5 destEnc |]; imm32Bytes imm |]

    | SUB_reg (dest, src) ->
        // REX.W + 29 /r (SUB r/m64, r64)
        encodeRegReg 0x29uy dest src

    | IMUL_reg (dest, src) ->
        // REX.W + 0F AF /r (IMUL r64, r/m64)
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| rex true destExt false srcExt; [| 0x0Fuy; 0xAFuy; modRM 3 destEnc srcEnc |] |]

    | IMUL_imm (dest, src, imm) ->
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        if fitsInt8 imm then
            // REX.W + 6B /r ib (IMUL r64, r/m64, imm8)
            Array.concat [| rex true destExt false srcExt; [| 0x6Buy; modRM 3 destEnc srcEnc |]; imm8Bytes (int imm) |]
        else
            // REX.W + 69 /r id (IMUL r64, r/m64, imm32)
            Array.concat [| rex true destExt false srcExt; [| 0x69uy; modRM 3 destEnc srcEnc |]; imm32Bytes imm |]

    | IDIV src ->
        // REX.W + F7 /7 (IDIV r/m64)
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| rex true false false srcExt; [| 0xF7uy; modRM 3 7 srcEnc |] |]

    | DIV src ->
        // REX.W + F7 /6 (DIV r/m64)
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| rex true false false srcExt; [| 0xF7uy; modRM 3 6 srcEnc |] |]

    | NEG dest ->
        // REX.W + F7 /3 (NEG r/m64)
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xF7uy; modRM 3 3 destEnc |] |]

    | NOT dest ->
        // REX.W + F7 /2 (NOT r/m64)
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xF7uy; modRM 3 2 destEnc |] |]

    | CQO ->
        // REX.W + 99 (CQO)
        [| 0x48uy; 0x99uy |]

    | XOR_reg (dest, src) ->
        // REX.W + 31 /r (XOR r/m64, r64)
        encodeRegReg 0x31uy dest src

    // --- Comparison and conditional ---

    | CMP_imm (src, imm) ->
        let (srcEnc, srcExt) = regEncoding src
        if fitsInt8 imm then
            // REX.W + 83 /7 ib (CMP r/m64, imm8)
            Array.concat [| rex true false false srcExt; [| 0x83uy; modRM 3 7 srcEnc |]; imm8Bytes (int imm) |]
        else
            // REX.W + 81 /7 id (CMP r/m64, imm32)
            Array.concat [| rex true false false srcExt; [| 0x81uy; modRM 3 7 srcEnc |]; imm32Bytes imm |]

    | CMP_reg (src1, src2) ->
        // REX.W + 39 /r (CMP r/m64, r64)
        encodeRegReg 0x39uy src1 src2

    | TEST_reg (src1, src2) ->
        // REX.W + 85 /r (TEST r/m64, r64)
        encodeRegReg 0x85uy src1 src2

    | SETcc (cond, dest) ->
        // 0F 90+cc /0 (SETcc r/m8) — then MOVZX to clear upper bits
        let cc = condCode cond
        let (destEnc, destExt) = regEncoding dest
        // Need REX prefix if dest is SPL/BPL/SIL/DIL (enc 4-7 with no extension)
        // or if dest needs REX.B extension
        let needsRex = destExt || destEnc >= 4
        let rexByte = if needsRex then [| 0x40uy ||| (if destExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| rexByte; [| 0x0Fuy; 0x90uy + cc; modRM 3 0 destEnc |] |]

    // --- Bitwise ---

    | AND_imm (dest, imm) ->
        let (destEnc, destExt) = regEncoding dest
        if fitsInt8 imm then
            Array.concat [| rex true false false destExt; [| 0x83uy; modRM 3 4 destEnc |]; imm8Bytes (int imm) |]
        else
            Array.concat [| rex true false false destExt; [| 0x81uy; modRM 3 4 destEnc |]; imm32Bytes imm |]

    | AND_reg (dest, src) ->
        // REX.W + 21 /r (AND r/m64, r64)
        encodeRegReg 0x21uy dest src

    | OR_reg (dest, src) ->
        // REX.W + 09 /r (OR r/m64, r64)
        encodeRegReg 0x09uy dest src

    | SHL_imm (dest, shift) ->
        // REX.W + C1 /4 ib (SHL r/m64, imm8)
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xC1uy; modRM 3 4 destEnc |]; imm8Bytes shift |]

    | SHR_imm (dest, shift) ->
        // REX.W + C1 /5 ib (SHR r/m64, imm8)
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xC1uy; modRM 3 5 destEnc |]; imm8Bytes shift |]

    | SHL_cl dest ->
        // REX.W + D3 /4 (SHL r/m64, CL)
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xD3uy; modRM 3 4 destEnc |] |]

    | SHR_cl dest ->
        // REX.W + D3 /5 (SHR r/m64, CL)
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true false false destExt; [| 0xD3uy; modRM 3 5 destEnc |] |]

    // --- Byte-level memory ---

    | MOV_store_byte (baseAddr, offset, src) ->
        // 88 /r (MOV r/m8, r8)
        let (srcEnc, srcExt) = regEncoding src
        let (baseEnc, baseExt) = regEncoding baseAddr
        let needsRex = srcExt || baseExt || srcEnc >= 4  // Need REX for SPL/BPL/SIL/DIL
        let rexByte = if needsRex then [| 0x40uy ||| (if srcExt then 0x04uy else 0x00uy) ||| (if baseExt then 0x01uy else 0x00uy) |] else [||]
        let needsSIB = (baseEnc = 4)
        let modBits =
            if offset = 0 && baseEnc <> 5 then 0
            elif fitsInt8 offset then 1
            else 2
        let modrm = modRM modBits srcEnc baseEnc
        if needsSIB then
            let sib = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x88uy; modrm; sib |]; disp |]
        else
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x88uy; modrm |]; disp |]

    | MOV_load_byte (dest, baseAddr, offset) ->
        // 0F B6 /r (MOVZX r32, r/m8) — zero-extends to 64-bit
        let (destEnc, destExt) = regEncoding dest
        let (baseEnc, baseExt) = regEncoding baseAddr
        let rexByte = if destExt || baseExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if baseExt then 0x01uy else 0x00uy) |] else [||]
        let needsSIB = (baseEnc = 4)
        let modBits =
            if offset = 0 && baseEnc <> 5 then 0
            elif fitsInt8 offset then 1
            else 2
        let modrm = modRM modBits destEnc baseEnc
        if needsSIB then
            let sib = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x0Fuy; 0xB6uy; modrm; sib |]; disp |]
        else
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| rexByte; [| 0x0Fuy; 0xB6uy; modrm |]; disp |]

    // --- Sign/zero extension ---

    | MOVZX_byte (dest, src) ->
        // 0F B6 /r (MOVZX r32, r/m8) — implicitly zero-extends to 64-bit
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        let needsRex = destExt || srcExt || srcEnc >= 4
        let rexByte = if needsRex then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| rexByte; [| 0x0Fuy; 0xB6uy; modRM 3 destEnc srcEnc |] |]

    | MOVZX_word (dest, src) ->
        // 0F B7 /r (MOVZX r32, r/m16) — implicitly zero-extends to 64-bit
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        let needsRex = destExt || srcExt
        let rexByte = if needsRex then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| rexByte; [| 0x0Fuy; 0xB7uy; modRM 3 destEnc srcEnc |] |]

    | MOVSX_byte (dest, src) ->
        // REX.W + 0F BE /r (MOVSX r64, r/m8)
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| rex true destExt false srcExt; [| 0x0Fuy; 0xBEuy; modRM 3 destEnc srcEnc |] |]

    | MOVSX_word (dest, src) ->
        // REX.W + 0F BF /r (MOVSX r64, r/m16)
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| rex true destExt false srcExt; [| 0x0Fuy; 0xBFuy; modRM 3 destEnc srcEnc |] |]

    | MOVSXD (dest, src) ->
        // REX.W + 63 /r (MOVSXD r64, r/m32)
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| rex true destExt false srcExt; [| 0x63uy; modRM 3 destEnc srcEnc |] |]

    | LEA_rip (dest, _label) ->
        // REX.W + 8D /r — [RIP + disp32] — displacement will be fixed up later
        let (destEnc, destExt) = regEncoding dest
        Array.concat [| rex true destExt false false; [| 0x8Duy; modRM 0 destEnc 5 |]; imm32Bytes 0 |]

    // --- Control flow ---

    | CALL _label ->
        // E8 cd (CALL rel32) — offset will be fixed up later
        Array.concat [| [| 0xE8uy |]; imm32Bytes 0 |]

    | CALL_reg reg ->
        // FF /2 (CALL r/m64)
        let (enc, ext) = regEncoding reg
        let rexByte = if ext then [| 0x41uy |] else [||]
        Array.concat [| rexByte; [| 0xFFuy; modRM 3 2 enc |] |]

    | JMP _label ->
        // E9 cd (JMP rel32) — offset will be fixed up later
        Array.concat [| [| 0xE9uy |]; imm32Bytes 0 |]

    | JMP_reg reg ->
        // FF /4 (JMP r/m64)
        let (enc, ext) = regEncoding reg
        let rexByte = if ext then [| 0x41uy |] else [||]
        Array.concat [| rexByte; [| 0xFFuy; modRM 3 4 enc |] |]

    | Jcc (_cond, _label) ->
        // 0F 80+cc cd (Jcc rel32) — offset will be fixed up later
        let cc = condCode _cond
        Array.concat [| [| 0x0Fuy; 0x80uy + cc |]; imm32Bytes 0 |]

    | RET ->
        // C3 (RET)
        [| 0xC3uy |]

    | SYSCALL ->
        // 0F 05 (SYSCALL)
        [| 0x0Fuy; 0x05uy |]

    | Label _ ->
        // Pseudo-instruction, no bytes emitted
        [||]

    // --- Floating-point (SSE2) ---

    | MOVSD_load (dest, baseAddr, offset) ->
        // F2 0F 10 /r (MOVSD xmm, m64)
        let (destEnc, destExt) = fregEncoding dest
        let (baseEnc, baseExt) = regEncoding baseAddr
        let rexByte = if destExt || baseExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if baseExt then 0x01uy else 0x00uy) |] else [||]
        let needsSIB = (baseEnc = 4)
        let modBits =
            if offset = 0 && baseEnc <> 5 then 0
            elif fitsInt8 offset then 1
            else 2
        let modrm = modRM modBits destEnc baseEnc
        if needsSIB then
            let sib = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x10uy; modrm; sib |]; disp |]
        else
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x10uy; modrm |]; disp |]

    | MOVSD_store (baseAddr, offset, src) ->
        // F2 0F 11 /r (MOVSD m64, xmm)
        let (srcEnc, srcExt) = fregEncoding src
        let (baseEnc, baseExt) = regEncoding baseAddr
        let rexByte = if srcExt || baseExt then [| 0x40uy ||| (if srcExt then 0x04uy else 0x00uy) ||| (if baseExt then 0x01uy else 0x00uy) |] else [||]
        let needsSIB = (baseEnc = 4)
        let modBits =
            if offset = 0 && baseEnc <> 5 then 0
            elif fitsInt8 offset then 1
            else 2
        let modrm = modRM modBits srcEnc baseEnc
        if needsSIB then
            let sib = byte ((0 <<< 6) ||| (4 <<< 3) ||| baseEnc)
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x11uy; modrm; sib |]; disp |]
        else
            let disp =
                if modBits = 0 then [||]
                elif modBits = 1 then imm8Bytes (int offset)
                else imm32Bytes offset
            Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x11uy; modrm |]; disp |]

    | MOVSD_reg (dest, src) ->
        // F2 0F 10 /r (MOVSD xmm1, xmm2)
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        let rexByte = if destExt || srcExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x10uy; modRM 3 destEnc srcEnc |] |]

    | ADDSD (dest, src) ->
        // F2 0F 58 /r (ADDSD xmm1, xmm2)
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        let rexByte = if destExt || srcExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x58uy; modRM 3 destEnc srcEnc |] |]

    | SUBSD (dest, src) ->
        // F2 0F 5C /r
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        let rexByte = if destExt || srcExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x5Cuy; modRM 3 destEnc srcEnc |] |]

    | MULSD (dest, src) ->
        // F2 0F 59 /r
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        let rexByte = if destExt || srcExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x59uy; modRM 3 destEnc srcEnc |] |]

    | DIVSD (dest, src) ->
        // F2 0F 5E /r
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        let rexByte = if destExt || srcExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x5Euy; modRM 3 destEnc srcEnc |] |]

    | XORPD (dest, src) ->
        // 66 0F 57 /r
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        let rexByte = if destExt || srcExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0x66uy |]; rexByte; [| 0x0Fuy; 0x57uy; modRM 3 destEnc srcEnc |] |]

    | SQRTSD (dest, src) ->
        // F2 0F 51 /r
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        let rexByte = if destExt || srcExt then [| 0x40uy ||| (if destExt then 0x04uy else 0x00uy) ||| (if srcExt then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0xF2uy |]; rexByte; [| 0x0Fuy; 0x51uy; modRM 3 destEnc srcEnc |] |]

    | UCOMISD (src1, src2) ->
        // 66 0F 2E /r
        let (src1Enc, src1Ext) = fregEncoding src1
        let (src2Enc, src2Ext) = fregEncoding src2
        let rexByte = if src1Ext || src2Ext then [| 0x40uy ||| (if src1Ext then 0x04uy else 0x00uy) ||| (if src2Ext then 0x01uy else 0x00uy) |] else [||]
        Array.concat [| [| 0x66uy |]; rexByte; [| 0x0Fuy; 0x2Euy; modRM 3 src1Enc src2Enc |] |]

    | CVTSI2SD (dest, src) ->
        // F2 REX.W 0F 2A /r (CVTSI2SD xmm, r64)
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| [| 0xF2uy |]; rex true destExt false srcExt; [| 0x0Fuy; 0x2Auy; modRM 3 destEnc srcEnc |] |]

    | CVTTSD2SI (dest, src) ->
        // F2 REX.W 0F 2C /r (CVTTSD2SI r64, xmm)
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        Array.concat [| [| 0xF2uy |]; rex true destExt false srcExt; [| 0x0Fuy; 0x2Cuy; modRM 3 destEnc srcEnc |] |]

    | MOVQ_to_gp (dest, src) ->
        // 66 REX.W 0F 7E /r (MOVQ r64, xmm)
        let (destEnc, destExt) = regEncoding dest
        let (srcEnc, srcExt) = fregEncoding src
        Array.concat [| [| 0x66uy |]; rex true srcExt false destExt; [| 0x0Fuy; 0x7Euy; modRM 3 srcEnc destEnc |] |]

    | MOVQ_from_gp (dest, src) ->
        // 66 REX.W 0F 6E /r (MOVQ xmm, r64)
        let (destEnc, destExt) = fregEncoding dest
        let (srcEnc, srcExt) = regEncoding src
        Array.concat [| [| 0x66uy |]; rex true destExt false srcExt; [| 0x0Fuy; 0x6Euy; modRM 3 destEnc srcEnc |] |]
