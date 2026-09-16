// ARM64Symbolic.fs - Symbolic ARM64 Instruction Types
//
// Defines ARM64 instructions with explicit data label references so that
// string/float literals can stay symbolic until final emission.

module ARM64Symbolic

/// Reuse ARM64 register and condition types
type Reg = ARM64.Reg
type FReg = ARM64.FReg
type Condition = ARM64.Condition

// Re-export register values for convenience in codegen
let X0: Reg = ARM64.X0
let X1: Reg = ARM64.X1
let X2: Reg = ARM64.X2
let X3: Reg = ARM64.X3
let X4: Reg = ARM64.X4
let X5: Reg = ARM64.X5
let X6: Reg = ARM64.X6
let X7: Reg = ARM64.X7
let X8: Reg = ARM64.X8
let X9: Reg = ARM64.X9
let X10: Reg = ARM64.X10
let X11: Reg = ARM64.X11
let X12: Reg = ARM64.X12
let X13: Reg = ARM64.X13
let X14: Reg = ARM64.X14
let X15: Reg = ARM64.X15
let X16: Reg = ARM64.X16
let X17: Reg = ARM64.X17
let X19: Reg = ARM64.X19
let X20: Reg = ARM64.X20
let X21: Reg = ARM64.X21
let X22: Reg = ARM64.X22
let X23: Reg = ARM64.X23
let X24: Reg = ARM64.X24
let X25: Reg = ARM64.X25
let X26: Reg = ARM64.X26
let X27: Reg = ARM64.X27
let X28: Reg = ARM64.X28
let X29: Reg = ARM64.X29
let X30: Reg = ARM64.X30
let SP: Reg = ARM64.SP

let D0: FReg = ARM64.D0
let D1: FReg = ARM64.D1
let D2: FReg = ARM64.D2
let D3: FReg = ARM64.D3
let D4: FReg = ARM64.D4
let D5: FReg = ARM64.D5
let D6: FReg = ARM64.D6
let D7: FReg = ARM64.D7
let D8: FReg = ARM64.D8
let D9: FReg = ARM64.D9
let D10: FReg = ARM64.D10
let D11: FReg = ARM64.D11
let D12: FReg = ARM64.D12
let D13: FReg = ARM64.D13
let D14: FReg = ARM64.D14
let D15: FReg = ARM64.D15
let D16: FReg = ARM64.D16
let D17: FReg = ARM64.D17
let D18: FReg = ARM64.D18
let D19: FReg = ARM64.D19
let D20: FReg = ARM64.D20
let D21: FReg = ARM64.D21
let D22: FReg = ARM64.D22
let D23: FReg = ARM64.D23
let D24: FReg = ARM64.D24
let D25: FReg = ARM64.D25
let D26: FReg = ARM64.D26
let D27: FReg = ARM64.D27
let D28: FReg = ARM64.D28
let D29: FReg = ARM64.D29
let D30: FReg = ARM64.D30
let D31: FReg = ARM64.D31

let EQ: Condition = ARM64.EQ
let NE: Condition = ARM64.NE
let LT: Condition = ARM64.LT
let GT: Condition = ARM64.GT
let LE: Condition = ARM64.LE
let GE: Condition = ARM64.GE

/// Data references for late pool resolution
type DataRef =
    | StringLiteral of string
    | FloatLiteral of float
    | Named of string

/// Label reference (code vs data)
type LabelRef =
    | CodeLabel of string
    | DataLabel of DataRef

/// ARM64 instruction types (symbolic label refs for data)
type Instr =
    | MOVZ of dest:Reg * imm:uint16 * shift:int
    | MOVN of dest:Reg * imm:uint16 * shift:int
    | MOVK of dest:Reg * imm:uint16 * shift:int
    | ADD_imm of dest:Reg * src:Reg * imm:uint16
    | ADD_reg of dest:Reg * src1:Reg * src2:Reg
    | ADD_shifted of dest:Reg * src1:Reg * src2:Reg * shift:int
    | SUB_imm of dest:Reg * src:Reg * imm:uint16
    | SUB_imm12 of dest:Reg * src:Reg * imm:uint16
    | SUB_reg of dest:Reg * src1:Reg * src2:Reg
    | SUB_shifted of dest:Reg * src1:Reg * src2:Reg * shift:int
    | SUBS_imm of dest:Reg * src:Reg * imm:uint16
    | MUL of dest:Reg * src1:Reg * src2:Reg
    | SDIV of dest:Reg * src1:Reg * src2:Reg
    | UDIV of dest:Reg * src1:Reg * src2:Reg
    | MSUB of dest:Reg * src1:Reg * src2:Reg * src3:Reg
    | MADD of dest:Reg * src1:Reg * src2:Reg * src3:Reg
    | CMP_imm of src:Reg * imm:uint16
    | CMP_reg of src1:Reg * src2:Reg
    | CSET of dest:Reg * cond:Condition
    | AND_reg of dest:Reg * src1:Reg * src2:Reg
    | AND_imm of dest:Reg * src:Reg * imm:uint64
    | ORR_reg of dest:Reg * src1:Reg * src2:Reg
    | EOR_reg of dest:Reg * src1:Reg * src2:Reg
    | LSL_reg of dest:Reg * src:Reg * shift:Reg
    | LSR_reg of dest:Reg * src:Reg * shift:Reg
    | LSL_imm of dest:Reg * src:Reg * shift:int
    | LSR_imm of dest:Reg * src:Reg * shift:int
    | MVN of dest:Reg * src:Reg
    | MOV_reg of dest:Reg * src:Reg
    | STRB of src:Reg * addr:Reg * offset:int
    | LDRB of dest:Reg * baseAddr:Reg * index:Reg
    | LDRB_imm of dest:Reg * baseAddr:Reg * offset:int
    | STRB_reg of src:Reg * addr:Reg
    | STP of reg1:Reg * reg2:Reg * addr:Reg * offset:int16
    | STP_pre of reg1:Reg * reg2:Reg * addr:Reg * offset:int16
    | LDP of reg1:Reg * reg2:Reg * addr:Reg * offset:int16
    | LDP_post of reg1:Reg * reg2:Reg * addr:Reg * offset:int16
    | STR of src:Reg * addr:Reg * offset:int16
    | LDR of dest:Reg * addr:Reg * offset:int16
    | STUR of src:Reg * addr:Reg * offset:int16
    | LDUR of dest:Reg * addr:Reg * offset:int16
    | BL of label:string
    | BLR of reg:Reg
    | BR of reg:Reg
    | CBZ of reg:Reg * label:string
    | CBNZ of reg:Reg * label:string
    | B_label of label:string
    | B_cond_label of cond:Condition * label:string
    | CBZ_offset of reg:Reg * offset:int
    | CBNZ_offset of reg:Reg * offset:int
    | TBZ of reg:Reg * bit:int * offset:int
    | TBNZ of reg:Reg * bit:int * offset:int
    | TBZ_label of reg:Reg * bit:int * label:string
    | TBNZ_label of reg:Reg * bit:int * label:string
    | B of offset:int
    | B_cond of cond:Condition * offset:int
    | NEG of dest:Reg * src:Reg
    | RET
    | SVC of imm:uint16
    | Label of string
    | ADRP of dest:Reg * label:LabelRef
    | ADD_label of dest:Reg * src:Reg * label:LabelRef
    | ADR of dest:Reg * label:LabelRef
    | LDR_fp of dest:FReg * addr:Reg * offset:int16
    | STR_fp of src:FReg * addr:Reg * offset:int16
    | STP_fp of freg1:FReg * freg2:FReg * addr:Reg * offset:int16
    | LDP_fp of freg1:FReg * freg2:FReg * addr:Reg * offset:int16
    | FADD of dest:FReg * src1:FReg * src2:FReg
    | FSUB of dest:FReg * src1:FReg * src2:FReg
    | FMUL of dest:FReg * src1:FReg * src2:FReg
    | FDIV of dest:FReg * src1:FReg * src2:FReg
    | FNEG of dest:FReg * src:FReg
    | FABS of dest:FReg * src:FReg
    | FSQRT of dest:FReg * src:FReg
    | FCMP of src1:FReg * src2:FReg
    | FMOV_reg of dest:FReg * src:FReg
    | FMOV_to_gp of dest:Reg * src:FReg
    | FMOV_from_gp of dest:FReg * src:Reg
    | SCVTF of dest:FReg * src:Reg
    | FCVTZS of dest:Reg * src:FReg
    | SXTB of dest:Reg * src:Reg
    | SXTH of dest:Reg * src:Reg
    | SXTW of dest:Reg * src:Reg
    | UXTB of dest:Reg * src:Reg
    | UXTH of dest:Reg * src:Reg
    | UXTW of dest:Reg * src:Reg

/// Classify labels from concrete ARM64 instructions
let private classifyLabel (name: string) : LabelRef =
    match name with
    | "_coverage_data" -> DataLabel (Named name)
    | "_leak_count" -> DataLabel (Named name)
    | _ -> CodeLabel name

/// Convert concrete ARM64 instruction to symbolic (for runtime helpers)
let ofARM64 (instr: ARM64.Instr) : Instr =
    match instr with
    | ARM64.MOVZ (dest, imm, shift) -> MOVZ (dest, imm, shift)
    | ARM64.MOVN (dest, imm, shift) -> MOVN (dest, imm, shift)
    | ARM64.MOVK (dest, imm, shift) -> MOVK (dest, imm, shift)
    | ARM64.ADD_imm (dest, src, imm) -> ADD_imm (dest, src, imm)
    | ARM64.ADD_reg (dest, src1, src2) -> ADD_reg (dest, src1, src2)
    | ARM64.ADD_shifted (dest, src1, src2, shift) -> ADD_shifted (dest, src1, src2, shift)
    | ARM64.SUB_imm (dest, src, imm) -> SUB_imm (dest, src, imm)
    | ARM64.SUB_imm12 (dest, src, imm) -> SUB_imm12 (dest, src, imm)
    | ARM64.SUB_reg (dest, src1, src2) -> SUB_reg (dest, src1, src2)
    | ARM64.SUB_shifted (dest, src1, src2, shift) -> SUB_shifted (dest, src1, src2, shift)
    | ARM64.SUBS_imm (dest, src, imm) -> SUBS_imm (dest, src, imm)
    | ARM64.MUL (dest, src1, src2) -> MUL (dest, src1, src2)
    | ARM64.SDIV (dest, src1, src2) -> SDIV (dest, src1, src2)
    | ARM64.UDIV (dest, src1, src2) -> UDIV (dest, src1, src2)
    | ARM64.MSUB (dest, src1, src2, src3) -> MSUB (dest, src1, src2, src3)
    | ARM64.MADD (dest, src1, src2, src3) -> MADD (dest, src1, src2, src3)
    | ARM64.CMP_imm (src, imm) -> CMP_imm (src, imm)
    | ARM64.CMP_reg (src1, src2) -> CMP_reg (src1, src2)
    | ARM64.CSET (dest, cond) -> CSET (dest, cond)
    | ARM64.AND_reg (dest, src1, src2) -> AND_reg (dest, src1, src2)
    | ARM64.AND_imm (dest, src, imm) -> AND_imm (dest, src, imm)
    | ARM64.ORR_reg (dest, src1, src2) -> ORR_reg (dest, src1, src2)
    | ARM64.EOR_reg (dest, src1, src2) -> EOR_reg (dest, src1, src2)
    | ARM64.LSL_reg (dest, src, shift) -> LSL_reg (dest, src, shift)
    | ARM64.LSR_reg (dest, src, shift) -> LSR_reg (dest, src, shift)
    | ARM64.LSL_imm (dest, src, shift) -> LSL_imm (dest, src, shift)
    | ARM64.LSR_imm (dest, src, shift) -> LSR_imm (dest, src, shift)
    | ARM64.MVN (dest, src) -> MVN (dest, src)
    | ARM64.MOV_reg (dest, src) -> MOV_reg (dest, src)
    | ARM64.STRB (src, addr, offset) -> STRB (src, addr, offset)
    | ARM64.LDRB (dest, baseAddr, index) -> LDRB (dest, baseAddr, index)
    | ARM64.LDRB_imm (dest, baseAddr, offset) -> LDRB_imm (dest, baseAddr, offset)
    | ARM64.STRB_reg (src, addr) -> STRB_reg (src, addr)
    | ARM64.STP (reg1, reg2, addr, offset) -> STP (reg1, reg2, addr, offset)
    | ARM64.STP_pre (reg1, reg2, addr, offset) -> STP_pre (reg1, reg2, addr, offset)
    | ARM64.LDP (reg1, reg2, addr, offset) -> LDP (reg1, reg2, addr, offset)
    | ARM64.LDP_post (reg1, reg2, addr, offset) -> LDP_post (reg1, reg2, addr, offset)
    | ARM64.STR (src, addr, offset) -> STR (src, addr, offset)
    | ARM64.LDR (dest, addr, offset) -> LDR (dest, addr, offset)
    | ARM64.STUR (src, addr, offset) -> STUR (src, addr, offset)
    | ARM64.LDUR (dest, addr, offset) -> LDUR (dest, addr, offset)
    | ARM64.BL label -> BL label
    | ARM64.BLR reg -> BLR reg
    | ARM64.BR reg -> BR reg
    | ARM64.CBZ (reg, label) -> CBZ (reg, label)
    | ARM64.CBNZ (reg, label) -> CBNZ (reg, label)
    | ARM64.B_label label -> B_label label
    | ARM64.B_cond_label (cond, label) -> B_cond_label (cond, label)
    | ARM64.CBZ_offset (reg, offset) -> CBZ_offset (reg, offset)
    | ARM64.CBNZ_offset (reg, offset) -> CBNZ_offset (reg, offset)
    | ARM64.TBZ (reg, bit, offset) -> TBZ (reg, bit, offset)
    | ARM64.TBNZ (reg, bit, offset) -> TBNZ (reg, bit, offset)
    | ARM64.TBZ_label (reg, bit, label) -> TBZ_label (reg, bit, label)
    | ARM64.TBNZ_label (reg, bit, label) -> TBNZ_label (reg, bit, label)
    | ARM64.B offset -> B offset
    | ARM64.B_cond (cond, offset) -> B_cond (cond, offset)
    | ARM64.NEG (dest, src) -> NEG (dest, src)
    | ARM64.RET -> RET
    | ARM64.SVC imm -> SVC imm
    | ARM64.Label name -> Label name
    | ARM64.ADRP (dest, label) -> ADRP (dest, classifyLabel label)
    | ARM64.ADD_label (dest, src, label) -> ADD_label (dest, src, classifyLabel label)
    | ARM64.ADR (dest, label) -> ADR (dest, classifyLabel label)
    | ARM64.LDR_fp (dest, addr, offset) -> LDR_fp (dest, addr, offset)
    | ARM64.STR_fp (src, addr, offset) -> STR_fp (src, addr, offset)
    | ARM64.STP_fp (freg1, freg2, addr, offset) -> STP_fp (freg1, freg2, addr, offset)
    | ARM64.LDP_fp (freg1, freg2, addr, offset) -> LDP_fp (freg1, freg2, addr, offset)
    | ARM64.FADD (dest, src1, src2) -> FADD (dest, src1, src2)
    | ARM64.FSUB (dest, src1, src2) -> FSUB (dest, src1, src2)
    | ARM64.FMUL (dest, src1, src2) -> FMUL (dest, src1, src2)
    | ARM64.FDIV (dest, src1, src2) -> FDIV (dest, src1, src2)
    | ARM64.FNEG (dest, src) -> FNEG (dest, src)
    | ARM64.FABS (dest, src) -> FABS (dest, src)
    | ARM64.FSQRT (dest, src) -> FSQRT (dest, src)
    | ARM64.FCMP (src1, src2) -> FCMP (src1, src2)
    | ARM64.FMOV_reg (dest, src) -> FMOV_reg (dest, src)
    | ARM64.FMOV_to_gp (dest, src) -> FMOV_to_gp (dest, src)
    | ARM64.FMOV_from_gp (dest, src) -> FMOV_from_gp (dest, src)
    | ARM64.SCVTF (dest, src) -> SCVTF (dest, src)
    | ARM64.FCVTZS (dest, src) -> FCVTZS (dest, src)
    | ARM64.SXTB (dest, src) -> SXTB (dest, src)
    | ARM64.SXTH (dest, src) -> SXTH (dest, src)
    | ARM64.SXTW (dest, src) -> SXTW (dest, src)
    | ARM64.UXTB (dest, src) -> UXTB (dest, src)
    | ARM64.UXTH (dest, src) -> UXTH (dest, src)
    | ARM64.UXTW (dest, src) -> UXTW (dest, src)

/// Convert concrete ARM64 instruction list to symbolic
let ofARM64List (instrs: ARM64.Instr list) : Instr list =
    instrs |> List.map ofARM64
