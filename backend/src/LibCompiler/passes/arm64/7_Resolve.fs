// 7_ARM64_Resolve.fs - ARM64 Symbolic Resolution
//
// Resolves symbolic label references into concrete ARM64 instructions and
// builds literal pools for string/float constants.

module ARM64_Resolve

type ResolveResult = {
    Instructions: ARM64.Instr list
    StringPool: LiteralPool.StringPool
    FloatPool: LiteralPool.FloatPool
}

type private PoolState = {
    StringPool: LiteralPool.StringPool
    FloatPool: LiteralPool.FloatPool
}

let private resolveLabelRef
    (state: PoolState)
    (labelRef: ARM64Symbolic.LabelRef)
    : string * PoolState =
    match labelRef with
    | ARM64Symbolic.CodeLabel name -> (name, state)
    | ARM64Symbolic.DataLabel dataRef ->
        match dataRef with
        | ARM64Symbolic.Named name -> (name, state)
        | ARM64Symbolic.StringLiteral value ->
            let (idx, pool') = LiteralPool.addString state.StringPool value
            ("str_" + string idx, { state with StringPool = pool' })
        | ARM64Symbolic.FloatLiteral value ->
            let (idx, pool') = LiteralPool.addFloat state.FloatPool value
            ("_float" + string idx, { state with FloatPool = pool' })

let private addLabelRefToPools
    (state: PoolState)
    (labelRef: ARM64Symbolic.LabelRef)
    : PoolState =
    match labelRef with
    | ARM64Symbolic.CodeLabel _ -> state
    | ARM64Symbolic.DataLabel dataRef ->
        match dataRef with
        | ARM64Symbolic.Named _ -> state
        | ARM64Symbolic.StringLiteral value ->
            let (_, pool') = LiteralPool.addString state.StringPool value
            { state with StringPool = pool' }
        | ARM64Symbolic.FloatLiteral value ->
            let (_, pool') = LiteralPool.addFloat state.FloatPool value
            { state with FloatPool = pool' }

let collectPools
    (instructions: ARM64Symbolic.Instr list)
    : LiteralPool.StringPool * LiteralPool.FloatPool =
    let initialState = { StringPool = LiteralPool.emptyStringPool; FloatPool = LiteralPool.emptyFloatPool }

    let updatePools (state: PoolState) (instr: ARM64Symbolic.Instr) : PoolState =
        match instr with
        | ARM64Symbolic.ADRP (_, labelRef)
        | ARM64Symbolic.ADD_label (_, _, labelRef)
        | ARM64Symbolic.ADR (_, labelRef) ->
            addLabelRefToPools state labelRef
        | _ -> state

    let rec loop state remaining =
        match remaining with
        | [] -> state
        | instr :: rest ->
            let nextState = updatePools state instr
            loop nextState rest
    let pools = loop initialState instructions
    (pools.StringPool, pools.FloatPool)

let private resolveInstr
    (state: PoolState)
    (instr: ARM64Symbolic.Instr)
    : ARM64.Instr * PoolState =
    match instr with
    | ARM64Symbolic.MOVZ (dest, imm, shift) -> (ARM64.MOVZ (dest, imm, shift), state)
    | ARM64Symbolic.MOVN (dest, imm, shift) -> (ARM64.MOVN (dest, imm, shift), state)
    | ARM64Symbolic.MOVK (dest, imm, shift) -> (ARM64.MOVK (dest, imm, shift), state)
    | ARM64Symbolic.ADD_imm (dest, src, imm) -> (ARM64.ADD_imm (dest, src, imm), state)
    | ARM64Symbolic.ADD_reg (dest, src1, src2) -> (ARM64.ADD_reg (dest, src1, src2), state)
    | ARM64Symbolic.ADD_shifted (dest, src1, src2, shift) -> (ARM64.ADD_shifted (dest, src1, src2, shift), state)
    | ARM64Symbolic.SUB_imm (dest, src, imm) -> (ARM64.SUB_imm (dest, src, imm), state)
    | ARM64Symbolic.SUB_imm12 (dest, src, imm) -> (ARM64.SUB_imm12 (dest, src, imm), state)
    | ARM64Symbolic.SUB_reg (dest, src1, src2) -> (ARM64.SUB_reg (dest, src1, src2), state)
    | ARM64Symbolic.SUB_shifted (dest, src1, src2, shift) -> (ARM64.SUB_shifted (dest, src1, src2, shift), state)
    | ARM64Symbolic.SUBS_imm (dest, src, imm) -> (ARM64.SUBS_imm (dest, src, imm), state)
    | ARM64Symbolic.MUL (dest, src1, src2) -> (ARM64.MUL (dest, src1, src2), state)
    | ARM64Symbolic.SDIV (dest, src1, src2) -> (ARM64.SDIV (dest, src1, src2), state)
    | ARM64Symbolic.UDIV (dest, src1, src2) -> (ARM64.UDIV (dest, src1, src2), state)
    | ARM64Symbolic.MSUB (dest, src1, src2, src3) -> (ARM64.MSUB (dest, src1, src2, src3), state)
    | ARM64Symbolic.MADD (dest, src1, src2, src3) -> (ARM64.MADD (dest, src1, src2, src3), state)
    | ARM64Symbolic.CMP_imm (src, imm) -> (ARM64.CMP_imm (src, imm), state)
    | ARM64Symbolic.CMP_reg (src1, src2) -> (ARM64.CMP_reg (src1, src2), state)
    | ARM64Symbolic.CSET (dest, cond) -> (ARM64.CSET (dest, cond), state)
    | ARM64Symbolic.AND_reg (dest, src1, src2) -> (ARM64.AND_reg (dest, src1, src2), state)
    | ARM64Symbolic.AND_imm (dest, src, imm) -> (ARM64.AND_imm (dest, src, imm), state)
    | ARM64Symbolic.ORR_reg (dest, src1, src2) -> (ARM64.ORR_reg (dest, src1, src2), state)
    | ARM64Symbolic.EOR_reg (dest, src1, src2) -> (ARM64.EOR_reg (dest, src1, src2), state)
    | ARM64Symbolic.LSL_reg (dest, src, shift) -> (ARM64.LSL_reg (dest, src, shift), state)
    | ARM64Symbolic.LSR_reg (dest, src, shift) -> (ARM64.LSR_reg (dest, src, shift), state)
    | ARM64Symbolic.LSL_imm (dest, src, shift) -> (ARM64.LSL_imm (dest, src, shift), state)
    | ARM64Symbolic.LSR_imm (dest, src, shift) -> (ARM64.LSR_imm (dest, src, shift), state)
    | ARM64Symbolic.MVN (dest, src) -> (ARM64.MVN (dest, src), state)
    | ARM64Symbolic.MOV_reg (dest, src) -> (ARM64.MOV_reg (dest, src), state)
    | ARM64Symbolic.STRB (src, addr, offset) -> (ARM64.STRB (src, addr, offset), state)
    | ARM64Symbolic.LDRB (dest, baseAddr, index) -> (ARM64.LDRB (dest, baseAddr, index), state)
    | ARM64Symbolic.LDRB_imm (dest, baseAddr, offset) -> (ARM64.LDRB_imm (dest, baseAddr, offset), state)
    | ARM64Symbolic.STRB_reg (src, addr) -> (ARM64.STRB_reg (src, addr), state)
    | ARM64Symbolic.STP (reg1, reg2, addr, offset) -> (ARM64.STP (reg1, reg2, addr, offset), state)
    | ARM64Symbolic.STP_pre (reg1, reg2, addr, offset) -> (ARM64.STP_pre (reg1, reg2, addr, offset), state)
    | ARM64Symbolic.LDP (reg1, reg2, addr, offset) -> (ARM64.LDP (reg1, reg2, addr, offset), state)
    | ARM64Symbolic.LDP_post (reg1, reg2, addr, offset) -> (ARM64.LDP_post (reg1, reg2, addr, offset), state)
    | ARM64Symbolic.STR (src, addr, offset) -> (ARM64.STR (src, addr, offset), state)
    | ARM64Symbolic.LDR (dest, addr, offset) -> (ARM64.LDR (dest, addr, offset), state)
    | ARM64Symbolic.STUR (src, addr, offset) -> (ARM64.STUR (src, addr, offset), state)
    | ARM64Symbolic.LDUR (dest, addr, offset) -> (ARM64.LDUR (dest, addr, offset), state)
    | ARM64Symbolic.BL label -> (ARM64.BL label, state)
    | ARM64Symbolic.BLR reg -> (ARM64.BLR reg, state)
    | ARM64Symbolic.BR reg -> (ARM64.BR reg, state)
    | ARM64Symbolic.CBZ (reg, label) -> (ARM64.CBZ (reg, label), state)
    | ARM64Symbolic.CBNZ (reg, label) -> (ARM64.CBNZ (reg, label), state)
    | ARM64Symbolic.B_label label -> (ARM64.B_label label, state)
    | ARM64Symbolic.B_cond_label (cond, label) -> (ARM64.B_cond_label (cond, label), state)
    | ARM64Symbolic.CBZ_offset (reg, offset) -> (ARM64.CBZ_offset (reg, offset), state)
    | ARM64Symbolic.CBNZ_offset (reg, offset) -> (ARM64.CBNZ_offset (reg, offset), state)
    | ARM64Symbolic.TBZ (reg, bit, offset) -> (ARM64.TBZ (reg, bit, offset), state)
    | ARM64Symbolic.TBNZ (reg, bit, offset) -> (ARM64.TBNZ (reg, bit, offset), state)
    | ARM64Symbolic.TBZ_label (reg, bit, label) -> (ARM64.TBZ_label (reg, bit, label), state)
    | ARM64Symbolic.TBNZ_label (reg, bit, label) -> (ARM64.TBNZ_label (reg, bit, label), state)
    | ARM64Symbolic.B offset -> (ARM64.B offset, state)
    | ARM64Symbolic.B_cond (cond, offset) -> (ARM64.B_cond (cond, offset), state)
    | ARM64Symbolic.NEG (dest, src) -> (ARM64.NEG (dest, src), state)
    | ARM64Symbolic.RET -> (ARM64.RET, state)
    | ARM64Symbolic.SVC imm -> (ARM64.SVC imm, state)
    | ARM64Symbolic.Label name -> (ARM64.Label name, state)
    | ARM64Symbolic.ADRP (dest, labelRef) ->
        let (label, state') = resolveLabelRef state labelRef
        (ARM64.ADRP (dest, label), state')
    | ARM64Symbolic.ADD_label (dest, src, labelRef) ->
        let (label, state') = resolveLabelRef state labelRef
        (ARM64.ADD_label (dest, src, label), state')
    | ARM64Symbolic.ADR (dest, labelRef) ->
        let (label, state') = resolveLabelRef state labelRef
        (ARM64.ADR (dest, label), state')
    | ARM64Symbolic.LDR_fp (dest, addr, offset) -> (ARM64.LDR_fp (dest, addr, offset), state)
    | ARM64Symbolic.STR_fp (src, addr, offset) -> (ARM64.STR_fp (src, addr, offset), state)
    | ARM64Symbolic.STP_fp (freg1, freg2, addr, offset) -> (ARM64.STP_fp (freg1, freg2, addr, offset), state)
    | ARM64Symbolic.LDP_fp (freg1, freg2, addr, offset) -> (ARM64.LDP_fp (freg1, freg2, addr, offset), state)
    | ARM64Symbolic.FADD (dest, src1, src2) -> (ARM64.FADD (dest, src1, src2), state)
    | ARM64Symbolic.FSUB (dest, src1, src2) -> (ARM64.FSUB (dest, src1, src2), state)
    | ARM64Symbolic.FMUL (dest, src1, src2) -> (ARM64.FMUL (dest, src1, src2), state)
    | ARM64Symbolic.FDIV (dest, src1, src2) -> (ARM64.FDIV (dest, src1, src2), state)
    | ARM64Symbolic.FNEG (dest, src) -> (ARM64.FNEG (dest, src), state)
    | ARM64Symbolic.FABS (dest, src) -> (ARM64.FABS (dest, src), state)
    | ARM64Symbolic.FSQRT (dest, src) -> (ARM64.FSQRT (dest, src), state)
    | ARM64Symbolic.FCMP (src1, src2) -> (ARM64.FCMP (src1, src2), state)
    | ARM64Symbolic.FMOV_reg (dest, src) -> (ARM64.FMOV_reg (dest, src), state)
    | ARM64Symbolic.FMOV_to_gp (dest, src) -> (ARM64.FMOV_to_gp (dest, src), state)
    | ARM64Symbolic.FMOV_from_gp (dest, src) -> (ARM64.FMOV_from_gp (dest, src), state)
    | ARM64Symbolic.SCVTF (dest, src) -> (ARM64.SCVTF (dest, src), state)
    | ARM64Symbolic.FCVTZS (dest, src) -> (ARM64.FCVTZS (dest, src), state)
    | ARM64Symbolic.SXTB (dest, src) -> (ARM64.SXTB (dest, src), state)
    | ARM64Symbolic.SXTH (dest, src) -> (ARM64.SXTH (dest, src), state)
    | ARM64Symbolic.SXTW (dest, src) -> (ARM64.SXTW (dest, src), state)
    | ARM64Symbolic.UXTB (dest, src) -> (ARM64.UXTB (dest, src), state)
    | ARM64Symbolic.UXTH (dest, src) -> (ARM64.UXTH (dest, src), state)
    | ARM64Symbolic.UXTW (dest, src) -> (ARM64.UXTW (dest, src), state)

let private resolveInstrs
    (instrs: ARM64Symbolic.Instr list)
    : ARM64.Instr list * PoolState =
    let initialState = { StringPool = LiteralPool.emptyStringPool; FloatPool = LiteralPool.emptyFloatPool }
    let rec loop acc state remaining =
        match remaining with
        | [] -> (List.rev acc, state)
        | instr :: rest ->
            let (resolved, nextState) = resolveInstr state instr
            loop (resolved :: acc) nextState rest
    loop [] initialState instrs

/// Resolve symbolic instructions into concrete ARM64 instructions and literal pools.
let resolve
    (instructions: ARM64Symbolic.Instr list)
    : ResolveResult =
    let (resolvedInstrs, pools) = resolveInstrs instructions
    {
        Instructions = resolvedInstrs
        StringPool = pools.StringPool
        FloatPool = pools.FloatPool
    }
