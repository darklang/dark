// 5_RegisterAllocation.fs - Register Allocation with Liveness Analysis (Pass 5)
//
// Allocates physical ARM64 registers to virtual registers in LIR CFG.
//
// Algorithm:
// 1. Compute liveness information using backward dataflow analysis
// 2. Build interference graph from liveness data
// 3. Use chordal graph coloring (SSA guarantees chordal interference graphs)
//    - Maximum Cardinality Search for Perfect Elimination Ordering (PEO)
//    - Greedy coloring in reverse PEO order (optimal for chordal graphs)
//    - Phi coalescing preferences to reduce register moves
// 4. Spill to stack when register pressure exceeds available registers
//
// General-Purpose Registers:
// - X0: reserved for return values
// - X1-X8: caller-saved (preferred for allocation)
// - X9-X10: excluded (used as compiler scratch registers)
// - X11-X13: reserved as scratch registers for spill code
// - X19-X26: callee-saved (used when caller-saved exhausted)
// - X27: reserved for free list base pointer
// - X28: reserved for heap bump pointer
// - X29: frame pointer
// - X30: link register
//
// Float Registers:
// - D0: float return value
// - D0-D7: caller-saved (saved around calls when live)
// - D8-D15: callee-saved
//
// Callee-saved registers (X19-X26, D8-D15) are saved/restored in prologue/epilogue.
//
// See docs/features/register-allocation.md for detailed documentation.

module RegisterAllocation

// ============================================================================
// Types
// ============================================================================

/// Live interval for a virtual register
type LiveInterval = {
    VRegId: int
    Start: int
    End: int
}

/// Bitset of VRegDomain indices
type BitSet = Bitset.Bitset

/// Dense domain for VReg IDs used by bitsets
type VRegDomain = {
    Ids: int array
    IndexOf: int array
    IndexOffset: int
    WordCount: int
}

/// Dense domain for basic block labels
type BlockIndex = {
    Labels: LIR.Label array
    EntryIndex: int
}

/// Result of register allocation
type AllocationResult = {
    Domain: VRegDomain
    Allocations: Allocation option array
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
}

/// Allocation target for a virtual register
and Allocation =
    | PhysReg of LIR.PhysReg
    | StackSlot of int

/// Liveness information for a basic block
type BlockLiveness = {
    LiveIn: BitSet
    LiveOut: BitSet
}

/// Timing information for register allocation phases
type RegisterAllocationTiming = {
    Phase: string
    ElapsedMs: float
}

type private InterferenceGraphTiming = {
    LiveIterationMs: float
    AdjacencyUpdatesMs: float
}

type private McsTiming = {
    SelectMs: float
    UpdateMs: float
}

type private ChordalColoringTiming = {
    CoalesceMs: float
    McsSelectMs: float
    McsUpdateMs: float
    GreedyMs: float
    ExpandMs: float
}

// ============================================================================
// Bitset Utilities (used to speed up register allocation)
// ============================================================================

let private buildVRegDomain (ids: int list) : VRegDomain =
    let sorted = List.sort ids
    let rec unique (last: int option) (remaining: int list) (acc: int list) : int list =
        match remaining with
        | [] -> List.rev acc
        | head :: tail ->
            match last with
            | Some value when value = head -> unique last tail acc
            | _ -> unique (Some head) tail (head :: acc)
    let ordered = unique None sorted []
    let idsArray = ordered |> List.toArray
    let wordCount = (idsArray.Length + 63) / 64
    match ordered with
    | [] ->
        { Ids = idsArray; IndexOf = [||]; IndexOffset = 0; WordCount = 0 }
    | _ ->
        let minId = List.head ordered
        let maxId = List.last ordered
        let size = maxId - minId + 1
        let indexOf = Array.create size -1
        ordered |> List.iteri (fun idx id -> indexOf.[id - minId] <- idx)
        { Ids = idsArray; IndexOf = indexOf; IndexOffset = minId; WordCount = wordCount }

let private tryIndexOf (domain: VRegDomain) (value: int) : int option =
    if domain.IndexOf.Length = 0 then
        None
    else
        let idx = value - domain.IndexOffset
        if idx < 0 || idx >= domain.IndexOf.Length then
            None
        else
            let mapped = domain.IndexOf.[idx]
            if mapped >= 0 then Some mapped else None

let private bitsetEmpty (wordCount: int) : BitSet =
    Bitset.empty wordCount

let private bitsetClone (bits: BitSet) : BitSet =
    Bitset.clone bits

let private bitsetIsEmpty (bits: BitSet) : bool =
    Bitset.isEmpty bits

let private bitsetEqual (left: BitSet) (right: BitSet) : bool =
    Bitset.equal left right

let private bitsetUnion (left: BitSet) (right: BitSet) : BitSet =
    Bitset.union left right

let private bitsetDiff (left: BitSet) (right: BitSet) : BitSet =
    Bitset.diff left right

let bitsetContains (domain: VRegDomain) (bits: BitSet) (value: int) : bool =
    match tryIndexOf domain value with
    | Some idx -> Bitset.containsIndex idx bits
    | None -> false

let private bitsetAddInPlace (domain: VRegDomain) (value: int) (bits: BitSet) : unit =
    match tryIndexOf domain value with
    | Some idx -> Bitset.addIndexInPlace idx bits
    | None -> ()

let private bitsetRemoveInPlace (domain: VRegDomain) (value: int) (bits: BitSet) : unit =
    match tryIndexOf domain value with
    | Some idx -> Bitset.removeIndexInPlace idx bits
    | None -> ()

let private bitsetAddIndexInPlace (idx: int) (bits: BitSet) : unit =
    Bitset.addIndexInPlace idx bits

let private bitsetRemoveIndexInPlace (idx: int) (bits: BitSet) : unit =
    Bitset.removeIndexInPlace idx bits

let private bitsetContainsIndex (idx: int) (bits: BitSet) : bool =
    Bitset.containsIndex idx bits

let private bitsetUnionInPlace (left: BitSet) (right: BitSet) : unit =
    Bitset.unionInPlace left right

let private bitsetIntersects (left: BitSet) (right: BitSet) : bool =
    Bitset.intersects left right

let private bitsetIter (domain: VRegDomain) (bits: BitSet) (f: int -> unit) : unit =
    Bitset.iterIndices bits (fun idx ->
        if idx < domain.Ids.Length then
            f domain.Ids.[idx])

let private bitsetIterIndices (bits: BitSet) (f: int -> unit) : unit =
    Bitset.iterIndices bits f

let private bitsetFromList (domain: VRegDomain) (values: int list) : BitSet =
    if List.isEmpty values then
        bitsetEmpty domain.WordCount
    else
        let bits = Bitset.empty domain.WordCount
        for value in values do
            match tryIndexOf domain value with
            | Some idx ->
                Bitset.addIndexInPlace idx bits
            | None ->
                Crash.crash $"BitSet: Missing vreg {value} in domain"
        bits

let private bitsetDiffInPlace (left: BitSet) (right: BitSet) : unit =
    Bitset.diffInPlace left right

let private bitsetCount (bits: BitSet) : int =
    Bitset.count bits

let private bitsetIndicesToList (bits: BitSet) : int list =
    Bitset.indicesToList bits

let private tryLabelIndex (labels: LIR.Label array) (label: LIR.Label) : int option =
    if labels.Length = 0 then
        None
    else
        let rec search low high =
            if low > high then
                None
            else
                let mid = (low + high) / 2
                let cmp = compare labels.[mid] label
                if cmp = 0 then
                    Some mid
                else if cmp < 0 then
                    search (mid + 1) high
                else
                    search low (mid - 1)
        search 0 (labels.Length - 1)

let private buildBlockIndex (cfg: LIR.CFG) : BlockIndex * LIR.BasicBlock array =
    let entries = cfg.Blocks |> Seq.toArray
    let labels = entries |> Array.map (fun kvp -> kvp.Key)
    let blocks = entries |> Array.map (fun kvp -> kvp.Value)
    let entryIndex =
        match tryLabelIndex labels cfg.Entry with
        | Some idx -> idx
        | None -> Crash.crash $"BlockIndex: Missing entry label {cfg.Entry}"
    ({ Labels = labels; EntryIndex = entryIndex }, blocks)

let private tryBlockIndex (index: BlockIndex) (label: LIR.Label) : int option =
    tryLabelIndex index.Labels label

let blockIndexOfLabel (index: BlockIndex) (label: LIR.Label) : int option =
    tryBlockIndex index label

let blockLivenessForLabel
    (index: BlockIndex)
    (liveness: BlockLiveness array)
    (label: LIR.Label)
    : BlockLiveness option =
    match tryBlockIndex index label with
    | Some idx -> Some liveness.[idx]
    | None -> None

let private blocksToMap (index: BlockIndex) (blocks: LIR.BasicBlock array) : Map<LIR.Label, LIR.BasicBlock> =
    Array.zip index.Labels blocks |> Map.ofArray

// ============================================================================
// Chordal Graph Coloring Types
// ============================================================================

/// Interference graph for register allocation
/// In SSA form, this graph is guaranteed to be chordal
type InterferenceGraph = {
    Domain: VRegDomain
    Vertices: BitSet                // Domain indices present in the graph
    Neighbors: BitSet array         // Adjacency bitsets per domain index
}

/// Result of graph coloring
type ColoringResult = {
    Domain: VRegDomain
    Colors: int option array        // Domain index → color (0..k-1)
    Spills: BitSet                  // Domain indices that must be spilled
    ChromaticNumber: int            // Max color used + 1
}

/// Profiling data for Maximum Cardinality Search
type McsProfile = {
    VertexCount: int
    SelectionChecks: int
    WeightUpdates: int
    BucketSkips: int
}

/// Build an interference graph from an explicit vertex list and edge list.
let buildInterferenceGraphFromEdges (vertices: int list) (edges: (int * int) list) : InterferenceGraph =
    let domain = buildVRegDomain vertices
    let n = domain.Ids.Length
    let wordCount = domain.WordCount
    let neighbors = Array.init n (fun _ -> bitsetEmpty wordCount)
    let present = bitsetEmpty wordCount

    for v in vertices do
        match tryIndexOf domain v with
        | Some idx -> bitsetAddIndexInPlace idx present
        | None -> Crash.crash $"Interference graph missing vertex {v}"

    for (u, v) in edges do
        if u <> v then
            match tryIndexOf domain u, tryIndexOf domain v with
            | Some idxU, Some idxV ->
                bitsetAddIndexInPlace idxU present
                bitsetAddIndexInPlace idxV present
                bitsetAddIndexInPlace idxV neighbors.[idxU]
                bitsetAddIndexInPlace idxU neighbors.[idxV]
            | _ ->
                Crash.crash $"Interference graph missing edge endpoint {u} or {v}"

    { Domain = domain; Vertices = present; Neighbors = neighbors }

/// Check if a graph contains a vertex.
let graphHasVertex (graph: InterferenceGraph) (vregId: int) : bool =
    bitsetContains graph.Domain graph.Vertices vregId

/// Get neighbors of a vertex in the interference graph.
let graphNeighbors (graph: InterferenceGraph) (vregId: int) : int list =
    match tryIndexOf graph.Domain vregId with
    | None -> []
    | Some idx ->
        if not (bitsetContainsIndex idx graph.Vertices) then
            []
        else
            let mutable acc = []
            bitsetIterIndices graph.Neighbors.[idx] (fun nidx ->
                if bitsetContainsIndex nidx graph.Vertices then
                    acc <- graph.Domain.Ids.[nidx] :: acc)
            List.rev acc

/// Get the assigned color of a vertex.
let colorOf (result: ColoringResult) (vregId: int) : int option =
    match tryIndexOf result.Domain vregId with
    | Some idx -> result.Colors.[idx]
    | None -> None

/// Check if a vertex was spilled.
let isSpill (result: ColoringResult) (vregId: int) : bool =
    match tryIndexOf result.Domain vregId with
    | Some idx -> bitsetContainsIndex idx result.Spills
    | None -> false

/// Count spilled vertices.
let spillCount (result: ColoringResult) : int =
    bitsetCount result.Spills

/// Count colored vertices.
let coloredCount (result: ColoringResult) : int =
    result.Colors
    |> Array.fold (fun acc color -> if color.IsSome then acc + 1 else acc) 0
// ============================================================================
// Liveness Analysis
// ============================================================================

/// Get virtual register IDs used (read) by an instruction
let getUsedVRegs (instr: LIR.Instr) : int list =
    let regToVReg (reg: LIR.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    let operandToVReg (op: LIR.Operand) : int option =
        match op with
        | LIR.Reg reg -> regToVReg reg
        | _ -> None

    match instr with
    | LIR.Mov (_, src) ->
        operandToVReg src |> Option.toList
    | LIR.Store (_, src) ->
        regToVReg src |> Option.toList
    | LIR.Add (_, left, right) | LIR.Sub (_, left, right) ->
        (regToVReg left |> Option.toList) @ (operandToVReg right |> Option.toList)
    | LIR.Mul (_, left, right) | LIR.Sdiv (_, left, right) | LIR.Udiv (_, left, right)
    | LIR.And (_, left, right) | LIR.Orr (_, left, right) | LIR.Eor (_, left, right)
    | LIR.Lsl (_, left, right) | LIR.Lsr (_, left, right) ->
        (regToVReg left |> Option.toList) @ (regToVReg right |> Option.toList)
    | LIR.Lsl_imm (_, src, _) | LIR.Lsr_imm (_, src, _) | LIR.And_imm (_, src, _) ->
        regToVReg src |> Option.toList
    | LIR.Msub (_, mulLeft, mulRight, sub) ->
        (regToVReg mulLeft |> Option.toList)
        @ (regToVReg mulRight |> Option.toList)
        @ (regToVReg sub |> Option.toList)
    | LIR.Madd (_, mulLeft, mulRight, add) ->
        (regToVReg mulLeft |> Option.toList)
        @ (regToVReg mulRight |> Option.toList)
        @ (regToVReg add |> Option.toList)
    | LIR.Cmp (left, right) ->
        (regToVReg left |> Option.toList) @ (operandToVReg right |> Option.toList)
    | LIR.Cset (_, _) -> []
    | LIR.Mvn (_, src) ->
        regToVReg src |> Option.toList
    | LIR.Sxtb (_, src) | LIR.Sxth (_, src) | LIR.Sxtw (_, src)
    | LIR.Uxtb (_, src) | LIR.Uxth (_, src) | LIR.Uxtw (_, src) ->
        regToVReg src |> Option.toList
    | LIR.Call (_, _, args) ->
        args |> List.choose operandToVReg
    | LIR.TailCall (_, args) ->
        args |> List.choose operandToVReg
    | LIR.IndirectCall (_, func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        funcVReg @ argsVRegs
    | LIR.IndirectTailCall (func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        funcVReg @ argsVRegs
    | LIR.ClosureAlloc (_, _, captures) ->
        captures |> List.choose operandToVReg
    | LIR.ClosureCall (_, closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        closureVReg @ argsVRegs
    | LIR.ClosureTailCall (closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        closureVReg @ argsVRegs
    | LIR.PrintInt64 reg | LIR.PrintBool reg
    | LIR.PrintInt64NoNewline reg | LIR.PrintBoolNoNewline reg
    | LIR.PrintHeapStringNoNewline reg | LIR.PrintList (reg, _)
    | LIR.PrintSum (reg, _) | LIR.PrintRecord (reg, _, _) ->
        regToVReg reg |> Option.toList
    | LIR.PrintFloatNoNewline _ -> []  // FP register, not GP
    | LIR.PrintChars _ -> []  // No registers used
    | LIR.PrintBytes reg -> regToVReg reg |> Option.toList
    | LIR.HeapAlloc (_, _) -> []
    | LIR.HeapStore (addr, _, src, valueType) ->
        let a = regToVReg addr |> Option.toList
        // For float values, the src register is an FVirtual (handled by float allocation)
        // so we don't include it in integer liveness
        let s =
            match valueType with
            | Some AST.TFloat64 -> []
            | _ -> operandToVReg src |> Option.toList
        a @ s
    | LIR.HeapLoad (_, addr, _) ->
        regToVReg addr |> Option.toList
    | LIR.RefCountInc (addr, _, _) ->
        regToVReg addr |> Option.toList
    | LIR.RefCountDec (addr, _, _) ->
        regToVReg addr |> Option.toList
    | LIR.StringConcat (_, left, right) ->
        (operandToVReg left |> Option.toList) @ (operandToVReg right |> Option.toList)
    | LIR.PrintHeapString reg ->
        regToVReg reg |> Option.toList
    | LIR.FileReadText (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileExists (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileWriteText (_, path, content) ->
        (operandToVReg path |> Option.toList) @ (operandToVReg content |> Option.toList)
    | LIR.FileAppendText (_, path, content) ->
        (operandToVReg path |> Option.toList) @ (operandToVReg content |> Option.toList)
    | LIR.FileDelete (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileSetExecutable (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileWriteFromPtr (_, path, ptr, length) ->
        (operandToVReg path |> Option.toList)
        @ (regToVReg ptr |> Option.toList)
        @ (regToVReg length |> Option.toList)
    | LIR.RawAlloc (_, numBytes) ->
        regToVReg numBytes |> Option.toList
    | LIR.RawFree ptr ->
        regToVReg ptr |> Option.toList
    | LIR.RawGet (_, ptr, byteOffset) ->
        (regToVReg ptr |> Option.toList) @ (regToVReg byteOffset |> Option.toList)
    | LIR.RawGetByte (_, ptr, byteOffset) ->
        (regToVReg ptr |> Option.toList) @ (regToVReg byteOffset |> Option.toList)
    | LIR.RawSet (ptr, byteOffset, value, _) ->
        (regToVReg ptr |> Option.toList)
        @ (regToVReg byteOffset |> Option.toList)
        @ (regToVReg value |> Option.toList)
    | LIR.RawSetByte (ptr, byteOffset, value) ->
        (regToVReg ptr |> Option.toList)
        @ (regToVReg byteOffset |> Option.toList)
        @ (regToVReg value |> Option.toList)
    // Int64ToFloat uses an integer source register
    | LIR.Int64ToFloat (_, src) ->
        regToVReg src |> Option.toList
    | LIR.RefCountIncString str ->
        operandToVReg str |> Option.toList
    | LIR.RefCountDecString str ->
        operandToVReg str |> Option.toList
    | LIR.RandomInt64 _ ->
        []  // No operands to read
    | LIR.DateNow _ ->
        []  // No operands to read
    | LIR.FloatToString _ ->
        []  // Float value is in FP register, tracked by getUsedFVRegs
    // ArgMoves/TailArgMoves contain operands that use virtual registers
    | LIR.ArgMoves moves ->
        moves |> List.choose (fun (_, op) -> operandToVReg op)
    | LIR.TailArgMoves moves ->
        moves |> List.choose (fun (_, op) -> operandToVReg op)
    // Phi sources are NOT regular uses - they are used at predecessor exits, not at the phi's block
    // The liveness analysis handles phi sources specially in computeLivenessBitsRaw
    | LIR.Phi _ -> []
    | _ -> []

/// Get virtual register ID defined (written) by an instruction
let getDefinedVReg (instr: LIR.Instr) : int option =
    let regToVReg (reg: LIR.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    match instr with
    | LIR.Mov (dest, _) -> regToVReg dest
    | LIR.Add (dest, _, _) | LIR.Sub (dest, _, _) -> regToVReg dest
    | LIR.Mul (dest, _, _) | LIR.Sdiv (dest, _, _) | LIR.Udiv (dest, _, _) | LIR.Msub (dest, _, _, _) | LIR.Madd (dest, _, _, _) -> regToVReg dest
    | LIR.Cset (dest, _) -> regToVReg dest
    | LIR.And (dest, _, _) | LIR.And_imm (dest, _, _) | LIR.Orr (dest, _, _) | LIR.Eor (dest, _, _)
    | LIR.Lsl (dest, _, _) | LIR.Lsr (dest, _, _) | LIR.Lsl_imm (dest, _, _) | LIR.Lsr_imm (dest, _, _) -> regToVReg dest
    | LIR.Mvn (dest, _) -> regToVReg dest
    | LIR.Sxtb (dest, _) | LIR.Sxth (dest, _) | LIR.Sxtw (dest, _)
    | LIR.Uxtb (dest, _) | LIR.Uxth (dest, _) | LIR.Uxtw (dest, _) -> regToVReg dest
    | LIR.Call (dest, _, _) -> regToVReg dest
    | LIR.TailCall _ -> None  // Tail calls don't return to caller
    | LIR.IndirectCall (dest, _, _) -> regToVReg dest
    | LIR.IndirectTailCall _ -> None  // Indirect tail calls don't return to caller
    | LIR.ClosureAlloc (dest, _, _) -> regToVReg dest
    | LIR.ClosureCall (dest, _, _) -> regToVReg dest
    | LIR.ClosureTailCall _ -> None  // Closure tail calls don't return to caller
    | LIR.HeapAlloc (dest, _) -> regToVReg dest
    | LIR.HeapLoad (dest, _, _) -> regToVReg dest
    | LIR.StringConcat (dest, _, _) -> regToVReg dest
    | LIR.LoadFuncAddr (dest, _) -> regToVReg dest
    | LIR.FileReadText (dest, _) -> regToVReg dest
    | LIR.FileExists (dest, _) -> regToVReg dest
    | LIR.FileWriteText (dest, _, _) -> regToVReg dest
    | LIR.FileAppendText (dest, _, _) -> regToVReg dest
    | LIR.FileDelete (dest, _) -> regToVReg dest
    | LIR.FileSetExecutable (dest, _) -> regToVReg dest
    | LIR.FileWriteFromPtr (dest, _, _, _) -> regToVReg dest
    | LIR.RawAlloc (dest, _) -> regToVReg dest
    | LIR.RawGet (dest, _, _) -> regToVReg dest
    | LIR.RawGetByte (dest, _, _) -> regToVReg dest
    | LIR.RawFree _ -> None
    | LIR.RawSet _ -> None
    | LIR.RawSetByte _ -> None
    // FloatToInt64 defines an integer destination register
    | LIR.FloatToInt64 (dest, _) -> regToVReg dest
    // FloatToBits defines an integer destination register
    | LIR.FloatToBits (dest, _) -> regToVReg dest
    // FpToGp defines an integer destination register
    | LIR.FpToGp (dest, _) -> regToVReg dest
    | LIR.RefCountIncString _ -> None
    | LIR.RefCountDecString _ -> None
    | LIR.RandomInt64 dest -> regToVReg dest
    | LIR.DateNow dest -> regToVReg dest
    | LIR.FloatToString (dest, _) -> regToVReg dest
    // Phi defines its destination at block entry
    | LIR.Phi (dest, _, _) -> regToVReg dest
    | _ -> None

// ============================================================================
// Float Register Liveness Analysis
// ============================================================================

/// Get FVirtual register IDs used (read) by an instruction
let getUsedFVRegs (instr: LIR.Instr) : int list =
    let fregToId (freg: LIR.FReg) : int option =
        match freg with
        | LIR.FVirtual id -> Some id
        | LIR.FPhysical _ -> None

    match instr with
    | LIR.FMov (_, src) -> fregToId src |> Option.toList
    | LIR.FAdd (_, left, right) | LIR.FSub (_, left, right)
    | LIR.FMul (_, left, right) | LIR.FDiv (_, left, right) ->
        [fregToId left; fregToId right] |> List.choose id
    | LIR.FNeg (_, src) | LIR.FAbs (_, src) | LIR.FSqrt (_, src) ->
        fregToId src |> Option.toList
    | LIR.FCmp (left, right) ->
        [fregToId left; fregToId right] |> List.choose id
    | LIR.FloatToInt64 (_, src) -> fregToId src |> Option.toList
    | LIR.FloatToBits (_, src) -> fregToId src |> Option.toList
    | LIR.FpToGp (_, src) -> fregToId src |> Option.toList
    | LIR.PrintFloat freg | LIR.PrintFloatNoNewline freg ->
        fregToId freg |> Option.toList
    | LIR.FArgMoves moves ->
        moves |> List.choose (fun (_, src) -> fregToId src)
    | LIR.FPhi _ -> []  // Phi sources handled specially
    | LIR.FloatToString (_, value) -> fregToId value |> Option.toList
    // HeapStore with float value: the Virtual register ID is shared with FVirtual
    | LIR.HeapStore (_, _, LIR.Reg (LIR.Virtual vregId), Some AST.TFloat64) -> [vregId]
    | _ -> []

/// Get FVirtual register ID defined (written) by an instruction
let getDefinedFVReg (instr: LIR.Instr) : int option =
    let fregToId (freg: LIR.FReg) : int option =
        match freg with
        | LIR.FVirtual id -> Some id
        | LIR.FPhysical _ -> None

    match instr with
    | LIR.FMov (dest, _) -> fregToId dest
    | LIR.FAdd (dest, _, _) | LIR.FSub (dest, _, _)
    | LIR.FMul (dest, _, _) | LIR.FDiv (dest, _, _) -> fregToId dest
    | LIR.FNeg (dest, _) | LIR.FAbs (dest, _) | LIR.FSqrt (dest, _) -> fregToId dest
    | LIR.FLoad (dest, _) -> fregToId dest
    | LIR.Int64ToFloat (dest, _) -> fregToId dest
    | LIR.GpToFp (dest, _) -> fregToId dest
    | LIR.FPhi (dest, _) -> fregToId dest
    | _ -> None

/// Get virtual register used by terminator
let getTerminatorUsedVRegs (term: LIR.Terminator) : int list =
    match term with
    | LIR.Branch (LIR.Virtual id, _, _) -> [id]
    | LIR.BranchZero (LIR.Virtual id, _, _) -> [id]
    | LIR.BranchBitZero (LIR.Virtual id, _, _, _) -> [id]
    | LIR.BranchBitNonZero (LIR.Virtual id, _, _, _) -> [id]
    | LIR.CondBranch _ -> []  // CondBranch uses condition flags, not a register
    | _ -> []

/// Get successor labels for a terminator
let getSuccessors (term: LIR.Terminator) : LIR.Label list =
    match term with
    | LIR.Ret -> []
    | LIR.Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
    | LIR.BranchZero (_, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | LIR.BranchBitZero (_, _, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | LIR.BranchBitNonZero (_, _, nonZeroLabel, zeroLabel) -> [nonZeroLabel; zeroLabel]
    | LIR.CondBranch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
    | LIR.Jump label -> [label]

let private addPhiUse
    (domain: VRegDomain)
    (predIdx: int)
    (vregId: int)
    (uses: (int * BitSet) list)
    : (int * BitSet) list =
    let rec insert remaining =
        match remaining with
        | [] ->
            let bits = bitsetEmpty domain.WordCount
            bitsetAddInPlace domain vregId bits
            [ (predIdx, bits) ]
        | (idx, bits) :: rest ->
            if idx = predIdx then
                bitsetAddInPlace domain vregId bits
                remaining
            else
                (idx, bits) :: insert rest
    insert uses

let private collectPhiUsesByPred
    (domain: VRegDomain)
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    : (int * BitSet) list array =
    let result = Array.init blocks.Length (fun _ -> [])
    for blockIdx in 0 .. blocks.Length - 1 do
        let block = blocks.[blockIdx]
        let mutable uses = []
        for instr in block.Instrs do
            match instr with
            | LIR.Phi (_, sources, _) ->
                for (op, predLabel) in sources do
                    match op with
                    | LIR.Reg (LIR.Virtual id) ->
                        match tryBlockIndex blockIndex predLabel with
                        | Some predIdx -> uses <- addPhiUse domain predIdx id uses
                        | None -> ()
                    | _ -> ()
            | _ -> ()
        result.[blockIdx] <- uses
    result

let private collectFPhiUsesByPred
    (domain: VRegDomain)
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    : (int * BitSet) list array =
    let result = Array.init blocks.Length (fun _ -> [])
    for blockIdx in 0 .. blocks.Length - 1 do
        let block = blocks.[blockIdx]
        let mutable uses = []
        for instr in block.Instrs do
            match instr with
            | LIR.FPhi (_, sources) ->
                for (freg, predLabel) in sources do
                    match freg with
                    | LIR.FVirtual id ->
                        match tryBlockIndex blockIndex predLabel with
                        | Some predIdx -> uses <- addPhiUse domain predIdx id uses
                        | None -> ()
                    | LIR.FPhysical _ -> ()
            | _ -> ()
        result.[blockIdx] <- uses
    result

/// Compute GEN and KILL sets for a basic block
/// GEN = variables used before being defined
/// KILL = variables defined
let computeGenKill (domain: VRegDomain) (block: LIR.BasicBlock) : BitSet * BitSet =
    // Process instructions in forward order
    let gen = bitsetEmpty domain.WordCount
    let kill = bitsetEmpty domain.WordCount

    for instr in block.Instrs do
        let used = getUsedVRegs instr
        let defined = getDefinedVReg instr

        // Add to GEN if used and not already killed (defined earlier in block)
        for u in used do
            if not (bitsetContains domain kill u) then
                bitsetAddInPlace domain u gen

        // Add to KILL if defined
        match defined with
        | Some d -> bitsetAddInPlace domain d kill
        | None -> ()

    // Also add terminator uses to GEN
    let termUses = getTerminatorUsedVRegs block.Terminator
    for u in termUses do
        if not (bitsetContains domain kill u) then
            bitsetAddInPlace domain u gen

    (gen, kill)

/// Compute liveness using backward dataflow analysis
/// Handles SSA phi nodes: phi sources are live at predecessor exits, not at phi's block entry
/// Collect all integer VReg IDs referenced in the CFG (uses, defs, phi sources/dests, terminators)
let private collectVRegIds (blocks: LIR.BasicBlock array) : int list =
    blocks
    |> Array.fold (fun acc block ->
        let acc =
            getTerminatorUsedVRegs block.Terminator
            |> List.fold (fun acc id -> id :: acc) acc
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.Phi (dest, sources, _) ->
                let acc =
                    match dest with
                    | LIR.Virtual id -> id :: acc
                    | LIR.Physical _ -> acc
                sources
                |> List.fold (fun acc (src, _) ->
                    match src with
                    | LIR.Reg (LIR.Virtual id) -> id :: acc
                    | _ -> acc) acc
            | _ ->
                let acc =
                    getUsedVRegs instr
                    |> List.fold (fun acc id -> id :: acc) acc
                match getDefinedVReg instr with
                | Some id -> id :: acc
                | None -> acc) acc
    ) []

/// Compute liveness using bitsets for the dataflow fixed point
let private computeLivenessBitsRaw
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (extraIds: int list)
    : VRegDomain * BlockLiveness array =
    let domain = buildVRegDomain (collectVRegIds blocks @ extraIds)
    let emptyBits = bitsetEmpty domain.WordCount

    let genKillBits =
        Array.init blocks.Length (fun idx ->
            computeGenKill domain blocks.[idx])

    let phiUsesBits = collectPhiUsesByPred domain blockIndex blocks

    let liveness = Array.init blocks.Length (fun _ -> { LiveIn = emptyBits; LiveOut = emptyBits })

    let phiUsesForEdge (succIdx: int) (predIdx: int) : BitSet =
        match phiUsesBits.[succIdx] |> List.tryFind (fun (idx, _) -> idx = predIdx) with
        | Some (_, bits) -> bits
        | None -> emptyBits

    let mutable changed = true
    while changed do
        changed <- false
        for blockIdx in 0 .. blocks.Length - 1 do
            let block = blocks.[blockIdx]
            let (gen, kill) = genKillBits.[blockIdx]
            let oldLiveness = liveness.[blockIdx]

            let successors = getSuccessors block.Terminator
            let newLiveOut =
                successors
                |> List.fold (fun acc succLabel ->
                    match tryBlockIndex blockIndex succLabel with
                    | Some succIdx ->
                        let liveInContrib = liveness.[succIdx].LiveIn
                        let phiContrib = phiUsesForEdge succIdx blockIdx
                        let succLive = bitsetUnion liveInContrib phiContrib
                        bitsetUnion acc succLive
                    | None -> acc
                ) emptyBits

            let newLiveIn = bitsetUnion gen (bitsetDiff newLiveOut kill)

            if not (bitsetEqual newLiveIn oldLiveness.LiveIn) || not (bitsetEqual newLiveOut oldLiveness.LiveOut) then
                changed <- true
                liveness.[blockIdx] <- { LiveIn = newLiveIn; LiveOut = newLiveOut }

    (domain, liveness)

/// Compute liveness using bitsets for the dataflow fixed point
let computeLivenessBits (cfg: LIR.CFG) : VRegDomain * BlockIndex * BlockLiveness array =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let (domain, liveness) = computeLivenessBitsRaw blockIndex blocks []
    (domain, blockIndex, liveness)

/// Compute float GEN and KILL sets for a basic block
/// GEN = float variables used before being defined
/// KILL = float variables defined
let computeFloatGenKill (domain: VRegDomain) (block: LIR.BasicBlock) : BitSet * BitSet =
    let gen = bitsetEmpty domain.WordCount
    let kill = bitsetEmpty domain.WordCount

    for instr in block.Instrs do
        let used = getUsedFVRegs instr
        let defined = getDefinedFVReg instr

        for u in used do
            if not (bitsetContains domain kill u) then
                bitsetAddInPlace domain u gen

        match defined with
        | Some d -> bitsetAddInPlace domain d kill
        | None -> ()

    (gen, kill)

/// Compute float liveness using backward dataflow analysis
/// Handles SSA FPhi nodes: phi sources are live at predecessor exits, not at phi's block entry
/// Collect all float VReg IDs referenced in the CFG (uses, defs, FPhi sources/dests)
let private collectFVRegIds (blocks: LIR.BasicBlock array) : int list =
    blocks
    |> Array.fold (fun acc block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.FPhi (dest, sources) ->
                let acc =
                    match dest with
                    | LIR.FVirtual id -> id :: acc
                    | LIR.FPhysical _ -> acc
                sources
                |> List.fold (fun acc (src, _) ->
                    match src with
                    | LIR.FVirtual id -> id :: acc
                    | LIR.FPhysical _ -> acc) acc
            | _ ->
                let acc =
                    getUsedFVRegs instr
                    |> List.fold (fun acc id -> id :: acc) acc
                match getDefinedFVReg instr with
                | Some id -> id :: acc
                | None -> acc) acc
    ) []

/// Compute float liveness using bitsets for the dataflow fixed point
let private computeFloatLivenessBitsRaw
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (extraIds: int list)
    : VRegDomain * BlockLiveness array =
    let domain = buildVRegDomain (collectFVRegIds blocks @ extraIds)
    let emptyBits = bitsetEmpty domain.WordCount

    let genKillBits =
        Array.init blocks.Length (fun idx ->
            computeFloatGenKill domain blocks.[idx])

    let fphiUsesBits = collectFPhiUsesByPred domain blockIndex blocks

    let liveness = Array.init blocks.Length (fun _ -> { LiveIn = emptyBits; LiveOut = emptyBits })

    let fphiUsesForEdge (succIdx: int) (predIdx: int) : BitSet =
        match fphiUsesBits.[succIdx] |> List.tryFind (fun (idx, _) -> idx = predIdx) with
        | Some (_, bits) -> bits
        | None -> emptyBits

    let mutable changed = true
    while changed do
        changed <- false
        for blockIdx in 0 .. blocks.Length - 1 do
            let block = blocks.[blockIdx]
            let (gen, kill) = genKillBits.[blockIdx]
            let oldLiveness = liveness.[blockIdx]

            let successors = getSuccessors block.Terminator
            let newLiveOut =
                successors
                |> List.fold (fun acc succLabel ->
                    match tryBlockIndex blockIndex succLabel with
                    | Some succIdx ->
                        let liveInContrib = liveness.[succIdx].LiveIn
                        let fphiContrib = fphiUsesForEdge succIdx blockIdx
                        let succLive = bitsetUnion liveInContrib fphiContrib
                        bitsetUnion acc succLive
                    | None -> acc
                ) emptyBits

            let newLiveIn = bitsetUnion gen (bitsetDiff newLiveOut kill)

            if not (bitsetEqual newLiveIn oldLiveness.LiveIn) || not (bitsetEqual newLiveOut oldLiveness.LiveOut) then
                changed <- true
                liveness.[blockIdx] <- { LiveIn = newLiveIn; LiveOut = newLiveOut }

    (domain, liveness)

/// Compute float liveness using bitsets for the dataflow fixed point
let computeFloatLivenessBits (cfg: LIR.CFG) : VRegDomain * BlockIndex * BlockLiveness array =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let (domain, liveness) = computeFloatLivenessBitsRaw blockIndex blocks []
    (domain, blockIndex, liveness)

/// Compute liveness at each instruction index within a block
/// Returns a list of live VReg sets, one per instruction (same order as Instrs)
let computeInstructionLiveness (domain: VRegDomain) (block: LIR.BasicBlock) (liveOut: BitSet) : BitSet list =
    // Walk backwards from the terminator, tracking liveness
    let live = bitsetClone liveOut

    // Handle terminator uses first
    let termUses = getTerminatorUsedVRegs block.Terminator
    for u in termUses do
        bitsetAddInPlace domain u live

    // Walk instructions in reverse, collecting liveness at each point
    // We want liveness AFTER each instruction (what's live when that instruction completes)
    let instrsReversed = List.rev block.Instrs
    let mutable livenessListReversed = []

    for instr in instrsReversed do
        // Record liveness at this point (after the instruction executes)
        livenessListReversed <- bitsetClone live :: livenessListReversed

        // Update liveness: remove definition, add uses
        match getDefinedVReg instr with
        | Some def -> bitsetRemoveInPlace domain def live
        | None -> ()

        for used in getUsedVRegs instr do
            bitsetAddInPlace domain used live

    // Since we walked backwards and prepended each result, the list is already
    // in forward order (matching the original instruction order)
    livenessListReversed

/// Compute float liveness at each instruction index within a block
/// Returns a list of live FVirtual ID sets, one per instruction (same order as Instrs)
let computeFloatInstructionLiveness (domain: VRegDomain) (block: LIR.BasicBlock) (liveOut: BitSet) : BitSet list =
    let live = bitsetClone liveOut

    let instrsReversed = List.rev block.Instrs
    let mutable livenessListReversed = []

    for instr in instrsReversed do
        livenessListReversed <- bitsetClone live :: livenessListReversed

        match getDefinedFVReg instr with
        | Some def -> bitsetRemoveInPlace domain def live
        | None -> ()

        for used in getUsedFVRegs instr do
            bitsetAddInPlace domain used live

    livenessListReversed

// ============================================================================
// Register Definitions
// ============================================================================

/// Caller-saved registers (X1-X7) - preferred for allocation
/// Note: X8 is excluded because StringConcat uses it as a scratch register for byte copying
/// Note: X9-X10 are excluded because they are used as compiler scratch registers
let callerSavedRegs = [
    LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5
    LIR.X6; LIR.X7
]

/// Callee-saved registers - used when caller-saved exhausted
/// These must be saved/restored in function prologue/epilogue
/// Note: X27 reserved for free list base (ARM64) / unused (x86_64)
/// On x86_64, X22→R14 and X23→R15 are reserved for heap/free list pointers
let calleeSavedRegsFor (arch: Platform.Arch) =
    match arch with
    | Platform.X86_64 ->
        // x86_64: X22 (R14) = heap ptr, X23 (R15) = free list — not allocatable
        // X24-X26 have no x86_64 equivalents
        [LIR.X19; LIR.X20; LIR.X21]
    | Platform.ARM64 ->
        // ARM64: X27/X28 reserved, X19-X26 allocatable
        [LIR.X19; LIR.X20; LIR.X21; LIR.X22; LIR.X23
         LIR.X24; LIR.X25; LIR.X26]

/// Check if an instruction is a non-tail call (requires SaveRegs/RestoreRegs)
let isNonTailCall (instr: LIR.Instr) : bool =
    match instr with
    | LIR.Call _ | LIR.IndirectCall _ | LIR.ClosureCall _ -> true
    | _ -> false

/// Check if a function has any non-tail calls
/// If it does, we prefer callee-saved registers to avoid per-call save/restore overhead
let hasNonTailCalls (blocks: LIR.BasicBlock array) : bool =
    blocks
    |> Array.exists (fun block ->
        block.Instrs |> List.exists isNonTailCall)

/// Get the optimal register allocation order based on calling pattern
/// - Functions with non-tail calls: prefer callee-saved (save once in prologue/epilogue)
/// - Leaf functions / tail-call-only: prefer caller-saved (no prologue/epilogue overhead)
let getAllocatableRegs (arch: Platform.Arch) (blocks: LIR.BasicBlock array) : LIR.PhysReg list =
    let calleeSaved = calleeSavedRegsFor arch
    if hasNonTailCalls blocks then
        calleeSaved @ callerSavedRegs
    else
        callerSavedRegs @ calleeSaved

// ============================================================================
// Chordal Graph Coloring Register Allocation
// ============================================================================

let private buildInterferenceGraphBitsetFastWithLivenessInternal
    (trackTiming: bool)
    (swOpt: System.Diagnostics.Stopwatch option)
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (domain: VRegDomain)
    (liveness: BlockLiveness array)
    (entryDefs: BitSet)
    : InterferenceGraph * InterferenceGraphTiming =
    let n = domain.Ids.Length
    let wordCount = domain.WordCount
    let adjacency = Array.init n (fun _ -> bitsetEmpty wordCount)
    let present = bitsetEmpty wordCount
    let mutable liveIterMs = 0.0
    let mutable adjacencyMs = 0.0

    let timeBlock (accumulate: float -> unit) (f: unit -> 'a) : 'a =
        if not trackTiming then
            f ()
        else
            match swOpt with
            | None -> f ()
            | Some sw ->
                let start = sw.Elapsed.TotalMilliseconds
                let result = f ()
                let delta = sw.Elapsed.TotalMilliseconds - start
                accumulate delta
                result

    let markPresentIdx (idx: int) =
        if idx >= 0 && idx < n then
            bitsetAddIndexInPlace idx present

    let markPresentValue (value: int) =
        match tryIndexOf domain value with
        | Some idx -> markPresentIdx idx
        | None -> ()

    let addEdgesToLive (defIdx: int) (live: BitSet) =
        if trackTiming then
            let liveIndices =
                timeBlock (fun delta -> liveIterMs <- liveIterMs + delta) (fun () ->
                    let mutable acc = []
                    bitsetIterIndices live (fun idx ->
                        if idx <> defIdx then
                            acc <- idx :: acc)
                    List.rev acc)

            timeBlock (fun delta -> adjacencyMs <- adjacencyMs + delta) (fun () ->
                bitsetUnionInPlace adjacency.[defIdx] live
                bitsetRemoveIndexInPlace defIdx adjacency.[defIdx]
                for idx in liveIndices do
                    bitsetAddIndexInPlace defIdx adjacency.[idx])
        else
            bitsetUnionInPlace adjacency.[defIdx] live
            bitsetRemoveIndexInPlace defIdx adjacency.[defIdx]
            bitsetIterIndices live (fun idx ->
                if idx <> defIdx then
                    bitsetAddIndexInPlace defIdx adjacency.[idx])

    for blockIdx in 0 .. blocks.Length - 1 do
        let block = blocks.[blockIdx]
        let blockLiveness = liveness.[blockIdx]
        let mutable live = bitsetClone blockLiveness.LiveOut

        let termUses = getTerminatorUsedVRegs block.Terminator
        for v in termUses do
            bitsetAddInPlace domain v live

        timeBlock (fun delta -> liveIterMs <- liveIterMs + delta) (fun () ->
            bitsetIterIndices live markPresentIdx)

        for instr in List.rev block.Instrs do
            let def = getDefinedVReg instr
            let uses = getUsedVRegs instr

            for u in uses do
                markPresentValue u

            match def with
            | Some d ->
                markPresentValue d
                match tryIndexOf domain d with
                | Some defIdx -> addEdgesToLive defIdx live
                | None -> ()
                bitsetRemoveInPlace domain d live
            | None -> ()

            for u in uses do
                bitsetAddInPlace domain u live

        if blockIdx = blockIndex.EntryIndex then
            bitsetIterIndices entryDefs (fun defIdx ->
                if bitsetContainsIndex defIdx live then
                    markPresentIdx defIdx
                    addEdgesToLive defIdx live)

    let timing = {
        LiveIterationMs = liveIterMs
        AdjacencyUpdatesMs = adjacencyMs
    }
    ({ Domain = domain; Vertices = present; Neighbors = adjacency }, timing)

/// Build interference graph from CFG using bitset liveness
let private buildInterferenceGraphBitsetWithLiveness
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (domain: VRegDomain)
    (liveness: BlockLiveness array)
    (entryDefs: BitSet)
    : InterferenceGraph =
    buildInterferenceGraphBitsetFastWithLivenessInternal false None blockIndex blocks domain liveness entryDefs
    |> fst

let private buildInterferenceGraphBitsetWithLivenessProfile
    (sw: System.Diagnostics.Stopwatch)
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (domain: VRegDomain)
    (liveness: BlockLiveness array)
    (entryDefs: BitSet)
    : InterferenceGraph * InterferenceGraphTiming =
    buildInterferenceGraphBitsetFastWithLivenessInternal true (Some sw) blockIndex blocks domain liveness entryDefs

/// Build interference graph from CFG using bitset liveness
let buildInterferenceGraphBitsetFast
    (cfg: LIR.CFG)
    (entryDefs: int list)
    : InterferenceGraph =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let (domain, liveness) = computeLivenessBitsRaw blockIndex blocks entryDefs
    let entryBits = bitsetFromList domain entryDefs
    buildInterferenceGraphBitsetWithLiveness blockIndex blocks domain liveness entryBits

/// Build interference graph from CFG using bitset liveness
let buildInterferenceGraphBitset
    (cfg: LIR.CFG)
    (entryDefs: int list)
    : InterferenceGraph =
    buildInterferenceGraphBitsetFast cfg entryDefs

/// Build float interference graph from CFG using bitset liveness
let private buildFloatInterferenceGraphBitsetWithLiveness
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (domain: VRegDomain)
    (liveness: BlockLiveness array)
    (entryDefs: BitSet)
    : InterferenceGraph =
    let ids = domain.Ids
    let n = ids.Length
    let wordCount = domain.WordCount
    let adjacency = Array.init n (fun _ -> bitsetEmpty wordCount)
    let present = bitsetEmpty wordCount

    let markPresentIdx (idx: int) =
        if idx >= 0 && idx < n then
            bitsetAddIndexInPlace idx present

    let markPresentValue (value: int) =
        match tryIndexOf domain value with
        | Some idx -> markPresentIdx idx
        | None -> ()

    let addEdgesToLive (defIdx: int) (live: BitSet) =
        bitsetUnionInPlace adjacency.[defIdx] live
        bitsetRemoveIndexInPlace defIdx adjacency.[defIdx]
        bitsetIterIndices live (fun idx ->
            if idx <> defIdx then
                bitsetAddIndexInPlace defIdx adjacency.[idx])

    for blockIdx in 0 .. blocks.Length - 1 do
        let block = blocks.[blockIdx]
        let blockLiveness = liveness.[blockIdx]
        let mutable live = bitsetClone blockLiveness.LiveOut
        bitsetIterIndices live markPresentIdx

        for instr in List.rev block.Instrs do
            let def = getDefinedFVReg instr
            let uses = getUsedFVRegs instr

            for u in uses do
                markPresentValue u

            match def with
            | Some d ->
                markPresentValue d
                match tryIndexOf domain d with
                | Some defIdx -> addEdgesToLive defIdx live
                | None -> ()
                bitsetRemoveInPlace domain d live
            | None -> ()

            for u in uses do
                bitsetAddInPlace domain u live

        if blockIdx = blockIndex.EntryIndex then
            bitsetIterIndices entryDefs (fun defIdx ->
                if bitsetContainsIndex defIdx live then
                    markPresentIdx defIdx
                    addEdgesToLive defIdx live)

    { Domain = domain; Vertices = present; Neighbors = adjacency }

let private normalizePair (a: int) (b: int) : int * int =
    if a < b then (a, b) else (b, a)

let private dedupePairs (pairs: (int * int) list) : (int * int) list =
    let sorted = pairs |> List.map (fun (a, b) -> normalizePair a b) |> List.sort
    let rec loop (last: (int * int) option) (acc: (int * int) list) (remaining: (int * int) list) =
        match remaining with
        | [] -> List.rev acc
        | head :: tail ->
            match last with
            | Some prev when prev = head -> loop last acc tail
            | _ -> loop (Some head) (head :: acc) tail
    loop None [] sorted

/// Collect move-related coalescing pairs from CFG.
/// Returns undirected pairs of virtual registers that are directly moved between.
let collectMovePairs (blocks: LIR.BasicBlock array) : (int * int) list =
    blocks
    |> Array.fold (fun acc block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.Mov (LIR.Virtual destId, LIR.Reg (LIR.Virtual srcId)) ->
                (destId, srcId) :: acc
            | _ -> acc) acc) []
    |> dedupePairs

/// Collect phi-related coalescing pairs from CFG.
/// Returns undirected pairs of virtual registers that flow into the same phi destination.
let collectPhiPairs (blocks: LIR.BasicBlock array) : (int * int) list =
    blocks
    |> Array.fold (fun acc block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.Phi (LIR.Virtual destId, sources, _) ->
                sources
                |> List.fold (fun acc (src, _) ->
                    match src with
                    | LIR.Reg (LIR.Virtual srcId) when srcId <> destId ->
                        (destId, srcId) :: acc
                    | _ -> acc) acc
            | _ -> acc) acc) []
    |> dedupePairs

/// Collect phi coalescing preferences from CFG.
/// Returns undirected pairs (vregId, vregId) representing preferred coalescing.
let collectPhiPreferences (blocks: LIR.BasicBlock array) : (int * int) list =
    collectPhiPairs blocks

/// Maximum Cardinality Search - computes Perfect Elimination Ordering for chordal graphs
/// Returns vertices in PEO order (first vertex is most "central")
/// Uses a bucket queue for linear-time selection in terms of vertices + edges.
let private maximumCardinalitySearchCore
    (graph: InterferenceGraph)
    (swOpt: System.Diagnostics.Stopwatch option)
    : int list * McsProfile * McsTiming option =
    let domain = graph.Domain
    let n = domain.Ids.Length
    let vertexCount = bitsetCount graph.Vertices
    if vertexCount = 0 then
        let profile = { VertexCount = 0; SelectionChecks = 0; WeightUpdates = 0; BucketSkips = 0 }
        let timing =
            match swOpt with
            | None -> None
            | Some _ -> Some { SelectMs = 0.0; UpdateMs = 0.0 }
        ([], profile, timing)
    else
        let inGraph = Array.create n false
        bitsetIterIndices graph.Vertices (fun idx -> inGraph.[idx] <- true)

        // Track weights and ordered status
        let weights = Array.zeroCreate<int> n
        let ordered = Array.create n false

        // Bucket queue state (weight -> list of vertices)
        let bucketHeads = Array.create vertexCount -1
        let next = Array.create n -1
        let prev = Array.create n -1

        // Initialize all vertices in bucket 0
        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                let head = bucketHeads.[0]
                next.[idx] <- head
                prev.[idx] <- -1
                if head <> -1 then prev.[head] <- idx
                bucketHeads.[0] <- idx

        let removeFromBucket (idx: int) (weight: int) : unit =
            let p = prev.[idx]
            let nidx = next.[idx]
            if p <> -1 then
                next.[p] <- nidx
            else
                bucketHeads.[weight] <- nidx
            if nidx <> -1 then
                prev.[nidx] <- p
            next.[idx] <- -1
            prev.[idx] <- -1

        let addToBucket (idx: int) (weight: int) : unit =
            let head = bucketHeads.[weight]
            next.[idx] <- head
            prev.[idx] <- -1
            if head <> -1 then prev.[head] <- idx
            bucketHeads.[weight] <- idx

        let timeBlock (accumulate: float -> unit) (f: unit -> 'a) : 'a =
            match swOpt with
            | None -> f ()
            | Some sw ->
                let start = sw.Elapsed.TotalMilliseconds
                let result = f ()
                let delta = sw.Elapsed.TotalMilliseconds - start
                accumulate delta
                result

        let mutable currentMax = 0
        let mutable ordering = []
        let mutable selectionChecks = 0
        let mutable weightUpdates = 0
        let mutable bucketSkips = 0
        let mutable selectMs = 0.0
        let mutable updateMs = 0.0

        for _ in 0 .. vertexCount - 1 do
            let idx =
                timeBlock (fun delta -> selectMs <- selectMs + delta) (fun () ->
                    while currentMax >= 0 && bucketHeads.[currentMax] = -1 do
                        currentMax <- currentMax - 1
                        bucketSkips <- bucketSkips + 1
                    if currentMax < 0 then
                        Crash.crash "MCS bucket queue empty before selecting all vertices"

                    let idx = bucketHeads.[currentMax]
                    selectionChecks <- selectionChecks + 1
                    removeFromBucket idx currentMax
                    ordered.[idx] <- true
                    ordering <- domain.Ids.[idx] :: ordering
                    idx)

            timeBlock (fun delta -> updateMs <- updateMs + delta) (fun () ->
                bitsetIterIndices graph.Neighbors.[idx] (fun nidx ->
                    if inGraph.[nidx] && not ordered.[nidx] then
                        let oldWeight = weights.[nidx]
                        removeFromBucket nidx oldWeight
                        let newWeight = oldWeight + 1
                        if newWeight >= vertexCount then
                            Crash.crash $"MCS weight overflow: {newWeight} >= {vertexCount}"
                        weights.[nidx] <- newWeight
                        addToBucket nidx newWeight
                        if newWeight > currentMax then currentMax <- newWeight
                        weightUpdates <- weightUpdates + 1))

        let profile = {
            VertexCount = vertexCount
            SelectionChecks = selectionChecks
            WeightUpdates = weightUpdates
            BucketSkips = bucketSkips
        }
        let timing =
            match swOpt with
            | None -> None
            | Some _ -> Some { SelectMs = selectMs; UpdateMs = updateMs }
        (List.rev ordering, profile, timing)

let maximumCardinalitySearchWithProfile (graph: InterferenceGraph) : int list * McsProfile =
    let (ordering, profile, _timing) = maximumCardinalitySearchCore graph None
    (ordering, profile)

let private maximumCardinalitySearchWithTiming
    (graph: InterferenceGraph)
    (sw: System.Diagnostics.Stopwatch)
    : int list * McsTiming =
    let (ordering, _profile, timingOpt) = maximumCardinalitySearchCore graph (Some sw)
    let timing =
        match timingOpt with
        | Some value -> value
        | None -> { SelectMs = 0.0; UpdateMs = 0.0 }
    (ordering, timing)

let maximumCardinalitySearch (graph: InterferenceGraph) : int list =
    let (ordering, _profile) = maximumCardinalitySearchWithProfile graph
    ordering

type private CoalescedGraph = {
    Graph: InterferenceGraph
    RepOfIndex: int array
    RepMembers: BitSet array
    Preferences: BitSet array
    Precolored: int option array
}

let private coalesceGraphFast
    (graph: InterferenceGraph)
    (precoloredPairs: (int * int) list)
    (movePairs: (int * int) list)
    (preferencePairs: (int * int) list)
    : CoalescedGraph =
    let domain = graph.Domain
    let n = domain.Ids.Length
    let wordCount = domain.WordCount
    if bitsetIsEmpty graph.Vertices then
        { Graph = graph
          RepOfIndex = Array.init n id
          RepMembers = Array.init n (fun _ -> bitsetEmpty wordCount)
          Preferences = Array.init n (fun _ -> bitsetEmpty wordCount)
          Precolored = Array.create n None }
    else
        let inGraph = Array.create n false
        bitsetIterIndices graph.Vertices (fun idx -> inGraph.[idx] <- true)

        let parent = Array.init n id
        let sizes = Array.create n 0
        let members = Array.init n (fun _ -> bitsetEmpty wordCount)
        let neighbors = Array.init n (fun _ -> bitsetEmpty wordCount)
        let precolor = Array.create n None
        let repId = Array.create n 0

        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                sizes.[idx] <- 1
                let bits = bitsetEmpty wordCount
                bitsetAddIndexInPlace idx bits
                members.[idx] <- bits
                neighbors.[idx] <- bitsetClone graph.Neighbors.[idx]
                repId.[idx] <- domain.Ids.[idx]
            else
                repId.[idx] <- domain.Ids.[idx]

        for (vregId, color) in precoloredPairs do
            match tryIndexOf domain vregId with
            | Some idx when inGraph.[idx] -> precolor.[idx] <- Some color
            | _ -> ()

        let rec find idx =
            let p = parent.[idx]
            if p = idx then
                idx
            else
                let root = find p
                parent.[idx] <- root
                root

        let canMerge rootA rootB =
            if rootA = rootB then
                false
            else
                match precolor.[rootA], precolor.[rootB] with
                | Some c1, Some c2 when c1 <> c2 -> false
                | _ ->
                    if bitsetIntersects neighbors.[rootA] members.[rootB] then
                        false
                    else
                        not (bitsetIntersects neighbors.[rootB] members.[rootA])

        let union rootA rootB =
            let ra, rb =
                if sizes.[rootA] < sizes.[rootB] then
                    (rootB, rootA)
                else
                    (rootA, rootB)
            parent.[rb] <- ra
            sizes.[ra] <- sizes.[ra] + sizes.[rb]
            bitsetUnionInPlace members.[ra] members.[rb]
            bitsetUnionInPlace neighbors.[ra] neighbors.[rb]
            bitsetDiffInPlace neighbors.[ra] members.[ra]
            match precolor.[ra], precolor.[rb] with
            | None, Some color -> precolor.[ra] <- Some color
            | _ -> ()
            if repId.[rb] < repId.[ra] then
                repId.[ra] <- repId.[rb]

        for (u, v) in movePairs do
            match tryIndexOf domain u, tryIndexOf domain v with
            | Some idxU, Some idxV when inGraph.[idxU] && inGraph.[idxV] ->
                let rootU = find idxU
                let rootV = find idxV
                if canMerge rootU rootV then
                    union rootU rootV
            | _ -> ()

        let rootOfIdx = Array.init n (fun idx -> if inGraph.[idx] then find idx else idx)

        let repIndexOfRoot = Array.create n -1
        for idx in 0 .. n - 1 do
            if inGraph.[idx] && parent.[idx] = idx then
                let repValue = repId.[idx]
                match tryIndexOf domain repValue with
                | Some repIdx -> repIndexOfRoot.[idx] <- repIdx
                | None -> Crash.crash $"coalesceGraphFast: Missing rep index for {repValue}"

        let repOfIndex = Array.create n -1
        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                let root = rootOfIdx.[idx]
                let repIdx = repIndexOfRoot.[root]
                if repIdx < 0 then
                    Crash.crash $"coalesceGraphFast: Missing rep for {domain.Ids.[idx]}"
                repOfIndex.[idx] <- repIdx

        let repMembers = Array.init n (fun _ -> bitsetEmpty wordCount)
        let repVertices = bitsetEmpty wordCount
        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                let repIdx = repOfIndex.[idx]
                bitsetAddIndexInPlace idx repMembers.[repIdx]
                bitsetAddIndexInPlace repIdx repVertices

        let repPrecolored = Array.create n None
        for idx in 0 .. n - 1 do
            if inGraph.[idx] && parent.[idx] = idx then
                let repIdx = repIndexOfRoot.[idx]
                match precolor.[idx] with
                | Some color -> repPrecolored.[repIdx] <- Some color
                | None -> ()

        let repPreferences = Array.init n (fun _ -> bitsetEmpty wordCount)
        for (u, v) in preferencePairs do
            match tryIndexOf domain u, tryIndexOf domain v with
            | Some idxU, Some idxV when inGraph.[idxU] && inGraph.[idxV] ->
                let repU = repOfIndex.[idxU]
                let repV = repOfIndex.[idxV]
                if repU <> repV && repU >= 0 && repV >= 0 then
                    bitsetAddIndexInPlace repV repPreferences.[repU]
                    bitsetAddIndexInPlace repU repPreferences.[repV]
            | _ -> ()

        let repNeighbors = Array.init n (fun _ -> bitsetEmpty wordCount)
        for idx in 0 .. n - 1 do
            if inGraph.[idx] && parent.[idx] = idx then
                let repIdx = repIndexOfRoot.[idx]
                bitsetIterIndices neighbors.[idx] (fun nidx ->
                    if inGraph.[nidx] then
                        let rootN = rootOfIdx.[nidx]
                        if rootN <> idx then
                            let repN = repIndexOfRoot.[rootN]
                            if repIdx <> repN && repIdx >= 0 && repN >= 0 then
                                bitsetAddIndexInPlace repN repNeighbors.[repIdx]
                                bitsetAddIndexInPlace repIdx repNeighbors.[repN])

        let repGraph = { Domain = domain; Vertices = repVertices; Neighbors = repNeighbors }

        { Graph = repGraph
          RepOfIndex = repOfIndex
          RepMembers = repMembers
          Preferences = repPreferences
          Precolored = repPrecolored }

let private expandColoring (result: ColoringResult) (repMembers: BitSet array) : ColoringResult =
    let domain = result.Domain
    let n = domain.Ids.Length
    let expandedColors = Array.create n None
    let expandedSpills = bitsetEmpty domain.WordCount

    for repIdx in 0 .. n - 1 do
        match result.Colors.[repIdx] with
        | Some color ->
            bitsetIterIndices repMembers.[repIdx] (fun memberIdx ->
                expandedColors.[memberIdx] <- Some color)
        | None -> ()

    bitsetIterIndices result.Spills (fun repIdx ->
        bitsetUnionInPlace expandedSpills repMembers.[repIdx])

    { Domain = domain
      Colors = expandedColors
      Spills = expandedSpills
      ChromaticNumber = result.ChromaticNumber }

let private emptyColoringResult (domain: VRegDomain) : ColoringResult =
    { Domain = domain
      Colors = Array.create domain.Ids.Length None
      Spills = bitsetEmpty domain.WordCount
      ChromaticNumber = 0 }

/// Greedy color in reverse PEO order with phi coalescing preferences
/// For chordal graphs, this produces an optimal coloring.
/// When preferences are provided, try to use colors that match coalesced partners.
/// Uses two-pass approach: first color vregs with no uncolored phi partners,
/// then color deferred vregs (whose partners are now colored).
let greedyColorReverse
    (graph: InterferenceGraph)
    (peo: int list)
    (precolored: int option array)
    (numColors: int)
    (preferences: BitSet array)
    : ColoringResult =
    let domain = graph.Domain
    let n = domain.Ids.Length
    let wordCount = domain.WordCount
    let colors = Array.create n None
    let spills = bitsetEmpty wordCount
    let mutable maxColor = -1

    // Apply pre-colored vertices
    for idx in 0 .. n - 1 do
        match precolored.[idx] with
        | Some c ->
            colors.[idx] <- Some c
            if c > maxColor then maxColor <- c
        | None -> ()

    let inGraph = Array.create n false
    bitsetIterIndices graph.Vertices (fun idx -> inGraph.[idx] <- true)

    let peoIndices =
        peo
        |> List.map (fun v ->
            match tryIndexOf domain v with
            | Some idx -> idx
            | None -> Crash.crash $"Greedy coloring missing vertex {v}")

    let markUsedColors (idx: int) (used: bool array) : unit =
        bitsetIterIndices graph.Neighbors.[idx] (fun nidx ->
            if inGraph.[nidx] then
                match colors.[nidx] with
                | Some c when c >= 0 && c < numColors -> used.[c] <- true
                | _ -> ())

    let colorVertex (idx: int) : unit =
        if colors.[idx].IsNone then
            let used = Array.create numColors false
            markUsedColors idx used

            let mutable prefColor = None
            bitsetIterIndices preferences.[idx] (fun pidx ->
                match colors.[pidx] with
                | Some c when prefColor.IsNone && c >= 0 && c < numColors && not used.[c] ->
                    prefColor <- Some c
                | _ -> ())

            let assignColor (c: int) =
                colors.[idx] <- Some c
                if c > maxColor then maxColor <- c

            match prefColor with
            | Some c -> assignColor c
            | None ->
                let mutable assigned = false
                for c in 0 .. numColors - 1 do
                    if not assigned && not used.[c] then
                        assignColor c
                        assigned <- true
                if not assigned then
                    bitsetAddIndexInPlace idx spills

    let hasUncoloredPartners (idx: int) : bool =
        let mutable found = false
        bitsetIterIndices preferences.[idx] (fun pidx ->
            if not found && inGraph.[pidx] && colors.[pidx].IsNone then
                found <- true)
        found

    let interferes (idx1: int) (idx2: int) : bool =
        bitsetContainsIndex idx2 graph.Neighbors.[idx1]

    let colorVertexWithPartners (idx: int) : unit =
        if colors.[idx].IsNone then
            let candidates =
                let mutable acc = []
                bitsetIterIndices preferences.[idx] (fun pidx ->
                    if inGraph.[pidx] && colors.[pidx].IsNone && not (interferes idx pidx) then
                        acc <- pidx :: acc)
                List.rev acc

            let rec filterMutuallyCompatible (acc: int list) (remaining: int list) =
                match remaining with
                | [] -> List.rev acc
                | p :: rest ->
                    let compatible = acc |> List.forall (fun a -> not (interferes p a))
                    if compatible then
                        filterMutuallyCompatible (p :: acc) rest
                    else
                        filterMutuallyCompatible acc rest

            let coalesceable = filterMutuallyCompatible [] candidates
            let allVertices = idx :: coalesceable
            let used = Array.create numColors false
            for vertex in allVertices do
                markUsedColors vertex used

            let mutable assigned = false
            for c in 0 .. numColors - 1 do
                if not assigned && not used.[c] then
                    for vertex in allVertices do
                        if colors.[vertex].IsNone then
                            colors.[vertex] <- Some c
                    if c > maxColor then maxColor <- c
                    assigned <- true

            if not assigned then
                bitsetAddIndexInPlace idx spills

    let deferred = bitsetEmpty wordCount
    for idx in List.rev peoIndices do
        if colors.[idx].IsNone then
            if hasUncoloredPartners idx then
                bitsetAddIndexInPlace idx deferred
            else
                colorVertex idx

    for idx in List.rev peoIndices do
        if bitsetContainsIndex idx deferred && colors.[idx].IsNone then
            colorVertexWithPartners idx

    { Domain = domain
      Colors = colors
      Spills = spills
      ChromaticNumber = if maxColor < 0 then 0 else maxColor + 1 }

/// Main chordal graph coloring function with phi coalescing preferences
let chordalGraphColor
    (graph: InterferenceGraph)
    (precoloredPairs: (int * int) list)
    (numColors: int)
    (preferencePairs: (int * int) list)
    (movePairs: (int * int) list)
    : ColoringResult =
    if bitsetIsEmpty graph.Vertices then
        emptyColoringResult graph.Domain
    else
        let coalesced = coalesceGraphFast graph precoloredPairs movePairs preferencePairs
        let peo = maximumCardinalitySearch coalesced.Graph
        let result = greedyColorReverse coalesced.Graph peo coalesced.Precolored numColors coalesced.Preferences
        expandColoring result coalesced.RepMembers

let private chordalGraphColorWithTiming
    (sw: System.Diagnostics.Stopwatch)
    (graph: InterferenceGraph)
    (precoloredPairs: (int * int) list)
    (numColors: int)
    (preferencePairs: (int * int) list)
    (movePairs: (int * int) list)
    : ColoringResult * ChordalColoringTiming =
    if bitsetIsEmpty graph.Vertices then
        (emptyColoringResult graph.Domain,
         { CoalesceMs = 0.0
           McsSelectMs = 0.0
           McsUpdateMs = 0.0
           GreedyMs = 0.0
           ExpandMs = 0.0 })
    else
        let timePhase (f: unit -> 'a) : 'a * float =
            let start = sw.Elapsed.TotalMilliseconds
            let result = f ()
            let elapsedMs = sw.Elapsed.TotalMilliseconds - start
            (result, elapsedMs)

        let (coalesced, coalesceMs) =
            timePhase (fun () -> coalesceGraphFast graph precoloredPairs movePairs preferencePairs)
        let (peo, mcsTiming) = maximumCardinalitySearchWithTiming coalesced.Graph sw
        let (result, greedyMs) =
            timePhase (fun () ->
                greedyColorReverse coalesced.Graph peo coalesced.Precolored numColors coalesced.Preferences)
        let (expanded, expandMs) =
            timePhase (fun () -> expandColoring result coalesced.RepMembers)

        let timing = {
            CoalesceMs = coalesceMs
            McsSelectMs = mcsTiming.SelectMs
            McsUpdateMs = mcsTiming.UpdateMs
            GreedyMs = greedyMs
            ExpandMs = expandMs
        }
        (expanded, timing)

/// Convert chordal graph coloring result to allocation result
/// Colors map to physical registers, spills map to stack slots
let coloringToAllocation (colorResult: ColoringResult) (registers: LIR.PhysReg list) : AllocationResult =
    let domain = colorResult.Domain
    let n = domain.Ids.Length
    let allocations = Array.create n None
    let mutable nextStackSlot = -8
    let mutable usedCalleeSaved : LIR.PhysReg list = []

    // Map colored vertices to physical registers
    for idx in 0 .. n - 1 do
        match colorResult.Colors.[idx] with
        | Some color ->
            if color < List.length registers then
                let reg = List.item color registers
                allocations.[idx] <- Some (PhysReg reg)
                // Track callee-saved register usage
                if List.contains reg [LIR.X19; LIR.X20; LIR.X21; LIR.X22; LIR.X23; LIR.X24; LIR.X25; LIR.X26] then
                    if not (List.contains reg usedCalleeSaved) then
                        usedCalleeSaved <- reg :: usedCalleeSaved
            else
                // Color out of range - treat as spill
                allocations.[idx] <- Some (StackSlot nextStackSlot)
                nextStackSlot <- nextStackSlot - 8
        | None -> ()

    // Map spilled vertices to stack slots
    bitsetIterIndices colorResult.Spills (fun idx ->
        if allocations.[idx].IsNone then
            allocations.[idx] <- Some (StackSlot nextStackSlot)
            nextStackSlot <- nextStackSlot - 8)

    // Compute 16-byte aligned stack size
    let stackSize =
        if nextStackSlot = -8 then 0
        else ((abs nextStackSlot + 15) / 16) * 16

    { Domain = domain
      Allocations = allocations
      StackSize = stackSize
      UsedCalleeSaved = usedCalleeSaved |> List.sort }

// ============================================================================
// Float Register Allocation
// ============================================================================

/// Float caller-saved registers (D0-D7)
let floatCallerSavedRegs : LIR.PhysFPReg list = [
    LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7
]

/// Float callee-saved registers (D8-D14). D15 (XMM15) is RESERVED as the float
/// scratch for breaking cycles in x64 FArgMoves parallel moves (analogous to the
/// integer R11 scratch, and to ARM64's D16). It must not be allocatable, or a cyclic
/// float-arg shuffle (e.g. swapping the two args of a 2-arg float call) corrupts a
/// live value. Keeping it out of this list removes it from allocatableFloatRegs and
/// from the callee-saved save/restore set.
let floatCalleeSavedRegs : LIR.PhysFPReg list = [
    LIR.D8; LIR.D9; LIR.D10; LIR.D11; LIR.D12; LIR.D13; LIR.D14
]

/// All allocatable float registers - caller-saved first, then callee-saved
let allocatableFloatRegs : LIR.PhysFPReg list = floatCallerSavedRegs @ floatCalleeSavedRegs

/// Float allocation result
type FAllocationResult = {
    Domain: VRegDomain
    Allocations: LIR.PhysFPReg option array
    UsedCalleeSavedF: LIR.PhysFPReg list
}

/// Convert physical FP register to integer for graph coloring
let physFPRegToInt (reg: LIR.PhysFPReg) : int =
    match reg with
    | LIR.D0 -> 0 | LIR.D1 -> 1 | LIR.D2 -> 2 | LIR.D3 -> 3
    | LIR.D4 -> 4 | LIR.D5 -> 5 | LIR.D6 -> 6 | LIR.D7 -> 7
    | LIR.D8 -> 8 | LIR.D9 -> 9 | LIR.D10 -> 10 | LIR.D11 -> 11
    | LIR.D12 -> 12 | LIR.D13 -> 13 | LIR.D14 -> 14 | LIR.D15 -> 15

/// Convert float coloring result to allocation
let floatColoringToAllocation (colorResult: ColoringResult) (registers: LIR.PhysFPReg list) : FAllocationResult =
    let domain = colorResult.Domain
    let n = domain.Ids.Length
    let allocations = Array.create n None
    let mutable usedCalleeSaved : LIR.PhysFPReg list = []

    for idx in 0 .. n - 1 do
        match colorResult.Colors.[idx] with
        | Some color ->
            if color < List.length registers then
                let reg = List.item color registers
                allocations.[idx] <- Some reg
                if List.contains reg floatCalleeSavedRegs && not (List.contains reg usedCalleeSaved) then
                    usedCalleeSaved <- reg :: usedCalleeSaved
        | None -> ()

    { Domain = domain
      Allocations = allocations
      UsedCalleeSavedF = usedCalleeSaved |> List.sort }

/// Run chordal graph coloring for float register allocation
/// additionalVRegs: FVirtual IDs that must be allocated (e.g., float parameters)
/// even if they don't appear in the CFG instructions
let private chordalFloatAllocationWithLiveness
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (additionalVRegs: BitSet)
    (domain: VRegDomain)
    (livenessBits: BlockLiveness array)
    : FAllocationResult =
    let graph = buildFloatInterferenceGraphBitsetWithLiveness blockIndex blocks domain livenessBits additionalVRegs
    // Add additional VRegs (like float params) as isolated vertices if not already in graph
    let graphWithParams : InterferenceGraph =
        { graph with Vertices = bitsetUnion graph.Vertices additionalVRegs }
    if bitsetIsEmpty graphWithParams.Vertices then
        // No float registers used - return empty allocation
        { Domain = domain
          Allocations = Array.create domain.Ids.Length None
          UsedCalleeSavedF = [] }
    else
        // No preferences for floats for now (could add FPhi coalescing later)
        let colorResult = chordalGraphColor graphWithParams [] (List.length allocatableFloatRegs) [] []
        floatColoringToAllocation colorResult allocatableFloatRegs

/// Run chordal graph coloring for float register allocation
/// additionalVRegs: FVirtual IDs that must be allocated (e.g., float parameters)
/// even if they don't appear in the CFG instructions
let chordalFloatAllocation (cfg: LIR.CFG) (additionalVRegs: int list) : FAllocationResult =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let (domain, livenessBits) = computeFloatLivenessBitsRaw blockIndex blocks additionalVRegs
    let additionalBits = bitsetFromList domain additionalVRegs
    chordalFloatAllocationWithLiveness blockIndex blocks additionalBits domain livenessBits

/// Apply float allocation to an FReg, converting FVirtual to FPhysical
let applyFloatAllocationToFReg (floatAllocation: FAllocationResult) (freg: LIR.FReg) : LIR.FReg =
    match freg with
    | LIR.FPhysical _ -> freg  // Already physical
    | LIR.FVirtual 1000 -> freg  // Fixed temp - keep as is, CodeGen handles it
    | LIR.FVirtual 1001 -> freg  // Fixed temp
    | LIR.FVirtual 2000 -> freg  // Fixed temp
    | LIR.FVirtual n when n >= 3000 && n < 4000 -> freg  // Call arg temps - fixed
    | LIR.FVirtual id ->
        match tryIndexOf floatAllocation.Domain id with
        | Some idx ->
            match floatAllocation.Allocations.[idx] with
            | Some physReg -> LIR.FPhysical physReg
            | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"
        | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"

/// Apply float allocation to an instruction
let applyFloatAllocationToInstr (floatAllocation: FAllocationResult) (instr: LIR.Instr) : LIR.Instr =
    let applyF = applyFloatAllocationToFReg floatAllocation
    match instr with
    | LIR.FMov (dest, src) -> LIR.FMov (applyF dest, applyF src)
    | LIR.FAdd (dest, left, right) -> LIR.FAdd (applyF dest, applyF left, applyF right)
    | LIR.FSub (dest, left, right) -> LIR.FSub (applyF dest, applyF left, applyF right)
    | LIR.FMul (dest, left, right) -> LIR.FMul (applyF dest, applyF left, applyF right)
    | LIR.FDiv (dest, left, right) -> LIR.FDiv (applyF dest, applyF left, applyF right)
    | LIR.FNeg (dest, src) -> LIR.FNeg (applyF dest, applyF src)
    | LIR.FAbs (dest, src) -> LIR.FAbs (applyF dest, applyF src)
    | LIR.FSqrt (dest, src) -> LIR.FSqrt (applyF dest, applyF src)
    | LIR.FCmp (left, right) -> LIR.FCmp (applyF left, applyF right)
    | LIR.FLoad (dest, value) -> LIR.FLoad (applyF dest, value)
    | LIR.Int64ToFloat (dest, src) -> LIR.Int64ToFloat (applyF dest, src)
    | LIR.FloatToInt64 (dest, src) -> LIR.FloatToInt64 (dest, applyF src)
    | LIR.FloatToBits (dest, src) -> LIR.FloatToBits (dest, applyF src)
    | LIR.FpToGp (dest, src) -> LIR.FpToGp (dest, applyF src)
    | LIR.GpToFp (dest, src) -> LIR.GpToFp (applyF dest, src)
    | LIR.PrintFloat freg -> LIR.PrintFloat (applyF freg)
    | LIR.PrintFloatNoNewline freg -> LIR.PrintFloatNoNewline (applyF freg)
    | LIR.FPhi (dest, sources) ->
        LIR.FPhi (applyF dest, sources |> List.map (fun (src, label) -> (applyF src, label)))
    | LIR.FArgMoves moves ->
        LIR.FArgMoves (moves |> List.map (fun (physReg, src) -> (physReg, applyF src)))
    | LIR.FloatToString (dest, value) -> LIR.FloatToString (dest, applyF value)
    // HeapStore with float value: the Virtual register ID is shared with FVirtual
    // We need to apply float allocation to convert Virtual(n) to the allocated physical register
    | LIR.HeapStore (addr, offset, LIR.Reg (LIR.Virtual vregId), Some AST.TFloat64) ->
        // Convert Virtual to the allocated FPhysical if it's in the float mapping
        let allocatedFReg = applyF (LIR.FVirtual vregId)
        // Convert the FReg back to a Virtual/Physical Reg for HeapStore
        let allocatedReg =
            match allocatedFReg with
            | LIR.FPhysical physFReg ->
                // Convert physical float reg to physical GP reg (for HeapStore operand format)
                // The actual STR_fp instruction will be generated in CodeGen based on valueType
                let physReg =
                    match physFReg with
                    | LIR.D0 -> LIR.X0 | LIR.D1 -> LIR.X1 | LIR.D2 -> LIR.X2 | LIR.D3 -> LIR.X3
                    | LIR.D4 -> LIR.X4 | LIR.D5 -> LIR.X5 | LIR.D6 -> LIR.X6 | LIR.D7 -> LIR.X7
                    | LIR.D8 -> LIR.X8 | LIR.D9 -> LIR.X9 | LIR.D10 -> LIR.X10 | LIR.D11 -> LIR.X11
                    | LIR.D12 -> LIR.X12 | LIR.D13 -> LIR.X13 | LIR.D14 -> LIR.X14 | LIR.D15 -> LIR.X15
                LIR.Physical physReg
            | LIR.FVirtual n -> LIR.Virtual n
        LIR.HeapStore (addr, offset, LIR.Reg allocatedReg, Some AST.TFloat64)
    | _ -> instr  // Non-float instructions unchanged

/// Apply float allocation to a basic block
let applyFloatAllocationToBlock (floatAllocation: FAllocationResult) (block: LIR.BasicBlock) : LIR.BasicBlock =
    { block with Instrs = block.Instrs |> List.map (applyFloatAllocationToInstr floatAllocation) }

/// Apply float allocation to basic blocks
let applyFloatAllocationToBlocks
    (floatAllocation: FAllocationResult)
    (blocks: LIR.BasicBlock array)
    : LIR.BasicBlock array =
    blocks |> Array.map (applyFloatAllocationToBlock floatAllocation)

/// Apply float allocation to a CFG
let applyFloatAllocationToCFG (floatAllocation: FAllocationResult) (cfg: LIR.CFG) : LIR.CFG =
    let (blockIndex, blocks) = buildBlockIndex cfg
    let updatedBlocks = applyFloatAllocationToBlocks floatAllocation blocks
    { cfg with Blocks = blocksToMap blockIndex updatedBlocks }

// ============================================================================
// Linear Scan Register Allocation (kept for reference, not used)
// ============================================================================

let private tryAllocation (allocation: AllocationResult) (vregId: int) : Allocation option =
    match tryIndexOf allocation.Domain vregId with
    | Some idx -> allocation.Allocations.[idx]
    | None -> None

let private tryFloatAllocation (floatAllocation: FAllocationResult) (fvregId: int) : LIR.PhysFPReg option =
    match tryIndexOf floatAllocation.Domain fvregId with
    | Some idx -> floatAllocation.Allocations.[idx]
    | None -> None

/// Get the caller-saved physical registers that contain live values
let getLiveCallerSavedRegs (allocation: AllocationResult) (liveVRegs: BitSet) : LIR.PhysReg list =
    let mutable regs = []
    bitsetIter allocation.Domain liveVRegs (fun vregId ->
        match tryAllocation allocation vregId with
        | Some (PhysReg reg) when List.contains reg callerSavedRegs ->
            if not (List.contains reg regs) then
                regs <- reg :: regs
        | _ -> ())
    regs |> List.sort  // Keep consistent order for deterministic output

/// Get the caller-saved physical float registers that contain live values
let getLiveCallerSavedFloatRegs
    (liveFVRegs: BitSet)
    (floatAllocation: FAllocationResult)
    : LIR.PhysFPReg list =
    let mutable regs = []
    bitsetIter floatAllocation.Domain liveFVRegs (fun vregId ->
        match tryFloatAllocation floatAllocation vregId with
        | Some reg when List.contains reg floatCallerSavedRegs ->
            if not (List.contains reg regs) then
                regs <- reg :: regs
        | _ -> ())
    regs |> List.sort

// ============================================================================
// Apply Allocation to LIR
// ============================================================================

/// Apply allocation to a register, returning the physical register and allocation info
let applyToReg (allocation: AllocationResult) (reg: LIR.Reg) : LIR.Reg * Allocation option =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, None)
    | LIR.Virtual id ->
        match tryAllocation allocation id with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, None)
        | Some (StackSlot offset) -> (LIR.Physical LIR.X11, Some (StackSlot offset))
        | None -> (LIR.Physical LIR.X11, None)

/// Apply allocation to an operand, returning load instructions if needed
let applyToOperand (allocation: AllocationResult) (operand: LIR.Operand) (tempReg: LIR.PhysReg)
    : LIR.Operand * LIR.Instr list =
    match operand with
    | LIR.Imm n -> (LIR.Imm n, [])
    | LIR.FloatImm f -> (LIR.FloatImm f, [])
    | LIR.StringSymbol value -> (LIR.StringSymbol value, [])
    | LIR.FloatSymbol value -> (LIR.FloatSymbol value, [])
    | LIR.StackSlot s -> (LIR.StackSlot s, [])
    | LIR.Reg reg ->
        match reg with
        | LIR.Physical p -> (LIR.Reg (LIR.Physical p), [])
        | LIR.Virtual id ->
            match tryAllocation allocation id with
            | Some (PhysReg physReg) -> (LIR.Reg (LIR.Physical physReg), [])
            | Some (StackSlot offset) ->
                let loadInstr = LIR.Mov (LIR.Physical tempReg, LIR.StackSlot offset)
                (LIR.Reg (LIR.Physical tempReg), [loadInstr])
            // Keep Virtual unchanged if not in integer mapping - it may be a float register
            // that will be handled by float allocation later
            | None -> (LIR.Reg (LIR.Virtual id), [])
    | LIR.FuncAddr name -> (LIR.FuncAddr name, [])

/// Apply allocation to an operand WITHOUT generating load instructions for spills.
/// Returns StackSlot for spilled values so CodeGen can load them at the right time.
/// Used for TailArgMoves where loads must be deferred to avoid using the same temp register.
let applyToOperandNoLoad (allocation: AllocationResult) (operand: LIR.Operand) : LIR.Operand =
    match operand with
    | LIR.Imm n -> LIR.Imm n
    | LIR.FloatImm f -> LIR.FloatImm f
    | LIR.StringSymbol value -> LIR.StringSymbol value
    | LIR.FloatSymbol value -> LIR.FloatSymbol value
    | LIR.StackSlot s -> LIR.StackSlot s
    | LIR.Reg reg ->
        match reg with
        | LIR.Physical p -> LIR.Reg (LIR.Physical p)
        | LIR.Virtual id ->
            match tryAllocation allocation id with
            | Some (PhysReg physReg) -> LIR.Reg (LIR.Physical physReg)
            | Some (StackSlot offset) -> LIR.StackSlot offset
            // Keep Virtual unchanged if not in integer mapping - it may be a float register
            | None -> LIR.Reg (LIR.Virtual id)
    | LIR.FuncAddr name -> LIR.FuncAddr name

/// Helper to load a spilled register
let loadSpilled (allocation: AllocationResult) (reg: LIR.Reg) (tempReg: LIR.PhysReg)
    : LIR.Reg * LIR.Instr list =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, [])
    | LIR.Virtual id ->
        match tryAllocation allocation id with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, [])
        | Some (StackSlot offset) ->
            let loadInstr = LIR.Mov (LIR.Physical tempReg, LIR.StackSlot offset)
            (LIR.Physical tempReg, [loadInstr])
        | None -> (LIR.Physical tempReg, [])

/// On x86_64, X8-X17 all alias to R11 (scratch). Using X12 and X13 as distinct
/// scratch registers for loading two spilled operands simultaneously will clobber
/// the first load when the second executes. On x86_64, the second operand of binary
/// ops uses applyToOperandNoLoad to keep it as a StackSlot, and the codegen handles
/// loading it into R11 after the first operand has been moved to the destination.
let private isX86_64 (arch: Platform.Arch) =
    match arch with Platform.X86_64 -> true | Platform.ARM64 -> false

/// On x86_64, when loading two spilled Reg-typed operands, the first must go to a
/// register that won't be clobbered by the second load (into X12=R11). This function
/// picks a safe register by checking what physical register the right operand uses.
/// If both left and right are spilled to stack, loads left into dest register
/// (unless dest conflicts with right's allocated register).
let private loadSpilledPair (arch: Platform.Arch) (mapping: AllocationResult) (left: LIR.Reg) (right: LIR.Reg) (destReg: LIR.Reg)
    : (LIR.Reg * LIR.Instr list) * (LIR.Reg * LIR.Instr list) =
    if not (isX86_64 arch) then
        (loadSpilled mapping left LIR.X12, loadSpilled mapping right LIR.X13)
    else
        // Check if left is actually spilled (StackSlot)
        let leftIsSpilled =
            match left with
            | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
            | _ -> false
        let rightIsSpilled =
            match right with
            | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
            | _ -> false
        if leftIsSpilled && rightIsSpilled then
            // Both spilled: load left into dest, right into X12
            let destPhys = match destReg with LIR.Physical p -> p | _ -> LIR.X12
            // Check that dest doesn't also alias R11
            let leftTemp =
                if destPhys <> LIR.X11 && destPhys <> LIR.X8 && destPhys <> LIR.X9 && destPhys <> LIR.X10
                   && destPhys <> LIR.X12 && destPhys <> LIR.X13 && destPhys <> LIR.X14
                   && destPhys <> LIR.X15 && destPhys <> LIR.X16 && destPhys <> LIR.X17
                then destPhys
                else LIR.X12  // fallback - both will be R11, but this is rare
            (loadSpilled mapping left leftTemp, loadSpilled mapping right LIR.X12)
        elif leftIsSpilled then
            // Only left spilled: check if right occupies the same register as X12
            let rightPhys =
                match right with
                | LIR.Physical p -> Some p
                | LIR.Virtual id -> match tryAllocation mapping id with Some (PhysReg p) -> Some p | _ -> None
            // If right is in a real register, use X12 for left (no conflict with right)
            (loadSpilled mapping left LIR.X12, loadSpilled mapping right LIR.X12)
        else
            // Right spilled or neither: use X12 for left, X12 for right (OK since left isn't spilled)
            (loadSpilled mapping left LIR.X12, loadSpilled mapping right LIR.X12)

/// Apply allocation to an instruction
let applyToInstr (arch: Platform.Arch) (mapping: AllocationResult) (instr: LIR.Instr) : LIR.Instr list =
    match instr with
    | LIR.Phi _ ->
        // Phi nodes are handled specially by resolvePhiNodes after allocation.
        // Skip them here - they will be removed and converted to moves at predecessor exits.
        []

    | LIR.FPhi _ ->
        // Float phi nodes are handled specially by resolvePhiNodes after allocation.
        // Skip them here - they will be removed and converted to FMov at predecessor exits.
        []

    | LIR.Mov (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcOp, srcLoads) = applyToOperand mapping src LIR.X12
        let movInstr = LIR.Mov (destReg, srcOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [movInstr] @ storeInstrs

    | LIR.Store (offset, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.Store (offset, srcReg)]

    | LIR.Add (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        let addInstr = LIR.Add (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [addInstr] @ storeInstrs

    | LIR.Sub (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        let subInstr = LIR.Sub (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [subInstr] @ storeInstrs

    | LIR.Mul (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let mulInstr = LIR.Mul (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [mulInstr] @ storeInstrs

    | LIR.Sdiv (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let divInstr = LIR.Sdiv (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [divInstr] @ storeInstrs

    | LIR.Udiv (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let divInstr = LIR.Udiv (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [divInstr] @ storeInstrs

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        if isX86_64 arch then
            // On x86_64, X12/X13/X14 all alias R11. Use loadSpilledPair for mul
            // operands (uses dest as safe temp), and X3 (RCX) for sub if it would
            // conflict with mulRight in R11.
            let ((mulLeftReg, mulLeftLoads), (mulRightReg, mulRightLoads)) =
                loadSpilledPair arch mapping mulLeft mulRight destReg
            let subIsSpilled =
                match sub with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let mulRightIsR11 = (mulRightReg = LIR.Physical LIR.X12 || mulRightReg = LIR.Physical LIR.X11)
            let subTemp = if subIsSpilled && mulRightIsR11 then LIR.X3 else LIR.X12
            let (subReg, subLoads) = loadSpilled mapping sub subTemp
            let msubInstr = LIR.Msub (destReg, mulLeftReg, mulRightReg, subReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs
        else
            let (mulLeftReg, mulLeftLoads) = loadSpilled mapping mulLeft LIR.X12
            let (mulRightReg, mulRightLoads) = loadSpilled mapping mulRight LIR.X13
            let (subReg, subLoads) = loadSpilled mapping sub LIR.X14
            let msubInstr = LIR.Msub (destReg, mulLeftReg, mulRightReg, subReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs

    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        if isX86_64 arch then
            let ((mulLeftReg, mulLeftLoads), (mulRightReg, mulRightLoads)) =
                loadSpilledPair arch mapping mulLeft mulRight destReg
            let addIsSpilled =
                match add with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let mulRightIsR11 = (mulRightReg = LIR.Physical LIR.X12 || mulRightReg = LIR.Physical LIR.X11)
            let addTemp = if addIsSpilled && mulRightIsR11 then LIR.X3 else LIR.X12
            let (addReg, addLoads) = loadSpilled mapping add addTemp
            let maddInstr = LIR.Madd (destReg, mulLeftReg, mulRightReg, addReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            mulLeftLoads @ mulRightLoads @ addLoads @ [maddInstr] @ storeInstrs
        else
            let (mulLeftReg, mulLeftLoads) = loadSpilled mapping mulLeft LIR.X12
            let (mulRightReg, mulRightLoads) = loadSpilled mapping mulRight LIR.X13
            let (addReg, addLoads) = loadSpilled mapping add LIR.X14
            let maddInstr = LIR.Madd (destReg, mulLeftReg, mulRightReg, addReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            mulLeftLoads @ mulRightLoads @ addLoads @ [maddInstr] @ storeInstrs

    | LIR.Cmp (left, right) ->
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        leftLoads @ rightLoads @ [LIR.Cmp (leftReg, rightOp)]

    | LIR.Cset (dest, cond) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let csetInstr = LIR.Cset (destReg, cond)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [csetInstr] @ storeInstrs

    | LIR.And (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let andInstr = LIR.And (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [andInstr] @ storeInstrs

    | LIR.And_imm (dest, src, imm) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let andInstr = LIR.And_imm (destReg, srcReg, imm)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [andInstr] @ storeInstrs

    | LIR.Orr (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let orrInstr = LIR.Orr (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [orrInstr] @ storeInstrs

    | LIR.Eor (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let eorInstr = LIR.Eor (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [eorInstr] @ storeInstrs

    | LIR.Lsl (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = loadSpilledPair arch mapping src shift destReg
        let lslInstr = LIR.Lsl (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lslInstr] @ storeInstrs

    | LIR.Lsr (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = loadSpilledPair arch mapping src shift destReg
        let lsrInstr = LIR.Lsr (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lsrInstr] @ storeInstrs

    | LIR.Lsl_imm (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let lslInstr = LIR.Lsl_imm (destReg, srcReg, shift)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [lslInstr] @ storeInstrs

    | LIR.Lsr_imm (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let lsrInstr = LIR.Lsr_imm (destReg, srcReg, shift)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [lsrInstr] @ storeInstrs

    | LIR.Mvn (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let mvnInstr = LIR.Mvn (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [mvnInstr] @ storeInstrs

    // Sign/zero extension instructions (for integer overflow)
    | LIR.Sxtb (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Sxtb (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Sxth (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Sxth (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Sxtw (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Sxtw (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Uxtb (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Uxtb (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Uxth (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Uxth (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Uxtw (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Uxtw (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Call (dest, funcName, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then
                    // On x86_64, X12/X13 both map to R11. Use applyToOperandNoLoad
                    // to keep spilled args as StackSlots - ArgMoves handles loading them.
                    (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.Call (destReg, funcName, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        argLoads @ [callInstr] @ storeInstrs

    | LIR.TailCall (funcName, args) ->
        // Tail calls have no destination - just apply allocation to args
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.TailCall (funcName, argOps)
        argLoads @ [callInstr]

    | LIR.IndirectCall (dest, func, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (funcReg, funcLoads) = loadSpilled mapping func LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.IndirectCall (destReg, funcReg, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        funcLoads @ argLoads @ [callInstr] @ storeInstrs

    | LIR.IndirectTailCall (func, args) ->
        // Indirect tail calls have no destination
        let (funcReg, funcLoads) = loadSpilled mapping func LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.IndirectTailCall (funcReg, argOps)
        funcLoads @ argLoads @ [callInstr]

    | LIR.ClosureAlloc (dest, funcName, captures) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocatedCaptures =
            captures |> List.mapi (fun i cap ->
                if isX86_64 arch then (applyToOperandNoLoad mapping cap, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping cap tempReg
            )
        let capLoads = allocatedCaptures |> List.collect snd
        let capOps = allocatedCaptures |> List.map fst
        let allocInstr = LIR.ClosureAlloc (destReg, funcName, capOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        capLoads @ [allocInstr] @ storeInstrs

    | LIR.ClosureCall (dest, closure, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (closureReg, closureLoads) = loadSpilled mapping closure LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.ClosureCall (destReg, closureReg, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        closureLoads @ argLoads @ [callInstr] @ storeInstrs

    | LIR.ClosureTailCall (closure, args) ->
        // Closure tail calls have no destination
        let (closureReg, closureLoads) = loadSpilled mapping closure LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.ClosureTailCall (closureReg, argOps)
        closureLoads @ argLoads @ [callInstr]

    // SaveRegs/RestoreRegs are handled specially in applyToBlockWithLiveness
    // These patterns handle the case where they've already been populated
    | LIR.SaveRegs (intRegs, floatRegs) -> [LIR.SaveRegs (intRegs, floatRegs)]
    | LIR.RestoreRegs (intRegs, floatRegs) -> [LIR.RestoreRegs (intRegs, floatRegs)]

    | LIR.ArgMoves moves ->
        // ArgMoves must preserve distinct sources for each argument.
        // Use no-load allocation so spilled values remain StackSlot and are
        // loaded per-move in CodeGen (avoids reusing a single temp).
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                let allocatedOp = applyToOperandNoLoad mapping srcOp
                (destReg, allocatedOp))
        [LIR.ArgMoves allocatedMoves]

    | LIR.TailArgMoves moves ->
        // Apply allocation WITHOUT loading spilled values into a temp register.
        // This is different from ArgMoves: for tail calls, we can't use a shared temp
        // because there's no SaveRegs to preserve values. CodeGen will handle StackSlots
        // by loading them directly into the destination register.
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                (destReg, applyToOperandNoLoad mapping srcOp))
        [LIR.TailArgMoves allocatedMoves]

    | LIR.FArgMoves moves ->
        // Pass through unchanged for now - float argument moves use physical registers only
        [LIR.FArgMoves moves]

    | LIR.PrintInt64 reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintInt64 regFinal]

    | LIR.PrintBool reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBool regFinal]

    | LIR.PrintInt64NoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintInt64NoNewline regFinal]

    | LIR.PrintBoolNoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBoolNoNewline regFinal]

    | LIR.PrintFloatNoNewline freg -> [LIR.PrintFloatNoNewline freg]

    | LIR.PrintHeapStringNoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintHeapStringNoNewline regFinal]

    | LIR.PrintList (listPtr, elemType) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping listPtr LIR.X12
        ptrLoads @ [LIR.PrintList (ptrFinal, elemType)]

    | LIR.PrintSum (sumPtr, variants) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping sumPtr LIR.X12
        ptrLoads @ [LIR.PrintSum (ptrFinal, variants)]

    | LIR.PrintRecord (recordPtr, typeName, fields) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping recordPtr LIR.X12
        ptrLoads @ [LIR.PrintRecord (ptrFinal, typeName, fields)]

    | LIR.PrintFloat freg -> [LIR.PrintFloat freg]
    | LIR.PrintString value -> [LIR.PrintString value]
    | LIR.RuntimeError message -> [LIR.RuntimeError message]
    | LIR.PrintChars chars -> [LIR.PrintChars chars]
    | LIR.PrintBytes reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBytes regFinal]

    // FP instructions pass through unchanged
    | LIR.FMov (dest, src) -> [LIR.FMov (dest, src)]
    | LIR.FLoad (dest, value) -> [LIR.FLoad (dest, value)]
    | LIR.FAdd (dest, left, right) -> [LIR.FAdd (dest, left, right)]
    | LIR.FSub (dest, left, right) -> [LIR.FSub (dest, left, right)]
    | LIR.FMul (dest, left, right) -> [LIR.FMul (dest, left, right)]
    | LIR.FDiv (dest, left, right) -> [LIR.FDiv (dest, left, right)]
    | LIR.FNeg (dest, src) -> [LIR.FNeg (dest, src)]
    | LIR.FAbs (dest, src) -> [LIR.FAbs (dest, src)]
    | LIR.FSqrt (dest, src) -> [LIR.FSqrt (dest, src)]
    | LIR.FCmp (left, right) -> [LIR.FCmp (left, right)]
    // Int64ToFloat: src is integer register, dest is FP register
    | LIR.Int64ToFloat (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.Int64ToFloat (dest, srcReg)]
    // GpToFp: move bits from GP register to FP register (src is integer, dest is FP)
    | LIR.GpToFp (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.GpToFp (dest, srcReg)]
    // FloatToInt64: src is FP register, dest is integer register
    | LIR.FloatToInt64 (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIR.FloatToInt64 (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // FpToGp: src is FP register, dest is integer register
    | LIR.FpToGp (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIR.FpToGp (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // FloatToBits: src is FP register, dest is integer register (bit copy)
    | LIR.FloatToBits (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIR.FloatToBits (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // Heap operations
    | LIR.HeapAlloc (dest, size) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocInstr = LIR.HeapAlloc (destReg, size)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [allocInstr] @ storeInstrs

    | LIR.HeapStore (addr, offset, src, vt) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let (srcOp, srcLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping src, [])
            else applyToOperand mapping src LIR.X13
        addrLoads @ srcLoads @ [LIR.HeapStore (addrReg, offset, srcOp, vt)]

    | LIR.HeapLoad (dest, addr, offset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let loadInstr = LIR.HeapLoad (destReg, addrReg, offset)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        addrLoads @ [loadInstr] @ storeInstrs

    | LIR.RefCountInc (addr, payloadSize, kind) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIR.RefCountInc (addrReg, payloadSize, kind)]

    | LIR.RefCountDec (addr, payloadSize, kind) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIR.RefCountDec (addrReg, payloadSize, kind)]

    | LIR.StringConcat (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftOp, leftLoads) = applyToOperand mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        let concatInstr = LIR.StringConcat (destReg, leftOp, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [concatInstr] @ storeInstrs

    | LIR.PrintHeapString reg ->
        let (regPhys, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintHeapString regPhys]

    | LIR.LoadFuncAddr (dest, funcName) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let loadInstr = LIR.LoadFuncAddr (destReg, funcName)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [loadInstr] @ storeInstrs

    | LIR.FileReadText (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileReadText (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileExists (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileExists (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileWriteText (dest, path, content) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (contentOp, contentLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping content, [])
            else applyToOperand mapping content LIR.X13
        let fileInstr = LIR.FileWriteText (destReg, pathOp, contentOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ contentLoads @ [fileInstr] @ storeInstrs

    | LIR.FileAppendText (dest, path, content) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (contentOp, contentLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping content, [])
            else applyToOperand mapping content LIR.X13
        let fileInstr = LIR.FileAppendText (destReg, pathOp, contentOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ contentLoads @ [fileInstr] @ storeInstrs

    | LIR.FileDelete (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileDelete (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileSetExecutable (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileSetExecutable (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        if isX86_64 arch then
            let ((ptrReg, ptrLoads), (lengthReg, lengthLoads)) =
                loadSpilledPair arch mapping ptr length destReg
            let fileInstr = LIR.FileWriteFromPtr (destReg, pathOp, ptrReg, lengthReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            pathLoads @ ptrLoads @ lengthLoads @ [fileInstr] @ storeInstrs
        else
            let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X13
            let (lengthReg, lengthLoads) = loadSpilled mapping length LIR.X14
            let fileInstr = LIR.FileWriteFromPtr (destReg, pathOp, ptrReg, lengthReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            pathLoads @ ptrLoads @ lengthLoads @ [fileInstr] @ storeInstrs

    | LIR.RawAlloc (dest, numBytes) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (numBytesReg, numBytesLoads) = loadSpilled mapping numBytes LIR.X12
        let allocInstr = LIR.RawAlloc (destReg, numBytesReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        numBytesLoads @ [allocInstr] @ storeInstrs

    | LIR.RawFree ptr ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        ptrLoads @ [LIR.RawFree ptrReg]

    | LIR.RawGet (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((ptrReg, ptrLoads), (offsetReg, offsetLoads)) = loadSpilledPair arch mapping ptr byteOffset destReg
        let getInstr = LIR.RawGet (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((ptrReg, ptrLoads), (offsetReg, offsetLoads)) = loadSpilledPair arch mapping ptr byteOffset destReg
        let getInstr = LIR.RawGetByte (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIR.RawSet (ptr, byteOffset, value, valueType) ->
        if isX86_64 arch then
            // On x86_64, X12/X13/X14 all alias R11. When both ptr and value are
            // spilled, loading both into R11 clobbers one. Save X3 (RCX) via
            // push/pop and use it as a non-R11 temp for ptr. The codegen already
            // handles ptr=RCX when value=R11(scratch).
            let ptrSpilled =
                match ptr with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let valueSpilled =
                match value with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            if ptrSpilled && valueSpilled then
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X3
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                [LIR.SaveRegs ([LIR.X3], [])]
                @ ptrLoads @ offsetLoads @ valueLoads
                @ [LIR.RawSet (ptrReg, offsetReg, valueReg, valueType)]
                @ [LIR.RestoreRegs ([LIR.X3], [])]
            else
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSet (ptrReg, offsetReg, valueReg, valueType)]
        else
            let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
            let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
            let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
            ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSet (ptrReg, offsetReg, valueReg, valueType)]

    | LIR.RawSetByte (ptr, byteOffset, value) ->
        if isX86_64 arch then
            let ptrSpilled =
                match ptr with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let valueSpilled =
                match value with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            if ptrSpilled && valueSpilled then
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X3
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                [LIR.SaveRegs ([LIR.X3], [])]
                @ ptrLoads @ offsetLoads @ valueLoads
                @ [LIR.RawSetByte (ptrReg, offsetReg, valueReg)]
                @ [LIR.RestoreRegs ([LIR.X3], [])]
            else
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSetByte (ptrReg, offsetReg, valueReg)]
        else
            let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
            let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
            let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
            ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSetByte (ptrReg, offsetReg, valueReg)]

    | LIR.RefCountIncString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIR.RefCountIncString strOp]

    | LIR.RefCountDecString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIR.RefCountDecString strOp]

    | LIR.RandomInt64 dest ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let randomInstr = LIR.RandomInt64 destReg
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [randomInstr] @ storeInstrs

    | LIR.DateNow dest ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let dateInstr = LIR.DateNow destReg
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [dateInstr] @ storeInstrs

    | LIR.FloatToString (dest, value) ->
        // FP register value is already physical after float allocation
        let (destReg, destAlloc) = applyToReg mapping dest
        let floatToStrInstr = LIR.FloatToString (destReg, value)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [floatToStrInstr] @ storeInstrs

    | LIR.CoverageHit exprId ->
        [LIR.CoverageHit exprId]  // No registers to allocate

    | LIR.Exit -> [LIR.Exit]

/// Apply allocation to terminator
let applyToTerminator (mapping: AllocationResult) (term: LIR.Terminator)
    : LIR.Instr list * LIR.Terminator =
    match term with
    | LIR.Ret -> ([], LIR.Ret)
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        match cond with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.Branch (LIR.Physical physReg, trueLabel, falseLabel))
            | Some (StackSlot offset) ->
                // Load condition from stack before branching
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel))
            | None ->
                ([], LIR.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel))
        | LIR.Physical p ->
            ([], LIR.Branch (LIR.Physical p, trueLabel, falseLabel))
    | LIR.BranchZero (cond, zeroLabel, nonZeroLabel) ->
        match cond with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.BranchZero (LIR.Physical physReg, zeroLabel, nonZeroLabel))
            | Some (StackSlot offset) ->
                // Load condition from stack before branching
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel))
            | None ->
                ([], LIR.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel))
        | LIR.Physical p ->
            ([], LIR.BranchZero (LIR.Physical p, zeroLabel, nonZeroLabel))
    | LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) ->
        match reg with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.BranchBitZero (LIR.Physical physReg, bit, zeroLabel, nonZeroLabel))
            | Some (StackSlot offset) ->
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.BranchBitZero (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel))
            | None ->
                ([], LIR.BranchBitZero (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel))
        | LIR.Physical p ->
            ([], LIR.BranchBitZero (LIR.Physical p, bit, zeroLabel, nonZeroLabel))
    | LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) ->
        match reg with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.BranchBitNonZero (LIR.Physical physReg, bit, nonZeroLabel, zeroLabel))
            | Some (StackSlot offset) ->
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.BranchBitNonZero (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel))
            | None ->
                ([], LIR.BranchBitNonZero (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel))
        | LIR.Physical p ->
            ([], LIR.BranchBitNonZero (LIR.Physical p, bit, nonZeroLabel, zeroLabel))
    | LIR.Jump label -> ([], LIR.Jump label)
    | LIR.CondBranch (cond, trueLabel, falseLabel) ->
        // CondBranch uses condition flags, not a register - pass through unchanged
        ([], LIR.CondBranch (cond, trueLabel, falseLabel))

/// Apply allocation to a basic block with liveness-aware SaveRegs/RestoreRegs population
let applyToBlockWithLiveness
    (arch: Platform.Arch)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (liveOut: BitSet)
    (floatLiveOut: BitSet)
    (block: LIR.BasicBlock)
    : LIR.BasicBlock =

    // Compute liveness at each instruction point
    let instrLiveness = computeInstructionLiveness mapping.Domain block liveOut
    let floatInstrLiveness = computeFloatInstructionLiveness floatAllocation.Domain block floatLiveOut

    // Process each instruction with its corresponding liveness
    // Debug: check lengths match
    let instrCount = List.length block.Instrs
    let livenessCount = List.length instrLiveness
    let floatLivenessCount = List.length floatInstrLiveness
    if instrCount <> livenessCount || instrCount <> floatLivenessCount then
        let message =
            $"Instruction count ({instrCount}) doesn't match liveness count ({livenessCount}) or float liveness count ({floatLivenessCount})"
        Crash.crash message

    // First pass: find SaveRegs/RestoreRegs pairs and compute the registers to save
    // For each SaveRegs, look ahead to find the matching RestoreRegs and use its liveness
    // This ensures SaveRegs and RestoreRegs have matching register lists
    let mutable savedRegsStack : (LIR.PhysReg list * LIR.PhysFPReg list) list = []

    let allocatedInstrs =
        List.zip3 block.Instrs instrLiveness floatInstrLiveness
        |> List.collect (fun (instr, liveAfter, floatLiveAfter) ->
            match instr with
            | LIR.SaveRegs ([], []) ->
                // At SaveRegs, we need to save registers that are:
                // 1. Currently live (have values that might be clobbered by the call)
                // 2. Needed after the call
                // The liveAfter here includes both categories, so we use it
                let liveCallerSaved = getLiveCallerSavedRegs mapping liveAfter
                let liveCallerSavedFloat = getLiveCallerSavedFloatRegs floatLiveAfter floatAllocation
                // Push onto stack for matching RestoreRegs
                savedRegsStack <- (liveCallerSaved, liveCallerSavedFloat) :: savedRegsStack
                applyToInstr arch mapping (LIR.SaveRegs (liveCallerSaved, liveCallerSavedFloat))
            | LIR.RestoreRegs ([], []) ->
                // Pop the matching SaveRegs registers
                let (liveCallerSaved, liveCallerSavedFloat) =
                    match savedRegsStack with
                    | (intRegs, floatRegs) :: tail ->
                        savedRegsStack <- tail
                        (intRegs, floatRegs)
                    | [] ->
                        Crash.crash "Unmatched RestoreRegs: SaveRegs stack is empty"
                applyToInstr arch mapping (LIR.RestoreRegs (liveCallerSaved, liveCallerSavedFloat))
            | _ ->
                applyToInstr arch mapping instr)

    let (termLoads, allocatedTerm) = applyToTerminator mapping block.Terminator
    { Label = block.Label
      Instrs = allocatedInstrs @ termLoads
      Terminator = allocatedTerm }

/// Apply allocation to CFG with liveness info
let applyToCFGWithLiveness
    (arch: Platform.Arch)
    (blocks: LIR.BasicBlock array)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (liveness: BlockLiveness array)
    (floatLiveness: BlockLiveness array)
    : LIR.BasicBlock array =
    let emptyFloat = bitsetEmpty floatAllocation.Domain.WordCount
    Array.init blocks.Length (fun idx ->
        let block = blocks.[idx]
        let blockLiveness = liveness.[idx]
        let floatBlockLiveness =
            if idx < floatLiveness.Length then floatLiveness.[idx]
            else { LiveIn = emptyFloat; LiveOut = emptyFloat }
        applyToBlockWithLiveness arch mapping floatAllocation blockLiveness.LiveOut floatBlockLiveness.LiveOut block)

// ============================================================================
// Float Move Generation (used by both phi resolution and param copies)
// ============================================================================

/// Generate float move instructions using allocation-based register mapping.
/// Uses the float allocation result instead of modulo-based mapping.
let generateFloatMoveInstrsWithAllocation
    (moves: (LIR.FReg * LIR.FReg) list)
    (floatAllocation: FAllocationResult) : LIR.Instr list =

    if List.isEmpty moves then []
    else
        // Map FVirtual to physical register ID using allocation
        let fregToPhysId (freg: LIR.FReg) : int =
            match freg with
            | LIR.FPhysical p -> physFPRegToInt p
            | LIR.FVirtual 1000 -> 18  // D18 (left temp for binary ops) - fixed
            | LIR.FVirtual 1001 -> 17  // D17 (right temp for binary ops) - fixed
            | LIR.FVirtual 2000 -> 16  // D16 (cycle resolution temp) - fixed
            | LIR.FVirtual n when n >= 3000 && n < 4000 ->
                19 + ((n - 3000) % 8)  // D19-D26 (call arg temps) - fixed
            | LIR.FVirtual id ->
                // Look up in allocation
                match tryFloatAllocation floatAllocation id with
                | Some physReg -> physFPRegToInt physReg
                | None ->
                    // Fallback to old modulo mapping for unallocated VRegs
                    // This can happen for VRegs that weren't in the CFG (e.g., dead code)
                    if id >= 0 && id <= 7 then 2 + id  // D2-D9 for params 0-7
                    elif id < 10000 then
                        let tempRegs = [| 0; 1; 10; 11; 12; 13; 14; 15; 27; 28; 29; 30; 31 |]
                        tempRegs[(id - 8) % 13]
                    else
                        let tempRegs = [| 0; 1; 10; 11; 12; 13; 14; 15; 27; 28; 29; 30; 31 |]
                        tempRegs[((id - 10000) + 7) % 13]

        // Convert moves to physical register IDs for cycle detection
        let physMoves = moves |> List.map (fun (dest, src) -> (fregToPhysId dest, fregToPhysId src))

        let getSrcPhysId (src: int) : int option = Some src

        let actions = ParallelMoves.resolve physMoves getSrcPhysId

        // Convert actions back to FMov instructions using original FRegs
        let maxPhysId =
            physMoves
            |> List.fold (fun acc (destId, srcId) -> max acc (max destId srcId)) 0
        let destMap = Array.create (maxPhysId + 1) None
        let srcMap = Array.create (maxPhysId + 1) None
        for (dest, src) in moves do
            let destId = fregToPhysId dest
            let srcId = fregToPhysId src
            if destId <= maxPhysId then destMap.[destId] <- Some dest
            if srcId <= maxPhysId then srcMap.[srcId] <- Some src

        actions
        |> List.collect (fun action ->
            match action with
            | ParallelMoves.SaveToTemp physId ->
                if physId >= 0 && physId < srcMap.Length then
                    match srcMap.[physId] with
                    | Some srcFreg -> [LIR.FMov (LIR.FVirtual 2000, srcFreg)]
                    | None -> []
                else
                    []
            | ParallelMoves.Move (destPhysId, srcPhysId) ->
                if destPhysId >= 0 && destPhysId < destMap.Length && srcPhysId >= 0 && srcPhysId < srcMap.Length then
                    match destMap.[destPhysId], srcMap.[srcPhysId] with
                    | Some destFreg, Some srcFreg -> [LIR.FMov (destFreg, srcFreg)]
                    | _ -> []
                else
                    []
            | ParallelMoves.MoveFromTemp destPhysId ->
                if destPhysId >= 0 && destPhysId < destMap.Length then
                    match destMap.[destPhysId] with
                    | Some destFreg -> [LIR.FMov (destFreg, LIR.FVirtual 2000)]
                    | None -> []
                else
                    [])

// ============================================================================
// Phi Resolution
// ============================================================================

/// Resolve phi nodes by inserting parallel moves at predecessor block exits.
/// This function:
/// 1. Finds all phi nodes in each block
/// 2. Drops phis whose destination is never used
/// 3. For each predecessor, collects all (dest, src) pairs for moves
/// 4. Uses ParallelMoves.resolve to sequence the moves properly (handling cycles)
/// 5. Inserts the moves at the end of each predecessor (before terminator)
/// 6. Removes phi nodes from blocks
let resolvePhiNodes
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (allocation: AllocationResult)
    (floatAllocation: FAllocationResult)
    : LIR.BasicBlock array =
    let neededDomain = buildVRegDomain (collectVRegIds blocks)
    let n = neededDomain.Ids.Length
    let wordCount = neededDomain.WordCount

    let phiSources = Array.init n (fun _ -> bitsetEmpty wordCount)
    blocks
    |> Array.iter (fun block ->
        block.Instrs
        |> List.iter (fun instr ->
            match instr with
            | LIR.Phi (LIR.Virtual destId, sources, _) ->
                match tryIndexOf neededDomain destId with
                | Some destIdx ->
                    sources
                    |> List.iter (fun (src, _) ->
                        match src with
                        | LIR.Reg (LIR.Virtual srcId) ->
                            match tryIndexOf neededDomain srcId with
                            | Some srcIdx -> bitsetAddIndexInPlace srcIdx phiSources.[destIdx]
                            | None -> ()
                        | _ -> ())
                | None -> ()
            | _ -> ()))

    let collectNonPhiUses (blocks: LIR.BasicBlock array) : BitSet =
        let uses = bitsetEmpty wordCount
        blocks
        |> Array.iter (fun block ->
            block.Instrs
            |> List.iter (fun instr ->
                match instr with
                | LIR.Phi _ -> ()
                | LIR.FPhi _ -> ()
                | _ ->
                    getUsedVRegs instr
                    |> List.iter (fun id -> bitsetAddInPlace neededDomain id uses))
            getTerminatorUsedVRegs block.Terminator
            |> List.iter (fun id -> bitsetAddInPlace neededDomain id uses))
        uses

    let collectPhysicalPhiSources (blocks: LIR.BasicBlock array) : BitSet =
        let uses = bitsetEmpty wordCount
        blocks
        |> Array.iter (fun block ->
            block.Instrs
            |> List.iter (fun instr ->
                match instr with
                | LIR.Phi (LIR.Physical _, sources, _) ->
                    sources
                    |> List.iter (fun (src, _) ->
                        match src with
                        | LIR.Reg (LIR.Virtual srcId) ->
                            bitsetAddInPlace neededDomain srcId uses
                        | _ -> ())
                | _ -> ()))
        uses

    let computeNeededVRegs (blocks: LIR.BasicBlock array) : BitSet =
        let rootUses = collectNonPhiUses blocks
        bitsetUnionInPlace rootUses (collectPhysicalPhiSources blocks)
        let rec expand (needed: BitSet) (worklist: int list) : BitSet =
            match worklist with
            | [] -> needed
            | vIdx :: rest ->
                let sources = phiSources.[vIdx]
                let newSources = bitsetDiff sources needed
                if bitsetIsEmpty newSources then
                    expand needed rest
                else
                    bitsetUnionInPlace needed newSources
                    let worklist' = (bitsetIndicesToList newSources) @ rest
                    expand needed worklist'
        expand rootUses (bitsetIndicesToList rootUses)

    let neededVRegs = computeNeededVRegs blocks

    let phiDestNeeded (dest: LIR.Reg) : bool =
        match dest with
        | LIR.Virtual id -> bitsetContains neededDomain neededVRegs id
        | LIR.Physical _ -> true

    // Get the allocation for a virtual register (register or stack slot)
    let getDestAllocation (reg: LIR.Reg) : Allocation =
        match reg with
        | LIR.Virtual id ->
            match tryAllocation allocation id with
            | Some alloc -> alloc
            | None -> Crash.crash $"RegisterAllocation: Virtual register {id} not found in allocation"
        | LIR.Physical p -> PhysReg p

    // Helper to convert a LIR.Operand to allocated version
    let operandToAllocated (op: LIR.Operand) : LIR.Operand =
        match op with
        | LIR.Reg (LIR.Virtual id) ->
            match tryAllocation allocation id with
            | Some (PhysReg r) -> LIR.Reg (LIR.Physical r)
            | Some (StackSlot offset) -> LIR.StackSlot offset
            | None -> op
        | LIR.Reg (LIR.Physical p) -> LIR.Reg (LIR.Physical p)
        | _ -> op

    // Collect all int phi info: for each phi, get (dest_reg, src_operand, pred_label)
    // This gives us: List of (dest, sources, valueType)
    let intPhiInfo =
        blocks
        |> Array.toList
        |> List.collect (fun block ->
            block.Instrs
            |> List.choose (fun instr ->
                match instr with
                | LIR.Phi (dest, sources, valueType) ->
                    if phiDestNeeded dest then
                        Some (dest, sources, valueType)
                    else
                        None
                | _ -> None))

    // Collect all float phi info: (dest FReg, source FRegs with labels)
    let floatPhiInfo =
        blocks
        |> Array.toList
        |> List.collect (fun block ->
            block.Instrs
            |> List.choose (fun instr ->
                match instr with
                | LIR.FPhi (dest, sources) -> Some (dest, sources)
                | _ -> None))

    // Group int phis by predecessor index: List<(dest_allocation, src_operand)> per block index
    // Keep the full Allocation type to handle both register and stack destinations
    let predecessorIntMoves = Array.init blocks.Length (fun _ -> [])
    for (dest, sources, _valueType) in intPhiInfo do
        let destAlloc = getDestAllocation dest
        for (src, predLabel) in sources do
            match tryBlockIndex blockIndex predLabel with
            | Some predIdx ->
                let srcAllocated = operandToAllocated src
                predecessorIntMoves.[predIdx] <- (destAlloc, srcAllocated) :: predecessorIntMoves.[predIdx]
            | None -> ()

    // Group float phis by predecessor index: List<(dest_freg, src_freg)> per block index
    // Float registers don't go through allocation - FVirtual maps directly to D regs in CodeGen
    let predecessorFloatMoves = Array.init blocks.Length (fun _ -> [])
    for (dest, sources) in floatPhiInfo do
        for (src, predLabel) in sources do
            match tryBlockIndex blockIndex predLabel with
            | Some predIdx ->
                predecessorFloatMoves.[predIdx] <- (dest, src) :: predecessorFloatMoves.[predIdx]
            | None -> ()

    // Generate move instructions for phi resolution using parallel move resolution
    // across both register and stack destinations (handles reg<->stack cycles).
    let generateIntMoveInstrs (moves: (Allocation * LIR.Operand) list) : LIR.Instr list =
        let getSrcAllocation (op: LIR.Operand) : Allocation option =
            match op with
            | LIR.Reg (LIR.Physical p) -> Some (PhysReg p)
            | LIR.StackSlot offset -> Some (StackSlot offset)
            | _ -> None

        let actions = ParallelMoves.resolve moves getSrcAllocation

        let saveToTemp (loc: Allocation) : LIR.Instr list =
            match loc with
            | PhysReg r -> [LIR.Mov (LIR.Physical LIR.X16, LIR.Reg (LIR.Physical r))]
            | StackSlot offset -> [LIR.Mov (LIR.Physical LIR.X16, LIR.StackSlot offset)]

        let moveFromTemp (loc: Allocation) : LIR.Instr list =
            match loc with
            | PhysReg r -> [LIR.Mov (LIR.Physical r, LIR.Reg (LIR.Physical LIR.X16))]
            | StackSlot offset -> [LIR.Store (offset, LIR.Physical LIR.X16)]

        let moveToDest (dest: Allocation) (src: LIR.Operand) : LIR.Instr list =
            match dest with
            | PhysReg r -> [LIR.Mov (LIR.Physical r, src)]
            | StackSlot offset ->
                match src with
                | LIR.Reg (LIR.Physical r) ->
                    [LIR.Store (offset, LIR.Physical r)]
                | _ ->
                    [LIR.Mov (LIR.Physical LIR.X16, src)
                     LIR.Store (offset, LIR.Physical LIR.X16)]

        actions
        |> List.collect (fun action ->
            match action with
            | ParallelMoves.SaveToTemp loc -> saveToTemp loc
            | ParallelMoves.Move (dest, src) -> moveToDest dest src
            | ParallelMoves.MoveFromTemp dest -> moveFromTemp dest)

    // Add moves to predecessor blocks
    let updatedBlocks = Array.copy blocks

    // Add int phi moves
    for predIdx in 0 .. updatedBlocks.Length - 1 do
        let moves = predecessorIntMoves.[predIdx]
        if not (List.isEmpty moves) then
            let predBlock = updatedBlocks.[predIdx]
            let moveInstrs = generateIntMoveInstrs moves
            updatedBlocks.[predIdx] <- { predBlock with Instrs = predBlock.Instrs @ moveInstrs }

    // Add float phi moves
    // IMPORTANT: For tail call blocks, the phi resolution is ALREADY handled by:
    // 1. FArgMoves: puts new values in D0-D7
    // 2. TailCall: jumps back to function entry
    // 3. Param copy at entry: copies D0-D7 to phi destination registers
    // So we should SKIP phi resolution for tail call backedges - it's redundant and incorrect.
    //
    // For non-tail-call predecessors, append moves at the end as usual.
    for predIdx in 0 .. updatedBlocks.Length - 1 do
        let moves = predecessorFloatMoves.[predIdx]
        if not (List.isEmpty moves) then
            let predBlock = updatedBlocks.[predIdx]
            // Add phi moves at end of predecessor block
            let moveInstrs = generateFloatMoveInstrsWithAllocation moves floatAllocation
            updatedBlocks.[predIdx] <- { predBlock with Instrs = predBlock.Instrs @ moveInstrs }

    // Remove phi and fphi nodes from all blocks
    updatedBlocks
    |> Array.map (fun block ->
        let filteredInstrs =
            block.Instrs
            |> List.filter (fun instr ->
                match instr with
                | LIR.Phi _ -> false
                | LIR.FPhi _ -> false
                | _ -> true)
        { block with Instrs = filteredInstrs })

// ============================================================================
// Main Entry Point
// ============================================================================

/// Parameter registers per ARM64 calling convention (X0-X7 for ints, D0-D7 for floats)
let parameterRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
let floatParamRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

let private appendTiming
    (phase: string)
    (elapsedMs: float)
    (timings: RegisterAllocationTiming list)
    : RegisterAllocationTiming list =
    timings @ [{ Phase = phase; ElapsedMs = elapsedMs }]

/// Allocate registers for a function
let private timePhase
    (swOpt: System.Diagnostics.Stopwatch option)
    (phase: string)
    (timings: RegisterAllocationTiming list)
    (f: unit -> 'a)
    : 'a * RegisterAllocationTiming list =
    match swOpt with
    | None -> (f (), timings)
    | Some sw ->
        let start = sw.Elapsed.TotalMilliseconds
        let result = f ()
        let elapsedMs = sw.Elapsed.TotalMilliseconds - start
        (result, appendTiming phase elapsedMs timings)

let private allocateRegistersInternal
    (arch: Platform.Arch)
    (swOpt: System.Diagnostics.Stopwatch option)
    (func: LIR.Function)
    : LIR.Function * RegisterAllocationTiming list =
    // Precompute parameter info with separate int/float counters (AAPCS64)
    // Needed for entry defs and float allocation.
    let paramsWithTypes = func.TypedParams |> List.map (fun tp -> (tp.Reg, tp.Type))
    let _, _, intParams, floatParams =
        paramsWithTypes
        |> List.fold (fun (intIdx, floatIdx, intAcc, floatAcc) (reg, typ) ->
            if typ = AST.TFloat64 then
                // Float parameter - uses D registers
                (intIdx, floatIdx + 1, intAcc, (reg, floatIdx) :: floatAcc)
            else
                // Int/other parameter - uses X registers
                (intIdx + 1, floatIdx, (reg, intIdx) :: intAcc, floatAcc)
        ) (0, 0, [], [])
    let intParams = List.rev intParams
    let floatParams = List.rev floatParams

    let intParamVRegIds =
        intParams
        |> List.choose (fun (reg, _) ->
            match reg with
            | LIR.Virtual id -> Some id
            | LIR.Physical _ -> None)

    // Extract FVirtual IDs from float params for allocation
    // Float params use Virtual register IDs that are also FVirtual IDs
    let floatParamFVirtualIds =
        floatParams
        |> List.choose (fun (reg, _) ->
            match reg with
            | LIR.Virtual id -> Some id
            | LIR.Physical _ -> None)

    let (blockIndex, blocks) = buildBlockIndex func.CFG

    // Step 1: Compute liveness (include int params in domain)
    let ((domain, livenessBits), timings) =
        timePhase swOpt "RegAlloc: Liveness" [] (fun () ->
            computeLivenessBitsRaw blockIndex blocks intParamVRegIds)
    let intParamBits = bitsetFromList domain intParamVRegIds

    // Step 2: Build interference graph
    let (graph, timings) =
        match swOpt with
        | None ->
            timePhase swOpt "RegAlloc: Interference Graph" timings (fun () ->
                buildInterferenceGraphBitsetWithLiveness blockIndex blocks domain livenessBits intParamBits)
        | Some sw ->
            let start = sw.Elapsed.TotalMilliseconds
            let (graph, igTiming) =
                buildInterferenceGraphBitsetWithLivenessProfile sw blockIndex blocks domain livenessBits intParamBits
            let totalMs = sw.Elapsed.TotalMilliseconds - start
            let timings =
                timings
                |> appendTiming "RegAlloc: Interference Graph" totalMs
                |> appendTiming "RegAlloc: Interference Graph - Live Iteration" igTiming.LiveIterationMs
                |> appendTiming "RegAlloc: Interference Graph - Adjacency Updates" igTiming.AdjacencyUpdatesMs
            (graph, timings)

    // Step 2b: Collect coalescing preferences and move pairs
    let ((preferences, movePairs), timings) =
        timePhase swOpt "RegAlloc: Coalescing Prep" timings (fun () ->
            let phiPairs = collectPhiPairs blocks
            let preferences = phiPairs
            let moves = collectMovePairs blocks
            let movePairs = dedupePairs (moves @ phiPairs)
            (preferences, movePairs))

    // Step 3: Run chordal graph coloring with phi coalescing
    // Use optimal register order based on calling pattern:
    // - Functions with non-tail calls: callee-saved first (save once in prologue/epilogue)
    // - Leaf functions / tail-call-only: caller-saved first (no prologue overhead)
    let (result, timings) =
        match swOpt with
        | None ->
            timePhase swOpt "RegAlloc: Coloring" timings (fun () ->
                let regs = getAllocatableRegs arch blocks
                let colorResult = chordalGraphColor graph [] (List.length regs) preferences movePairs
                coloringToAllocation colorResult regs)
        | Some sw ->
            let start = sw.Elapsed.TotalMilliseconds
            let regs = getAllocatableRegs arch blocks
            let (colorResult, colorTiming) =
                chordalGraphColorWithTiming sw graph [] (List.length regs) preferences movePairs
            let result = coloringToAllocation colorResult regs
            let totalMs = sw.Elapsed.TotalMilliseconds - start
            let timings =
                timings
                |> appendTiming "RegAlloc: Coloring" totalMs
                |> appendTiming "RegAlloc: Coloring - Coalesce" colorTiming.CoalesceMs
                |> appendTiming "RegAlloc: Coloring - MCS Select" colorTiming.McsSelectMs
                |> appendTiming "RegAlloc: Coloring - MCS Bucket Update" colorTiming.McsUpdateMs
                |> appendTiming "RegAlloc: Coloring - Greedy" colorTiming.GreedyMs
                |> appendTiming "RegAlloc: Coloring - Expand" colorTiming.ExpandMs
            (result, timings)

    // Step 3b: Parameter info already computed (needed for float allocation and param moves)

    // Step 3c: Run float register allocation
    // Include float param FVirtuals so they get allocated even if not used in CFG
    let ((floatDomain, floatLiveness), timings) =
        timePhase swOpt "RegAlloc: Float Liveness" timings (fun () ->
            computeFloatLivenessBitsRaw blockIndex blocks floatParamFVirtualIds)
    let floatParamBits = bitsetFromList floatDomain floatParamFVirtualIds
    let (floatAllocation, timings) =
        timePhase swOpt "RegAlloc: Float Allocation" timings (fun () ->
            chordalFloatAllocationWithLiveness blockIndex blocks floatParamBits floatDomain floatLiveness)

    let ((intParamCopyInstrs, floatParamCopyInstrs, entryEdgePhiInstrs), timings) =
        timePhase swOpt "RegAlloc: Param Moves" timings (fun () ->
            // Step 5: Build mapping that copies INT parameters from X0-X7
            // to wherever chordal graph coloring allocated them.
            // IMPORTANT: Use proper parallel move resolution to handle cycles!
            // (e.g., X1→X2 and X2→X1 require a temp register)
            let intParamMoves =
                intParams
                |> List.choose (fun (reg, paramIdx) ->
                    match reg with
                    | LIR.Virtual id ->
                        let paramReg = List.item paramIdx parameterRegs
                        match tryAllocation result id with
                        | Some (PhysReg allocatedReg) when allocatedReg <> paramReg ->
                            // Need to copy from paramReg to allocatedReg
                            Some (allocatedReg, LIR.Reg (LIR.Physical paramReg))
                        | Some (StackSlot _offset) ->
                            // Store to stack - not a register move, handle separately
                            None // We'll handle stack stores separately
                        | _ -> None // Same register or not in mapping
                    | LIR.Physical _ -> None)

            // Collect stack stores separately (they don't conflict with register moves)
            let intParamStackStores =
                intParams
                |> List.choose (fun (reg, paramIdx) ->
                    match reg with
                    | LIR.Virtual id ->
                        let paramReg = List.item paramIdx parameterRegs
                        match tryAllocation result id with
                        | Some (StackSlot offset) ->
                            Some (LIR.Store (offset, LIR.Physical paramReg))
                        | _ -> None
                    | LIR.Physical _ -> None)

            // Use parallel move resolution for register-to-register moves
            let getSrcReg (op: LIR.Operand) : LIR.PhysReg option =
                match op with
                | LIR.Reg (LIR.Physical r) -> Some r
                | _ -> None

            let moveActions = ParallelMoves.resolve intParamMoves getSrcReg

            // Convert move actions to LIR instructions using X16 as temp register
            let regMoveInstrs =
                moveActions
                |> List.collect (fun action ->
                    match action with
                    | ParallelMoves.SaveToTemp reg ->
                        [LIR.Mov (LIR.Physical LIR.X16, LIR.Reg (LIR.Physical reg))]
                    | ParallelMoves.Move (dest, src) ->
                        [LIR.Mov (LIR.Physical dest, src)]
                    | ParallelMoves.MoveFromTemp dest ->
                        [LIR.Mov (LIR.Physical dest, LIR.Reg (LIR.Physical LIR.X16))])

            // IMPORTANT: stack stores must happen BEFORE register shuffles.
            // Otherwise a shuffle may clobber a source parameter register
            // (for example X5) before we spill that original parameter value.
            let intParamCopyInstrs = intParamStackStores @ regMoveInstrs

            // Step 6: Build mapping that copies FLOAT parameters from D0-D7
            // Float parameters use FVirtual registers (same ID as Virtual)
            // and don't go through linear scan - they map directly in CodeGen
            // IMPORTANT: Use parallel move resolution to handle cases where destination
            // registers collide with source registers (e.g., when FVirtual id maps to D0
            // which is also a source register for other params)
            let floatParamMoves =
                floatParams
                |> List.choose (fun (reg, paramIdx) ->
                    match reg with
                    | LIR.Virtual id ->
                        // Float param comes in D0/D1/etc, needs to be in FVirtual id
                        let srcDReg = List.item paramIdx floatParamRegs
                        let destFVirtual = LIR.FVirtual id
                        Some (destFVirtual, LIR.FPhysical srcDReg)
                    | LIR.Physical _ -> None)

            let floatParamCopyInstrs = generateFloatMoveInstrsWithAllocation floatParamMoves floatAllocation

            // Step 6b: Extract entry-edge phi moves for float phis
            // For phis at the entry block, we need to add moves from entry-edge sources
            // to phi destinations. These moves don't get added by resolvePhiNodes because
            // there's no predecessor block for "before function entry".
            let entryBlockBeforeResolution = blocks.[blockIndex.EntryIndex]

            let entryEdgeFloatPhiMoves =
                entryBlockBeforeResolution.Instrs
                |> List.choose (fun instr ->
                    match instr with
                    | LIR.FPhi (dest, sources) ->
                        // Find sources where the predecessor label doesn't exist in the CFG
                        // (these are entry-edge sources)
                        let entryEdgeSources =
                            sources
                            |> List.filter (fun (_, predLabel) ->
                                match tryBlockIndex blockIndex predLabel with
                                | Some _ -> false
                                | None -> true)
                        // Generate moves for entry-edge sources
                        entryEdgeSources
                        |> List.map (fun (src, _) -> (dest, src))
                        |> Some
                    | _ -> None)
                |> List.concat

            // Generate FMov instructions for entry-edge phi resolution
            // Use parallel move resolution to handle potential register conflicts
            let entryEdgePhiInstrs = generateFloatMoveInstrsWithAllocation entryEdgeFloatPhiMoves floatAllocation

            (intParamCopyInstrs, floatParamCopyInstrs, entryEdgePhiInstrs))

    // Step 7: Resolve phi nodes (convert to moves at predecessor exits)
    // This must happen BEFORE applying allocation since we need to know where each
    // value is allocated to generate the correct moves
    let (blocksWithPhiResolved, timings) =
        timePhase swOpt "RegAlloc: Phi Resolution" timings (fun () ->
            resolvePhiNodes blockIndex blocks result floatAllocation)

    // Step 8: Apply allocation to CFG with liveness info for SaveRegs/RestoreRegs population
    let (allocatedBlocks, timings) =
        timePhase swOpt "RegAlloc: Apply Allocation" timings (fun () ->
            let allocatedBlocks =
                applyToCFGWithLiveness arch blocksWithPhiResolved result floatAllocation livenessBits floatLiveness
            applyFloatAllocationToBlocks floatAllocation allocatedBlocks)

    let ((cfgWithParamCopies, allocatedTypedParams), timings) =
        timePhase swOpt "RegAlloc: Finalize" timings (fun () ->
            // Step 9: Insert parameter copy instructions at the start of the entry block
            // Float param copies go first (they use separate register bank)
            // Entry-edge phi moves come after param copies (they copy from param FVirtual to phi dest FVirtual)
            // IMPORTANT: Apply float allocation to param copy instructions since they were generated
            // before applyFloatAllocationToCFG ran and still contain FVirtual registers
            let allocatedFloatParamCopyInstrs =
                floatParamCopyInstrs |> List.map (applyFloatAllocationToInstr floatAllocation)
            let allocatedEntryEdgePhiInstrs =
                entryEdgePhiInstrs |> List.map (applyFloatAllocationToInstr floatAllocation)

            let updatedBlocks = Array.copy allocatedBlocks
            let entryBlock = updatedBlocks.[blockIndex.EntryIndex]
            let entryBlockWithCopies = {
                entryBlock with
                    Instrs = allocatedFloatParamCopyInstrs @ allocatedEntryEdgePhiInstrs @ intParamCopyInstrs @ entryBlock.Instrs
            }

            updatedBlocks.[blockIndex.EntryIndex] <- entryBlockWithCopies
            let cfgWithParamCopies : LIR.CFG =
                { Entry = func.CFG.Entry; Blocks = blocksToMap blockIndex updatedBlocks }

            // Step 10: Set parameters to calling convention registers
            // Create TypedLIRParams with physical registers
            let allocatedTypedParams : LIR.TypedLIRParam list =
                func.TypedParams
                |> List.mapi (fun i tp -> { Reg = LIR.Physical (List.item i parameterRegs); Type = tp.Type })

            (cfgWithParamCopies, allocatedTypedParams))

    let allocatedFunc : LIR.Function = {
        Name = func.Name
        TypedParams = allocatedTypedParams
        CFG = cfgWithParamCopies
        StackSize = result.StackSize
        UsedCalleeSaved = result.UsedCalleeSaved
    }

    (allocatedFunc, timings)

/// Allocate registers for a function
let allocateRegisters (arch: Platform.Arch) (func: LIR.Function) : LIR.Function =
    allocateRegistersInternal arch None func |> fst

/// Allocate registers for a function and collect phase timings
let allocateRegistersWithTiming
    (arch: Platform.Arch)
    (func: LIR.Function)
    : LIR.Function * RegisterAllocationTiming list =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    allocateRegistersInternal arch (Some sw) func
