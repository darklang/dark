// 3.1_SSA_Construction.fs - SSA Construction Pass
//
// Converts MIR to SSA (Static Single Assignment) form by:
// 1. Computing dominators and dominance frontiers
// 2. Inserting phi nodes at join points
// 3. Renaming variables so each definition has a unique name
//
// After SSA construction, every virtual register is defined exactly once.
// This enables powerful optimizations like GVN, SCCP, and easy DCE.

module SSA_Construction

open MIR

/// Predecessors map: for each label, which labels can jump to it
type Predecessors = Map<Label, Label list>

type private LabelIndex = {
    Labels: Label array
    IndexOf: Map<Label, int>
}

let private buildLabelIndex (cfg: CFG) : LabelIndex =
    let labels = cfg.Blocks |> Map.keys |> Seq.toArray
    let indexOf =
        labels
        |> Array.mapi (fun idx label -> (label, idx))
        |> Array.toList
        |> Map.ofList
    { Labels = labels; IndexOf = indexOf }

/// Build predecessors map from CFG
let buildPredecessors (cfg: CFG) : Predecessors =
    let addEdge (from: Label) (toLabel: Label) (preds: Predecessors) : Predecessors =
        let existing = Map.tryFind toLabel preds |> Option.defaultValue []
        Map.add toLabel (from :: existing) preds

    cfg.Blocks
    |> Map.fold (fun preds label block ->
        // Add edges from terminator
        match block.Terminator with
        | Ret _ -> preds
        | Jump target -> addEdge label target preds
        | Branch (_, trueLabel, falseLabel) ->
            preds |> addEdge label trueLabel |> addEdge label falseLabel
    ) Map.empty

/// Compute immediate dominators using iterative dataflow
/// Returns map from label to its immediate dominator
type Dominators = Map<Label, Label>

let computeDominators (cfg: CFG) (preds: Predecessors) : Dominators =
    let labelIndex = buildLabelIndex cfg
    let labels = labelIndex.Labels |> Array.toList
    let entry = cfg.Entry

    // First, compute reachable blocks from entry using BFS
    // This is critical because unreachable blocks have no well-defined dominators
    // and would cause cycles in the idom tree if included
    let rec findReachable (queue: Label list) (visited: Set<Label>) : Set<Label> =
        match queue with
        | [] -> visited
        | current :: rest ->
            if Set.contains current visited then
                findReachable rest visited
            else
                let visited' = Set.add current visited
                let block = Map.tryFind current cfg.Blocks
                let successors =
                    match block with
                    | Some b ->
                        match b.Terminator with
                        | Ret _ -> []
                        | Jump target -> [target]
                        | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
                    | None -> []
                findReachable (rest @ successors) visited'

    let reachableBlocks = findReachable [entry] Set.empty
    let reachableLabels = labels |> List.filter (fun l -> Set.contains l reachableBlocks)
    let reachableLabelIndices =
        reachableLabels
        |> List.map (fun label ->
            match Map.tryFind label labelIndex.IndexOf with
            | Some idx -> (label, idx)
            | None -> Crash.crash $"SSA: Missing label index for {label}")

    let wordCount = Bitset.wordCount labelIndex.Labels.Length
    let reachableMask =
        let bits = Bitset.empty wordCount
        reachableLabelIndices |> List.iter (fun (_, idx) -> Bitset.addIndexInPlace idx bits)
        bits
    let entryIndex =
        match Map.tryFind entry labelIndex.IndexOf with
        | Some idx -> idx
        | None -> Crash.crash $"SSA: Missing entry label index for {entry}"
    let entryBits = Bitset.singleton wordCount entryIndex

    // Initialize: entry dominates itself, other reachable blocks dominated by all reachable
    // Unreachable blocks are NOT included in the dominator computation
    let initialDoms =
        reachableLabelIndices
        |> List.fold (fun m (label, _idx) ->
            if label = entry then
                Map.add label entryBits m
            else
                Map.add label (Bitset.clone reachableMask) m  // Initially dominated by all reachable
        ) Map.empty

    // Iterate until fixed point (only for reachable blocks)
    let rec iterate (doms: Map<Label, Bitset.Bitset>) =
        let (changed, doms') =
            reachableLabelIndices
            |> List.fold (fun (changed, m) (label, labelIdx) ->
                if label = entry then
                    (changed, m)
                else
                    // Only consider reachable predecessors
                    let predLabels =
                        Map.tryFind label preds
                        |> Option.defaultValue []
                        |> List.filter (fun p -> Set.contains p reachableBlocks)
                    if List.isEmpty predLabels then
                        (changed, m)
                    else
                        // Dom(n) = {n} union (intersection of Dom(p) for all predecessors p)
                        let predDoms =
                            predLabels
                            |> List.map (fun p -> Map.tryFind p m |> Option.defaultValue (Bitset.empty wordCount))
                        let intersection =
                            match predDoms with
                            | [] -> Bitset.empty wordCount
                            | first :: rest -> Bitset.intersectMany first rest
                        let newDom = Bitset.add labelIdx intersection
                        let oldDom = Map.tryFind label m |> Option.defaultValue (Bitset.empty wordCount)
                        if Bitset.equal newDom oldDom then
                            (changed, m)
                        else
                            (true, Map.add label newDom m)
            ) (false, doms)
        if changed then iterate doms' else doms'

    let allDoms = iterate initialDoms

    // Extract immediate dominator from dominator sets
    // idom(n) is the dominator of n that is dominated by all other dominators of n (closest one)
    // Only process reachable blocks
    reachableLabelIndices
    |> List.fold (fun idoms (label, labelIdx) ->
        if label = entry then
            idoms  // Entry has no immediate dominator
        else
            let doms = Map.tryFind label allDoms |> Option.defaultValue (Bitset.empty wordCount)
            // Remove self from dominators
            let strictDoms = Bitset.diff doms (Bitset.singleton wordCount labelIdx)
            if Bitset.isEmpty strictDoms then
                idoms
            else
                // idom is the unique strict dominator that is dominated by all other strict dominators
                // In other words, it's the "closest" dominator to the node
                let strictIndices = Bitset.indicesToList strictDoms
                let idomIdx =
                    strictIndices
                    |> List.tryFind (fun dIdx ->
                        let dLabel = labelIndex.Labels.[dIdx]
                        let dDoms = Map.tryFind dLabel allDoms |> Option.defaultValue (Bitset.empty wordCount)
                        // d is idom if all other strict dominators dominate d
                        // i.e., all other strict dominators are in Dom(d)
                        strictIndices
                        |> List.forall (fun otherIdx ->
                            otherIdx = dIdx || Bitset.containsIndex otherIdx dDoms)
                    )
                match idomIdx with
                | Some dIdx -> Map.add label labelIndex.Labels.[dIdx] idoms
                | None ->
                    Crash.crash $"SSA: Could not find immediate dominator for block {label} (malformed CFG)"
    ) Map.empty

/// Dominance frontier: blocks where dominance ends
/// DF(n) = blocks that n dominates a predecessor of, but not the block itself
type DominanceFrontier = Map<Label, Set<Label>>

let computeDominanceFrontier (cfg: CFG) (preds: Predecessors) (idoms: Dominators) : DominanceFrontier =
    let labelIndex = buildLabelIndex cfg
    let labels = labelIndex.Labels
    let labelCount = labels.Length
    let wordCount = Bitset.wordCount labelCount

    let idomIndex =
        Array.init labelCount (fun idx ->
            let label = labels.[idx]
            Map.tryFind label idoms
            |> Option.bind (fun parent -> Map.tryFind parent labelIndex.IndexOf))

    let dfSets = Array.init labelCount (fun _ -> Bitset.empty wordCount)

    // For each block b, for each predecessor p of b:
    // Walk up the dominator tree from p until we reach idom(b)
    // All blocks on this path have b in their dominance frontier
    labels
    |> Array.iteri (fun bIdx b ->
        let bPreds = Map.tryFind b preds |> Option.defaultValue []
        bPreds
        |> List.iter (fun p ->
            match Map.tryFind p labelIndex.IndexOf with
            | None -> ()
            | Some pIdx ->
                let rec walk currentIdx =
                    match idomIndex.[bIdx] with
                    | Some idomIdx when currentIdx = idomIdx -> ()
                    | _ ->
                        Bitset.addIndexInPlace bIdx dfSets.[currentIdx]
                        match idomIndex.[currentIdx] with
                        | Some parentIdx when parentIdx <> currentIdx -> walk parentIdx
                        | _ -> ()
                walk pIdx))

    labels
    |> Array.mapi (fun idx label ->
        let frontier =
            dfSets.[idx]
            |> Bitset.indicesToList
            |> List.map (fun fIdx -> labels.[fIdx])
            |> Set.ofList
        (label, frontier))
    |> Array.toList
    |> Map.ofList

/// Get all variable definitions in a basic block
/// Returns set of VRegs that are defined (written to) in the block
let getBlockDefs (block: BasicBlock) : Set<VReg> =
    block.Instrs
    |> List.fold (fun defs instr ->
        match instr with
        | Mov (dest, _, _) -> Set.add dest defs
        | BinOp (dest, _, _, _, _) -> Set.add dest defs
        | UnaryOp (dest, _, _) -> Set.add dest defs
        | Call (dest, _, _, _, _) -> Set.add dest defs
        | TailCall _ -> defs  // Tail calls have no destination
        | IndirectCall (dest, _, _, _, _) -> Set.add dest defs
        | IndirectTailCall _ -> defs  // Indirect tail calls have no destination
        | ClosureAlloc (dest, _, _) -> Set.add dest defs
        | ClosureCall (dest, _, _, _, _) -> Set.add dest defs
        | ClosureTailCall _ -> defs  // Closure tail calls have no destination
        | HeapAlloc (dest, _) -> Set.add dest defs
        | HeapStore _ -> defs  // No destination register
        | HeapLoad (dest, _, _, _) -> Set.add dest defs
        | StringConcat (dest, _, _) -> Set.add dest defs
        | RefCountInc _ -> defs
        | RefCountDec _ -> defs
        | Print _ -> defs
        | FileReadText (dest, _) -> Set.add dest defs
        | FileExists (dest, _) -> Set.add dest defs
        | FileWriteText (dest, _, _) -> Set.add dest defs
        | FileAppendText (dest, _, _) -> Set.add dest defs
        | FileDelete (dest, _) -> Set.add dest defs
        | FileSetExecutable (dest, _) -> Set.add dest defs
        | FileWriteFromPtr (dest, _, _, _) -> Set.add dest defs
        | Phi (dest, _, _) -> Set.add dest defs
        | RawAlloc (dest, _) -> Set.add dest defs
        | RawFree _ -> defs
        | RawGet (dest, _, _, _) -> Set.add dest defs
        | RawGetByte (dest, _, _) -> Set.add dest defs
        | RawSet _ -> defs
        | RawSetByte _ -> defs
        | FloatSqrt (dest, _) -> Set.add dest defs
        | FloatAbs (dest, _) -> Set.add dest defs
        | FloatNeg (dest, _) -> Set.add dest defs
        | Int64ToFloat (dest, _) -> Set.add dest defs
        | FloatToInt64 (dest, _) -> Set.add dest defs
        | FloatToBits (dest, _) -> Set.add dest defs
        | RefCountIncString _ -> defs
        | RefCountDecString _ -> defs
        | RandomInt64 dest -> Set.add dest defs
        | DateNow dest -> Set.add dest defs
        | FloatToString (dest, _) -> Set.add dest defs
        | RuntimeError _ -> defs
        | CoverageHit _ -> defs  // No destination register
    ) Set.empty

/// Get all variables defined anywhere in the CFG
let getAllDefs (cfg: CFG) : Map<VReg, Set<Label>> =
    cfg.Blocks
    |> Map.fold (fun defSites label block ->
        let blockDefs = getBlockDefs block
        blockDefs
        |> Set.fold (fun sites vreg ->
            let existing = Map.tryFind vreg sites |> Option.defaultValue Set.empty
            Map.add vreg (Set.add label existing) sites
        ) defSites
    ) Map.empty

/// Extract VRegs used in an operand
let getOperandUses (op: Operand) : Set<VReg> =
    match op with
    | Register vreg -> Set.singleton vreg
    | _ -> Set.empty

/// Get all variables used (read) in a basic block
/// Returns set of VRegs that are read in the block
let getBlockUses (block: BasicBlock) : Set<VReg> =
    let instrUses =
        block.Instrs
        |> List.fold (fun uses instr ->
            match instr with
            | Mov (_, src, _) -> Set.union uses (getOperandUses src)
            | BinOp (_, _, left, right, _) ->
                uses |> Set.union (getOperandUses left) |> Set.union (getOperandUses right)
            | UnaryOp (_, _, src) -> Set.union uses (getOperandUses src)
            | Call (_, _, args, _, _) ->
                args |> List.fold (fun u a -> Set.union u (getOperandUses a)) uses
            | TailCall (_, args, _, _) ->
                args |> List.fold (fun u a -> Set.union u (getOperandUses a)) uses
            | IndirectCall (_, func, args, _, _) ->
                let funcUses = getOperandUses func
                let argUses = args |> List.fold (fun u a -> Set.union u (getOperandUses a)) Set.empty
                uses |> Set.union funcUses |> Set.union argUses
            | IndirectTailCall (func, args, _, _) ->
                let funcUses = getOperandUses func
                let argUses = args |> List.fold (fun u a -> Set.union u (getOperandUses a)) Set.empty
                uses |> Set.union funcUses |> Set.union argUses
            | ClosureAlloc (_, _, captures) ->
                captures |> List.fold (fun u c -> Set.union u (getOperandUses c)) uses
            | ClosureCall (_, closure, args, _, _) ->
                let closureUses = getOperandUses closure
                let argUses = args |> List.fold (fun u a -> Set.union u (getOperandUses a)) Set.empty
                uses |> Set.union closureUses |> Set.union argUses
            | ClosureTailCall (closure, args, _) ->
                let closureUses = getOperandUses closure
                let argUses = args |> List.fold (fun u a -> Set.union u (getOperandUses a)) Set.empty
                uses |> Set.union closureUses |> Set.union argUses
            | HeapAlloc _ -> uses
            | HeapStore (addr, _, src, _) ->
                uses |> Set.add addr |> Set.union (getOperandUses src)
            | HeapLoad (_, addr, _, _) -> Set.add addr uses
            | StringConcat (_, left, right) ->
                uses |> Set.union (getOperandUses left) |> Set.union (getOperandUses right)
            | RefCountInc (addr, _, _) -> Set.add addr uses
            | RefCountDec (addr, _, _) -> Set.add addr uses
            | Print (src, _) -> Set.union uses (getOperandUses src)
            | FileReadText (_, path) -> Set.union uses (getOperandUses path)
            | FileExists (_, path) -> Set.union uses (getOperandUses path)
            | FileWriteText (_, path, content) ->
                uses |> Set.union (getOperandUses path) |> Set.union (getOperandUses content)
            | FileAppendText (_, path, content) ->
                uses |> Set.union (getOperandUses path) |> Set.union (getOperandUses content)
            | FileDelete (_, path) -> Set.union uses (getOperandUses path)
            | FileSetExecutable (_, path) -> Set.union uses (getOperandUses path)
            | FileWriteFromPtr (_, path, ptr, length) ->
                uses |> Set.union (getOperandUses path) |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses length)
            | Phi (_, sources, _) ->
                sources |> List.fold (fun u (src, _) -> Set.union u (getOperandUses src)) uses
            | RawAlloc (_, numBytes) -> Set.union uses (getOperandUses numBytes)
            | RawFree ptr -> Set.union uses (getOperandUses ptr)
            | RawGet (_, ptr, offset, _) ->
                uses |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses offset)
            | RawGetByte (_, ptr, offset) ->
                uses |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses offset)
            | RawSet (ptr, offset, value, _) ->
                uses |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses offset) |> Set.union (getOperandUses value)
            | RawSetByte (ptr, offset, value) ->
                uses |> Set.union (getOperandUses ptr) |> Set.union (getOperandUses offset) |> Set.union (getOperandUses value)
            | FloatSqrt (_, src) -> Set.union uses (getOperandUses src)
            | FloatAbs (_, src) -> Set.union uses (getOperandUses src)
            | FloatNeg (_, src) -> Set.union uses (getOperandUses src)
            | Int64ToFloat (_, src) -> Set.union uses (getOperandUses src)
            | FloatToInt64 (_, src) -> Set.union uses (getOperandUses src)
            | FloatToBits (_, src) -> Set.union uses (getOperandUses src)
            | RefCountIncString str -> Set.union uses (getOperandUses str)
            | RefCountDecString str -> Set.union uses (getOperandUses str)
            | RandomInt64 _ -> uses  // No operand uses
            | DateNow _ -> uses      // No operand uses
            | FloatToString (_, value) -> Set.union uses (getOperandUses value)
            | RuntimeError _ -> uses
            | CoverageHit _ -> uses  // No operand uses
        ) Set.empty

    // Also include uses in terminator
    let termUses =
        match block.Terminator with
        | Ret op -> getOperandUses op
        | Branch (cond, _, _) -> getOperandUses cond
        | Jump _ -> Set.empty

    Set.union instrUses termUses

/// Get successor labels of a block
let getSuccessors (block: BasicBlock) : Label list =
    match block.Terminator with
    | Ret _ -> []
    | Jump target -> [target]
    | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

/// Compute liveness information for the CFG
/// Returns (liveIn, liveOut) maps from Label to Set<VReg>
/// A variable is live-in at a block if it may be used before being defined
/// A variable is live-out at a block if it's live-in at any successor
let computeLiveness (cfg: CFG) : Map<Label, Set<VReg>> * Map<Label, Set<VReg>> =
    let labels = cfg.Blocks |> Map.keys |> List.ofSeq

    // Precompute uses and defs for each block
    let blockUses = labels |> List.map (fun l -> (l, getBlockUses (Map.find l cfg.Blocks))) |> Map.ofList
    let blockDefs = labels |> List.map (fun l -> (l, getBlockDefs (Map.find l cfg.Blocks))) |> Map.ofList

    // Initialize live-out to empty
    let initialLiveOut = labels |> List.map (fun l -> (l, Set.empty)) |> Map.ofList

    // Iterative dataflow analysis (backward)
    let rec fixpoint (liveOut: Map<Label, Set<VReg>>) =
        let (changed, liveOut') =
            labels
            |> List.fold (fun (changed, lo) label ->
                let block = Map.find label cfg.Blocks
                let successors = getSuccessors block

                // Live-out = union of live-in of all successors
                let newLiveOut =
                    successors
                    |> List.fold (fun acc succ ->
                        // Live-in of successor = uses + (live-out - defs)
                        let succUses = Map.find succ blockUses
                        let succDefs = Map.find succ blockDefs
                        let succLiveOut = Map.tryFind succ lo |> Option.defaultValue Set.empty
                        let succLiveIn = Set.union succUses (Set.difference succLiveOut succDefs)
                        Set.union acc succLiveIn
                    ) Set.empty

                let oldLiveOut = Map.find label lo
                if newLiveOut = oldLiveOut then
                    (changed, lo)
                else
                    (true, Map.add label newLiveOut lo)
            ) (false, liveOut)

        if changed then fixpoint liveOut' else liveOut'

    let finalLiveOut = fixpoint initialLiveOut

    // Compute live-in from live-out
    let liveIn =
        labels
        |> List.map (fun label ->
            let uses = Map.find label blockUses
            let defs = Map.find label blockDefs
            let lo = Map.find label finalLiveOut
            let li = Set.union uses (Set.difference lo defs)
            (label, li)
        )
        |> Map.ofList

    (liveIn, finalLiveOut)

/// Insert phi nodes at dominance frontiers
/// For each variable v defined in block b:
///   For each block d in DF(b):
///     Insert phi node for v in d (if not already present AND v is live-in at d)
///     This also counts as a definition, so recursively process
let insertPhiNodes (cfg: CFG) (df: DominanceFrontier) (preds: Predecessors) (liveIn: Map<Label, Set<VReg>>) (funcParams: VReg list) (paramTypes: AST.Type list) : CFG =
    // Create a map from parameter VReg to its type
    let paramTypeMap =
        List.zip funcParams paramTypes
        |> Map.ofList
    // Get all definitions from instructions in the CFG
    let instrDefs = getAllDefs cfg

    // Add function parameters as definitions at the entry block
    // This is critical for self-recursive functions: params are defined at entry (from args)
    // AND re-defined in recursive blocks (before jumping back). SSA needs both definition
    // sites to insert phi nodes at the loop header (the entry block for such functions).
    let allDefs =
        funcParams
        |> List.fold (fun defs vreg ->
            let existing = Map.tryFind vreg defs |> Option.defaultValue Set.empty
            Map.add vreg (Set.add cfg.Entry existing) defs
        ) instrDefs

    // Worklist algorithm: for each variable, propagate phi insertion
    let rec insertForVar (vreg: VReg) (worklist: Set<Label>) (phiBlocks: Set<Label>) (cfg': CFG) : CFG =
        if Set.isEmpty worklist then
            cfg'
        else
            let block = Set.minElement worklist
            let worklist' = Set.remove block worklist

            // Get dominance frontier of this block
            let frontier = Map.tryFind block df |> Option.defaultValue Set.empty

            // For each block in the frontier, insert phi if not already there AND variable is live
            let (worklist'', phiBlocks', cfg'') =
                frontier
                |> Set.fold (fun (wl, pb, c) dfBlock ->
                    if Set.contains dfBlock pb then
                        (wl, pb, c)  // Already has phi for this var
                    else
                        // Only insert phi if variable is live-in at this block
                        let blockLiveIn = Map.tryFind dfBlock liveIn |> Option.defaultValue Set.empty
                        if not (Set.contains vreg blockLiveIn) then
                            (wl, pb, c)  // Variable not live here, skip phi
                        else
                            // Insert phi node
                            let blockPreds = Map.tryFind dfBlock preds |> Option.defaultValue []
                            // Create phi with placeholder sources (will be renamed later)
                            let phiSources = blockPreds |> List.map (fun p -> (Register vreg, p))
                            // Look up type for this vreg (if it's a parameter)
                            let valueType = Map.tryFind vreg paramTypeMap
                            let phiInstr = Phi (vreg, phiSources, valueType)

                            // Add to block (at the beginning)
                            let existingBlock = Map.find dfBlock c.Blocks
                            let newBlock = { existingBlock with Instrs = phiInstr :: existingBlock.Instrs }
                            let c' = { c with Blocks = Map.add dfBlock newBlock c.Blocks }

                            // Add to worklist (phi is a definition, may need more phis)
                            let wl' = Set.add dfBlock wl
                            let pb' = Set.add dfBlock pb

                            (wl', pb', c')
                ) (worklist', phiBlocks, cfg')

            insertForVar vreg worklist'' phiBlocks' cfg''

    // Process all variables
    allDefs
    |> Map.fold (fun cfg' vreg defSites ->
        insertForVar vreg defSites Set.empty cfg'
    ) cfg

/// Rename variables to SSA form
/// Each definition gets a fresh version number
/// Uses dominator tree traversal to maintain scoping
type RenamingState = {
    /// Current version for each original VReg
    CurrentVersion: Map<VReg, int>
    /// Stack of versions for each VReg (for backtracking)
    VersionStack: Map<VReg, int list>
    /// Next available version number
    NextVersion: int
    /// Map from (original VReg, version) to new VReg
    VersionToReg: Map<VReg * int, VReg>
    /// Original floatRegs set (VReg IDs that are floats)
    OriginalFloatRegs: Set<int>
    /// Updated floatRegs set (includes SSA renamed VRegs)
    FloatRegs: Set<int>
}

/// Create initial renaming state, starting VReg numbers above any existing VRegs
let createInitialRenamingState (cfg: CFG) (floatRegs: Set<int>) : RenamingState =
    // Find the highest VReg number used in the CFG
    let maxVReg =
        cfg.Blocks
        |> Map.fold (fun maxSoFar _ block ->
            let blockMax =
                block.Instrs
                |> List.fold (fun m instr ->
                    match instr with
                    | Mov (VReg n, _, _) -> max m n
                    | BinOp (VReg n, _, _, _, _) -> max m n
                    | UnaryOp (VReg n, _, _) -> max m n
                    | Call (VReg n, _, _, _, _) -> max m n
                    | IndirectCall (VReg n, _, _, _, _) -> max m n
                    | ClosureAlloc (VReg n, _, _) -> max m n
                    | ClosureCall (VReg n, _, _, _, _) -> max m n
                    | HeapAlloc (VReg n, _) -> max m n
                    | HeapLoad (VReg n, _, _, _) -> max m n
                    | StringConcat (VReg n, _, _) -> max m n
                    | FileReadText (VReg n, _) -> max m n
                    | FileExists (VReg n, _) -> max m n
                    | FileWriteText (VReg n, _, _) -> max m n
                    | FileAppendText (VReg n, _, _) -> max m n
                    | FileDelete (VReg n, _) -> max m n
                    | FileSetExecutable (VReg n, _) -> max m n
                    | FileWriteFromPtr (VReg n, _, _, _) -> max m n
                    | Phi (VReg n, _, _) -> max m n
                    | _ -> m
                ) 0
            max maxSoFar blockMax
        ) 0

    {
        CurrentVersion = Map.empty
        VersionStack = Map.empty
        NextVersion = maxVReg + 10000  // Start SSA VRegs well above original VRegs
        VersionToReg = Map.empty
        OriginalFloatRegs = floatRegs
        FloatRegs = floatRegs  // Start with original floatRegs, will be extended
    }

/// Create new version for a definition
let newVersion (state: RenamingState) (vreg: VReg) : int * VReg * RenamingState =
    let version = state.NextVersion
    let newReg = VReg (state.NextVersion)

    // Push onto stack
    let stack = Map.tryFind vreg state.VersionStack |> Option.defaultValue []

    // If the original VReg was a float, the new SSA version is also a float
    let (VReg origId) = vreg
    let updatedFloatRegs =
        if Set.contains origId state.OriginalFloatRegs then
            Set.add state.NextVersion state.FloatRegs
        else
            state.FloatRegs

    let state' = {
        CurrentVersion = Map.add vreg version state.CurrentVersion
        VersionStack = Map.add vreg (version :: stack) state.VersionStack
        NextVersion = state.NextVersion + 1
        VersionToReg = Map.add (vreg, version) newReg state.VersionToReg
        OriginalFloatRegs = state.OriginalFloatRegs
        FloatRegs = updatedFloatRegs
    }
    (version, newReg, state')

/// Get the renamed VReg for a use
let getRenamedReg (state: RenamingState) (vreg: VReg) : VReg =
    let version = Map.tryFind vreg state.CurrentVersion |> Option.defaultValue 0
    Map.tryFind (vreg, version) state.VersionToReg
    |> Option.defaultValue vreg

/// Rename operand
let renameOperand (state: RenamingState) (op: Operand) : Operand =
    match op with
    | Register vreg -> Register (getRenamedReg state vreg)
    | other -> other

/// Rename instruction (uses and defs)
let renameInstr (state: RenamingState) (instr: Instr) : Instr * RenamingState =
    match instr with
    | Mov (dest, src, vt) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (Mov (newDest, src', vt), state')

    | BinOp (dest, op, left, right, opType) ->
        let left' = renameOperand state left
        let right' = renameOperand state right
        let (_, newDest, state') = newVersion state dest
        (BinOp (newDest, op, left', right', opType), state')

    | UnaryOp (dest, op, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (UnaryOp (newDest, op, src'), state')

    | Call (dest, funcName, args, argTypes, returnType) ->
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (Call (newDest, funcName, args', argTypes, returnType), state')

    | TailCall (funcName, args, argTypes, returnType) ->
        let args' = args |> List.map (renameOperand state)
        (TailCall (funcName, args', argTypes, returnType), state)  // No dest

    | IndirectCall (dest, func, args, argTypes, returnType) ->
        let func' = renameOperand state func
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (IndirectCall (newDest, func', args', argTypes, returnType), state')

    | IndirectTailCall (func, args, argTypes, returnType) ->
        let func' = renameOperand state func
        let args' = args |> List.map (renameOperand state)
        (IndirectTailCall (func', args', argTypes, returnType), state)  // No dest

    | ClosureAlloc (dest, funcName, captures) ->
        let captures' = captures |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (ClosureAlloc (newDest, funcName, captures'), state')

    | ClosureCall (dest, closure, args, argTypes, returnType) ->
        let closure' = renameOperand state closure
        let args' = args |> List.map (renameOperand state)
        let (_, newDest, state') = newVersion state dest
        (ClosureCall (newDest, closure', args', argTypes, returnType), state')

    | ClosureTailCall (closure, args, argTypes) ->
        let closure' = renameOperand state closure
        let args' = args |> List.map (renameOperand state)
        (ClosureTailCall (closure', args', argTypes), state)  // No dest

    | HeapAlloc (dest, size) ->
        let (_, newDest, state') = newVersion state dest
        (HeapAlloc (newDest, size), state')

    | HeapStore (addr, offset, src, vt) ->
        let src' = renameOperand state src
        // addr is used, not defined
        let addr' = getRenamedReg state addr
        (HeapStore (addr', offset, src', vt), state)

    | HeapLoad (dest, addr, offset, vt) ->
        let addr' = getRenamedReg state addr
        let (_, newDest, state') = newVersion state dest
        (HeapLoad (newDest, addr', offset, vt), state')

    | StringConcat (dest, left, right) ->
        let left' = renameOperand state left
        let right' = renameOperand state right
        let (_, newDest, state') = newVersion state dest
        (StringConcat (newDest, left', right'), state')

    | RefCountInc (addr, size, kind) ->
        let addr' = getRenamedReg state addr
        (RefCountInc (addr', size, kind), state)

    | RefCountDec (addr, size, kind) ->
        let addr' = getRenamedReg state addr
        (RefCountDec (addr', size, kind), state)

    | Print (src, vt) ->
        let src' = renameOperand state src
        (Print (src', vt), state)

    | FileReadText (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileReadText (newDest, path'), state')

    | FileExists (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileExists (newDest, path'), state')

    | FileWriteText (dest, path, content) ->
        let path' = renameOperand state path
        let content' = renameOperand state content
        let (_, newDest, state') = newVersion state dest
        (FileWriteText (newDest, path', content'), state')

    | FileAppendText (dest, path, content) ->
        let path' = renameOperand state path
        let content' = renameOperand state content
        let (_, newDest, state') = newVersion state dest
        (FileAppendText (newDest, path', content'), state')

    | FileDelete (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileDelete (newDest, path'), state')

    | FileSetExecutable (dest, path) ->
        let path' = renameOperand state path
        let (_, newDest, state') = newVersion state dest
        (FileSetExecutable (newDest, path'), state')

    | FileWriteFromPtr (dest, path, ptr, length) ->
        let path' = renameOperand state path
        let ptr' = renameOperand state ptr
        let length' = renameOperand state length
        let (_, newDest, state') = newVersion state dest
        (FileWriteFromPtr (newDest, path', ptr', length'), state')

    | Phi (dest, sources, valueType) ->
        // Phi sources are renamed when processing predecessors
        // Here we just rename the destination
        let (_, newDest, state') = newVersion state dest
        (Phi (newDest, sources, valueType), state')

    | RawAlloc (dest, numBytes) ->
        let numBytes' = renameOperand state numBytes
        let (_, newDest, state') = newVersion state dest
        (RawAlloc (newDest, numBytes'), state')

    | RawFree ptr ->
        let ptr' = renameOperand state ptr
        (RawFree ptr', state)

    | RawGet (dest, ptr, byteOffset, valueType) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let (_, newDest, state') = newVersion state dest
        (RawGet (newDest, ptr', byteOffset', valueType), state')

    | RawGetByte (dest, ptr, byteOffset) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let (_, newDest, state') = newVersion state dest
        (RawGetByte (newDest, ptr', byteOffset'), state')

    | RawSet (ptr, byteOffset, value, valueType) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let value' = renameOperand state value
        (RawSet (ptr', byteOffset', value', valueType), state)

    | RawSetByte (ptr, byteOffset, value) ->
        let ptr' = renameOperand state ptr
        let byteOffset' = renameOperand state byteOffset
        let value' = renameOperand state value
        (RawSetByte (ptr', byteOffset', value'), state)

    | FloatSqrt (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatSqrt (newDest, src'), state')

    | FloatAbs (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatAbs (newDest, src'), state')

    | FloatNeg (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatNeg (newDest, src'), state')

    | Int64ToFloat (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (Int64ToFloat (newDest, src'), state')

    | FloatToInt64 (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatToInt64 (newDest, src'), state')

    | FloatToBits (dest, src) ->
        let src' = renameOperand state src
        let (_, newDest, state') = newVersion state dest
        (FloatToBits (newDest, src'), state')

    | RefCountIncString str ->
        let str' = renameOperand state str
        (RefCountIncString str', state)

    | RefCountDecString str ->
        let str' = renameOperand state str
        (RefCountDecString str', state)

    | RandomInt64 dest ->
        let (_, newDest, state') = newVersion state dest
        (RandomInt64 newDest, state')

    | DateNow dest ->
        let (_, newDest, state') = newVersion state dest
        (DateNow newDest, state')

    | FloatToString (dest, value) ->
        let value' = renameOperand state value
        let (_, newDest, state') = newVersion state dest
        (FloatToString (newDest, value'), state')

    | RuntimeError message ->
        (RuntimeError message, state)

    | CoverageHit exprId ->
        (CoverageHit exprId, state)  // No registers to rename

/// Rename terminator
let renameTerminator (state: RenamingState) (term: Terminator) : Terminator =
    match term with
    | Ret op -> Ret (renameOperand state op)
    | Branch (cond, trueLabel, falseLabel) -> Branch (renameOperand state cond, trueLabel, falseLabel)
    | Jump label -> Jump label

/// Rename a basic block
let renameBlock (state: RenamingState) (block: BasicBlock) : BasicBlock * RenamingState =
    // Rename all instructions
    let (instrs', state') =
        block.Instrs
        |> List.fold (fun (acc, s) instr ->
            let (instr', s') = renameInstr s instr
            (acc @ [instr'], s')
        ) ([], state)

    // Rename terminator
    let term' = renameTerminator state' block.Terminator

    ({ block with Instrs = instrs'; Terminator = term' }, state')

/// Update phi sources for successors
let updatePhiSourcesForSuccessors (cfg: CFG) (currentLabel: Label) (state: RenamingState) : CFG =
    let block = Map.find currentLabel cfg.Blocks

    // Get successor labels from terminator
    let terminatorSuccessors =
        match block.Terminator with
        | Ret _ -> []
        | Jump target -> [target]
        | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

    // For each successor, update phi nodes that come from currentLabel
    terminatorSuccessors
    |> List.fold (fun cfg' succLabel ->
        let succBlock = Map.find succLabel cfg'.Blocks
        let instrs' =
            succBlock.Instrs
            |> List.map (fun instr ->
                match instr with
                | Phi (dest, sources, valueType) ->
                    // Update the source that comes from currentLabel
                    let sources' =
                        sources
                        |> List.map (fun (op, fromLabel) ->
                            if fromLabel = currentLabel then
                                (renameOperand state op, fromLabel)
                            else
                                (op, fromLabel)
                        )
                    Phi (dest, sources', valueType)
                | other -> other
            )
        let succBlock' = { succBlock with Instrs = instrs' }
        { cfg' with Blocks = Map.add succLabel succBlock' cfg'.Blocks }
    ) cfg

/// Build dominator tree children
let buildDomTree (idoms: Dominators) : Map<Label, Label list> =
    idoms
    |> Map.fold (fun tree label idom ->
        let children = Map.tryFind idom tree |> Option.defaultValue []
        Map.add idom (label :: children) tree
    ) Map.empty

/// Pop versions for definitions in a block (for backtracking)
let popVersions (state: RenamingState) (block: BasicBlock) : RenamingState =
    let defs = getBlockDefs block
    defs
    |> Set.fold (fun s vreg ->
        match Map.tryFind vreg s.VersionStack with
        | Some (_ :: rest) ->
            let currentVersion = List.tryHead rest |> Option.defaultValue 0
            { s with
                VersionStack = Map.add vreg rest s.VersionStack
                CurrentVersion = Map.add vreg currentVersion s.CurrentVersion }
        | _ -> s
    ) state

/// Rename CFG using dominator tree traversal
/// Rename CFG to SSA form
/// Returns (renamed CFG, updated floatRegs set with SSA versions)
let renameCFG (cfg: CFG) (idoms: Dominators) (floatRegs: Set<int>) : CFG * Set<int> =
    let domTree = buildDomTree idoms

    // DFS traversal of dominator tree
    // Returns (cfg, state) where state has updated NextVersion for siblings
    let rec visit (label: Label) (state: RenamingState) (cfg': CFG) : CFG * RenamingState =
        let block = Map.find label cfg'.Blocks

        // Rename this block
        let (block', state') = renameBlock state block
        let cfg'' = { cfg' with Blocks = Map.add label block' cfg'.Blocks }

        // Update phi sources in successors
        let cfg''' = updatePhiSourcesForSuccessors cfg'' label state'

        // Visit children in dominator tree
        // Thread the state through to preserve NextVersion across siblings
        let children = Map.tryFind label domTree |> Option.defaultValue []
        let (cfg'''', finalState) =
            children
            |> List.fold (fun (c, s) child ->
                // Each child inherits current versions from state', but uses s.NextVersion
                // Also inherit FloatRegs to accumulate all float VRegs
                let childState = { state' with NextVersion = s.NextVersion; FloatRegs = s.FloatRegs }
                let (c', s') = visit child childState c
                (c', s')
            ) (cfg''', state')

        // Pop versions for backtracking (restore CurrentVersion/VersionStack from before this block)
        // but keep the NextVersion and FloatRegs from children
        let stateAfterPop =
            { popVersions finalState block' with
                NextVersion = finalState.NextVersion
                FloatRegs = finalState.FloatRegs }

        (cfg'''', stateAfterPop)

    // Start from entry with initial state based on CFG's existing VRegs
    let initialState = createInitialRenamingState cfg floatRegs
    let (resultCfg, finalState) = visit cfg.Entry initialState cfg
    (resultCfg, finalState.FloatRegs)

/// Convert a function to SSA form
let convertFunctionToSSA (func: Function) : Function =
    let cfg = func.CFG
    let preds = buildPredecessors cfg
    let idoms = computeDominators cfg preds
    let df = computeDominanceFrontier cfg preds idoms

    // Compute liveness to only insert phi nodes for live variables
    let (liveIn, _) = computeLiveness cfg

    // Insert phi nodes (only for live variables)
    // Pass function params so they're treated as defined at entry (for self-recursive functions)
    let paramRegs = func.TypedParams |> List.map (fun tp -> tp.Reg)
    let paramTypes = func.TypedParams |> List.map (fun tp -> tp.Type)
    let cfgWithPhis = insertPhiNodes cfg df preds liveIn paramRegs paramTypes

    // Rename variables and update floatRegs with SSA versions
    let (ssaCFG, updatedFloatRegs) = renameCFG cfgWithPhis idoms func.FloatRegs

    { func with CFG = ssaCFG; FloatRegs = updatedFloatRegs }

/// Convert a program to SSA form
let convertToSSA (program: Program) : Program =
    let (Program (functions, variants, records)) = program
    let functions' = functions |> List.map convertFunctionToSSA
    Program (functions', variants, records)
