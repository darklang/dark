// 3.5_MIR_Optimize.fs - MIR/SSA Optimization Pass
//
// Performs optimizations on MIR in SSA form:
// - Constant folding: evaluate constant operations
// - Common subexpression elimination (CSE): reuse identical computations
// - Copy propagation: eliminate trivial moves and phis
// - Dead code elimination (DCE): remove unused instructions
// - CFG simplification: remove empty blocks, merge blocks
// - Loop-invariant code motion (LICM): hoist loop-invariant expressions
//
// These optimizations leverage SSA form where each variable is defined exactly once.

module MIR_Optimize

open MIR
open Output
open SSA_Construction

type OptimizeOptions = {
    EnableConstFolding: bool
    EnableCSE: bool
    EnableCopyProp: bool
    EnableDCE: bool
    EnableCFGSimplify: bool
    EnableLICM: bool
}

let defaultOptimizeOptions = {
    EnableConstFolding = true
    EnableCSE = true
    EnableCopyProp = true
    EnableDCE = true
    EnableCFGSimplify = true
    EnableLICM = true
}

/// Check if an instruction has side effects (must be preserved even if unused)
let hasSideEffects (instr: Instr) : bool =
    match instr with
    | Mov _ -> false
    | BinOp _ -> false
    | UnaryOp _ -> false
    | Phi _ -> false
    | HeapLoad _ -> false
    // These have side effects
    | Call _ -> true  // Function calls may have side effects
    | TailCall _ -> true  // Tail calls have side effects
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true  // Indirect tail calls have side effects
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true  // Closure tail calls have side effects
    | HeapAlloc _ -> true  // Allocates memory
    | HeapStore _ -> true  // Writes to memory
    | StringConcat _ -> true  // Allocates memory
    | RefCountInc _ -> true
    | RefCountDec _ -> true
    | Print _ -> true
    | FileReadText _ -> true
    | FileExists _ -> true
    | FileWriteText _ -> true
    | FileAppendText _ -> true
    | FileDelete _ -> true
    | FileSetExecutable _ -> true
    | FileWriteFromPtr _ -> true  // File I/O
    | RawAlloc _ -> true  // Allocates memory
    | RawFree _ -> true   // Frees memory
    | RawGet _ -> false   // Pure memory read
    | RawGetByte _ -> false  // Pure memory read (byte)
    | RawSet _ -> true    // Writes to memory
    | RawSetByte _ -> true  // Writes to memory (byte)
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | Int64ToFloat _ -> false // Pure conversion
    | FloatToInt64 _ -> false // Pure conversion
    | FloatToBits _ -> false  // Pure conversion
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RandomInt64 _ -> true  // Syscall
    | DateNow _ -> true      // Syscall
    | FloatToString _ -> false  // Pure conversion (allocates but no visible side effect)
    | RuntimeError _ -> true
    | CoverageHit _ -> true  // Must not be eliminated (tracking side effect)

/// Get the destination VReg of an instruction (if any)
let getInstrDest (instr: Instr) : VReg option =
    match instr with
    | Mov (dest, _, _) -> Some dest
    | BinOp (dest, _, _, _, _) -> Some dest
    | UnaryOp (dest, _, _) -> Some dest
    | Call (dest, _, _, _, _) -> Some dest
    | TailCall _ -> None  // Tail calls don't return here
    | IndirectCall (dest, _, _, _, _) -> Some dest
    | IndirectTailCall _ -> None  // Indirect tail calls don't return here
    | ClosureAlloc (dest, _, _) -> Some dest
    | ClosureCall (dest, _, _, _, _) -> Some dest
    | ClosureTailCall _ -> None  // Closure tail calls don't return here
    | HeapAlloc (dest, _) -> Some dest
    | HeapLoad (dest, _, _, _) -> Some dest
    | StringConcat (dest, _, _) -> Some dest
    | FileReadText (dest, _) -> Some dest
    | FileExists (dest, _) -> Some dest
    | FileWriteText (dest, _, _) -> Some dest
    | FileAppendText (dest, _, _) -> Some dest
    | FileDelete (dest, _) -> Some dest
    | FileSetExecutable (dest, _) -> Some dest
    | FileWriteFromPtr (dest, _, _, _) -> Some dest
    | Phi (dest, _, _) -> Some dest
    | RawAlloc (dest, _) -> Some dest
    | RawGet (dest, _, _, _) -> Some dest
    | RawGetByte (dest, _, _) -> Some dest
    | FloatSqrt (dest, _) -> Some dest
    | FloatAbs (dest, _) -> Some dest
    | FloatNeg (dest, _) -> Some dest
    | Int64ToFloat (dest, _) -> Some dest
    | FloatToInt64 (dest, _) -> Some dest
    | FloatToBits (dest, _) -> Some dest
    | HeapStore _ -> None
    | RefCountInc _ -> None
    | RefCountDec _ -> None
    | Print _ -> None
    | RawFree _ -> None
    | RawSet _ -> None
    | RawSetByte _ -> None
    | RefCountIncString _ -> None
    | RefCountDecString _ -> None
    | RandomInt64 dest -> Some dest
    | DateNow dest -> Some dest
    | FloatToString (dest, _) -> Some dest
    | RuntimeError _ -> None
    | CoverageHit _ -> None

/// Get all VRegs used by an instruction
let getInstrUses (instr: Instr) : Set<VReg> =
    let fromOperand op =
        match op with
        | Register vreg -> Set.singleton vreg
        | _ -> Set.empty

    match instr with
    | Mov (_, src, _) -> fromOperand src
    | BinOp (_, _, left, right, _) -> Set.union (fromOperand left) (fromOperand right)
    | UnaryOp (_, _, src) -> fromOperand src
    | Call (_, _, args, _, _) -> args |> List.map fromOperand |> Set.unionMany
    | TailCall (_, args, _, _) -> args |> List.map fromOperand |> Set.unionMany
    | IndirectCall (_, func, args, _, _) -> Set.unionMany ((fromOperand func) :: (args |> List.map fromOperand))
    | IndirectTailCall (func, args, _, _) -> Set.unionMany ((fromOperand func) :: (args |> List.map fromOperand))
    | ClosureAlloc (_, _, captures) -> captures |> List.map fromOperand |> Set.unionMany
    | ClosureCall (_, closure, args, _, _) -> Set.unionMany ((fromOperand closure) :: (args |> List.map fromOperand))
    | ClosureTailCall (closure, args, _) -> Set.unionMany ((fromOperand closure) :: (args |> List.map fromOperand))
    | HeapAlloc _ -> Set.empty
    | HeapStore (addr, _, src, _) -> Set.add addr (fromOperand src)
    | HeapLoad (_, addr, _, _) -> Set.singleton addr
    | StringConcat (_, left, right) -> Set.union (fromOperand left) (fromOperand right)
    | RefCountInc (addr, _, _) -> Set.singleton addr
    | RefCountDec (addr, _, _) -> Set.singleton addr
    | Print (src, _) -> fromOperand src
    | FileReadText (_, path) -> fromOperand path
    | FileExists (_, path) -> fromOperand path
    | FileWriteText (_, path, content) -> Set.union (fromOperand path) (fromOperand content)
    | FileAppendText (_, path, content) -> Set.union (fromOperand path) (fromOperand content)
    | FileDelete (_, path) -> fromOperand path
    | FileSetExecutable (_, path) -> fromOperand path
    | FileWriteFromPtr (_, path, ptr, length) -> Set.unionMany [fromOperand path; fromOperand ptr; fromOperand length]
    | Phi (_, sources, _) -> sources |> List.map (fun (op, _) -> fromOperand op) |> Set.unionMany
    | RawAlloc (_, numBytes) -> fromOperand numBytes
    | RawFree ptr -> fromOperand ptr
    | RawGet (_, ptr, byteOffset, _) -> Set.union (fromOperand ptr) (fromOperand byteOffset)
    | RawGetByte (_, ptr, byteOffset) -> Set.union (fromOperand ptr) (fromOperand byteOffset)
    | RawSet (ptr, byteOffset, value, _) -> Set.unionMany [fromOperand ptr; fromOperand byteOffset; fromOperand value]
    | RawSetByte (ptr, byteOffset, value) -> Set.unionMany [fromOperand ptr; fromOperand byteOffset; fromOperand value]
    | FloatSqrt (_, src) -> fromOperand src
    | FloatAbs (_, src) -> fromOperand src
    | FloatNeg (_, src) -> fromOperand src
    | Int64ToFloat (_, src) -> fromOperand src
    | FloatToInt64 (_, src) -> fromOperand src
    | FloatToBits (_, src) -> fromOperand src
    | RefCountIncString str -> fromOperand str
    | RefCountDecString str -> fromOperand str
    | RandomInt64 _ -> Set.empty  // No operand uses
    | DateNow _ -> Set.empty      // No operand uses
    | FloatToString (_, value) -> fromOperand value
    | RuntimeError _ -> Set.empty
    | CoverageHit _ -> Set.empty  // No operand uses

/// Get VRegs used by terminator
let getTerminatorUses (term: Terminator) : Set<VReg> =
    match term with
    | Ret op ->
        match op with
        | Register vreg -> Set.singleton vreg
        | _ -> Set.empty
    | Branch (cond, _, _) ->
        match cond with
        | Register vreg -> Set.singleton vreg
        | _ -> Set.empty
    | Jump _ -> Set.empty

/// Get successors from a basic block terminator
let getSuccessors (block: BasicBlock) : Label list =
    match block.Terminator with
    | Ret _ -> []
    | Jump label -> [label]
    | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

/// Build successor map for the CFG
let buildSuccessors (cfg: CFG) : Map<Label, Label list> =
    cfg.Blocks |> Map.map (fun _ block -> getSuccessors block)

/// Check if dominator dominates node (using idom chain)
let dominates (entry: Label) (idoms: Dominators) (dominator: Label) (node: Label) : bool =
    if dominator = node then
        true
    elif dominator = entry then
        node = entry || Map.containsKey node idoms
    else
        let rec walk current =
            match Map.tryFind current idoms with
            | None -> false
            | Some parent ->
                if parent = dominator then true
                elif parent = entry then false
                else walk parent
        walk node

/// Identify natural loops via backedges (header dominates source)
let findNaturalLoops (cfg: CFG) : Map<Label, Set<Label>> =
    let preds = buildPredecessors cfg
    let idoms = computeDominators cfg preds
    let entry = cfg.Entry
    let succs = buildSuccessors cfg

    let backedges =
        succs
        |> Map.fold (fun acc from successors ->
            successors
            |> List.fold (fun acc' succ ->
                if dominates entry idoms succ from then
                    let existing = Map.tryFind succ acc' |> Option.defaultValue []
                    Map.add succ (from :: existing) acc'
                else
                    acc'
            ) acc
        ) Map.empty

    backedges
    |> Map.fold (fun loops header sources ->
        let loopBlocks =
            sources
            |> List.fold (fun acc source ->
                let initial = Set.ofList [header; source]
                let rec grow work loopSet =
                    match work with
                    | [] -> loopSet
                    | node :: rest ->
                        let nodePreds = Map.tryFind node preds |> Option.defaultValue []
                        let (loopSet', work') =
                            nodePreds
                            |> List.fold (fun (setAcc, workAcc) pred ->
                                if Set.contains pred setAcc then
                                    (setAcc, workAcc)
                                elif dominates entry idoms header pred then
                                    (Set.add pred setAcc, pred :: workAcc)
                                else
                                    (setAcc, workAcc)
                            ) (loopSet, rest)
                        grow work' loopSet'
                Set.union acc (grow [source] initial)
            ) Set.empty

        if Set.isEmpty loopBlocks then loops else Map.add header loopBlocks loops
    ) Map.empty

/// Check if an instruction is safe to hoist out of a loop
let isHoistableInstr (instr: Instr) : bool =
    match instr with
    | BinOp _ -> true
    | UnaryOp _ -> true
    | HeapLoad _ -> true
    | FloatSqrt _ -> true
    | FloatAbs _ -> true
    | FloatNeg _ -> true
    | Int64ToFloat _ -> true
    | FloatToInt64 _ -> true
    | FloatToBits _ -> true
    | _ -> false

/// Apply loop-invariant code motion for loops with a simple preheader
let applyLoopInvariantCodeMotion (cfg: CFG) : CFG * bool =
    let loops = findNaturalLoops cfg
    let preds = buildPredecessors cfg
    let labelName (Label name) = name
    let buildCopyMapForLicm (cfg': CFG) : Map<VReg, VReg> =
        let phiDests =
            cfg'.Blocks
            |> Map.fold (fun dests _ block ->
                block.Instrs
                |> List.fold (fun acc instr ->
                    match instr with
                    | Phi (dest, _, _) -> Set.add dest acc
                    | _ -> acc
                ) dests
            ) Set.empty

        cfg'.Blocks
        |> Map.fold (fun acc _ block ->
            block.Instrs
            |> List.fold (fun mapAcc instr ->
                match instr with
                | Mov (dest, Register src, _) when dest <> src ->
                    if Set.contains dest phiDests || Map.containsKey dest mapAcc then mapAcc
                    else Map.add dest src mapAcc
                | _ -> mapAcc
            ) acc
        ) Map.empty

    let resolveCopyForLicm (copyMap: Map<VReg, VReg>) (op: Operand) : Operand =
        let rec resolve visited op' =
            match op' with
            | Register vreg ->
                if Set.contains vreg visited then
                    op'
                else
                    match Map.tryFind vreg copyMap with
                    | Some src -> resolve (Set.add vreg visited) (Register src)
                    | None -> op'
            | _ -> op'
        resolve Set.empty op

    loops
    |> Map.fold (fun (cfgAcc, changedAcc) header loopBlocks ->
        let copyMap = buildCopyMapForLicm cfgAcc
        let outsidePreds =
            Map.tryFind header preds
            |> Option.defaultValue []
            |> List.filter (fun pred -> not (Set.contains pred loopBlocks))

        let tryGetPreheader =
            match outsidePreds with
            | [preheader] ->
                match Map.tryFind preheader cfgAcc.Blocks with
                | Some block ->
                    match block.Terminator with
                    | Jump target when target = header -> Some preheader
                    | _ -> None
                | None -> None
            | _ -> None

        match tryGetPreheader with
        | None -> (cfgAcc, changedAcc)
        | Some preheader ->
            let loopDefs =
                loopBlocks
                |> Set.fold (fun defs label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> defs
                    | Some block ->
                        block.Instrs
                        |> List.fold (fun defs' instr ->
                            match getInstrDest instr with
                            | Some dest -> Set.add dest defs'
                            | None -> defs'
                        ) defs
                ) Set.empty

            let blockOrder =
                header :: (loopBlocks |> Set.remove header |> Set.toList |> List.sortBy labelName)

            let resolveOp (op: Operand) : Operand =
                resolveCopyForLicm copyMap op

            let resolveInvariantOperand (invariantMap: Map<VReg, Operand>) (op: Operand) : Operand =
                let rec resolve visited op' =
                    match op' with
                    | Register vreg ->
                        if Set.contains vreg visited then
                            op'
                        else
                            match Map.tryFind vreg invariantMap with
                            | Some mapped -> resolve (Set.add vreg visited) mapped
                            | None -> op'
                    | _ -> op'
                resolve Set.empty op

            let rec findInvariantPhis (current: Map<VReg, Operand>) : Map<VReg, Operand> =
                let next =
                    loopBlocks
                    |> Set.fold (fun acc label ->
                        match Map.tryFind label cfgAcc.Blocks with
                        | None -> acc
                        | Some block ->
                            block.Instrs
                            |> List.fold (fun acc' instr ->
                                match instr with
                                | Phi (dest, sources, _) ->
                                    let sources' =
                                        sources
                                        |> List.map (fun (op, lbl) ->
                                            (resolveInvariantOperand acc' (resolveOp op), lbl))
                                    let outsideSources =
                                        sources'
                                        |> List.filter (fun (_, lbl) -> not (Set.contains lbl loopBlocks))
                                    let insideSources =
                                        sources'
                                        |> List.filter (fun (_, lbl) -> Set.contains lbl loopBlocks)
                                    match outsideSources with
                                    | [] -> acc'
                                    | (outsideOp, _) :: rest ->
                                        if rest |> List.forall (fun (op, _) -> op = outsideOp) then
                                            let outsideInvariant =
                                                match outsideOp with
                                                | Register vreg ->
                                                    not (Set.contains vreg loopDefs) || Map.containsKey vreg acc'
                                                | _ -> true
                                            let insideOk =
                                                insideSources
                                                |> List.forall (fun (op, _) ->
                                                    match op with
                                                    | Register vreg when vreg = dest -> true
                                                    | _ -> op = outsideOp
                                                )
                                            if outsideInvariant && insideOk then Map.add dest outsideOp acc' else acc'
                                        else
                                            acc'
                                | _ -> acc'
                            ) acc
                    ) current
                if next = current then current else findInvariantPhis next

            let invariantPhiMap = findInvariantPhis Map.empty
            let invariantPhis = invariantPhiMap |> Map.toList |> List.map fst |> Set.ofList

            let rewriteInvariantInstr (instr: Instr) : Instr =
                let rewriteOperand op = resolveInvariantOperand invariantPhiMap op
                match instr with
                | BinOp (dest, op, left, right, operandType) ->
                    BinOp (dest, op, rewriteOperand left, rewriteOperand right, operandType)
                | UnaryOp (dest, op, src) ->
                    UnaryOp (dest, op, rewriteOperand src)
                | HeapLoad (dest, addr, offset, vt) ->
                    match rewriteOperand (Register addr) with
                    | Register addr' -> HeapLoad (dest, addr', offset, vt)
                    | _ -> Crash.crash "LICM: HeapLoad address should remain a register"
                | FloatSqrt (dest, src) -> FloatSqrt (dest, rewriteOperand src)
                | FloatAbs (dest, src) -> FloatAbs (dest, rewriteOperand src)
                | FloatNeg (dest, src) -> FloatNeg (dest, rewriteOperand src)
                | Int64ToFloat (dest, src) -> Int64ToFloat (dest, rewriteOperand src)
                | FloatToInt64 (dest, src) -> FloatToInt64 (dest, rewriteOperand src)
                | FloatToBits (dest, src) -> FloatToBits (dest, rewriteOperand src)
                | _ -> instr

            let rec findHoistable invariants hoistMap =
                let (invariants', hoistMap', changed) =
                    blockOrder
                    |> List.fold (fun (invAcc, mapAcc, chAcc) label ->
                        match Map.tryFind label cfgAcc.Blocks with
                        | None -> (invAcc, mapAcc, chAcc)
                        | Some block ->
                            let (blockHoists, invAcc', blockChanged) =
                                block.Instrs
                                |> List.fold (fun (hoists, invs, ch) instr ->
                                    match getInstrDest instr with
                                    | None -> (hoists, invs, ch)
                                    | Some dest ->
                                        let usesInvariant =
                                            getInstrUses instr
                                            |> Set.forall (fun vreg ->
                                                not (Set.contains vreg loopDefs) || Set.contains vreg invs
                                            )
                                        if Set.contains dest invs then
                                            (hoists, invs, ch)
                                        elif isHoistableInstr instr && usesInvariant then
                                            (hoists @ [instr], Set.add dest invs, true)
                                        else
                                            (hoists, invs, ch)
                                ) ([], invAcc, false)
                            let mapAcc' =
                                if List.isEmpty blockHoists then mapAcc
                                else
                                    let existing = Map.tryFind label mapAcc |> Option.defaultValue []
                                    Map.add label (existing @ blockHoists) mapAcc
                            (invAcc', mapAcc', chAcc || blockChanged)
                    ) (invariants, hoistMap, false)

                if changed then findHoistable invariants' hoistMap' else (invariants', hoistMap')

            let (_, hoistMap) = findHoistable invariantPhis Map.empty
            if Map.isEmpty hoistMap then
                (cfgAcc, changedAcc)
            else
                let hoistedInstrs =
                    blockOrder
                    |> List.collect (fun label -> Map.tryFind label hoistMap |> Option.defaultValue [])
                    |> List.map rewriteInvariantInstr

                let blocks' =
                    cfgAcc.Blocks
                    |> Map.map (fun label block ->
                        if label = preheader then
                            { block with Instrs = block.Instrs @ hoistedInstrs }
                        elif Set.contains label loopBlocks then
                            let hoistedDests =
                                Map.tryFind label hoistMap
                                |> Option.defaultValue []
                                |> List.choose getInstrDest
                                |> Set.ofList
                            let instrs' =
                                block.Instrs
                                |> List.filter (fun instr ->
                                    match getInstrDest instr with
                                    | Some dest -> not (Set.contains dest hoistedDests)
                                    | None -> true)
                            { block with Instrs = instrs' }
                        else
                            block
                    )

                ({ cfgAcc with Blocks = blocks' }, true)
    ) (cfg, false)

/// Build map from SSA destination to the registers used by its defining instruction.
let private buildDefUseMap (cfg: CFG) : Map<VReg, Set<VReg>> =
    cfg.Blocks
    |> Map.fold (fun defUses _ block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match getInstrDest instr with
            | Some dest -> Map.add dest (getInstrUses instr) acc
            | None -> acc
        ) defUses
    ) Map.empty

/// Collect registers that are directly required by side effects and control flow.
let private collectRootUses (cfg: CFG) : Set<VReg> =
    cfg.Blocks
    |> Map.fold (fun roots _ block ->
        let sideEffectUses =
            block.Instrs
            |> List.fold (fun acc instr ->
                if hasSideEffects instr then
                    Set.union acc (getInstrUses instr)
                else
                    acc
            ) Set.empty
        let termUses = getTerminatorUses block.Terminator
        Set.unionMany [roots; sideEffectUses; termUses]
    ) Set.empty

/// Mark live SSA destinations by walking backwards from root uses.
let private collectLiveDestinations (cfg: CFG) : Set<VReg> =
    let defUseMap = buildDefUseMap cfg
    let roots = collectRootUses cfg

    let rec loop (work: VReg list) (queued: Set<VReg>) (live: Set<VReg>) : Set<VReg> =
        match work with
        | [] -> live
        | reg :: rest ->
            let queued' = Set.remove reg queued
            match Map.tryFind reg defUseMap with
            | None ->
                // Parameters or registers without a local definition.
                loop rest queued' live
            | Some uses when Set.contains reg live ->
                loop rest queued' live
            | Some uses ->
                let (rest', queued'') =
                    uses
                    |> Set.fold (fun (pending, queuedAcc) usedReg ->
                        if Set.contains usedReg queuedAcc || Set.contains usedReg live then
                            (pending, queuedAcc)
                        else
                            (usedReg :: pending, Set.add usedReg queuedAcc)
                    ) (rest, queued')
                loop rest' queued'' (Set.add reg live)

    loop (Set.toList roots) roots Set.empty

/// Dead Code Elimination
/// Remove instructions whose destinations are never used (unless they have side effects)
let eliminateDeadCode (cfg: CFG) : CFG * bool =
    let liveDests = collectLiveDestinations cfg

    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match getInstrDest instr with
                    | Some dest when not (Set.contains dest liveDests) && not (hasSideEffects instr) ->
                        // Dead instruction - remove it
                        (acc', true)
                    | _ ->
                        // Keep instruction
                        (acc' @ [instr], ch')
                ) ([], false)

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Copy Propagation
/// Replace uses of copy destinations with their sources
/// For: dest = src, replace all uses of dest with src
type CopyMap = Map<VReg, Operand>

let buildCopyMap (cfg: CFG) : CopyMap =
    // First, collect all phi destinations - these should not be copy propagated
    let phiDests =
        cfg.Blocks
        |> Map.fold (fun dests _ block ->
            block.Instrs
            |> List.fold (fun d instr ->
                match instr with
                | Phi (dest, _, _) -> Set.add dest d
                | _ -> d
            ) dests
        ) Set.empty

    cfg.Blocks
    |> Map.fold (fun copies _ block ->
        block.Instrs
        |> List.fold (fun m instr ->
            match instr with
            | Mov (dest, Register src, vt) when dest <> src ->
                // Don't add if dest is a phi destination or already in map
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Mov (dest, (Int64Const _ as src), vt) ->
                // Constant propagation: track constant moves too
                // Only propagate if this is for integer/bool types, not for heap types like strings
                // This prevents incorrectly propagating Int64Const 0L to string variables
                let isIntOrBoolType =
                    match vt with
                    | Some AST.TInt64 | Some AST.TInt32 | Some AST.TInt16 | Some AST.TInt8
                    | Some AST.TUInt64 | Some AST.TUInt32 | Some AST.TUInt16 | Some AST.TUInt8
                    | Some AST.TBool | Some AST.TUnit | None -> true
                    | _ -> false  // Don't propagate for TString, TList, etc.
                if Set.contains dest phiDests || Map.containsKey dest m || not isIntOrBoolType then m
                else Map.add dest src m
            | Mov (dest, (BoolConst _ as src), _) ->
                // Constant propagation: track constant moves too
                if Set.contains dest phiDests || Map.containsKey dest m then m
                else Map.add dest src m
            | Phi (dest, [(Register src, _)], _) when dest <> src ->
                // Trivial phi with single register source
                if Map.containsKey dest m then m
                else Map.add dest (Register src) m
            | Phi (dest, sources, vt) ->
                // Check if all sources are the same register
                if Map.containsKey dest m then m
                else
                    match sources with
                    | (Register firstSrc, _) :: rest ->
                        if rest |> List.forall (fun (s, _) -> s = Register firstSrc) then
                            if dest <> firstSrc then
                                Map.add dest (Register firstSrc) m
                            else
                                m
                        else
                            m
                    | _ -> m
            | _ -> m
        ) copies
    ) Map.empty

/// Transitively resolve a copy chain (with cycle detection)
let resolveCopy (copies: CopyMap) (op: Operand) : Operand =
    let rec resolve visited op' =
        match op' with
        | Register vreg ->
            if Set.contains vreg visited then
                // Cycle detected, stop here
                op'
            else
                match Map.tryFind vreg copies with
                | Some resolvedOp -> resolve (Set.add vreg visited) resolvedOp
                | None -> op'
        | _ -> op'
    resolve Set.empty op

/// Apply copy propagation to an operand
let propagateCopyOperand (copies: CopyMap) (op: Operand) : Operand =
    resolveCopy copies op

/// Apply copy propagation to an instruction
let propagateCopyInstr (copies: CopyMap) (instr: Instr) : Instr =
    let p = propagateCopyOperand copies
    match instr with
    | Mov (dest, src, vt) -> Mov (dest, p src, vt)
    | BinOp (dest, op, left, right, opType) -> BinOp (dest, op, p left, p right, opType)
    | UnaryOp (dest, op, src) -> UnaryOp (dest, op, p src)
    | Call (dest, name, args, argTypes, retType) -> Call (dest, name, List.map p args, argTypes, retType)
    | TailCall (name, args, argTypes, retType) -> TailCall (name, List.map p args, argTypes, retType)
    | IndirectCall (dest, func, args, argTypes, retType) -> IndirectCall (dest, p func, List.map p args, argTypes, retType)
    | IndirectTailCall (func, args, argTypes, retType) -> IndirectTailCall (p func, List.map p args, argTypes, retType)
    | ClosureAlloc (dest, name, captures) -> ClosureAlloc (dest, name, List.map p captures)
    | ClosureCall (dest, closure, args, argTypes, retType) -> ClosureCall (dest, p closure, List.map p args, argTypes, retType)
    | ClosureTailCall (closure, args, argTypes) -> ClosureTailCall (p closure, List.map p args, argTypes)
    | HeapAlloc (dest, size) -> HeapAlloc (dest, size)
    | HeapStore (addr, offset, src, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapStore (addr', offset, p src, vt)
    | HeapLoad (dest, addr, offset, vt) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        HeapLoad (dest, addr', offset, vt)
    | StringConcat (dest, left, right) -> StringConcat (dest, p left, p right)
    | RefCountInc (addr, size, kind) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountInc (addr', size, kind)
    | RefCountDec (addr, size, kind) ->
        let addr' = match p (Register addr) with Register v -> v | _ -> addr
        RefCountDec (addr', size, kind)
    | Print (src, vt) -> Print (p src, vt)
    | FileReadText (dest, path) -> FileReadText (dest, p path)
    | FileExists (dest, path) -> FileExists (dest, p path)
    | FileWriteText (dest, path, content) -> FileWriteText (dest, p path, p content)
    | FileAppendText (dest, path, content) -> FileAppendText (dest, p path, p content)
    | FileDelete (dest, path) -> FileDelete (dest, p path)
    | FileSetExecutable (dest, path) -> FileSetExecutable (dest, p path)
    | FileWriteFromPtr (dest, path, ptr, length) -> FileWriteFromPtr (dest, p path, p ptr, p length)
    // Don't propagate copies into phi sources - phis are merge points and their
    // sources represent values flowing from specific predecessor blocks
    | Phi (dest, sources, valueType) -> Phi (dest, sources, valueType)
    | RawAlloc (dest, numBytes) -> RawAlloc (dest, p numBytes)
    | RawFree ptr -> RawFree (p ptr)
    | RawGet (dest, ptr, byteOffset, valueType) -> RawGet (dest, p ptr, p byteOffset, valueType)
    | RawGetByte (dest, ptr, byteOffset) -> RawGetByte (dest, p ptr, p byteOffset)
    | RawSet (ptr, byteOffset, value, valueType) -> RawSet (p ptr, p byteOffset, p value, valueType)
    | RawSetByte (ptr, byteOffset, value) -> RawSetByte (p ptr, p byteOffset, p value)
    | FloatSqrt (dest, src) -> FloatSqrt (dest, p src)
    | FloatAbs (dest, src) -> FloatAbs (dest, p src)
    | FloatNeg (dest, src) -> FloatNeg (dest, p src)
    | Int64ToFloat (dest, src) -> Int64ToFloat (dest, p src)
    | FloatToInt64 (dest, src) -> FloatToInt64 (dest, p src)
    | FloatToBits (dest, src) -> FloatToBits (dest, p src)
    | RefCountIncString str -> RefCountIncString (p str)
    | RefCountDecString str -> RefCountDecString (p str)
    | RandomInt64 dest -> RandomInt64 dest
    | DateNow dest -> DateNow dest
    | FloatToString (dest, value) -> FloatToString (dest, p value)
    | RuntimeError message -> RuntimeError message
    | CoverageHit exprId -> CoverageHit exprId

/// Apply copy propagation to terminator
let propagateCopyTerminator (copies: CopyMap) (term: Terminator) : Terminator =
    let p = propagateCopyOperand copies
    match term with
    | Ret op -> Ret (p op)
    | Branch (cond, trueLabel, falseLabel) -> Branch (p cond, trueLabel, falseLabel)
    | Jump label -> Jump label

/// Apply copy propagation to CFG
let applyCopyPropagation (cfg: CFG) : CFG * bool =
    let copies = buildCopyMap cfg

    if Map.isEmpty copies then
        (cfg, false)
    else
        // Track if any actual changes were made
        let mutable changed = false

        let blocks' =
            cfg.Blocks
            |> Map.map (fun _ block ->
                let instrs' =
                    block.Instrs
                    |> List.map (fun instr ->
                        let instr' = propagateCopyInstr copies instr
                        if instr' <> instr then changed <- true
                        instr'
                    )
                let term' = propagateCopyTerminator copies block.Terminator
                if term' <> block.Terminator then changed <- true
                { block with Instrs = instrs'; Terminator = term' }
            )
        ({ cfg with Blocks = blocks' }, changed)

/// CFG Simplification: Remove empty blocks (just a jump)
let simplifyEmptyBlocks (cfg: CFG) : CFG * bool =
    // Find blocks that only contain a Jump
    let emptyBlocks =
        cfg.Blocks
        |> Map.filter (fun label block ->
            label <> cfg.Entry &&  // Don't remove entry block
            List.isEmpty block.Instrs &&
            match block.Terminator with
            | Jump _ -> true
            | _ -> false
        )
        |> Map.map (fun _ block ->
            match block.Terminator with
            | Jump target -> target
            | _ -> Crash.crash "Expected Jump"
        )

    if Map.isEmpty emptyBlocks then
        (cfg, false)
    else
        // Redirect jumps through empty blocks (follow chains)
        let redirectLabel label =
            let rec follow visited current =
                if Set.contains current visited then
                    current
                else
                    match Map.tryFind current emptyBlocks with
                    | None -> current
                    | Some next -> follow (Set.add current visited) next
            follow Set.empty label

        let blocks' =
            cfg.Blocks
            |> Map.filter (fun label _ -> not (Map.containsKey label emptyBlocks))
            |> Map.map (fun _ block ->
                let term' =
                    match block.Terminator with
                    | Jump target -> Jump (redirectLabel target)
                    | Branch (cond, trueLabel, falseLabel) ->
                        Branch (cond, redirectLabel trueLabel, redirectLabel falseLabel)
                    | Ret op -> Ret op

                // Also update phi sources
                let instrs' =
                    block.Instrs
                    |> List.map (fun instr ->
                        match instr with
                        | Phi (dest, sources, valueType) ->
                            let sources' = sources |> List.map (fun (op, lbl) -> (op, redirectLabel lbl))
                            Phi (dest, sources', valueType)
                        | other -> other
                    )

                { block with Instrs = instrs'; Terminator = term' }
            )

        ({ cfg with Blocks = blocks' }, true)

/// Simplify join blocks that only return a phi-selected value.
/// Pattern:
///   pred1: ...; Jump join
///   pred2: ...; Jump join
///   join:
///     p <- Phi([(v1, pred1), (v2, pred2)])
///     Ret p
/// Becomes:
///   pred1: ...; Ret v1
///   pred2: ...; Ret v2
/// and removes `join`.
let simplifyRetPhiJoins (cfg: CFG) : CFG * bool =
    let preds = buildPredecessors cfg

    let candidateMappings : Map<Label, Map<Label, Operand>> =
        cfg.Blocks
        |> Map.toList
        |> List.choose (fun (joinLabel, joinBlock) ->
            match joinBlock.Instrs, joinBlock.Terminator with
            | [Phi (phiDest, sources, _)], Ret (Register retReg) when phiDest = retReg ->
                let predLabels = Map.tryFind joinLabel preds |> Option.defaultValue []
                let predSet = predLabels |> Set.ofList
                let sourceSet = sources |> List.map snd |> Set.ofList

                // Require exact predecessor/source match and direct jumps to join.
                let allJumpToJoin =
                    predLabels
                    |> List.forall (fun predLabel ->
                        match Map.tryFind predLabel cfg.Blocks with
                        | Some predBlock ->
                            match predBlock.Terminator with
                            | Jump target -> target = joinLabel
                            | _ -> false
                        | None -> false)

                if predSet = sourceSet && allJumpToJoin then
                    let sourceMap = sources |> List.map (fun (op, lbl) -> (lbl, op)) |> Map.ofList
                    Some (joinLabel, sourceMap)
                else
                    None
            | _ ->
                None)
        |> Map.ofList

    if Map.isEmpty candidateMappings then
        (cfg, false)
    else
        let allJoinLabels = candidateMappings |> Map.keys |> Set.ofSeq
        let allPredLabels =
            candidateMappings
            |> Map.values
            |> Seq.collect Map.keys
            |> Set.ofSeq

        let blocks' =
            cfg.Blocks
            |> Map.filter (fun label _ -> not (Set.contains label allJoinLabels))
            |> Map.map (fun label block ->
                if Set.contains label allPredLabels then
                    // A predecessor may feed multiple candidate joins only in impossible CFGs
                    // (single terminator), so pick the matching join by current terminator.
                    match block.Terminator with
                    | Jump target ->
                        match Map.tryFind target candidateMappings with
                        | Some sourceMap ->
                            match Map.tryFind label sourceMap with
                            | Some retOp ->
                                { block with Terminator = Ret retOp }
                            | None ->
                                block
                        | None ->
                            block
                    | _ ->
                        block
                else
                    block)

        ({ cfg with Blocks = blocks' }, true)

/// Simplify branches with constant boolean conditions
let simplifyConstantBranches (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let term' =
                match block.Terminator with
                | Branch (BoolConst true, trueLabel, _) -> Jump trueLabel
                | Branch (BoolConst false, _, falseLabel) -> Jump falseLabel
                | other -> other
            let changed' = ch || term' <> block.Terminator
            (Map.add label { block with Terminator = term' } acc, changed')
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Remove unreachable blocks and trim phi sources from unreachable predecessors
let eliminateUnreachableBlocks (cfg: CFG) : CFG * bool =
    let succs = buildSuccessors cfg

    let rec walk (work: Label list) (visited: Set<Label>) : Set<Label> =
        match work with
        | [] -> visited
        | label :: rest ->
            if Set.contains label visited then
                walk rest visited
            else
                let next = Map.tryFind label succs |> Option.defaultValue []
                walk (next @ rest) (Set.add label visited)

    let reachable = walk [cfg.Entry] Set.empty

    let reachableBlocks =
        cfg.Blocks
        |> Map.filter (fun label _ -> Set.contains label reachable)

    let (blocks', phiChanged) =
        reachableBlocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match instr with
                    | Phi (dest, sources, valueType) ->
                        let sources' = sources |> List.filter (fun (_, srcLabel) -> Set.contains srcLabel reachable)
                        if List.isEmpty sources' then
                            Crash.crash $"Phi in {label} has no reachable sources after CFG prune"
                        let instr' = Phi (dest, sources', valueType)
                        (acc' @ [instr'], ch' || sources' <> sources)
                    | _ ->
                        (acc' @ [instr], ch')
                ) ([], false)
            (Map.add label { block with Instrs = instrs' } acc, ch || instrChanged)
        ) (Map.empty, false)

    let removedBlocks = Map.count cfg.Blocks <> Map.count blocks'
    ({ cfg with Blocks = blocks' }, removedBlocks || phiChanged)

/// Truncate a 64-bit value to the appropriate integer type width
/// This ensures proper overflow/wraparound behavior for smaller integer types
let truncateToType (value: int64) (opType: AST.Type) : int64 =
    match opType with
    | AST.TInt8 -> int64 (int8 value)      // Truncate to signed 8-bit
    | AST.TInt16 -> int64 (int16 value)    // Truncate to signed 16-bit
    | AST.TInt32 -> int64 (int32 value)    // Truncate to signed 32-bit
    | AST.TUInt8 -> int64 (uint8 value)    // Truncate to unsigned 8-bit
    | AST.TUInt16 -> int64 (uint16 value)  // Truncate to unsigned 16-bit
    | AST.TUInt32 -> int64 (uint32 value)  // Truncate to unsigned 32-bit
    | _ -> value                            // Int64/UInt64 and other types: no truncation

/// Euclidean modulo: result has the sign of the divisor
let euclideanMod (a: int64) (b: int64) : int64 =
    let remainder = a % b
    if remainder = 0L then 0L
    elif (remainder > 0L && b < 0L) || (remainder < 0L && b > 0L) then remainder + b
    else remainder

/// Constant Folding for MIR
/// Evaluate operations on constants at compile time
let tryFoldBinOp (op: BinOp) (left: Operand) (right: Operand) (opType: AST.Type) : Operand option =
    match op, left, right with
    // Integer arithmetic - apply truncation for proper overflow behavior
    | Add, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a + b) opType))
    | Sub, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a - b) opType))
    | Mul, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a * b) opType))
    // Division: avoid divide by zero and INT64_MIN / -1 overflow
    | Div, Int64Const a, Int64Const b when b <> 0L && not (a = System.Int64.MinValue && b = -1L) -> Some (Int64Const (truncateToType (a / b) opType))
    | Mod, Int64Const a, Int64Const b when b > 0L -> Some (Int64Const (truncateToType (euclideanMod a b) opType))

    // Comparisons
    | Eq, Int64Const a, Int64Const b -> Some (BoolConst (a = b))
    | Neq, Int64Const a, Int64Const b -> Some (BoolConst (a <> b))
    | Lt, Int64Const a, Int64Const b -> Some (BoolConst (a < b))
    | Gt, Int64Const a, Int64Const b -> Some (BoolConst (a > b))
    | Lte, Int64Const a, Int64Const b -> Some (BoolConst (a <= b))
    | Gte, Int64Const a, Int64Const b -> Some (BoolConst (a >= b))

    // Boolean operations
    | And, BoolConst a, BoolConst b -> Some (BoolConst (a && b))
    | Or, BoolConst a, BoolConst b -> Some (BoolConst (a || b))

    // Algebraic identities
    | Add, Int64Const 0L, x -> Some x
    | Add, x, Int64Const 0L -> Some x
    | Sub, x, Int64Const 0L -> Some x
    | Sub, x, y when x = y -> Some (Int64Const 0L)  // x - x = 0
    | Mul, Int64Const 1L, x -> Some x
    | Mul, x, Int64Const 1L -> Some x
    | Mul, Int64Const 0L, _ -> Some (Int64Const 0L)
    | Mul, _, Int64Const 0L -> Some (Int64Const 0L)
    | Mul, Int64Const -1L, x -> None  // Could transform to Neg, but need instruction change
    | Mul, x, Int64Const -1L -> None  // Could transform to Neg
    | Div, x, Int64Const 1L -> Some x
    | Div, x, y when x = y && y <> Int64Const 0L -> Some (Int64Const 1L)  // x / x = 1 (if x != 0)
    | Mod, _, Int64Const 1L -> Some (Int64Const 0L)  // x % 1 = 0

    // Bitwise identities
    | BitAnd, Int64Const 0L, _ -> Some (Int64Const 0L)
    | BitAnd, _, Int64Const 0L -> Some (Int64Const 0L)
    | BitAnd, Int64Const -1L, x -> Some x  // -1 = all bits set
    | BitAnd, x, Int64Const -1L -> Some x
    | BitAnd, x, y when x = y -> Some x  // x & x = x
    | BitOr, Int64Const 0L, x -> Some x
    | BitOr, x, Int64Const 0L -> Some x
    | BitOr, Int64Const -1L, _ -> Some (Int64Const -1L)
    | BitOr, _, Int64Const -1L -> Some (Int64Const -1L)
    | BitOr, x, y when x = y -> Some x  // x | x = x
    | BitXor, Int64Const 0L, x -> Some x
    | BitXor, x, Int64Const 0L -> Some x
    | BitXor, x, y when x = y -> Some (Int64Const 0L)  // x ^ x = 0

    // Shift identities
    | Shl, x, Int64Const 0L -> Some x  // x << 0 = x
    | Shr, x, Int64Const 0L -> Some x  // x >> 0 = x
    | Shl, Int64Const 0L, _ -> Some (Int64Const 0L)  // 0 << n = 0
    | Shr, Int64Const 0L, _ -> Some (Int64Const 0L)  // 0 >> n = 0

    // Boolean short-circuit
    | And, BoolConst false, _ -> Some (BoolConst false)
    | And, _, BoolConst false -> Some (BoolConst false)
    | And, BoolConst true, x -> Some x
    | And, x, BoolConst true -> Some x
    | Or, BoolConst true, _ -> Some (BoolConst true)
    | Or, _, BoolConst true -> Some (BoolConst true)
    | Or, BoolConst false, x -> Some x
    | Or, x, BoolConst false -> Some x

    | _ -> None

/// Common Subexpression Elimination (CSE)
/// Detect identical computations and replace with reference to first result

/// Expression key for CSE - represents a pure computation
type ExprKey =
    | BinExpr of BinOp * Operand * Operand * AST.Type
    | UnaryExpr of UnaryOp * Operand

/// Check if a binary operation is commutative (order of operands doesn't matter)
let isCommutative (op: BinOp) : bool =
    match op with
    | Add | Mul | And | Or | Eq | Neq | BitAnd | BitOr | BitXor -> true
    | Sub | Div | Mod | Lt | Gt | Lte | Gte | Shl | Shr -> false

/// Normalize operand order for commutative operations (for consistent hashing)
let normalizeOperands (op: BinOp) (left: Operand) (right: Operand) : Operand * Operand =
    if isCommutative op then
        // Use structural comparison to ensure consistent ordering
        if compare left right <= 0 then (left, right) else (right, left)
    else
        (left, right)

/// Build expression key for a BinOp
let makeBinExprKey (op: BinOp) (left: Operand) (right: Operand) (opType: AST.Type) : ExprKey =
    let (l, r) = normalizeOperands op left right
    BinExpr (op, l, r, opType)

/// Build expression key for a UnaryOp
let makeUnaryExprKey (op: UnaryOp) (src: Operand) : ExprKey =
    UnaryExpr (op, src)

/// Apply CSE to a CFG
/// Note: This is a local CSE within each basic block (not global)
let applyCSE (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            // For each block, track expressions we've seen
            let (instrs', _, instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', exprMap: Map<ExprKey, VReg>, ch') instr ->
                    match instr with
                    | BinOp (dest, op, left, right, opType) ->
                        let key = makeBinExprKey op left right opType
                        match Map.tryFind key exprMap with
                        | Some prevDest ->
                            // Found a previous computation - replace with copy
                            let copy = Mov (dest, Register prevDest, None)
                            (acc' @ [copy], exprMap, true)
                        | None ->
                            // New expression - add to map
                            let exprMap' = Map.add key dest exprMap
                            (acc' @ [instr], exprMap', ch')
                    | UnaryOp (dest, op, src) ->
                        let key = makeUnaryExprKey op src
                        match Map.tryFind key exprMap with
                        | Some prevDest ->
                            // Found a previous computation - replace with copy
                            let copy = Mov (dest, Register prevDest, None)
                            (acc' @ [copy], exprMap, true)
                        | None ->
                            // New expression - add to map
                            let exprMap' = Map.add key dest exprMap
                            (acc' @ [instr], exprMap', ch')
                    | _ ->
                        (acc' @ [instr], exprMap, ch')
                ) ([], Map.empty, false)

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Try to fold a unary operation on a constant
let tryFoldUnaryOp (op: UnaryOp) (src: Operand) : Operand option =
    match op, src with
    | Neg, Int64Const n -> Some (Int64Const (-n))
    | Not, BoolConst b -> Some (BoolConst (not b))
    | _ -> None

/// Apply constant folding to a CFG
let applyConstantFolding (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match instr with
                    | BinOp (dest, op, left, right, opType) ->
                        match tryFoldBinOp op left right opType with
                        | Some result ->
                            (acc' @ [Mov (dest, result, None)], true)
                        | None ->
                            (acc' @ [instr], ch')
                    | UnaryOp (dest, op, src) ->
                        match tryFoldUnaryOp op src with
                        | Some result ->
                            (acc' @ [Mov (dest, result, None)], true)
                        | None ->
                            (acc' @ [instr], ch')
                    | _ ->
                        (acc' @ [instr], ch')
                ) ([], false)

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Run all optimizations in a single pass (returns whether anything changed)
let optimizeCFGOnce (options: OptimizeOptions) (cfg: CFG) : CFG * bool =
    let (cfg1, changed1) =
        if options.EnableConstFolding then applyConstantFolding cfg else (cfg, false)
    let (cfg2, changed2) =
        if options.EnableCSE then applyCSE cfg1 else (cfg1, false)
    let (cfg3, changed3) =
        if options.EnableCopyProp then applyCopyPropagation cfg2 else (cfg2, false)
    // Run constant folding again after copy propagation
    // This catches cases like: v1 = -127; v2 = v1 - 2
    // After copy prop: v2 = Int64Const(-127) - Int64Const(2) -> can fold
    let (cfg4, changed4) =
        if options.EnableConstFolding then applyConstantFolding cfg3 else (cfg3, false)
    let (cfg5, changed5) =
        if options.EnableLICM then applyLoopInvariantCodeMotion cfg4 else (cfg4, false)
    let (cfg6, changed6) =
        if options.EnableDCE then eliminateDeadCode cfg5 else (cfg5, false)
    let (cfg7, changed7) =
        if options.EnableCFGSimplify then simplifyConstantBranches cfg6 else (cfg6, false)
    let (cfg8, changed8) =
        if options.EnableCFGSimplify then eliminateUnreachableBlocks cfg7 else (cfg7, false)
    let (cfg9, changed9) =
        if options.EnableCFGSimplify then simplifyRetPhiJoins cfg8 else (cfg8, false)
    let (cfg10, changed10) =
        if options.EnableCFGSimplify then simplifyEmptyBlocks cfg9 else (cfg9, false)
    let changed = changed1 || changed2 || changed3 || changed4 || changed5 || changed6 || changed7 || changed8 || changed9 || changed10
    (cfg10, changed)

/// Run all optimizations until fixed point
let optimizeCFGWithOptions (options: OptimizeOptions) (cfg: CFG) : CFG =
    let rec loop current remaining =
        if remaining <= 0 then
            current
        else
            let (next, changed) = optimizeCFGOnce options current
            if changed then
                loop next (remaining - 1)
            else
                next
    loop cfg 10

let optimizeCFG (cfg: CFG) : CFG =
    optimizeCFGWithOptions defaultOptimizeOptions cfg

/// Optimize a function
let optimizeFunctionWithOptions (options: OptimizeOptions) (func: Function) : Function =
    let cfg' = optimizeCFGWithOptions options func.CFG
    { func with CFG = cfg' }

let optimizeFunction (func: Function) : Function =
    let cfg' = optimizeCFG func.CFG
    { func with CFG = cfg' }

/// Optimize a program
let optimizeProgramWithOptions (options: OptimizeOptions) (program: Program) : Program =
    let (Program (functions, variants, records)) = program
    let functions' = functions |> List.map (optimizeFunctionWithOptions options)
    Program (functions', variants, records)

let optimizeProgram (program: Program) : Program =
    optimizeProgramWithOptions defaultOptimizeOptions program

let optimizeConstFolding (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = true
            EnableCSE = false
            EnableCopyProp = false
            EnableDCE = false
            EnableCFGSimplify = false
            EnableLICM = false }
        program

let optimizeCopyProp (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableCSE = false
            EnableCopyProp = true
            EnableDCE = false
            EnableCFGSimplify = false
            EnableLICM = false }
        program

let optimizeDCE (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableCSE = false
            EnableCopyProp = false
            EnableDCE = true
            EnableCFGSimplify = false
            EnableLICM = false }
        program
