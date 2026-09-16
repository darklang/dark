// ParallelMoves.fs - Parallel move resolution algorithm
//
// This module implements the parallel move resolution algorithm used for:
// - TailArgMoves in code generation
// - Phi resolution in SSA-based register allocation
//
// The algorithm correctly sequences parallel moves to avoid clobbering values.
// It handles:
// - Simple moves (no conflict)
// - Chain moves (must reorder)
// - Cycles (need temp register)
// - Self-moves (eliminated as no-ops)

module ParallelMoves

/// Result of parallel move resolution - actions to perform in order
type MoveAction<'Reg, 'Src> =
    | SaveToTemp of 'Reg           // Save register to temp (before cycle)
    | Move of dest:'Reg * src:'Src // Regular move
    | MoveFromTemp of 'Reg         // Move from temp to dest (end of cycle)

/// Resolve parallel moves into a sequence of actions
///
/// Parameters:
/// - moves: List of (dest, src) pairs representing parallel moves
/// - getSrcReg: Function to extract source register from src if it's a register (None for immediates, stack slots, etc.)
///
/// Returns: List of actions to perform in order to correctly implement the parallel moves
let resolve<'Reg, 'Src when 'Reg : equality and 'Reg : comparison and 'Src : equality>
    (moves: ('Reg * 'Src) list)
    (getSrcReg: 'Src -> 'Reg option)
    : MoveAction<'Reg, 'Src> list =

    // Filter out self-loops (X1 <- X1) since they're no-ops
    let isSelfLoop (destReg: 'Reg, srcOp: 'Src) =
        match getSrcReg srcOp with
        | Some srcReg -> srcReg = destReg
        | None -> false

    let nonSelfLoops = moves |> List.filter (not << isSelfLoop)

    if List.isEmpty nonSelfLoops then
        []
    else
        let mutable allActions : MoveAction<'Reg, 'Src> list = []
        let mutable remaining = nonSelfLoops

        // Collect all source registers used by register-source moves
        let allRegSrcRegs =
            nonSelfLoops
            |> List.choose (fun (_, srcOp) -> getSrcReg srcOp)
            |> Set.ofList

        // Phase 1: Emit non-register-source moves whose destination is NOT used as a source
        // IMPORTANT: A move like X0 <- Imm cannot be emitted early if X0 is a source for another move!
        let (nonRegMoves, regMoves) =
            nonSelfLoops |> List.partition (fun (_, srcOp) -> getSrcReg srcOp = None)

        let (safeNonRegMoves, unsafeNonRegMoves) =
            nonRegMoves |> List.partition (fun (dest, _) -> not (Set.contains dest allRegSrcRegs))

        for (dest, src) in safeNonRegMoves do
            allActions <- allActions @ [Move (dest, src)]

        remaining <- unsafeNonRegMoves @ regMoves

        // Phase 2: Iteratively emit moves where dest is not a source for remaining moves
        let mutable changed = true
        while changed && not (List.isEmpty remaining) do
            changed <- false
            let remainingSrcs =
                remaining
                |> List.choose (fun (_, srcOp) -> getSrcReg srcOp)
                |> Set.ofList

            let (safe, unsafe) =
                remaining |> List.partition (fun (destReg, _) -> not (Set.contains destReg remainingSrcs))

            if not (List.isEmpty safe) then
                changed <- true
                for (dest, src) in safe do
                    allActions <- allActions @ [Move (dest, src)]
                remaining <- unsafe

        // Phase 3: Handle cycles using temp register
        // At this point, all remaining moves form cycles. For each cycle:
        // 1. Save the FIRST destination to temp (it gets clobbered first but read later)
        // 2. Emit all moves in DEPENDENCY ORDER (so we read from registers before they're overwritten)
        // 3. Any move that reads the saved register uses temp instead
        //
        // Example cycle: X0 <- X1, X1 <- X2, X2 <- X0
        // 1. Save X0 to temp (X0 is written first but X2 <- X0 reads it later)
        // 2. Emit in order: X0 <- X1, X1 <- X2, X2 <- temp

        while not (List.isEmpty remaining) do
            // Pick the first move and save its destination
            let (firstDest, _) = remaining.Head
            let savedReg = firstDest

            // Save this register to temp
            allActions <- allActions @ [SaveToTemp savedReg]

            // Build ordered chain starting from savedReg:
            // 1. Find move that writes to savedReg (the first move to emit)
            // 2. Get its source register
            // 3. Find move that writes to that source register
            // 4. Repeat until we find a move that reads savedReg (end of cycle)
            let rec buildOrderedChain (currentDest: 'Reg) (movesLeft: ('Reg * 'Src) list)
                                      (chain: ('Reg * 'Src) list) : ('Reg * 'Src) list =
                match movesLeft |> List.tryFind (fun (d, _) -> d = currentDest) with
                | Some ((dest, src) as move) ->
                    let movesLeft' = movesLeft |> List.filter (fun m -> m <> move)
                    let newChain = move :: chain
                    match getSrcReg src with
                    | Some srcReg when srcReg <> savedReg ->
                        // Continue following the chain
                        buildOrderedChain srcReg movesLeft' newChain
                    | _ ->
                        // End of cycle (source is savedReg or not a register)
                        newChain
                | None ->
                    // No more moves to this destination
                    chain

            let orderedMoves = buildOrderedChain savedReg remaining [] |> List.rev

            // Emit moves in order, using MoveFromTemp for moves that read savedReg
            for (dest, src) in orderedMoves do
                match getSrcReg src with
                | Some srcReg when srcReg = savedReg ->
                    // Use temp instead of the saved register
                    allActions <- allActions @ [MoveFromTemp dest]
                | _ ->
                    allActions <- allActions @ [Move (dest, src)]

            remaining <- remaining |> List.filter (fun m -> not (List.contains m orderedMoves))

        allActions
