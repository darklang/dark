// 2.7_TailCallDetection.fs - Tail Call Detection Pass
//
// Detects tail calls in ANF and transforms them to tail call variants:
// - Call → TailCall
// - IndirectCall → IndirectTailCall
// - ClosureCall → ClosureTailCall
//
// A call is in tail position if:
// - It's in a Let binding where the body eventually returns the same variable
// - Both branches of an If are in tail position if the If itself is
//
// This runs AFTER RefCountInsertion, so RefCountDec operations may be inserted
// between the call and the return. We look through any RefCountDec operations
// to find the final Return. This is crucial because without TCO, functions like
// __reverseHelper would use regular calls instead of tail calls, causing the
// intermediate cons cells to be freed prematurely (leading to corrupted results
// when the free list reuses those cells for subsequent allocations).
//
// CURRENT STATUS: TCO is ENABLED. The DCE bug that caused 197 test failures
// has been fixed (DeadCodeElimination.fs was not recognizing TailCall as a
// function call, causing stdlib functions called via tail call to be removed).
//
// See docs/features/tail-call-optimization.md for detailed documentation.

module TailCallDetection

open ANF

/// Check if a CExpr is a RefCountDec operation
let isRefCountDec (cexpr: CExpr) : bool =
    match cexpr with
    | RefCountDec _ -> true
    | RefCountDecString _ -> true
    | _ -> false

/// Check if an expression eventually returns a specific TempId
/// Looks through any RefCountDec operations to find the final Return
let rec isReturnOf (tempId: TempId) (expr: AExpr) : bool =
    match expr with
    | Return (Var tid) when tid = tempId -> true
    | Let (_, cexpr, body) when isRefCountDec cexpr ->
        // RefCountDec followed by more expressions - look through it
        isReturnOf tempId body
    | _ -> false

/// Transform a Call to TailCall if it's in tail position
let convertToTailCall (cexpr: CExpr) : CExpr =
    match cexpr with
    | Call (funcName, args) -> TailCall (funcName, args)
    | BorrowedCall (funcName, args) -> TailCall (funcName, args)
    | IndirectCall (func, args) -> IndirectTailCall (func, args)
    | ClosureCall (closure, args) -> ClosureTailCall (closure, args)
    | _ -> cexpr

let private wrapBindings (bindings: (TempId * CExpr) list) (body: AExpr) : AExpr =
    List.foldBack (fun (tempId, cexpr) acc -> Let (tempId, cexpr, acc)) bindings body

let rec private resolveAliasRoot
    (aliasRoots: Map<TempId, TempId>)
    (tempId: TempId)
    (visited: Set<TempId>)
    : TempId =
    if Set.contains tempId visited then
        tempId
    else
        match Map.tryFind tempId aliasRoots with
        | Some next when next <> tempId ->
            resolveAliasRoot aliasRoots next (Set.add tempId visited)
        | _ ->
            tempId

let private canonicalTempId (aliasRoots: Map<TempId, TempId>) (tempId: TempId) : TempId =
    resolveAliasRoot aliasRoots tempId Set.empty

let private extendAliasRoots
    (aliasRoots: Map<TempId, TempId>)
    (tempId: TempId)
    (cexpr: CExpr)
    : Map<TempId, TempId> =
    let sourceAlias =
        match cexpr with
        | Atom (Var tid) -> Some tid
        | TypedAtom (Var tid, _) -> Some tid
        | _ -> None

    match sourceAlias with
    | Some sourceTid ->
        Map.add tempId (canonicalTempId aliasRoots sourceTid) aliasRoots
    | None ->
        aliasRoots

let private tailCallArgTempIds
    (aliasRoots: Map<TempId, TempId>)
    (cexpr: CExpr)
    : Set<TempId> =
    let fromAtom (atom: Atom) : Set<TempId> =
        match atom with
        | Var tid -> Set.singleton (canonicalTempId aliasRoots tid)
        | _ -> Set.empty
    match cexpr with
    | TailCall (_, args) ->
        args |> List.fold (fun acc atom -> Set.union acc (fromAtom atom)) Set.empty
    | IndirectTailCall (func, args) ->
        (fromAtom func, args)
        ||> List.fold (fun acc atom -> Set.union acc (fromAtom atom))
    | ClosureTailCall (closure, args) ->
        (fromAtom closure, args)
        ||> List.fold (fun acc atom -> Set.union acc (fromAtom atom))
    | _ ->
        Set.empty

let rec private collectMovableDecPrefix
    (aliasRoots: Map<TempId, TempId>)
    (tailArgTemps: Set<TempId>)
    (expr: AExpr)
    : (TempId * CExpr) list * AExpr =
    match expr with
    | Let (tmpId, RefCountDec (Var tid, size, kind), rest)
        when not (Set.contains (canonicalTempId aliasRoots tid) tailArgTemps) ->
        let (bindings, remaining) = collectMovableDecPrefix aliasRoots tailArgTemps rest
        ((tmpId, RefCountDec (Var tid, size, kind)) :: bindings, remaining)
    | Let (tmpId, RefCountDecString atom, rest) ->
        let overlaps =
            match atom with
            | Var tid -> Set.contains (canonicalTempId aliasRoots tid) tailArgTemps
            | _ -> false
        if overlaps then
            ([], expr)
        else
            let (bindings, remaining) = collectMovableDecPrefix aliasRoots tailArgTemps rest
            ((tmpId, RefCountDecString atom) :: bindings, remaining)
    | _ ->
        ([], expr)

let private isDirectReturnOf (tempId: TempId) (expr: AExpr) : bool =
    match expr with
    | Return (Var tid) when tid = tempId -> true
    | _ -> false

/// Check if a CExpr is a call (direct, indirect, or closure)
let isCallExpr (cexpr: CExpr) : bool =
    match cexpr with
    | Call _ | BorrowedCall _ | IndirectCall _ | ClosureCall _ -> true
    | _ -> false

/// Detect and transform tail calls in an expression.
/// The 'inTailPosition' parameter indicates if the current expression
/// is in tail position (its result is directly returned).
let rec detectTailCalls
    (currentFuncName: string)
    (inTailPosition: bool)
    (aliasRoots: Map<TempId, TempId>)
    (expr: AExpr)
    : AExpr =
    match expr with
    | Return atom ->
        // Return is always a base case - just return it
        Return atom

    | Let (tempId, cexpr, body) ->
        // Check if this is a tail call pattern:
        // Let (t, Call(...), Return (Var t))
        if inTailPosition && isCallExpr cexpr && isReturnOf tempId body then
            // This is a tail call! Convert the call to tail call variant
            let tailCall = convertToTailCall cexpr
            match tailCall with
            | TailCall (targetFunc, _) when targetFunc <> currentFuncName ->
                // For non-self tailcalls, execute movable cleanup decs before the tailcall.
                // Any dec left after a tailcall would be unreachable.
                let tailArgTemps = tailCallArgTempIds aliasRoots tailCall
                let (movableDecs, remainingBody) = collectMovableDecPrefix aliasRoots tailArgTemps body
                if isDirectReturnOf tempId remainingBody then
                    wrapBindings movableDecs (Let (tempId, tailCall, remainingBody))
                else
                    // Cleanup remains after the call (typically overlap with a tail argument),
                    // so keep a normal call to preserve the post-call unwind work.
                    let aliasRoots' = extendAliasRoots aliasRoots tempId cexpr
                    let body' = detectTailCalls currentFuncName inTailPosition aliasRoots' body
                    Let (tempId, cexpr, body')
            | TailCall (targetFunc, _) when targetFunc = currentFuncName ->
                // Self-tailcall lowering currently cannot preserve cleanup decrefs that overlap
                // tail arguments (the decref is required on unwind). Keep these as normal calls.
                let tailArgTemps = tailCallArgTempIds aliasRoots tailCall
                let (movableDecs, remainingBody) = collectMovableDecPrefix aliasRoots tailArgTemps body
                if isDirectReturnOf tempId remainingBody then
                    wrapBindings movableDecs (Let (tempId, tailCall, remainingBody))
                else
                    let aliasRoots' = extendAliasRoots aliasRoots tempId cexpr
                    let body' = detectTailCalls currentFuncName inTailPosition aliasRoots' body
                    Let (tempId, cexpr, body')
            | _ ->
                Let (tempId, tailCall, body)
        else
            // Not a tail call - recurse into body
            // Body is in tail position if current expression is
            let aliasRoots' = extendAliasRoots aliasRoots tempId cexpr
            let body' = detectTailCalls currentFuncName inTailPosition aliasRoots' body
            Let (tempId, cexpr, body')

    | If (cond, thenBranch, elseBranch) ->
        // If expression: both branches are in tail position if If is
        let thenBranch' = detectTailCalls currentFuncName inTailPosition aliasRoots thenBranch
        let elseBranch' = detectTailCalls currentFuncName inTailPosition aliasRoots elseBranch
        If (cond, thenBranch', elseBranch')

/// Detect tail calls in a function
let detectTailCallsInFunction (func: Function) : Function =
    // Function body is always in tail position
    let body' = detectTailCalls func.Name true Map.empty func.Body
    { func with Body = body' }

/// Detect tail calls in a program
let detectTailCallsInProgram (program: ANF.Program) : ANF.Program =
    // TCO is ENABLED - the DCE bug that caused 197 test failures has been fixed
    // (DeadCodeElimination.fs was not recognizing TailCall as a function call)
    let (ANF.Program (functions, main)) = program
    ANF.Program (functions |> List.map detectTailCallsInFunction, main)
