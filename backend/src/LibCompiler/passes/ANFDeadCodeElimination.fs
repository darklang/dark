// ANFDeadCodeElimination.fs - ANF-level Dead Code Elimination
//
// Extracts call graph from ANF functions and determines reachability.
// Used for stdlib tree-shaking and coverage without re-compiling stdlib.

module ANFDeadCodeElimination

/// Extract function names from an atom
let private extractFromAtom (atom: ANF.Atom) : string list =
    match atom with
    | ANF.FuncRef name -> [name]
    | _ -> []

/// Extract function names from a complex expression
let private extractFromCExpr (cexpr: ANF.CExpr) : string list =
    match cexpr with
    | ANF.Call (funcName, args) ->
        funcName :: (args |> List.collect extractFromAtom)
    | ANF.BorrowedCall (funcName, args) ->
        funcName :: (args |> List.collect extractFromAtom)
    | ANF.TailCall (funcName, args) ->
        funcName :: (args |> List.collect extractFromAtom)
    | ANF.IndirectCall (func, args) ->
        extractFromAtom func @ (args |> List.collect extractFromAtom)
    | ANF.IndirectTailCall (func, args) ->
        extractFromAtom func @ (args |> List.collect extractFromAtom)
    | ANF.ClosureAlloc (funcName, captures) ->
        funcName :: (captures |> List.collect extractFromAtom)
    | ANF.ClosureCall (closure, args) ->
        extractFromAtom closure @ (args |> List.collect extractFromAtom)
    | ANF.ClosureTailCall (closure, args) ->
        extractFromAtom closure @ (args |> List.collect extractFromAtom)
    | ANF.Atom atom -> extractFromAtom atom
    | ANF.TypedAtom (atom, _) -> extractFromAtom atom
    | ANF.Prim (_, left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.UnaryPrim (_, atom) -> extractFromAtom atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        extractFromAtom cond @ extractFromAtom thenVal @ extractFromAtom elseVal
    | ANF.TupleAlloc atoms -> atoms |> List.collect extractFromAtom
    | ANF.TupleGet (tuple, _) -> extractFromAtom tuple
    | ANF.StringConcat (left, right) ->
        extractFromAtom left @ extractFromAtom right
    | ANF.RefCountInc (atom, _, _) -> extractFromAtom atom
    | ANF.RefCountDec (atom, _, _) -> extractFromAtom atom
    | ANF.Print (atom, _) -> extractFromAtom atom
    | ANF.FileReadText path -> extractFromAtom path
    | ANF.FileExists path -> extractFromAtom path
    | ANF.FileWriteText (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FileAppendText (path, content) ->
        extractFromAtom path @ extractFromAtom content
    | ANF.FileDelete path -> extractFromAtom path
    | ANF.FileSetExecutable path -> extractFromAtom path
    | ANF.FileWriteFromPtr (path, ptr, length) ->
        extractFromAtom path @ extractFromAtom ptr @ extractFromAtom length
    | ANF.FloatSqrt atom -> extractFromAtom atom
    | ANF.FloatAbs atom -> extractFromAtom atom
    | ANF.FloatNeg atom -> extractFromAtom atom
    | ANF.Int64ToFloat atom -> extractFromAtom atom
    | ANF.FloatToInt64 atom -> extractFromAtom atom
    | ANF.FloatToBits atom -> extractFromAtom atom
    | ANF.RawAlloc numBytes -> extractFromAtom numBytes
    | ANF.RawFree ptr -> extractFromAtom ptr
    | ANF.RawGet (ptr, offset, _) ->
        extractFromAtom ptr @ extractFromAtom offset
    | ANF.RawGetByte (ptr, offset) ->
        extractFromAtom ptr @ extractFromAtom offset
    | ANF.RawSet (ptr, offset, value, _) ->
        extractFromAtom ptr @ extractFromAtom offset @ extractFromAtom value
    | ANF.RawSetByte (ptr, offset, value) ->
        extractFromAtom ptr @ extractFromAtom offset @ extractFromAtom value
    | ANF.RefCountIncString atom -> extractFromAtom atom
    | ANF.RefCountDecString atom -> extractFromAtom atom
    | ANF.RandomInt64 -> []  // No atoms
    | ANF.DateNow -> []      // No atoms
    | ANF.FloatToString atom -> extractFromAtom atom
    | ANF.RuntimeError _ -> []  // No atoms

/// Extract function names from an ANF expression
let rec private extractFromAExpr (aexpr: ANF.AExpr) : string list =
    match aexpr with
    | ANF.Let (_, cexpr, body) ->
        extractFromCExpr cexpr @ extractFromAExpr body
    | ANF.Return atom -> extractFromAtom atom
    | ANF.If (cond, thenBranch, elseBranch) ->
        extractFromAtom cond @ extractFromAExpr thenBranch @ extractFromAExpr elseBranch

/// Extract function names called from an ANF function
let getCalledFunctions (func: ANF.Function) : Set<string> =
    extractFromAExpr func.Body |> Set.ofList

/// Build call graph from list of ANF functions
let buildCallGraph (funcs: ANF.Function list) : Map<string, Set<string>> =
    funcs
    |> List.map (fun f -> f.Name, getCalledFunctions f)
    |> Map.ofList

/// Compute transitive closure of reachable functions
let findReachable (callGraph: Map<string, Set<string>>) (roots: Set<string>) : Set<string> =
    let rec visit visited toVisit =
        if Set.isEmpty toVisit then visited
        else
            let name = Set.minElement toVisit
            let toVisit' = Set.remove name toVisit
            if Set.contains name visited then visit visited toVisit'
            else
                let visited' = Set.add name visited
                let calls = Map.tryFind name callGraph |> Option.defaultValue Set.empty
                let toVisit'' = Set.union toVisit' (Set.difference calls visited')
                visit visited' toVisit''
    visit Set.empty roots

/// Get the set of stdlib functions reachable from user functions
let getReachableStdlib (stdlibCallGraph: Map<string, Set<string>>)
                       (userFuncs: ANF.Function list) : Set<string> =
    // Get all functions called from user code
    let userCalls =
        userFuncs
        |> List.collect (fun f -> getCalledFunctions f |> Set.toList)
        |> Set.ofList
    // Expand to transitive closure
    findReachable stdlibCallGraph userCalls
