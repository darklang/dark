// DeadCodeElimination.fs - Dead Code Elimination (Tree Shaking)
//
// Filters out unused stdlib functions based on call graph reachability.
// This reduces CodeGen work by only processing functions that are actually used.

module DeadCodeElimination

/// Get the display string helper for lists used by PrintSum codegen
let private getListDisplayStringFunc (elemType: AST.Type) : string option =
    match elemType with
    | AST.TInt64 -> Some "Stdlib.List.toDisplayString_i64"
    | AST.TBool -> Some "Stdlib.List.toDisplayString_bool"
    | AST.TString -> Some "Stdlib.List.toDisplayString_str"
    | AST.TFloat64 -> Some "Stdlib.List.toDisplayString_f64"
    | _ -> None

/// Extract function names from an operand
let private extractFromOperand (op: LIR.Operand) : string list =
    match op with
    | LIR.FuncAddr name -> [name]
    | _ -> []

/// Extract function names from a single instruction
let private extractCallsFromInstr (instr: LIR.Instr) : string list =
    match instr with
    | LIR.Call (_, funcName, args) ->
        funcName :: (args |> List.collect extractFromOperand)
    | LIR.TailCall (funcName, args) ->
        funcName :: (args |> List.collect extractFromOperand)
    | LIR.IndirectTailCall (_, args) ->
        // Function pointer is in a register - we can't statically determine the target
        args |> List.collect extractFromOperand
    | LIR.ClosureAlloc (_, funcName, captures) ->
        funcName :: (captures |> List.collect extractFromOperand)
    | LIR.ClosureTailCall (_, args) ->
        // Closure pointer is in a register - we can't statically determine the target
        args |> List.collect extractFromOperand
    | LIR.LoadFuncAddr (_, funcName) -> [funcName]
    | LIR.PrintSum (_, variants) ->
        variants
        |> List.collect (fun (_, _, payloadType) ->
            match payloadType with
            | Some (AST.TList elemType) ->
                match getListDisplayStringFunc elemType with
                | Some funcName -> [funcName]
                | None -> []
            | _ -> [])
    | LIR.HeapStore (_, _, src, _) -> extractFromOperand src
    | LIR.Mov (_, src) -> extractFromOperand src
    | LIR.StringConcat (_, left, right) ->
        extractFromOperand left @ extractFromOperand right
    | _ -> []

/// Extract function names called from a LIR function
let getCalledFunctions (func: LIR.Function) : Set<string> =
    func.CFG.Blocks
    |> Map.toSeq
    |> Seq.collect (fun (_, block) -> block.Instrs)
    |> Seq.collect extractCallsFromInstr
    |> Set.ofSeq

/// Build call graph from list of functions
let buildCallGraph (funcs: LIR.Function list) : Map<string, Set<string>> =
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

/// Filter functions to only include reachable ones
let filterFunctions (callGraph: Map<string, Set<string>>)
                    (userFuncs: LIR.Function list)
                    (stdlibFuncs: LIR.Function list) : LIR.Function list =
    // Get all functions called from user code
    let userCalls =
        userFuncs
        |> List.collect (fun f -> getCalledFunctions f |> Set.toList)
        |> Set.ofList
    // Expand to transitive closure
    let reachable = findReachable callGraph userCalls
    // Filter stdlib to only reachable
    stdlibFuncs |> List.filter (fun f -> Set.contains f.Name reachable)
