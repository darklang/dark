// 5.5_FunctionTreeShaking.fs - Function Tree Shaking Pass
//
// Prunes unused user and stdlib functions by walking call graphs.
// This keeps code generation and binary size focused on reachable functions.

module FunctionTreeShaking

/// Build root set for user reachability (explicit entry when provided)
let private buildUserRoots (entryName: string option) (functions: LIR.Function list) : Set<string> =
    match entryName with
    | Some name ->
        if functions |> List.exists (fun f -> f.Name = name) then
            Set.ofList [name]
        else
            Crash.crash $"FunctionTreeShaking: entry '{name}' not found in user functions"
    | None ->
        functions |> List.map (fun f -> f.Name) |> Set.ofList

/// Filter user functions to only include reachable ones
let filterUserFunctions (entryName: string option) (functions: LIR.Function list) : LIR.Function list =
    let roots = buildUserRoots entryName functions
    let userCallGraph = DeadCodeElimination.buildCallGraph functions
    let reachableNames = DeadCodeElimination.findReachable userCallGraph roots
    functions |> List.filter (fun f -> Set.contains f.Name reachableNames)

/// Filter stdlib functions to only include those reachable from user code
let filterStdlibFunctions
    (stdlibCallGraph: Map<string, Set<string>>)
    (userFunctions: LIR.Function list)
    (stdlibFunctions: LIR.Function list)
    : LIR.Function list =
    DeadCodeElimination.filterFunctions stdlibCallGraph userFunctions stdlibFunctions

/// Compute reachable stdlib function names from a user ANF program
let getReachableStdlibNames
    (stdlibCallGraph: Map<string, Set<string>>)
    (userProgram: ANF.Program)
    : Set<string> =
    let (ANF.Program (userFuncs, userMainExpr)) = userProgram
    let startFunc = { ANF.Name = "_start"
                      ANF.TypedParams = []
                      ANF.ReturnType = AST.TUnit
                      ANF.ReturnOwnership = ANF.OwnedReturn
                      ANF.Body = userMainExpr }
    let userFuncsWithStart = startFunc :: userFuncs
    ANFDeadCodeElimination.getReachableStdlib stdlibCallGraph userFuncsWithStart
