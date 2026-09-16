// 2.6_PrintInsertion.fs - Print Insertion Pass
//
// Inserts a Print instruction at the end of the main expression.
// This ensures the program's result is printed before exiting.
//
// This pass runs after RC insertion and before ANF-to-MIR conversion.

module PrintInsertion

open ANF

/// Get the toDisplayString function name for list element type
let getListDisplayStringFunc (elemType: AST.Type) : string option =
    match elemType with
    | AST.TInt64 -> Some "Stdlib.List.toDisplayString_i64"
    | AST.TBool -> Some "Stdlib.List.toDisplayString_bool"
    | AST.TString -> Some "Stdlib.List.toDisplayString_str"
    | AST.TFloat64 -> Some "Stdlib.List.toDisplayString_f64"
    | _ -> None

/// Wrap the return value with a Print instruction
/// Transforms: Return atom  →  Let (_, Print (atom, type), Return atom)
/// For list types, generates: Call toDisplayString, then Print the string
let rec wrapReturnWithPrint (programType: AST.Type) (varGen: VarGen) (expr: AExpr) : AExpr * VarGen =
    let defaultPrintType =
        match programType with
        // Builtin.testRuntimeError has a bottom-like compile-time type.
        // Printing should stay concrete so downstream passes never see it.
        | AST.TRuntimeError -> AST.TUnit
        | _ -> programType

    match expr with
    | Return atom ->
        // Dead-code elimination can reduce a typed expression branch to `()`
        // (for example, `Builtin.testRuntimeError` in a selected match arm).
        // Printing must follow the runtime atom shape, not only the original program type.
        let printType =
            match atom with
            | ANF.UnitLiteral -> AST.TUnit
            | _ -> defaultPrintType

        // For list types, call toDisplayString first
        match printType with
        | AST.TSum ("Stdlib.Option.Option", [AST.TList elemType]) ->
            match getListDisplayStringFunc elemType with
            | Some toDisplayStringName ->
                // Keep the display helper reachable so tree shaking doesn't drop it.
                let (keepFunc, varGen1) = freshVar varGen
                let (printTmp, varGen2) = freshVar varGen1
                let keepExpr = Atom (FuncRef toDisplayStringName)
                let printExpr = Print (atom, printType)
                (Let (keepFunc, keepExpr, Let (printTmp, printExpr, Return atom)), varGen2)
            | None ->
                // Unsupported element type, fall back to simple print
                let (printTmp, varGen') = freshVar varGen
                (Let (printTmp, Print (atom, printType), Return atom), varGen')
        | AST.TList elemType ->
            match getListDisplayStringFunc elemType with
            | Some toDisplayStringName ->
                // Generate: let strTmp = Call(toDisplayString, [list]) in
                //           let _ = Print(strTmp, String) in Return atom
                let (strTmp, varGen1) = freshVar varGen
                let (printTmp, varGen2) = freshVar varGen1
                let callExpr = Call (toDisplayStringName, [atom])
                let printExpr = Print (Var strTmp, AST.TString)
                (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), varGen2)
            | None ->
                // Unsupported element type, fall back to simple print
                let (printTmp, varGen') = freshVar varGen
                (Let (printTmp, Print (atom, printType), Return atom), varGen')
        | AST.TFloat64 ->
            // For Float64, call Float.toString first, then print the string
            let (strTmp, varGen1) = freshVar varGen
            let (printTmp, varGen2) = freshVar varGen1
            let callExpr = Call ("Stdlib.Float.toString", [atom])
            let printExpr = Print (Var strTmp, AST.TString)
            (Let (strTmp, callExpr, Let (printTmp, printExpr, Return atom)), varGen2)
        | _ ->
            // Non-list types: simple print
            let (printTmp, varGen') = freshVar varGen
            (Let (printTmp, Print (atom, printType), Return atom), varGen')
    | Let (tempId, cexpr, body) ->
        // Recurse into body
        let (body', varGen') = wrapReturnWithPrint programType varGen body
        (Let (tempId, cexpr, body'), varGen')
    | If (cond, thenBranch, elseBranch) ->
        // Wrap both branches
        let (thenBranch', varGen1) = wrapReturnWithPrint programType varGen thenBranch
        let (elseBranch', varGen2) = wrapReturnWithPrint programType varGen1 elseBranch
        (If (cond, thenBranch', elseBranch'), varGen2)

/// Insert Print at the end of the main expression
let insertPrint (functions: ANF.Function list) (mainExpr: ANF.AExpr) (programType: AST.Type) : ANF.Program =
    let varGen = VarGen 2000  // Start high to avoid conflicts
    let (exprWithPrint, _) = wrapReturnWithPrint programType varGen mainExpr
    ANF.Program (functions, exprWithPrint)

/// Insert Print into a named entry function
let insertPrintInEntry (entryName: string) (programType: AST.Type) (functions: ANF.Function list) : Result<ANF.Function list, string> =
    let varGen = VarGen 2000  // Start high to avoid conflicts
    let rec update found remaining =
        match remaining with
        | [] ->
            if found then Ok []
            else Error $"Entry function '{entryName}' not found for print insertion"
        | f :: rest ->
            if f.Name = entryName then
                let (bodyWithPrint, _) = wrapReturnWithPrint programType varGen f.Body
                update true rest
                |> Result.map (fun updatedTail -> { f with Body = bodyWithPrint } :: updatedTail)
            else
                update found rest
                |> Result.map (fun updatedTail -> f :: updatedTail)
    update false functions
