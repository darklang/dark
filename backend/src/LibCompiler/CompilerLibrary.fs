// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open CodeGen
open IRPrinter

open System
open System.IO
open System.Diagnostics
open System.Reflection
open Output

/// Timing for a single compiler pass
type PassTiming = {
    Pass: string
    Elapsed: TimeSpan
}

/// Recorder for compiler pass timings
type PassTimingRecorder = PassTiming -> unit

/// Result of execution with timing
type ExecutionOutput = {
    ExitCode: int
    Stdout: string
    Stderr: string
    RuntimeTime: TimeSpan
}

/// Compilation mode for labeling and test behavior
type CompileMode =
    | FullProgram
    | TestExpression

/// Source syntax for parsing Dark programs
type SourceSyntax =
    | CompilerSyntax
    | InterpreterSyntax

/// Shared compiler warning settings.
let defaultWarningSettings : AST.WarningSettings = AST.defaultWarningSettings

/// Result of compilation with timing
type CompileReport = {
    Result: Result<byte array, string>
    CompileTime: TimeSpan
}

/// Compiler options for controlling optimization behavior
type CompilerOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
    /// Disable ANF-level optimizations (constant folding, propagation, etc.)
    DisableANFOpt: bool
    /// Disable ANF constant folding (includes algebraic identities and constant branches)
    DisableANFConstFolding: bool
    /// Disable ANF constant propagation
    DisableANFConstProp: bool
    /// Disable ANF copy propagation
    DisableANFCopyProp: bool
    /// Disable ANF dead code elimination
    DisableANFDCE: bool
    /// Disable ANF strength reduction (pow2 mul/div/mod)
    DisableANFStrengthReduction: bool
    /// Disable ANF function inlining
    DisableInlining: bool
    /// Disable tail call optimization
    DisableTCO: bool
    /// Disable MIR-level optimizations (DCE, copy/constant propagation on SSA)
    DisableMIROpt: bool
    /// Disable MIR constant folding
    DisableMIRConstFolding: bool
    /// Disable MIR common subexpression elimination
    DisableMIRCSE: bool
    /// Disable MIR copy propagation
    DisableMIRCopyProp: bool
    /// Disable MIR dead code elimination
    DisableMIRDCE: bool
    /// Disable MIR CFG simplification
    DisableMIRCFGSimplify: bool
    /// Disable MIR loop-invariant code motion
    DisableMIRLICM: bool
    /// Disable LIR-level optimizations (peephole optimizations)
    DisableLIROpt: bool
    /// Disable LIR peephole optimizations
    DisableLIRPeephole: bool
    /// Disable function tree shaking (pruning unused stdlib/user functions)
    DisableFunctionTreeShaking: bool
    /// Enable runtime expression coverage tracking
    EnableCoverage: bool
    /// Enable leak checking (debug only)
    EnableLeakCheck: bool
    /// Warning compatibility settings passed into type checking
    Warnings: AST.WarningSettings
    /// Dump ANF representations to stdout
    DumpANF: bool
    /// Dump MIR representations to stdout
    DumpMIR: bool
    /// Dump LIR representations to stdout (before and after register allocation)
    DumpLIR: bool
}

/// Default compiler options
let defaultOptions : CompilerOptions = {
    DisableFreeList = false
    DisableANFOpt = false
    DisableANFConstFolding = false
    DisableANFConstProp = false
    DisableANFCopyProp = false
    DisableANFDCE = false
    DisableANFStrengthReduction = false
    DisableInlining = false
    DisableTCO = false
    DisableMIROpt = false
    DisableMIRConstFolding = false
    DisableMIRCSE = false
    DisableMIRCopyProp = false
    DisableMIRDCE = false
    DisableMIRCFGSimplify = false
    DisableMIRLICM = false
    DisableLIROpt = false
    DisableLIRPeephole = false
    DisableFunctionTreeShaking = false
    EnableCoverage = false
    EnableLeakCheck = false
    Warnings = AST.defaultWarningSettings
    DumpANF = false
    DumpMIR = false
    DumpLIR = false
}

let private recordPassTiming
    (recorder: PassTimingRecorder option)
    (pass: string)
    (elapsedMs: float)
    : unit =
    match recorder with
    | None -> ()
    | Some record ->
        record { Pass = pass; Elapsed = TimeSpan.FromMilliseconds(elapsedMs) }

/// Determine whether to dump a specific IR, based on verbosity or explicit option
let private shouldDumpIR (verbosity: int) (enabled: bool) : bool =
    verbosity >= 3 || enabled

let private buildANFOptimizeOptions (options: CompilerOptions) : ANF_Optimize.OptimizeOptions =
    let enabled = not options.DisableANFOpt
    {
        EnableConstFolding = enabled && not options.DisableANFConstFolding
        EnableConstProp = enabled && not options.DisableANFConstProp
        EnableCopyProp = enabled && not options.DisableANFCopyProp
        EnableDCE = enabled && not options.DisableANFDCE
        EnableStrengthReduction = enabled && not options.DisableANFStrengthReduction
    }

let private shouldRunANFOptimize (anfOptions: ANF_Optimize.OptimizeOptions) : bool =
    anfOptions.EnableConstFolding
    || anfOptions.EnableConstProp
    || anfOptions.EnableCopyProp
    || anfOptions.EnableDCE
    || anfOptions.EnableStrengthReduction

let private buildMIROptimizeOptions (options: CompilerOptions) : MIR_Optimize.OptimizeOptions =
    let enabled = not options.DisableMIROpt
    {
        EnableConstFolding = enabled && not options.DisableMIRConstFolding
        EnableCSE = enabled && not options.DisableMIRCSE
        EnableCopyProp = enabled && not options.DisableMIRCopyProp
        EnableDCE = enabled && not options.DisableMIRDCE
        EnableCFGSimplify = enabled && not options.DisableMIRCFGSimplify
        EnableLICM = enabled && not options.DisableMIRLICM
    }

let private shouldRunMIROptimize (mirOptions: MIR_Optimize.OptimizeOptions) : bool =
    mirOptions.EnableConstFolding
    || mirOptions.EnableCSE
    || mirOptions.EnableCopyProp
    || mirOptions.EnableDCE
    || mirOptions.EnableCFGSimplify
    || mirOptions.EnableLICM

let private formatPassGroup (label: string) (passes: (string * bool) list) : string =
    let enabled =
        passes
        |> List.choose (fun (name, isEnabled) -> if isEnabled then Some name else None)
    let enabledNames = String.concat ", " enabled
    match enabled with
    | [] -> $"{label} (disabled)"
    | _ -> $"{label} ({enabledNames})"

/// Print ANF program in a consistent, human-readable format
let private printANFProgram (title: string) (program: ANF.Program) : unit =
    println title
    println (formatANF program)
    println ""

/// Print MIR program (with CFG) in a consistent format
let private printMIRProgram (title: string) (program: MIR.Program) : unit =
    println title
    println (formatMIR program)
    println ""

/// Print symbolic LIR program (with CFG) in a consistent format
let private printLIRProgram (title: string) (program: LIR.Program) : unit =
    println title
    println (formatLIR program)
    println ""

/// Run SSA + MIR/LIR optimizations, returning an optimized LIR program
let private compileMirToLir
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (stageSuffix: string)
    (mirProgram: MIR.Program)
    : Result<LIR.Program, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [3.1/7] SSA Construction{suffix}..."
    let ssaStart = sw.Elapsed.TotalMilliseconds
    let ssaProgram = SSA_Construction.convertToSSA mirProgram
    let ssaElapsed = sw.Elapsed.TotalMilliseconds - ssaStart
    recordPassTiming passTimingRecorder "SSA Construction" ssaElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(ssaElapsed, 1)
        println $"        {t}ms"

    let mirOptions = buildMIROptimizeOptions options
    let mirPassLabel =
        formatPassGroup
            "MIR Optimizations"
            [
                ("const_folding", mirOptions.EnableConstFolding)
                ("cse", mirOptions.EnableCSE)
                ("copy_prop", mirOptions.EnableCopyProp)
                ("dce", mirOptions.EnableDCE)
                ("cfg_simplify", mirOptions.EnableCFGSimplify)
                ("licm", mirOptions.EnableLICM)
            ]
    if verbosity >= 1 then println $"  [3.5/7] {mirPassLabel}{suffix}..."
    let mirOptStart = sw.Elapsed.TotalMilliseconds
    let optimizedProgram =
        if shouldRunMIROptimize mirOptions then
            MIR_Optimize.optimizeProgramWithOptions mirOptions ssaProgram
        else
            ssaProgram
    let mirOptElapsed = sw.Elapsed.TotalMilliseconds - mirOptStart
    recordPassTiming passTimingRecorder "MIR Optimizations" mirOptElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(mirOptElapsed, 1)
        println $"        {t}ms"

    if verbosity >= 1 then println $"  [4/7] MIR → LIR{suffix}..."
    let lirStart = sw.Elapsed.TotalMilliseconds
    let lirResult = MIR_to_LIR.toLIR optimizedProgram
    match lirResult with
    | Error err -> Error $"LIR conversion error: {err}"
    | Ok lirProgram ->
        let lirElapsed = sw.Elapsed.TotalMilliseconds - lirStart
        recordPassTiming passTimingRecorder "MIR -> LIR" lirElapsed
        if shouldDumpIR verbosity options.DumpLIR then
            printLIRProgram "=== LIR (Low-level IR with CFG) ===" lirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(lirElapsed, 1)
            println $"        {t}ms"

        let lirPassLabel =
            formatPassGroup
                "LIR Peephole"
                [("peephole", not options.DisableLIROpt && not options.DisableLIRPeephole)]
        if verbosity >= 1 then println $"  [4.5/7] {lirPassLabel}{suffix}..."
        let lirOptStart = sw.Elapsed.TotalMilliseconds
        let optimizedLir =
            if options.DisableLIROpt || options.DisableLIRPeephole then
                lirProgram
            else
                LIR_Peephole.optimizeProgram lirProgram
        let lirOptElapsed = sw.Elapsed.TotalMilliseconds - lirOptStart
        recordPassTiming passTimingRecorder "LIR Peephole" lirOptElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(lirOptElapsed, 1)
            println $"        {t}ms"
        Ok optimizedLir

/// Allocate registers for a list of symbolic LIR functions
let private allocateRegistersForFunctions
    (functions: LIR.Function list)
    : LIR.Function list =
    let arch = match Platform.detectArch () with Ok a -> a | Error _ -> Platform.ARM64
    functions |> List.map (RegisterAllocation.allocateRegisters arch)

/// Run MIR+LIR passes (including register allocation) from ANF functions
let private lowerToAllocatedLir
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (stageSuffix: string)
    (functions: ANF.Function list)
    (typeMap: ANF.TypeMap)
    (registries: AST_to_ANF.Registries)
    (externalReturnTypes: Map<string, AST.Type>)
    : Result<LIR.Function list, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    let functionOrder = functions |> List.map (fun f -> f.Name)
    let compileFunctions (functionsToCompile: ANF.Function list) : Result<LIR.Function list, string> =
        if List.isEmpty functionsToCompile then
            Ok []
        else
            if verbosity >= 1 then println $"  [3/7] ANF → MIR{suffix}..."
            let mirStart = sw.Elapsed.TotalMilliseconds
            let anfProgram = ANF.Program (functionsToCompile, ANF.Return ANF.UnitLiteral)
            let mirResult =
                ANF_to_MIR.toMIRFunctionsOnly
                    anfProgram
                    typeMap
                    registries.FuncParams
                    registries.VariantLookup
                    registries.TypeReg
                    options.EnableCoverage
                    externalReturnTypes
            match mirResult with
            | Error err -> Error $"MIR conversion error: {err}"
            | Ok (mirFuncs, variantRegistry, mirRecordRegistry) ->
                let mirProgram = MIR.Program (mirFuncs, variantRegistry, mirRecordRegistry)
                let mirElapsed = sw.Elapsed.TotalMilliseconds - mirStart
                recordPassTiming passTimingRecorder "ANF -> MIR" mirElapsed
                if shouldDumpIR verbosity options.DumpMIR then
                    printMIRProgram "=== MIR (Control Flow Graph) ===" mirProgram
                if verbosity >= 2 then
                    let t = System.Math.Round(mirElapsed, 1)
                    println $"        {t}ms"
                compileMirToLir verbosity options sw passTimingRecorder stageSuffix mirProgram
                |> Result.bind (fun lirProgram ->
                    if verbosity >= 1 then println "  [5/7] Register Allocation..."
                    let allocStart = sw.Elapsed.TotalMilliseconds
                    let (LIR.Program lirFuncs) = lirProgram
                    let allocatedFuncs = allocateRegistersForFunctions lirFuncs
                    let allocElapsed = sw.Elapsed.TotalMilliseconds - allocStart
                    recordPassTiming passTimingRecorder "Register Allocation" allocElapsed
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocElapsed, 1)
                        println $"        {t}ms"
                    Ok allocatedFuncs)

    let compileFunctionsWithTiming
        (label: string)
        (functionsToCompile: ANF.Function list)
        : Result<LIR.Function list, string> =
        if List.isEmpty functionsToCompile then
            Ok []
        else
            let startTime = sw.Elapsed.TotalMilliseconds
            compileFunctions functionsToCompile
            |> Result.map (fun compiled ->
                let elapsed = sw.Elapsed.TotalMilliseconds - startTime
                recordPassTiming passTimingRecorder label elapsed
                compiled)

    let (startFunctions, otherFunctions) =
        functions |> List.partition (fun func -> func.Name = "_start")

    let compileResult =
        match passTimingRecorder, startFunctions with
        | Some _, _ :: _ ->
            compileFunctionsWithTiming "Start Function Compilation" startFunctions
            |> Result.bind (fun compiledStart ->
                compileFunctions otherFunctions
                |> Result.map (fun compiledOther -> compiledStart @ compiledOther))
        | _ ->
            compileFunctions functions

    compileResult
    |> Result.map (fun compiledFuncs ->
        // Keep per-name queues so duplicate function names (e.g. lifted __closure_N from
        // different compilation units) preserve distinct bodies in original order.
        let compiledQueues : Map<string, LIR.Function list> =
            List.foldBack
                (fun (func: LIR.Function) (acc: Map<string, LIR.Function list>) ->
                    let existing = Map.tryFind func.Name acc |> Option.defaultValue []
                    Map.add func.Name (func :: existing) acc)
                compiledFuncs
                Map.empty

        let rec rebuildOrder
            (remainingNames: string list)
            (queues: Map<string, LIR.Function list>)
            (acc: LIR.Function list)
            : LIR.Function list =
            match remainingNames with
            | [] ->
                List.rev acc
            | name :: rest ->
                match Map.tryFind name queues with
                | Some (nextFunc :: remainingFuncs) ->
                    let queues' =
                        if List.isEmpty remainingFuncs then
                            Map.remove name queues
                        else
                            Map.add name remainingFuncs queues
                    rebuildOrder rest queues' (nextFunc :: acc)
                | _ ->
                    Crash.crash $"lowerToAllocatedLir: missing compiled function for '{name}'"

        rebuildOrder functionOrder compiledQueues [])

let private buildConversionResult
    (program: ANF.Program)
    (registries: AST_to_ANF.Registries)
    : AST_to_ANF.ConversionResult =
    {
        Program = program
        TypeReg = registries.TypeReg
        VariantLookup = registries.VariantLookup
        FuncReg = registries.FuncReg
        FuncParams = registries.FuncParams
        ModuleRegistry = registries.ModuleRegistry
    }

/// Run ANF optimization + RC insertion, returning a final ANF function list and type map
let private buildAnf
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (registries: AST_to_ANF.Registries)
    (functions: ANF.Function list)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<ANF.Function list * ANF.TypeMap, string> =

    let anfOptions = buildANFOptimizeOptions options
    let anfPassLabel =
        formatPassGroup
            "ANF Optimizations"
            [
                ("const_folding", anfOptions.EnableConstFolding)
                ("const_prop", anfOptions.EnableConstProp)
                ("copy_prop", anfOptions.EnableCopyProp)
                ("dce", anfOptions.EnableDCE)
                ("strength_reduction", anfOptions.EnableStrengthReduction)
            ]
    if verbosity >= 1 then println $"  [2.3/7] {anfPassLabel}..."
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (before optimization) ===" anfProgram
    let anfOptStart = sw.Elapsed.TotalMilliseconds
    let anfOptimized =
        if shouldRunANFOptimize anfOptions then
            ANF_Optimize.optimizeProgramWithOptions anfOptions anfProgram
        else
            anfProgram
    let anfOptElapsed = sw.Elapsed.TotalMilliseconds - anfOptStart
    recordPassTiming passTimingRecorder "ANF Optimizations" anfOptElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(anfOptElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after optimization) ===" anfOptimized

    if verbosity >= 1 then println "  [2.4/7] ANF Inlining..."
    let inlineStart = sw.Elapsed.TotalMilliseconds
    let anfInlined =
        if options.DisableInlining then
            anfOptimized
        else
            ANF_Inlining.inlineProgramDefault anfOptimized
    let inlineElapsed = sw.Elapsed.TotalMilliseconds - inlineStart
    recordPassTiming passTimingRecorder "ANF Inlining" inlineElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(inlineElapsed, 1)
        println $"        {t}ms"

    let convResult = buildConversionResult anfInlined registries

    if verbosity >= 1 then println "  [2.5/7] Reference Count Insertion..."
    let rcStart = sw.Elapsed.TotalMilliseconds
    let rcResult = RefCountInsertion.insertRCInProgram convResult
    match rcResult with
    | Error err -> Error $"Reference count insertion error: {err}"
    | Ok (anfAfterRC, typeMap) ->
        let rcElapsed = sw.Elapsed.TotalMilliseconds - rcStart
        recordPassTiming passTimingRecorder "Reference Count Insertion" rcElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(rcElapsed, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after RC insertion) ===" anfAfterRC

        let (ANF.Program (finalFunctions, _)) = anfAfterRC
        Ok (finalFunctions, typeMap)

/// Run tail call detection on a function list (for post-print insertion TCO)
let private applyTco
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (functions: ANF.Function list)
    (passTimingRecorder: PassTimingRecorder option)
    : ANF.Function list =
    if verbosity >= 1 then println "  [2.7/7] Tail Call Detection..."
    let tcoStart = sw.Elapsed.TotalMilliseconds
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
    let anfAfterTCO =
        if options.DisableTCO then
            anfProgram
        else
            TailCallDetection.detectTailCallsInProgram anfProgram
    let tcoElapsed = sw.Elapsed.TotalMilliseconds - tcoStart
    recordPassTiming passTimingRecorder "Tail Call Detection" tcoElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(tcoElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after Tail Call Detection) ===" anfAfterTCO
    let (ANF.Program (tcoFunctions, _)) = anfAfterTCO
    tcoFunctions

/// Run codegen, encoding, and binary generation
let private generateBinary
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (codegenLabel: string)
    (emitLabel: string)
    (dumpAsm: bool)
    (dumpMachineCode: bool)
    (allocatedProgram: LIR.Program)
    : Result<byte array, string> =

    match Platform.detectArch () with
    | Error err -> Error $"Architecture detection error: {err}"
    | Ok Platform.X86_64 ->
        // x86-64 backend
        if verbosity >= 1 then println codegenLabel
        let codegenStart = sw.Elapsed.TotalMilliseconds
        let codegenResult = CodeGen_X86_64.translateProgram allocatedProgram options.EnableLeakCheck
        match codegenResult with
        | Error err -> Error $"x86-64 code generation error: {err}"
        | Ok x86Instructions ->
            let codegenElapsed = sw.Elapsed.TotalMilliseconds - codegenStart
            recordPassTiming passTimingRecorder "Code Generation" codegenElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(codegenElapsed, 1)
                println $"        {t}ms"

            if dumpAsm && verbosity >= 3 then
                println "=== x86-64 Assembly Instructions ==="
                for (i, instr) in List.indexed x86Instructions do
                    println $"  {i}: {instr}"
                println ""

            if verbosity >= 1 then println (emitLabel.Replace("{format}", "ELF"))
            let emitStart = sw.Elapsed.TotalMilliseconds
            match X86_64_Resolve.resolveAndEncode x86Instructions with
            | Error err -> Error $"x86-64 resolve error: {err}"
            | Ok resolveResult ->
                // Patch data labels (e.g., leak counter) if there are deferred fixups
                let patchedResult =
                    if List.isEmpty resolveResult.DeferredFixups then
                        Ok resolveResult
                    else
                        let elfHeaderSize = 64
                        let programHeaderSize = 56
                        let codeFileOffset = elfHeaderSize + programHeaderSize
                        let codeSize = resolveResult.MachineCode.Length
                        let alignedDataStart = (codeFileOffset + codeSize + 7) &&& (~~~7)
                        // Leak counter is at start of data section (no float/string pools on x86_64)
                        let leakCounterFileOffset = alignedDataStart
                        let dataLabels = Map.ofList [("_leak_count", leakCounterFileOffset)]
                        X86_64_Resolve.patchDataLabels resolveResult dataLabels codeFileOffset
                match patchedResult with
                | Error err -> Error $"x86-64 data label error: {err}"
                | Ok resolveResult ->
                let entryOffset =
                    match Map.tryFind "_start" resolveResult.LabelPositions with
                    | Some offset -> offset
                    | None -> 0
                let binary =
                    Binary_Generation_ELF_X86_64.createExecutableWithPools
                        resolveResult.MachineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool
                        options.EnableLeakCheck entryOffset
                let emitElapsed = sw.Elapsed.TotalMilliseconds - emitStart
                recordPassTiming passTimingRecorder "x86-64 Emit" emitElapsed
                if verbosity >= 2 then
                    let t = System.Math.Round(emitElapsed, 1)
                    println $"        {t}ms"
                Ok binary

    | Ok Platform.ARM64 ->
        // ARM64 backend (original)
        if verbosity >= 1 then println codegenLabel
        let codegenStart = sw.Elapsed.TotalMilliseconds
        let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
        let codegenOptions : CodeGen.CodeGenOptions = {
            DisableFreeList = options.DisableFreeList
            EnableCoverage = options.EnableCoverage
            CoverageExprCount = coverageExprCount
            EnableLeakCheck = options.EnableLeakCheck
        }
        let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
        match codegenResult with
        | Error err -> Error $"Code generation error: {err}"
        | Ok arm64Instructions ->
            let codegenElapsed = sw.Elapsed.TotalMilliseconds - codegenStart
            recordPassTiming passTimingRecorder "Code Generation" codegenElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(codegenElapsed, 1)
                println $"        {t}ms"

            if dumpAsm && verbosity >= 3 then
                println "=== ARM64 Assembly Instructions ==="
                for (i, instr) in List.indexed arm64Instructions do
                    println $"  {i}: {instr}"
                println ""

            match Platform.detectOS () with
            | Error err -> Error $"Platform detection error: {err}"
            | Ok os ->
                let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                if verbosity >= 1 then println (emitLabel.Replace("{format}", formatName))
                let emitStart = sw.Elapsed.TotalMilliseconds
                let emitResult = ARM64_Emit.emitBinary arm64Instructions os options.EnableLeakCheck
                match emitResult with
                | Error err -> Error $"ARM64 emit error: {err}"
                | Ok emit ->
                    let emitElapsed = sw.Elapsed.TotalMilliseconds - emitStart
                    recordPassTiming passTimingRecorder "ARM64 Emit" emitElapsed
                    if verbosity >= 2 then
                        let t = System.Math.Round(emitElapsed, 1)
                        println $"        {t}ms"

                    if dumpMachineCode && verbosity >= 3 then
                        println "=== Machine Code (hex) ==="
                        for i in 0 .. 4 .. (emit.MachineCode.Length - 1) do
                            if i + 3 < emit.MachineCode.Length then
                                let bytes = sprintf "%02x %02x %02x %02x" emit.MachineCode.[i] emit.MachineCode.[i+1] emit.MachineCode.[i+2] emit.MachineCode.[i+3]
                                println $"  {i:X4}: {bytes}"
                        println $"Total: {emit.MachineCode.Length} bytes\n"

                    Ok emit.Binary


let private buildBaseFuncNames
    (registries: AST_to_ANF.Registries)
    : Set<string> =
    registries.FuncParams
    |> Map.fold (fun acc name _ -> Set.add name acc) Set.empty

let private mergeReturnTypes
    (baseReturnTypes: Map<string, AST.Type>)
    (overlayReturnTypes: Map<string, AST.Type>)
    : Map<string, AST.Type> =
    Map.fold (fun acc k v -> Map.add k v acc) baseReturnTypes overlayReturnTypes

/// Shared compilation context used across pipeline steps
type PipelineContext = {
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    SpecRegistry: AST_to_ANF.SpecRegistry
    Registries: AST_to_ANF.Registries
    BaseFuncNames: Set<string>
    ReturnTypes: Map<string, AST.Type>
}

let private buildContext
    (typeCheckEnv: TypeChecking.TypeCheckEnv)
    (genericFuncDefs: AST_to_ANF.GenericFuncDefs)
    (specRegistry: AST_to_ANF.SpecRegistry)
    (registries: AST_to_ANF.Registries)
    (returnTypes: Map<string, AST.Type>)
    : PipelineContext =
    let baseFuncNames = buildBaseFuncNames registries
    {
        TypeCheckEnv = typeCheckEnv
        GenericFuncDefs = genericFuncDefs
        SpecRegistry = specRegistry
        Registries = registries
        BaseFuncNames = baseFuncNames
        ReturnTypes = returnTypes
    }

/// Compiled preamble context - extends stdlib for a test file
/// Preamble functions are compiled ONCE per file, then reused for all tests in that file
type PreambleContext = {
    /// Extended compilation context (stdlib + preamble)
    Context: PipelineContext
    /// Preamble's ANF functions (after mono, inline, lift, ANF, RC, TCO)
    ANFFunctions: ANF.Function list
    /// Type map from RC insertion (merged with stdlib's TypeMap)
    TypeMap: ANF.TypeMap
    /// Preamble's symbolic LIR functions after register allocation
    SymbolicFunctions: LIR.Function list
}

/// Parsed and typechecked preamble analysis for suite-level specialization
type PreambleAnalysis = {
    TypedAST: AST.Program
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
}

/// Result of compiling stdlib - can be reused across compilations
type StdlibResult = {
    /// Parsed stdlib AST (for merging with user AST)
    AST: AST.Program
    /// Type-checked stdlib with inferred types
    TypedAST: AST.Program
    /// Shared compilation context (typecheck env + registries)
    Context: PipelineContext
    /// Pre-allocated stdlib functions (physical registers assigned, ready for merge)
    AllocatedFunctions: LIR.Function list
    /// Call graph for dead code elimination (which stdlib funcs call which other funcs)
    StdlibCallGraph: Map<string, Set<string>>
    /// Stdlib ANF functions indexed by name (for coverage analysis)
    StdlibANFFunctions: Map<string, ANF.Function>
    /// Call graph at ANF level (for coverage analysis reachability)
    StdlibANFCallGraph: Map<string, Set<string>>
    /// TypeMap from RC insertion (needed for getReachableStdlibFunctions)
    StdlibTypeMap: ANF.TypeMap
}

/// Context for compiling user code
type CompileContext =
    | StdlibOnly of StdlibResult
    | StdlibWithPreamble of StdlibResult * PreambleContext

/// Request for compiling source code
type CompileRequest = {
    Context: CompileContext
    Mode: CompileMode
    SourceSyntax: SourceSyntax
    Source: string
    SourceFile: string
    AllowInternal: bool
    Verbosity: int
    Options: CompilerOptions
    PassTimingRecorder: PassTimingRecorder option
}


// Helper functions for exception-to-Result conversion (Darklang compatibility)

/// Extract return types from a FuncReg (FunctionRegistry maps func name -> full type)
/// This is needed because buildReturnTypeReg only includes functions in the current program,
/// but we need return types for all callable functions (including stdlib)
let private extractReturnTypes (funcReg: Map<string, AST.Type>) : Map<string, AST.Type> =
    funcReg
    |> Map.toSeq
    |> Seq.choose (fun (name, typ) ->
        match typ with
        | AST.TFunction (_, retType) -> Some (name, retType)
        | other -> Crash.crash $"extractReturnTypes: Non-function type '{other}' found in FuncReg for '{name}'")
    |> Map.ofSeq

let private emptyRegistries (moduleRegistry: AST.ModuleRegistry) : AST_to_ANF.Registries =
    {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        ModuleRegistry = moduleRegistry
    }

let private liftLambdasWithBase
    (baseRegistries: AST_to_ANF.Registries)
    (baseFuncNames: Set<string>)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let baseFuncReturnTypes = extractReturnTypes baseRegistries.FuncReg
    let baseFuncParamsWithReservedNames =
        baseFuncNames
        |> Set.fold (fun acc name ->
            if Map.containsKey name acc then
                acc
            else
                Map.add name [] acc) baseRegistries.FuncParams
    AST_to_ANF.liftLambdasInProgram
        baseRegistries.TypeReg
        baseRegistries.VariantLookup
        baseFuncParamsWithReservedNames
        baseFuncReturnTypes
        program

let private mergeSpecRegistries
    (baseRegistry: AST_to_ANF.SpecRegistry)
    (overlayRegistry: AST_to_ANF.SpecRegistry)
    : AST_to_ANF.SpecRegistry =
    Map.fold (fun acc key value -> Map.add key value acc) baseRegistry overlayRegistry

let private collectLocalSpecs
    (genericDefs: AST_to_ANF.GenericFuncDefs)
    (program: AST.Program)
    : Set<AST_to_ANF.SpecKey> =
    let (AST.Program topLevels) = program
    let allSpecs =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> AST_to_ANF.collectTypeAppsFromFunc f
            | AST.Expression e -> AST_to_ANF.collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty
    allSpecs
    |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

type private MonomorphizationMode =
    | Monomorphize of AST_to_ANF.GenericFuncDefs option
    | ReplaceTypeApps of AST_to_ANF.SpecRegistry
    | SpecializeLocalAndReplace of AST_to_ANF.SpecRegistry

let private prepareProgramForAnf
    (monomorphization: MonomorphizationMode)
    (baseRegistries: AST_to_ANF.Registries)
    (baseFuncNames: Set<string>)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let monomorphizedResult =
        match monomorphization with
        | Monomorphize None ->
            Ok (AST_to_ANF.monomorphize program)
        | Monomorphize (Some defs) ->
            Ok (AST_to_ANF.monomorphizeWithExternalDefs defs program)
        | ReplaceTypeApps specRegistry ->
            AST_to_ANF.replaceTypeAppsInProgramWithRegistry specRegistry program
        | SpecializeLocalAndReplace specRegistry ->
            let localGenericDefs = AST_to_ANF.extractGenericFuncDefs program
            if Map.isEmpty localGenericDefs then
                AST_to_ANF.replaceTypeAppsInProgramWithRegistry specRegistry program
            else
                let localSpecs = collectLocalSpecs localGenericDefs program
                let specialization = AST_to_ANF.specializeFromSpecs localGenericDefs localSpecs
                let combinedSpecRegistry =
                    mergeSpecRegistries specRegistry specialization.SpecRegistry
                let (AST.Program items) = program
                let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
                let programWithSpecializations = AST.Program (specializedTopLevels @ items)
                AST_to_ANF.replaceTypeAppsInProgramWithRegistry combinedSpecRegistry programWithSpecializations
    match monomorphizedResult with
    | Error err -> Error err
    | Ok monomorphized ->
        let (AST.Program topLevels) = monomorphized
        let localFuncNames =
            topLevels
            |> List.choose (function AST.FunctionDef f -> Some f.Name | _ -> None)
            |> Set.ofList
        let knownFuncNames = Set.union baseFuncNames localFuncNames
        let needsLowering = AST_to_ANF.programNeedsLambdaLowering knownFuncNames monomorphized
        if needsLowering then
            let inlined = AST_to_ANF.inlineLambdasInProgram monomorphized
            liftLambdasWithBase baseRegistries baseFuncNames inlined
        else
            Ok monomorphized

let private buildRegistriesForProgram
    (moduleRegistry: AST.ModuleRegistry)
    (baseRegistries: AST_to_ANF.Registries)
    (typeDefs: AST.TypeDef list)
    (functions: AST.FunctionDef list)
    : AST_to_ANF.Registries * AST_to_ANF.Registries * AST.FunctionDef list =
    let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
    let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
    let localRegistries = AST_to_ANF.buildRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
    let mergedRegistries = AST_to_ANF.mergeRegistries baseRegistries localRegistries
    (mergedRegistries, localRegistries, resolvedFunctions)

let private convertTypedProgramToConversionResult
    (moduleRegistry: AST.ModuleRegistry)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.ConversionResult, string> =
    let baseRegistries = emptyRegistries moduleRegistry
    let baseFuncNames = buildBaseFuncNames baseRegistries
    prepareProgramForAnf (Monomorphize None) baseRegistries baseFuncNames typedProgram
    |> Result.bind (fun liftedProgram ->
        AST_to_ANF.splitTopLevels liftedProgram
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let (registries, _localRegistries, resolvedFunctions) =
                buildRegistriesForProgram moduleRegistry baseRegistries typeDefs functions
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                AST_to_ANF.convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    buildConversionResult (ANF.Program (anfFuncs, anfExpr)) registries))))

let private convertTypedProgramToUserOnlyWithMode
    (baseContext: PipelineContext)
    (monomorphization: MonomorphizationMode)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.UserOnlyResult, string> =
    let baseFuncNames = baseContext.BaseFuncNames
    prepareProgramForAnf monomorphization baseContext.Registries baseFuncNames typedProgram
    |> Result.bind (fun liftedProgram ->
        AST_to_ANF.splitTopLevels liftedProgram
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let (registries, localRegistries, resolvedFunctions) =
                buildRegistriesForProgram baseContext.Registries.ModuleRegistry baseContext.Registries typeDefs functions
            let localReturnTypes = extractReturnTypes localRegistries.FuncReg
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                AST_to_ANF.convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    {
                        UserFunctions = anfFuncs
                        MainExpr = anfExpr
                        TypeReg = registries.TypeReg
                        VariantLookup = registries.VariantLookup
                        FuncReg = registries.FuncReg
                        LocalReturnTypes = localReturnTypes
                        FuncParams = registries.FuncParams
                        ModuleRegistry = registries.ModuleRegistry
                    }))))

let private convertTypedProgramToUserOnly
    (baseContext: PipelineContext)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.UserOnlyResult, string> =
    convertTypedProgramToUserOnlyWithMode
        baseContext
        (Monomorphize (Some baseContext.GenericFuncDefs))
        typedProgram

/// Try to delete a file, ignoring any errors
let private tryDeleteFile (path: string) : unit =
    try File.Delete(path) with _ -> ()

/// Try to start a process, returning Result instead of throwing
let private tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message

let private checkProgramWithBaseEnvForSyntax
    (sourceSyntax: SourceSyntax)
    (warningSettings: AST.WarningSettings)
    (baseEnv: TypeChecking.TypeCheckEnv)
    (program: AST.Program)
    : Result<AST.Type * AST.Program * TypeChecking.TypeCheckEnv, TypeChecking.TypeError> =
    match sourceSyntax with
    | InterpreterSyntax ->
        TypeChecking.checkProgramWithBaseEnvAndSettings baseEnv true warningSettings program
    | CompilerSyntax ->
        TypeChecking.checkProgramWithBaseEnvAndSettings baseEnv false warningSettings program

/// Parse and typecheck a preamble, returning typed AST + preamble typecheck env
let analyzePreamble
    (sourceSyntax: SourceSyntax)
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    : Result<PreambleAnalysis, string> =
    let preambleTerminator =
        match sourceSyntax with
        | CompilerSyntax -> "0"
        | InterpreterSyntax -> "0L"
    let preambleSource = preamble + $"\n{preambleTerminator}"
    (match sourceSyntax with
     | CompilerSyntax -> Parser.parseString allowInternal preambleSource
     | InterpreterSyntax -> InterpreterParser.parseString allowInternal preambleSource)
    |> Result.mapError (fun err -> $"Preamble parse error: {err}")
    |> Result.bind (fun preambleAst ->
        checkProgramWithBaseEnvForSyntax
            sourceSyntax
            defaultWarningSettings
            stdlib.Context.TypeCheckEnv
            preambleAst
        |> Result.mapError (fun typeErr -> $"Preamble type error: {TypeChecking.typeErrorToString typeErr}")
        |> Result.map (fun (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
            let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
            {
                TypedAST = typedPreambleAst
                TypeCheckEnv = preambleTypeCheckEnv
                GenericFuncDefs = preambleGenericDefs
            }))

/// Load a .dark file allowing internal identifiers (for stdlib sources)
let private loadDarkFileAllowInternal (filename: string) : Result<AST.Program, string> =
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    let possiblePaths = [
        Path.Combine(exeDir, filename)
        Path.Combine(exeDir, "..", "..", "..", "..", "src", "DarkCompiler", filename)
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", filename)
    ]
    let filePath = possiblePaths |> List.tryFind File.Exists
    match filePath with
    | None ->
        let pathsStr = String.Join(", ", possiblePaths)
        Error $"Could not find {filename} in any of: {pathsStr}"
    | Some path ->
        let source = File.ReadAllText(path)
        Parser.parseString true source
        |> Result.mapError (fun err -> $"Error parsing {filename}: {err}")

/// Load the stdlib and unicode_data.dark files
/// Returns the merged stdlib AST or an error message
let private loadStdlib () : Result<AST.Program, string> =
    let stdlibFiles = [
        "stdlib/Int8.dark"
        "stdlib/Int16.dark"
        "stdlib/Int32.dark"
        "stdlib/Int64.dark"
        "stdlib/UInt8.dark"
        "stdlib/UInt16.dark"
        "stdlib/UInt32.dark"
        "stdlib/UInt64.dark"
        "stdlib/Bool.dark"
        "stdlib/Builtin.dark"
        "stdlib/Tuple2.dark"
        "stdlib/Tuple3.dark"
        "stdlib/Result.dark"
        "stdlib/Option.dark"
        "stdlib/List.dark"
        "stdlib/Float.dark"
        "stdlib/Path.dark"
        "stdlib/Platform.dark"
        "unicode_data.dark"
        "stdlib/String.dark"
        "stdlib/__Hash.dark"
        "stdlib/Dict.dark"
        "stdlib/__HAMT.dark"
        "stdlib/Uuid.dark"
        "stdlib/Date.dark"
        "stdlib/Bytes.dark"
        "stdlib/Char.dark"
        "stdlib/AWS.dark"
        "stdlib/Base64.dark"
        "stdlib/Crypto.dark"
        "stdlib/Math.dark"
        "stdlib/__FingerTree.dark"
        "stdlib/Rpc.dark"
    ]
    let mergeFile (acc: AST.TopLevel list) (filename: string) : Result<AST.TopLevel list, string> =
        match loadDarkFileAllowInternal filename with
        | Error err -> Error err
        | Ok (AST.Program items) ->
            Ok (acc @ items)
    stdlibFiles
    |> List.fold (fun acc filename -> Result.bind (fun items -> mergeFile items filename) acc) (Ok [])
    |> Result.bind (fun items -> Ok (AST.Program items))


/// Build stdlib in isolation, returning reusable result
/// This can be called once and the result reused for multiple user program compilations
let buildStdlibWithTrace
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    match loadStdlib() with
    | Error e ->
        Error e
    | Ok stdlibAst ->
        // Add dummy main expression for type checking (stdlib has no main)
        let (AST.Program items) = stdlibAst
        let withMain = AST.Program (items @ [AST.Expression AST.UnitLiteral])

        match TypeChecking.checkProgramWithEnv withMain with
        | Error e ->
            let msg = TypeChecking.typeErrorToString e
            Error msg
        | Ok (_, typedStdlib, typeCheckEnv) ->
            // Extract generic function definitions for on-demand monomorphization
            let genericFuncDefs = AST_to_ANF.extractGenericFuncDefs typedStdlib
            // Build module registry once (reused across all compilations)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match convertTypedProgramToConversionResult moduleRegistry typedStdlib with
            | Error e ->
                Error e
            | Ok anfResult ->
                let sw = Stopwatch.StartNew()
                let registries : AST_to_ANF.Registries = {
                    TypeReg = anfResult.TypeReg
                    VariantLookup = anfResult.VariantLookup
                    FuncReg = anfResult.FuncReg
                    FuncParams = anfResult.FuncParams
                    ModuleRegistry = anfResult.ModuleRegistry
                }
                let returnTypes = extractReturnTypes registries.FuncReg
                let context = buildContext typeCheckEnv genericFuncDefs Map.empty registries returnTypes
                let (ANF.Program (stdlibFunctions, _)) = anfResult.Program
                let stdlibOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                match buildAnf 0 stdlibOptions sw registries stdlibFunctions passTimingRecorder with
                | Error e ->
                    Error e
                | Ok (anfFunctions, typeMap) ->
                    let tcoFunctions = applyTco 0 stdlibOptions sw anfFunctions passTimingRecorder
                    let stdlibFuncMap =
                        tcoFunctions
                        |> List.map (fun f -> f.Name, f)
                        |> Map.ofList
                    let stdlibLiftedFuncNames =
                        tcoFunctions
                        |> List.map (fun f -> f.Name)
                        |> Set.ofList
                    let contextWithLiftedNames = {
                        context with
                            BaseFuncNames = Set.union context.BaseFuncNames stdlibLiftedFuncNames
                    }
                    let stdlibANFCallGraph = ANFDeadCodeElimination.buildCallGraph tcoFunctions

                    let externalReturnTypes = returnTypes
                    match lowerToAllocatedLir
                        0
                        stdlibOptions
                        sw
                        passTimingRecorder
                        "stdlib"
                        tcoFunctions
                        typeMap
                        registries
                        externalReturnTypes with
                    | Error e ->
                        Error e
                    | Ok allocatedFuncs ->
                        let stdlibCallGraph = DeadCodeElimination.buildCallGraph allocatedFuncs
                        Ok {
                            AST = stdlibAst
                            TypedAST = typedStdlib
                            Context = contextWithLiftedNames
                            AllocatedFunctions = allocatedFuncs
                            StdlibCallGraph = stdlibCallGraph
                            StdlibANFFunctions = stdlibFuncMap
                            StdlibANFCallGraph = stdlibANFCallGraph
                            StdlibTypeMap = typeMap
                        }

/// Build stdlib in isolation with default settings
let buildStdlib () : Result<StdlibResult, string> =
    buildStdlibWithTrace None

/// Build stdlib specializations for a spec set and merge them into the stdlib result
let buildStdlibSpecializations
    (stdlib: StdlibResult)
    (specs: Set<AST_to_ANF.SpecKey>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    if Set.isEmpty specs then
        Ok stdlib
    else
        let specialization = AST_to_ANF.specializeFromSpecs stdlib.Context.GenericFuncDefs specs
        let combinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry
        let existingNames =
            stdlib.StdlibANFFunctions
            |> Map.keys
            |> Set.ofSeq
        let newSpecializedFuncs =
            specialization.SpecializedFuncs
            |> List.filter (fun f -> not (Set.contains f.Name existingNames))

        if List.isEmpty newSpecializedFuncs then
            let updatedContext = { stdlib.Context with SpecRegistry = combinedSpecRegistry }
            Ok {
                stdlib with
                    Context = updatedContext
            }
        else
            let rec mapResult (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
                match items with
                | [] -> Ok []
                | x :: xs ->
                    f x
                    |> Result.bind (fun x' ->
                        mapResult f xs
                        |> Result.map (fun xs' -> x' :: xs'))

            AST_to_ANF.splitTopLevels stdlib.TypedAST
            |> Result.bind (fun (typeDefs, _functions, _expr) ->
                let (registries, localRegistries, resolvedFunctions) =
                    buildRegistriesForProgram
                        stdlib.Context.Registries.ModuleRegistry
                        stdlib.Context.Registries
                        typeDefs
                        newSpecializedFuncs
                let localReturnTypes = extractReturnTypes localRegistries.FuncReg

                let replacedFunctionsResult =
                    resolvedFunctions
                    |> mapResult (AST_to_ANF.replaceTypeAppsInFuncWithRegistry combinedSpecRegistry)

                replacedFunctionsResult
                |> Result.bind (fun replacedFunctions ->
                    let varGen = ANF.VarGen 0
                    AST_to_ANF.convertFunctions registries varGen replacedFunctions
                    |> Result.bind (fun (anfFuncs, _varGen1) ->
                        let stdlibOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                        let sw = Stopwatch.StartNew()
                        buildAnf 0 stdlibOptions sw registries anfFuncs passTimingRecorder
                        |> Result.bind (fun (anfFunctions, typeMap) ->
                            let tcoFunctions = applyTco 0 stdlibOptions sw anfFunctions passTimingRecorder
                            let newAnfFuncMap =
                                tcoFunctions
                                |> List.map (fun f -> f.Name, f)
                                |> Map.ofList
                            let externalReturnTypes =
                                mergeReturnTypes stdlib.Context.ReturnTypes localReturnTypes
                            lowerToAllocatedLir
                                0
                                stdlibOptions
                                sw
                                passTimingRecorder
                                "stdlib_specializations"
                                tcoFunctions
                                typeMap
                                registries
                                externalReturnTypes
                            |> Result.bind (fun allocatedFuncs ->
                                let allLirFuncs = stdlib.AllocatedFunctions @ allocatedFuncs
                                let mergedStdlibTypeMap =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap
                                let mergedStdlibAnfFunctions =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibANFFunctions newAnfFuncMap
                                let allAnfFunctions =
                                    mergedStdlibAnfFunctions
                                    |> Map.toList
                                    |> List.map snd
                                let stdlibCallGraph = DeadCodeElimination.buildCallGraph allLirFuncs
                                let stdlibAnfCallGraph = ANFDeadCodeElimination.buildCallGraph allAnfFunctions
                                let specializedFuncNames =
                                    mergedStdlibAnfFunctions
                                    |> Map.keys
                                    |> Set.ofSeq
                                let baseFuncNames =
                                    Set.union
                                        stdlib.Context.BaseFuncNames
                                        (Set.union (buildBaseFuncNames registries) specializedFuncNames)
                                let updatedContext = {
                                    stdlib.Context with
                                        Registries = registries
                                        SpecRegistry = combinedSpecRegistry
                                        BaseFuncNames = baseFuncNames
                                        ReturnTypes = externalReturnTypes
                                }
                                Ok {
                                    stdlib with
                                        Context = updatedContext
                                        AllocatedFunctions = allLirFuncs
                                        StdlibCallGraph = stdlibCallGraph
                                        StdlibANFFunctions = mergedStdlibAnfFunctions
                                        StdlibANFCallGraph = stdlibAnfCallGraph
                                        StdlibTypeMap = mergedStdlibTypeMap
                                }
                            )
                        )
                    )
                )
            )

type private UserCompileLabels = {
    Parse: string
    TypeCheck: string
    Anf: string
    StageSuffix: string
}

type private UserCompilePlan = {
    AllowInternal: bool
    SourceSyntax: SourceSyntax
    Verbosity: int
    Options: CompilerOptions
    PassTimingRecorder: PassTimingRecorder option
    Stdlib: StdlibResult
    BaseContext: PipelineContext
    Monomorphization: MonomorphizationMode
    PrebuiltSymbolicFunctions: LIR.Function list
    SkipFunctionNames: Set<string>
    EmitFunctionEvents: bool
    TreeShakeUserFunctions: bool
    Labels: UserCompileLabels
    SourceFile: string
    Source: string
    /// When Some, the pipeline skips the text parser and uses this AST directly.
    /// This is the seam the PT->AST bridge feeds (compiler-merge airlift, plan §6).
    PrebuiltAst: AST.Program option
}

/// Parse source text into AST using a selected Darklang syntax
let parseProgram
    (sourceSyntax: SourceSyntax)
    (allowInternal: bool)
    (source: string)
    : Result<AST.Program, string> =
    match sourceSyntax with
    | CompilerSyntax -> Parser.parseString allowInternal source
    | InterpreterSyntax -> InterpreterParser.parseString allowInternal source

/// Compile a user/test program against a prebuilt stdlib/preamble context
let private compileUserWithPlan (plan: UserCompilePlan) : CompileReport =
    let sw = Stopwatch.StartNew()
    let result =
        try
            // Pass 1: Parse user code only (or use a pre-built AST from the bridge)
            if plan.Verbosity >= 1 then println plan.Labels.Parse
            let parseResult =
                match plan.PrebuiltAst with
                | Some ast -> Ok ast
                | None -> parseProgram plan.SourceSyntax plan.AllowInternal plan.Source
            let parseTime = sw.Elapsed.TotalMilliseconds
            recordPassTiming plan.PassTimingRecorder "Parse" parseTime
            if plan.Verbosity >= 2 then
                let t = System.Math.Round(parseTime, 1)
                println $"        {t}ms"

            match parseResult with
            | Error err -> Error $"Parse error: {err}"
            | Ok userAst ->
                // Pass 1.5: Type Checking (user code with base TypeCheckEnv)
                if plan.Verbosity >= 1 then println plan.Labels.TypeCheck
                let typeCheckResult =
                    checkProgramWithBaseEnvForSyntax
                        plan.SourceSyntax
                        plan.Options.Warnings
                        plan.BaseContext.TypeCheckEnv
                        userAst
                let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
                recordPassTiming plan.PassTimingRecorder "Type Checking" typeCheckTime
                if plan.Verbosity >= 2 then
                    let t = System.Math.Round(typeCheckTime, 1)
                    println $"        {t}ms"

                match typeCheckResult with
                | Error typeErr -> Error (TypeChecking.typeErrorToString typeErr)
                | Ok (programType, typedUserAst, _userEnv) ->
                    if plan.Verbosity >= 3 then
                        println $"Program type: {TypeChecking.typeToString programType}"
                        println ""

                    // Pass 2: AST → ANF (user only)
                    if plan.Verbosity >= 1 then println plan.Labels.Anf
                    let userOnlyResult =
                        convertTypedProgramToUserOnlyWithMode
                            plan.BaseContext
                            plan.Monomorphization
                            typedUserAst
                    let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                    recordPassTiming plan.PassTimingRecorder "AST -> ANF" anfTime
                    if plan.Verbosity >= 2 then
                        let t = System.Math.Round(anfTime, 1)
                        println $"        {t}ms"

                    match userOnlyResult with
                    | Error err -> Error $"ANF conversion error: {err}"
                    | Ok userOnly ->
                        let functionsToCompile =
                            userOnly.UserFunctions
                            |> List.filter (fun f -> not (Set.contains f.Name plan.SkipFunctionNames))

                        if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                            println $"  [COMPILE] {functionsToCompile.Length} user functions compiled fresh"
                            for f in functionsToCompile do
                                println $"    - {f.Name}"

                        let entryFunction =
                            AST_to_ANF.synthesizeEntryFunction "_start" programType userOnly.MainExpr
                        let userRegistries : AST_to_ANF.Registries = {
                            TypeReg = userOnly.TypeReg
                            VariantLookup = userOnly.VariantLookup
                            FuncReg = userOnly.FuncReg
                            FuncParams = userOnly.FuncParams
                            ModuleRegistry = userOnly.ModuleRegistry
                        }
                        let anfResult =
                            buildAnf
                                plan.Verbosity
                                plan.Options
                                sw
                                userRegistries
                                (entryFunction :: functionsToCompile)
                                plan.PassTimingRecorder
                        match anfResult with
                        | Error err -> Error err
                        | Ok (anfFunctions, typeMap) ->
                            if plan.Verbosity >= 1 then println "  [2.6/7] Print Insertion..."
                            let printStart = sw.Elapsed.TotalMilliseconds
                            match PrintInsertion.insertPrintInEntry "_start" programType anfFunctions with
                            | Error err -> Error $"Print insertion error: {err}"
                            | Ok printedFunctions ->
                                let printElapsed = sw.Elapsed.TotalMilliseconds - printStart
                                recordPassTiming plan.PassTimingRecorder "Print Insertion" printElapsed
                                if plan.Verbosity >= 2 then
                                    let t = System.Math.Round(printElapsed, 1)
                                    println $"        {t}ms"
                                if shouldDumpIR plan.Verbosity plan.Options.DumpANF then
                                    let printProgram = ANF.Program (printedFunctions, ANF.Return ANF.UnitLiteral)
                                    printANFProgram "=== ANF (after Print insertion) ===" printProgram

                                let tcoFunctions = applyTco plan.Verbosity plan.Options sw printedFunctions plan.PassTimingRecorder
                                let externalReturnTypes =
                                    mergeReturnTypes plan.BaseContext.ReturnTypes userOnly.LocalReturnTypes
                                let userLirResult =
                                    lowerToAllocatedLir
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        plan.PassTimingRecorder
                                        plan.Labels.StageSuffix
                                        tcoFunctions
                                        typeMap
                                        userRegistries
                                        externalReturnTypes
                                match userLirResult with
                                | Error err -> Error err
                                | Ok allocatedUserFuncs ->
                                    let allSymbolicUserFuncs = plan.PrebuiltSymbolicFunctions @ allocatedUserFuncs
                                    let finalUserFuncs =
                                        if plan.TreeShakeUserFunctions then
                                            if plan.Verbosity >= 1 then println "  [5.5/7] Function Tree Shaking..."
                                            let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                            let shakenUserFuncs =
                                                if plan.Options.DisableFunctionTreeShaking then
                                                    allSymbolicUserFuncs
                                                else
                                                    FunctionTreeShaking.filterUserFunctions (Some "_start") allSymbolicUserFuncs
                                            let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                            recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                            shakenUserFuncs
                                        else
                                            allSymbolicUserFuncs

                                    if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                                        println $"  [COMBINED] fresh: {allocatedUserFuncs.Length}, total: {allSymbolicUserFuncs.Length}"
                                        for f in allSymbolicUserFuncs do
                                            println $"    - {f.Name}"
                                        println $"  [TreeShaking] user funcs: {finalUserFuncs.Length}"

                                    // Filter stdlib functions to only include reachable ones (dead code elimination)
                                    let reachableStdlib =
                                        if plan.Options.DisableFunctionTreeShaking then plan.Stdlib.AllocatedFunctions
                                        else
                                            let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                            FunctionTreeShaking.filterStdlibFunctions
                                                plan.Stdlib.StdlibCallGraph
                                                finalUserFuncs
                                                plan.Stdlib.AllocatedFunctions
                                            |> fun shakenStdlib ->
                                                let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                                recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                                shakenStdlib

                                    // Combine reachable stdlib functions with user functions
                                    let allFuncs = reachableStdlib @ finalUserFuncs
                                    let allocatedProgram = LIR.Program allFuncs
                                    if shouldDumpIR plan.Verbosity plan.Options.DumpLIR then
                                        printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                                    let binaryResult =
                                        generateBinary
                                            plan.Verbosity
                                            plan.Options
                                            sw
                                            plan.PassTimingRecorder
                                            "  [6/7] Code Generation..."
                                            "  [7/7] ARM64 Emit ({format})..."
                                            false
                                            false
                                            allocatedProgram
                                    match binaryResult with
                                    | Error err -> Error err
                                    | Ok binary ->
                                        Ok binary
        with
        | ex ->
            Error $"Compilation failed: {ex.Message}"
    sw.Stop()
    match result with
    | Ok _ when plan.Verbosity >= 1 ->
        println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"
    | _ -> ()
    { Result = result; CompileTime = sw.Elapsed }

/// Build preamble with stdlib as base, returning extended context for test compilation
/// Preamble functions go through the full pipeline (parse → typecheck → mono → inline → lift → ANF → RC → TCO)
/// The result is built once per file and reused for all tests in that file
let buildPreambleContext
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    (sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult * PreambleContext, string> =
    // Handle empty preamble - return a context that just wraps stdlib
    if String.IsNullOrWhiteSpace(preamble) then
        let emptyContext = {
            Context = stdlib.Context
            ANFFunctions = []
            TypeMap = stdlib.StdlibTypeMap
            SymbolicFunctions = []
        }
        Ok (stdlib, emptyContext)
    else
        // Parse preamble with dummy expression (parser requires a main expression)
        let preambleSource = preamble + "\n0"
        match Parser.parseString allowInternal preambleSource with
        | Error err ->
            let msg = $"Preamble parse error: {err}"
            Error msg
        | Ok preambleAst ->
            // Type-check preamble with stdlib context
            match TypeChecking.checkProgramWithBaseEnv stdlib.Context.TypeCheckEnv preambleAst with
            | Error typeErr ->
                let msg = $"Preamble type error: {TypeChecking.typeErrorToString typeErr}"
                Error msg
            | Ok (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
                // Extract generic function definitions from preamble
                let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
                // Merge stdlib generics with preamble generics
                let mergedGenericDefs = Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs preambleGenericDefs

                // Convert preamble to ANF (mono → inline → lift → ANF)
                match convertTypedProgramToUserOnly stdlib.Context typedPreambleAst with
                | Error err ->
                    let msg = $"Preamble ANF conversion error: {err}"
                    Error msg
                | Ok preambleUserOnly ->
                    let preambleRegistries : AST_to_ANF.Registries = {
                        TypeReg = preambleUserOnly.TypeReg
                        VariantLookup = preambleUserOnly.VariantLookup
                        FuncReg = preambleUserOnly.FuncReg
                        FuncParams = preambleUserOnly.FuncParams
                        ModuleRegistry = preambleUserOnly.ModuleRegistry
                    }
                    let preambleOptions = defaultOptions
                    let sw = Stopwatch.StartNew()
                    let preambleReturnTypes =
                        mergeReturnTypes stdlib.Context.ReturnTypes preambleUserOnly.LocalReturnTypes
                    let pipelineContext =
                        buildContext preambleTypeCheckEnv mergedGenericDefs Map.empty preambleRegistries preambleReturnTypes
                    match buildAnf 0 preambleOptions sw preambleRegistries preambleUserOnly.UserFunctions passTimingRecorder with
                    | Error err ->
                        let rcPrefix = "Reference count insertion error: "
                        let msg =
                            if err.StartsWith(rcPrefix) then
                                let suffix = err.Substring(rcPrefix.Length)
                                $"Preamble RC insertion error: {suffix}"
                            else
                                $"Preamble {err}"
                        Error msg
                    | Ok (preambleFunctions, typeMap) ->
                        let tcoFunctions = applyTco 0 preambleOptions sw preambleFunctions passTimingRecorder
                        let preambleExternalReturnTypes = preambleReturnTypes
                        match lowerToAllocatedLir
                            0
                            preambleOptions
                            sw
                            passTimingRecorder
                            "preamble"
                            tcoFunctions
                            typeMap
                            preambleRegistries
                            preambleExternalReturnTypes with
                        | Error err ->
                            let msg = $"Preamble {err}"
                            Error msg
                        | Ok allocatedFuncs ->
                            let stdlibFuncNames =
                                stdlib.AllocatedFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let isStdlibFunction (name: string) : bool =
                                Set.contains name stdlibFuncNames
                            let preambleOnlyFuncs =
                                allocatedFuncs
                                |> List.filter (fun func -> not (isStdlibFunction func.Name))
                            let preambleSymbolicFuncs = preambleOnlyFuncs
                            let preambleLiftedFuncNames =
                                tcoFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let pipelineContextWithLiftedNames = {
                                pipelineContext with
                                    BaseFuncNames =
                                        Set.union pipelineContext.BaseFuncNames preambleLiftedFuncNames
                            }

                            // Merge TypeMaps (stdlib + preamble)
                            let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                            let context = {
                                Context = pipelineContextWithLiftedNames
                                ANFFunctions = tcoFunctions
                                TypeMap = mergedTypeMap
                                SymbolicFunctions = preambleSymbolicFuncs
                            }
                            Ok (stdlib, context)

/// Build preamble context from a typed preamble analysis and precomputed specializations
let buildPreambleContextFromAnalysis
    (stdlib: StdlibResult)
    (analysis: PreambleAnalysis)
    (specialization: AST_to_ANF.SpecializationResult)
    (sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult * PreambleContext, string> =
    let combinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry

    let mergedGenericDefs =
        Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs analysis.GenericFuncDefs

    let (AST.Program items) = analysis.TypedAST
    let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
    let programWithSpecializations = AST.Program (specializedTopLevels @ items)

    convertTypedProgramToUserOnlyWithMode
        stdlib.Context
        (ReplaceTypeApps combinedSpecRegistry)
        programWithSpecializations
    |> Result.bind (fun preambleUserOnly ->
        let preambleRegistries : AST_to_ANF.Registries = {
            TypeReg = preambleUserOnly.TypeReg
            VariantLookup = preambleUserOnly.VariantLookup
            FuncReg = preambleUserOnly.FuncReg
            FuncParams = preambleUserOnly.FuncParams
            ModuleRegistry = preambleUserOnly.ModuleRegistry
        }
        let preambleOptions = defaultOptions
        let sw = Stopwatch.StartNew()
        let preambleReturnTypes =
            mergeReturnTypes stdlib.Context.ReturnTypes preambleUserOnly.LocalReturnTypes
        let pipelineContext =
            buildContext analysis.TypeCheckEnv mergedGenericDefs combinedSpecRegistry preambleRegistries preambleReturnTypes
        match buildAnf 0 preambleOptions sw preambleRegistries preambleUserOnly.UserFunctions passTimingRecorder with
        | Error err ->
            let rcPrefix = "Reference count insertion error: "
            let msg =
                if err.StartsWith(rcPrefix) then
                    let suffix = err.Substring(rcPrefix.Length)
                    $"Preamble RC insertion error: {suffix}"
                else
                    $"Preamble {err}"
            Error msg
        | Ok (preambleFunctions, typeMap) ->
            let tcoFunctions = applyTco 0 preambleOptions sw preambleFunctions passTimingRecorder
            let preambleExternalReturnTypes = preambleReturnTypes
            match lowerToAllocatedLir
                0
                preambleOptions
                sw
                passTimingRecorder
                "preamble"
                tcoFunctions
                typeMap
                preambleRegistries
                preambleExternalReturnTypes with
            | Error err ->
                let msg = $"Preamble {err}"
                Error msg
            | Ok allocatedFuncs ->
                let stdlibFuncNames =
                    stdlib.AllocatedFunctions
                    |> List.map (fun func -> func.Name)
                    |> Set.ofList
                let isStdlibFunction (name: string) : bool =
                    Set.contains name stdlibFuncNames
                let preambleOnlyFuncs =
                    allocatedFuncs
                    |> List.filter (fun func -> not (isStdlibFunction func.Name))
                let preambleSymbolicFuncs = preambleOnlyFuncs

                let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                Ok (stdlib, {
                    Context = pipelineContext
                    ANFFunctions = tcoFunctions
                    TypeMap = mergedTypeMap
                    SymbolicFunctions = preambleSymbolicFuncs
                }))

let private labelsForMode (mode: CompileMode) : UserCompileLabels =
    match mode with
    | FullProgram ->
        {
            Parse = "  [1/7] Parse..."
            TypeCheck = "  [1.5/7] Type Checking (with stdlib env)..."
            Anf = "  [2/7] AST → ANF (user only)..."
            StageSuffix = "user only"
        }
    | TestExpression ->
        {
            Parse = "  [1/7] Parse (test expr only)..."
            TypeCheck = "  [1.5/7] Type Checking (with preamble env)..."
            Anf = "  [2/7] AST → ANF (test expr only)..."
            StageSuffix = ""
        }

let private buildCompilePlan (request: CompileRequest) : UserCompilePlan =
    let (stdlib, baseContext, prebuiltSymbolic, skipNames) =
        match request.Context with
        | StdlibOnly stdlib ->
            stdlib, stdlib.Context, [], Set.empty
        | StdlibWithPreamble (stdlib, preambleCtx) ->
            let preambleFuncs = preambleCtx.SymbolicFunctions
            let preambleFuncNameSet =
                preambleFuncs |> List.map (fun f -> f.Name) |> Set.ofList
            stdlib, preambleCtx.Context, preambleFuncs, preambleFuncNameSet

    let emitFunctionEvents, treeShakeUserFunctions =
        match request.Mode with
        | FullProgram -> false, false
        | TestExpression -> true, true

    let monomorphization =
        match request.Mode with
        | FullProgram -> Monomorphize (Some baseContext.GenericFuncDefs)
        | TestExpression -> SpecializeLocalAndReplace baseContext.SpecRegistry

    {
        AllowInternal = request.AllowInternal
        SourceSyntax = request.SourceSyntax
        Verbosity = request.Verbosity
        Options = request.Options
        PassTimingRecorder = request.PassTimingRecorder
        Stdlib = stdlib
        BaseContext = baseContext
        Monomorphization = monomorphization
        PrebuiltSymbolicFunctions = prebuiltSymbolic
        SkipFunctionNames = skipNames
        EmitFunctionEvents = emitFunctionEvents
        TreeShakeUserFunctions = treeShakeUserFunctions
        Labels = labelsForMode request.Mode
        SourceFile = request.SourceFile
        Source = request.Source
        PrebuiltAst = None
    }

/// Compile source code to binary (in-memory, no file I/O)
let compile (request: CompileRequest) : CompileReport =
    let plan = buildCompilePlan request
    compileUserWithPlan plan

/// Compile a pre-built AST.Program (bridged from ProgramTypes), skipping the
/// text parser entirely. The request supplies context/mode/options; its Source
/// field is ignored. This is the entry the compiler-merge PT->AST bridge uses
/// (plan §6) so we never round-trip through the compiler's diverged parser.
let compileAstProgram (request: CompileRequest) (ast: AST.Program) : CompileReport =
    let plan = { buildCompilePlan request with PrebuiltAst = Some ast }
    compileUserWithPlan plan

/// Execute compiled binary and capture output
/// Execute a compiled binary. timeoutMs > 0 kills the process (exit -999,
/// stderr "TIMEOUT") if it doesn't finish in time — essential for sweeping fns
/// that may hang (e.g. the x64 multi-element list runtime bug). <= 0 = no timeout.
let execute (verbosity: int) (timeoutMs: int) (binary: byte array) : ExecutionOutput =
    let sw = Stopwatch.StartNew()
    let finish (exitCode: int) (stdout: string) (stderr: string) : ExecutionOutput =
        sw.Stop()
        { ExitCode = exitCode
          Stdout = stdout
          Stderr = stderr
          RuntimeTime = sw.Elapsed }

    if verbosity >= 1 then println ""
    if verbosity >= 1 then println "  Execution:"

    // Write binary to temp file
    if verbosity >= 1 then println "    • Writing binary to temp file..."
    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    // Write and flush to disk to minimize (but not eliminate) "Text file busy" race
    do
        use stream = new IO.FileStream(tempPath, IO.FileMode.Create, IO.FileAccess.Write, IO.FileShare.None)
        stream.Write(binary, 0, binary.Length)
        stream.Flush(true)  // Flush both stream and OS buffers to disk

    let writeTime = sw.Elapsed.TotalMilliseconds
    if verbosity >= 2 then println $"      {System.Math.Round(writeTime, 1)}ms"

    let result =
        try
            // Make executable using Unix file mode
            if verbosity >= 1 then println "    • Setting executable permissions..."
            let permissions = File.GetUnixFileMode(tempPath)
            File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)
            let chmodTime = sw.Elapsed.TotalMilliseconds - writeTime
            if verbosity >= 2 then println $"      {System.Math.Round(chmodTime, 1)}ms"

            // Code sign with adhoc signature (required for macOS only)
            let codesignResult =
                match Platform.detectOS () with
                | Error err ->
                    // Platform detection failed
                    Some $"Platform detection failed: {err}"
                | Ok os ->
                    if Platform.requiresCodeSigning os then
                        if verbosity >= 1 then println "    • Code signing (adhoc)..."
                        let codesignStart = sw.Elapsed.TotalMilliseconds
                        let codesignInfo = ProcessStartInfo("codesign")
                        codesignInfo.Arguments <- $"-s - \"{tempPath}\""
                        codesignInfo.UseShellExecute <- false
                        codesignInfo.RedirectStandardOutput <- true
                        codesignInfo.RedirectStandardError <- true
                        let codesignProc = Process.Start(codesignInfo)
                        codesignProc.WaitForExit()

                        if codesignProc.ExitCode <> 0 then
                            let stderr = codesignProc.StandardError.ReadToEnd()
                            Some $"Code signing failed: {stderr}"
                        else
                            let codesignTime = sw.Elapsed.TotalMilliseconds - codesignStart
                            if verbosity >= 2 then println $"      {System.Math.Round(codesignTime, 1)}ms"
                            None
                    else
                        if verbosity >= 1 then println "    • Code signing skipped (not required on Linux)"
                        None

            match codesignResult with
            | Some errorMsg ->
                // Code signing or platform detection failed - return error
                finish -1 "" errorMsg
            | None ->
                // Execute (with retry for "Text file busy" race condition)
                // Even with flush, kernel may not have fully synced file/permissions in fast test runs
                if verbosity >= 1 then println "    • Running binary..."
                let execStart = sw.Elapsed.TotalMilliseconds
                let execInfo = ProcessStartInfo(tempPath)
                execInfo.RedirectStandardOutput <- true
                execInfo.RedirectStandardError <- true
                execInfo.UseShellExecute <- false

                // Retry up to 3 times with small delay if we get "Text file busy"
                let rec startWithRetry attempts =
                    match tryStartProcess execInfo with
                    | Ok proc -> Ok proc
                    | Error msg when msg.Contains("Text file busy") && attempts > 0 ->
                        Threading.Thread.Sleep(10)  // Wait 10ms before retry
                        startWithRetry (attempts - 1)
                    | Error msg -> Error msg

                match startWithRetry 3 with
                | Error msg ->
                    finish -1 "" $"Failed to start process: {msg}"
                | Ok execProc ->
                    use proc = execProc
                    // Start async reads immediately to avoid blocking
                    let stdoutTask = proc.StandardOutput.ReadToEndAsync()
                    let stderrTask = proc.StandardError.ReadToEndAsync()

                    // Wait for process to complete (with optional timeout)
                    let exited =
                        if timeoutMs > 0 then proc.WaitForExit(timeoutMs)
                        else proc.WaitForExit(); true

                    if not exited then
                        // Hung: kill it and report a distinct timeout result.
                        (try proc.Kill(true) with _ -> ())
                        finish -999 "" "TIMEOUT"
                    else
                        // Now wait for output to be fully read
                        let stdout = stdoutTask.Result
                        let stderr = stderrTask.Result

                        let execTime = sw.Elapsed.TotalMilliseconds - execStart
                        if verbosity >= 2 then println $"      {System.Math.Round(execTime, 1)}ms"

                        if verbosity >= 1 then
                            println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                        finish proc.ExitCode stdout stderr
        finally
            // Cleanup - ignore deletion errors
            tryDeleteFile tempPath
    result

/// Get all stdlib function names from the prebuilt stdlib
let getAllStdlibFunctionNamesFromStdlib (stdlib: StdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq

/// Get the set of stdlib function names reachable from user code (using prebuilt stdlib)
/// Used for coverage analysis without re-compiling stdlib
let getReachableStdlibFunctionsFromStdlib (stdlib: StdlibResult) (source: string) : Result<Set<string>, string> =
    // Parse user code
    match Parser.parseString false source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        // Type check with stdlib environment
        match TypeChecking.checkProgramWithBaseEnv stdlib.Context.TypeCheckEnv userAst with
        | Error typeErr -> Error (TypeChecking.typeErrorToString typeErr)
        | Ok (programType, typedUserAst, _) ->
            // Convert to ANF
            match convertTypedProgramToUserOnly stdlib.Context typedUserAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                let coverageOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                let sw = Stopwatch.StartNew()
                let entryFunction =
                    AST_to_ANF.synthesizeEntryFunction "_start" programType userOnly.MainExpr
                let userRegistries : AST_to_ANF.Registries = {
                    TypeReg = userOnly.TypeReg
                    VariantLookup = userOnly.VariantLookup
                    FuncReg = userOnly.FuncReg
                    FuncParams = userOnly.FuncParams
                    ModuleRegistry = userOnly.ModuleRegistry
                }
                match buildAnf 0 coverageOptions sw userRegistries (entryFunction :: userOnly.UserFunctions) None with
                | Error err -> Error err
                | Ok (userFunctions, _typeMap) ->
                    match PrintInsertion.insertPrintInEntry "_start" programType userFunctions with
                    | Error err -> Error $"Print insertion error: {err}"
                    | Ok printedFunctions ->
                        let tcoFunctions = applyTco 0 coverageOptions sw printedFunctions None
                        let reachableStdlibNames =
                            ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph tcoFunctions
                        Ok reachableStdlibNames
