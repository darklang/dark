// 7_ARM64_Emit.fs - ARM64 Emission (Encoding + Binary Generation)
//
// Resolves symbolic data labels into literal pools, encodes ARM64 instructions,
// and produces a platform-specific binary in a single pass.

module ARM64_Emit

type EmitResult = {
    MachineCode: ARM64.MachineCode list
    Binary: byte array
}

/// Resolve label refs, encode machine code, and generate a binary for the target OS
let emitBinary
    (instructions: ARM64Symbolic.Instr list)
    (os: Platform.OS)
    (enableLeakCheck: bool)
    : Result<EmitResult, string> =
    let (stringPool, floatPool) = ARM64_Resolve.collectPools instructions
    let machineCode =
        ARM64_Encoding.encodeSymbolicWithPools
            instructions
            stringPool
            floatPool
            os
            enableLeakCheck
    let binary =
        match os with
        | Platform.MacOS ->
            Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
        | Platform.Linux ->
            Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
    Ok { MachineCode = machineCode; Binary = binary }
