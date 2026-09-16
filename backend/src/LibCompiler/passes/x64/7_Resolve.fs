// 7_X86_64_Resolve.fs - x86-64 Label Resolution and Fixup
//
// Resolves symbolic labels (CALL, JMP, Jcc, LEA_rip) into concrete
// relative offsets. Uses a two-pass approach:
//
// Pass 1: Encode all instructions to get their byte sizes, record
//         label positions and fixup locations.
// Pass 2: Patch rel32 fields with correct relative offsets.
//
// x86-64 relative branches are offset from the END of the instruction
// (i.e., the address of the next instruction), not the start.

module X86_64_Resolve

open X86_64

/// A fixup records where a rel32 placeholder needs to be patched
type Fixup = {
    /// Byte offset in the output where the rel32 starts
    PatchOffset: int
    /// Byte offset of the instruction AFTER this one (where PC will be when executing)
    NextInstrOffset: int
    /// The label name this fixup targets
    TargetLabel: string
}

/// Encode a list of x86-64 instructions with label resolution.
/// Result of resolving and encoding
type ResolveResult = {
    MachineCode: byte array
    LabelPositions: Map<string, int>
    /// Fixups deferred for data labels resolved after code size is known
    DeferredFixups: Fixup list
}

/// Returns the final machine code bytes and label positions.
let resolveAndEncode (instructions: Instr list) : Result<ResolveResult, string> =
    // Pass 1: encode all instructions, collect label positions and fixups
    let mutable labelPositions : Map<string, int> = Map.empty
    let mutable fixups : Fixup list = []
    let mutable offset = 0
    let mutable encodedChunks : (byte array) list = []

    for instr in instructions do
        match instr with
        | Label name ->
            labelPositions <- Map.add name offset labelPositions
            // Labels emit no bytes
        | CALL label ->
            let bytes = X86_64_Encoding.encodeInstruction instr
            fixups <- { PatchOffset = offset + 1  // rel32 starts after the 0xE8 opcode byte
                        NextInstrOffset = offset + bytes.Length
                        TargetLabel = label } :: fixups
            encodedChunks <- bytes :: encodedChunks
            offset <- offset + bytes.Length
        | JMP label ->
            let bytes = X86_64_Encoding.encodeInstruction instr
            fixups <- { PatchOffset = offset + 1  // rel32 starts after the 0xE9 opcode byte
                        NextInstrOffset = offset + bytes.Length
                        TargetLabel = label } :: fixups
            encodedChunks <- bytes :: encodedChunks
            offset <- offset + bytes.Length
        | Jcc (_, label) ->
            let bytes = X86_64_Encoding.encodeInstruction instr
            fixups <- { PatchOffset = offset + 2  // rel32 starts after 0F 8x (2-byte opcode)
                        NextInstrOffset = offset + bytes.Length
                        TargetLabel = label } :: fixups
            encodedChunks <- bytes :: encodedChunks
            offset <- offset + bytes.Length
        | LEA_rip (_, label) ->
            let bytes = X86_64_Encoding.encodeInstruction instr
            // REX.W 8D ModR/M disp32 — the disp32 starts at offset 3 (REX + opcode + ModR/M)
            fixups <- { PatchOffset = offset + 3
                        NextInstrOffset = offset + bytes.Length
                        TargetLabel = label } :: fixups
            encodedChunks <- bytes :: encodedChunks
            offset <- offset + bytes.Length
        | _ ->
            let bytes = X86_64_Encoding.encodeInstruction instr
            encodedChunks <- bytes :: encodedChunks
            offset <- offset + bytes.Length

    // Concatenate all encoded chunks
    let result = encodedChunks |> List.rev |> Array.concat

    // Pass 2: apply fixups (defer unknown labels for data label patching later)
    let mutable deferred : Fixup list = []
    for fixup in fixups do
        match Map.tryFind fixup.TargetLabel labelPositions with
        | None ->
            deferred <- fixup :: deferred
        | Some targetOffset ->
            // rel32 = target - nextInstr
            let rel = targetOffset - fixup.NextInstrOffset
            let relBytes = [|
                byte (uint32 rel &&& 0xFFu)
                byte ((uint32 rel >>> 8) &&& 0xFFu)
                byte ((uint32 rel >>> 16) &&& 0xFFu)
                byte ((uint32 rel >>> 24) &&& 0xFFu)
            |]
            result.[fixup.PatchOffset] <- relBytes.[0]
            result.[fixup.PatchOffset + 1] <- relBytes.[1]
            result.[fixup.PatchOffset + 2] <- relBytes.[2]
            result.[fixup.PatchOffset + 3] <- relBytes.[3]

    Ok { MachineCode = result; LabelPositions = labelPositions; DeferredFixups = List.rev deferred }

/// Patch deferred fixups with data label positions.
/// dataLabels maps label names to file offsets. codeFileOffset is where code starts in the file.
let patchDataLabels (result: ResolveResult) (dataLabels: Map<string, int>) (codeFileOffset: int) : Result<ResolveResult, string> =
    let mutable errors : string list = []
    for fixup in result.DeferredFixups do
        match Map.tryFind fixup.TargetLabel dataLabels with
        | None ->
            errors <- $"Undefined label: {fixup.TargetLabel}" :: errors
        | Some fileOffset ->
            let targetCodeOffset = fileOffset - codeFileOffset
            let rel = targetCodeOffset - fixup.NextInstrOffset
            let relBytes = [|
                byte (uint32 rel &&& 0xFFu)
                byte ((uint32 rel >>> 8) &&& 0xFFu)
                byte ((uint32 rel >>> 16) &&& 0xFFu)
                byte ((uint32 rel >>> 24) &&& 0xFFu)
            |]
            result.MachineCode.[fixup.PatchOffset] <- relBytes.[0]
            result.MachineCode.[fixup.PatchOffset + 1] <- relBytes.[1]
            result.MachineCode.[fixup.PatchOffset + 2] <- relBytes.[2]
            result.MachineCode.[fixup.PatchOffset + 3] <- relBytes.[3]
    if errors.IsEmpty then
        Ok { result with DeferredFixups = [] }
    else
        Error (String.concat "\n" (List.rev errors))
