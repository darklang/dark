// 8_Binary_Generation_ELF.fs - ELF Binary Generation (Pass 8, x64 backend)
//
// Generates a complete ELF64 executable from x86-64 machine code for Linux.
// Uses the shared Elf64Header / Elf64ProgramHeader types from Binary_ELF.fs.
// Machine code is passed as a byte array because x86-64 has variable-length
// instructions (1-15 bytes each).

module Binary_Generation_ELF_X86_64

/// Create an x86-64 ELF executable with float and string data.
/// entryOffset: byte offset of _start within machineCode (default 0).
let createExecutableWithPools
    (machineCode: byte array)
    (stringPool: LiteralPool.StringPool)
    (floatPool: LiteralPool.FloatPool)
    (enableLeakCheck: bool)
    (entryOffset: int)
    : byte array =

    // Create float data (goes after code, before strings)
    let floatBytes = Binary_Generation_ELF.createFloatData floatPool

    // Create string data
    let stringBytes = Binary_Generation_ELF.createStringData stringPool

    let dataBytes =
        let floatAndStringBytes = Array.append floatBytes stringBytes
        let leakBytes = if enableLeakCheck then Array.create 8 0uy else [||]
        let leakStart = ((floatAndStringBytes.Length + 7) / 8) * 8
        let leakPadding = Array.create (leakStart - floatAndStringBytes.Length) 0uy
        if enableLeakCheck then
            Array.concat [floatAndStringBytes; leakPadding; leakBytes]
        else
            floatAndStringBytes

    let codeSize = uint64 machineCode.Length
    let dataSize = uint64 dataBytes.Length

    // ELF structures
    let elfHeaderSize = 64UL
    let programHeaderSize = 56UL
    let numProgramHeaders = 1us

    // Load address - typical for user-space programs
    let baseVAddr = 0x400000UL

    // Code starts right after headers
    let codeFileOffset = elfHeaderSize + (uint64 numProgramHeaders * programHeaderSize)
    let entryVAddr = baseVAddr + codeFileOffset + uint64 entryOffset

    // Create ELF identification bytes
    let ident = Array.create 16 0uy
    ident.[0] <- Binary_ELF.EI_MAG0
    ident.[1] <- Binary_ELF.EI_MAG1
    ident.[2] <- Binary_ELF.EI_MAG2
    ident.[3] <- Binary_ELF.EI_MAG3
    ident.[4] <- Binary_ELF.ELFCLASS64
    ident.[5] <- Binary_ELF.ELFDATA2LSB
    ident.[6] <- Binary_ELF.EV_CURRENT
    ident.[7] <- Binary_ELF.ELFOSABI_NONE

    let header : Binary_ELF.Elf64Header = {
        Ident = ident
        Type = Binary_ELF.ET_EXEC
        Machine = Binary_ELF.EM_X86_64
        Version = 1u
        Entry = entryVAddr
        PhOff = elfHeaderSize
        ShOff = 0UL
        Flags = 0u
        EhSize = uint16 elfHeaderSize
        PhEntSize = uint16 programHeaderSize
        PhNum = numProgramHeaders
        ShEntSize = 0us
        ShNum = 0us
        ShStrNdx = 0us
    }

    let alignedDataOffset = (codeFileOffset + codeSize + 7UL) &&& (~~~7UL)
    let alignmentPadding = alignedDataOffset - (codeFileOffset + codeSize)
    let segmentFileSize = codeFileOffset + codeSize + alignmentPadding + dataSize
    let segmentMemSize = segmentFileSize

    let segmentFlags =
        if enableLeakCheck then
            Binary_ELF.PF_R ||| Binary_ELF.PF_W ||| Binary_ELF.PF_X
        else
            Binary_ELF.PF_R ||| Binary_ELF.PF_X

    let codeSegment : Binary_ELF.Elf64ProgramHeader = {
        Type = Binary_ELF.PT_LOAD
        Flags = segmentFlags
        Offset = 0UL
        VAddr = baseVAddr
        PAddr = baseVAddr
        FileSize = segmentFileSize
        MemSize = segmentMemSize
        Align = 0x1000UL
    }

    let binary : Binary_ELF.ElfBinary = {
        Header = header
        ProgramHeaders = [codeSegment]
        MachineCode = machineCode
        StringData = dataBytes
    }

    Binary_Generation_ELF.serializeElf binary
