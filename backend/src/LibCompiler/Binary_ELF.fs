// Binary_ELF.fs - ELF Binary Format Types
//
// Defines data structures for the ELF (Executable and Linkable Format)
// used by Linux and other Unix-like systems.
//
// ELF is the standard executable format for Linux. This module defines
// the types needed to represent ELF headers, program headers, and segments.
//
// Structure of an ELF executable:
// - ELF Header: Identifies the file type, architecture, and entry point
// - Program Headers: Describe segments to load into memory
// - Data: The actual code and data
//
// Our minimal executable has:
// - ELF64 header for ARM64
// - One PT_LOAD program header (executable code segment)
// - The machine code

module Binary_ELF

/// ELF magic number (0x7F 'E' 'L' 'F')
let EI_MAG0 = 0x7Fuy
let EI_MAG1 = byte 'E'
let EI_MAG2 = byte 'L'
let EI_MAG3 = byte 'F'

/// ELF class (32 or 64 bit)
let ELFCLASS64 = 2uy

/// ELF data encoding (endianness)
let ELFDATA2LSB = 1uy  // Little-endian

/// ELF version
let EV_CURRENT = 1uy

/// OS/ABI identification
let ELFOSABI_NONE = 0uy  // System V

/// Object file type
let ET_EXEC = 2us  // Executable file

/// Machine architecture
let EM_AARCH64 = 183us  // ARM 64-bit
let EM_X86_64 = 62us    // AMD x86-64

/// Program header type
let PT_LOAD = 1u  // Loadable segment

/// Program header flags
let PF_X = 1u  // Execute
let PF_W = 2u  // Write
let PF_R = 4u  // Read

/// ELF64 Header (64 bytes)
type Elf64Header = {
    Ident: byte array         // 16 bytes: ELF identification
    Type: uint16              // Object file type (ET_EXEC)
    Machine: uint16           // Architecture (EM_AARCH64)
    Version: uint32           // ELF version
    Entry: uint64             // Entry point virtual address
    PhOff: uint64             // Program header table file offset
    ShOff: uint64             // Section header table file offset (0 = none)
    Flags: uint32             // Processor-specific flags
    EhSize: uint16            // ELF header size
    PhEntSize: uint16         // Program header entry size
    PhNum: uint16             // Number of program header entries
    ShEntSize: uint16         // Section header entry size
    ShNum: uint16             // Number of section header entries
    ShStrNdx: uint16          // Section header string table index
}

/// ELF64 Program Header (56 bytes)
type Elf64ProgramHeader = {
    Type: uint32              // Segment type (PT_LOAD)
    Flags: uint32             // Segment flags (PF_R | PF_X)
    Offset: uint64            // Segment file offset
    VAddr: uint64             // Segment virtual address
    PAddr: uint64             // Segment physical address (same as VAddr)
    FileSize: uint64          // Segment size in file
    MemSize: uint64           // Segment size in memory
    Align: uint64             // Segment alignment
}

/// Complete ELF binary structure
type ElfBinary = {
    Header: Elf64Header
    ProgramHeaders: Elf64ProgramHeader list
    MachineCode: byte array
    StringData: byte array  // Constant string data (placed after code)
}
