// Binary.fs - Mach-O Binary Format Types
//
// Defines data structures for the Mach-O binary format used by macOS.
//
// Mach-O is the executable format for macOS. This module defines the types
// needed to represent Mach-O headers, load commands, segments, and sections.
//
// Structure of a Mach-O executable:
// - Mach-O Header: Identifies the file type and architecture
// - Load Commands: Instructions for the loader (segments, entry point, etc.)
// - Data: The actual code and data sections
//
// Our minimal executable has:
// - __PAGEZERO segment (4GB unmapped memory for security)
// - __TEXT segment with __text section (executable code)
// - LC_MAIN command (specifies entry point)

module Binary

/// Mach-O magic number for 64-bit
let MH_MAGIC_64 = 0xFEEDFACFu

/// CPU type constants
let CPU_TYPE_ARM64 = 0x0100000Cu
let CPU_SUBTYPE_ARM64_ALL = 0u

/// File type constants
let MH_EXECUTE = 0x2u  // Executable file

/// Flags
let MH_NOUNDEFS = 0x1u
let MH_DYLDLINK = 0x4u
let MH_TWOLEVEL = 0x80u
let MH_PIE = 0x200000u

/// Load command types
let LC_SEGMENT_64 = 0x19u
let LC_MAIN = 0x80000028u
let LC_LOAD_DYLINKER = 0xEu
let LC_LOAD_DYLIB = 0xCu
let LC_UUID = 0x1Bu
let LC_BUILD_VERSION = 0x32u
let LC_SYMTAB = 0x2u
let LC_DYSYMTAB = 0xBu

/// Virtual memory protections
let VM_PROT_READ = 0x1u
let VM_PROT_WRITE = 0x2u
let VM_PROT_EXECUTE = 0x4u

/// Section flags
let S_REGULAR = 0x0u
let S_ATTR_PURE_INSTRUCTIONS = 0x80000000u
let S_ATTR_SOME_INSTRUCTIONS = 0x00000400u

/// Mach-O header
type MachHeader = {
    Magic: uint32
    CpuType: uint32
    CpuSubType: uint32
    FileType: uint32
    NumCommands: uint32
    SizeOfCommands: uint32
    Flags: uint32
    Reserved: uint32  // For 64-bit
}

/// Section within a segment
type Section64 = {
    SectionName: string  // Max 16 bytes
    SegmentName: string  // Max 16 bytes
    Address: uint64
    Size: uint64
    Offset: uint32
    Align: uint32
    RelocationOffset: uint32
    NumRelocations: uint32
    Flags: uint32
    Reserved1: uint32
    Reserved2: uint32
    Reserved3: uint32
}

/// LC_SEGMENT_64 load command
type SegmentCommand64 = {
    Command: uint32
    CommandSize: uint32
    SegmentName: string  // Max 16 bytes
    VmAddress: uint64
    VmSize: uint64
    FileOffset: uint64
    FileSize: uint64
    MaxProt: uint32
    InitProt: uint32
    NumSections: uint32
    Flags: uint32
    Sections: Section64 list
}

/// LC_MAIN load command (entry point)
type MainCommand = {
    Command: uint32
    CommandSize: uint32
    EntryOffset: uint64
    StackSize: uint64
}

/// LC_LOAD_DYLINKER load command
type DylinkerCommand = {
    Command: uint32
    CommandSize: uint32
    Name: string  // Path to dylinker (e.g., "/usr/lib/dyld")
}

/// LC_LOAD_DYLIB load command
type DylibCommand = {
    Command: uint32
    CommandSize: uint32
    Name: string  // Path to library (e.g., "/usr/lib/libSystem.B.dylib")
    Timestamp: uint32
    CurrentVersion: uint32
    CompatibilityVersion: uint32
}

/// LC_UUID load command
type UuidCommand = {
    Command: uint32
    CommandSize: uint32
    Uuid: byte array  // 16 bytes
}

/// LC_BUILD_VERSION load command
type BuildVersionCommand = {
    Command: uint32
    CommandSize: uint32
    Platform: uint32  // 1 = macOS
    MinOS: uint32     // Minimum OS version (e.g., 11.0 = 0xB0000)
    Sdk: uint32       // SDK version
    NumTools: uint32  // Number of tool entries (0 for simplicity)
}

/// LC_SYMTAB load command
type SymtabCommand = {
    Command: uint32
    CommandSize: uint32
    SymbolTableOffset: uint32
    NumSymbols: uint32
    StringTableOffset: uint32
    StringTableSize: uint32
}

/// LC_DYSYMTAB load command
type DysymtabCommand = {
    Command: uint32
    CommandSize: uint32
    // Simplified - just the basic fields
    LocalSymIndex: uint32
    NumLocalSymbols: uint32
    ExtDefSymIndex: uint32
    NumExtDefSymbols: uint32
    UndefSymIndex: uint32
    NumUndefSymbols: uint32
    // Zero out the rest
    TocOffset: uint32
    NumTocEntries: uint32
    ModTableOffset: uint32
    NumModTableEntries: uint32
    ExtRefSymOffset: uint32
    NumExtRefSyms: uint32
    IndirectSymOffset: uint32
    NumIndirectSyms: uint32
    ExtRelOffset: uint32
    NumExtRel: uint32
    LocRelOffset: uint32
    NumLocRel: uint32
}

/// Complete Mach-O binary structure
type MachOBinary = {
    Header: MachHeader
    PageZeroCommand: SegmentCommand64
    TextSegmentCommand: SegmentCommand64
    LinkeditSegmentCommand: SegmentCommand64
    DylinkerCommand: DylinkerCommand
    DylibCommand: DylibCommand
    SymtabCommand: SymtabCommand
    DysymtabCommand: DysymtabCommand
    UuidCommand: UuidCommand
    BuildVersionCommand: BuildVersionCommand
    MainCommand: MainCommand
    MachineCode: byte array
    StringData: byte array  // Constant string data (placed after code)
}
