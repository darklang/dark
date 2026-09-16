// Runtime.fs - Runtime Support Functions
//
// Provides runtime support for language features that require complex
// instruction sequences (e.g., I/O, string conversion).
//
// Current functions:
// - generatePrintInt64: Convert int64 in X0 to ASCII and print to stdout
//
// Platform-specific:
// - Uses Platform.fs to get correct syscall numbers for the target OS

module Runtime

/// Generate ARM64 instructions to print int64 in X0 to stdout with newline
/// Then exit with code 0
///
/// Algorithm:
/// 1. Allocate 32-byte stack buffer
/// 2. Convert X0 (int64) to ASCII decimal string (work backwards from end)
/// 3. Handle negative numbers (negate value, prepend '-')
/// 4. Handle special case: zero
/// 5. Write buffer to stdout via write syscall
/// 6. Exit with code 0 via exit syscall
///
/// Register usage:
/// - X0: Input value, then stdout fd, then exit code
/// - X1: Buffer pointer (works backwards from end)
/// - X2: Working value / length / temp
/// - X3: Divisor (10) / digit / temp
/// - X4: Quotient from division
/// - X5: Remainder (digit to store)
/// - X6: Negative flag (0 = positive, 1 = negative)
/// - X16: Syscall number
///
/// Uses platform-specific syscall numbers from Platform module
let generatePrintInt64 () : ARM64.Instr list =
    // Platform detection at runtime - if it fails, default to Linux (most common for development)
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 32 bytes on stack for buffer (plenty for 64-bit number + sign + newline)
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

        // Setup: X1 = buffer pointer (start at end, work backwards)
        // X2 = value to print (from X0)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)  // X1 = SP + 31 (end of buffer)
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)  // X2 = value to convert

        // Store newline at end of buffer
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // '\n' = 10
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back

        // Instruction 6-7: Check if negative and set flag in X6
        ARM64.MOVZ (ARM64.X6, 0us, 0)  // 6: X6 = 0 (assume positive)
        ARM64.TBNZ (ARM64.X2, 63, 29)  // 7: If negative, branch forward +29 to inst 36 (handle_negative)

        // Instruction 8: Check if zero (branch forward +24 to print_zero at inst 32)
        ARM64.CBZ_offset (ARM64.X2, 24)

        // Instruction 9-17: convert_loop - Extract digits by dividing by 10
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // 9: divisor = 10
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)  // 10: X4 = value / 10
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)  // 11: X5 = value % 10
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // 12: Convert to ASCII
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)  // 13: Store digit
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 14: Move pointer back
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)  // 15: value = value / 10
        ARM64.CBZ_offset (ARM64.X2, 2)  // 16: If zero, skip (+2) to store_minus_if_needed at inst 18
        ARM64.B (-8)  // 17: Loop back (-8) to inst 9 (convert_loop start)

        // Instruction 18-21: store_minus_if_needed - If X6=1 (was negative), store '-'
        ARM64.CBZ_offset (ARM64.X6, 4)  // 18: If X6=0 (positive), skip (+4) to write_output at inst 22
        ARM64.MOVZ (ARM64.X3, 45us, 0)  // 19: '-' = ASCII 45
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)  // 20: Store '-' at X1
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 21: Move pointer back

        // Instruction 22-31: write_output - Write buffer to stdout
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)  // 22: X1 was one past first char
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)  // 23: End of buffer
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)  // 24: length = end - start
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // 25: stdout = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)  // 26: write syscall (X16 macOS, X8 Linux)
        ARM64.SVC syscalls.SvcImmediate  // 27: call write (platform-specific)
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // 28: Deallocate stack
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // 29: exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)  // 30: exit syscall (X16 macOS, X8 Linux)
        ARM64.SVC syscalls.SvcImmediate  // 31: call exit (platform-specific)

        // Instruction 32-35: print_zero - Special case for value 0
        ARM64.MOVZ (ARM64.X2, 48us, 0)  // 32: '0' = 48
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)  // 33: Store '0'
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 34: Move pointer back
        ARM64.B (-13)  // 35: Branch back (-13) to inst 22 (write_output)

        // Instruction 36-38: handle_negative - Negate value and set flag
        ARM64.NEG (ARM64.X2, ARM64.X2)  // 36: X2 = -X2 (get absolute value)
        ARM64.MOVZ (ARM64.X6, 1us, 0)  // 37: X6 = 1 (negative flag)
        ARM64.B (-30)  // 38: Branch back (-30) to inst 8 (CBZ zero check)
    ]

/// Generate ARM64 instructions to print boolean in X0 to stdout with newline
/// Then exit with code 0
///
/// Algorithm:
/// 1. Check if X0 == 0 (false) or != 0 (true)
/// 2. Store "true\n" or "false\n" on stack
/// 3. Write to stdout via write syscall
/// 4. Exit with code 0
///
/// Register usage:
/// - X0: Input boolean value (0=false, non-zero=true)
/// - X1: Buffer pointer
/// - X2: Length
/// - X3: Temp for storing chars
/// - X16/X8: Syscall number
let generatePrintBool () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 16 bytes on stack for buffer (16-byte aligned)
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

        // Check if value is false (X0 == 0)
        ARM64.CBZ_offset (ARM64.X0, 17)  // If false, branch to print_false (+17 instructions)

        // print_true: Store "true\n" on stack (5 bytes)
        // 't' = 116, 'r' = 114, 'u' = 117, 'e' = 101, '\n' = 10
        ARM64.MOVZ (ARM64.X3, 116us, 0)  // 't'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 114us, 0)  // 'r'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 117us, 0)  // 'u'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X2, 5us, 0)    // length = 5

        // Write and exit
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.B (16)  // Jump to cleanup_and_exit (+16 instructions)

        // print_false: Store "false\n" on stack (6 bytes)
        // 'f' = 102, 'a' = 97, 'l' = 108, 's' = 115, 'e' = 101, '\n' = 10
        ARM64.MOVZ (ARM64.X3, 102us, 0)  // 'f'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 97us, 0)   // 'a'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 108us, 0)  // 'l'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 115us, 0)  // 's'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 5)
        ARM64.MOVZ (ARM64.X2, 6us, 0)    // length = 6

        // Write
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // cleanup_and_exit:
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)  // Deallocate stack
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print string to stdout with newline, then exit
///
/// Assumes:
/// - X0 = string address (set up by caller via ADRP + ADD_label)
/// - stringLen = length of string to print (not including any newline)
///
/// Algorithm:
/// 1. Save string address
/// 2. Print string via write syscall
/// 3. Print newline via write syscall
/// 4. Exit with code 0
///
/// Register usage:
/// - X0: string address, then stdout fd
/// - X1: buffer pointer
/// - X2: length
/// - X16/X8: Syscall number (platform-specific)
let generatePrintString (stringLen: int) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // String layout: [length:8][data:N] - skip length prefix
        // X0 points to string start (length field), data is at X0+8
        ARM64.ADD_imm (ARM64.X1, ARM64.X0, 8us)  // X1 = data address (skip 8-byte length)
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // X0 = stdout fd (1)

        // Load string length into X2
        // For lengths > 16 bits, we'd need MOVK, but strings this long are unlikely
        ARM64.MOVZ (ARM64.X2, uint16 stringLen, 0)  // X2 = length

        // Call write syscall
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Now print newline: allocate 1 byte on stack
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)  // 16-byte aligned
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // '\n' = 10
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)  // Store newline at SP

        // Write newline
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer = SP
        ARM64.MOVZ (ARM64.X2, 1us, 0)  // length = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Cleanup stack
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)

        // Exit with code 0
        ARM64.MOVZ (ARM64.X0, 0us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print float in D0 to stdout with newline
/// Then exit with code 0
///
/// Algorithm:
/// 1. Extract integer part: FCVTZS X0, D0
/// 2. Check if negative and handle sign
/// 3. Print integer part (absolute value)
/// 4. Print decimal point '.'
/// 5. Extract fractional part: frac = abs(D0) - floor(abs(D0))
/// 6. Multiply by 10^2 to get 2 decimal digits
/// 7. Print fractional digits, trimming a trailing zero
/// 8. Print newline and exit
///
/// Register usage:
/// - D0: Input float value
/// - D1-D3: Working FP registers
/// - X0-X6: Working integer registers (same as PrintInt64)
/// - X7: Loop counter for fractional digits
/// - X16/X8: Syscall number (platform-specific)
let generatePrintFloat () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 48 bytes on stack for buffer (room for sign, digits, decimal, digits, newline)
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

        // Save D0 in case we need the original value
        // Store it on stack at [SP+32] (8 bytes for double)
        ARM64.STR_fp (ARM64.D0, ARM64.SP, 32s)

        // Check if negative using D0's sign bit (before FCVTZS, which loses sign for -0.x values)
        ARM64.MOVZ (ARM64.X6, 0us, 0)  // X6 = 0 (assume positive)
        ARM64.FMOV_to_gp (ARM64.X0, ARM64.D0)  // Get bit pattern of D0
        ARM64.TBNZ (ARM64.X0, 63, 2)  // If sign bit set, set X6 = 1
        ARM64.B (2)  // Skip setting X6
        ARM64.MOVZ (ARM64.X6, 1us, 0)  // X6 = 1 (negative)

        // Setup: X1 = buffer pointer (start at end, work backwards)
        // Use SP+31 as the end of buffer (we'll build digits backwards from here)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)  // X1 = SP + 31 (end of buffer)

        // Extract integer part using FCVTZS (truncate toward zero)
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)  // X0 = integer part (may be negative)
        // Make X2 = absolute value of X0
        ARM64.TBNZ (ARM64.X0, 63, 3)  // If negative, skip the MOV and go to NEG
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)  // X2 = X0 (positive)
        ARM64.B (2)  // Skip NEG
        ARM64.NEG (ARM64.X2, ARM64.X0)  // X2 = -X0 (make positive)

        // === Print integer part (positive value in X2) ===
        // Label: after_sign_check
        // Instruction offset: 14

        // Check if integer part is zero (branch to print_zero_int)
        ARM64.CBZ_offset (ARM64.X2, 64)  // If zero, branch to print_zero_int (inst 77)

        // convert_int_loop - Extract digits by dividing by 10
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // divisor = 10
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)  // X4 = value / 10
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)  // X5 = value % 10
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // Convert to ASCII
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)  // Store digit
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)  // value = value / 10
        ARM64.CBZ_offset (ARM64.X2, 2)  // If zero, done with integer
        ARM64.B (-8)  // Loop back

        // store_minus_if_needed - If X6=1 (was negative), store '-'
        ARM64.CBZ_offset (ARM64.X6, 4)  // If X6=0 (positive), skip
        ARM64.MOVZ (ARM64.X3, 45us, 0)  // '-' = ASCII 45
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)  // Store '-'
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back

        // print_integer_part - Write integer digits to stdout
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)  // X1 was one past first char
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)  // End of int buffer
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)  // length = end - start
        // Save X6 to stack at [SP+40] before syscalls (syscalls may clobber X0-X7)
        ARM64.STR (ARM64.X6, ARM64.SP, 40s)
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_decimal_point - Print '.'
        ARM64.MOVZ (ARM64.X3, 46us, 0)  // '.' = 46
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)  // Store at [SP]
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // buffer = SP
        ARM64.MOVZ (ARM64.X2, 1us, 0)  // length = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // === Extract and print fractional part (1 or 2 decimal digits) ===
        // Reload original float value
        ARM64.LDR_fp (ARM64.D0, ARM64.SP, 32s)  // Reload original value

        // Get integer part again and compute fractional part
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)  // X0 = integer part (signed)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)  // D1 = (double)integer (signed)
        ARM64.FSUB (ARM64.D0, ARM64.D0, ARM64.D1)  // D0 = fractional part (signed, same sign as original)

        // Multiply fractional part by 100 to get 2 digits
        ARM64.MOVZ (ARM64.X0, 100us, 0)  // 100
        ARM64.SCVTF (ARM64.D1, ARM64.X0)  // D1 = 100.0
        ARM64.FMUL (ARM64.D0, ARM64.D0, ARM64.D1)  // D0 = frac * 100 (might be negative)
        ARM64.FCVTZS (ARM64.X7, ARM64.D0)  // X7 = fractional digits as integer (might be negative)

        // Take absolute value of X7 if negative
        ARM64.TBNZ (ARM64.X7, 63, 2)  // If sign bit set, skip the B
        ARM64.B (2)  // Skip NEG if positive
        ARM64.NEG (ARM64.X7, ARM64.X7)  // Negate to make positive

        // Extract both digits BEFORE any syscalls (syscalls clobber X0-X7)
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X7, ARM64.X3)  // X4 = X7 / 10 (tens digit)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X7)  // X5 = X7 % 10 (ones digit)
        // Store digits at [SP+1] and [SP+2] for later
        ARM64.ADD_imm (ARM64.X4, ARM64.X4, 48us)  // Convert tens to ASCII
        ARM64.STRB (ARM64.X4, ARM64.SP, 1)  // Store tens digit at [SP+1]
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // Convert ones to ASCII
        ARM64.STRB (ARM64.X5, ARM64.SP, 2)  // Store ones digit at [SP+2]

        // Print one or two digits in one syscall (trim trailing zero)
        ARM64.MOVZ (ARM64.X0, 1us, 0)  // stdout
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 1us)  // buffer at [SP+1]
        ARM64.SUBS_imm (ARM64.X5, ARM64.X5, 48us)  // Set flags if ones digit was '0'
        ARM64.CSET (ARM64.X2, ARM64.NE)  // X2 = 1 when ones digit != 0
        ARM64.ADD_imm (ARM64.X2, ARM64.X2, 1us)  // length = 1 or 2
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_newline_and_exit
        ARM64.MOVZ (ARM64.X3, 10us, 0)  // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X2, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Cleanup and exit
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)  // Deallocate stack
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_zero_int - Integer part is zero, just store '0' (instruction 75)
        ARM64.MOVZ (ARM64.X2, 48us, 0)  // '0' = 48
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)  // Store '0'
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back
        ARM64.B (-57)  // Branch back to store_minus_if_needed (inst 23)
    ]

/// Generate ARM64 instructions to exit with code 0
let generateExit () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        ARM64.MOVZ (ARM64.X0, 0us, 0)  // exit code = 0
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Exit, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print int64 in X0 to stdout with newline (NO EXIT)
/// Same as generatePrintInt64 but returns instead of exiting
let generatePrintInt64NoExit () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 32 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

        // Setup: X1 = buffer pointer, X2 = value
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)

        // Store newline at end of buffer
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)

        // Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
        ARM64.MOVZ (ARM64.X6, 0us, 0)
        ARM64.MOVZ (ARM64.X3, 10us, 0)

        // Check for negative: if X2 < 0, branch to handle_negative (at instruction 33)
        ARM64.CMP_imm (ARM64.X2, 0us)
        ARM64.B_cond (ARM64.LT, 25)  // 33 - 8 = 25

        // Check for zero: if X2 == 0, branch to print_zero (at instruction 29)
        ARM64.CBZ_offset (ARM64.X2, 20)  // 29 - 9 = 20

        // digit_loop: Extract digits
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBNZ_offset (ARM64.X2, -6)

        // store_minus_if_needed
        ARM64.CBZ_offset (ARM64.X6, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // write_output
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // Deallocate stack
        ARM64.B (8)  // Skip past print_zero (4) and handle_negative (3) + 1 to exit runtime (8 instructions)

        // print_zero (at instruction 29)
        ARM64.MOVZ (ARM64.X2, 48us, 0)
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.B (-15)  // Jump to store_minus_if_needed: 17 - 32 = -15

        // handle_negative (at instruction 33)
        ARM64.NEG (ARM64.X2, ARM64.X2)
        ARM64.MOVZ (ARM64.X6, 1us, 0)
        ARM64.B (-26)  // Jump to check_zero: 9 - 35 = -26
    ]

/// Generate ARM64 instructions to print int64 in X0 to stderr with newline (NO EXIT)
/// Same as generatePrintInt64NoExit but writes to file descriptor 2
let generatePrintInt64ToStderrNoExit () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 32 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

        // Setup: X1 = buffer pointer, X2 = value
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)

        // Store newline at end of buffer
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)

        // Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
        ARM64.MOVZ (ARM64.X6, 0us, 0)
        ARM64.MOVZ (ARM64.X3, 10us, 0)

        // Check for negative: if X2 < 0, branch to handle_negative (at instruction 33)
        ARM64.CMP_imm (ARM64.X2, 0us)
        ARM64.B_cond (ARM64.LT, 25)  // 33 - 8 = 25

        // Check for zero: if X2 == 0, branch to print_zero (at instruction 29)
        ARM64.CBZ_offset (ARM64.X2, 20)  // 29 - 9 = 20

        // digit_loop: Extract digits
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBNZ_offset (ARM64.X2, -6)

        // store_minus_if_needed
        ARM64.CBZ_offset (ARM64.X6, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // write_output
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)
        ARM64.MOVZ (ARM64.X0, 2us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // Deallocate stack
        ARM64.B (8)  // Skip past print_zero (4) and handle_negative (3) + 1 to exit runtime (8 instructions)

        // print_zero (at instruction 29)
        ARM64.MOVZ (ARM64.X2, 48us, 0)
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.B (-15)  // Jump to store_minus_if_needed: 17 - 32 = -15

        // handle_negative (at instruction 33)
        ARM64.NEG (ARM64.X2, ARM64.X2)
        ARM64.MOVZ (ARM64.X6, 1us, 0)
        ARM64.B (-26)  // Jump to check_zero: 9 - 35 = -26
    ]

/// Generate ARM64 instructions to print boolean in X0 to stdout with newline (NO EXIT)
/// Same as generatePrintBool but returns instead of exiting
let generatePrintBoolNoExit () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 16 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

        // Check if false (X0 == 0), branch to print_false (+17 instructions)
        ARM64.CBZ_offset (ARM64.X0, 17)

        // print_true: Store "true\n" on stack (5 bytes)
        ARM64.MOVZ (ARM64.X3, 116us, 0)  // 't'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 114us, 0)  // 'r'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 117us, 0)  // 'u'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X2, 5us, 0)    // length = 5

        // Write and cleanup (no exit)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.B (18)  // Jump to cleanup (+18 instructions to skip false branch)

        // print_false: Store "false\n" on stack (6 bytes)
        ARM64.MOVZ (ARM64.X3, 102us, 0)  // 'f'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 97us, 0)   // 'a'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 108us, 0)  // 'l'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 115us, 0)  // 's'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X3, 10us, 0)   // '\n'
        ARM64.STRB (ARM64.X3, ARM64.SP, 5)
        ARM64.MOVZ (ARM64.X2, 6us, 0)    // length = 6

        // Write
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // cleanup (no exit):
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
    ]

/// Generate ARM64 instructions to print int64 in X0 to stdout WITHOUT newline
/// For use in tuple/list element printing
let generatePrintInt64NoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 32 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

        // Setup: X1 = buffer pointer (start at end-1, no newline), X2 = value
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 30us)  // One less than with newline
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)

        // Initialize: X6 = 0 (positive flag), X3 = 10 (divisor)
        ARM64.MOVZ (ARM64.X6, 0us, 0)
        ARM64.MOVZ (ARM64.X3, 10us, 0)

        // Check for negative: if X2 < 0, branch to handle_negative (at index 31)
        ARM64.CMP_imm (ARM64.X2, 0us)
        ARM64.B_cond (ARM64.LT, 25)  // 31 - 6 = 25

        // Check for zero: if X2 == 0, branch to print_zero (at index 27)
        ARM64.CBZ_offset (ARM64.X2, 20)  // 27 - 7 = 20

        // digit_loop: Extract digits
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBNZ_offset (ARM64.X2, -6)

        // store_minus_if_needed
        ARM64.CBZ_offset (ARM64.X6, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // write_output
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 31us)  // End of buffer area
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // Deallocate stack
        ARM64.B (8)  // Skip past print_zero (4) and handle_negative (3) + 1 to exit

        // print_zero (at index 27)
        ARM64.MOVZ (ARM64.X2, 48us, 0)
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.B (-15)  // Jump to store_minus_if_needed at index 15: 15 - 30 = -15

        // handle_negative (at index 31)
        ARM64.NEG (ARM64.X2, ARM64.X2)
        ARM64.MOVZ (ARM64.X6, 1us, 0)
        ARM64.B (-25)  // Jump to digit_loop at index 8: 8 - 33 = -25
    ]

/// Generate ARM64 instructions to print boolean in X0 to stdout WITHOUT newline
/// For use in tuple/list element printing
let generatePrintBoolNoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 16 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

        // Check if false (X0 == 0), branch to print_false at index 16
        ARM64.CBZ_offset (ARM64.X0, 15)  // 16 - 1 = 15

        // print_true: Store "true" on stack (4 bytes, no newline)
        ARM64.MOVZ (ARM64.X3, 116us, 0)  // 't'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 114us, 0)  // 'r'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 117us, 0)  // 'u'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X2, 4us, 0)    // length = 4 (no newline)

        // Write and cleanup
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.B (16)  // Jump to cleanup at index 31: 31 - 15 = 16

        // print_false: Store "false" on stack (5 bytes, no newline)
        ARM64.MOVZ (ARM64.X3, 102us, 0)  // 'f'
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, 97us, 0)   // 'a'
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, 108us, 0)  // 'l'
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, 115us, 0)  // 's'
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, 101us, 0)  // 'e'
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X2, 5us, 0)    // length = 5 (no newline)

        // Write
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // cleanup:
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
    ]

/// Generate ARM64 instructions to print float in D0 to stdout WITHOUT newline
/// For use in tuple/list element printing
/// Similar to generatePrintFloat but doesn't add newline or exit
let generatePrintFloatNoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Allocate 48 bytes on stack for buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

        // Save D0 at [SP+32]
        ARM64.STR_fp (ARM64.D0, ARM64.SP, 32s)

        // Check if negative using D0's sign bit
        ARM64.MOVZ (ARM64.X6, 0us, 0)  // X6 = 0 (assume positive)
        ARM64.FMOV_to_gp (ARM64.X0, ARM64.D0)  // Get bit pattern
        ARM64.TBNZ (ARM64.X0, 63, 2)  // If sign bit set, set X6 = 1
        ARM64.B (2)  // Skip setting X6
        ARM64.MOVZ (ARM64.X6, 1us, 0)  // X6 = 1 (negative)

        // Setup: X1 = buffer pointer (start at end)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)

        // Extract integer part
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)
        ARM64.TBNZ (ARM64.X0, 63, 3)
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)
        ARM64.B (2)
        ARM64.NEG (ARM64.X2, ARM64.X0)

        // Check if integer part is zero
        ARM64.CBZ_offset (ARM64.X2, 55)  // Branch to print_zero_int (instruction 69)

        // convert_int_loop
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBZ_offset (ARM64.X2, 2)
        ARM64.B (-8)

        // store_minus_if_needed
        ARM64.CBZ_offset (ARM64.X6, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // print_integer_part
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)
        ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)
        ARM64.STR (ARM64.X6, ARM64.SP, 40s)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // print_decimal_point
        ARM64.MOVZ (ARM64.X3, 46us, 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X2, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Extract and print fractional part (1 or 2 decimal digits)
        ARM64.LDR_fp (ARM64.D0, ARM64.SP, 32s)
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)
        ARM64.FSUB (ARM64.D0, ARM64.D0, ARM64.D1)
        ARM64.MOVZ (ARM64.X0, 100us, 0)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)
        ARM64.FMUL (ARM64.D0, ARM64.D0, ARM64.D1)
        ARM64.FCVTZS (ARM64.X7, ARM64.D0)

        // Take absolute value of X7
        ARM64.TBNZ (ARM64.X7, 63, 2)
        ARM64.B (2)
        ARM64.NEG (ARM64.X7, ARM64.X7)

        // Extract digits
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X7, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X7)
        ARM64.ADD_imm (ARM64.X4, ARM64.X4, 48us)
        ARM64.STRB (ARM64.X4, ARM64.SP, 1)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.SP, 2)

        // Print one or two digits (trim trailing zero)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 1us)
        ARM64.SUBS_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.CSET (ARM64.X2, ARM64.NE)
        ARM64.ADD_imm (ARM64.X2, ARM64.X2, 1us)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate

        // Cleanup (no newline, no exit)
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
        ARM64.B (5)  // Skip past print_zero_int (4 instructions + 1 to land after)

        // print_zero_int (instruction 69)
        ARM64.MOVZ (ARM64.X2, 48us, 0)
        ARM64.STRB (ARM64.X2, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.B (-48)  // Branch back to store_minus_if_needed (instruction 24)
    ]

/// Generate ARM64 instructions to print heap string WITHOUT newline
/// Expects: X9 = data address, X10 = length
/// For use in tuple/list element printing
let generatePrintStringNoNewline () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        // Write string to stdout
        ARM64.MOVZ (ARM64.X0, 1us, 0)        // fd = stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.X9)   // buffer = X9
        ARM64.MOV_reg (ARM64.X2, ARM64.X10)  // length = X10
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions to print a sequence of literal characters
/// Used for printing delimiters like "(", ")", "[", "]", ", " etc.
///
/// Algorithm:
/// 1. Allocate aligned stack buffer
/// 2. Store each byte on stack
/// 3. Write to stdout via syscall
/// 4. Deallocate stack
///
/// No newline is added - caller controls newlines
let generatePrintChars (chars: byte list) : ARM64.Instr list =
    if List.isEmpty chars then [] else
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    let len = List.length chars
    // Stack allocation must be 16-byte aligned
    let stackSize = max 16 ((len + 15) / 16 * 16)
    [
        // Allocate stack buffer
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 stackSize)
    ]
    @ (chars |> List.mapi (fun i b ->
        [
            ARM64.MOVZ (ARM64.X3, uint16 b, 0)
            ARM64.STRB (ARM64.X3, ARM64.SP, i)
        ]) |> List.concat)
    @ [
        // Write to stdout
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)          // buffer
        ARM64.MOVZ (ARM64.X2, uint16 len, 0)        // length
        ARM64.MOVZ (ARM64.X0, 1us, 0)               // stdout = 1
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        // Deallocate stack
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 stackSize)
    ]

/// Generate ARM64 instructions to print literal characters to stderr
let generatePrintCharsToStderr (chars: byte list) : ARM64.Instr list =
    if List.isEmpty chars then [] else
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    let len = List.length chars
    let stackSize = max 16 ((len + 15) / 16 * 16)
    [
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 stackSize)
    ]
    @ (chars |> List.mapi (fun i b ->
        [
            ARM64.MOVZ (ARM64.X3, uint16 b, 0)
            ARM64.STRB (ARM64.X3, ARM64.SP, i)
        ]) |> List.concat)
    @ [
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X2, uint16 len, 0)
        ARM64.MOVZ (ARM64.X0, 2us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 stackSize)
    ]

/// Generate ARM64 instructions to print bytes as "<N bytes>\n"
/// Expects: X19 = bytes pointer (callee-saved)
/// Bytes layout: [length:8][data:N][refcount:8]
let generatePrintBytes () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    // Print "<"
    [
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)
        ARM64.MOVZ (ARM64.X3, uint16 (byte '<'), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X2, 1us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
        // Load length from [X19] into X0
        ARM64.LDR (ARM64.X0, ARM64.X19, 0s)
    ]
    @ generatePrintInt64NoNewline ()
    @ [
        // Print " bytes>\n" (8 characters)
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)
        ARM64.MOVZ (ARM64.X3, uint16 (byte ' '), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 0)
        ARM64.MOVZ (ARM64.X3, uint16 (byte 'b'), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 1)
        ARM64.MOVZ (ARM64.X3, uint16 (byte 'y'), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 2)
        ARM64.MOVZ (ARM64.X3, uint16 (byte 't'), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 3)
        ARM64.MOVZ (ARM64.X3, uint16 (byte 'e'), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 4)
        ARM64.MOVZ (ARM64.X3, uint16 (byte 's'), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 5)
        ARM64.MOVZ (ARM64.X3, uint16 (byte '>'), 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 6)
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.STRB (ARM64.X3, ARM64.SP, 7)
        ARM64.MOVZ (ARM64.X0, 1us, 0)
        ARM64.MOV_reg (ARM64.X1, ARM64.SP)
        ARM64.MOVZ (ARM64.X2, 8us, 0)
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
    ]

/// Generate ARM64 instructions to perform write syscall only
///
/// Assumes caller has set up:
/// - X0 = file descriptor (usually 1 for stdout)
/// - X1 = buffer pointer
/// - X2 = length
///
/// Does NOT print newline or exit - caller handles those if needed
let generateWriteSyscall () : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os
    [
        ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
        ARM64.SVC syscalls.SvcImmediate
    ]

/// Generate ARM64 instructions for Stdlib.File.exists
/// Input: pathReg contains heap string pointer (format: [len:8][data:N])
/// Output: destReg = 1 if file exists, 0 if not
///
/// Algorithm:
/// 1. Get data pointer from heap string (path + 8)
/// 2. Save the path string to a null-terminated temp buffer on stack
/// 3. Call access(path, F_OK) or faccessat(AT_FDCWD, path, F_OK, 0)
/// 4. If syscall returns 0 (success), set dest = 1; else dest = 0
let generateFileExists (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    // We need to null-terminate the string for the syscall
    // Stack layout: [saved regs:16][path:256]
    // Use simpler approach: just copy the string and add null terminator
    // Note: This is a simplification - paths > 255 chars will be truncated
    match os with
    | Platform.MacOS ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes, 16-byte aligned)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg), X20 = dest pointer for later
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // Use non-allocatable registers for copy loop to avoid clobbering live values
            // X10 = string length from [X19]
            ARM64.LDR (ARM64.X10, ARM64.X19, 0s)

            // X9 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X9, ARM64.SP)
            // X11 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 8us)

            // Copy loop: copies X10 bytes from X11 to X9
            // copy_loop (7 instructions, 0-6):
            ARM64.CBZ_offset (ARM64.X10, 7)     // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0) // 1: Load byte from src
            ARM64.STRB (ARM64.X12, ARM64.X9, 0) // 2: Store byte to dest
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us) // 3: dest++
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us) // 4: src++
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us) // 5: len--
            ARM64.B (-6)                        // 6: Loop back to CBZ

            // null_term: Store null terminator at X9
            ARM64.MOVZ (ARM64.X12, 0us, 0)      // 7: X12 = 0
            ARM64.STRB (ARM64.X12, ARM64.X9, 0) // 8: Store 0 as null terminator

            // Call access(path, F_OK)
            // X0 = path (SP), X1 = mode (F_OK = 0)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, 0us, 0)       // F_OK = 0
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Access, 0)  // access syscall
            ARM64.SVC syscalls.SvcImmediate

            // If X0 == 0, file exists; else doesn't
            // Use CMP + CSET to convert to boolean
            ARM64.CMP_imm (ARM64.X0, 0us)
            ARM64.CSET (ARM64.X0, ARM64.EQ)     // X0 = 1 if was 0, else 0

            // Cleanup - restore registers before moving result to dest
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)  // Deallocate path buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]
    | Platform.Linux ->
        [
            // Save callee-saved registers we'll use, plus extra space for caller-saved
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)  // Save X21, X22 for preserving X1, X2
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

            // Allocate stack space for path (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg)
            ARM64.MOV_reg (ARM64.X19, pathReg)

            // Save potentially live caller-saved registers (X1-X4) to callee-saved registers
            // This preserves any values that might be allocated to these registers
            ARM64.MOV_reg (ARM64.X21, ARM64.X1)  // Save X1
            ARM64.MOV_reg (ARM64.X22, ARM64.X2)  // Save X2

            // Use non-allocatable registers for copy loop to avoid clobbering live values
            // X10 = string length from [X19]
            ARM64.LDR (ARM64.X10, ARM64.X19, 0s)

            // X9 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X9, ARM64.SP)
            // X11 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 8us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)     // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0) // 1: Load byte from src
            ARM64.STRB (ARM64.X12, ARM64.X9, 0) // 2: Store byte to dest
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us) // 3: dest++
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us) // 4: src++
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us) // 5: len--
            ARM64.B (-6)                        // 6: Loop back to CBZ

            // null_term: Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)      // 7: X12 = 0
            ARM64.STRB (ARM64.X12, ARM64.X9, 0) // 8: Store 0 as null terminator

            // Call faccessat(AT_FDCWD, path, F_OK, 0)
            // X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = mode (F_OK = 0), X3 = flags (0)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)      // X0 = -100 (AT_FDCWD)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, 0us, 0)       // F_OK = 0
            ARM64.MOVZ (ARM64.X3, 0us, 0)       // flags = 0
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Access, 0)  // faccessat syscall
            ARM64.SVC syscalls.SvcImmediate

            // If X0 == 0, file exists; else doesn't
            ARM64.CMP_imm (ARM64.X0, 0us)
            ARM64.CSET (ARM64.X0, ARM64.EQ)

            // Restore caller-saved registers
            ARM64.MOV_reg (ARM64.X1, ARM64.X21)  // Restore X1
            ARM64.MOV_reg (ARM64.X2, ARM64.X22)  // Restore X2

            // Cleanup - restore callee-saved registers
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]

/// Generate ARM64 instructions to delete a file and return Result<Unit, String>
/// destReg: destination register for the Result pointer
/// pathReg: register containing heap string pointer to file path
///
/// Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
/// - tag 0 = Ok, tag 1 = Error
/// - payload = pointer to value (0 for Unit, string pointer for Error)
let generateFileDelete (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    match os with
    | Platform.MacOS ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes, 16-byte aligned)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg), X20 = dest pointer for later
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop: copies X2 bytes from X1 to X0
            // copy_loop (7 instructions, 0-6):
            ARM64.CBZ_offset (ARM64.X2, 7)      // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4) // 1: Load byte from src [X1 + 0]
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)  // 2: Store byte to dest
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us) // 3: dest++
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us) // 4: src++
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us) // 5: len--
            ARM64.B (-6)                        // 6: Loop back to CBZ

            // null_term: Store null terminator at X0
            ARM64.MOVZ (ARM64.X3, 0us, 0)       // X3 = 0
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)  // Store null terminator

            // Call unlink(path)
            // X0 = path (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Unlink, 0)  // unlink syscall
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)  // X0 = result pointer
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // bump allocator

            // Check if unlink succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)  // If failed, jump to error path

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)       // tag = 0 (Ok)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)  // payload = 0 (Unit)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1
            ARM64.B (26)  // Jump to cleanup (skip 26 error path instructions)

            // Error path: Result = Error("Error")
            // Allocate error string: "Error" (5 chars)
            // String format: [length:8][data:N][refcount:8]
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)  // X2 = error string pointer  (1)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // (2)
            ARM64.MOVZ (ARM64.X1, 5us, 0)       // length = 5  (3)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)  // Store length  (4)
            // Store "Error" byte by byte: E=69, r=114, r=114, o=111, r=114
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'  (5)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)  // (6)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (7)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (8)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)  // (9)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (10)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (11)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)  // (12)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (13)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'  (14)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)  // (15)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (16)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (17)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)  // (18)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (19)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (20)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s) // refcount = 1  (21)
            // Now store Result with error
            ARM64.MOVZ (ARM64.X1, 1us, 0)       // tag = 1 (Error)  (22)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag  (23)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)  // payload = error string pointer  (24)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (25)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1  (26)

            // Cleanup - restore registers and move result to dest
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)  // Deallocate path buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]
    | Platform.Linux ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg)
            ARM64.MOV_reg (ARM64.X19, pathReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X2, 7)      // 0: If length == 0, skip to null_term at inst 7
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // null_term: Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // Call unlinkat(AT_FDCWD, path, 0)
            // X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = flags (0)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)      // X0 = -100 (AT_FDCWD)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, 0us, 0)       // flags = 0
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Unlink, 0)  // unlinkat syscall
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)  // X0 = result pointer
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // bump allocator

            // Check if unlink succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)  // If failed, jump to error path

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)       // tag = 0 (Ok)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)  // payload = 0 (Unit)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1
            ARM64.B (26)  // Jump to cleanup (skip 26 error path instructions)

            // Error path: Result = Error("Error")
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)  // X2 = error string pointer  (1)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // (2)
            ARM64.MOVZ (ARM64.X1, 5us, 0)       // length = 5  (3)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)  // Store length  (4)
            // Store "Error" byte by byte: E=69, r=114, r=114, o=111, r=114
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'  (5)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)  // (6)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (7)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (8)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)  // (9)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (10)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (11)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)  // (12)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (13)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'  (14)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)  // (15)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (16)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'  (17)
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)  // (18)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)  // (19)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (20)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s) // refcount = 1  (21)
            ARM64.MOVZ (ARM64.X1, 1us, 0)       // tag = 1 (Error)  (22)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)  // Store tag  (23)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)  // payload = error string pointer  (24)
            ARM64.MOVZ (ARM64.X1, 1us, 0)  // (25)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s) // refcount = 1  (26)

            // Cleanup - restore registers and move result to dest
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)   // Move result to dest after restoration
        ]

/// Generate ARM64 instructions to set executable permission on a file
/// destReg: destination register for the Result pointer
/// pathReg: register containing heap string pointer to file path
///
/// Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
/// - tag 0 = Ok, tag 1 = Error
/// - payload = pointer to value (0 for Unit, string pointer for Error)
let generateFileSetExecutable (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    match os with
    | Platform.MacOS ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes, 16-byte aligned)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg), X20 = dest pointer for later
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop: copies X2 bytes from X1 to X0
            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // null_term: Store null terminator at X0
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // Call chmod(path, 0755)
            // X0 = path (SP), X1 = mode (0755 = 493 in decimal)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, 493us, 0)   // 0755 = 493
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Chmod, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Check if chmod succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)
            ARM64.B (26)

            // Error path: Result = Error("Error")
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X1, 5us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)

            // Cleanup
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]
    | Platform.Linux ->
        [
            // Save callee-saved registers we'll use
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)

            // Allocate stack space for path (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 256us)

            // X19 = heap string base (pathReg)
            ARM64.MOV_reg (ARM64.X19, pathReg)

            // X2 = string length from [X19]
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)

            // X0 = dest pointer (SP)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            // X1 = source pointer (X19 + 8)
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)
            // X4 = 0 (for LDRB offset)
            ARM64.MOVZ (ARM64.X4, 0us, 0)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // null_term: Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // Call fchmodat(AT_FDCWD, path, 0755, 0)
            // X0 = dirfd (AT_FDCWD = -100), X1 = path, X2 = mode (0755 = 493), X3 = flags (0)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)
            ARM64.MOVZ (ARM64.X2, 493us, 0)   // 0755 = 493
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Chmod, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Save syscall result in X21
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)

            // Allocate Result on heap
            ARM64.MOV_reg (ARM64.X0, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Check if chmod succeeded (X21 == 0)
            ARM64.CMP_imm (ARM64.X21, 0us)
            ARM64.B_cond (ARM64.NE, 6)

            // Success path: Result = Ok(())
            ARM64.MOVZ (ARM64.X1, 0us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X1, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)
            ARM64.B (26)

            // Error path: Result = Error("Error")
            ARM64.MOV_reg (ARM64.X2, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X1, 5us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 0s)
            ARM64.MOVZ (ARM64.X1, 69us, 0)      // 'E'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 8us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 9us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 10us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 111us, 0)     // 'o'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 11us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 114us, 0)     // 'r'
            ARM64.ADD_imm (ARM64.X3, ARM64.X2, 12us)
            ARM64.STRB_reg (ARM64.X1, ARM64.X3)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X2, 16s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 0s)
            ARM64.STR (ARM64.X2, ARM64.X0, 8s)
            ARM64.MOVZ (ARM64.X1, 1us, 0)
            ARM64.STR (ARM64.X1, ARM64.X0, 16s)

            // Cleanup
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 256us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 0s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]

/// Generate ARM64 instructions to read file contents and return Result<String, String>
/// destReg: destination register for the Result pointer
/// pathReg: register containing heap string pointer to file path
///
/// Result memory layout: [tag:8][payload:8][refcount:8] = 24 bytes
/// - tag 0 = Ok, tag 1 = Error
/// - payload = pointer to string
///
/// String memory layout: [length:8][data:N][refcount:8]
///
/// Algorithm:
/// 1. Copy path to stack with null terminator (same as FileExists)
/// 2. open() syscall - if fails, return Error("File not found")
/// 3. fstat() syscall - get file size
/// 4. Allocate string buffer on heap
/// 5. read() syscall - read file contents
/// 6. close() syscall
/// 7. Construct Result with Ok(string) or Error(message)
let generateFileReadText (destReg: ARM64.Reg) (pathReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    // For now, implement a simplified version that:
    // - Opens the file
    // - Reads up to 4096 bytes
    // - Returns Ok(contents) or Error("File not found")
    //
    // Stack layout: [saved X19-X25: 64][stat buffer: 144][path: 256][padding: 16] = 480 bytes
    // (stat buffer is 128 bytes on Linux, 144 on macOS - use 144 for safety)
    match os with
    | Platform.Linux ->
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.STP (ARM64.X25, ARM64.X26, ARM64.SP, -64s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 64us)

            // Allocate stack space for: stat buffer (144) + path (256) + caller-saved (64) = 464 bytes
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)  // Can only sub 255 at a time
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 209us)  // Total: 464 bytes

            // X19 = path heap string, X20 = dest register, X21 = file descriptor
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+400
            ARM64.STR (ARM64.X1, ARM64.SP, 400s)
            ARM64.STR (ARM64.X2, ARM64.SP, 408s)
            ARM64.STR (ARM64.X3, ARM64.SP, 416s)
            ARM64.STR (ARM64.X4, ARM64.SP, 424s)
            ARM64.STR (ARM64.X5, ARM64.SP, 432s)
            ARM64.STR (ARM64.X6, ARM64.SP, 440s)
            ARM64.STR (ARM64.X7, ARM64.SP, 448s)
            ARM64.STR (ARM64.X8, ARM64.SP, 456s)

            // Use non-allocatable registers for copy loop
            // X10 = string length from heap string
            ARM64.LDR (ARM64.X10, ARM64.X19, 0s)

            // X9 = stack dest for null-terminated path (SP + 144 for after stat buffer)
            ARM64.ADD_imm (ARM64.X9, ARM64.SP, 144us)
            // X11 = heap string data (X19 + 8)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 8us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)     // 0: If length == 0, skip to null_term
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us)
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us)
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)

            // openat(AT_FDCWD, path, O_RDONLY, 0)
            // X0 = dirfd (AT_FDCWD = -100)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)
            // X1 = path
            ARM64.ADD_imm (ARM64.X1, ARM64.SP, 144us)
            // X2 = flags (O_RDONLY = 0)
            ARM64.MOVZ (ARM64.X2, 0us, 0)
            // X3 = mode (not used for O_RDONLY)
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            // syscall
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed (fd < 0 means X0 has sign bit set)
            // X21 = fd
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 31)  // If negative, branch to error path (+31 instructions)

            // fstat(fd, statbuf) - X0 = fd, X1 = statbuf
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // stat buffer at SP
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Fstat, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Get file size from stat buffer (st_size is at offset 48 on Linux ARM64)
            ARM64.LDR (ARM64.X22, ARM64.SP, 48s)  // X22 = file size

            // Allocate heap space for string: [len:8][data:N][refcount:8]
            // Size = 8 + size + 8 = size + 16, round up to next 8 bytes
            // Simpler: just add 24 (16 + 8 for alignment padding)
            ARM64.ADD_imm (ARM64.X23, ARM64.X22, 24us)  // X23 = size + 24 (with padding)

            // Allocate from heap (bump allocator)
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)  // X24 = string pointer
            ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23)  // bump heap pointer

            // Store length at [X24]
            ARM64.STR (ARM64.X22, ARM64.X24, 0s)

            // read(fd, buf, count) - read file contents
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)  // fd
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)  // buf = string data area
            ARM64.MOV_reg (ARM64.X2, ARM64.X22)  // count = file size
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Read, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Store refcount = 1 at [X24 + 8 + size]
            ARM64.ADD_imm (ARM64.X25, ARM64.X24, 8us)
            ARM64.ADD_reg (ARM64.X25, ARM64.X25, ARM64.X22)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Result: [tag:8][payload:8][refcount:8] = 24 bytes
            ARM64.MOV_reg (ARM64.X25, ARM64.X28)  // X25 = Result pointer
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)  // bump heap

            // Store Ok tag (0)
            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)  // tag = 0

            // Store string pointer as payload
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)  // payload = string ptr

            // Store refcount = 1
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            // Move result to dest
            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            // Jump to cleanup
            ARM64.B 30  // Skip error path (29 instructions + 1 to land on cleanup)

            // === Error path (file not found) ===
            // Create error string "File not found" and Error result
            // For simplicity, create a short error message

            // Allocate error string: "Error" = 5 chars + len + refcount = 24 bytes
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)  // X24 = error string
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Store length = 5
            ARM64.MOVZ (ARM64.X0, 5us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            // Store "Error" (ASCII: 69, 114, 114, 111, 114)
            // E=69, r=114, r=114, o=111, r=114
            // Store at [X24+8] through [X24+12]
            ARM64.MOVZ (ARM64.X0, 69us, 0)  // 'E'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 111us, 0)  // 'o'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            // Store refcount = 1 at [X24 + 8 + 5] = [X24 + 13]
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)

            // Allocate Error Result
            ARM64.MOV_reg (ARM64.X25, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            // Store Error tag (1)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)

            // Store error string as payload
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)

            // Store refcount = 1
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            // Move result to dest
            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            // === Cleanup - save result to X0 before restoring callee-saved registers ===
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 400s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 408s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 416s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 424s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 432s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 440s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 448s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 456s)

            // Deallocate 464-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 209us)

            // Restore callee-saved registers
            ARM64.LDP (ARM64.X25, ARM64.X26, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 32s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 48s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
    | Platform.MacOS ->
        // macOS version - similar structure with different syscall numbers
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.STP (ARM64.X25, ARM64.X26, ARM64.SP, -64s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 64us)

            // Allocate stack: stat buffer (144) + path (256) + caller-saved (64) = 464 bytes
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 209us)

            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X20, destReg)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+400
            ARM64.STR (ARM64.X1, ARM64.SP, 400s)
            ARM64.STR (ARM64.X2, ARM64.SP, 408s)
            ARM64.STR (ARM64.X3, ARM64.SP, 416s)
            ARM64.STR (ARM64.X4, ARM64.SP, 424s)
            ARM64.STR (ARM64.X5, ARM64.SP, 432s)
            ARM64.STR (ARM64.X6, ARM64.SP, 440s)
            ARM64.STR (ARM64.X7, ARM64.SP, 448s)
            ARM64.STR (ARM64.X8, ARM64.SP, 456s)

            // Copy path with null terminator using non-allocatable registers
            // X10 = string length
            ARM64.LDR (ARM64.X10, ARM64.X19, 0s)
            // X9 = dest (SP + 144)
            ARM64.ADD_imm (ARM64.X9, ARM64.SP, 144us)
            // X11 = source (X19 + 8)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 8us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us)
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us)
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)

            // open(path, O_RDONLY)
            ARM64.ADD_imm (ARM64.X0, ARM64.SP, 144us)
            ARM64.MOVZ (ARM64.X1, 0us, 0)  // O_RDONLY
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 31)  // If negative, branch to error path

            // fstat(fd, statbuf)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Fstat, 0)
            ARM64.SVC syscalls.SvcImmediate

            // st_size at offset 96 on macOS
            ARM64.LDR (ARM64.X22, ARM64.SP, 96s)

            // Allocate string: size + 24 (with padding for alignment)
            ARM64.ADD_imm (ARM64.X23, ARM64.X22, 24us)

            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X23)

            ARM64.STR (ARM64.X22, ARM64.X24, 0s)

            // read
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.MOV_reg (ARM64.X2, ARM64.X22)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Read, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.ADD_imm (ARM64.X25, ARM64.X24, 8us)
            ARM64.ADD_reg (ARM64.X25, ARM64.X25, ARM64.X22)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)

            // close
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Result
            ARM64.MOV_reg (ARM64.X25, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            ARM64.B 30  // Skip error path (29 instructions + 1 to land on cleanup)

            // Error path (same as Linux)
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            ARM64.MOVZ (ARM64.X0, 69us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 111us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)

            ARM64.MOV_reg (ARM64.X25, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 0s)
            ARM64.STR (ARM64.X24, ARM64.X25, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X25, 16s)

            ARM64.MOV_reg (ARM64.X20, ARM64.X25)

            // Cleanup - save result to X0 before restoring callee-saved registers
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 400s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 408s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 416s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 424s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 432s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 440s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 448s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 456s)

            // Deallocate 464-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 209us)

            ARM64.LDP (ARM64.X25, ARM64.X26, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 32s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 48s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]

/// Generate ARM64 instructions to write content to a file
/// pathReg: register containing pointer to heap string (path)
/// contentReg: register containing pointer to heap string (content)
/// append: if true, append to file; if false, overwrite
/// Returns Result<Unit, String> in destReg
/// On success, result contains Ok(()) - tag=0, payload=0
/// On failure, result contains Error("Error") - tag=1, payload=error string ptr
let generateFileWriteText (destReg: ARM64.Reg) (pathReg: ARM64.Reg) (contentReg: ARM64.Reg) (append: bool) : ARM64.Instr list =
    // Get platform and syscalls
    let os =
        match Platform.detectOS () with
        | Ok os -> os
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    // Open flags:
    // Write: O_WRONLY | O_CREAT | O_TRUNC
    // Append: O_WRONLY | O_CREAT | O_APPEND
    // Linux: O_WRONLY=1, O_CREAT=64, O_TRUNC=512, O_APPEND=1024
    // macOS: O_WRONLY=1, O_CREAT=0x200, O_TRUNC=0x400, O_APPEND=8
    let (writeFlags, appendFlags) =
        match os with
        | Platform.Linux -> (577us, 1089us)  // 1|64|512, 1|64|1024
        | Platform.MacOS -> (1537us, 521us)  // 1|0x200|0x400, 1|0x200|8
    let flags = if append then appendFlags else writeFlags

    match os with
    | Platform.Linux ->
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

            // Allocate stack for path buffer (256) + caller-saved regs (64) = 320 bytes
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65us)

            // Save path and content pointers
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X22, contentReg)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+256
            ARM64.STR (ARM64.X1, ARM64.SP, 256s)
            ARM64.STR (ARM64.X2, ARM64.SP, 264s)
            ARM64.STR (ARM64.X3, ARM64.SP, 272s)
            ARM64.STR (ARM64.X4, ARM64.SP, 280s)
            ARM64.STR (ARM64.X5, ARM64.SP, 288s)
            ARM64.STR (ARM64.X6, ARM64.SP, 296s)
            ARM64.STR (ARM64.X7, ARM64.SP, 304s)
            ARM64.STR (ARM64.X8, ARM64.SP, 312s)

            // Copy path to stack buffer with null terminator using non-allocatable registers
            // X10 = path length
            ARM64.LDR (ARM64.X10, ARM64.X19, 0s)
            // X9 = dest buffer (SP)
            ARM64.MOV_reg (ARM64.X9, ARM64.SP)
            // X11 = source data (X19 + 8)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 8us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us)
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us)
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)

            // openat(AT_FDCWD, path, flags, mode)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)  // AT_FDCWD = -100
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, flags, 0)  // flags
            ARM64.MOVZ (ARM64.X3, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed
            ARM64.MOV_reg (ARM64.X21, ARM64.X0)  // X21 = fd
            ARM64.TBNZ (ARM64.X0, 63, 18)  // If negative, branch to error path (17 instructions + 1)

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)  // fd
            ARM64.ADD_imm (ARM64.X1, ARM64.X22, 8us)  // buf = content data
            ARM64.LDR (ARM64.X2, ARM64.X22, 0s)  // count = content length
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Ok Result: [tag=0][payload=0][refcount=1]
            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)  // tag = 0 (Ok)
            ARM64.STR (ARM64.X0, ARM64.X23, 8s)  // payload = 0 (Unit)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)  // refcount = 1
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)
            ARM64.B 30  // Jump to cleanup (skip 29 error path instructions + 1)

            // Error path: Create Error("Error") result
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)  // length = 5
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            // Store "Error" bytes
            ARM64.MOVZ (ARM64.X0, 69us, 0)  // 'E'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 111us, 0)  // 'o'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)  // 'r'
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)  // refcount

            // Allocate Error Result
            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)  // tag = 1 (Error)
            ARM64.STR (ARM64.X24, ARM64.X23, 8s)  // payload = error string
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)  // refcount
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)

            // Cleanup - save result to X0 before restoring callee-saved registers
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 256s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 264s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 272s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 280s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 288s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 296s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 304s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 312s)

            // Deallocate 320-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65us)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]
    | Platform.MacOS ->
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.STP (ARM64.X23, ARM64.X24, ARM64.SP, -48s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)

            // Allocate stack for path buffer (256) + caller-saved regs (64) = 320 bytes
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 65us)

            // Save path and content pointers
            ARM64.MOV_reg (ARM64.X19, pathReg)
            ARM64.MOV_reg (ARM64.X22, contentReg)

            // Save ALL potentially live caller-saved registers (X1-X8) at SP+256
            ARM64.STR (ARM64.X1, ARM64.SP, 256s)
            ARM64.STR (ARM64.X2, ARM64.SP, 264s)
            ARM64.STR (ARM64.X3, ARM64.SP, 272s)
            ARM64.STR (ARM64.X4, ARM64.SP, 280s)
            ARM64.STR (ARM64.X5, ARM64.SP, 288s)
            ARM64.STR (ARM64.X6, ARM64.SP, 296s)
            ARM64.STR (ARM64.X7, ARM64.SP, 304s)
            ARM64.STR (ARM64.X8, ARM64.SP, 312s)

            // Copy path to stack buffer using non-allocatable registers
            // X10 = path length
            ARM64.LDR (ARM64.X10, ARM64.X19, 0s)
            // X9 = dest buffer (SP)
            ARM64.MOV_reg (ARM64.X9, ARM64.SP)
            // X11 = source data (X19 + 8)
            ARM64.ADD_imm (ARM64.X11, ARM64.X19, 8us)

            // Copy loop (7 instructions, 0-6)
            ARM64.CBZ_offset (ARM64.X10, 7)
            ARM64.LDRB_imm (ARM64.X12, ARM64.X11, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)
            ARM64.ADD_imm (ARM64.X9, ARM64.X9, 1us)
            ARM64.ADD_imm (ARM64.X11, ARM64.X11, 1us)
            ARM64.SUB_imm (ARM64.X10, ARM64.X10, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X12, 0us, 0)
            ARM64.STRB (ARM64.X12, ARM64.X9, 0)

            // open(path, flags, mode)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, flags, 0)
            ARM64.MOVZ (ARM64.X2, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            ARM64.MOV_reg (ARM64.X21, ARM64.X0)
            ARM64.TBNZ (ARM64.X0, 63, 18)  // If negative, branch to error path (17 instructions + 1)

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.ADD_imm (ARM64.X1, ARM64.X22, 8us)
            ARM64.LDR (ARM64.X2, ARM64.X22, 0s)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X21)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Allocate Ok Result
            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 0us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)
            ARM64.STR (ARM64.X0, ARM64.X23, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)
            ARM64.B 30  // Jump to cleanup (skip 29 error path instructions + 1)

            // Error path
            ARM64.MOV_reg (ARM64.X24, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)

            ARM64.MOVZ (ARM64.X0, 5us, 0)
            ARM64.STR (ARM64.X0, ARM64.X24, 0s)

            ARM64.MOVZ (ARM64.X0, 69us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 8us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 9us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 10us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 111us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 11us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)
            ARM64.MOVZ (ARM64.X0, 114us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 12us)
            ARM64.STRB_reg (ARM64.X0, ARM64.X1)

            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.ADD_imm (ARM64.X1, ARM64.X24, 13us)
            ARM64.STR (ARM64.X0, ARM64.X1, 0s)

            ARM64.MOV_reg (ARM64.X23, ARM64.X28)
            ARM64.ADD_imm (ARM64.X28, ARM64.X28, 24us)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 0s)
            ARM64.STR (ARM64.X24, ARM64.X23, 8s)
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.STR (ARM64.X0, ARM64.X23, 16s)
            ARM64.MOV_reg (ARM64.X20, ARM64.X23)

            // Cleanup - save result to X0 before restoring callee-saved registers
            ARM64.MOV_reg (ARM64.X0, ARM64.X20)

            // Restore ALL caller-saved registers (X1-X8) we saved at start
            ARM64.LDR (ARM64.X1, ARM64.SP, 256s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 264s)
            ARM64.LDR (ARM64.X3, ARM64.SP, 272s)
            ARM64.LDR (ARM64.X4, ARM64.SP, 280s)
            ARM64.LDR (ARM64.X5, ARM64.SP, 288s)
            ARM64.LDR (ARM64.X6, ARM64.SP, 296s)
            ARM64.LDR (ARM64.X7, ARM64.SP, 304s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 312s)

            // Deallocate 320-byte stack buffer
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 65us)
            ARM64.LDP (ARM64.X23, ARM64.X24, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 32s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.MOV_reg (destReg, ARM64.X0)  // Move result to dest after restoration
        ]

/// Generate ARM64 instructions to get 8 random bytes as Int64
/// destReg: destination register for the random Int64
/// Uses getrandom (Linux) or getentropy (macOS) syscall
/// Note: This function saves/restores caller-saved registers X1, X2, X8
/// that may contain live values, since the syscall clobbers them.
let generateRandomInt64 (destReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    match os with
    | Platform.MacOS ->
        [
            // Save X1 (caller-saved, may contain live value)
            // Allocate 32 bytes: 8 for X1, 8 for buffer, 16 for alignment
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)
            ARM64.STR (ARM64.X1, ARM64.SP, 24s)  // Save X1 at SP+24

            // Call getentropy(buffer, 8)
            // X0 = buffer pointer (SP), X1 = length (8)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, 8us, 0)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Getrandom, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Load 8 bytes from buffer into X0
            ARM64.LDR (ARM64.X0, ARM64.SP, 0s)

            // Restore X1
            ARM64.LDR (ARM64.X1, ARM64.SP, 24s)

            // Cleanup stack
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)

            // Move result to destination
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]
    | Platform.Linux ->
        [
            // Save X1, X2, X8 (caller-saved, may contain live values)
            // Allocate 48 bytes: 8 for buffer, 8 each for X1/X2/X8 = 32, 16 for alignment
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.STR (ARM64.X1, ARM64.SP, 40s)  // Save X1 at SP+40
            ARM64.STR (ARM64.X2, ARM64.SP, 32s)  // Save X2 at SP+32
            ARM64.STR (ARM64.X8, ARM64.SP, 24s)  // Save X8 at SP+24

            // Call getrandom(buffer, 8, flags=0)
            // X0 = buffer pointer (SP), X1 = length (8), X2 = flags (0)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, 8us, 0)
            ARM64.MOVZ (ARM64.X2, 0us, 0)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Getrandom, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Load 8 bytes from buffer into X0
            ARM64.LDR (ARM64.X0, ARM64.SP, 0s)

            // Restore X1, X2, X8
            ARM64.LDR (ARM64.X1, ARM64.SP, 40s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 32s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 24s)

            // Cleanup stack
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)

            // Move result to destination
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]

/// Generate ARM64 instructions to get current Unix epoch seconds as Int64
/// destReg: destination register for the timestamp
/// Uses gettimeofday (macOS) or clock_gettime (Linux) syscall
/// Note: This function saves/restores caller-saved registers that may contain live values.
let generateDateNow (destReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    match os with
    | Platform.MacOS ->
        [
            // Save X1 (caller-saved, may contain live value)
            // Allocate 32 bytes: 8 for X1, 16 for timeval struct (tv_sec, tv_usec), 8 for alignment
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)
            ARM64.STR (ARM64.X1, ARM64.SP, 24s)  // Save X1 at SP+24

            // Call gettimeofday(tv, NULL)
            // X0 = timeval pointer (SP), X1 = timezone (NULL)
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)
            ARM64.MOVZ (ARM64.X1, 0us, 0)  // NULL timezone
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Gettimeofday, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Load tv_sec (first 8 bytes of timeval) into X0
            ARM64.LDR (ARM64.X0, ARM64.SP, 0s)

            // Restore X1
            ARM64.LDR (ARM64.X1, ARM64.SP, 24s)

            // Cleanup stack
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)

            // Move result to destination
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]
    | Platform.Linux ->
        [
            // Save X1, X2, X8 (caller-saved, may contain live values)
            // Allocate 48 bytes: 16 for timespec struct (tv_sec, tv_nsec), 8 each for X1/X2/X8 = 24, 8 for alignment
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 48us)
            ARM64.STR (ARM64.X1, ARM64.SP, 40s)  // Save X1 at SP+40
            ARM64.STR (ARM64.X2, ARM64.SP, 32s)  // Save X2 at SP+32
            ARM64.STR (ARM64.X8, ARM64.SP, 24s)  // Save X8 at SP+24

            // Call clock_gettime(CLOCK_REALTIME, ts)
            // X0 = clock_id (0 = CLOCK_REALTIME), X1 = timespec pointer (SP)
            ARM64.MOVZ (ARM64.X0, 0us, 0)  // CLOCK_REALTIME = 0
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Gettimeofday, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Load tv_sec (first 8 bytes of timespec) into X0
            ARM64.LDR (ARM64.X0, ARM64.SP, 0s)

            // Restore X1, X2, X8
            ARM64.LDR (ARM64.X1, ARM64.SP, 40s)
            ARM64.LDR (ARM64.X2, ARM64.SP, 32s)
            ARM64.LDR (ARM64.X8, ARM64.SP, 24s)

            // Cleanup stack
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 48us)

            // Move result to destination
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]

/// Generate code for FileWriteFromPtr: write raw bytes to a file
/// pathReg: register containing heap string pointer to file path
/// ptrReg: register containing raw pointer to bytes
/// lengthReg: register containing length in bytes
/// destReg: destination register (result = 1 on success, 0 on failure)
let generateFileWriteFromPtr (destReg: ARM64.Reg) (pathReg: ARM64.Reg) (ptrReg: ARM64.Reg) (lengthReg: ARM64.Reg) : ARM64.Instr list =
    let os =
        match Platform.detectOS () with
        | Ok platform -> platform
        | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
    let syscalls = ARM64.syscallConfigFor os

    // O_WRONLY | O_CREAT | O_TRUNC
    let writeFlags =
        match os with
        | Platform.Linux -> 577us  // 1|64|512
        | Platform.MacOS -> 1537us  // 1|0x200|0x400

    match os with
    | Platform.Linux ->
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

            // Allocate stack for path buffer (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 1us)

            // Save registers
            ARM64.MOV_reg (ARM64.X19, pathReg)     // X19 = path heap string
            ARM64.MOV_reg (ARM64.X20, ptrReg)     // X20 = data pointer
            ARM64.MOV_reg (ARM64.X21, lengthReg)  // X21 = length

            // Copy path to stack buffer with null terminator
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)  // X2 = path length
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)  // X0 = dest buffer
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)  // X1 = source data
            ARM64.MOVZ (ARM64.X4, 0us, 0)  // X4 = index

            // Copy loop
            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // openat(AT_FDCWD, path, flags, mode)
            ARM64.MOVZ (ARM64.X0, 100us, 0)
            ARM64.NEG (ARM64.X0, ARM64.X0)  // AT_FDCWD = -100
            ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X2, writeFlags, 0)  // flags
            ARM64.MOVZ (ARM64.X3, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed
            ARM64.MOV_reg (ARM64.X22, ARM64.X0)  // X22 = fd
            ARM64.TBNZ (ARM64.X0, 63, 11)  // If negative, branch to error path

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X22)  // fd
            ARM64.MOV_reg (ARM64.X1, ARM64.X20)  // buf = data pointer
            ARM64.MOV_reg (ARM64.X2, ARM64.X21)  // count = length
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X22)
            ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Success: result = 1
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.B 2  // Skip error path

            // Error path: result = 0
            ARM64.MOVZ (ARM64.X0, 0us, 0)

            // Cleanup
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 1us)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]

    | Platform.MacOS ->
        [
            // Save callee-saved registers
            ARM64.STP (ARM64.X19, ARM64.X20, ARM64.SP, -16s)
            ARM64.STP (ARM64.X21, ARM64.X22, ARM64.SP, -32s)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

            // Allocate stack for path buffer (256 bytes)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 1us)

            // Save registers
            ARM64.MOV_reg (ARM64.X19, pathReg)     // X19 = path heap string
            ARM64.MOV_reg (ARM64.X20, ptrReg)     // X20 = data pointer
            ARM64.MOV_reg (ARM64.X21, lengthReg)  // X21 = length

            // Copy path to stack buffer with null terminator
            ARM64.LDR (ARM64.X2, ARM64.X19, 0s)  // X2 = path length
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)  // X0 = dest buffer
            ARM64.ADD_imm (ARM64.X1, ARM64.X19, 8us)  // X1 = source data
            ARM64.MOVZ (ARM64.X4, 0us, 0)  // X4 = index

            // Copy loop
            ARM64.CBZ_offset (ARM64.X2, 7)
            ARM64.LDRB (ARM64.X3, ARM64.X1, ARM64.X4)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)
            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
            ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
            ARM64.B (-6)

            // Store null terminator
            ARM64.MOVZ (ARM64.X3, 0us, 0)
            ARM64.STRB (ARM64.X3, ARM64.X0, 0)

            // open(path, flags, mode) - macOS uses open, not openat
            ARM64.MOV_reg (ARM64.X0, ARM64.SP)  // path
            ARM64.MOVZ (ARM64.X1, writeFlags, 0)  // flags
            ARM64.MOVZ (ARM64.X2, 420us, 0)  // mode 0644
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Open, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Check if open failed
            ARM64.MOV_reg (ARM64.X22, ARM64.X0)  // X22 = fd
            ARM64.TBNZ (ARM64.X0, 63, 10)  // If negative, branch to error path

            // write(fd, buf, count)
            ARM64.MOV_reg (ARM64.X0, ARM64.X22)  // fd
            ARM64.MOV_reg (ARM64.X1, ARM64.X20)  // buf = data pointer
            ARM64.MOV_reg (ARM64.X2, ARM64.X21)  // count = length
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Write, 0)
            ARM64.SVC syscalls.SvcImmediate

            // close(fd)
            ARM64.MOV_reg (ARM64.X0, ARM64.X22)
            ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Close, 0)
            ARM64.SVC syscalls.SvcImmediate

            // Success: result = 1
            ARM64.MOVZ (ARM64.X0, 1us, 0)
            ARM64.B 2  // Skip error path

            // Error path: result = 0
            ARM64.MOVZ (ARM64.X0, 0us, 0)

            // Cleanup
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 255us)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 1us)
            ARM64.LDP (ARM64.X21, ARM64.X22, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X19, ARM64.X20, ARM64.SP, 16s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)
            ARM64.MOV_reg (destReg, ARM64.X0)
        ]

/// Generate ARM64 instructions to flush coverage data to file
/// Writes coverage counters to /tmp/dark_cov.bin before program exit
/// coverageExprCount: number of expressions (determines bytes to write = count * 8)
///
/// Uses ADRP+ADD to get _coverage_data address from BSS section
/// Opens file, writes data, closes file (errors are silently ignored)
let generateCoverageFlush (coverageExprCount: int) : ARM64.Instr list =
    if coverageExprCount = 0 then
        []
    else
        let os =
            match Platform.detectOS () with
            | Ok platform -> platform
            | Error err -> Crash.crash $"Runtime: Platform detection failed: {err}"
        let syscalls = ARM64.syscallConfigFor os

        // O_WRONLY | O_CREAT | O_TRUNC
        let writeFlags =
            match os with
            | Platform.Linux -> 577us  // 1|64|512
            | Platform.MacOS -> 1537us  // 1|0x200|0x400

        // Path: "/tmp/dark_cov.bin" = 18 bytes + null = 19 bytes, round to 24 for alignment
        let byteCount = coverageExprCount * 8

        match os with
        | Platform.Linux ->
            [
                // Allocate stack for path (24 bytes, 8-byte aligned)
                ARM64.SUB_imm (ARM64.SP, ARM64.SP, 24us)

                // Write "/tmp/dark_cov.bin\0" to stack
                // First 8 bytes: "/tmp/dar" = 0x7261642F706D742F
                ARM64.MOVZ (ARM64.X9, 0x2F74us, 0)   // "/t"
                ARM64.MOVK (ARM64.X9, 0x6D70us, 16)  // "mp"
                ARM64.MOVK (ARM64.X9, 0x642Fus, 32)  // "/d"
                ARM64.MOVK (ARM64.X9, 0x7261us, 48)  // "ar"
                ARM64.STR (ARM64.X9, ARM64.SP, 0s)

                // Next 8 bytes: "k_cov.bi" = 0x69622E766F635F6B
                ARM64.MOVZ (ARM64.X9, 0x5F6Bus, 0)   // "k_"
                ARM64.MOVK (ARM64.X9, 0x6F63us, 16)  // "co"
                ARM64.MOVK (ARM64.X9, 0x2E76us, 32)  // "v."
                ARM64.MOVK (ARM64.X9, 0x6962us, 48)  // "bi"
                ARM64.STR (ARM64.X9, ARM64.SP, 8s)

                // Last 4 bytes: "n\0\0\0" = 0x0000006E
                ARM64.MOVZ (ARM64.X9, 0x006Eus, 0)   // "n\0"
                ARM64.STR (ARM64.X9, ARM64.SP, 16s)

                // Get coverage data address via ADRP+ADD
                ARM64.ADRP (ARM64.X10, "_coverage_data")
                ARM64.ADD_label (ARM64.X10, ARM64.X10, "_coverage_data")

                // openat(AT_FDCWD, path, flags, mode)
                ARM64.MOVZ (ARM64.X0, 100us, 0)
                ARM64.NEG (ARM64.X0, ARM64.X0)  // AT_FDCWD = -100
                ARM64.MOV_reg (ARM64.X1, ARM64.SP)  // path
                ARM64.MOVZ (ARM64.X2, writeFlags, 0)  // flags
                ARM64.MOVZ (ARM64.X3, 420us, 0)  // mode 0644
                ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Open, 0)
                ARM64.SVC syscalls.SvcImmediate

                // Check if open failed (X0 < 0)
                ARM64.TBNZ (ARM64.X0, 63, 6)  // If negative, skip to cleanup

                // Save fd
                ARM64.MOV_reg (ARM64.X11, ARM64.X0)

                // write(fd, buf, count)
                ARM64.MOV_reg (ARM64.X0, ARM64.X11)  // fd
                ARM64.MOV_reg (ARM64.X1, ARM64.X10)  // buf = _coverage_data
            ] @
            (if byteCount < 65536 then
                [ARM64.MOVZ (ARM64.X2, uint16 byteCount, 0)]
             else
                // Large count - load in two parts
                [ARM64.MOVZ (ARM64.X2, uint16 (byteCount &&& 0xFFFF), 0)
                 ARM64.MOVK (ARM64.X2, uint16 ((byteCount >>> 16) &&& 0xFFFF), 16)]) @
            [
                ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Write, 0)
                ARM64.SVC syscalls.SvcImmediate

                // close(fd)
                ARM64.MOV_reg (ARM64.X0, ARM64.X11)
                ARM64.MOVZ (ARM64.X8, syscalls.Numbers.Close, 0)
                ARM64.SVC syscalls.SvcImmediate

                // Cleanup stack
                ARM64.ADD_imm (ARM64.SP, ARM64.SP, 24us)
            ]

        | Platform.MacOS ->
            [
                // Allocate stack for path (24 bytes, 8-byte aligned)
                ARM64.SUB_imm (ARM64.SP, ARM64.SP, 24us)

                // Write "/tmp/dark_cov.bin\0" to stack (same as Linux)
                ARM64.MOVZ (ARM64.X9, 0x2F74us, 0)
                ARM64.MOVK (ARM64.X9, 0x6D70us, 16)
                ARM64.MOVK (ARM64.X9, 0x642Fus, 32)
                ARM64.MOVK (ARM64.X9, 0x7261us, 48)
                ARM64.STR (ARM64.X9, ARM64.SP, 0s)

                ARM64.MOVZ (ARM64.X9, 0x5F6Bus, 0)
                ARM64.MOVK (ARM64.X9, 0x6F63us, 16)
                ARM64.MOVK (ARM64.X9, 0x2E76us, 32)
                ARM64.MOVK (ARM64.X9, 0x6962us, 48)
                ARM64.STR (ARM64.X9, ARM64.SP, 8s)

                ARM64.MOVZ (ARM64.X9, 0x006Eus, 0)
                ARM64.STR (ARM64.X9, ARM64.SP, 16s)

                // Get coverage data address via ADRP+ADD
                ARM64.ADRP (ARM64.X10, "_coverage_data")
                ARM64.ADD_label (ARM64.X10, ARM64.X10, "_coverage_data")

                // open(path, flags, mode) - macOS uses direct open syscall
                ARM64.MOV_reg (ARM64.X0, ARM64.SP)  // path
                ARM64.MOVZ (ARM64.X1, writeFlags, 0)  // flags
                ARM64.MOVZ (ARM64.X2, 420us, 0)  // mode 0644
                ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Open, 0)
                ARM64.SVC syscalls.SvcImmediate

                // Check if open failed (X0 < 0)
                ARM64.TBNZ (ARM64.X0, 63, 6)  // If negative, skip to cleanup

                // Save fd
                ARM64.MOV_reg (ARM64.X11, ARM64.X0)

                // write(fd, buf, count)
                ARM64.MOV_reg (ARM64.X0, ARM64.X11)  // fd
                ARM64.MOV_reg (ARM64.X1, ARM64.X10)  // buf = _coverage_data
            ] @
            (if byteCount < 65536 then
                [ARM64.MOVZ (ARM64.X2, uint16 byteCount, 0)]
             else
                [ARM64.MOVZ (ARM64.X2, uint16 (byteCount &&& 0xFFFF), 0)
                 ARM64.MOVK (ARM64.X2, uint16 ((byteCount >>> 16) &&& 0xFFFF), 16)]) @
            [
                ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Write, 0)
                ARM64.SVC syscalls.SvcImmediate

                // close(fd)
                ARM64.MOV_reg (ARM64.X0, ARM64.X11)
                ARM64.MOVZ (ARM64.X16, syscalls.Numbers.Close, 0)
                ARM64.SVC syscalls.SvcImmediate

                // Cleanup stack
                ARM64.ADD_imm (ARM64.SP, ARM64.SP, 24us)
            ]

/// Generate ARM64 instructions to convert a float to a heap string
/// destReg: destination register for the heap string pointer
/// valueReg: FP register containing the float value
/// The string format is "-123.45" (integer part + "." + 1-2 decimal digits)
/// Heap string layout: [8 bytes length][N bytes data][8 bytes refcount]
let generateFloatToString (destReg: ARM64.Reg) (valueReg: ARM64.FReg) : ARM64.Instr list =
    // This function:
    // 1. Converts float to characters in a buffer on stack
    // 2. Allocates heap string using bump allocator (X28)
    // 3. Copies characters to heap and sets length
    // 4. Returns heap pointer in destReg
    //
    // Uses: X0-X7 (scratch), D0-D1 (float), X19 (heap ptr), X20 (sign flag), X21 (saved length)
    // Stack layout (64 bytes):
    //   [SP+0..31]: character buffer (32 bytes)
    //   [SP+32..39]: saved D0
    //   [SP+40..47]: saved X19
    //   [SP+48..55]: saved X20
    //   [SP+56..63]: saved X21
    [
        // Save callee-saved registers and allocate stack
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 64us)
        ARM64.STR (ARM64.X19, ARM64.SP, 40s)
        ARM64.STR (ARM64.X20, ARM64.SP, 48s)
        ARM64.STR (ARM64.X21, ARM64.SP, 56s)

        // Move input to D0
        ARM64.FMOV_reg (ARM64.D0, valueReg)
        ARM64.STR_fp (ARM64.D0, ARM64.SP, 32s)  // Save original value

        // Check if negative using sign bit
        ARM64.MOVZ (ARM64.X20, 0us, 0)  // X20 = 0 (assume positive)
        ARM64.FMOV_to_gp (ARM64.X0, ARM64.D0)  // Get bit pattern
        ARM64.TBNZ (ARM64.X0, 63, 2)  // If sign bit set, X20 = 1
        ARM64.B (2)  // Skip setting X20
        ARM64.MOVZ (ARM64.X20, 1us, 0)  // X20 = 1 (negative)

        // X1 = buffer pointer (start at end of buffer, work backwards)
        ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)  // X1 = SP + 31 (end of buffer)

        // Extract integer part
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)  // X0 = integer part (signed)
        // Make X2 = absolute value of X0
        ARM64.TBNZ (ARM64.X0, 63, 3)  // If negative, skip to NEG
        ARM64.MOV_reg (ARM64.X2, ARM64.X0)  // X2 = X0 (positive)
        ARM64.B (2)  // Skip NEG
        ARM64.NEG (ARM64.X2, ARM64.X0)  // X2 = -X0 (make positive)

        // === Build fractional part first (1-2 digits) ===
        ARM64.LDR_fp (ARM64.D0, ARM64.SP, 32s)
        ARM64.FCVTZS (ARM64.X0, ARM64.D0)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)
        ARM64.FSUB (ARM64.D0, ARM64.D0, ARM64.D1)  // D0 = fractional part

        // Multiply by 100 to get 2 digits
        ARM64.MOVZ (ARM64.X0, 100us, 0)
        ARM64.SCVTF (ARM64.D1, ARM64.X0)
        ARM64.FMUL (ARM64.D0, ARM64.D0, ARM64.D1)
        ARM64.FCVTZS (ARM64.X7, ARM64.D0)  // X7 = fractional digits (may be negative)

        // Take absolute value of X7
        ARM64.TBNZ (ARM64.X7, 63, 2)
        ARM64.B (2)
        ARM64.NEG (ARM64.X7, ARM64.X7)

        // Extract both fractional digits
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X7, ARM64.X3)  // X4 = tens digit
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X7)  // X5 = ones digit

        // Store fractional digits (backwards): ones first, then tens
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // ASCII
        ARM64.MOV_reg (ARM64.X7, ARM64.X5)  // Keep ones digit for length trim
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X4, ARM64.X4, 48us)  // ASCII
        ARM64.STRB (ARM64.X4, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // Store decimal point
        ARM64.MOVZ (ARM64.X3, 46us, 0)  // '.'
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // === Build integer part ===
        // X2 still has absolute value of integer part
        ARM64.CBZ_offset (ARM64.X2, 11)  // If zero, print just "0"

        // Convert integer digits (loop)
        ARM64.MOVZ (ARM64.X3, 10us, 0)
        ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)
        ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)
        ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)
        ARM64.STRB (ARM64.X5, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.MOV_reg (ARM64.X2, ARM64.X4)
        ARM64.CBZ_offset (ARM64.X2, 2)
        ARM64.B (-8)
        ARM64.B (4)  // Skip zero case

        // Zero case: just store '0'
        ARM64.MOVZ (ARM64.X3, 48us, 0)  // '0'
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // Add minus sign if negative
        ARM64.CBZ_offset (ARM64.X20, 4)
        ARM64.MOVZ (ARM64.X3, 45us, 0)  // '-'
        ARM64.STRB (ARM64.X3, ARM64.X1, 0)
        ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)

        // X1 now points one before first char
        // Adjust X1 to point to first char, calculate length (trim trailing zero)
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)  // X1 = start of string
        ARM64.ADD_imm (ARM64.X3, ARM64.SP, 32us)  // X3 = end+1 of buffer (SP+32)
        ARM64.SUBS_imm (ARM64.X7, ARM64.X7, 48us)  // Set flags if ones digit was '0'
        ARM64.CSET (ARM64.X4, ARM64.EQ)  // X4 = 1 when ones digit == 0
        ARM64.SUB_reg (ARM64.X3, ARM64.X3, ARM64.X4)  // Drop ones digit if needed
        ARM64.SUB_reg (ARM64.X21, ARM64.X3, ARM64.X1)  // X21 = length (save for later)

        // Allocate heap string using bump allocator (X28)
        // Total size = 8 (length) + string length + 8 (refcount), aligned to 8
        ARM64.ADD_imm (ARM64.X0, ARM64.X21, 23us)  // X0 = len + 16 + 7 (for alignment)
        ARM64.AND_imm (ARM64.X0, ARM64.X0, 0xFFFFFFFFFFFFFFF8UL)  // Round down to 8-byte alignment
        ARM64.MOV_reg (ARM64.X19, ARM64.X28)  // X19 = current heap pointer (our allocation)
        ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X0)  // Bump allocator

        // Store length at [X19]
        ARM64.STR (ARM64.X21, ARM64.X19, 0s)

        // Initialize refcount to 1 at [X19 + 8 + length]
        ARM64.ADD_reg (ARM64.X3, ARM64.X19, ARM64.X21)  // X3 = X19 + length
        ARM64.MOVZ (ARM64.X4, 1us, 0)  // X4 = 1
        ARM64.STR (ARM64.X4, ARM64.X3, 8s)  // Store refcount at [X19 + 8 + length]

        // Copy string from stack buffer to heap
        // X1 = source (stack buffer start), X19+8 = dest, X21 = length
        ARM64.ADD_imm (ARM64.X3, ARM64.X19, 8us)  // X3 = dest
        ARM64.MOV_reg (ARM64.X2, ARM64.X21)  // X2 = length (counter)

        // Copy loop (byte by byte)
        ARM64.CBZ_offset (ARM64.X2, 6)  // If length 0, skip
        ARM64.LDRB_imm (ARM64.X4, ARM64.X1, 0)
        ARM64.STRB (ARM64.X4, ARM64.X3, 0)
        ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)
        ARM64.ADD_imm (ARM64.X3, ARM64.X3, 1us)
        ARM64.SUB_imm (ARM64.X2, ARM64.X2, 1us)
        ARM64.CBNZ_offset (ARM64.X2, -5)

        // Move result to dest register
        ARM64.MOV_reg (destReg, ARM64.X19)

        // Restore callee-saved registers
        ARM64.LDR (ARM64.X19, ARM64.SP, 40s)
        ARM64.LDR (ARM64.X20, ARM64.SP, 48s)
        ARM64.LDR (ARM64.X21, ARM64.SP, 56s)
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 64us)
    ]
