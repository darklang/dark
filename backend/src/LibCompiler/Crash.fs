// Crash.fs - Dependency-free crash helper
//
// Provides a single crash function for internal invariant violations.

module Crash

/// Crash the program with an error message.
/// Used for internal invariant violations (unreachable code).
///
/// When the compiler is migrated to Darklang (self-hosting), this will
/// be replaced with Darklang's error handling (Result types or similar).
let crash (message: string) : 'a =
    failwith message

/// Crash to mark incomplete work that should never be hit in production.
/// Use this when a developer or AI needs to flag missing logic.
let TODO (message: string) : 'a =
    crash $"TODO: {message}"
