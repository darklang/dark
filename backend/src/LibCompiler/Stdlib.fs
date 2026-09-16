// Stdlib.fs - Standard Library Module Definitions
//
// Defines intrinsic Stdlib module signatures used directly by the compiler.
// Non-intrinsic stdlib functions are loaded from stdlib/*.dark.

module Stdlib

open AST

/// Intrinsic Stdlib.Int64 functions
let int64IntrinsicModule : ModuleDef = {
    Name = "Stdlib.Int64"
    Functions = [
        // toFloat : (Int64) -> Float
        { Name = "toFloat"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TFloat64 }
    ]
}

/// Intrinsic Stdlib.Float functions
let floatIntrinsicModule : ModuleDef = {
    Name = "Stdlib.Float"
    Functions = [
        // sqrt : (Float) -> Float
        { Name = "sqrt"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        // abs : (Float) -> Float
        { Name = "abs"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        // negate : (Float) -> Float
        { Name = "negate"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TFloat64 }
        // toInt : (Float) -> Int64
        { Name = "toInt"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TInt64 }
        // toBits : (Float) -> UInt64
        { Name = "toBits"; TypeParams = []; ParamTypes = [TFloat64]; ReturnType = TUInt64 }
    ]
}

/// Helper to create Result<T, String> type
let resultType (okType: Type) : Type =
    TSum ("Stdlib.Result.Result", [okType; TString])

/// Stdlib.File module - file I/O operations (intrinsics)
/// These are special-cased in the compiler and generate syscalls
let fileModule : ModuleDef = {
    Name = "Stdlib.File"
    Functions = [
        // readText : (String) -> Result<String, String>
        { Name = "readText"; TypeParams = []; ParamTypes = [TString]; ReturnType = resultType TString }
        // exists : (String) -> Bool
        { Name = "exists"; TypeParams = []; ParamTypes = [TString]; ReturnType = TBool }
        // writeText : (String, String) -> Result<Unit, String>
        { Name = "writeText"; TypeParams = []; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
        // appendText : (String, String) -> Result<Unit, String>
        { Name = "appendText"; TypeParams = []; ParamTypes = [TString; TString]; ReturnType = resultType TUnit }
        // delete : (String) -> Result<Unit, String>
        { Name = "delete"; TypeParams = []; ParamTypes = [TString]; ReturnType = resultType TUnit }
        // setExecutable : (String) -> Result<Unit, String>
        { Name = "setExecutable"; TypeParams = []; ParamTypes = [TString]; ReturnType = resultType TUnit }
        // writeFromPtr : (String, RawPtr, Int64) -> Bool - write raw bytes to file
        { Name = "writeFromPtr"; TypeParams = []; ParamTypes = [TString; TRawPtr; TInt64]; ReturnType = TBool }
    ]
}

/// Stdlib.Path module - path operations
/// combine is defined in stdlib/Path.dark, tempDir is constant-folded at compile time
let pathModule : ModuleDef = {
    Name = "Stdlib.Path"
    Functions = [
        // tempDir : () -> String - returns system temp directory
        { Name = "tempDir"; TypeParams = []; ParamTypes = []; ReturnType = TString }
        // combine is defined in stdlib/Path.dark
    ]
}

/// Stdlib.Platform module - platform detection
/// These are constant-folded at compile time based on target platform
let platformModule : ModuleDef = {
    Name = "Stdlib.Platform"
    Functions = [
        // isMacOS : () -> Bool
        { Name = "isMacOS"; TypeParams = []; ParamTypes = []; ReturnType = TBool }
        // isLinux : () -> Bool
        { Name = "isLinux"; TypeParams = []; ParamTypes = []; ReturnType = TBool }
    ]
}

/// Stdlib.Random module - random number generation (intrinsics)
/// These are special-cased in the compiler and generate syscalls
let randomModule : ModuleDef = {
    Name = "Stdlib.Random"
    Functions = [
        // int64 : () -> Int64 - returns 8 random bytes as Int64
        { Name = "int64"; TypeParams = []; ParamTypes = []; ReturnType = TInt64 }
    ]
}

/// Stdlib.Date module - date/time operations (intrinsics)
/// now() is special-cased in the compiler to generate syscalls
/// Other Date functions are defined in Date.dark as pure Dark code
let dateModule : ModuleDef = {
    Name = "Stdlib.Date"
    Functions = [
        // now : () -> Int64 - returns current Unix epoch seconds
        { Name = "now"; TypeParams = []; ParamTypes = []; ReturnType = TInt64 }
    ]
}

/// Raw memory intrinsics - internal only for HAMT implementation
/// These functions bypass the type system and should only be used in stdlib code
/// The names start with __ to indicate they are internal
let rawMemoryIntrinsics : ModuleFunc list = [
    // __raw_alloc : (Int64) -> RawPtr - allocate raw bytes
    { Name = "__raw_alloc"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TRawPtr }
    // __raw_free : (RawPtr) -> Unit - free raw memory
    { Name = "__raw_free"; TypeParams = []; ParamTypes = [TRawPtr]; ReturnType = TUnit }
    // __raw_get<v> : (RawPtr, Int64) -> v - read 8 bytes at offset, typed as v
    { Name = "__raw_get"; TypeParams = ["v"]; ParamTypes = [TRawPtr; TInt64]; ReturnType = TVar "v" }
    // __raw_set<v> : (RawPtr, Int64, v) -> Unit - write 8 bytes at offset
    { Name = "__raw_set"; TypeParams = ["v"]; ParamTypes = [TRawPtr; TInt64; TVar "v"]; ReturnType = TUnit }
    // __raw_get_byte : (RawPtr, Int64) -> Int64 - read 1 byte at offset, zero-extended
    { Name = "__raw_get_byte"; TypeParams = []; ParamTypes = [TRawPtr; TInt64]; ReturnType = TInt64 }
    // __raw_set_byte : (RawPtr, Int64, Int64) -> Unit - write 1 byte at offset
    { Name = "__raw_set_byte"; TypeParams = []; ParamTypes = [TRawPtr; TInt64; TInt64]; ReturnType = TUnit }
    // __rawptr_to_int64 : (RawPtr) -> Int64 - cast pointer to int (for tagging)
    { Name = "__rawptr_to_int64"; TypeParams = []; ParamTypes = [TRawPtr]; ReturnType = TInt64 }
    // __int64_to_rawptr : (Int64) -> RawPtr - cast int to pointer (for memory ops)
    { Name = "__int64_to_rawptr"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TRawPtr }
    // __refcount_inc_string : (String) -> Unit - increment string refcount
    { Name = "__refcount_inc_string"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
    // __refcount_dec_string : (String) -> Unit - decrement string refcount, free if 0
    { Name = "__refcount_dec_string"; TypeParams = []; ParamTypes = [TString]; ReturnType = TUnit }
    // __string_to_int64 : (String) -> Int64 - cast string pointer to int (for storage)
    { Name = "__string_to_int64"; TypeParams = []; ParamTypes = [TString]; ReturnType = TInt64 }
    // __int64_to_string : (Int64) -> String - cast int to string pointer (for retrieval)
    { Name = "__int64_to_string"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TString }

    // Bytes intrinsics - for byte array operations
    // __bytes_to_int64 : (Bytes) -> Int64 - cast bytes pointer to int (for storage)
    { Name = "__bytes_to_int64"; TypeParams = []; ParamTypes = [TBytes]; ReturnType = TInt64 }
    // __int64_to_bytes : (Int64) -> Bytes - cast int to bytes pointer (for retrieval)
    { Name = "__int64_to_bytes"; TypeParams = []; ParamTypes = [TInt64]; ReturnType = TBytes }

    // Dict intrinsics - for type-safe Dict<k, v> operations
    // __empty_dict<k, v> : () -> Dict<k, v> - create empty dict (null pointer)
    { Name = "__empty_dict"; TypeParams = ["k"; "v"]; ParamTypes = []; ReturnType = TDict(TVar "k", TVar "v") }
    // __dict_is_null<k, v> : (Dict<k, v>) -> Bool - check if dict is empty/null
    { Name = "__dict_is_null"; TypeParams = ["k"; "v"]; ParamTypes = [TDict(TVar "k", TVar "v")]; ReturnType = TBool }
    // __dict_get_tag<k, v> : (Dict<k, v>) -> Int64 - get tag bits from dict pointer
    { Name = "__dict_get_tag"; TypeParams = ["k"; "v"]; ParamTypes = [TDict(TVar "k", TVar "v")]; ReturnType = TInt64 }
    // __dict_to_rawptr<k, v> : (Dict<k, v>) -> RawPtr - convert dict to raw pointer (strips tag)
    { Name = "__dict_to_rawptr"; TypeParams = ["k"; "v"]; ParamTypes = [TDict(TVar "k", TVar "v")]; ReturnType = TRawPtr }
    // __rawptr_to_dict<k, v> : (RawPtr, Int64) -> Dict<k, v> - create dict from pointer + tag
    { Name = "__rawptr_to_dict"; TypeParams = ["k"; "v"]; ParamTypes = [TRawPtr; TInt64]; ReturnType = TDict(TVar "k", TVar "v") }

    // Key intrinsics - for generic key hashing and comparison
    // __hash<k> : (k) -> Int64 - hash any key type
    { Name = "__hash"; TypeParams = ["k"]; ParamTypes = [TVar "k"]; ReturnType = TInt64 }
    // __key_eq<k> : (k, k) -> Bool - compare two keys for equality
    { Name = "__key_eq"; TypeParams = ["k"]; ParamTypes = [TVar "k"; TVar "k"]; ReturnType = TBool }

    // List intrinsics - for Finger Tree implementation
    // __list_empty<a> : () -> List<a> - create empty list (null pointer with tag 0)
    { Name = "__list_empty"; TypeParams = ["a"]; ParamTypes = []; ReturnType = TList(TVar "a") }
    // __list_is_null<a> : (List<a>) -> Bool - check if list is empty/null
    { Name = "__list_is_null"; TypeParams = ["a"]; ParamTypes = [TList(TVar "a")]; ReturnType = TBool }
    // __list_get_tag<a> : (List<a>) -> Int64 - get tag bits from list pointer (low 3 bits)
    { Name = "__list_get_tag"; TypeParams = ["a"]; ParamTypes = [TList(TVar "a")]; ReturnType = TInt64 }
    // __list_to_rawptr<a> : (List<a>) -> RawPtr - convert list to raw pointer (strips tag)
    { Name = "__list_to_rawptr"; TypeParams = ["a"]; ParamTypes = [TList(TVar "a")]; ReturnType = TRawPtr }
    // __rawptr_to_list<a> : (RawPtr, Int64) -> List<a> - create list from pointer + tag
    { Name = "__rawptr_to_list"; TypeParams = ["a"]; ParamTypes = [TRawPtr; TInt64]; ReturnType = TList(TVar "a") }
]

/// All intrinsic Stdlib modules
let allModules : ModuleDef list = [
    int64IntrinsicModule
    floatIntrinsicModule
    fileModule
    pathModule
    platformModule
    randomModule
    dateModule
]

/// Build the module registry from all modules
/// Maps qualified function names (e.g., "Stdlib.Int64.add") to their definitions
let buildModuleRegistry () : ModuleRegistry =
    let moduleFuncs =
        allModules
        |> List.collect (fun m ->
            m.Functions
            |> List.map (fun f -> ($"{m.Name}.{f.Name}", f)))
    // Add raw memory intrinsics directly (no module prefix)
    let rawMemFuncs =
        rawMemoryIntrinsics
        |> List.map (fun f -> (f.Name, f))
    (moduleFuncs @ rawMemFuncs)
    |> Map.ofList

/// Get a function, trying with Stdlib prefix if not found
/// This allows writing Option.isSome instead of Stdlib.Option.isSome
/// Returns both the function and the resolved name (which may differ from the input)
let tryGetFunctionWithFallback (registry: ModuleRegistry) (qualifiedName: string) : (ModuleFunc * string) option =
    let tryLookupName (name: string) : (ModuleFunc * string) option =
        match Map.tryFind name registry with
        | Some f ->
            Some (f, name)
        | None ->
            // Legacy compatibility: many upstream tests still reference *_v0 names.
            if name.EndsWith("_v0") then
                let canonicalName = name.Substring(0, name.Length - 3)
                match Map.tryFind canonicalName registry with
                | Some f ->
                    Some (f, canonicalName)
                | None ->
                    None
            else
                None

    match tryLookupName qualifiedName with
    | Some resolved ->
        Some resolved
    | None ->
        // Try with Stdlib prefix if name has at least one dot (Module.func)
        if qualifiedName.Contains(".") && not (qualifiedName.StartsWith("Stdlib.")) then
            tryLookupName ("Stdlib." + qualifiedName)
        else
            None

/// Get the type of a module function as an AST.Type
let getFunctionType (func: ModuleFunc) : Type =
    TFunction (func.ParamTypes, func.ReturnType)
