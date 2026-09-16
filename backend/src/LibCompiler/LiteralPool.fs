// LiteralPool.fs - Literal pool storage for late constant resolution
//
// Provides string and float literal pools used during ARM64 emission/encoding.

module LiteralPool

/// String pool for late constant resolution (used by LIR/codegen)
type StringPool = {
    Strings: Map<int, string * int>
    StringToId: Map<string, int>
    NextId: int
}

/// Float pool for late constant resolution (used by LIR/codegen)
/// Uses int64 bit representation as key to distinguish -0.0 from 0.0
type FloatPool = {
    Floats: Map<int, float>
    FloatBitsToId: Map<int64, int>
    NextId: int
}

/// Empty string pool
let emptyStringPool : StringPool = {
    Strings = Map.empty
    StringToId = Map.empty
    NextId = 0
}

/// Empty float pool
let emptyFloatPool : FloatPool = {
    Floats = Map.empty
    FloatBitsToId = Map.empty
    NextId = 0
}

/// Add a string to the pool (deduplicated), returning index and updated pool
let addString (pool: StringPool) (value: string) : int * StringPool =
    match Map.tryFind value pool.StringToId with
    | Some idx -> (idx, pool)
    | None ->
        let len = System.Text.Encoding.UTF8.GetByteCount value
        let idx = pool.NextId
        let strings = Map.add idx (value, len) pool.Strings
        let stringToId = Map.add value idx pool.StringToId
        let pool' = { pool with Strings = strings; StringToId = stringToId; NextId = idx + 1 }
        (idx, pool')

/// Add a float to the pool (deduplicated by bit pattern), returning index and updated pool
/// Uses bit-level comparison to distinguish -0.0 from 0.0 (they're equal in IEEE 754 comparison)
let addFloat (pool: FloatPool) (value: float) : int * FloatPool =
    let bits = System.BitConverter.DoubleToInt64Bits(value)
    match Map.tryFind bits pool.FloatBitsToId with
    | Some idx -> (idx, pool)
    | None ->
        let idx = pool.NextId
        let floats = Map.add idx value pool.Floats
        let floatBitsToId = Map.add bits idx pool.FloatBitsToId
        let pool' = { pool with Floats = floats; FloatBitsToId = floatBitsToId; NextId = idx + 1 }
        (idx, pool')
