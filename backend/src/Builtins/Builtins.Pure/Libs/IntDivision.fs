/// Shared integer division, remainder and modulus semantics.
/// Native builtins translate errors into runtime errors. Dark wrappers validate
/// inputs and construct the public Result values.
module Builtins.Pure.Libs.IntDivision

open Prelude
open LibExecution.RuntimeTypes

module RTE = RuntimeError


/// Native division failure, shared across widths
type DivisionError = | DivisionByZero

/// Native modulus failures, shared across widths
type ModulusError =
  | ZeroModulus
  | NegativeModulus

/// The value, or what `/` raises where `divide` returns an `Error`
let divisionOrRaise (vm : VMState) (r : Result<'a, DivisionError>) : 'a =
  match r with
  | Ok v -> v
  | Error DivisionByZero ->
    RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID

/// The value, or what `%` raises where `mod` returns an `Error`
let modulusOrRaise (vm : VMState) (r : Result<'a, ModulusError>) : 'a =
  match r with
  | Ok v -> v
  | Error ZeroModulus -> RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.threadID
  | Error NegativeModulus ->
    RTE.Ints.NegativeModulus |> RTE.Int |> raiseRTE vm.threadID


// One set per width, each over that width's own F# type. Signed fixed-width
// division wraps: its only overflow, `MinValue / -1`, is `MinValue` (.NET
// throws for Int32, Int64 and Int128, so every signed width says it
// explicitly). `MinValue % -1` throws the same way though the answer is
// always 0. Unsigned division cannot overflow. The modulus must be positive:
// the contract `%` has always had, which `mod` keeps.

// Int8

let divideInt8 (a : sbyte) (b : sbyte) : Result<sbyte, DivisionError> =
  if b = 0y then Error DivisionByZero
  elif a = System.SByte.MinValue && b = -1y then Ok System.SByte.MinValue
  else Ok(a / b)

let remainderInt8 (a : sbyte) (b : sbyte) : Result<sbyte, DivisionError> =
  if b = 0y then Error DivisionByZero
  elif b = -1y then Ok 0y
  else Ok(a % b)

let moduloInt8 (a : sbyte) (m : sbyte) : Result<sbyte, ModulusError> =
  if m = 0y then
    Error ZeroModulus
  elif m < 0y then
    Error NegativeModulus
  else
    let r = a % m
    Ok(if r < 0y then m + r else r)


// UInt8

let divideUInt8 (a : byte) (b : byte) : Result<byte, DivisionError> =
  if b = 0uy then Error DivisionByZero else Ok(a / b)

let moduloUInt8 (a : byte) (m : byte) : Result<byte, ModulusError> =
  if m = 0uy then Error ZeroModulus else Ok(a % m)


// Int16

let divideInt16 (a : int16) (b : int16) : Result<int16, DivisionError> =
  if b = 0s then Error DivisionByZero
  elif a = System.Int16.MinValue && b = -1s then Ok System.Int16.MinValue
  else Ok(a / b)

let remainderInt16 (a : int16) (b : int16) : Result<int16, DivisionError> =
  if b = 0s then Error DivisionByZero
  elif b = -1s then Ok 0s
  else Ok(a % b)

let moduloInt16 (a : int16) (m : int16) : Result<int16, ModulusError> =
  if m = 0s then
    Error ZeroModulus
  elif m < 0s then
    Error NegativeModulus
  else
    let r = a % m
    Ok(if r < 0s then m + r else r)


// UInt16

let divideUInt16 (a : uint16) (b : uint16) : Result<uint16, DivisionError> =
  if b = 0us then Error DivisionByZero else Ok(a / b)

let moduloUInt16 (a : uint16) (m : uint16) : Result<uint16, ModulusError> =
  if m = 0us then Error ZeroModulus else Ok(a % m)


// Int32

let divideInt32 (a : int32) (b : int32) : Result<int32, DivisionError> =
  if b = 0l then Error DivisionByZero
  elif a = System.Int32.MinValue && b = -1l then Ok System.Int32.MinValue
  else Ok(a / b)

let remainderInt32 (a : int32) (b : int32) : Result<int32, DivisionError> =
  if b = 0l then Error DivisionByZero
  elif b = -1l then Ok 0l
  else Ok(a % b)

let moduloInt32 (a : int32) (m : int32) : Result<int32, ModulusError> =
  if m = 0l then
    Error ZeroModulus
  elif m < 0l then
    Error NegativeModulus
  else
    let r = a % m
    Ok(if r < 0l then m + r else r)


// UInt32

let divideUInt32 (a : uint32) (b : uint32) : Result<uint32, DivisionError> =
  if b = 0ul then Error DivisionByZero else Ok(a / b)

let moduloUInt32 (a : uint32) (m : uint32) : Result<uint32, ModulusError> =
  if m = 0ul then Error ZeroModulus else Ok(a % m)


// Int64

let divideInt64 (a : int64) (b : int64) : Result<int64, DivisionError> =
  if b = 0L then Error DivisionByZero
  elif a = System.Int64.MinValue && b = -1L then Ok System.Int64.MinValue
  else Ok(a / b)

let remainderInt64 (a : int64) (b : int64) : Result<int64, DivisionError> =
  if b = 0L then Error DivisionByZero
  elif b = -1L then Ok 0L
  else Ok(a % b)

let moduloInt64 (a : int64) (m : int64) : Result<int64, ModulusError> =
  if m = 0L then
    Error ZeroModulus
  elif m < 0L then
    Error NegativeModulus
  else
    let r = a % m
    Ok(if r < 0L then m + r else r)


// UInt64

let divideUInt64 (a : uint64) (b : uint64) : Result<uint64, DivisionError> =
  if b = 0UL then Error DivisionByZero else Ok(a / b)

let moduloUInt64 (a : uint64) (m : uint64) : Result<uint64, ModulusError> =
  if m = 0UL then Error ZeroModulus else Ok(a % m)


// Int128

let divideInt128
  (a : System.Int128)
  (b : System.Int128)
  : Result<System.Int128, DivisionError> =
  if b = System.Int128.Zero then
    Error DivisionByZero
  elif a = System.Int128.MinValue && b = System.Int128.NegativeOne then
    Ok System.Int128.MinValue
  else
    Ok(a / b)

let remainderInt128
  (a : System.Int128)
  (b : System.Int128)
  : Result<System.Int128, DivisionError> =
  if b = System.Int128.Zero then Error DivisionByZero
  elif b = System.Int128.NegativeOne then Ok System.Int128.Zero
  else Ok(a % b)

let moduloInt128
  (a : System.Int128)
  (m : System.Int128)
  : Result<System.Int128, ModulusError> =
  if m = System.Int128.Zero then
    Error ZeroModulus
  elif m < System.Int128.Zero then
    Error NegativeModulus
  else
    let r = a % m
    Ok(if r < System.Int128.Zero then m + r else r)


// UInt128

let divideUInt128
  (a : System.UInt128)
  (b : System.UInt128)
  : Result<System.UInt128, DivisionError> =
  if b = System.UInt128.Zero then Error DivisionByZero else Ok(a / b)

let moduloUInt128
  (a : System.UInt128)
  (m : System.UInt128)
  : Result<System.UInt128, ModulusError> =
  if m = System.UInt128.Zero then Error ZeroModulus else Ok(a % m)


// Int, arbitrary precision: nothing overflows

let divideInt (a : DarkInt) (b : DarkInt) : Result<DarkInt, DivisionError> =
  if DarkInt.isZero b then Error DivisionByZero else Ok(DarkInt.divide a b)

let remainderInt (a : DarkInt) (b : DarkInt) : Result<DarkInt, DivisionError> =
  if DarkInt.isZero b then
    Error DivisionByZero
  else
    Ok(DarkInt.ofBigInt (DarkInt.toBigInt a % DarkInt.toBigInt b))

let moduloInt (a : DarkInt) (m : DarkInt) : Result<DarkInt, ModulusError> =
  let m = DarkInt.toBigInt m
  if m.IsZero then
    Error ZeroModulus
  elif m.Sign < 0 then
    Error NegativeModulus
  else
    let r = DarkInt.toBigInt a % m
    Ok(DarkInt.ofBigInt (if r.Sign < 0 then m + r else r))
