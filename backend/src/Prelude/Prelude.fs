module Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks

// ----------------------
// Fix a few functions everywhere
// ----------------------

// Ensure a type is always used
[<RequiresExplicitTypeArgumentsAttribute>]
let ignore<'a> (a : 'a) : unit = ignore<'a> a

[<CompilerMessageAttribute("failwith is banned, use Exception.raise* instead",
                           0,
                           IsError = true,
                           IsHidden = true)>]
let failwith = failwith

[<CompilerMessageAttribute("printf is banned, use Prelude.printInline instead",
                           0,
                           IsError = true,
                           IsHidden = true)>]
let printf = printf

[<CompilerMessageAttribute("printfn is banned, use Prelude.print instead",
                           0,
                           IsError = true,
                           IsHidden = true)>]
let printfn = printfn


// ----------------------
// Null
// ----------------------
// https://stackoverflow.com/a/11696947/104021
let inline isNull (x : ^T when ^T : not struct) = obj.ReferenceEquals(x, null)

// ----------------------
// Shared Types
// ----------------------
// Some fundamental types that we want to use everywhere.

type NEList<'a> = NEList.NEList<'a>
type Metadata = Exception.Metadata

type HashSet<'a> = HashSet.HashSet<'a>

type Ply<'a> = Ply.Ply<'a>
let uply = Ply.uply

type uuid = System.Guid
type CanvasID = uuid
type UserID = uuid
type tlid = uint64
type id = uint64

let id (x : int) : id = uint64 x

// since we hide F#'s default 'id' fn just above
let identity a = a


// ----------------------
// Debugging
// ----------------------

let debuG (msg : string) (a : 'a) : unit =
  // Don't deadlock when debugging
  NonBlockingConsole.writeLine $"DEBUG: {msg} ({a})"

let debug (msg : string) (a : 'a) : 'a =
  debuG msg a
  a

// Print the value of s, alongside with length and the bytes in the string
let debugString (msg : string) (s : string) : string =
  let bytes = s |> System.Text.Encoding.UTF8.GetBytes |> System.BitConverter.ToString
  NonBlockingConsole.writeLine $"DEBUG: {msg} ('{s}': (len {s.Length}, {bytes})"
  s

let debuGByteArray (msg : string) (a : byte array) : unit =
  let bytes = a |> System.BitConverter.ToString
  NonBlockingConsole.writeLine $"DEBUG: {msg} (len {a.Length}, {bytes}"


let debugByteArray (msg : string) (a : byte array) : byte array =
  debuGByteArray msg a
  a

let debuGList (msg : string) (list : List<'a>) : unit =
  if list = [] then
    NonBlockingConsole.writeLine $"DEBUG: {msg} (len 0, [])"
  else

    [ $"DEBUG: {msg} (len {List.length list}, [" ]
    @ List.map (fun item -> $"  {item}") list
    @ [ $"])" ]
    |> String.concat "\n"
    |> NonBlockingConsole.writeLine

let debugList (msg : string) (list : List<'a>) : List<'a> =
  debuGList msg list
  list

let debuGSet (msg : string) (set : Set<'a>) : unit =
  if set = Set.empty then
    NonBlockingConsole.writeLine $"DEBUG: {msg} (len 0, {{}})"
  else
    [ $"DEBUG: {msg} (len {Set.count set}, {{" ]
    @ (set |> Set.toList |> List.map (fun item -> $"  {item}"))
    @ [ $"}})" ]
    |> String.concat "\n"
    |> NonBlockingConsole.writeLine

let debugSet (msg : string) (set : Set<'a>) : Set<'a> =
  debuGSet msg set
  set

let debuGArray (msg : string) (array : 'a[]) : unit =
  if array.Length = 0 then
    NonBlockingConsole.writeLine $"DEBUG: {msg} (len 0, [])"
  else
    [ $"DEBUG: {msg} (len {array.Length}, [" ]
    @ (array |> Array.toList |> List.map (fun item -> $"  {item}"))
    @ [ $"])" ]
    |> String.concat "\n"
    |> NonBlockingConsole.writeLine

let debugArray (msg : string) (array : 'a[]) : 'a[] =
  debuGArray msg array
  array

let debuGMap (msg : string) (map : Map<'k, 'v>) : unit =
  if map = Map.empty then
    NonBlockingConsole.writeLine $"DEBUG: {msg} (len 0, [])"
  else
    [ $"DEBUG: {msg} (len {Map.count map}, [" ]
    @ (Map.toList map |> List.map (fun (k, v) -> $"  ({k}, {v})"))
    @ [ $"])" ]
    |> String.concat "\n"
    |> NonBlockingConsole.writeLine

let debugMap (msg : string) (map : Map<'k, 'v>) : Map<'k, 'v> =
  debuGMap msg map
  map


let debugBy (msg : string) (f : 'a -> 'b) (v : 'a) : 'a =
  NonBlockingConsole.writeLine $"DEBUG: {msg} {f v}"
  v

let printInline (string : string) : unit = NonBlockingConsole.writeInline string

let print (string : string) : unit = NonBlockingConsole.writeLine string

let printTime (string : string) : unit =
  let now = System.DateTime.UtcNow.ToString("mm:ss.ff")
  print $"{now} {string}"

// Print the value of `a`. Note that since this is wrapped in a task, it must
// resolve the task before it can print, which could lead to different ordering
// of operations.
let debugPly (msg : string) (a : Ply.Ply<'a>) : Ply.Ply<'a> =
  uply {
    let! a = a
    NonBlockingConsole.writeLine $"DEBUG: {msg} ({a})"
    return a
  }

let debugTask (msg : string) (a : Task<'a>) : Task<'a> =
  task {
    let! a = a
    NonBlockingConsole.writeLine $"DEBUG: {msg} ({a})"
    return a
  }

let printMetadata (prefix : string) (metadata : Exception.Metadata) =
  try
    List.iter (fun (k, v) -> print (sprintf "%s:  %s: %A" prefix k v)) metadata
  with _ ->
    ()

let rec printException'
  (prefix : string)
  (count : int)
  (metadata : Exception.Metadata)
  (e : exn)
  : unit =
  print $"{prefix}: error: {e.Message}"
  printMetadata prefix metadata
  printMetadata prefix (Exception.toMetadata e)
  print $"{prefix}: exceptionType: {e.GetType()}"
  print $"{prefix}: {e.StackTrace}"
  if not (isNull e.InnerException) then
    printException' $"prefex.inner[{count}]" (count + 1) [] e.InnerException

let printException
  (prefix : string)
  (metadata : Exception.Metadata)
  (e : exn)
  : unit =
  printException' prefix 0 metadata e


// ----------------------
// Assertions
// ----------------------
// Asserts are problematic because they don't run in prod, and if they did they
// wouldn't be caught by the webserver

let assert_ (msg : string) (metadata : Exception.Metadata) (cond : bool) : unit =
  if cond then
    ()
  else
    Exception.raiseInternal $"Assertion failure: {msg}" metadata

let assertEq (msg : string) (expected : 'a) (actual : 'a) : unit =
  if expected <> actual then
    Exception.raiseInternal
      $"Assertion equality failure: {msg}"
      [ "expected", expected :> obj; "actual", actual :> obj ]

let assertIn (msg : string) (expected : List<'a>) (actual : 'a) : unit =
  if not (List.contains actual expected) then
    Exception.raiseInternal
      $"Assertion equality failure: {msg}"
      [ "expected", expected :> obj; "actual", actual :> obj ]


let assertFn (msg : string) (fn : 'a -> bool) (arg : 'a) : unit =
  if not (fn arg) then
    Exception.raiseInternal $"Function failure: {msg}" [ "arg", arg :> obj ]

let assertFn2 (msg : string) (fn : 'a -> 'b -> bool) (arg1 : 'a) (arg2 : 'b) : unit =
  if not (fn arg1 arg2) then
    Exception.raiseInternal
      $"Function failure: {msg}"
      [ "arg1", arg1 :> obj; "arg2", arg2 :> obj ]

let assertFn3
  (msg : string)
  (fn : 'a -> 'b -> 'c -> bool)
  (arg1 : 'a)
  (arg2 : 'b)
  (arg3 : 'c)
  : unit =
  if not (fn arg1 arg2 arg3) then
    Exception.raiseInternal
      $"Function failure: {msg}"
      [ "arg1", arg1 :> obj; "arg2", arg2 :> obj; "arg3", arg3 :> obj ]


let assertRe (msg : string) (pattern : string) (input : string) : unit =
  let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
  if m.Success then
    ()
  else
    Exception.raiseInternal
      $"Assertion regex failure: {msg}"
      [ "input", input; "pattern", pattern ]

// ----------------------
// Standard conversion functions
// ----------------------
// There are multiple ways to convert things in dotnet. Let's have a consistent set we use.
// We use an explicit sign for Floats, instead of making it implicit in the
// first digit, because otherwise we lose the sign on 0, and can't represent
// things like -0.5
type Sign =
  | Positive
  | Negative

// Given a float, read it correctly into two ints: whole number and fraction
let readFloat (f : float) : (Sign * string * string) =
  let sign =
    // (0.0 = -0.0) is true in .Net, so it can be quite tough to figure out the sign
    if string f = "-0" then Negative
    else if f >= 0.0 then Positive
    else Negative
  let asStr = f.ToString("G53").Split "."
  let whole =
    match sign with
    | Negative -> String.dropLeft 1 asStr[0]
    | Positive -> asStr[0]
  let fraction = if asStr.Length = 1 then "0" else asStr[1]
  sign, whole, fraction


let makeFloat (sign : Sign) (whole : string) (fraction : string) : float =
  try
    if whole <> "" then assert_ "non-zero string" [] (whole[0] <> '-')
    if whole <> "0" then assertRe $"makefloat" "[1-9][0-9]*" whole
    let sign =
      match sign with
      | Positive -> ""
      | Negative -> "-"
    $"{sign}{whole}.{fraction}" |> System.Double.Parse
  with e ->
    Exception.raiseInternal
      $"makeFloat failed"
      [ "sign", sign; "whole", whole; "fraction", fraction; "inner", e ]





type NodaTime.Instant with

  static member UnixEpoch = NodaTime.Instant.FromUnixTimeSeconds 0

  static member now() : NodaTime.Instant =
    NodaTime.SystemClock.Instance.GetCurrentInstant()

  // Convert to an LocalDateTime with an implicit Utc timezone
  member this.toUtcLocalTimeZone() : NodaTime.LocalDateTime =
    let utc = NodaTime.DateTimeZone.Utc
    let zonedDateTime = NodaTime.ZonedDateTime(this, utc)
    let mutable localDateTime = NodaTime.LocalDateTime()
    let mutable (tz : NodaTime.DateTimeZone) = null
    let mutable (offset : NodaTime.Offset) = NodaTime.Offset()
    zonedDateTime.Deconstruct(&localDateTime, &tz, &offset)
    localDateTime

  member this.toIsoString() : string =
    let dt = this.ToDateTimeUtc()
    dt.ToString("s", System.Globalization.CultureInfo.InvariantCulture) + "Z"

  // Returns a new datetime with truncated
  member this.truncate() : NodaTime.Instant =
    // get it to zero milliseconds
    let dt = this.ToDateTimeUtc()
    let dt = dt.AddTicks(-dt.Ticks % System.TimeSpan.TicksPerSecond)
    NodaTime.Instant.FromDateTimeUtc dt


  static member ofIsoString(str : string) : NodaTime.Instant =
    let dt =
      System.DateTime.ParseExact(
        str,
        "yyyy-MM-ddTHH:mm:ssZ",
        System.Globalization.CultureInfo.InvariantCulture
      )
    let utcDateTime = System.DateTime(dt.Ticks, System.DateTimeKind.Utc)
    NodaTime.Instant.FromDateTimeUtc utcDateTime
  static member ofUtcInstant
    (
      year,
      month,
      day,
      hour,
      minute,
      second : int
    ) : NodaTime.Instant =
    let local = NodaTime.LocalDateTime(year, month, day, hour, minute, second)
    NodaTime
      .ZonedDateTime(local, NodaTime.DateTimeZone.Utc, NodaTime.Offset.Zero)
      .ToInstant()

  static member parse(str : string) : NodaTime.Instant =
    // Parse using standard. Assume always UTC
    let dt = System.DateTime.Parse(str)
    let utcDateTime = System.DateTime(dt.Ticks, System.DateTimeKind.Utc)
    NodaTime.Instant.FromDateTimeUtc utcDateTime



// ----------------------
// Random numbers
// ----------------------

// .NET's System.Random is a PRNG, and it is relatively easy to work out the random
// seed. Better to use a True random number generator.
type RNG = System.Security.Cryptography.RandomNumberGenerator

// return a System.Random PRNG with a truly random seed. This allows using the
// System.Random methods which have many more options.
let randomSeeded () : System.Random =
  let seed = RNG.GetInt32(System.Int32.MaxValue)
  System.Random(seed)


let gid () : uint64 =
  try
    let rand64 = RNG.GetBytes(8) |> System.BitConverter.ToUInt64
    // Dark only has 63 bits of positive numbers, so to make it easier to convert IDs
    // to dark, we keep IDs in that range.
    let mask =
      0b0111_1111_1111_1111_1111_1111_1111_1111_0011_1111_1111_1111_1111_1111_1111_1111UL
    rand64 &&& mask
  with e ->
    Exception.raiseInternal $"gid failed" [ "message", e.Message; "inner", e ]

let guuid(): uuid =
  System.Guid.NewGuid()

let randomString (length : int) : string =
  let result =
    Array.init length (fun _ -> char (RNG.GetInt32(int32 'A', int32 'Z')))
    |> System.String

  assertEq "randomString length is correct" result.Length length
  result


module NumericLiteralQ =
  let FromZero () = System.Int128.Zero
  let FromOne () = System.Int128.One
  let FromInt32 (i : int) = System.Int128.Parse(string i)
  let FromInt64 (i : int64) = System.Int128.Parse(string i)
  let FromString (s : string) = System.Int128.Parse(s)

module NumericLiteralZ =
  let FromZero () = System.UInt128.Zero
  let FromOne () = System.UInt128.One
  let FromInt32 (i : int) = System.UInt128.Parse(string i)
  let FromInt64 (i : int64) = System.UInt128.Parse(string i)
  let FromString (s : string) = System.UInt128.Parse(s)
