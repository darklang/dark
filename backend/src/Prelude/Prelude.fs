module Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open System.Text.RegularExpressions

// ----------------------
// Fix a few functions everywhere
// ----------------------

// Ensure a type is always used
[<RequiresExplicitTypeArgumentsAttribute>]
let ignore<'a> (a : 'a) : unit = ignore<'a> a

[<CompilerMessageAttribute("failwith is banned, use Prelude.Exception.raise* instead",
                           0,
                           IsError = true,
                           IsHidden = true)>]
let failwith = failwith

[<CompilerMessageAttribute("printf is banned, use Prelude.print instead",
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
// Exceptions
// We don't use the F# exception syntax as we want to allow wrapping inner exceptions
// ----------------------

type Metadata = List<string * obj>

/// An error within Dark itself - we need to rollbar this and address it.
///
/// Do not show to anyone, unless within an Analysis request.
type InternalException(message : string, metadata : Metadata, inner : exn) =
  inherit System.Exception(message, inner)
  member _.metadata = metadata
  new(msg : string) = InternalException(msg, [], null)
  new(msg : string, metadata : Metadata) = InternalException(msg, metadata, null)
  new(msg : string, inner : exn) = InternalException(msg, [], inner)

// A grand user exception was caused by the incorrect actions of a grand user. The
// msg is suitable to show to the grand user. We don't care about grandUser
// exceptions, they're normal. When a message can be safely propagated to show to the
// grand user, it's safe to use this exception, even if it's more properly defined by
// another exception type.
type GrandUserException(message : string, inner : exn) =
  inherit System.Exception(message, inner)
  new(msg : string) = GrandUserException(msg, null)

/// An error during code execution, which is the responsibility of the
/// User/Developer. The message can be shown to the developer. You can alternatively
/// use GrandUser exception in code which is used in both Libraries and in the
/// HttpFramework.
type CodeException(message : string, inner : exn) =
  inherit System.Exception(message, inner)
  new(msg : string) = CodeException(msg, null)

/// An editor exception is one which is caused by an invalid action on the part of
/// the Dark editor, such as an Redo or rename that isn't allowed.  We are
/// interested in these, as the editor should have caught this on the client and not
/// made the request. The message may be shown to the logged-in user, and should be
/// suitable for this.
type EditorException(message : string, inner : exn) =
  inherit System.Exception(message, inner)
  new(msg : string) = EditorException(msg, null)

// A pageable exception will cause the pager to go off! This is something that should
// never happen and is an indicator that the service is broken in some way.  The
// pager goes off because a pageable exception sets the `{ is_pageable: true }`
// metadata, which causes a honeycomb trigger that sets off PagerDuty.
type PageableException(message : string, metadata : Metadata, inner : exn) =
  inherit System.Exception(message, inner)
  member _.metadata = metadata


// This is for tracing
let mutable exceptionCallback = (fun (_e : exn) -> ())

let mutable sendRollbarError = (fun (_message : string) (_metadata : Metadata) -> ())

module Exception =

  /// Returns a list of exceptions of this exception, and all nested inner
  /// exceptions.
  let rec getMessages (e : exn) : List<string> =
    if isNull e.InnerException then
      [ e.Message ]
    else
      e.Message :: getMessages e.InnerException

  let toMetadata (e : exn) : Metadata =
    let thisMetadata =
      match e with
      | :? PageableException as e -> [ "is_pageable", true :> obj ] @ e.metadata
      | :? InternalException as e -> e.metadata
      | :? EditorException
      | :? CodeException
      | :? GrandUserException
      | _ -> []
    thisMetadata

  let rec nestedMetadata (e : exn) : Metadata =
    let innerMetadata =
      if not (isNull e.InnerException) then nestedMetadata e.InnerException else []
    let thisMetadata =
      match e with
      | :? PageableException as e -> [ "is_pageable", true :> obj ] @ e.metadata
      | :? InternalException as e -> e.metadata
      | :? EditorException
      | :? CodeException
      | :? GrandUserException
      | _ -> []
    thisMetadata @ innerMetadata


  let callExceptionCallback (e : exn) =
    try
      exceptionCallback e
    with e ->
      // We're completely screwed at this point
      System.Console.WriteLine "Exception calling callExceptionCallback"
      System.Console.WriteLine(e.Message)
      System.Console.WriteLine e.StackTrace


  let raiseGrandUser (msg : string) =
    let e = GrandUserException(msg)
    callExceptionCallback e
    raise e

  let raiseCode (msg : string) =
    let e = CodeException(msg)
    callExceptionCallback e
    raise e

  let unwrapOptionCode (msg : string) (o : Option<'a>) : 'a =
    match o with
    | Some v -> v
    | None -> raiseCode msg

  let unwrapResultCode (r : Result<'a, string>) : 'a =
    match r with
    | Ok v -> v
    | Error msg -> raiseCode msg

  let raiseEditor (msg : string) =
    let e = EditorException(msg)
    callExceptionCallback e
    raise e

  let raiseInternal (msg : string) (tags : Metadata) =
    let e = InternalException(msg, tags)
    callExceptionCallback e
    raise e

  let unwrapOptionInternal (msg : string) (tags : Metadata) (o : Option<'a>) : 'a =
    match o with
    | Some v -> v
    | None -> raiseInternal msg tags

  let unwrapResultInternal (tags : Metadata) (r : Result<'a, 'msg>) : 'a =
    match r with
    | Ok v -> v
    | Error msg -> raiseInternal (string msg) tags

  let reraiseAsPageable (msg : string) (tags : Metadata) (e : exn) =
    let e = PageableException(msg, tags, e)
    callExceptionCallback e
    raise e

  let unknownErrorMessage = "Unknown error"

  let toGrandUserMessage (e : exn) : string =
    match e with
    | :? GrandUserException as e -> e.Message
    | _ -> unknownErrorMessage

  let taskCatch (f : unit -> Task<'r>) : Task<Option<'r>> =
    task {
      try
        let! result = f ()
        return Some result
      with _ ->
        return None
    }

  let catch (f : unit -> 'r) : Option<'r> =
    try
      Some(f ())
    with _ ->
      None

  let catchError (f : unit -> 'r) : Result<'r, string> =
    try
      Ok(f ())
    with e ->
      Error e.Message

type System.Exception with

  /// <summary>
  /// This hack adds a `Reraise` method to exceptions, since
  /// it's not normally possible to reraise exceptions within F# CEs.
  /// </summary>
  ///
  /// <remarks>
  /// Sources:
  /// - https://github.com/fsharp/fslang-suggestions/issues/660#issuecomment-382070639
  /// - https://stackoverflow.com/questions/57383
  /// </remarks>
  member this.Reraise() =
    (System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture this).Throw()
    Unchecked.defaultof<_>

// ----------------------
// Regex patterns
// ----------------------

// Active pattern for regexes
let (|Regex|_|) (pattern : string) (input : string) =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

let (|RegexAny|_|) (pattern : string) (input : string) =
  let options = RegexOptions.Singleline
  let m = Regex.Match(input, pattern, options)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None



let matches (pattern : string) (input : string) : bool =
  let m = Regex.Match(input, pattern)
  m.Success

// ----------------------
// Runtime info
// ----------------------
/// LIGHTTODO
let isWasm = System.OperatingSystem.IsBrowser()

// ----------------------
// Debugging
// ----------------------

type BlockingCollection = System.Collections.Concurrent.BlockingCollection<string>

type NonBlockingConsole() =

  // It seems like printing on the Console can cause a deadlock. I observed that all
  // the tasks in the threadpool were blocking on Console.WriteLine, and that the
  // logging thread in the background was blocked on one of those threads. This is
  // like a known issue with a known solution:
  // https://stackoverflow.com/a/3670628/104021.

  // Note that there are sometimes other loggers, such as in IHosts, which may also
  // need to move off the console logger.

  // This adds a collection which receives all output from WriteLine. Then, a
  // background thread writes the output to Console.

  static let mQueue : BlockingCollection = new BlockingCollection()

  // Use a lock so that wait() doesn't return until the thread has actually printed
  // (it would finish once it was removed from the queue)
  static let mLock : obj = obj ()

  static do
    let f () =
      while true do
        lock mLock (fun () ->
          try
            let mutable v = null
            // Don't block (eg with `Take`) while holding the lock
            if mQueue.TryTake(&v) then
              System.Console.WriteLine(v)
            else
              System.Threading.Thread.Sleep 1 // 1ms
          with e ->
            System.Console.WriteLine(
              $"Exception in blocking queue thread: {e.Message}"
            ))


    // Background threads aren't supported in Blazor
    if not isWasm then
      let thread = System.Threading.Thread(f)
      thread.IsBackground <- true
      thread.Name <- "Prelude.NonBlockingConsole printer"
      thread.Start()

  static member wait() : unit =
    let mutable shouldWait = true
    while shouldWait do
      lock mLock (fun () -> shouldWait <- mQueue.Count > 0)

  static member WriteLine(value : string) : unit =
    if isWasm then System.Console.WriteLine value else mQueue.Add(value)


let debuG (msg : string) (a : 'a) : unit =
  // Don't deadlock when debugging
  NonBlockingConsole.WriteLine $"DEBUG: {msg} ({a})"

let debug (msg : string) (a : 'a) : 'a =
  debuG msg a
  a

// Print the value of s, alongside with length and the bytes in the string
let debugString (msg : string) (s : string) : string =
  let bytes = s |> System.Text.Encoding.UTF8.GetBytes |> System.BitConverter.ToString
  NonBlockingConsole.WriteLine $"DEBUG: {msg} ('{s}': (len {s.Length}, {bytes})"
  s

let debuGByteArray (msg : string) (a : byte array) : unit =
  let bytes = a |> System.BitConverter.ToString
  NonBlockingConsole.WriteLine $"DEBUG: {msg} (len {a.Length}, {bytes}"


let debugByteArray (msg : string) (a : byte array) : byte array =
  debuGByteArray msg a
  a

let debuGList (msg : string) (list : List<'a>) : unit =
  if list = [] then
    NonBlockingConsole.WriteLine $"DEBUG: {msg} (len 0, [])"
  else

    [ $"DEBUG: {msg} (len {List.length list}, [" ]
    @ List.map (fun item -> $"  {item}") list
    @ [ $"])" ]
    |> String.concat "\n"
    |> NonBlockingConsole.WriteLine

let debugList (msg : string) (list : List<'a>) : List<'a> =
  debuGList msg list
  list

let debuGArray (msg : string) (array : 'a[]) : unit =
  if array.Length = 0 then
    NonBlockingConsole.WriteLine $"DEBUG: {msg} (len 0, [])"
  else
    [ $"DEBUG: {msg} (len {array.Length}, [" ]
    @ (array |> Array.toList |> List.map (fun item -> $"  {item}"))
    @ [ $"])" ]
    |> String.concat "\n"
    |> NonBlockingConsole.WriteLine

let debugArray (msg : string) (array : 'a[]) : 'a[] =
  debuGArray msg array
  array

let debuGMap (msg : string) (map : Map<'k, 'v>) : unit =
  if map = Map.empty then
    NonBlockingConsole.WriteLine $"DEBUG: {msg} (len 0, [])"
  else
    [ $"DEBUG: {msg} (len {Map.count map}, [" ]
    @ (Map.toList map |> List.map (fun (k, v) -> $"  ({k}, {v})"))
    @ [ $"])" ]
    |> String.concat "\n"
    |> NonBlockingConsole.WriteLine

let debugMap (msg : string) (map : Map<'k, 'v>) : Map<'k, 'v> =
  debuGMap msg map
  map


let debugBy (msg : string) (f : 'a -> 'b) (v : 'a) : 'a =
  NonBlockingConsole.WriteLine $"DEBUG: {msg} {f v}"
  v

let print (string : string) : unit = NonBlockingConsole.WriteLine string


// Print the value of `a`. Note that since this is wrapped in a task, it must
// resolve the task before it can print, which could lead to different ordering
// of operations.
let debugPly (msg : string) (a : Ply.Ply<'a>) : Ply.Ply<'a> =
  uply {
    let! a = a
    NonBlockingConsole.WriteLine $"DEBUG: {msg} ({a})"
    return a
  }

let debugTask (msg : string) (a : Task<'a>) : Task<'a> =
  task {
    let! a = a
    NonBlockingConsole.WriteLine $"DEBUG: {msg} ({a})"
    return a
  }

let printMetadata (prefix : string) (metadata : Metadata) =
  try
    List.iter (fun (k, v) -> print (sprintf "%s:  %s: %A" prefix k v)) metadata
  with _ ->
    ()

let rec printException'
  (prefix : string)
  (count : int)
  (metadata : Metadata)
  (e : exn)
  : unit =
  print $"{prefix}: error: {e.Message}"
  printMetadata prefix metadata
  printMetadata prefix (Exception.toMetadata e)
  print $"{prefix}: exceptionType: {e.GetType()}"
  print $"{prefix}: {e.StackTrace}"
  if not (isNull e.InnerException) then
    printException' $"prefex.inner[{count}]" (count + 1) [] e.InnerException

let printException (prefix : string) (metadata : Metadata) (e : exn) : unit =
  printException' prefix 0 metadata e


// ----------------------
// Assertions
// ----------------------
// Asserts are problematic because they don't run in prod, and if they did they
// wouldn't be caught by the webserver

let assert_ (msg : string) (metadata : Metadata) (cond : bool) : unit =
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
  if not (Tablecloth.List.includes actual expected) then
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
  let m = Regex.Match(input, pattern)
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

let parseInt (str : string) : Option<int> =
  try
    Some(int str)
  with _ ->
    None

let parseInt64 (str : string) : int64 =
  try
    assertRe "int64" @"-?\d+" str
    System.Convert.ToInt64 str
  with e ->
    Exception.raiseInternal $"parseInt64 failed" [ "str", str; "inner", e ]

let parseUInt64 (str : string) : uint64 =
  try
    assertRe "uint64" @"-?\d+" str
    System.Convert.ToUInt64 str
  with e ->
    Exception.raiseInternal $"parseUInt64 failed" [ "str", str; "inner", e ]

let parseBigint (str : string) : bigint =
  try
    assertRe "bigint" @"-?\d+" str
    System.Numerics.BigInteger.Parse str
  with e ->
    Exception.raiseInternal $"parseBigint failed" [ "str", str; "inner", e ]

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
    | Negative -> Tablecloth.String.dropLeft 1 asStr[0]
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

// The default System.Text.Encoding.UTF8 replaces invalid bytes on error. We
// sometimes want this, but often we want to assert that the bytes we can are valid
// and throw if they aren't.
module UTF8 =

  // This encoding throws errors on invalid bytes
  let utf8EncodingWithExceptions = System.Text.UTF8Encoding(false, true)

  // Throws if the bytes are not valid UTF8
  let ofBytesUnsafe (input : byte array) : string =
    utf8EncodingWithExceptions.GetString input

  // I'm assuming if it makes it into a UTF8 string it must be valid? That might be
  // an incorrect assumption.
  let toBytes (input : string) : byte array =
    utf8EncodingWithExceptions.GetBytes input

  let toBytesOpt (input : string) : byte array option =
    try
      Some(toBytes input)
    with e ->
      None

  let ofBytesOpt (input : byte array) : string option =
    try
      Some(ofBytesUnsafe input)
    with e ->
      None


  // Use this to ignore errors
  let ofBytesWithReplacement (input : byte array) : string =
    System.Text.Encoding.UTF8.GetString input

// Base64 comes in various flavors, typically URLsafe (has '-' and '_' with no
// padding) or regular (has + and / and has '=' padding at the end)
module Base64 =

  type Base64UrlEncoded = Base64UrlEncoded of string
  type Base64DefaultEncoded = Base64DefaultEncoded of string

  type T =
    | UrlEncoded of Base64UrlEncoded
    | DefaultEncoded of Base64DefaultEncoded

    override this.ToString() =
      match this with
      | UrlEncoded(Base64UrlEncoded s) -> s
      | DefaultEncoded(Base64DefaultEncoded s) -> s

  /// Encodes to base64 strings (using '+' and '/'), with padding. The result is not url-safe.
  let defaultEncodeToString (input : byte array) : string =
    System.Convert.ToBase64String input

  // Convert to base64 string
  let encode (input : byte array) : T =
    input |> defaultEncodeToString |> Base64DefaultEncoded |> DefaultEncoded

  // type-safe wrapper for an already-encoded urlEncoded string
  let fromUrlEncoded (string : string) : T = string |> Base64UrlEncoded |> UrlEncoded

  // type-safe wrapper for an already-encoded defaultEncoded string
  let fromDefaultEncoded (string : string) : T =
    string |> Base64DefaultEncoded |> DefaultEncoded

  // If we don't know how it's encoded, covert to urlEncoded as we can be certain then.
  let fromEncoded (string : string) : T =
    string.Replace('+', '-').Replace('/', '_').Replace("=", "")
    |> Base64UrlEncoded
    |> UrlEncoded

  let asUrlEncodedString (b64 : T) : string =
    match b64 with
    | UrlEncoded(Base64UrlEncoded s) -> s
    | DefaultEncoded(Base64DefaultEncoded s) ->
      s.Replace('+', '-').Replace('/', '_').Replace("=", "")

  let asDefaultEncodedString (b64 : T) : string =
    match b64 with
    | DefaultEncoded(Base64DefaultEncoded s) -> s
    | UrlEncoded(Base64UrlEncoded s) ->
      let initial = s.Replace('-', '+').Replace('_', '/')
      let length = initial.Length

      if length % 4 = 2 then $"{initial}=="
      else if length % 4 = 3 then $"{initial}="
      else initial

  /// Encodes to url-safe base64 strings (using '-' and '_'), with no padding
  let urlEncodeToString (input : byte array) : string =
    input |> encode |> asUrlEncodedString

  // Takes an already-encoded base64 string, and decodes it to bytes
  let decode (b64 : T) : byte[] =
    b64 |> asDefaultEncodedString |> System.Convert.FromBase64String

  let decodeFromString (input : string) : byte array = input |> fromEncoded |> decode

  let decodeOpt (b64 : T) : byte[] option =
    try
      b64 |> decode |> Some
    with _ ->
      None

let sha1digest (input : string) : byte[] =
  use sha1 = System.Security.Cryptography.SHA1.Create()
  input |> UTF8.toBytes |> sha1.ComputeHash

let truncateToInt32 (v : int64) : int32 =
  try
    int32 v
  with :? System.OverflowException ->
    if v > 0L then System.Int32.MaxValue else System.Int32.MinValue

let truncateToInt64 (v : bigint) : int64 =
  try
    int64 v
  with :? System.OverflowException ->
    if v > 0I then System.Int64.MaxValue else System.Int64.MinValue

let urlEncodeExcept (keep : string) (s : string) : string =
  let keep = UTF8.toBytes keep |> set
  let encodeByte (b : byte) : byte array =
    // CLEANUP make a nicer version of this that's designed for this use case
    // We do want to escape the following: []+&^%#@"<>/;
    // We don't want to escape the following: *$@!:?,.-_'
    // cut+paste this fn to its usages; Prelude to only have idiomatic version
    if
      (b >= (byte 'a') && b <= (byte 'z'))
      || (b >= (byte '0') && b <= (byte '9'))
      || (b >= (byte 'A') && b <= (byte 'Z'))
      || keep.Contains b
    then
      [| b |]
    else
      UTF8.toBytes ("%" + b.ToString("X2"))
  s |> UTF8.toBytes |> Array.collect encodeByte |> UTF8.ofBytesUnsafe


// urlEncode values in a query string. Note that the encoding is slightly different
// to urlEncoding keys.
// https://secretgeek.net/uri_enconding
let urlEncodeValue (s : string) : string = urlEncodeExcept "*$@!:()~?/.-_='" s

// urlEncode keys in a query string. Note that the encoding is slightly different to
// query string values
let urlEncodeKey (s : string) : string = urlEncodeExcept "*$@!:()~?/.,-_'" s




module Uuid =
  let nilNamespace : System.Guid = System.Guid "00000000-0000-0000-0000-000000000000"

  let uuidV5 (data : string) (nameSpace : System.Guid) : System.Guid =
    Faithlife.Utility.GuidUtility.Create(nameSpace, data, 5)

module Tuple2 =
  let fromKeyValuePair
    (kvp : System.Collections.Generic.KeyValuePair<'a, 'b>)
    : ('a * 'b) =
    kvp.Key, kvp.Value

  let toKeyValuePair
    ((k, v) : 'a * 'b)
    : (System.Collections.Generic.KeyValuePair<'a, 'b>) =
    System.Collections.Generic.KeyValuePair<'a, 'b>(k, v)

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
    // CLEANUP To be compabible to OCAML, keep this at 32 bit for now.
    // This currently keeps us at 30 bits - we'd like to instead use 64, or
    // settle on 63. Current blocker is client (needs research)
    // 0b0000_0000_0000_0000_0000_0000_0000_0000_0011_1111_1111_1111_1111_1111_1111_1111L
    let mask : uint64 = 1073741823UL
    rand64 &&& mask
  with e ->
    Exception.raiseInternal $"gid failed" [ "message", e.Message; "inner", e ]

let randomString (length : int) : string =
  let result =
    Array.init length (fun _ -> char (RNG.GetInt32(int32 'A', int32 'Z')))
    |> System.String

  assertEq "randomString length is correct" result.Length length
  result

// ----------------------
// TODO move elsewhere
// ----------------------
module String =
  // Returns a seq of EGC (extended grapheme cluster - essentially a visible
  // screen character)
  // https://stackoverflow.com/a/4556612/104021
  let toEgcSeq (s : string) : seq<string> =
    seq {
      let tee = System.Globalization.StringInfo.GetTextElementEnumerator(s)

      while tee.MoveNext() do
        yield tee.GetTextElement()
    }

  let splitOnNewline (str : string) : List<string> =
    if str = "" then
      []
    else
      str.Split([| "\n"; "\r\n" |], System.StringSplitOptions.None) |> Array.toList


  let lengthInEgcs (s : string) : int =
    System.Globalization.StringInfo(s).LengthInTextElements

  let normalize (s : string) : string = s.Normalize()

  let equalsCaseInsensitive (s1 : string) (s2 : string) : bool =
    System.String.Equals(s1, s2, System.StringComparison.InvariantCultureIgnoreCase)

  let replace (old : string) (newStr : string) (s : string) : string =
    s.Replace(old, newStr)

  /// Adds 'a' or 'an' (the indefinite article) in front of a string, based on
  /// whether it begins with a vowel
  let articleFor (nextWord : string) : string =
    let vowels = Set [ 'A'; 'E'; 'I'; 'O'; 'U'; 'a'; 'e'; 'i'; 'o'; 'u' ]
    if nextWord = "" then ""
    else if Set.contains nextWord.[0] vowels then "an"
    else "a"

  let toOrdinal (n : int) : string =
    let suffix =
      match n % 10 with
      | 1 -> "st"
      | 2 -> "nd"
      | 3 -> "rd"
      | _ -> "th"

    string n + suffix

  let truncateWithElipsis (maxLen : int) (s : string) : string =
    if s.Length <= maxLen then s else s.Substring(0, maxLen - 3) + "..."

  let isCapitalized (s : string) : bool = s.Length > 0 && System.Char.IsUpper(s.[0])


module Map =
  let mergeFavoringRight (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
    FSharpPlus.Map.union m2 m1

  let mergeFavoringLeft (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
    FSharpPlus.Map.union m1 m2


module HashSet =
  type T<'v> = System.Collections.Generic.HashSet<'v>

  let add (v : 'v) (s : T<'v>) : unit =
    let (_ : bool) = s.Add v
    ()

  let empty () : T<'v> = System.Collections.Generic.HashSet<'v>()

  let toList (d : T<'v>) : List<'v> =
    seq {
      let mutable e = d.GetEnumerator()

      while e.MoveNext() do
        yield e.Current
    }
    |> Seq.toList



module Dictionary =
  type T<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

  let get (k : 'k) (t : T<'k, 'v>) : Option<'v> =
    FSharpPlus.Dictionary.tryGetValue k t

  let containsKey (k : 'k) (t : T<'k, 'v>) : bool = t.ContainsKey k

  let add (k : 'k) (v : 'v) (d : T<'k, 'v>) : unit =
    d[k] <- v
    ()

  let empty () : T<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>()

  let keys = FSharpPlus.Dictionary.keys
  let values = FSharpPlus.Dictionary.values

  let toList (d : T<'k, 'v>) : List<'k * 'v> =
    seq {
      let mutable e = d.GetEnumerator()

      while e.MoveNext() do
        yield (e.Current.Key, e.Current.Value)
    }
    |> Seq.toList

  let fromList (l : List<'k * 'v>) : T<'k, 'v> =
    let result = empty ()
    List.iter (fun (k, v) -> result[k] <- v) l
    result

  let toMap (d : T<'k, 'v>) : Map<'k, 'v> = d |> toList |> Map.ofList

module ResizeArray =
  type T<'v> = ResizeArray<'v>
  let empty () = T()

  let iter (f : 'v -> unit) (l : T<'v>) : unit =
    FSharpx.Collections.ResizeArray.iter f l

  let map (f : 'v -> 'v2) (l : T<'v>) : T<'v2> =
    FSharpx.Collections.ResizeArray.map f l

  let append (v : 'v) (list : T<'v>) : unit = list.Add(v)

  let toList (l : T<'v>) : List<'v> = FSharpx.Collections.ResizeArray.toList l

  let toSeq (l : T<'v>) : seq<'v> = FSharpx.Collections.ResizeArray.toSeq l


// ----------------------
// Lazy utilities
// ----------------------
module Lazy =
  let inline force (l : Lazy<_>) = l.Force()
  let map f l = lazy ((f << force) l)
  let bind f l = lazy ((force << f << force) l)


// ----------------------
// Important types
// ----------------------
type tlid = uint64

type id = uint64


// This is important to prevent auto-serialization accidentally leaking this,
// though it never should anyway
type Password = Password of byte array

// ----------------------
// Json auto-serialization
// ----------------------
module Json =

  let isBaseType (t : System.Type) =
    t = typeof<int>
    || t = typeof<bool>
    || t = typeof<string>
    || t = typeof<float>
    || t = typeof<int64>
    || t = typeof<uint64>
    || t = typeof<NodaTime.Instant>
    || t = typeof<System.Guid>

  module Vanilla =

    open System.Text.Json
    open System.Text.Json.Serialization
    open NodaTime.Serialization.SystemTextJson

    type Int64Converter() =
      // We serialize int64s as valid JSON numbers for as long as we're allowed, and
      // then we switch to strings. Since the deserialization is type-directed, we
      // always know we're looking to convert them to int64s, so if we see a string
      // we know exactly what it means
      inherit JsonConverter<int64>()

      override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
        if reader.TokenType = JsonTokenType.String then
          let str = reader.GetString()
          parseInt64 str
        else
          reader.GetInt64()

      override _.Write(writer : Utf8JsonWriter, value : int64, _options) =
        // largest int64 for which all smaller ints can be represented in a double
        // without losing precision
        if value > 9007199254740992L then
          writer.WriteStringValue(value.ToString())
        else if value < -9007199254740992L then
          writer.WriteStringValue(value.ToString())
        else
          writer.WriteNumberValue(value)

    type UInt64Converter() =
      // We serialize uint64s as valid JSON numbers for as long as we're allowed, and
      // then we switch to strings. Since the deserialization is type-directed, we
      // always know we're looking to convert them to uint64s, so if we see a string
      // we know exactly what it means
      inherit JsonConverter<uint64>()

      override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
        if reader.TokenType = JsonTokenType.String then
          let str = reader.GetString()
          parseUInt64 str
        else
          reader.GetUInt64()

      override _.Write(writer : Utf8JsonWriter, value : uint64, _options) =
        // largest uint64 for which all smaller ints can be represented in a double
        // without losing precision
        if value > 9007199254740992UL then
          writer.WriteStringValue(value.ToString())
        else
          writer.WriteNumberValue(value)

    type PasswordConverter() =
      inherit JsonConverter<Password>()

      override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
        reader.GetString() |> UTF8.toBytes |> Password

      override _.Write(writer : Utf8JsonWriter, _ : Password, _options) =
        writer.WriteStringValue("Redacted")

    type NonEmptyListValueConverter<'TValue>() =
      inherit JsonConverter<FSharpPlus.Data.NonEmptyList<'TValue>>()

      override this.Read
        (
          reader : byref<Utf8JsonReader>,
          typeToConvert : System.Type,
          options : JsonSerializerOptions
        ) =
        JsonSerializer.Deserialize<'TValue seq>(&reader, options)
        |> Seq.toList
        |> FSharpPlus.Data.NonEmptyList.ofList


      override this.Write
        (
          writer : Utf8JsonWriter,
          value : FSharpPlus.Data.NonEmptyList<'TValue>,
          options : JsonSerializerOptions
        ) =
        let value = FSharpPlus.Data.NonEmptyList.toList value
        JsonSerializer.Serialize(writer, (List.toSeq value), options)


    type NonEmptyListConverter() =
      inherit JsonConverterFactory()
      override this.CanConvert(typeToConvert : System.Type) : bool =
        typeToConvert.IsGenericType
        && List.contains
          (typeToConvert.GetGenericTypeDefinition())
          [ typedefof<FSharpPlus.Data.NonEmptyList<_>>
            typedefof<System.Collections.Generic.IReadOnlyCollection<_>> ]

      override this.CreateConverter
        (
          typeToConvert : System.Type,
          _options : JsonSerializerOptions
        ) : JsonConverter =
        let typArgs = typeToConvert.GetGenericArguments()
        let converterType =
          typedefof<NonEmptyListValueConverter<_>>.MakeGenericType(typArgs)
        System.Activator.CreateInstance(converterType) :?> JsonConverter


    // Since we're getting this back from OCaml in DDates, we need to use the
    // timezone even though there isn't one in the type
    type LocalDateTimeConverter() =
      inherit JsonConverter<NodaTime.LocalDateTime>()

      override _.Read(reader : byref<Utf8JsonReader>, tipe, options) =
        let rawToken = reader.GetString()
        try
          (NodaTime.Instant.ofIsoString rawToken).toUtcLocalTimeZone ()
        with
        // We briefly used this converter for `Vanilla` - this is us "falling
        // back" so we're able to read values serialized during that time.
        | _ ->
          NodaConverters.LocalDateTimeConverter.Read(&reader, tipe, options)

      override _.Write
        (
          writer : Utf8JsonWriter,
          value : NodaTime.LocalDateTime,
          _options
        ) =
        let value =
          NodaTime
            .ZonedDateTime(value, NodaTime.DateTimeZone.Utc, NodaTime.Offset.Zero)
            .ToInstant()
            .toIsoString ()
        writer.WriteStringValue(value)

    type RawBytesConverter() =
      inherit JsonConverter<byte array>()
      // In OCaml, we wrap the in DBytes with a RawBytes, whose serializer uses
      // the url-safe version of base64. It's not appropriate for all byte
      // arrays, but I think this is the only user. If not, we'll need to add a
      // RawBytes type.
      override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
        reader.GetString() |> Base64.fromUrlEncoded |> Base64.decode

      override _.Write(writer : Utf8JsonWriter, value : byte[], _) =
        value |> Base64.urlEncodeToString |> writer.WriteStringValue


    // This is used for "normal" JSON conversion, such as converting Pos into
    // json. It does not feature anything for conversion to OCaml-compatible
    // stuff, such as may be required to communicate with the fuzzer or the
    // frontend. It does handle F#-specific constructs, and prevents exposing
    // passwords (just in case).

    let getDefaultOptions () =
      let fsharpConverter =
        JsonFSharpConverter(unionEncoding = (JsonUnionEncoding.ExternalTag))
      let options = JsonSerializerOptions()
      options.MaxDepth <- System.Int32.MaxValue // infinite
      options.NumberHandling <- JsonNumberHandling.AllowNamedFloatingPointLiterals
      options.Converters.Add(NodaConverters.InstantConverter)
      options.Converters.Add(LocalDateTimeConverter())
      options.Converters.Add(UInt64Converter())
      options.Converters.Add(Int64Converter())
      options.Converters.Add(PasswordConverter())
      options.Converters.Add(RawBytesConverter())
      options.Converters.Add(NonEmptyListConverter())
      options.Converters.Add(fsharpConverter)

      options

    let _options = getDefaultOptions ()

    let _prettyOptions =
      let options = getDefaultOptions ()
      options.WriteIndented <- true
      options

    let _deserializeWithCommentsOptions =
      let options = getDefaultOptions ()
      options.ReadCommentHandling <- JsonCommentHandling.Skip
      options

    let registerConverter (c : JsonConverter<'a>) =
      // insert in the front as the formatter will use the first converter that
      // supports the type, not the best one
      _options.Converters.Insert(0, c)
      _prettyOptions.Converters.Insert(0, c)
      _deserializeWithCommentsOptions.Converters.Insert(0, c)

    // ----------------------
    // Tracking serializers
    // ----------------------

    // Serializers are easy to be unsure about. We use the Vanilla serializer a lot
    // for random types, and we would like to be sure that we don't accidentally
    // change the format of those types by adding new converters or whatever. Our
    // solution this is:

    // 1) track all types that are serialized and also how they are serialized
    // 2) store sample input/output for each type
    // 3) During testing, assert stored sample output has not changed
    // 4) warn when serializing using a type that is not allowed/tested serializer.

    // We track the types by explicitly calling the `allow<type>` function.  This
    // adds the type to a dictionary: at test-time we check all members of the
    // dictionary are accounted for. At runtime we warn if we're attempting to serialize
    // a type that hasn't been allowed. During development, we upgrade that warning
    // to an exception.
    //
    // See also Serializers.Tests.fs and [/docs/serialization.md]

    let mutable allowedTypes = Dictionary.T<string, string>()

    let rec isSerializable (t : System.Type) : bool =
      if isBaseType t then true else allowedTypes.ContainsKey(string t)

    let allow<'a> (reason : string) : unit =
      try
        let key = string typeof<'a>
        let reason =
          if allowedTypes.ContainsKey key then
            $"{reason}+{allowedTypes[key]}"
          else
            reason
        allowedTypes[key] <- reason
        ()
      with _ ->
        System.Console.Write("error allowing Vanilla type")

    let assertSerializable (t : System.Type) : unit =
      if not (isSerializable t) then
        sendRollbarError
          "Invalid serialization call to type not allowed: add `Json.Vanilla.allow<type>()` to allow it to be serialized"
          [ "type", string t ]


    let serialize (data : 'a) : string =
      assertSerializable typeof<'a>
      JsonSerializer.Serialize(data, _options)

    let serializeToStream (stream : System.IO.Stream, data : 'a) : Task<unit> =
      task {
        assertSerializable typeof<'a>
        return! JsonSerializer.SerializeAsync(stream, data, _options)
      }

    let deserialize<'a> (json : string) : 'a =
      assertSerializable typeof<'a>
      JsonSerializer.Deserialize<'a>(json, _options)

    let deserializeWithComments<'a> (json : string) : 'a =
      assertSerializable typeof<'a>
      JsonSerializer.Deserialize<'a>(json, _deserializeWithCommentsOptions)

    let prettySerialize (data : 'a) : string =
      assertSerializable typeof<'a>
      JsonSerializer.Serialize(data, _prettyOptions)



// ----------------------
// Functions we'll later add to Tablecloth
// ----------------------
module Tablecloth =
  module Result =
    let unwrapWith (f : 'err -> 'ok) (t : Result<'ok, 'err>) : 'ok =
      match t with
      | Ok v -> v
      | Error v -> f v

    [<CompilerMessageAttribute("Result.unwrapUnsafe is banned, use Prelude.Exception.unwrapResult* instead",
                               0,
                               IsError = true,
                               IsHidden = true)>]
    let unwrapUnsafe = Tablecloth.Result.unwrapUnsafe

  module Option =

    [<CompilerMessageAttribute("Option.unwrapUnsafe is banned, use Prelude.Exception.unwrapOption* instead",
                               0,
                               IsError = true,
                               IsHidden = true)>]
    let unwrapUnsafe = Tablecloth.Option.unwrapUnsafe

    [<CompilerMessageAttribute("Option.get is banned, use Prelude.Exception.unwrapOption* instead",
                               0,
                               IsError = true,
                               IsHidden = true)>]
    let get = Option.get

  module String =
    let take (count : int) (str : string) : string =
      if count >= str.Length then str else str.Substring(0, count)

    let removeSuffix (suffix : string) (str : string) : string =
      if str.EndsWith(suffix) then
        str.Substring(0, str.Length - suffix.Length)
      else
        str


  module Map =
    let fromListBy (f : 'v -> 'k) (l : List<'v>) : Map<'k, 'v> =
      List.fold (fun (m : Map<'k, 'v>) v -> m.Add(f v, v)) Map.empty l

type Ply<'a> = Ply.Ply<'a>
let uply = FSharp.Control.Tasks.Affine.Unsafe.uply

module Ply =
  let map (f : 'a -> 'b) (v : Ply<'a>) : Ply<'b> =
    uply {
      let! v = v
      return f v
    }

  let bind (f : 'a -> Ply<'b>) (v : Ply<'a>) : Ply<'b> =
    uply {
      let! v = v
      return! f v
    }

  let toTask (v : Ply<'a>) : Task<'a> = Ply.TplPrimitives.runPlyAsTask v


  // These functions are sequential versions of List/Map functions like map/iter/etc.
  // They await each list item before they process the next.  This ensures each
  // request in the list is processed to completion before the next one is done,
  // making sure that, for example, a HttpClient call will finish before the next one
  // starts. Will allow other requests to run which waiting.

  module List =
    let flatten (list : List<Ply<'a>>) : Ply<List<'a>> =
      let rec loop (acc : Ply<List<'a>>) (xs : List<Ply<'a>>) =
        uply {
          let! acc = acc

          match xs with
          | [] -> return List.rev acc
          | x :: xs ->
            let! x = x
            return! loop (uply { return (x :: acc) }) xs
        }

      loop (uply { return [] }) list

    let foldSequentially
      (f : 'state -> 'a -> Ply<'state>)
      (initial : 'state)
      (list : List<'a>)
      : Ply<'state> =
      List.fold
        (fun (accum : Ply<'state>) (arg : 'a) ->
          uply {
            let! accum = accum
            return! f accum arg
          })
        (Ply initial)
        list

    let foldSequentiallyWithIndex
      (f : int -> 'state -> 'a -> Ply<'state>)
      (initial : 'state)
      (list : List<'a>)
      : Ply<'state> =
      List.fold
        (fun (accum : (Ply<int * 'state>)) (arg : 'a) ->
          uply {
            let! (i, state) = accum
            let! result = f i state arg
            return (i + 1, result)
          })
        (Ply((0, initial)))
        list
      |> map Tablecloth.Tuple2.second



    let mapSequentially (f : 'a -> Ply<'b>) (list : List<'a>) : Ply<List<'b>> =
      list
      |> foldSequentially
        (fun (accum : List<'b>) (arg : 'a) ->
          uply {
            let! result = f arg
            return result :: accum
          })
        []
      |> map List.rev

    let filterSequentially (f : 'a -> Ply<bool>) (list : List<'a>) : Ply<List<'a>> =
      uply {
        let! filtered =
          List.fold
            (fun (accum : Ply<List<'a>>) (arg : 'a) ->
              uply {
                let! (accum : List<'a>) = accum
                let! keep = f arg
                return (if keep then (arg :: accum) else accum)
              })
            (Ply [])
            list

        return List.rev filtered
      }

    let iterSequentially (f : 'a -> Ply<unit>) (list : List<'a>) : Ply<unit> =
      List.fold
        (fun (accum : Ply<unit>) (arg : 'a) ->
          uply {
            do! accum // resolve the previous task before doing this one
            return! f arg
          })
        (Ply(()))
        list

    let findSequentially (f : 'a -> Ply<bool>) (list : List<'a>) : Ply<Option<'a>> =
      List.fold
        (fun (accum : Ply<Option<'a>>) (arg : 'a) ->
          uply {
            match! accum with
            | Some v -> return Some v
            | None ->
              let! result = f arg
              return (if result then Some arg else None)
          })
        (Ply None)
        list

    let filterMapSequentially
      (f : 'a -> Ply<Option<'b>>)
      (list : List<'a>)
      : Ply<List<'b>> =
      uply {
        let! filtered =
          List.fold
            (fun (accum : Ply<List<'b>>) (arg : 'a) ->
              uply {
                let! (accum : List<'b>) = accum
                let! keep = f arg

                let result =
                  match keep with
                  | Some v -> v :: accum
                  | None -> accum

                return result
              })
            (Ply [])
            list

        return List.rev filtered
      }



  module Map =
    let foldSequentially
      (f : 'state -> 'key -> 'a -> Ply<'state>)
      (initial : 'state)
      (dict : Map<'key, 'a>)
      : Ply<'state> =
      Map.fold
        (fun (accum : Ply<'state>) (key : 'key) (arg : 'a) ->
          uply {
            let! (accum : 'state) = accum
            return! f accum key arg
          })
        (Ply(initial))
        dict

    let mapSequentially
      (f : 'a -> Ply<'b>)
      (dict : Map<'key, 'a>)
      : Ply<Map<'key, 'b>> =
      foldSequentially
        (fun (accum : Map<'key, 'b>) (key : 'key) (arg : 'a) ->
          uply {
            let! result = f arg
            return Map.add key result accum
          })
        Map.empty
        dict

    let filterSequentially
      (f : 'key -> 'a -> Ply<bool>)
      (dict : Map<'key, 'a>)
      : Ply<Map<'key, 'a>> =
      foldSequentially
        (fun (accum : Map<'key, 'a>) (key : 'key) (arg : 'a) ->
          uply {
            let! keep = f key arg
            return (if keep then (Map.add key arg accum) else accum)
          })
        Map.empty
        dict

    let filterMapSequentially
      (f : 'key -> 'a -> Ply<Option<'b>>)
      (dict : Map<'key, 'a>)
      : Ply<Map<'key, 'b>> =
      foldSequentially
        (fun (accum : Map<'key, 'b>) (key : 'key) (arg : 'a) ->
          uply {
            let! keep = f key arg

            let result =
              match keep with
              | Some v -> Map.add key v accum
              | None -> accum

            return result
          })
        Map.empty
        dict


// ----------------------
// Task utility functions
// ----------------------
module Task =

  let map (f : 'a -> 'b) (v : Task<'a>) : Task<'b> =
    task {
      let! v = v
      return f v
    }

  let bind (f : 'a -> Task<'b>) (v : Task<'a>) : Task<'b> =
    task {
      let! v = v
      return! f v
    }



  let flatten (list : List<Task<'a>>) : Task<List<'a>> =
    Task.WhenAll list |> map Array.toList

  let foldSequentially
    (f : 'state -> 'a -> Task<'state>)
    (initial : 'state)
    (list : List<'a>)
    : Task<'state> =
    List.fold
      (fun (accum : Task<'state>) (arg : 'a) ->
        task {
          let! accum = accum
          return! f accum arg
        })
      (Task.FromResult initial)
      list

  let mapSequentially (f : 'a -> Task<'b>) (list : List<'a>) : Task<List<'b>> =

    list
    |> foldSequentially
      (fun (accum : List<'b>) (arg : 'a) ->
        task {
          let! result = f arg
          return result :: accum
        })
      []
    |> map List.rev

  let mapInParallel (f : 'a -> Task<'b>) (list : List<'a>) : Task<List<'b>> =
    List.map f list |> flatten

  /// Call [f v], after claiming the passed semaphore. Releases the semaphore when done
  let execWithSemaphore
    (semaphore : System.Threading.SemaphoreSlim)
    (f : 'a -> Task<'b>)
    (v : 'a)
    : Task<'b> =
    task {
      try
        do! semaphore.WaitAsync()
        return! f v
      finally
        semaphore.Release() |> ignore<int>
    }

  let mapWithConcurrency
    (concurrencyCount : int)
    (f : 'a -> Task<'b>)
    (list : List<'a>)
    : Task<List<'b>> =
    let semaphore = new System.Threading.SemaphoreSlim(concurrencyCount)
    let f = execWithSemaphore semaphore f
    List.map f list |> flatten

  let iterInParallel (f : 'a -> Task<unit>) (list : List<'a>) : Task<unit> =
    task {
      let! (_completedTasks : unit[]) = List.map f list |> Task.WhenAll
      return ()
    }

  let iterWithConcurrency
    (concurrencyCount : int)
    (f : 'a -> Task<unit>)
    (list : List<'a>)
    : Task<unit> =
    let semaphore = new System.Threading.SemaphoreSlim(concurrencyCount)
    let f = execWithSemaphore semaphore f
    task {
      let! (_completedTasks : unit[]) = List.map f list |> Task.WhenAll
      return ()
    }

  let filterSequentially (f : 'a -> Task<bool>) (list : List<'a>) : Task<List<'a>> =
    task {
      let! filtered =
        List.fold
          (fun (accum : Task<List<'a>>) (arg : 'a) ->
            task {
              let! (accum : List<'a>) = accum
              let! keep = f arg
              return (if keep then (arg :: accum) else accum)
            })
          (Task.FromResult [])
          list

      return List.rev filtered
    }

  let iterSequentially (f : 'a -> Task<unit>) (list : List<'a>) : Task<unit> =
    List.fold
      (fun (accum : Task<unit>) (arg : 'a) ->
        task {
          do! accum // resolve the previous task before doing this one
          return! f arg
        })
      (Task.FromResult())
      list

  let findSequentially (f : 'a -> Task<bool>) (list : List<'a>) : Task<Option<'a>> =
    List.fold
      (fun (accum : Task<Option<'a>>) (arg : 'a) ->
        task {
          match! accum with
          | Some v -> return Some v
          | None ->
            let! result = f arg
            return (if result then Some arg else None)
        })
      (Task.FromResult None)
      list

  let filterMapSequentially
    (f : 'a -> Task<Option<'b>>)
    (list : List<'a>)
    : Task<List<'b>> =
    task {
      let! filtered =
        List.fold
          (fun (accum : Task<List<'b>>) (arg : 'a) ->
            task {
              let! (accum : List<'b>) = accum
              let! keep = f arg

              let result =
                match keep with
                | Some v -> v :: accum
                | None -> accum

              return result
            })
          (Task.FromResult [])
          list

      return List.rev filtered
    }



// ----------------------
// Shared Types
// ----------------------
// Some fundamental types that we want to use everywhere.

// DO NOT define any serialization on these types. If you want to serialize
// them, you should move these to the files with specific formats and serialize
// them there.

type NonEmptyList<'a> = FSharpPlus.Data.NonEmptyList<'a>

module NonEmptyList =
  // Need to list these out explicitly because F# doesn't support opening modules
  // like this
  let toList = FSharpPlus.Data.NonEmptyList.toList

  let iter (f : 'a -> unit) (l : NonEmptyList<'a>) : unit =
    l |> toList |> List.iter f

  let map = FSharpPlus.Data.NonEmptyList.map
  let ofList = FSharpPlus.Data.NonEmptyList.ofList
  let ofSeq = FSharpPlus.Data.NonEmptyList.ofSeq
  let singleton = FSharpPlus.Data.NonEmptyList.singleton


type CanvasID = System.Guid
type UserID = System.Guid

/// User to represent handlers in their lowest-level form: a triple of space * name * modifier
/// "Space" is "HTTP", "WORKER", "REPL", etc.
///
/// "Modifier" options differ based on space.
/// e.g. HTTP handler may have "GET" modifier.
///
/// Handlers which don't have modifiers (e.g. repl, worker) nearly
/// always (but not actually always) have `_` as their modifier.
type HandlerDesc = (string * string * string)

module HttpHeaders =
  type AspHeaders = System.Net.Http.Headers.HttpHeaders
  type HttpResponseMessage = System.Net.Http.HttpResponseMessage

  // We include these here as the _most_ basic http header types and functionality.
  // Anything even remotely more complicated should be put next to where it's used,
  // as historically we've gotten a lot wrong here and needed to make changes that we
  // couldn't make if the functionality was here.
  type Header = string * string
  type T = List<Header>

  let get (headerName) (headers : T) : string option =
    headers
    |> List.tryFind (fun ((k : string), (_ : string)) ->
      String.equalsCaseInsensitive headerName k)
    |> Option.map (fun (_k, v) -> v)

  /// Get Dark-style headers from an Asp.Net HttpResponseMessage
  let headersForAspNetResponse (response : HttpResponseMessage) : T =
    let fromAspNetHeaders (headers : AspHeaders) : T =
      headers
      |> Seq.map Tuple2.fromKeyValuePair
      |> Seq.map (fun (k, v) -> (k, v |> Seq.toList |> String.concat ","))
      |> Seq.toList
    fromAspNetHeaders response.Headers @ fromAspNetHeaders response.Content.Headers

let id (x : int) : id = uint64 x

// since we hide F#'s default 'id' fn just above
let identity a = a
