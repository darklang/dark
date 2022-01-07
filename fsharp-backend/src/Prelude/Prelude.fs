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

module Result =

  [<CompilerMessageAttribute("Result.unwrapUnsafe is banned, use Prelude.Exception.unwrapOption* instead",
                             0,
                             IsError = true,
                             IsHidden = true)>]
  let unwrapUnsafe = Tablecloth.Option.unwrapUnsafe


// ----------------------
// Null
// ----------------------
// https://stackoverflow.com/a/11696947/104021
let inline isNull (x : ^T when ^T : not struct) = obj.ReferenceEquals(x, null)

// ----------------------
// Exceptions
// ----------------------

// Exceptions indicate who is responsible for a problem, and include messages which
// may be shown to users
type DarkExceptionData =
  // Do not show to anyone, we need to rollbar this and address it
  | InternalError of string * List<string * obj>

  // An error caused by the grand user making the request, show the error to the
  // requester no matter who they are
  | GrandUserError of string

  // An error caused by how the developer wrote the code, such as calling a function
  // with the wrong type
  | DeveloperError of string

  // An error caused by the editor doing something it shouldn't, so as an Redo or
  // rename that isn't allowed. The editor should have caught this on the client and
  // not made the request.
  | EditorError of string

  // An error in library or framework code, such as calling a function with a negative
  // number when it doesn't support it. We probably caused this by allowing it to
  // happen, so we definitely want to fix it, but it's OK to tell the developer what
  // happened (not grandusers though)
  | LibraryError of string * List<string * obj>

exception DarkException of data : DarkExceptionData with
  override this.Message : string =
    match this.data with
    | InternalError (msg, _) -> msg
    | DeveloperError msg -> msg
    | EditorError msg -> msg
    | LibraryError (msg, _) -> msg
    | GrandUserError msg -> msg


exception PageableException of inner : System.Exception with
  override this.Message = this.inner.Message

// This is for tracing
let mutable exceptionCallback =
  (fun (e : exn) (typ : string) (msg : string) (tags : List<string * obj>) -> ())

module Exception =

  let toMetadata (e : DarkException) : List<string * obj> =
    match e.data with
    | InternalError (_, md)
    | LibraryError (_, md) -> md
    | DeveloperError _
    | EditorError _
    | GrandUserError _ -> []



  let callExceptionCallback e typ msg tags =
    try
      exceptionCallback e typ msg tags
    with
    | e ->
      // We're completely screwed at this point
      System.Console.WriteLine "Exception calling Exceptioncallback"
      System.Console.WriteLine e.Message
      System.Console.WriteLine e.StackTrace


  // A grand user exception was caused by the incorrect actions of a grand user. The
  // msg is suitable to show to the grand user. We don't care about grandUser
  // exceptions, they're normal.
  let raiseGrandUser (msg : string) =
    let e = DarkException(GrandUserError(msg))
    callExceptionCallback e "grand" msg []
    raise e

  // A developer exception is one caused by the incorrect actions of our
  // user/developer. The msg is suitable to show to the user.
  let raiseDeveloper (msg : string) =
    let e = DarkException(DeveloperError(msg))
    callExceptionCallback e "developer" msg []
    raise e

  let unwrapResultDeveloper (r : Result<'ok, string>) : 'ok =
    match r with
    | Ok v -> v
    | Error msg -> raiseDeveloper msg

  // An editor exception is one which is caused by an invalid action on the part of
  // the Dark editor. We are interested in these. The message may be shown to the
  // logged-in user, and should be suitable for this.
  let raiseEditor (msg : string) =
    let e = DarkException(EditorError(msg))
    callExceptionCallback e "editor" msg []
    raise e

  // An internal error. Should be rollbarred, and should not be shown to users.
  let raiseInternal (msg : string) (tags : List<string * obj>) =
    let e = DarkException(InternalError(msg, tags))
    callExceptionCallback e "internal" msg tags
    raise e

  let unwrapOptionInternal
    (msg : string)
    (tags : List<string * obj>)
    (o : Option<'a>)
    : 'a =
    match o with
    | Some v -> v
    | None -> raiseInternal msg tags

  let unwrapResultInternal (tags : List<string * obj>) (r : Result<'a, 'msg>) : 'a =
    match r with
    | Ok v -> v
    | Error msg -> raiseInternal (string msg) tags

  let reraiseAsPageable (e : exn) = raise (PageableException e)



  // An error in library code - should not be shown to grand users. May be shown to
  // logged-in developers (most typically in a DError or a Result).
  let raiseLibrary (msg : string) (tags : List<string * obj>) =
    let e = DarkException(LibraryError(msg, tags))
    callExceptionCallback e "library" msg tags
    raise e

  let unknownErrorMessage = "Unknown error"

  let toGrandUserMessage (e : exn) : string =
    match e with
    | DarkException (InternalError _)
    | DarkException (DeveloperError _)
    | DarkException (LibraryError _)
    | DarkException (EditorError _) -> unknownErrorMessage
    | DarkException (GrandUserError msg) -> msg
    | _ -> unknownErrorMessage

  let toDeveloperMessage (e : exn) : string =
    match e with
    | DarkException (InternalError _) -> unknownErrorMessage
    | DarkException (DeveloperError msg)
    | DarkException (LibraryError (msg, _))
    | DarkException (EditorError msg)
    | DarkException (GrandUserError msg) -> msg
    | _ -> unknownErrorMessage


  let taskCatch (f : unit -> Task<'r>) : Task<Option<'r>> =
    task {
      try
        let! result = f ()
        return Some result
      with
      | _ -> return None
    }

  let catch (f : unit -> 'r) : Option<'r> =
    try
      Some(f ())
    with
    | _ -> None




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
let isBlazor = System.OperatingSystem.IsBrowser()

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
  static do
    let f () =
      while true do
        try
          let v = mQueue.Take()
          System.Console.WriteLine(v)
        with
        | e ->
          System.Console.WriteLine(
            $"Exception in blocking queue thread: {e.Message}"
          )
    // Background threads aren't supported in Blazor
    if not isBlazor then
      let thread = System.Threading.Thread(f)
      do
        thread.IsBackground <- true
        thread.Name <- "Prelude.NonBlockingConsole printer"
        thread.Start()


  static member WriteLine(value : string) : unit =
    if isBlazor then System.Console.WriteLine value else mQueue.Add(value)


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

let debugByteArray (msg : string) (a : byte array) : byte array =
  let bytes = a |> System.BitConverter.ToString
  NonBlockingConsole.WriteLine $"DEBUG: {msg} (len {a.Length}, {bytes}"
  a

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



// ----------------------
// Assertions
// ----------------------
// Asserts are problematic because they don't run in prod, and if they did they
// wouldn't be caught by the webserver

let assert_ (msg : string) (cond : bool) : unit =
  if cond then () else Exception.raiseInternal $"Assertion failure: {msg}" []

let assertEq (msg : string) (expected : 'a) (actual : 'a) : unit =
  if expected <> actual then
    Exception.raiseInternal
      $"Assertion equality failure: {msg}"
      [ "expected", expected :> obj; "actual", actual :> obj ]

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

let parseInt64 (str : string) : int64 =
  try
    assertRe "int64" @"-?\d+" str
    System.Convert.ToInt64 str
  with
  | e -> Exception.raiseInternal $"parseInt64 failed" [ "str", str; "inner", e ]

let parseUInt64 (str : string) : uint64 =
  try
    assertRe "uint64" @"-?\d+" str
    System.Convert.ToUInt64 str
  with
  | e -> Exception.raiseInternal $"parseUInt64 failed" [ "str", str; "inner", e ]

let parseBigint (str : string) : bigint =
  try
    assertRe "bigint" @"-?\d+" str
    System.Numerics.BigInteger.Parse str
  with
  | e -> Exception.raiseInternal $"parseBigint failed" [ "str", str; "inner", e ]

let parseFloat (whole : string) (fraction : string) : float =
  try
    assertRe "whole" @"-?\d+" whole
    assertRe "fraction" @"\d+" fraction
    System.Double.Parse($"{whole}.{fraction}")
  with
  | e ->
    Exception.raiseInternal
      $"parseFloat failed"
      [ "whole", whole; "fraction", fraction; "inner", e ]

// We use an explicit sign for Floats, instead of making it implicit in the
// first digit, because otherwise we lose the sign on 0, and can't represent
// things like -0.5
type Sign =
  | Positive
  | Negative

// Given a float, read it correctly into two ints: whole number and fraction
let readFloat (f : float) : (Sign * bigint * bigint) =
  let sign =
    // (0.0 = -0.0) is true in .Net, so it can be quite tough to figure out the sign
    if string f = "-0" then Negative
    else if f >= 0.0 then Positive
    else Negative
  let asStr = f.ToString("G53").Split "."

  if asStr.Length = 1 then
    sign, abs (parseBigint asStr[0]), 0I
  else
    sign, abs (parseBigint asStr[0]), parseBigint asStr[1]


let makeFloat (sign : Sign) (whole : bigint) (fraction : bigint) : float =
  try
    assert_ "makefloat" (whole >= 0I)
    let sign =
      match sign with
      | Positive -> ""
      | Negative -> "-"
    $"{sign}{whole}.{fraction}" |> System.Double.Parse
  with
  | e ->
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
    with
    | e -> None

  let ofBytesOpt (input : byte array) : string option =
    try
      Some(ofBytesUnsafe input)
    with
    | e -> None


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
      | UrlEncoded (Base64UrlEncoded s) -> s
      | DefaultEncoded (Base64DefaultEncoded s) -> s

  // Convert to string with default base64 encoding
  let defaultEncodeToString (input : byte array) : string =
    System.Convert.ToBase64String input

  // Convert to base64 string
  let encode (input : byte array) : T =
    input |> defaultEncodeToString |> Base64DefaultEncoded |> DefaultEncoded

  // Convert to string with url-flavored base64 encoding



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
    | UrlEncoded (Base64UrlEncoded s) -> s
    | DefaultEncoded (Base64DefaultEncoded s) ->
      s.Replace('+', '-').Replace('/', '_').Replace("=", "")

  let asDefaultEncodedString (b64 : T) : string =
    match b64 with
    | DefaultEncoded (Base64DefaultEncoded s) -> s
    | UrlEncoded (Base64UrlEncoded s) ->
      let initial = s.Replace('-', '+').Replace('_', '/')
      let length = initial.Length

      if length % 4 = 2 then $"{initial}=="
      else if length % 4 = 3 then $"{initial}="
      else initial

  let urlEncodeToString (input : byte array) : string =
    input |> encode |> asUrlEncodedString

  // Takes an already-encoded base64 string, and decodes it to bytes
  let decode (b64 : T) : byte [] =
    b64 |> asDefaultEncodedString |> System.Convert.FromBase64String

  let decodeFromString (input : string) : byte array = input |> fromEncoded |> decode

  let decodeOpt (b64 : T) : byte [] option =
    try
      b64 |> decode |> Some
    with
    | _ -> None

let sha1digest (input : string) : byte [] =
  use sha1 = System.Security.Cryptography.SHA1.Create()
  input |> UTF8.toBytes |> sha1.ComputeHash

let truncateToInt32 (v : int64) : int32 =
  try
    int32 v
  with
  | :? System.OverflowException ->
    if v > 0L then System.Int32.MaxValue else System.Int32.MinValue

let truncateToInt64 (v : bigint) : int64 =
  try
    int64 v
  with
  | :? System.OverflowException ->
    if v > 0I then System.Int64.MaxValue else System.Int64.MinValue

let urlEncodeExcept (keep : string) (s : string) : string =
  let keep = UTF8.toBytes keep |> set
  let encodeByte (b : byte) : byte array =
    // CLEANUP make a nicer version of this that's designed for this use case
    // We do want to escape the following: []+&^%#@"<>/;
    // We don't want to escape the following: *$@!:?,.-_'
    if (b >= (byte 'a') && b <= (byte 'z'))
       || (b >= (byte '0') && b <= (byte '9'))
       || (b >= (byte 'A') && b <= (byte 'Z'))
       || keep.Contains b then
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
    Faithlife.Utility.GuidUtility.Create(nilNamespace, data, 5)

module Tuple2 =
  let fromKeyValuePair
    (kvp : System.Collections.Generic.KeyValuePair<'a, 'b>)
    : ('a * 'b) =
    kvp.Key, kvp.Value

  let toKeyValuePair
    ((k, v) : 'a * 'b)
    : (System.Collections.Generic.KeyValuePair<'a, 'b>) =
    System.Collections.Generic.KeyValuePair<'a, 'b>(k, v)


type System.DateTime with

  member this.toIsoString() : string =
    this.ToString("s", System.Globalization.CultureInfo.InvariantCulture) + "Z"

  static member ofIsoString(str : string) : System.DateTime =
    System.DateTime.Parse(str, System.Globalization.CultureInfo.InvariantCulture)

// ----------------------
// Random numbers
// ----------------------

// .NET's System.Random is a PRNG, and on .NET Core, this is seeded from an
// OS-generated truly-random number.
// https://github.com/dotnet/runtime/issues/23198#issuecomment-668263511 We
// also use a single global value for the VM, so that users cannot be
// guaranteed to get multiple consequetive values (as other requests may intervene)
let gid () : uint64 =
  try
    let rand64 : uint64 = uint64 (System.Random.Shared.NextInt64())
    // Keep 30 bits
    // 0b0000_0000_0000_0000_0000_0000_0000_0000_0011_1111_1111_1111_1111_1111_1111_1111L
    let mask : uint64 = 1073741823UL
    rand64 &&& mask
  with
  | e -> Exception.raiseInternal $"gid failed" [ "message", e.Message; "inner", e ]

let randomString (length : int) : string =
  let result =
    Array.init length (fun _ -> char (System.Random.Shared.Next(0x41, 0x5a)))
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
    str.Split([| "\n"; "\r\n" |], System.StringSplitOptions.None) |> Array.toList


  let lengthInEgcs (s : string) : int =
    System.Globalization.StringInfo(s).LengthInTextElements

  let normalize (s : string) : string = s.Normalize()

  let equalsCaseInsensitive (s1 : string) (s2 : string) : bool =
    System.String.Equals(s1, s2, System.StringComparison.InvariantCultureIgnoreCase)

  let replace (old : string) (newStr : string) (s : string) : string =
    s.Replace(old, newStr)


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

  let add (k : 'k) (v : 'v) (d : T<'k, 'v>) : T<'k, 'v> =
    d[k] <- v
    d

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

// In real code, only create these from LibService.Telemetry
type ExecutionID =
  | ExecutionID of string

  override this.ToString() : string =
    let (ExecutionID str) = this
    str




// This is important to prevent auto-serialization accidentally leaking this,
// though it never should anyway
type Password = Password of byte array

// ----------------------
// Json auto-serialization
// ----------------------
module Json =
  module AutoSerialize =
    open Newtonsoft.Json
    open Microsoft.FSharp.Reflection

    type FSharpDuConverter() =
      inherit JsonConverter()

      override _.WriteJson(writer, value, serializer) =
        let unionType = value.GetType()
        let case, fields = FSharpValue.GetUnionFields(value, unionType)
        writer.WriteStartArray()
        writer.WriteValue case.Name
        Array.iter (fun field -> serializer.Serialize(writer, field)) fields
        writer.WriteEndArray()

      override _.ReadJson(reader, destinationType, _, serializer : JsonSerializer) =
        match reader.TokenType with
        | JsonToken.StartArray -> ()
        | _ ->
          Exception.raiseInternal
            "Incorrect starting token for union, should be array"
            [ "tokenType", reader.TokenType; "destinationType", destinationType ]


        let caseName : string =
          reader.Read() |> ignore<bool>
          reader.Value :?> string

        let caseInfo =
          FSharpType.GetUnionCases(destinationType)
          |> Array.find (fun f -> f.Name = caseName)

        let fields : System.Reflection.PropertyInfo [] = caseInfo.GetFields()

        let readElements () =
          let rec read index acc =
            match reader.TokenType with
            | JsonToken.EndArray -> acc
            | _ ->
              let value = serializer.Deserialize(reader, fields[index].PropertyType)

              reader.Read() |> ignore<bool>
              read (index + 1) (acc @ [ value ])

          reader.Read() |> ignore<bool>
          read 0 List.empty

        let args = readElements () |> Array.ofList

        FSharpValue.MakeUnion(caseInfo, args)

      override _.CanConvert(objectType) = FSharpType.IsUnion objectType

    // http://gorodinski.com/blog/2013/01/05/json-dot-net-type-converters-for-f-option-list-tuple/
    type FSharpListConverter() =
      inherit JsonConverter()

      override _.CanConvert(t : System.Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>

      override _.WriteJson(writer, value, serializer) =
        let list = value :?> System.Collections.IEnumerable |> Seq.cast
        serializer.Serialize(writer, list)

      override _.ReadJson(reader, t, _, serializer) =
        let itemType = t.GetGenericArguments()[0]

        let collectionType =
          typedefof<System.Collections.Generic.IEnumerable<_>>.MakeGenericType
            (itemType)

        let collection =
          serializer.Deserialize(reader, collectionType)
          :?> System.Collections.IEnumerable
          |> Seq.cast

        let listType = typedefof<list<_>>.MakeGenericType (itemType)
        let cases = FSharpType.GetUnionCases(listType)

        let rec make =
          function
          | [] -> FSharpValue.MakeUnion(cases[0], [||])
          | head :: tail -> FSharpValue.MakeUnion(cases[1], [| head; (make tail) |])

        make (collection |> Seq.toList)

    // http://gorodinski.com/blog/2013/01/05/json-dot-net-type-converters-for-f-option-list-tuple/
    type FSharpTupleConverter() =
      inherit JsonConverter()

      override _.CanConvert(t : System.Type) = FSharpType.IsTuple(t)

      override _.WriteJson(writer, value, serializer) =
        let values = FSharpValue.GetTupleFields(value)
        serializer.Serialize(writer, values)

      override _.ReadJson(reader, t, existingValue, serializer) =
        let advance = reader.Read >> ignore<bool>
        let deserialize t = serializer.Deserialize(reader, t)
        let itemTypes = FSharpType.GetTupleElements(t)

        let readElements () =
          let rec read index acc =
            match reader.TokenType with
            | JsonToken.EndArray -> acc
            | _ ->
              let value = deserialize (itemTypes[index])
              advance ()
              read (index + 1) (acc @ [ value ])

          advance ()
          read 0 List.empty

        match reader.TokenType with
        | JsonToken.StartArray ->
          let values = readElements ()
          FSharpValue.MakeTuple(values |> List.toArray, t)
        | _ ->
          Exception.raiseInternal "Invalid token" [ "existingValue", existingValue ]


    type TLIDConverter() =
      inherit JsonConverter<tlid>()

      override _.ReadJson(reader : JsonReader, _, _, _, _) =
        let rawToken = string reader.Value
        parseUInt64 rawToken

      override _.WriteJson(writer : JsonWriter, value : tlid, _ : JsonSerializer) =
        writer.WriteValue(value)

    type PasswordConverter() =
      inherit JsonConverter<Password>()

      override _.ReadJson(reader : JsonReader, _, _, _, _) =
        let rawToken = string reader.Value |> UTF8.toBytes
        Password rawToken

      override _.WriteJson
        (
          writer : JsonWriter,
          value : Password,
          _ : JsonSerializer
        ) : unit =
        writer.WriteValue("Redacted")


    // We don't use this at the moment
    type OCamlOptionConverter() =
      inherit JsonConverter()

      override _.CanConvert(t : System.Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

      override _.WriteJson(writer : JsonWriter, value, serializer : JsonSerializer) =
        let value =
          if value = null then
            null
          else
            let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
            fields[0]

        serializer.Serialize(writer, value)

      override x.ReadJson
        (
          reader : JsonReader,
          t : System.Type,
          _,
          serializer : JsonSerializer
        ) =
        let cases = FSharpType.GetUnionCases(t)

        if reader.TokenType = JsonToken.Null then
          FSharpValue.MakeUnion(cases[0], [||])
        else
          let innerType = t.GetGenericArguments()[0]

          let innerType =
            if innerType.IsValueType then
              (typedefof<System.Nullable<_>>).MakeGenericType([| innerType |])
            else
              innerType

          let value = serializer.Deserialize(reader, innerType)

          if value = null then
            FSharpValue.MakeUnion(cases[0], [||])
          else
            FSharpValue.MakeUnion(cases[1], [| value |])

    type OCamlFloatConverter() =
      inherit JsonConverter<double>()

      override _.ReadJson(reader : JsonReader, _, v, _, _) =
        let rawToken = string reader.Value

        match rawToken with
        | "Infinity" -> System.Double.PositiveInfinity
        | "infinity" -> System.Double.PositiveInfinity
        | "-Infinity" -> System.Double.NegativeInfinity
        | "-infinity" -> System.Double.NegativeInfinity
        | "NaN" -> System.Double.NaN
        | _ ->
          let style = System.Globalization.NumberStyles.Float
          System.Double.Parse(rawToken, style)

      override _.WriteJson
        (
          writer : JsonWriter,
          value : double,
          serializer : JsonSerializer
        ) =
        match value with
        | System.Double.PositiveInfinity -> writer.WriteRawValue "Infinity"
        | System.Double.NegativeInfinity -> writer.WriteRawValue "-Infinity"
        | _ when System.Double.IsNaN value -> writer.WriteRawValue "NaN"
        | _ -> writer.WriteValue(value)

    type OCamlRawBytesConverter() =
      inherit JsonConverter<byte array>()
      // In OCaml, we wrap the in DBytes with a RawBytes, whose serializer uses
      // the url-safe version of base64. It's not appropriate for all byte
      // arrays, but I think this is the only user. If not, we'll need to add a
      // RawBytes type.
      override _.ReadJson(reader : JsonReader, _, v, _, _) =
        reader.Value :?> string |> Base64.fromUrlEncoded |> Base64.decode

      override _.WriteJson
        (
          writer : JsonWriter,
          value : byte [],
          _ : JsonSerializer
        ) =
        value |> Base64.urlEncodeToString |> writer.WriteValue

  // This is used for "normal" JSON conversion, such as converting Pos into
  // json. It does not feature anything for conversion to OCaml-compatible
  // stuff, such as may be required to communicate with the fuzzer or the
  // frontend. It does handle F#-specific constructs, and prevents exposing
  // passwords (just in case).
  module Vanilla =
    open Newtonsoft.Json

    let getSettings () =
      let settings = JsonSerializerSettings()
      // This might be a potential vulnerability, turn it off anyway
      settings.MetadataPropertyHandling <- MetadataPropertyHandling.Ignore
      // This is a potential vulnerability
      settings.TypeNameHandling <- TypeNameHandling.None
      // dont deserialize date-looking string as dates
      settings.DateParseHandling <- DateParseHandling.None
      settings.Converters.Add(AutoSerialize.TLIDConverter())
      settings.Converters.Add(AutoSerialize.PasswordConverter())
      settings.Converters.Add(AutoSerialize.FSharpListConverter())
      settings.Converters.Add(AutoSerialize.FSharpDuConverter())
      settings.Converters.Add(AutoSerialize.FSharpTupleConverter()) // gets tripped up on null, so put this last
      settings

    let _settings = getSettings ()

    let registerConverter (c : JsonConverter<'a>) =
      // insert in the front as the formatter will use the first converter that
      // supports the type, not the best one
      _settings.Converters.Insert(0, c)

    let serialize (data : 'a) : string = JsonConvert.SerializeObject(data, _settings)

    let prettySerialize (data : 'a) : string =
      let settings = getSettings ()
      settings.Formatting <- Formatting.Indented
      JsonConvert.SerializeObject(data, settings)

    let deserialize<'a> (json : string) : 'a =
      JsonConvert.DeserializeObject<'a>(json, _settings)



  module OCamlCompatible =
    open Newtonsoft.Json
    open Newtonsoft.Json.Converters

    let _settings =
      (let settings = JsonSerializerSettings()
       // This might be a potential vulnerability, turn it off anyway
       settings.MetadataPropertyHandling <- MetadataPropertyHandling.Ignore
       // This is a potential vulnerability
       settings.TypeNameHandling <- TypeNameHandling.None
       // dont deserialize date-looking string as dates
       settings.DateParseHandling <- DateParseHandling.None
       settings.Converters.Add(AutoSerialize.TLIDConverter())
       settings.Converters.Add(AutoSerialize.PasswordConverter())
       settings.Converters.Add(AutoSerialize.FSharpListConverter())
       settings.Converters.Add(AutoSerialize.OCamlOptionConverter())
       settings.Converters.Add(AutoSerialize.FSharpDuConverter())
       settings.Converters.Add(AutoSerialize.OCamlRawBytesConverter())
       settings.Converters.Add(AutoSerialize.OCamlFloatConverter())
       settings.Converters.Add(AutoSerialize.FSharpTupleConverter()) // gets tripped up so put last
       settings)

    let registerConverter (c : JsonConverter<'a>) =
      // insert in the front as the formatter will use the first converter that
      // supports the type, not the best one
      _settings.Converters.Insert(0, c)

    let serialize (data : 'a) : string = JsonConvert.SerializeObject(data, _settings)

    let deserialize<'a> (json : string) : 'a =
      JsonConvert.DeserializeObject<'a>(json, _settings)

// ----------------------
// Functions we'll later add to Tablecloth
// ----------------------
module Tablecloth =
  module Result =
    let unwrapWith (f : 'err -> 'ok) (t : Result<'ok, 'err>) : 'ok =
      match t with
      | Ok v -> v
      | Error v -> f v

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

  let iterInParallel (f : 'a -> Task<unit>) (list : List<'a>) : Task<unit> =
    task {
      let! (completedTasks : unit []) = List.map f list |> Task.WhenAll
      return ()
    }

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

type pos = { x : int; y : int }

type CanvasID = System.Guid
type UserID = System.Guid

// since these are all usernames, use types for safety
module UserName =
  type T =
    private
    | UserName of string

    override this.ToString() = let (UserName username) = this in username

  let banned =
    // originally from https://ldpreload.com/blog/names-to-reserve
    // we allow www, because we have a canvas there
    [ "abuse"
      "admin"
      "administrator"
      "autoconfig"
      "broadcasthost"
      "ftp"
      "hostmaster"
      "imap"
      "info"
      "is"
      "isatap"
      "it"
      "localdomain"
      "localhost"
      "mail"
      "mailer-daemon"
      "marketing"
      "mis"
      "news"
      "nobody"
      "noc"
      "noreply"
      "no-reply"
      "pop"
      "pop3"
      "postmaster"
      "root"
      "sales"
      "security"
      "smtp"
      "ssladmin"
      "ssladministrator"
      "sslwebmaster"
      "support"
      "sysadmin"
      "usenet"
      "uucp"
      "webmaster"
      "wpad"
      // original to us from here
      "billing"
      "dev"

      // alpha, but not beta, because user beta already exists (with ownership
      // transferred to us)
      "alpha" ]
    |> Set


  let validate (name : string) : Result<string, string> =
    let regex = @"^[a-z][a-z0-9_]{2,20}$"
    if Set.contains name banned then
      Error "Username is not allowed"
    else if String.length name > 20 then
      Error "Username was too long, must be <= 20."
    else if System.Text.RegularExpressions.Regex.IsMatch(name, regex) then
      Ok name
    else
      Error
        $"Invalid username '{name}', can only contain lowercase roman letters and digits, or '-' or '_'"

  // Create throws an InternalException. Validate before calling create to do user-visible errors
  let create (str : string) : T =
    str |> validate |> Exception.unwrapResultInternal [] |> UserName

  // For testing and creating banned names
  let createUnsafe (str : string) : T = UserName str

module OrgName =
  type T =
    private
    | OrgName of string

    override this.ToString() = let (OrgName orgName) = this in orgName

  let validate = UserName.validate

  // Create throws an InternalException. Validate before calling create to do user-visible errors
  let create (str : string) : T =
    str |> validate |> Exception.unwrapResultInternal [] |> OrgName

module OwnerName =
  type T =
    private
    | OwnerName of string

    override this.ToString() = let (OwnerName name) = this in name
    member this.toUserName() : UserName.T = UserName.create (string this)

  let validate = UserName.validate

  // Create throws an InternalException. Validate before calling create to do user-visible errors
  let create (str : string) : T =
    str |> validate |> Exception.unwrapResultInternal [] |> OwnerName

module CanvasName =
  type T =
    private
    | CanvasName of string

    override this.ToString() = let (CanvasName name) = this in name

  let validate (name : string) : Result<string, string> =
    let regex = "^([a-z0-9]+[_-]?)*[a-z0-9]$"

    if String.length name > 64 then
      Error "Canvas name was too long, must be <= 64."
    else if System.Text.RegularExpressions.Regex.IsMatch(name, regex) then
      Ok name
    else
      Error
        $"Invalid canvas name '{name}', can only contain roman lettets, digits, and '-' or '_'"


  // Create throws an InternalException. Validate before calling create to do user-visible errors
  let create (name : string) : T =
    name |> validate |> Exception.unwrapResultInternal [] |> CanvasName


module HttpHeaders =
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
    |> Option.map (fun (k, v) -> v)


let id (x : int) : id = uint64 x
