module Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

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
// Important types
// ----------------------
type tlid = uint64

type id = uint64

// This is important to prevent auto-serialization accidentally leaking this,
// though it never should anyway
type Password = Password of byte array

type NEList<'a> = NEList.NEList<'a>
type Metadata = Exception.Metadata

type HashSet<'a> = HashSet.HashSet<'a>

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

let print (string : string) : unit = NonBlockingConsole.writeLine string


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
// Lazy utilities
// ----------------------
module Lazy =
  let inline force (l : Lazy<_>) = l.Force()
  let map f l = lazy ((f << force) l)
  let bind f l = lazy ((force << f << force) l)


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

    type NEListValueConverter<'TValue>() =
      inherit JsonConverter<NEList<'TValue>>()

      override this.Read
        (
          reader : byref<Utf8JsonReader>,
          typeToConvert : System.Type,
          options : JsonSerializerOptions
        ) =
        let list =
          JsonSerializer.Deserialize<'TValue seq>(&reader, options) |> Seq.toList
        match list with
        | [] -> Exception.raiseInternal "Empty list" []
        | head :: tail -> NEList.ofList head tail


      override this.Write
        (
          writer : Utf8JsonWriter,
          value : NEList<'TValue>,
          options : JsonSerializerOptions
        ) =
        let value = NEList.toList value
        JsonSerializer.Serialize(writer, (List.toSeq value), options)


    type NEListConverter() =
      inherit JsonConverterFactory()
      override this.CanConvert(typeToConvert : System.Type) : bool =
        typeToConvert.IsGenericType
        && List.contains
          (typeToConvert.GetGenericTypeDefinition())
          [ typedefof<NEList<_>>
            typedefof<System.Collections.Generic.IReadOnlyCollection<_>> ]

      override this.CreateConverter
        (
          typeToConvert : System.Type,
          _options : JsonSerializerOptions
        ) : JsonConverter =
        let typArgs = typeToConvert.GetGenericArguments()
        let converterType =
          typedefof<NEListValueConverter<_>>.MakeGenericType(typArgs)
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
      options.Converters.Add(NEListConverter())
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
        Exception.sendRollbarError
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

    [<CompilerMessageAttribute("Result.unwrapUnsafe is banned, use Exception.unwrapResult* instead",
                               0,
                               IsError = true,
                               IsHidden = true)>]
    let unwrapUnsafe = Tablecloth.Result.unwrapUnsafe

    let orElse
      (f : unit -> Result<'ok, 'err>)
      (t : Result<'ok, 'err>)
      : Result<'ok, 'err> =
      match t with
      | Ok _ -> t
      | Error _ -> f ()

  module Option =

    [<CompilerMessageAttribute("Option.unwrapUnsafe is banned, use Exception.unwrapOption* instead",
                               0,
                               IsError = true,
                               IsHidden = true)>]
    let unwrapUnsafe = Tablecloth.Option.unwrapUnsafe

    [<CompilerMessageAttribute("Option.get is banned, use Exception.unwrapOption* instead",
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

  module List =
    let splitLast (l : List<'a>) : Option<List<'a> * 'a> =
      match List.rev l with
      | [] -> None
      | head :: tail -> Some(List.rev tail, head)

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

    let mapSequentiallyWithIndex
      (f : int -> 'a -> Ply<'b>)
      (list : List<'a>)
      : Ply<List<'b>> =
      list
      |> foldSequentiallyWithIndex
        (fun (i : int) (accum : List<'b>) (arg : 'a) ->
          uply {
            let! result = f i arg
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

  module NEList =
    let mapSequentially (f : 'a -> Ply<'b>) (list : NEList<'a>) : Ply<NEList<'b>> =
      uply {
        let! head = f list.head
        let! tail = List.mapSequentially f list.tail
        return NEList.ofList head tail
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
// Shared Types
// ----------------------
// Some fundamental types that we want to use everywhere.

// DO NOT define any serialization on these types. If you want to serialize
// them, you should move these to the files with specific formats and serialize
// them there.
type CanvasID = System.Guid
type UserID = System.Guid

let id (x : int) : id = uint64 x

// since we hide F#'s default 'id' fn just above
let identity a = a
