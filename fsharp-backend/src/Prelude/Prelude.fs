module Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Text.RegularExpressions
// ----------------------
// Null
// ----------------------
// https://stackoverflow.com/a/11696947/104021
let inline isNull (x : ^T when ^T : not struct) = obj.ReferenceEquals(x, null)

// ----------------------
// Exceptions
// ----------------------

// Exceptions that should not be exposed to users, and that indicate unexpected
// behaviour

exception InternalException of string

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
// Debugging
// ----------------------
let debuG (msg : string) (a : 'a) : unit = printfn $"DEBUG: {msg} ({a})"

let debug (msg : string) (a : 'a) : 'a =
  debuG msg a
  a

// Print the value of `a`. Note that since this is wrapped in a task, it must
// resolve the task before it can print, which could lead to different ordering
// of operations.
let debugTask (msg : string) (a : Task<'a>) : Task<'a> =
  task {
    let! a = a
    printfn $"DEBUG: {msg} ({a})"
    return a
  }

let fstodo (msg : string) : 'a = failwith $"Code not yet ported to F#: {msg}"

// ----------------------
// Assertions
// ----------------------
// Asserts are problematic because they don't run in prod, and if they did they
// wouldn't be caught by the webserver

let assert_ (msg : string) (cond : bool) : unit =
  if cond then () else failwith $"Assertion failure, expected: {msg}"

let assertEq (msg : string) (expected : 'a) (actual : 'a) : unit =
  if expected <> actual then
    failwith $"Assertion failure: {msg} (expected {expected}, got {actual})"

let assertRe (msg : string) (pattern : string) (input : string) : unit =
  let m = Regex.Match(input, pattern)

  if m.Success then
    ()
  else
    failwith $"Assertion failure: {msg} (but \"{input}\" ~= /{pattern}/)"

// ----------------------
// Standard conversion functions
// ----------------------
// There are multiple ways to convert things in dotnet. Let's have a consistent set we use.

let parseInt64 (str : string) : int64 =
  try
    assertRe "int64" @"-?\d+" str
    System.Convert.ToInt64 str
  with e -> raise (InternalException $"parseInt64 failed: {str} - {e}")

let parseUInt64 (str : string) : uint64 =
  try
    assertRe "uint64" @"-?\d+" str
    System.Convert.ToUInt64 str
  with e -> raise (InternalException $"parseUInt64 failed: {str} - {e}")

let parseBigint (str : string) : bigint =
  try
    assertRe "bigint" @"-?\d+" str
    System.Numerics.BigInteger.Parse str
  with e -> raise (InternalException $"parseBigint failed: {str} - {e}")

let parseFloat (whole : string) (fraction : string) : float =
  try
    // FSTODO: don't actually assert, report to rollbar
    assertRe "whole" @"-?\d+" whole
    assertRe "fraction" @"\d+" fraction
    System.Double.Parse($"{whole}.{fraction}")
  with e -> raise (InternalException $"parseFloat failed: {whole}.{fraction} - {e}")

let makeFloat (positiveSign : bool) (whole : bigint) (fraction : bigint) : float =
  try
    assert_ "makefloat" (whole >= 0I)
    let sign = if positiveSign then "" else "-"
    $"{sign}{whole}.{fraction}" |> System.Double.Parse
  with e ->
    raise (InternalException $"makeFloat failed: {sign}{whole}.{fraction} - {e}")

let toBytes (input : string) : byte array = System.Text.Encoding.UTF8.GetBytes input

let ofBytes (input : byte array) : string = System.Text.Encoding.UTF8.GetString input

let base64Encode (input : byte []) : string = input |> System.Convert.ToBase64String

// Convert a base64 encoded string to one that is url-safe
let base64ToUrlEncoded (str : string) : string =
  str.Replace('+', '-').Replace('/', '_').Replace("=", "")

// Convert a url-safe base64 string to one that users the more traditional format
let base64FromUrlEncoded (str : string) : string =
  let initial = str.Replace('-', '+').Replace('_', '/')
  let length = initial.Length

  if length % 4 = 2 then $"{initial}=="
  else if length % 4 = 3 then $"{initial}="
  else initial

let base64UrlEncode (bytes : byte []) : string =
  bytes |> base64Encode |> base64ToUrlEncoded

let base64Decode (encoded : string) : byte [] =
  encoded |> System.Convert.FromBase64String

let sha1digest (input : string) : byte [] =
  use sha1 = new System.Security.Cryptography.SHA1CryptoServiceProvider()
  input |> toBytes |> sha1.ComputeHash

let toString (v : 'a) : string = v.ToString()


module Uuid =
  let nilNamespace : System.Guid = System.Guid "00000000-0000-0000-0000-000000000000"

  let uuidV5 (data : string) (nameSpace : System.Guid) : System.Guid =
    Faithlife.Utility.GuidUtility.Create(nilNamespace, data, 5)

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
let random : System.Random = System.Random()

let gid () : uint64 =
  try
    // get enough bytes for an int64, trim it to an int31 for now to match the frontend
    let bytes = Array.create 8 (byte 0)
    random.NextBytes(bytes)
    let rand64 : uint64 = System.BitConverter.ToUInt64(bytes, 0)
    // Keep 30 bits
    // 0b0000_0000_0000_0000_0000_0000_0000_0000_0011_1111_1111_1111_1111_1111_1111_1111L
    let mask : uint64 = 1073741823UL
    rand64 &&& mask
  with e -> raise (InternalException $"gid failed: {e}")

let randomString (length : int) : string =
  let result =
    Array.init length (fun _ -> char (random.Next(0x41, 0x5a))) |> System.String

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

module HashSet =
  type T<'v> = System.Collections.Generic.HashSet<'v>

  let add (v : 'v) (s : T<'v>) : unit =
    let (_ : bool) = s.Add v
    ()

  let toList (d : T<'v>) : List<'v> =
    seq {
      let mutable e = d.GetEnumerator()

      while e.MoveNext() do
        yield e.Current
    }
    |> Seq.toList



module Dictionary =
  type T<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
  let tryGetValue = FSharpPlus.Dictionary.tryGetValue

  let add (k : 'k) (v : 'v) (d : T<'k, 'v>) : T<'k, 'v> =
    d.Add(k, v)
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
    let result = System.Collections.Generic.Dictionary<'k, 'v>()
    List.iter (fun (k, v) -> result.Add(k, v)) l
    result



// ----------------------
// TaskOrValue
// ----------------------
// A way of combining non-task values with tasks, complete with computation expressions

type TaskOrValue<'T> =
  | Task of Task<'T>
  | Value of 'T

module TaskOrValue =
  let toTask (v : TaskOrValue<'a>) : Task<'a> =
    task {
      match v with
      | Task t -> return! t
      | Value v -> return v
    }

  // Functions for a computation Expressions
  let unit v = Value v

  let tryWith (v : TaskOrValue<'a>) (f : exn -> TaskOrValue<'a>) : TaskOrValue<'a> =
    match v with
    | Value v -> Value v
    | Task t ->
        Task(
          task {
            try
              let! newt = t
              return newt
            with e -> return! toTask (f e)
          }
        )

  let delay (f : unit -> TaskOrValue<'a>) : TaskOrValue<'a> =
    Task(task { return! toTask (f ()) })

  // Create a new TaskOrValue that first runs 'vt' and then
  // continues with whatever TaskOrValue is produced by 'f'.
  let bind (f : 'a -> TaskOrValue<'b>) (vt : TaskOrValue<'a>) : TaskOrValue<'b> =
    match vt with
    | Value v -> f v
    | Task t ->
        Task(
          task {
            let! v = t
            return! toTask (f v)
          }
        )


type TaskOrValueBuilder() =
  // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
  // This lets us use let!, do! - These can be overloaded
  // so I define two overloads for 'let!' - one taking regular
  // Task and one our TaskOrValue. You can then call both using
  // the let! syntax.
  member x.Bind(tv : TaskOrValue<'a>, f : 'a -> TaskOrValue<'b>) : TaskOrValue<'b> =
    TaskOrValue.bind f tv

  member x.Bind(t : Task<'a>, f : 'a -> TaskOrValue<'b>) : TaskOrValue<'b> =
    TaskOrValue.bind f (Task t)

  // This lets us use return
  member x.Return(v : 'a) : TaskOrValue<'a> = TaskOrValue.unit (v)

  // This lets us use return!
  member x.ReturnFrom(tv : TaskOrValue<'a>) : TaskOrValue<'a> = tv

  member x.Zero() : TaskOrValue<unit> = TaskOrValue.unit (())

  // These lets us use try
  member x.TryWith
    (
      tv : TaskOrValue<'a>,
      f : exn -> TaskOrValue<'a>
    ) : TaskOrValue<'a> =
    TaskOrValue.tryWith tv f

  member x.Delay(f : unit -> TaskOrValue<'a>) : TaskOrValue<'a> = TaskOrValue.delay f

let taskv = TaskOrValueBuilder()

// Processes each item of the list in order, waiting for the previous one to
// finish. This ensures each request in the list is processed to completion
// before the next one is done, making sure that, for example, a HttpClient
// call will finish before the next one starts. Will allow other requests to
// run which waiting.
//
// Why can't this be done in a simple map? We need to resolve element i in
// element (i+1)'s task expression.
let map_s (f : 'a -> TaskOrValue<'b>) (list : List<'a>) : TaskOrValue<List<'b>> =
  taskv {
    let! result =
      match list with
      | [] -> taskv { return [] }
      | head :: tail ->
          taskv {
            let firstComp =
              taskv {
                let! result = f head
                return ([], result)
              }

            let! ((accum, lastcomp) : (List<'b> * 'b)) =
              List.fold
                (fun (prevcomp : TaskOrValue<List<'b> * 'b>) (arg : 'a) ->
                  taskv {
                    // Ensure the previous computation is done first
                    let! ((accum, prev) : (List<'b> * 'b)) = prevcomp
                    let! result = f arg

                    return (prev :: accum, result)
                  })
                firstComp
                tail

            return List.rev (lastcomp :: accum)
          }

    return (result |> Seq.toList)
  }

let iter_s (f : 'a -> TaskOrValue<unit>) (list : List<'a>) : TaskOrValue<unit> =
  taskv {
    match list with
    | [] -> return ()
    | head :: tail ->
        return!
          List.fold
            (fun (prevcomp : TaskOrValue<unit>) (arg : 'a) ->
              taskv {
                // Ensure the previous computation is done first
                let! (prev : unit) = prevcomp
                return! f arg
              })
            (f head)
            tail
  }


let filter_s
  (f : 'a -> TaskOrValue<bool>)
  (list : List<'a>)
  : TaskOrValue<List<'a>> =
  taskv {
    let! result =
      match list with
      | [] -> taskv { return [] }
      | head :: tail ->
          taskv {
            let firstComp =
              taskv {
                let! keep = f head
                return ([], (keep, head))
              }

            let! ((accum, lastcomp) : (List<'a> * (bool * 'a))) =
              List.fold
                (fun (prevcomp : TaskOrValue<List<'a> * (bool * 'a)>) (arg : 'a) ->
                  taskv {
                    // Ensure the previous computation is done first
                    let! ((accum, (prevkeep, prev)) : (List<'a> * (bool * 'a))) =
                      prevcomp

                    let accum = if prevkeep then prev :: accum else accum

                    let! keep = (f arg)

                    return (accum, (keep, arg))
                  })
                firstComp
                tail

            let (lastkeep, lastval) = lastcomp

            let accum = if lastkeep then lastval :: accum else accum

            return List.rev accum
          }

    return (result |> Seq.toList)
  }

let filter_map
  (f : 'a -> TaskOrValue<Option<'b>>)
  (list : List<'a>)
  : TaskOrValue<List<'b>> =
  taskv {
    let! result =
      match list with
      | [] -> taskv { return [] }
      | head :: tail ->
          taskv {
            let firstComp =
              taskv {
                let! newValue = f head
                return ([], newValue)
              }

            let! ((accum, lastcomp) : (List<'b> * Option<'b>)) =
              List.fold
                (fun (prevcomp : TaskOrValue<List<'b> * Option<'b>>) (arg : 'a) ->
                  taskv {
                    let! ((accum, prevValue) : (List<'b> * Option<'b>)) = prevcomp

                    let accum =
                      match prevValue with
                      | Some pv -> pv :: accum
                      | None -> accum

                    let! newValue = (f arg)

                    return (accum, newValue)
                  })
                firstComp
                tail

            let accum =
              match lastcomp with
              | Some lv -> lv :: accum
              | None -> accum

            return List.rev accum
          }

    return (result |> Seq.toList)
  }

let find_s
  (f : 'a -> TaskOrValue<bool>)
  (list : List<'a>)
  : TaskOrValue<Option<'a>> =
  List.fold
    (fun (accum : TaskOrValue<Option<'a>>) (arg : 'a) ->
      taskv {
        match! accum with
        | Some v -> return Some v
        | None ->
            let! result = f arg
            return (if result then Some arg else None)
      })
    (taskv { return None })
    list

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
  module AutoSerialize =
    open Newtonsoft.Json
    open Microsoft.FSharp.Reflection

    // Serialize bigints as strings
    type BigIntConverter() =
      inherit JsonConverter<bigint>()

      override _.ReadJson(reader : JsonReader, _, _, _, s) : bigint =
        reader.Value.ToString() |> parseBigint

      override _.WriteJson
        (
          writer : JsonWriter,
          value : bigint,
          serializer : JsonSerializer
        ) =
        writer.WriteRawValue(value.ToString())

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
        | _ -> failwith "Incorrect starting token for union: should be array"

        let caseName : string =
          reader.Read() |> ignore
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
                let value =
                  serializer.Deserialize(reader, fields.[index].PropertyType)

                reader.Read() |> ignore
                read (index + 1) (acc @ [ value ])

          reader.Read() |> ignore
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
        let itemType = t.GetGenericArguments().[0]

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
          | [] -> FSharpValue.MakeUnion(cases.[0], [||])
          | head :: tail -> FSharpValue.MakeUnion(cases.[1], [| head; (make tail) |])

        make (collection |> Seq.toList)

    // http://gorodinski.com/blog/2013/01/05/json-dot-net-type-converters-for-f-option-list-tuple/
    type FSharpTupleConverter() =
      inherit JsonConverter()

      override _.CanConvert(t : System.Type) = FSharpType.IsTuple(t)

      override _.WriteJson(writer, value, serializer) =
        let values = FSharpValue.GetTupleFields(value)
        serializer.Serialize(writer, values)

      override _.ReadJson(reader, t, existingValue, serializer) =
        let advance = reader.Read >> ignore
        let deserialize t = serializer.Deserialize(reader, t)
        let itemTypes = FSharpType.GetTupleElements(t)

        let readElements () =
          let rec read index acc =
            match reader.TokenType with
            | JsonToken.EndArray -> acc
            | _ ->
                let value = deserialize (itemTypes.[index])
                advance ()
                read (index + 1) (acc @ [ value ])

          advance ()
          read 0 List.empty

        match reader.TokenType with
        | JsonToken.StartArray ->
            let values = readElements ()
            FSharpValue.MakeTuple(values |> List.toArray, t)
        | _ -> failwith $"invalid token: {existingValue}"


    type TLIDConverter() =
      inherit JsonConverter<tlid>()

      override _.ReadJson(reader : JsonReader, _, _, _, _) =
        let rawToken = reader.Value.ToString()
        parseUInt64 rawToken

      override _.WriteJson(writer : JsonWriter, value : tlid, _ : JsonSerializer) =
        writer.WriteValue(value)

    type PasswordConverter() =
      inherit JsonConverter<Password>()

      override _.ReadJson(reader : JsonReader, _, _, _, _) =
        failwith "unsupported deserialization of password"
        Password(toBytes "password should never be read here")

      override _.WriteJson
        (
          writer : JsonWriter,
          value : Password,
          _ : JsonSerializer
        ) =
        failwith "unsupported serialization of password"
        writer.WriteValue "<password should never be written here>"


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
            fields.[0]

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
          FSharpValue.MakeUnion(cases.[0], [||])
        else
          let innerType = t.GetGenericArguments().[0]

          let innerType =
            if innerType.IsValueType then
              (typedefof<System.Nullable<_>>).MakeGenericType([| innerType |])
            else
              innerType

          let value = serializer.Deserialize(reader, innerType)

          if value = null then
            FSharpValue.MakeUnion(cases.[0], [||])
          else
            FSharpValue.MakeUnion(cases.[1], [| value |])

    type OCamlFloatConverter() =
      inherit JsonConverter<double>()

      override _.ReadJson(reader : JsonReader, _, v, _, _) =
        let rawToken = reader.Value.ToString()

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
        reader.Value :?> string
        |> base64FromUrlEncoded
        |> System.Convert.FromBase64String

      override _.WriteJson
        (
          writer : JsonWriter,
          value : byte [],
          _ : JsonSerializer
        ) =
        value
        |> System.Convert.ToBase64String
        |> base64ToUrlEncoded
        |> writer.WriteValue

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
      settings.Converters.Add(AutoSerialize.BigIntConverter())
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
       settings.Converters.Add(AutoSerialize.BigIntConverter())
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

    let merge (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
      FSharpPlus.Map.union m1 m2

  module Result =
    // Returns `Ok ()` if no errors, or `Error list` otherwise
    let combineErrorsUnit (l : List<Result<'ok, 'err>>) : Result<unit, List<'err>> =
      List.fold
        (fun l r ->
          match l, r with
          | _, Ok _ -> l
          | Ok (), Error e -> Error [ e ]
          | Error l, Error e -> Error(e :: l))
        (Ok())
        l



// ----------------------
// Task list processing
// ----------------------
module Task =
  // Processes each item of the list in order, waiting for the previous one to
  // finish. This ensures each request in the list is processed to completion
  // before the next one is done, making sure that, for example, a HttpClient
  // call will finish before the next one starts. Will allow other requests to
  // run which waiting.
  //
  // Why can't this be done in a simple map? We need to resolve element i in
  // element (i+1)'s task expression.
  let mapSequentially (f : 'a -> Task<'b>) (list : List<'a>) : Task<List<'b>> =
    task {
      let! result =
        match list with
        | [] -> task { return [] }
        | head :: tail ->
            task {
              let firstComp =
                task {
                  let! result = f head
                  return ([], result)
                }

              let! ((accum, lastcomp) : (List<'b> * 'b)) =
                List.fold
                  (fun (prevcomp : Task<List<'b> * 'b>) (arg : 'a) ->
                    task {
                      // Ensure the previous computation is done first
                      let! ((accum, prev) : (List<'b> * 'b)) = prevcomp
                      let accum = prev :: accum

                      let! result = (f arg)

                      return (accum, result)
                    })
                  firstComp
                  tail

              return List.rev (lastcomp :: accum)
            }

      return (result |> Seq.toList)
    }

  let filterSequentially (f : 'a -> Task<bool>) (list : List<'a>) : Task<List<'a>> =
    task {
      let! result =
        match list with
        | [] -> task { return [] }
        | head :: tail ->
            task {
              let firstComp =
                task {
                  let! keep = f head
                  return ([], (keep, head))
                }

              let! ((accum, lastcomp) : (List<'a> * (bool * 'a))) =
                List.fold
                  (fun (prevcomp : Task<List<'a> * (bool * 'a)>) (arg : 'a) ->
                    task {
                      // Ensure the previous computation is done first
                      let! ((accum, (prevkeep, prev)) : (List<'a> * (bool * 'a))) =
                        prevcomp

                      let accum = if prevkeep then prev :: accum else accum

                      let! keep = (f arg)

                      return (accum, (keep, arg))
                    })
                  firstComp
                  tail

              let (lastkeep, lastval) = lastcomp
              let accum = if lastkeep then lastval :: accum else accum
              return List.rev accum
            }

      return (result |> Seq.toList)
    }

  let iterSequentially (f : 'a -> Task<unit>) (list : List<'a>) : Task<unit> =
    task {
      match list with
      | [] -> return ()
      | head :: tail ->
          let firstComp =
            task {
              let! result = f head
              return ([], result)
            }

          let! ((accum, lastcomp) : (List<unit> * unit)) =
            List.fold
              (fun (prevcomp : Task<List<unit> * unit>) (arg : 'a) ->
                task {
                  // Ensure the previous computation is done first
                  let! ((accum, prev) : (List<unit> * unit)) = prevcomp
                  let accum = prev :: accum

                  let! result = f arg

                  return (accum, result)
                })
              firstComp
              tail

          return List.head (lastcomp :: accum)
    }

  // takes a list of tasks and calls f on it, turning it into a single task
  let flatten (list : List<Task<'a>>) : Task<List<'a>> =
    let rec loop (acc : Task<List<'a>>) (xs : List<Task<'a>>) =
      task {
        let! acc = acc

        match xs with
        | [] -> return List.rev acc
        | x :: xs ->
            let! x = x
            return! loop (task { return (x :: acc) }) xs
      }

    loop (task { return [] }) list

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




// ----------------------
// Shared Types
// ----------------------
// Some fundamental types that we want to use everywhere.

// DO NOT define any serialization on these types. If you want to serialize
// them, you should move these to the files with specific formats and serialize
// them there.

type pos = { x : int; y : int }

// We use an explicit sign for Floats, instead of making it implicit in the
// first digit, because otherwise we lose the sign on 0, and can't represent
// things like -0.5
type Sign =
  | Positive
  | Negative

type CanvasID = System.Guid
type UserID = System.Guid

// since these are all usernames, use types for safety
module UserName =
  type T =
    private
    | UserName of string

    override this.ToString() = let (UserName username) = this in username

  let create (str : string) : T = UserName(Tablecloth.String.toLowercase str)

module OrgName =
  type T =
    private
    | OrgName of string

    override this.ToString() = let (OrgName orgName) = this in orgName

  let create (str : string) : T = OrgName(Tablecloth.String.toLowercase str)

module OwnerName =
  type T =
    private
    | OwnerName of string

    override this.ToString() = let (OwnerName name) = this in name
    member this.toUserName() : UserName.T = UserName.create (this.ToString())

  let create (str : string) : T = OwnerName(Tablecloth.String.toLowercase str)

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
      Error $"Canvas name can only contain a-z, 0-9, and '-' or '_' but is {name}"


  let create (name : string) : T =
    name |> validate |> Tablecloth.Result.unwrapUnsafe |> CanvasName

let id (x : int) : id = uint64 x
