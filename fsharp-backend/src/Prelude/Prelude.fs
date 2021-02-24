module Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Text.RegularExpressions

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

let assert_ (msg : string) (cond : bool) : unit = if cond then () else failwith msg

let assertEq (msg : string) (expected : 'a) (actual : 'a) : unit =
  if expected <> actual then
    assert_ $"assertion failure: {msg} (expected {expected}, got {actual})" false

let assertRe (msg : string) (pattern : string) (input : string) : unit =
  let m = Regex.Match(input, pattern)
  if m.Success then () else assert_ $"{msg} (\"{input}\" ~= /{pattern}/)" false

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

let base64Encode (input : string) : string =
  input |> toBytes |> System.Convert.ToBase64String

let base64UrlEncode (str : string) : string =
  (base64Encode str).Replace('+', '-').Replace('/', '_').Replace("=", "")

let base64Decode (encoded : string) : string =
  encoded |> System.Convert.FromBase64String |> ofBytes

let sha1digest (input : string) : byte [] =
  use sha1 = new System.Security.Cryptography.SHA1CryptoServiceProvider()
  input |> toBytes |> sha1.ComputeHash

let toString (v : 'a) : string = v.ToString()

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
  member x.Bind(tv, f) = TaskOrValue.bind f tv
  member x.Bind(t, f) = TaskOrValue.bind f (Task t)
  // This lets us use return
  member x.Return(v) = TaskOrValue.unit (v)
  // This lets us use return!
  member x.ReturnFrom(tv) = tv
  member x.Zero() = TaskOrValue.unit (())
  // These lets us use try
  member x.TryWith(tv, f) = TaskOrValue.tryWith tv f
  member x.Delay(f) = TaskOrValue.delay f
// To make this usable, this will need a few more
// especially for reasonable exception handling..

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

// ----------------------
// Json auto-serialization
// ----------------------
module Json =
  module AutoSerialize =
    open System.Text.Json
    open System.Text.Json.Serialization

    // Serialize bigints as strings
    type BigIntConverter() =
      inherit JsonConverter<bigint>()

      override this.Read(reader : byref<Utf8JsonReader>,
                         _typ : System.Type,
                         _options : JsonSerializerOptions) =
        reader.GetString() |> parseBigint

      override this.Write(writer : Utf8JsonWriter,
                          value : bigint,
                          _options : JsonSerializerOptions) =
        writer.WriteStringValue(value.ToString())

    type TLIDConverter() =
      inherit JsonConverter<tlid>()

      override this.Read(reader : byref<Utf8JsonReader>,
                         _typ : System.Type,
                         _options : JsonSerializerOptions) =
        if reader.TokenType = JsonTokenType.String then
          let str = reader.GetString()
          parseUInt64 str
        else
          reader.GetUInt64()

      override this.Write(writer : Utf8JsonWriter,
                          value : tlid,
                          _options : JsonSerializerOptions) =
        writer.WriteNumberValue(value)

    type FloatConverter() =
      // We need this because OCaml gives us Infinity and NaN in our JSON
      inherit JsonConverter<double>()

      // We need this because OCaml gives us Infinity and NaN in our JSON. Note
      // that unlike other places, this is type-directed so we know we're
      // expecting a float and can check specific things
      override this.Read(reader : byref<Utf8JsonReader>,
                         _typ : System.Type,
                         _options : JsonSerializerOptions) =
        let rawToken = reader.ValueSpan.ToArray() |> ofBytes
        printfn $"rawtoken: {rawToken}"

        match rawToken with
        | "Infinity" -> System.Double.PositiveInfinity
        | "infinity" -> System.Double.PositiveInfinity
        | "-Infinity" -> System.Double.NegativeInfinity
        | "-infinity" -> System.Double.NegativeInfinity
        | "NaN" -> System.Double.NaN
        | _ -> reader.GetDouble()

      override this.Write(writer : Utf8JsonWriter,
                          value : double,
                          _options : JsonSerializerOptions) =
        match value with
        | System.Double.PositiveInfinity -> writer.WriteStringValue "Infinity"
        | System.Double.NegativeInfinity -> writer.WriteStringValue "-Infinity"
        | _ when System.Double.IsNaN value -> writer.WriteStringValue "NaN"
        | _ -> writer.WriteNumberValue(value)


    let _options =
      (let fsharpConverter =
        JsonFSharpConverter(
          unionEncoding =
            (JsonUnionEncoding.InternalTag ||| JsonUnionEncoding.UnwrapOption)
        )

       let options = JsonSerializerOptions()
       options.Converters.Add(TLIDConverter())
       options.Converters.Add(BigIntConverter())
       options.Converters.Add(FloatConverter())
       options.Converters.Add(fsharpConverter)
       options)

    let registerConverter (c : JsonConverter<'a>) =
      // insert in the front as the formatter will use the first converter that
      // supports the type, not the best one
      _options.Converters.Insert(0, c)

    let serialize (data : 'a) : string = JsonSerializer.Serialize(data, _options)

    let deserialize<'a> (json : string) : 'a =
      JsonSerializer.Deserialize<'a>(json, _options)

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

  let create (name : string) : T =
    if String.length name > 64 then
      failwith $"Canvas name was too long, must be <= 64."

    CanvasName(Tablecloth.String.toLowercase name)

let id (x : int) : id = uint64 x
