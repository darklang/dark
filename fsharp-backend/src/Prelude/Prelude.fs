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

// ----------------------
// Debugging
// ----------------------

let debug (msg : string) (a : 'a) : 'a =
  printfn $"DEBUG: {msg} ({a})"
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

// ----------------------
// Assertions
// ----------------------
// Asserts are problematic because they don't run in prod, and if they did they
// wouldn't be caught by the webserver

let assert_ (msg : string) (cond : bool) : unit = if cond then () else failwith msg

let assertEq (msg : string) (expected : 'a) (actual : 'a) : unit =
  if expected <> actual then assert_
                               $"assertion failure: {msg} (expected {expected}, got {
                                                                                       actual
                               })"
                               false

let assertRe (msg : string) (pattern : string) (input : string) : unit =
  let m = Regex.Match(input, pattern)
  if m.Success then () else assert_ $"{msg} ({input} ~= /{pattern}/)" false

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

let base64Encode (input : string) : string =
  input |> System.Text.Encoding.UTF8.GetBytes |> System.Convert.ToBase64String

let base64Decode (encoded : string) : string =
  encoded
  |> System.Convert.FromBase64String
  |> System.Text.Encoding.UTF8.GetString


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
    let bytes = Array.init 8 (fun _ -> (byte) 0)
    random.NextBytes(bytes)
    let rand64 : uint64 = System.BitConverter.ToUInt64(bytes, 0)
    // Keep 30 bits
    // 0b0000_0000_0000_0000_0000_0000_0000_0000_0011_1111_1111_1111_1111_1111_1111_1111L
    let mask : uint64 = 1073741823UL
    rand64 &&& mask
  with e -> raise (InternalException $"gid failed: {e}")

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

  let lengthInEgcs (s : string) : int =
    System.Globalization.StringInfo(s).LengthInTextElements

  let toLower (str : string) : string = str.ToLower()

  let toUpper (str : string) : string = str.ToUpper()

  let base64UrlEncode (str : string) : string =

    let inputBytes = System.Text.Encoding.UTF8.GetBytes(str)

    // Special "url-safe" base64 encode.
    System
      .Convert
      .ToBase64String(inputBytes)
      .Replace('+', '-')
      .Replace('/', '_')
      .Replace("=", "")

// ----------------------
// TaskOrValue
// ----------------------
// A way of combining non-task values with tasks, complete with computation expressions

open System.Threading.Tasks
open FSharp.Control.Tasks

type TaskOrValue<'T> =
  | Task of Task<'T>
  | Value of 'T

module TaskOrValue =
  // Wraps a value in TaskOrValue
  let unit v = Value v

  let toTask (v : TaskOrValue<'a>) : Task<'a> =
    task {
      match v with
      | Task t -> return! t
      | Value v -> return v
    }

  // Create a new TaskOrValue that first runs 'vt' and then
  // continues with whatever TaskorValue is produced by 'f'.
  let bind f vt =
    match vt with
    | Value v ->
        // It was a value, so we return 'f v' directly
        f v
    | Task t ->
        // It was a task, so we need to unwrap that and create
        // a new task - inside Task. If 'f v' returns a task, we
        // still need to return this as task though.
        Task(
          task {
            let! v = t

            match f v with
            | Value v -> return v
            | Task t -> return! t
          }
        )

type TaskOrValueBuilder() =
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


let filter_s (f : 'a -> TaskOrValue<bool>)
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
// Json auto-serialization
// ----------------------
module Json =
  module AutoSerialize =
    open System.Text.Json
    open System.Text.Json.Serialization

    let _options =
      (let options = JsonSerializerOptions()

       options.Converters.Add(
         JsonFSharpConverter(unionEncoding = JsonUnionEncoding.InternalTag)
       )

       options)

    let serialize (data : 'a) : string = JsonSerializer.Serialize(data, _options)

    let deserialize<'a> (json : string) : 'a =
      JsonSerializer.Deserialize<'a>(json, _options)

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

// ----------------------
// Tablecloth
// ----------------------
module Tablecloth =
  // An implementation of https://github.com/darklang/tablecloth, in F#. Intended
  // to be upstreamed.

  module Result =

    let and_ (a : Result<'ok, 'err>) (b : Result<'ok, 'err>) : Result<'ok, 'err> =
      match a with
      | Ok _ -> b
      | _ -> a

    let okOrRaise (r : Result<'ok, 'err>) : 'ok =
      match r with
      | Error e -> failwith $"Error in okOrRaise: {e}"
      | Ok o -> o

  module String =
    let startsWith (prefix : string) (str : string) : bool = str.StartsWith prefix
    let endsWith (suffix : string) (str : string) : bool = str.EndsWith suffix

    let splitOnNewline (str : string) : List<string> =
      str.Split([| "\n"; "\r\n" |], System.StringSplitOptions.None) |> Array.toList

    let dropLeft (count : int) (str : string) : string = str.Remove(0, count)
