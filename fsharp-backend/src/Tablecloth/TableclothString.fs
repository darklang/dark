module Tablecloth.String

// Functions for working with ["strings"]

type t = string

module String = FSharp.Core.String

let initialize f length =
  String.init length (fun i ->
    let c : char = (f i)
    c.ToString())

let repeat count s = String.replicate count s

let fromChar (c : char) = c.ToString()

let from_char s = fromChar s

let fromArray (characters : char array) : string = System.String(characters)

let from_array cs = fromArray cs

let fromList (characters : char list) : string =
  characters |> List.toArray |> System.String

let from_list cs = fromList cs

let length (s : string) = s.Length

let isEmpty s = length s = 0

let is_empty s = isEmpty s

let get (i : int) (s : string) = s[i]

let getAt index s =
  if index >= 0 && index < length s then Some(get index s) else None

let get_at index s = getAt index s

// let ( .?[] ) (string : string) (index : int) : char option = getAt string ~index

let uncons (s : string) : (char * string) option =
  (match s with
   | "" -> None
   | s -> Some(s[0], s[1 .. (String.length s - 1)]))

let dropLeft (count : int) (s : string) = s[count..]

let drop_left count s = dropLeft count s

let dropRight (count : int) (s : string) = s[0 .. (s.Length - (count + 1))]

let drop_right count s = dropRight count s

let split (on : string) (s : string) =
  if s = "" then [] else s.Split(on) |> List.fromArray

let startsWith (prefix : string) (s : string) = s.StartsWith(prefix)

let starts_with prefix s = startsWith prefix s

let endsWith (suffix : string) (s : string) = s.EndsWith(suffix)

let ends_with suffix s = endsWith suffix s

let toLowercase (s : string) = s.ToLower()

let to_lowercase s = toLowercase s

let toUppercase (s : string) = s.ToUpper()

let to_uppercase s = toUppercase s

let capitalize (s : string) =
  if s.Length = 0 then ""
  else if s.Length = 1 then System.Char.ToUpper(s[0]).ToString()
  else System.Char.ToUpper(s[0]).ToString() + s[1..]

let uncapitalize (s : string) =
  if s.Length = 0 then ""
  else if s.Length = 1 then System.Char.ToLower(s[0]).ToString()
  else System.Char.ToLower(s[0]).ToString() + s[1..]

let isCapitalized (s : string) =
  if s.Length = 0 then false else System.Char.IsUpper(s[0])

let is_capitalized s = isCapitalized s

let includes (substring : string) (s : string) = s.Contains(substring)

open System.Globalization

let reverse (s : string) =
  let graphemes =
    seq {
      let tee = System.Globalization.StringInfo.GetTextElementEnumerator(s)

      while tee.MoveNext() do
        yield tee.GetTextElement()
    }

  graphemes |> Seq.toList |> List.reverse |> String.concat ""

let slice from ``to`` (str : string) = str[from..``to``]

let indexOf (needle : string) (haystack : string) : int option =
  let result = haystack.IndexOf(needle) in if result = -1 then None else Some result

let index_of needle haystack = indexOf needle haystack

let indexOfRight (needle : string) (haystack : string) =
  let result = haystack.LastIndexOf(needle) in

  if result = -1 then None else Some result

let index_of_right n s = indexOfRight n s

let insertAt (index : int) (value : string) (s : string) = s.Insert(index, value)

let insert_at index value s = insertAt index value s

let toArray (s : string) = s.ToCharArray()

let to_array s = toArray s

let toList (s : string) = s.ToCharArray() |> List.fromArray

let to_list s = toList s

let trim (s : string) = s.Trim()

let trimLeft (s : string) = s.TrimStart()

let trim_left s = trimLeft s

let trimRight (s : string) = s.TrimEnd()

let trim_right s = trimRight s

let padLeft ``with`` targetLength (s : string) : string =
  if length s >= targetLength then
    s
  else
    let paddingLength = targetLength - length s in
    let count = paddingLength / length ``with`` in
    let padding = slice 0 paddingLength (repeat count ``with``)
    padding + s


let pad_left ``with`` t s = padLeft ``with`` t s

let padRight ``with`` targetLength (s : string) : string =
  if length s >= targetLength then
    s
  else
    let paddingLength = targetLength - length s in
    let count = paddingLength / length ``with`` in
    let padding = slice 0 paddingLength (repeat count ``with``) in
    s + padding


let pad_right ``with`` t s = padRight ``with`` t s

let forEach f s = String.iter f s

let for_each f s = forEach f s

let fold (initial : 'a) (f : 'a -> char -> 'a) (s : string) : 'a =
  s |> toArray |> Array.fold initial (fun accum c -> f accum c)

let equal (s1 : string) (s2 : string) = s1 = s2

let compare (s1 : string) (s2 : string) = s1.CompareTo s2
