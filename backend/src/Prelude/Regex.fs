module Regex

open System.Text.RegularExpressions

// ----------------------
// Patterns
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
