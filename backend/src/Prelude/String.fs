module String

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

let take (count : int) (str : string) : string =
  if count >= str.Length then str else str.Substring(0, count)


let toLowercase (s : string) : string = s.ToLower()

let toUppercase (s : string) : string = s.ToUpper()

let trim (s : string) : string = s.Trim()
