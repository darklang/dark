/// Unicode case mapping for `Stdlib.String` and `Stdlib.Char`.
///
/// Both follow Unicode's default, language-neutral rules, so the same string maps the
/// same way on every machine, whatever its locale: `"i"` uppercases to `"I"`, never to
/// the Turkish `"İ"`.
///
/// - A String gets the full mapping: the one-to-one mappings, the SpecialCasing rules
///   that turn one character into several (`"ß"` to `"SS"`, `"ﬁ"` to `"FI"`, `"İ"` to
///   `"i̇"`), and Greek final sigma, the one context rule that is not language-specific.
/// - A Char gets the simple mapping alone, one character to one, so the result is always
///   a single Char: `'ß'` has no one-character uppercase and stays `'ß'`.
///
/// .NET has only the simple mapping, so the multi-character rules are carried here.
///
/// This is F# rather than Dark for speed. A Dark `toUppercase` with this table as a
/// package `val` measured 125 to 263x slower (a 1 MB string: 91 ms here, 11.4 s in
/// Dark), mostly from copying the whole table on every read of the `val` and from the
/// cost of each call. Revisit once the VM is within about 10x of this.
module Builtins.Pure.CaseMapping

open System.Text
open System.Globalization


/// Unicode's simple uppercase. .NET's invariant mapping, except for dotless `ı`, which
/// .NET leaves alone and Unicode uppercases to `I`.
let simpleUpper (r : Rune) : Rune =
  if r.Value = 0x0131 then Rune 0x0049 else Rune.ToUpperInvariant r

/// Unicode's simple lowercase. .NET's invariant mapping, except for dotted `İ`, which
/// .NET leaves alone and Unicode lowercases to `i`.
let simpleLower (r : Rune) : Rune =
  if r.Value = 0x0130 then Rune 0x0069 else Rune.ToLowerInvariant r


/// The uppercase mappings that give more than one codepoint: the unconditional entries
/// of Unicode's SpecialCasing.txt. Lowercase has one such entry, `İ`, in `toLowercase`.
///
/// Combining marks are written as escapes, named after the row, since they would
/// otherwise sit invisibly on the letter before them. So are two keys that Unicode
/// counts as the same letter as an earlier one, and that would look identical to it.
/// Every capital in a Greek row is Greek: `Ι` is iota, `Α` alpha, `Η` eta, `Ρ` rho,
/// `Υ` upsilon.
let private multiUpper : System.Collections.Generic.IDictionary<int, string> =
  [ "ß", "SS"
    "ŉ", "ʼN" // modifier letter apostrophe
    "ǰ", "J\u030C" // caron
    "ΐ", "Ι\u0308\u0301" // diaeresis, acute accent
    "ΰ", "Υ\u0308\u0301" // diaeresis, acute accent
    "և", "ԵՒ"
    "ẖ", "H\u0331" // macron below
    "ẗ", "T\u0308" // diaeresis
    "ẘ", "W\u030A" // ring above
    "ẙ", "Y\u030A" // ring above
    "ẚ", "Aʾ" // modifier letter right half ring
    "ὐ", "Υ\u0313" // comma above
    "ὒ", "Υ\u0313\u0300" // comma above, grave accent
    "ὔ", "Υ\u0313\u0301" // comma above, acute accent
    "ὖ", "Υ\u0313\u0342" // comma above, perispomeni
    "ᾀ", "ἈΙ"
    "ᾁ", "ἉΙ"
    "ᾂ", "ἊΙ"
    "ᾃ", "ἋΙ"
    "ᾄ", "ἌΙ"
    "ᾅ", "ἍΙ"
    "ᾆ", "ἎΙ"
    "ᾇ", "ἏΙ"
    "ᾈ", "ἈΙ"
    "ᾉ", "ἉΙ"
    "ᾊ", "ἊΙ"
    "ᾋ", "ἋΙ"
    "ᾌ", "ἌΙ"
    "ᾍ", "ἍΙ"
    "ᾎ", "ἎΙ"
    "ᾏ", "ἏΙ"
    "ᾐ", "ἨΙ"
    "ᾑ", "ἩΙ"
    "ᾒ", "ἪΙ"
    "ᾓ", "ἫΙ"
    "ᾔ", "ἬΙ"
    "ᾕ", "ἭΙ"
    "ᾖ", "ἮΙ"
    "ᾗ", "ἯΙ"
    "ᾘ", "ἨΙ"
    "ᾙ", "ἩΙ"
    "ᾚ", "ἪΙ"
    "ᾛ", "ἫΙ"
    "ᾜ", "ἬΙ"
    "ᾝ", "ἭΙ"
    "ᾞ", "ἮΙ"
    "ᾟ", "ἯΙ"
    "ᾠ", "ὨΙ"
    "ᾡ", "ὩΙ"
    "ᾢ", "ὪΙ"
    "ᾣ", "ὫΙ"
    "ᾤ", "ὬΙ"
    "ᾥ", "ὭΙ"
    "ᾦ", "ὮΙ"
    "ᾧ", "ὯΙ"
    "ᾨ", "ὨΙ"
    "ᾩ", "ὩΙ"
    "ᾪ", "ὪΙ"
    "ᾫ", "ὫΙ"
    "ᾬ", "ὬΙ"
    "ᾭ", "ὭΙ"
    "ᾮ", "ὮΙ"
    "ᾯ", "ὯΙ"
    "ᾲ", "ᾺΙ"
    "ᾳ", "ΑΙ"
    "ᾴ", "ΆΙ"
    "ᾶ", "Α\u0342" // perispomeni
    "ᾷ", "Α\u0342Ι" // perispomeni
    "ᾼ", "ΑΙ"
    "ῂ", "ῊΙ"
    "ῃ", "ΗΙ"
    "ῄ", "ΉΙ"
    "ῆ", "Η\u0342" // perispomeni
    "ῇ", "Η\u0342Ι" // perispomeni
    "ῌ", "ΗΙ"
    "ῒ", "Ι\u0308\u0300" // diaeresis, grave accent
    "\u1FD3", "Ι\u0308\u0301" // a second ΐ; diaeresis, acute accent
    "ῖ", "Ι\u0342" // perispomeni
    "ῗ", "Ι\u0308\u0342" // diaeresis, perispomeni
    "ῢ", "Υ\u0308\u0300" // diaeresis, grave accent
    "\u1FE3", "Υ\u0308\u0301" // a second ΰ; diaeresis, acute accent
    "ῤ", "Ρ\u0313" // comma above
    "ῦ", "Υ\u0342" // perispomeni
    "ῧ", "Υ\u0308\u0342" // diaeresis, perispomeni
    "ῲ", "ῺΙ"
    "ῳ", "ΩΙ"
    "ῴ", "ΏΙ"
    "ῶ", "Ω\u0342" // perispomeni
    "ῷ", "Ω\u0342Ι" // perispomeni
    "ῼ", "ΩΙ"
    "ﬀ", "FF"
    "ﬁ", "FI"
    "ﬂ", "FL"
    "ﬃ", "FFI"
    "ﬄ", "FFL"
    "ﬅ", "ST"
    "ﬆ", "ST"
    "ﬓ", "ՄՆ"
    "ﬔ", "ՄԵ"
    "ﬕ", "ՄԻ"
    "ﬖ", "ՎՆ"
    "ﬗ", "ՄԽ" ]
  |> List.map (fun (lower, upper) -> (Rune.GetRuneAt(lower, 0)).Value, upper)
  |> dict


/// Letters' word-internal punctuation (Word_Break MidLetter, MidNumLet, Single_Quote),
/// which Unicode counts as case-ignorable: a sigma before an apostrophe is still final.
let private wordInternal : Set<int> =
  set
    [ 0x0027
      0x002E
      0x003A
      0x00B7
      0x0387
      0x055F
      0x05F4
      0x2018
      0x2019
      0x2024
      0x2027
      0xFE13
      0xFE52
      0xFE55
      0xFF07
      0xFF0E
      0xFF1A ]

/// Unicode's Case_Ignorable: marks, format characters, modifiers, and the punctuation
/// above. A cased letter can sit on either side of one and still be next to a sigma.
let private isCaseIgnorable (r : Rune) : bool =
  match Rune.GetUnicodeCategory r with
  | UnicodeCategory.NonSpacingMark
  | UnicodeCategory.EnclosingMark
  | UnicodeCategory.Format
  | UnicodeCategory.ModifierLetter
  | UnicodeCategory.ModifierSymbol -> true
  | _ -> wordInternal.Contains r.Value

/// Unicode's Cased, close enough for final sigma: a letter with a case, or anything with
/// a case mapping.
let private isCased (r : Rune) : bool =
  match Rune.GetUnicodeCategory r with
  | UnicodeCategory.UppercaseLetter
  | UnicodeCategory.LowercaseLetter
  | UnicodeCategory.TitlecaseLetter -> true
  | _ -> simpleUpper r <> r || simpleLower r <> r

/// Whether the `Σ` at <paramref name="i"/> ends a word, and so lowercases to `ς`: a cased
/// letter before it and none after it, looking past case-ignorable characters both ways.
let private isFinalSigma (runes : Rune[]) (i : int) : bool =
  let nearest (indices : seq<int>) : Option<Rune> =
    indices
    |> Seq.map (fun j -> runes[j])
    |> Seq.tryFind (fun r -> not (isCaseIgnorable r))

  let before = nearest (seq { i - 1 .. -1 .. 0 })
  let after = nearest (seq { i + 1 .. runes.Length - 1 })
  Option.exists isCased before && not (Option.exists isCased after)


/// Map each scalar of <paramref name="s"/> to a string, given the whole sequence for
/// context, then normalize the result, as string literals are, so a mapping that leaves a
/// letter and marks composes them where Unicode has a composed form.
let private mapScalars (f : Rune[] -> int -> string) (s : string) : string =
  let runes = s.EnumerateRunes() |> Array.ofSeq
  let mapped = Array.init runes.Length (f runes)
  System.String.Concat(mapped) |> String.normalize


/// `Stdlib.String.toUppercase`: the full mapping, so the result can be longer.
let toUppercase (s : string) : string =
  if Ascii.IsValid s then
    s.ToUpperInvariant()
  else
    s
    |> mapScalars (fun runes i ->
      let r = runes[i]
      match multiUpper.TryGetValue r.Value with
      | true, upper -> upper
      | _ -> (simpleUpper r).ToString())

/// `Stdlib.String.toLowercase`: the full mapping, so the result can be longer.
let toLowercase (s : string) : string =
  if Ascii.IsValid s then
    s.ToLowerInvariant()
  else
    s
    |> mapScalars (fun runes i ->
      let r = runes[i]
      if r.Value = 0x0130 then "i\u0307"
      elif r.Value = 0x03A3 && isFinalSigma runes i then "\u03C2"
      else (simpleLower r).ToString())


/// `Stdlib.Char.toUppercase`: the simple mapping, scalar by scalar.
let charToUppercase (c : string) : string =
  c |> mapScalars (fun runes i -> (simpleUpper runes[i]).ToString())

/// `Stdlib.Char.toLowercase`: the simple mapping, scalar by scalar.
let charToLowercase (c : string) : string =
  c |> mapScalars (fun runes i -> (simpleLower runes[i]).ToString())
