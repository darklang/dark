module Prelude

module String =
  // Returns a seq of EGC (extended grapheme cluster - essentially a visible
  // screen character)
  // https://stackoverflow.com/a/4556612/104021
  let toTextElements (s: string): seq<string> =
    seq {
      let tee =
        System.Globalization.StringInfo.GetTextElementEnumerator(s)

      while tee.MoveNext() do
        yield tee.GetTextElement()
    }
