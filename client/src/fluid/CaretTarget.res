open Prelude

@ppx.deriving(show({with_path: false}))
type rec t = FluidCursorTypes.CaretTarget.t

@ocaml.doc(" [forARStringOpenQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the open quote of the string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. ")
let forARStringOpenQuote = (id: id, offset: int): t => {
  Recover.asserT("unexpected openquote offset", ~debug=offset, offset == 0 || offset == 1)
  {
    astRef: ARString(id, SPOpenQuote),
    offset: offset,
  }
}

@ocaml.doc(" [forARStringText id offset] produces an ARString caretTarget
* pointing to an [offset] into the text of the string with [id].
* [offset] may be negative but cannot represent something out of string bounds. ")
let forARStringBody = (id: id, offset: int, str: string): t => {
  if str == "" {
    forARStringOpenQuote(id, 1)
  } else {
    Recover.asserT(
      "unexpected string body offset",
      ~debug=(offset, str),
      offset >= 0 && 0 < String.length(str),
    )
    {
      astRef: ARString(id, SPBody),
      offset: offset,
    }
  }
}

@ocaml.doc(" [forARStringCloseQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the close quote of the string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. ")
let forARStringCloseQuote = (id: id, offset: int): t => {
  Recover.asserT("unexpected closequote offset", ~debug=offset, offset == 0 || offset == 1)
  {astRef: ARString(id, SPCloseQuote), offset: offset}
}

@ocaml.doc(" [forMPPStringOpenQuote id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the open quote of the pattern string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. ")
let forMPPStringOpenQuote = (id: id, offset: int): t => {
  astRef: ARMPattern(id, MPPString),
  offset: offset,
}

@ocaml.doc(" [forMPPStringText id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the text of the pattern string with [id].
* [offset] may be negative but cannot represent something out of string bounds. ")
let forMPPStringText = (id: id, offset: int): t => {
  astRef: ARMPattern(id, MPPString),
  offset: 1 + offset,
}

@ocaml.doc(" [forMPPStringCloseQuote id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the close quote of the pattern string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. ")
let forMPPStringCloseQuote = (id: id, offset: int, fullStr: string): t => {
  let lenPlusOpenQuote = 1 + String.length(fullStr)
  {
    astRef: ARMPattern(id, MPPString),
    offset: lenPlusOpenQuote + offset,
  }
}
