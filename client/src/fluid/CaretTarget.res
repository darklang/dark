open Prelude

@ppx.deriving(show({with_path: false}))
type rec t = FluidCursorTypes.CaretTarget.t

@ocaml.doc(" [forARStringOpenQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the open quote of the string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. ")
let forARStringOpenQuote = (id: id, offset: int): t => {
  astRef: ARString(id, SPOpenQuote),
  offset: offset,
}

@ocaml.doc(" [forARStringText id offset] produces an ARString caretTarget
* pointing to an [offset] into the text of the string with [id].
* [offset] may be negative but cannot represent something out of string bounds. ")
let forARStringText = (id: id, offset: int): t => {
  astRef: ARString(id, SPOpenQuote),
  offset: 1 + offset,
}

@ocaml.doc(" [forARStringCloseQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the close quote of the string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. ")
let forARStringCloseQuote = (id: id, offset: int, fullStr: string): t => {
  let lenPlusOpenQuote = 1 + String.length(fullStr)
  {astRef: ARString(id, SPOpenQuote), offset: lenPlusOpenQuote + offset}
}

@ocaml.doc(" [forMPPStringOpenQuote id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the open quote of the pattern string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. ")
let forMPPStringOpenQuote = (id: id, offset: int): t => {
  astRef: ARMPattern(id, MPPString(SPOpenQuote)),
  offset: offset,
}

@ocaml.doc(" [forMPPStringText id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the text of the pattern string with [id].
* [offset] may be negative but cannot represent something out of string bounds. ")
let forMPPStringText = (id: id, offset: int): t => {
  astRef: ARMPattern(id, MPPString(SPOpenQuote)),
  offset: 1 + offset,
}

@ocaml.doc(" [forARStringCloseQuote id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the close quote of the pattern string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. ")
let forMPPStringCloseQuote = (id: id, offset: int, fullStr: string): t => {
  let lenPlusOpenQuote = 1 + String.length(fullStr)
  {
    astRef: ARMPattern(id, MPPString(SPOpenQuote)),
    offset: lenPlusOpenQuote + offset,
  }
}
