open Prelude

type t = caretTarget

(** [forARStringOpenQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the open quote of the string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. *)
let forARStringOpenQuote (id : ID.t) (offset : int) : t =
  {astRef = ARString (id, SPOpenQuote); offset}


(** [forARStringText id offset] produces an ARString caretTarget
* pointing to an [offset] into the text of the string with [id].
* [offset] may be negative but cannot represent something out of string bounds. *)
let forARStringText (id : ID.t) (offset : int) : t =
  {astRef = ARString (id, SPOpenQuote); offset = 1 + offset}


(** [forARStringCloseQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the close quote of the string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. *)
let forARStringCloseQuote (id : ID.t) (offset : int) (fullStr : string) : t =
  let lenPlusOpenQuote = 1 + String.length fullStr in
  {astRef = ARString (id, SPOpenQuote); offset = lenPlusOpenQuote + offset}


(** [forPPStringOpenQuote id offset] produces an ARPattern PPString caretTarget
* pointing to an [offset] into the open quote of the pattern string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. *)
let forPPStringOpenQuote (id : ID.t) (offset : int) : t =
  {astRef = ARPattern (id, PPString SPOpenQuote); offset}


(** [forPPStringText id offset] produces an ARPattern PPString caretTarget
* pointing to an [offset] into the text of the pattern string with [id].
* [offset] may be negative but cannot represent something out of string bounds. *)
let forPPStringText (id : ID.t) (offset : int) : t =
  {astRef = ARPattern (id, PPString SPOpenQuote); offset = 1 + offset}


(** [forARStringCloseQuote id offset] produces an ARPattern PPString caretTarget
* pointing to an [offset] into the close quote of the pattern string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. *)
let forPPStringCloseQuote (id : ID.t) (offset : int) (fullStr : string) : t =
  let lenPlusOpenQuote = 1 + String.length fullStr in
  { astRef = ARPattern (id, PPString SPOpenQuote)
  ; offset = lenPlusOpenQuote + offset }
