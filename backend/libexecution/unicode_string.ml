open Core_kernel

type t = string [@@deriving show]

(* General note about operations in this file:
 * [`Grapheme_cluster] is an extended grapheme cluster
 * in accordance with UAX 29 (see https://erratique.ch/software/uuseg/doc/Uuseg/index.html).
 *
 * Performing operations on strings (such as concatenation) may "denormalize" them
 * which means that we have to re-normalize. We use the [`NFC] normalization form,
 * which is the best form used for general text (see http://www.unicode.org/faq/normalization.html#2).
 *
 * For identifiers, [`NFKC] is preferred in situations where there are security concerns
 * due to, for example, visual spoofing. See http://www.unicode.org/faq/normalization.html#2.
 * We do not currently use that form.
 *)

(* This just treats it like any old string, if we have lots of tests that use this
 * we should consider only printing ASCII chars, and outputting hex syntax bytes
 * like "hello, \xfd\xfd" *)
let pp t = Format.pp_print_string t

(* from http://erratique.ch/software/uucp/doc/Uucp.Case.html#caseexamples *)
let cmap_utf_8 cmap s =
  let b = Buffer.create (String.length s * 2) in
  let rec add_map _ _ u =
    let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
    match cmap u with
    | `Self ->
        Uutf.Buffer.add_utf_8 b u
    | `Uchars us ->
        List.iter us (Uutf.Buffer.add_utf_8 b)
  in
  Uutf.String.fold_utf_8 add_map () s ;
  Buffer.contents b


module Character = struct
  type t = string

  let equal a b = a = b

  let compare a b = Stdlib.compare a b

  let to_string t = t

  let pp t = Format.pp_print_string t

  (* Characters are simply 'views' of parts of a Unicode_string.t, and it would
   * be best to allow no creation of Character.t values outside of iterating
   * over a unicode string after applying a clustering algorithm. But sometimes
   * we need to, and there isn't a good way to validate it. Here be dragons.
   * When creating, you need to make sure this is a valid EGC (such as ASCII -
   * all ASCII is valid here). *)
  let unsafe_of_string t = t

  let to_yojson t = `String t

  (* See of_string comment *)
  let of_yojson j = match j with `String s -> Ok s | _ -> Error "Not bytes"
end

let normalize_utf_8 (s : string) : t = Uunf_string.normalize_utf_8 `NFC s

(* This validates that the passed string is UTF-8 encoded, and also normalizes
 * it to a common normalization form (NFC). It does this in two passes. It's
 * possible to do this in a single pass via a much less ergonic normalization
 * entry-point in Uunf. Worthwhile optimisation, but not a priority rn *)
let of_utf8_encoded_string (s : string) : t option =
  (* the decoder has mutable state *)
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec validate_string () =
    match Uutf.decode decoder with
    | `Uchar ch when ch = Stdlib.Uchar.min ->
        `Err (* U+0000 is rejected by postgres *)
    | `Uchar _ ->
        validate_string ()
    | `End ->
        `Ok
    | `Await ->
        validate_string ()
    | `Malformed _ ->
        `Err
  in
  match validate_string () with
  | `Ok ->
      (* Note, this changes the byte representation such that the input string and the
       * Unicode_string.t no longer match *)
      Some (normalize_utf_8 s)
  | `Err ->
      None


let to_utf8_bytes s =
  (* This process works because the internal
     implementation here is via utf8 encoding.
     However, that detail shouldn't leak into the
     interface. *)
  let len = String.length s in
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set bytes i (String.unsafe_get s i)
  done ;
  bytes


let of_string = of_utf8_encoded_string

let of_utf8_encoded_string_exn ?message s =
  let possible_t = of_utf8_encoded_string s in
  let msg = Option.value ~default:("Invalid UTF-8 String: " ^ s) message in
  Option.value_exn ~message:msg possible_t


let of_string_exn = of_utf8_encoded_string_exn

let to_utf8_encoded_string t = t

let to_string = to_utf8_encoded_string

(* validity/normalization is NOT closed over appending
 * but String::append (deprecated) depends on this. *)
let append_broken l r = l ^ r

(* We have to renormalize because concatenation can often break normalization
 * (http://www.unicode.org/faq/normalization.html#5).
 * Note that there is an opportunity for optimization
 * "because at most a few characters in the immediate area of the adjoined strings need processing"
 *)
let append l r = l ^ r |> normalize_utf_8

let concat ~sep xs = String.concat ~sep xs

let map_characters ~f t =
  t
  |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc seg -> f seg :: acc) []
  |> List.rev


let characters t = map_characters ~f:ident t

let of_character g = of_utf8_encoded_string_exn g

let of_characters gs = of_utf8_encoded_string_exn (String.concat ~sep:"" gs)

(* Note: this is an O(N) operation, as we don't stash the length anywhere.
 * This should be turned into an O(1) operation, with the `t` in this module
 * being changed to a record that holds the buffer + the length. To achieve that
 * we'd have to rewrite the `of_utf8_encoded_string` function to do validation,
 * normalization, and length counting in a single pass. We'd also have to recalculate
 * the length in any function that hands back a new `t` but doesn't re-validate/normalize.
 * There's nothing fundamental preventing us from doing that, but it's left as future work
 * *)
let length t =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc _ -> 1 + acc) 0 t


let is_substring ~substring t = String.is_substring ~substring t

let starts_with ~prefix t = String.is_prefix ~prefix t

let ends_with ~suffix t = String.is_suffix ~suffix t

(* I don't know whether or not UTF-8 validity/normalization are defined operations
 * for the naive byte-sequence find+replace operations, hence the re-validation/normalization
 * after the fact. I couldn't find anything on a cursory Google, but I'd probably have to
 * read the RFCs to be sure. Re-validation will explode if we get any hits in prod, which
 * would be a good confirmation. Don't remove the re-validation unless you're sure it's safe *)
let replace ~search ~replace t =
  of_utf8_encoded_string_exn (Util.string_replace search replace t)


(* See the above comment for replace *)
let regexp_replace ~pattern ~replacement t =
  Libtarget.regexp_replace ~pattern ~replacement t |> of_utf8_encoded_string_exn


(* See the above comment for replace. Similar issue here, are all parts of the split string still
 * valid? *)
let split ~sep t =
  t |> Libtarget.string_split ~sep |> List.map ~f:of_utf8_encoded_string_exn


let rev t =
  t
  |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc seg -> seg :: acc) []
  |> String.concat


let uppercase = cmap_utf_8 Uucp.Case.Map.to_upper

let lowercase = cmap_utf_8 Uucp.Case.Map.to_lower

let slice s ~first ~last =
  let clamp_unchecked t ~min ~max =
    if t < min then min else if t <= max then t else max
  in
  let len = length s in
  let min = 0 in
  let max = len + 1 in
  (* If we get negative indices, we need to treat them as indices from the end
   * which means that we need to add [len] to them. We clamp the result to
   * a value within range of the actual string: *)
  let first =
    if first >= 0 then first else len + first |> clamp_unchecked ~min ~max
  in
  let last =
    if last >= 0 then last else len + last |> clamp_unchecked ~min ~max
  in
  let b = Buffer.create (String.length s) in
  (* To slice, we iterate through every EGC, adding it to the buffer
   * if it is within the specified index range: *)
  let slicer_func acc seg =
    if acc >= first && acc < last then Buffer.add_string b seg else () ;
    1 + acc
  in
  ignore (s |> Uuseg_string.fold_utf_8 `Grapheme_cluster slicer_func 0) ;
  (* We don't need to renormalize because all normalization forms are closed
   * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
  Buffer.contents b


let first_n s num_egcs =
  let b = Buffer.create (String.length s) in
  (* We iterate through every EGC, adding it to the buffer
   * if its index < num_egcs: *)
  let first_func idx seg =
    if idx < num_egcs then Buffer.add_string b seg else () ;
    1 + idx
  in
  ignore (s |> Uuseg_string.fold_utf_8 `Grapheme_cluster first_func 0) ;
  (* We don't need to renormalize because all normalization forms are closed
   * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
  Buffer.contents b


let drop_first_n s num_egcs =
  let b = Buffer.create (String.length s) in
  (* We iterate through every EGC, adding it to the buffer
   * if its index >= num_egcs. This works by the inverse of the logic for [first_n]: *)
  let first_func idx seg =
    if idx >= num_egcs then Buffer.add_string b seg else () ;
    1 + idx
  in
  ignore (s |> Uuseg_string.fold_utf_8 `Grapheme_cluster first_func 0) ;
  (* We don't need to renormalize because all normalization forms are closed
   * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
  Buffer.contents b


let last_n s num_egcs =
  let b = Buffer.create (String.length s) in
  (* We iterate through every EGC, adding it to the buffer
   * if its [idx] >= ([s_egc_count] - [num_egcs]).
   * Consider if the string is "abcde" and [num_egcs] = 2,
   * [s_egc_count] = 5; 5-2 = 3. The index of "d" is 3 and
   * we want to keep it and everything after it so we end up with "de". *)
  let s_egc_count = length s in
  let start_idx = s_egc_count - num_egcs in
  let last_func idx seg =
    if idx >= start_idx then Buffer.add_string b seg else () ;
    1 + idx
  in
  ignore (s |> Uuseg_string.fold_utf_8 `Grapheme_cluster last_func 0) ;
  (* We don't need to renormalize because all normalization forms are closed
   * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
  Buffer.contents b


let drop_last_n s num_egcs =
  let b = Buffer.create (String.length s) in
  (* We iterate through every EGC, adding it to the buffer
   * if its [idx] < ([s_egc_count] - [num_egcs]).
   * This works by the inverse of the logic for [last_n]. *)
  let s_egc_count = length s in
  let start_idx = s_egc_count - num_egcs in
  let last_func idx seg =
    if idx < start_idx then Buffer.add_string b seg else () ;
    1 + idx
  in
  ignore (s |> Uuseg_string.fold_utf_8 `Grapheme_cluster last_func 0) ;
  (* We don't need to renormalize because all normalization forms are closed
   * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
  Buffer.contents b


let pad_start s ~pad_with target_egcs =
  let max a b = if a > b then a else b in
  (* Compute the size in bytes and # of required EGCs for s and pad_with: *)
  let pad_size = String.length pad_with in
  let pad_egcs = length pad_with in
  let s_size = String.length s in
  let s_egcs = length s in
  (* Compute how many copies of pad_with we require,
   * accounting for the string longer than [target_egcs]: *)
  let req_egcs = target_egcs - s_egcs in
  let req_pads = max 0 (if pad_egcs = 0 then 0 else req_egcs / pad_egcs) in
  (* Create a buffer large enough to hold the padded result: *)
  let req_size = s_size + (req_pads * pad_size) in
  let b = Buffer.create req_size in
  (* Fill with the required number of pads: *)
  for i = 1 to req_pads do
    Buffer.add_string b pad_with
  done ;
  (* Finish by filling with the string: *)
  Buffer.add_string b s ;
  (* Renormalize because concatenation may break normalization
   * (see https://unicode.org/reports/tr15/#Concatenation): *)
  Buffer.contents b |> normalize_utf_8


let pad_end s ~pad_with target_egcs =
  let max a b = if a > b then a else b in
  (* Compute the size in bytes and # of required EGCs for s and pad_with: *)
  let pad_size = String.length pad_with in
  let pad_egcs = length pad_with in
  let s_size = String.length s in
  let s_egcs = length s in
  (* Compute how many copies of pad_with we require,
   * accounting for the string longer than [target_egcs]: *)
  let req_egcs = target_egcs - s_egcs in
  let req_pads = max 0 (if pad_egcs = 0 then 0 else req_egcs / pad_egcs) in
  (* Create a buffer large enough to hold the padded result: *)
  let req_size = s_size + (req_pads * pad_size) in
  let b = Buffer.create req_size in
  (* Start the buffer with the string: *)
  Buffer.add_string b s ;
  (* Finish by filling with the required number of pads: *)
  for i = 1 to req_pads do
    Buffer.add_string b pad_with
  done ;
  (* Renormalize because concatenation may break normalization
   * (see https://unicode.org/reports/tr15/#Concatenation): *)
  Buffer.contents b |> normalize_utf_8


let trim_start s =
  let b = Buffer.create (String.length s) in
  let seen_non_ws = ref false in
  let trimmer_func _ _ u =
    let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
    let is_ws = Uucp.White.is_white_space u in
    match (!seen_non_ws, is_ws) with
    | false, false ->
        seen_non_ws := true ;
        Uutf.Buffer.add_utf_8 b u
    | false, true ->
        ()
    | true, true | true, false ->
        Uutf.Buffer.add_utf_8 b u
  in
  Uutf.String.fold_utf_8 trimmer_func () s ;
  Buffer.contents b


(* This implementation is terrible but I don't have time to do the index matches *)
let trim_end t = t |> rev |> trim_start |> rev

let trim t = t |> trim_start |> trim_end

(* Structual equality is okay because the byte-structure of normalized strings is stable *)
let equal a b = a = b

(* Structual compare is okay because the byte-structure of normalized strings is stable,
 * and defines a total order. That order is arbitrary wrt. to the Unicode codepoint table.
 * It _may_ be accidentally lexicographic for the ASCII compatible subset of UTF-8. *)
let compare a b = Stdlib.compare a b

let to_yojson t = `String t

let of_yojson j =
  match j with
  | `String s ->
    ( match of_utf8_encoded_string s with
    | Some ss ->
        Ok ss
    | None ->
        Error ("Invalid string: " ^ s) )
  | _ ->
      Error "Not a string"
