open Core_kernel

type t = string [@@deriving show]

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

  let compare a b = Pervasives.compare a b

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

(* This validates that the passed string is UTF-8 encoded, and also normalizes
 * it to a common normalization form (NFC). It does this in two passes. It's
 * possible to do this in a single pass via a much less ergonic normalization
 * entry-point in Uunf. Worthwhile optimisation, but not a priority rn *)
let of_utf8_encoded_string (s : string) : t option =
  (* the decoder has mutable state *)
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec validate_string () =
    match Uutf.decode decoder with
    | `Uchar ch when ch = Uchar.min ->
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
      Some (Uunf_string.normalize_utf_8 `NFC s)
  | `Err ->
      None


let to_utf8_bytes s = 
  (* This process works because the internal
     implementation here is via utf8 encoding.
     However, that detail shouldn't leak into the
     interface. *)
  let len = String.length s in
  let bytes = Bytes.create len in
  for i = 0 to len-1 do
    Bytes.unsafe_set bytes i (String.unsafe_get s i)
  done;
  bytes

let of_string = of_utf8_encoded_string

let of_utf8_encoded_string_exn ?message s =
  let possible_t = of_utf8_encoded_string s in
  let msg = Option.value ~default:("Invalid UTF-8 String: " ^ s) message in
  Option.value_exn ~message:msg possible_t


let of_string_exn = of_utf8_encoded_string_exn

let to_utf8_encoded_string t = t

let to_string = to_utf8_encoded_string

(* validity/normalization is closed over appending *)
let append l r = l ^ r

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

(* I don't know whether or not UTF-8 validity/normalization are defined operations
 * for the naive byte-sequence find+replace operations, hence the re-validation/normalization
 * after the fact. I couldn't find anything on a cursory Google, but I'd probably have to
 * read the RFCs to be sure. Re-validation will explode if we get any hits in prod, which
 * would be a good confirmation. Don't remove the re-validation unless you're sure it's safe *)
let replace ~search ~replace t =
  of_utf8_encoded_string_exn (Util.string_replace search replace t)


(* See the above comment for replace *)
let regexp_replace ~pattern ~replacement t =
  Libtarget.regexp_replace ~pattern ~replacement t
  |> of_utf8_encoded_string_exn


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

let trim_left s =
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
let trim_right t = t |> rev |> trim_left |> rev

let trim t = t |> trim_left |> trim_right

(* Structual equality is okay because the byte-structure of normalized strings is stable *)
let equal a b = a = b

(* Structual compare is okay because the byte-structure of normalized strings is stable,
 * and defines a total order. That order is arbitrary wrt. to the Unicode codepoint table.
 * It _may_ be accidentally lexicographic for the ASCII compatible subset of UTF-8. *)
let compare a b = Pervasives.compare a b

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
