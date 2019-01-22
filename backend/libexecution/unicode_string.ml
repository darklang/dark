open Core_kernel

type t = string

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

  let to_yojson t = `String t

  (* This is needed to make upstream derivers compile correctly, though
   * we don't seem to actually _need_ this implementation because we
   * have special json encoders/decoders for dvals. Implementing this
   * is tricky, and it's best for now to allow no creation of
   * Character.t values outside of iterating over a unicode string
   * after applying a clustering algorithm. ie. these values are
   * simply 'views' of parts of a Unicode_string.t *)
  let of_yojson j = failwith "Not implemented: Character of yojson"
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
