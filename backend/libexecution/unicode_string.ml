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

  let of_yojson j = failwith "TODO(ian)"
end

let of_utf8 (s : string) : t option =
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


let of_utf8_exn ?message s =
  let possible_t = of_utf8 s in
  let msg = Option.value ~default:("Invalid UTF-8 String: " ^ s) message in
  Option.value_exn ~message:msg possible_t


let to_utf8 t = t

(* validity/normalization is closed over appending *)
let append l r = l ^ r

let concat ~sep xs = String.concat ~sep xs

let map_graphemes ~f t =
  t
  |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc seg -> f seg :: acc) []
  |> List.rev


let graphemes t = map_graphemes ~f:ident t

let of_grapheme g = of_utf8_exn g

let of_graphemes gs = of_utf8_exn (String.concat ~sep:"" gs)

let length t =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc _ -> 1 + acc) 0 t


let is_substring ~substring t = String.is_substring ~substring t

(* TODO: Reconsider this -- is this dangerous? I'm unsure, going to revalidate *)
let replace ~search ~replace t =
  of_utf8_exn (Util.string_replace search replace t)


(* TODO: Reconsider this -- is this dangerous? I'm unsure, going to revalidate *)
let regexp_replace ~pattern ~replacement t =
  Libtarget.regexp_replace ~pattern ~replacement t |> of_utf8_exn


let split ~sep t = t |> Libtarget.string_split ~sep |> List.map ~f:of_utf8_exn

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
    ( match of_utf8 s with
    | Some ss ->
        Ok ss
    | None ->
        Error ("Invalid string: " ^ s) )
  | _ ->
      Error "Not a string"
